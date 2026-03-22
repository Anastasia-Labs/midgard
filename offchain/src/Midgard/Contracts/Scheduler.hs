module Midgard.Contracts.Scheduler (initScheduler, scheduleNextOperator) where

import Control.Monad (when)
import Control.Monad.Except (MonadError (throwError))
import Control.Monad.Reader (runReaderT)
import Data.ByteString qualified as BS
import Data.Foldable (traverse_)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)

import Cardano.Api qualified as C
import Convex.BuildTx (
  MonadBuildTx,
  TxBuilder,
  addBtx,
  addReference,
  addRequiredSignature,
  assetValue,
  execBuildTx,
  findIndexReference,
  findIndexSpending,
  mintPlutus,
  payToScriptInlineDatum,
  setMinAdaDepositAll,
 )
import Convex.Class (
  MonadBlockchain (queryNetworkId, queryProtocolParameters, querySlotNo),
  MonadUtxoQuery,
  utxosByPaymentCredential,
 )
import Convex.PlutusLedger.V1 (transPOSIXTime, unTransPOSIXTime, unTransPubKeyHash)
import Convex.Utxos (toTxOut)
import PlutusLedgerApi.Common (BuiltinByteString, fromBuiltin, toBuiltin)
import PlutusLedgerApi.V3 (POSIXTime, PubKeyHash (PubKeyHash, getPubKeyHash))
import PlutusTx.Builtins qualified as PlutusTx

import Midgard.Constants (shiftDuration)
import Midgard.Contracts.Utils (
  LinkedListInfo (..),
  findFinalUTxONode,
  findOutputIndexWithAsset,
  findUTxOWithAsset,
  findUTxOWithLink,
  inlineDatumFromUTxO,
  listAssetNameFromUTxO,
  spendPlutusInlineDatumWithRedeemerFinal,
  utcTimeToEnclosingSlot,
 )
import Midgard.ScriptUtils (mintingPolicyId, toMintingPolicy, toValidator, validatorHash)
import Midgard.Scripts (
  MidgardScripts (
    MidgardScripts,
    activeOperatorsPolicy,
    activeOperatorsValidator,
    registeredOperatorsPolicy,
    registeredOperatorsValidator,
    schedulerPolicy,
    schedulerValidator
  ),
 )
import Midgard.Types.ActiveOperators qualified as ActiveOperators
import Midgard.Types.RegisteredOperators qualified as RegisteredOperators
import Midgard.Types.Scheduler qualified as Scheduler

initScheduler ::
  ( C.HasScriptLanguageInEra C.PlutusScriptV3 era
  , MonadBuildTx era m
  , C.IsBabbageBasedEra era
  ) =>
  C.NetworkId ->
  POSIXTime ->
  MidgardScripts ->
  m ()
initScheduler
  netId
  startTime
  MidgardScripts {schedulerValidator, schedulerPolicy} = do
    let C.PolicyId policyId = mintingPolicyId schedulerPolicy
    -- The scheduler token should be minted.
    mintPlutus (toMintingPolicy schedulerPolicy) Scheduler.Init Scheduler.assetName 1
    -- And sent to the scheduler validator.
    let datum :: Scheduler.Datum
        datum =
          Scheduler.Datum
            { -- Starts with an invalid pub key hash. The aim is to replace it after the shift end time.
              operator = PubKeyHash $ PlutusTx.toBuiltin BS.empty
            , startTime = startTime
            }
    payToScriptInlineDatum
      netId
      (validatorHash schedulerValidator)
      datum
      C.NoStakeAddress
      (assetValue policyId Scheduler.assetName 1)

{- | Set the scheduler to designate the next operator, either before or after the current operator's shift end time.
In the first case, existing shift operator must sign the transaction.
Note: This will either 'Advance' or 'Rewind' depending on the current operator position.
-}
scheduleNextOperator ::
  forall era m.
  ( MonadError String m
  , MonadBlockchain era m
  , MonadUtxoQuery m
  , C.HasScriptLanguageInEra C.PlutusScriptV3 era
  , C.IsBabbageBasedEra era
  ) =>
  Bool ->
  MidgardScripts ->
  m (TxBuilder era, POSIXTime)
scheduleNextOperator
  isBeforeShiftEnd
  ms@MidgardScripts
    { schedulerValidator
    , schedulerPolicy
    , activeOperatorsValidator
    , activeOperatorsPolicy
    } = do
    params <- queryProtocolParameters
    netId <- queryNetworkId
    (currentSlot, _, _) <- querySlotNo
    -- Find the scheduler UTxO. There should be only one.
    schedulerUtxos <-
      utxosByPaymentCredential $
        C.PaymentCredentialByScript $
          validatorHash schedulerValidator
    (schedulerTxIn, (schedulerUtxoAnyEra, _)) <-
      maybe (throwError "No scheduler state found") pure $
        findUTxOWithAsset schedulerUtxos $
          C.AssetId (mintingPolicyId schedulerPolicy) Scheduler.assetName
    let schedulerTxOut = toTxOut @era schedulerUtxoAnyEra
    -- Obtain the current operator and shift info.
    schedulerDatum <-
      maybe (throwError "Invalid scheduler datum") pure $
        inlineDatumFromUTxO @Scheduler.Datum schedulerTxOut
    let Scheduler.Datum {operator = currentOperator, startTime = currentStartTime} = schedulerDatum
    -- Find the next operator in schedule. This should be the previous node in the active operators linked list.
    activeOperatorsUtxos <-
      utxosByPaymentCredential $
        C.PaymentCredentialByScript $
          validatorHash activeOperatorsValidator
    let activeOperatorsListInfo =
          LinkedListInfo
            { ownerPolicyId = mintingPolicyId activeOperatorsPolicy
            , rootAssetName = ActiveOperators.rootAssetName
            , nodeAssetNamePrefix = ActiveOperators.nodeAssetNamePrefix
            }
        currentOperatorBytes = fromBuiltin $ getPubKeyHash currentOperator
    -- If currentOperator is empty bytestring, it's a placeholder. Find the last node and set it as operator.
    -- Otherwise, find the _previous_ node of the current operator.
    let finderF = if BS.null currentOperatorBytes then findFinalUTxONode else flip findUTxOWithLink currentOperatorBytes
    (predecessorActiveNodeTxIn, (predecessorActiveNodeUtxoAnyEra, _)) <-
      maybe
        (throwError "Previous active operator node not found")
        pure
        . flip runReaderT activeOperatorsListInfo
        $ finderF activeOperatorsUtxos
    let predecessorActiveNodeTxOut = toTxOut @era predecessorActiveNodeUtxoAnyEra
    -- Figure out the next shift starting slot so it can be set in the validity range.
    let nextShiftStartTime = unTransPOSIXTime currentStartTime + shiftDuration
    nextShiftStartSlot <- utcTimeToEnclosingSlot . posixSecondsToUTCTime $ nextShiftStartTime
    -- Decide whether to advance or rewind and obtain the information necessary for the chosen path.
    (nextOperator, additionalRefs, mkRedeemer) <-
      constructAdvanceOrRewind ms schedulerTxIn predecessorActiveNodeTxIn predecessorActiveNodeTxOut
    let nextSchedulerDatum =
          Scheduler.Datum
            { operator = PubKeyHash nextOperator
            , startTime = transPOSIXTime nextShiftStartTime
            }
    pure . (,transPOSIXTime nextShiftStartTime) . execBuildTx $ do
      -- Witness the next operator being added and any other requirements.
      addReference predecessorActiveNodeTxIn
      traverse_ addReference additionalRefs
      -- Update the datum to reflect the next operator's shift.
      spendPlutusInlineDatumWithRedeemerFinal
        (toValidator schedulerValidator)
        schedulerTxIn
        mkRedeemer
      payToScriptInlineDatum
        netId
        (validatorHash schedulerValidator)
        nextSchedulerDatum
        C.NoStakeAddress
        (txOutValue schedulerTxOut)
      -- If a shift is ending prematurely, the existing operator must sign off.
      when isBeforeShiftEnd $ do
        -- Assumption: Current operator is valid if we're passed isBeforeShiftEnd = rue.
        currentOperatorC <- either (error . show) pure $ unTransPubKeyHash currentOperator
        addRequiredSignature currentOperatorC
      addBtx $ setValidity currentSlot nextShiftStartSlot
      setMinAdaDepositAll params
    where
      txOutValue (C.TxOut _ val _ _) = C.txOutValueToValue val

      -- Set the validity based on whether or nor we're advancing after a shift end or before.
      setValidity currentSlot nextShiftStartSlot txBody
        | isBeforeShiftEnd =
            txBody
              { C.txValidityUpperBound =
                  C.TxValidityUpperBound (C.shelleyBasedEra @era) . Just
                  -- Either 5 minutes into the future, or just before next shift start, whichever is earlier.
                  $
                    min (currentSlot + 300) (nextShiftStartSlot - 1)
              }
        | otherwise =
            txBody
              { -- Shift start slot must be strictly in the past.
                C.txValidityLowerBound = C.TxValidityLowerBound (C.allegraBasedEra @era) $ nextShiftStartSlot + 1
              }

{- | Decide whether we need to 'Advance' or 'Rewind' based on what the previous node is, and yield all necessary
structures to perform the right operation.
-}
constructAdvanceOrRewind ::
  forall era m ctx.
  ( MonadError String m
  , MonadBlockchain era m
  , MonadUtxoQuery m
  , C.HasScriptLanguageInEra C.PlutusScriptV3 era
  , C.IsBabbageBasedEra era
  ) =>
  MidgardScripts ->
  C.TxIn ->
  C.TxIn ->
  C.TxOut ctx era ->
  m
    ( BuiltinByteString
    , [C.TxIn]
    , C.TxBodyContent C.BuildTx era -> Scheduler.SpendRedeemer
    )
constructAdvanceOrRewind
  MidgardScripts
    { schedulerPolicy
    , activeOperatorsValidator
    , activeOperatorsPolicy
    , registeredOperatorsValidator
    , registeredOperatorsPolicy
    }
  schedulerTxIn
  predecessorActiveNodeTxIn
  predecessorActiveNodeTxOut = do
    activeNodeAssetName <-
      maybe (throwError "Previous active operator node missing operator NFT") pure $
        listAssetNameFromUTxO (mintingPolicyId activeOperatorsPolicy) predecessorActiveNodeTxOut
    -- It may be that we're at the head of the list and the previous node is root. At this point, we must rewind back.
    if activeNodeAssetName == ActiveOperators.rootAssetName
      then do
        activeOperatorsUtxos <-
          utxosByPaymentCredential $
            C.PaymentCredentialByScript $
              validatorHash activeOperatorsValidator
        -- Rewind to the last active operators node.
        (finalActiveOperatorTxIn, (finalActiveOperatorUtxoAnyEra, _)) <-
          maybe
            (throwError "Final active operator node not found")
            pure
            . flip
              runReaderT
              LinkedListInfo
                { ownerPolicyId = mintingPolicyId activeOperatorsPolicy
                , rootAssetName = ActiveOperators.rootAssetName
                , nodeAssetNamePrefix = ActiveOperators.nodeAssetNamePrefix
                }
            $ findFinalUTxONode activeOperatorsUtxos
        nextOperator <-
          maybe (throwError "Active operator asset not found for final node") (pure . assetNameToActiveOperatorKey)
            . listAssetNameFromUTxO (mintingPolicyId activeOperatorsPolicy)
            $ toTxOut @era finalActiveOperatorUtxoAnyEra
        -- We'll also need the final registered operators node.
        registeredOperatorsUtxos <-
          utxosByPaymentCredential $
            C.PaymentCredentialByScript $
              validatorHash registeredOperatorsValidator
        (finalRegisteredOperatorTxIn, _) <-
          maybe
            (throwError "Final registered operator node not found")
            pure
            . flip
              runReaderT
              LinkedListInfo
                { ownerPolicyId = mintingPolicyId registeredOperatorsPolicy
                , rootAssetName = RegisteredOperators.rootAssetName
                , nodeAssetNamePrefix = RegisteredOperators.nodeAssetNamePrefix
                }
            $ findFinalUTxONode registeredOperatorsUtxos
        -- TODO: Make validity upper bound based on finalRegisteredOperatorUtxoAnyEra datum activation time.
        -- let registeredTailTxOut = toTxOut @era registeredTailUtxoAnyEra
        --   registeredTailDatum <-
        --     maybe (throwError "Invalid registered operators tail datum") pure $
        --       inlineDatumFromUTxO @RegisteredOperators.Datum registeredTailTxOut
        --   validityUpperBound <- case LinkedList.elementData registeredTailDatum of
        --     LinkedList.Root _ -> pure Nothing
        --     LinkedList.Node RegisteredOperators.NodeData {activationTime} ->
        --       Just <$> utcTimeToEnclosingSlot (posixSecondsToUTCTime $ unTransPOSIXTime activationTime)
        let mkRedeemer txBody =
              Scheduler.Rewind
                { schedulerInputIndex = toInteger $ findIndexSpending schedulerTxIn txBody
                , schedulerOutputIndex =
                    toInteger $
                      findOutputIndexWithAsset
                        (mintingPolicyId schedulerPolicy)
                        Scheduler.assetName
                        txBody
                , activeNodeRefInputIndex = toInteger $ findIndexReference finalActiveOperatorTxIn txBody
                , activeRootRefInputIndex = toInteger $ findIndexReference predecessorActiveNodeTxIn txBody
                , registeredElementRefInputIndex = toInteger $ findIndexReference finalRegisteredOperatorTxIn txBody
                }
        pure (nextOperator, [finalActiveOperatorTxIn, finalRegisteredOperatorTxIn], mkRedeemer)
      else do
        let mkRedeemer txBody =
              Scheduler.Advance
                { schedulerInputIndex = toInteger $ findIndexSpending schedulerTxIn txBody
                , schedulerOutputIndex =
                    toInteger $
                      findOutputIndexWithAsset
                        (mintingPolicyId schedulerPolicy)
                        Scheduler.assetName
                        txBody
                , activeNodeRefInputIndex = toInteger $ findIndexReference predecessorActiveNodeTxIn txBody
                }
        pure (assetNameToActiveOperatorKey activeNodeAssetName, [], mkRedeemer)

assetNameToActiveOperatorKey :: C.AssetName -> BuiltinByteString
assetNameToActiveOperatorKey =
  toBuiltin . BS.drop ActiveOperators.nodeAssetNamePrefixLen . C.serialiseToRawBytes
