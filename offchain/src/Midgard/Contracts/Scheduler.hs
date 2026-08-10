module Midgard.Contracts.Scheduler (initScheduler, scheduleNextOperator, currentScheduleInfo) where

import Control.Monad (guard)
import Control.Monad.Except (MonadError (throwError))
import Control.Monad.Reader (runReaderT)
import Control.Monad.Trans (MonadTrans (lift))
import Control.Monad.Trans.Maybe (MaybeT (runMaybeT))
import Data.ByteString qualified as BS
import Data.Foldable (traverse_)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime, utcTimeToPOSIXSeconds)

import Cardano.Api qualified as C
import Convex.BuildTx (
  MonadBuildTx,
  TxBuilder,
  addBtx,
  addReference,
  assetValue,
  execBuildTx,
  findIndexReference,
  findIndexSpending,
  mintPlutusRef,
  payToScriptInlineDatum,
  setMinAdaDepositAll,
 )
import Convex.Class (
  MonadBlockchain (queryNetworkId, queryProtocolParameters, querySlotNo),
  MonadUtxoQuery,
  utxosByPaymentCredential,
 )
import Convex.PlutusLedger.V1 (transPOSIXTime, unTransPOSIXTime)
import Convex.Utxos (toTxOut)
import PlutusLedgerApi.Common (BuiltinByteString, toBuiltin)
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
  slotToEndUTCTime,
  spendPlutusInlineDatumWithRedeemerFinal,
  utcTimeToEnclosingSlot,
 )
import Midgard.ScriptUtils (mintingPolicyId, plutusVersion, toValidator, validatorHash)
import Midgard.Scripts (
  MidgardRefScripts (MidgardRefScripts, schedulerPolicyRef),
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
import Midgard.Types.LinkedList (nodeKeyFromAssetName, nodeKeyToPOSIXTime)
import Midgard.Types.RegisteredOperators qualified as RegisteredOperators
import Midgard.Types.Scheduler qualified as Scheduler

initScheduler ::
  ( C.HasScriptLanguageInEra C.PlutusScriptV3 era
  , MonadBuildTx era m
  , C.IsBabbageBasedEra era
  ) =>
  C.NetworkId ->
  MidgardScripts ->
  MidgardRefScripts ->
  m ()
initScheduler
  netId
  MidgardScripts {schedulerValidator, schedulerPolicy}
  MidgardRefScripts {schedulerPolicyRef} = do
    let C.PolicyId policyId = mintingPolicyId schedulerPolicy
    -- Use reference script to mint.
    addReference schedulerPolicyRef
    -- The scheduler token should be minted.
    mintPlutusRef
      schedulerPolicyRef
      (plutusVersion schedulerPolicy)
      policyId
      Scheduler.Init
      Scheduler.assetName
      1
    -- And sent to the scheduler validator.
    let datum :: Scheduler.Datum
        datum = Scheduler.NoActiveOperators
    payToScriptInlineDatum
      netId
      (validatorHash schedulerValidator)
      datum
      C.NoStakeAddress
      (assetValue policyId Scheduler.assetName 1)

{- | Set the scheduler to designate the next operator. This requires the current shift to come to an end.
Note: This will either go to the next operator or rewind depending on the current operator position.
-}
scheduleNextOperator ::
  forall era m.
  ( MonadError String m
  , MonadBlockchain era m
  , MonadUtxoQuery m
  , C.HasScriptLanguageInEra C.PlutusScriptV3 era
  , C.IsBabbageBasedEra era
  ) =>
  MidgardScripts ->
  m (TxBuilder era, POSIXTime)
scheduleNextOperator
  ms@MidgardScripts
    { schedulerValidator
    , schedulerPolicy
    } = do
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
    case schedulerDatum of
      Scheduler.NoActiveOperators -> appointFirstOperator ms (schedulerTxIn, schedulerTxOut)
      Scheduler.ActiveOperator {operator = currentOperator, startTime = currentStartTime} -> advanceOrRewindScheduler ms (schedulerTxIn, schedulerTxOut) currentOperator currentStartTime

{- | Appoint the very first operator into the scheduler.
This will pick the last operator node in the active operators set.
-}
appointFirstOperator ::
  forall era m ctx.
  ( MonadError String m
  , MonadBlockchain era m
  , MonadUtxoQuery m
  , C.HasScriptLanguageInEra C.PlutusScriptV3 era
  , C.IsBabbageBasedEra era
  ) =>
  MidgardScripts -> (C.TxIn, C.TxOut ctx era) -> m (TxBuilder era, POSIXTime)
appointFirstOperator
  MidgardScripts
    { activeOperatorsValidator
    , activeOperatorsPolicy
    , registeredOperatorsValidator
    , registeredOperatorsPolicy
    , schedulerValidator
    , schedulerPolicy
    }
  (schedulerTxIn, schedulerTxOut) = do
    params <- queryProtocolParameters
    netId <- queryNetworkId
    (currentSlot, _, _) <- querySlotNo
    -- The final node in the active operator set should be the first operator.
    activeOperatorsUtxos <-
      utxosByPaymentCredential $
        C.PaymentCredentialByScript $
          validatorHash activeOperatorsValidator
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
    -- Must witness the registered operators set to ensure no operator is waiting to be activated.
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
    -- In 4 minutes. Note: Must be less than env.max_validity_range.
    -- Note: Validity upper bound slot is exclusive when specified in Cardano.Api.
    -- i.e The inclusive upper bound is the second _before_ this slot begins.
    let validityUpperBoundExclusive = currentSlot + (4 * 60)
    nextShiftStartTime <- transPOSIXTime . utcTimeToPOSIXSeconds <$> slotToEndUTCTime (validityUpperBoundExclusive - 1)
    pure . (,nextShiftStartTime) . execBuildTx $ do
      addReference finalActiveOperatorTxIn
      addReference finalRegisteredOperatorTxIn
      spendPlutusInlineDatumWithRedeemerFinal
        (toValidator schedulerValidator)
        schedulerTxIn
        ( \txBody ->
            Scheduler.SpendRedeemer
              { schedulerInputIndex = toInteger $ findIndexSpending schedulerTxIn txBody
              , schedulerOutputIndex =
                  toInteger $
                    findOutputIndexWithAsset
                      (mintingPolicyId schedulerPolicy)
                      Scheduler.assetName
                      txBody
              , advancingApproach =
                  Scheduler.AppointFirstOperator
                    { newShiftsOperatorNodeRefInputIndex = toInteger $ findIndexReference finalActiveOperatorTxIn txBody
                    , registeredElementRefInputIndex = toInteger $ findIndexReference finalRegisteredOperatorTxIn txBody
                    }
              }
        )
      payToScriptInlineDatum
        netId
        (validatorHash schedulerValidator)
        Scheduler.ActiveOperator
          { operator = PubKeyHash nextOperator
          , startTime = nextShiftStartTime
          }
        C.NoStakeAddress
        (txOutValue schedulerTxOut)
      -- Short validity range based on the shift start time.
      addBtx $ \txBody ->
        txBody
          { C.txValidityUpperBound =
              C.TxValidityUpperBound (C.shelleyBasedEra @era) $ Just validityUpperBoundExclusive
          , C.txValidityLowerBound = C.TxValidityLowerBound (C.allegraBasedEra @era) currentSlot
          }
      setMinAdaDepositAll params
    where
      txOutValue (C.TxOut _ val _ _) = C.txOutValueToValue val

-- | Non-first operator case. Either advance or rewind the scheduler assuming the current shift has ended.
advanceOrRewindScheduler ::
  forall era m.
  ( MonadError String m
  , MonadBlockchain era m
  , MonadUtxoQuery m
  , C.HasScriptLanguageInEra C.PlutusScriptV3 era
  , C.IsBabbageBasedEra era
  ) =>
  MidgardScripts ->
  (C.TxIn, C.TxOut C.CtxUTxO era) ->
  PubKeyHash ->
  POSIXTime ->
  m (TxBuilder era, POSIXTime)
advanceOrRewindScheduler
  ms@MidgardScripts
    { schedulerValidator
    , schedulerPolicy
    , activeOperatorsValidator
    , activeOperatorsPolicy
    }
  (schedulerTxIn, schedulerTxOut)
  currentOperator
  currentStartTime = do
    params <- queryProtocolParameters
    netId <- queryNetworkId
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
        currentOperatorBytes = PlutusTx.fromBuiltin $ getPubKeyHash currentOperator
    (predecessorActiveNodeTxIn, (predecessorActiveNodeUtxoAnyEra, _)) <-
      maybe
        (throwError "Previous active operator node not found")
        pure
        . flip runReaderT activeOperatorsListInfo
        $ findUTxOWithLink activeOperatorsUtxos currentOperatorBytes
    let predecessorActiveNodeTxOut = toTxOut @era predecessorActiveNodeUtxoAnyEra
    -- Figure out the next shift (the shift we're scheduling for) starting slot so it can be set in the validity range.
    let nextShiftStartTime = unTransPOSIXTime currentStartTime + shiftDuration
    nextShiftStartSlot <- utcTimeToEnclosingSlot . posixSecondsToUTCTime $ nextShiftStartTime
    -- Decide whether to advance or rewind and obtain the information necessary for the chosen path.
    (nextOperator, additionalRefs, mkApproach, validityUpperBoundM) <-
      constructAdvanceOrRewind ms predecessorActiveNodeTxIn predecessorActiveNodeTxOut
    let nextSchedulerDatum =
          Scheduler.ActiveOperator
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
        ( \txBody ->
            Scheduler.SpendRedeemer
              { schedulerInputIndex = toInteger $ findIndexSpending schedulerTxIn txBody
              , schedulerOutputIndex =
                  toInteger $
                    findOutputIndexWithAsset
                      (mintingPolicyId schedulerPolicy)
                      Scheduler.assetName
                      txBody
              , advancingApproach = mkApproach txBody
              }
        )
      payToScriptInlineDatum
        netId
        (validatorHash schedulerValidator)
        nextSchedulerDatum
        C.NoStakeAddress
        (txOutValue schedulerTxOut)
      addBtx $ setValidityBasedOnShift nextShiftStartSlot
      addBtx $ updateValidityUpperBoundIfNeeded validityUpperBoundM
      setMinAdaDepositAll params
    where
      txOutValue (C.TxOut _ val _ _) = C.txOutValueToValue val

      -- Update the validity upper bound if needed (decided by constructAdvanceOrRewind).
      updateValidityUpperBoundIfNeeded Nothing txBody = txBody
      updateValidityUpperBoundIfNeeded
        (Just upperSlot)
        txBody@(C.TxBodyContent {C.txValidityUpperBound = C.TxValidityUpperBound era (Just existingUpperSlot)}) =
          txBody {C.txValidityUpperBound = C.TxValidityUpperBound era . Just $ min upperSlot existingUpperSlot}
      updateValidityUpperBoundIfNeeded
        (Just upperSlot)
        txBody@(C.TxBodyContent {C.txValidityUpperBound = C.TxValidityUpperBound era Nothing}) =
          txBody {C.txValidityUpperBound = C.TxValidityUpperBound era $ Just upperSlot}

      -- Set the validity based on whether or nor we're advancing after a shift end or before.
      setValidityBasedOnShift nextShiftStartSlot txBody =
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
  C.TxOut ctx era ->
  m
    ( BuiltinByteString
    , [C.TxIn]
    , C.TxBodyContent C.BuildTx era -> Scheduler.AdvancingApproach
    , Maybe C.SlotNo
    )
constructAdvanceOrRewind
  MidgardScripts
    { activeOperatorsValidator
    , activeOperatorsPolicy
    , registeredOperatorsValidator
    , registeredOperatorsPolicy
    }
  predecessorActiveNodeTxIn
  predecessorActiveNodeTxOut = do
    activeNodeAssetName <-
      maybe (throwError "Previous active operator node missing operator NFT") pure $
        listAssetNameFromUTxO (mintingPolicyId activeOperatorsPolicy) predecessorActiveNodeTxOut
    -- It may be that we're at the head of the list and the previous node is root. At this point, we must rewind back.
    if activeNodeAssetName == ActiveOperators.rootAssetName
      then do
        -- Rewind case.
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
        (finalRegisteredOperatorTxIn, (finalRegisteredOperatorUtxoAnyEra, _)) <-
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
        -- In case there is an operator in the registered operators list, ensure this scheduler transaction completes
        -- _before_ they can be activated.
        let finalRegisteredOperatorTxOut = toTxOut @era finalRegisteredOperatorUtxoAnyEra
        finalRegisteredOperatorAssetName <-
          maybe (throwError "Final registered operators node missing list asset") pure $
            listAssetNameFromUTxO
              (mintingPolicyId registeredOperatorsPolicy)
              finalRegisteredOperatorTxOut
        validityUpperBound <- runMaybeT $ do
          -- If the final node is just the root node, there are no pending operators to activate.
          guard $ finalRegisteredOperatorAssetName /= RegisteredOperators.rootAssetName
          let earliestOperatorActivationTime =
                nodeKeyToPOSIXTime $
                  nodeKeyFromAssetName RegisteredOperators.nodeAssetNamePrefixLen finalRegisteredOperatorAssetName
          lift $ utcTimeToEnclosingSlot (posixSecondsToUTCTime $ unTransPOSIXTime earliestOperatorActivationTime)
        let mkRedeemer txBody =
              Scheduler.RewindDueToEndOfShift
                { activeOperatorsRootRefInputIndex = toInteger $ findIndexReference predecessorActiveNodeTxIn txBody
                , activeOperatorsLastNodeRefInputIndex = toInteger $ findIndexReference finalActiveOperatorTxIn txBody
                , registeredElementRefInputIndex = toInteger $ findIndexReference finalRegisteredOperatorTxIn txBody
                }
        pure (nextOperator, [finalActiveOperatorTxIn, finalRegisteredOperatorTxIn], mkRedeemer, validityUpperBound)
      else do
        -- Advance case.
        let mkRedeemer txBody =
              Scheduler.GoToNextDueToEndOfShift
                { newShiftsOperatorNodeRefInputIndex = toInteger $ findIndexReference predecessorActiveNodeTxIn txBody
                }
        pure (assetNameToActiveOperatorKey activeNodeAssetName, [], mkRedeemer, Nothing)

-- | Obtain the currently scheduled operator PKH and the next shift start time as per the scheduler TxIn.
currentScheduleInfo ::
  forall era m.
  ( MonadError String m
  , MonadBlockchain era m
  , MonadUtxoQuery m
  , C.HasScriptLanguageInEra C.PlutusScriptV3 era
  , C.IsBabbageBasedEra era
  ) =>
  MidgardScripts ->
  m (C.TxIn, Maybe (PubKeyHash, POSIXTime))
currentScheduleInfo MidgardScripts {schedulerValidator, schedulerPolicy} = do
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
  pure . (schedulerTxIn,) $ case schedulerDatum of
    Scheduler.NoActiveOperators -> Nothing
    Scheduler.ActiveOperator {startTime = currentStartTime, operator = currentOperator} ->
      let nextShiftStartTime = unTransPOSIXTime currentStartTime + shiftDuration
       in Just (currentOperator, transPOSIXTime nextShiftStartTime)

assetNameToActiveOperatorKey :: C.AssetName -> BuiltinByteString
assetNameToActiveOperatorKey =
  toBuiltin . BS.drop ActiveOperators.nodeAssetNamePrefixLen . C.serialiseToRawBytes
