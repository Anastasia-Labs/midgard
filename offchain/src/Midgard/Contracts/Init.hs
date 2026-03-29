module Midgard.Contracts.Init (publishMidgardMintingPolicy, initProtocol) where

import Control.Monad.Except (MonadError)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)

import Cardano.Api qualified as C
import Convex.BuildTx (
  TxBuilder,
  addBtx,
  assetValue,
  createRefScriptNoDatum,
  execBuildTx,
  mintPlutus,
  payToScriptInlineDatum,
  setMinAdaDepositAll,
 )
import Convex.Class (MonadBlockchain (queryNetworkId, queryProtocolParameters, querySlotNo))
import Convex.PlutusLedger.V1 (transPOSIXTime)
import PlutusLedgerApi.V1 (ScriptHash (ScriptHash), currencySymbol, scriptHashAddress, toBuiltin)
import Ply (
  PlutusVersion (PlutusV3),
  ScriptParameter (AsRedeemer),
  TypedScript,
 )

import Midgard.Constants (hubOracleAssetName, hubOracleMintingScript, hubOracleScriptHash, refScriptStorage)
import Midgard.Contracts.ActiveOperators (initActiveOperators)
import Midgard.Contracts.RegisteredOperators (initRegisteredOperators)
import Midgard.Contracts.RetiredOperators (initRetiredOperators)
import Midgard.Contracts.Scheduler (initScheduler)
import Midgard.Contracts.StateQueue (initStateQueue)
import Midgard.Contracts.Utils (slotToEndUTCTime)
import Midgard.ScriptUtils (mintingPolicyId, policyIdBytes, scriptHashBytes, toMintingPolicy, validatorHash)
import Midgard.Scripts (
  MidgardRefScripts (..),
  MidgardScripts (..),
 )
import Midgard.Types.HubOracle qualified as HubOracle

initProtocol ::
  forall m.
  (MonadError String m, MonadBlockchain C.ConwayEra m) =>
  MidgardScripts ->
  MidgardRefScripts ->
  m (TxBuilder C.ConwayEra)
initProtocol
  scripts@MidgardScripts
    { retiredOperatorsPolicy
    , registeredOperatorsPolicy
    , activeOperatorsPolicy
    , schedulerPolicy
    , registeredOperatorsValidator
    , activeOperatorsValidator
    , retiredOperatorsValidator
    , schedulerValidator
    , stateQueuePolicy
    , stateQueueValidator
    }
  refScripts =
    do
      netId <- queryNetworkId
      params <- queryProtocolParameters
      (currentSlot, _, _) <- querySlotNo
      let validityUpperBoundExclusive = currentSlot + 300
      currentTime <- utcTimeToPOSIXSeconds <$> slotToEndUTCTime (validityUpperBoundExclusive - 1)
      pure . execBuildTx $ do
        -- The hub oracle is required for all initializations.
        -- TODO (chase): The real hub oracle must be parameterized by a nonce UTxO.
        mintPlutus hubOracleMintingScript () hubOracleAssetName 1
        payToScriptInlineDatum
          netId
          hubOracleScriptHash
          -- TODO (chase): Update some of the dummy values as needed.
          HubOracle.Datum
            { retiredOperators = scriptCurrencySymbol retiredOperatorsPolicy
            , activeOperators = scriptCurrencySymbol activeOperatorsPolicy
            , registeredOperators = scriptCurrencySymbol registeredOperatorsPolicy
            , scheduler = scriptCurrencySymbol schedulerPolicy
            , stateQueue = scriptCurrencySymbol stateQueuePolicy
            , fraudProofCatalogue = scriptCurrencySymbol registeredOperatorsPolicy
            , fraudProof = scriptCurrencySymbol registeredOperatorsPolicy
            , deposit = scriptCurrencySymbol registeredOperatorsPolicy
            , withdrawal = scriptCurrencySymbol registeredOperatorsPolicy
            , txOrder = scriptCurrencySymbol registeredOperatorsPolicy
            , settlement = scriptCurrencySymbol registeredOperatorsPolicy
            , payout = scriptCurrencySymbol registeredOperatorsPolicy
            , registeredOperatorsAddr = scriptHashAddress (scriptHash registeredOperatorsValidator)
            , activeOperatorsAddr = scriptHashAddress (scriptHash activeOperatorsValidator)
            , retiredOperatorsAddr = scriptHashAddress (scriptHash retiredOperatorsValidator)
            , schedulerAddr = scriptHashAddress (scriptHash schedulerValidator)
            , stateQueueAddr = scriptHashAddress (scriptHash stateQueueValidator)
            , fraudProofCatalogueAddr = scriptHashAddress (scriptHash registeredOperatorsValidator)
            , fraudProofAddr = scriptHashAddress (scriptHash registeredOperatorsValidator)
            , depositAddr = scriptHashAddress (scriptHash registeredOperatorsValidator)
            , withdrawalAddr = scriptHashAddress (scriptHash registeredOperatorsValidator)
            , txOrderAddr = scriptHashAddress (scriptHash registeredOperatorsValidator)
            , settlementAddr = scriptHashAddress (scriptHash registeredOperatorsValidator)
            , reserveAddr = scriptHashAddress (scriptHash registeredOperatorsValidator)
            , payoutAddr = scriptHashAddress (scriptHash registeredOperatorsValidator)
            , reserveObserver = scriptHash registeredOperatorsValidator
            }
          C.NoStakeAddress
          -- For some reason, sc-tools can't balance min-ada. Even 'setMinAdaDepositAll' doesn't work.
          (assetValue hubOracleScriptHash hubOracleAssetName 1)
        initRegisteredOperators netId scripts refScripts
        initActiveOperators netId scripts refScripts
        initRetiredOperators netId scripts refScripts
        initScheduler netId (transPOSIXTime currentTime) scripts
        initStateQueue netId (currentSlot, transPOSIXTime currentTime) scripts
        addBtx $ \txBody ->
          txBody
            { C.txValidityLowerBound = C.TxValidityLowerBound (C.allegraBasedEra @C.ConwayEra) currentSlot
            , C.txValidityUpperBound =
                C.TxValidityUpperBound (C.shelleyBasedEra @C.ConwayEra) $ Just validityUpperBoundExclusive
            }
        setMinAdaDepositAll params
    where
      scriptCurrencySymbol = currencySymbol . policyIdBytes . mintingPolicyId
      scriptHash = ScriptHash . toBuiltin . scriptHashBytes . validatorHash

publishMidgardMintingPolicy ::
  (MonadBlockchain C.ConwayEra m) =>
  TypedScript PlutusV3 '[AsRedeemer redeemer] ->
  m (TxBuilder C.ConwayEra)
publishMidgardMintingPolicy mintingPolicy = do
  netId <- queryNetworkId
  params <- queryProtocolParameters
  pure . execBuildTx $ do
    createRefScriptNoDatum
      (refScriptStorage netId)
      (C.PlutusScript C.plutusScriptVersion $ toMintingPolicy mintingPolicy)
      mempty
    setMinAdaDepositAll params
