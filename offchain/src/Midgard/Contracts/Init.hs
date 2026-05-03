module Midgard.Contracts.Init (publishMidgardMintingPolicy, initProtocol) where

import Control.Monad.Except (MonadError)

import Cardano.Api qualified as C
import Convex.BuildTx (
  TxBuilder,
  assetValue,
  createRefScriptNoDatum,
  execBuildTx,
  mintPlutus,
  payToScriptInlineDatum,
  setMinAdaDepositAll,
 )
import Convex.Class (
  MonadBlockchain (
    queryEraHistory,
    queryNetworkId,
    queryProtocolParameters,
    querySlotNo,
    querySystemStart
  ),
 )
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
      eraHistory <- queryEraHistory
      systemStart <- querySystemStart
      (currentSlot, _, _) <- querySlotNo
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
        blockCommitStartTime <- initStateQueue netId eraHistory systemStart currentSlot scripts
        initScheduler netId blockCommitStartTime scripts
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
