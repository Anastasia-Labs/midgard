module Midgard.Contracts.Init (publishMidgardMintingPolicy, initProtocol) where

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
import Convex.Class (MonadBlockchain (queryNetworkId, queryProtocolParameters))
import PlutusLedgerApi.V1 (currencySymbol)
import Ply (
  PlutusVersion (PlutusV3),
  ScriptParameter (AsRedeemer),
  TypedScript,
 )

import Midgard.Constants (hubOracleAssetName, hubOracleMintingPolicyId', hubOracleMintingScript, hubOracleValidator, refScriptStorage)
import Midgard.Contracts.ActiveOperators (initActiveOperators)
import Midgard.Contracts.RegisteredOperators (initRegisteredOperators)
import Midgard.Contracts.RetiredOperators (initRetiredOperators)
import Midgard.ScriptUtils (mintingPolicyId, policyIdBytes, toMintingPolicy)
import Midgard.Scripts (
  MidgardRefScripts (..),
  MidgardScripts (
    MidgardScripts,
    activeOperatorsPolicy,
    registeredOperatorsPolicy,
    retiredOperatorsPolicy
  ),
 )
import Midgard.Types.HubOracle qualified as HubOracle

initProtocol ::
  forall m.
  (MonadBlockchain C.ConwayEra m) =>
  MidgardScripts ->
  MidgardRefScripts ->
  m (TxBuilder C.ConwayEra)
initProtocol
  scripts@MidgardScripts
    { retiredOperatorsPolicy
    , registeredOperatorsPolicy
    , activeOperatorsPolicy
    }
  refScripts =
    do
      netId <- queryNetworkId
      params <- queryProtocolParameters
      pure . execBuildTx $ do
        -- The hub oracle is required for all initializations.
        -- TODO (chase): The real hub oracle must be parameterized by a nonce UTxO.
        mintPlutus hubOracleMintingScript () hubOracleAssetName 1
        payToScriptInlineDatum
          netId
          (C.hashScript $ C.PlutusScript C.plutusScriptVersion hubOracleValidator)
          HubOracle.Datum
            { retiredOperators = scriptCurrencySymbol retiredOperatorsPolicy
            , activeOperators = scriptCurrencySymbol activeOperatorsPolicy
            , registeredOperators = scriptCurrencySymbol registeredOperatorsPolicy
            }
          C.NoStakeAddress
          -- For some reason, sc-tools can't balance min-ada. Even 'setMinAdaDepositAll' doesn't work.
          (assetValue hubOracleMintingPolicyId' hubOracleAssetName 1)
        initRegisteredOperators netId scripts refScripts
        initActiveOperators netId scripts refScripts
        initRetiredOperators netId scripts refScripts
        setMinAdaDepositAll params
    where
      scriptCurrencySymbol = currencySymbol . policyIdBytes . mintingPolicyId

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
