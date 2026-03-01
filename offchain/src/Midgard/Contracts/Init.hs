module Midgard.Contracts.Init (initProtocol) where

import Cardano.Api qualified as C
import Convex.BuildTx (
  MonadBuildTx,
  assetValue,
  mintPlutus,
  payToScriptInlineDatum,
 )

import Midgard.Constants (hubOracleAssetName, hubOracleMintingPolicyId', hubOracleMintingScript, hubOracleValidator)
import Midgard.Contracts.ActiveOperators (initActiveOperators)
import Midgard.Contracts.RegisteredOperators (initRegisteredOperators)
import Midgard.Contracts.RetiredOperators (initRetiredOperators)
import Midgard.ScriptUtils (mintingPolicyId, policyIdBytes)
import Midgard.Scripts
import Midgard.Types.HubOracle qualified as HubOracle
import PlutusLedgerApi.V1 (currencySymbol)

initProtocol ::
  forall era m.
  ( C.IsBabbageBasedEra era
  , C.HasScriptLanguageInEra C.PlutusScriptV3 era
  , MonadBuildTx era m
  ) =>
  C.NetworkId ->
  MidgardScripts ->
  m ()
initProtocol
  netId
  scripts@MidgardScripts
    { retiredOperatorsPolicy
    , registeredOperatorsPolicy
    , activeOperatorsPolicy
    } = do
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
      (assetValue hubOracleMintingPolicyId' hubOracleAssetName 1 <> C.lovelaceToValue 2_000_000)
    initRegisteredOperators netId scripts
    initActiveOperators netId scripts
    initRetiredOperators netId scripts
    where
      scriptCurrencySymbol = currencySymbol . policyIdBytes . mintingPolicyId
