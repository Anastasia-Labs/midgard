module Midgard.Contracts.Init (initProtocol) where

import Cardano.Api qualified as C
import Convex.BuildTx (
  MonadBuildTx,
  assetValue,
  mintPlutus,
  payToScriptInlineDatum,
 )
import Convex.Class (MonadBlockchain (queryNetworkId))

import Midgard.Constants (hubOracleAssetName, hubOracleMintingPolicyId', hubOracleMintingScript, hubOracleValidator)
import Midgard.Contracts.ActiveOperators (initActiveOperators)
import Midgard.Contracts.RegisteredOperators (initRegisteredOperators)
import Midgard.Contracts.RetiredOperators (initRetiredOperators)
import Midgard.ScriptUtils (mintingPolicyId, policyIdBytes)
import Midgard.Scripts
import Midgard.Types.HubOracle qualified as HubOracle
import PlutusLedgerApi.V1 (currencySymbol)

initProtocol ::
  ( C.IsBabbageBasedEra era
  , C.HasScriptLanguageInEra C.PlutusScriptV3 era
  , MonadBuildTx era m
  , MonadBlockchain era m
  ) =>
  MidgardScripts -> m ()
initProtocol
  scripts@MidgardScripts
    { retiredOperatorsPolicy
    , registeredOperatorsPolicy
    , activeOperatorsPolicy
    } = do
    netId <- queryNetworkId
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
      (assetValue hubOracleMintingPolicyId' hubOracleAssetName 1)
    initRegisteredOperators scripts
    initActiveOperators scripts
    initRetiredOperators scripts
    where
      scriptCurrencySymbol = currencySymbol . policyIdBytes . mintingPolicyId
