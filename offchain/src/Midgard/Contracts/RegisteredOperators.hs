module Midgard.Contracts.RegisteredOperators (initRegisteredOperators) where

import Cardano.Api qualified as C
import Convex.BuildTx (
  MonadBuildTx,
  assetValue,
  mintPlutus,
  payToScriptInlineDatum,
 )
import Convex.Class (MonadBlockchain (queryNetworkId))

import Midgard.ScriptUtils (mintingPolicyId, toMintingPolicy, validatorHash)
import Midgard.Scripts (MidgardScripts (MidgardScripts, registeredOperatorsPolicy, registeredOperatorsValidator))
import Midgard.Types.RegisteredOperators qualified as RegisteredOperators

initRegisteredOperators :: (MonadBlockchain era m, C.HasScriptLanguageInEra C.PlutusScriptV3 era, MonadBuildTx era m, C.IsBabbageBasedEra era) => MidgardScripts -> m ()
initRegisteredOperators MidgardScripts {registeredOperatorsValidator, registeredOperatorsPolicy} = do
  netId <- queryNetworkId
  let rootAssetName = C.UnsafeAssetName ""
  let C.PolicyId policyId = mintingPolicyId registeredOperatorsPolicy
  mintPlutus (toMintingPolicy registeredOperatorsPolicy) RegisteredOperators.Init (C.UnsafeAssetName "") 1
  payToScriptInlineDatum netId (validatorHash registeredOperatorsValidator) () C.NoStakeAddress (assetValue policyId rootAssetName 1)
