module Midgard.Contracts.RetiredOperators (initRetiredOperators) where

import Cardano.Api qualified as C
import Convex.BuildTx (
  MonadBuildTx,
  assetValue,
  mintPlutus,
  payToScriptInlineDatum,
 )
import Convex.Class (MonadBlockchain (queryNetworkId))

import Midgard.ScriptUtils (mintingPolicyId, toMintingPolicy, validatorHash)
import Midgard.Scripts (MidgardScripts (MidgardScripts, retiredOperatorsPolicy, retiredOperatorsValidator))
import Midgard.Types.RegisteredOperators qualified as RegisteredOperators

initRetiredOperators :: (MonadBlockchain era m, C.HasScriptLanguageInEra C.PlutusScriptV3 era, MonadBuildTx era m, C.IsBabbageBasedEra era) => MidgardScripts -> m ()
initRetiredOperators MidgardScripts {retiredOperatorsValidator, retiredOperatorsPolicy} = do
  netId <- queryNetworkId
  let rootAssetName = C.UnsafeAssetName ""
  let C.PolicyId policyId = mintingPolicyId retiredOperatorsPolicy
  -- The registered operators token should be minted.
  mintPlutus (toMintingPolicy retiredOperatorsPolicy) RegisteredOperators.Init (C.UnsafeAssetName "") 1
  -- And sent to the registered operators validator.
  payToScriptInlineDatum netId (validatorHash retiredOperatorsValidator) () C.NoStakeAddress (assetValue policyId rootAssetName 1)
