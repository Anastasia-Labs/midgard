module Midgard.Contracts.ActiveOperators (initActiveOperators) where

import Cardano.Api qualified as C
import Convex.BuildTx (
  MonadBuildTx,
  assetValue,
  mintPlutus,
  payToScriptInlineDatum,
 )
import Convex.Class (MonadBlockchain (queryNetworkId))

import Midgard.ScriptUtils (mintingPolicyId, toMintingPolicy, validatorHash)
import Midgard.Scripts (MidgardScripts (MidgardScripts, activeOperatorsPolicy, activeOperatorsValidator))
import Midgard.Types.ActiveOperators qualified as ActiveOperators

initActiveOperators :: (MonadBlockchain era m, C.HasScriptLanguageInEra C.PlutusScriptV3 era, MonadBuildTx era m, C.IsBabbageBasedEra era) => MidgardScripts -> m ()
initActiveOperators MidgardScripts {activeOperatorsValidator, activeOperatorsPolicy} = do
  netId <- queryNetworkId
  let C.PolicyId policyId = mintingPolicyId activeOperatorsPolicy
  -- The active operators token should be minted.
  mintPlutus (toMintingPolicy activeOperatorsPolicy) ActiveOperators.Init (C.UnsafeAssetName "") 1
  -- And sent to the active operators validator.
  payToScriptInlineDatum netId (validatorHash activeOperatorsValidator) () C.NoStakeAddress (assetValue policyId ActiveOperators.rootKey 1)
