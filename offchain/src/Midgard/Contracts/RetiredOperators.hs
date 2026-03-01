module Midgard.Contracts.RetiredOperators (initRetiredOperators) where

import Cardano.Api qualified as C
import Convex.BuildTx (
  MonadBuildTx,
  assetValue,
  mintPlutus,
  payToScriptInlineDatum,
 )

import Midgard.ScriptUtils (mintingPolicyId, toMintingPolicy, validatorHash)
import Midgard.Scripts (MidgardScripts (MidgardScripts, retiredOperatorsPolicy, retiredOperatorsValidator))
import Midgard.Types.RetiredOperators qualified as RetiredOperators

initRetiredOperators ::
  ( C.HasScriptLanguageInEra C.PlutusScriptV3 era
  , MonadBuildTx era m
  , C.IsBabbageBasedEra era
  ) =>
  C.NetworkId ->
  MidgardScripts ->
  m ()
initRetiredOperators
  netId
  MidgardScripts {retiredOperatorsValidator, retiredOperatorsPolicy} = do
    let C.PolicyId policyId = mintingPolicyId retiredOperatorsPolicy
    -- The registered operators token should be minted.
    mintPlutus
      (toMintingPolicy retiredOperatorsPolicy)
      RetiredOperators.Init {outputIndex = 0}
      RetiredOperators.rootKey
      1
    -- And sent to the registered operators validator.
    payToScriptInlineDatum netId (validatorHash retiredOperatorsValidator) () C.NoStakeAddress (assetValue policyId RetiredOperators.rootKey 1)
