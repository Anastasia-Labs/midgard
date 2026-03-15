module Midgard.Contracts.RetiredOperators (initRetiredOperators) where

import Cardano.Api qualified as C
import Convex.BuildTx (
  MonadBuildTx,
  addReference,
  assetValue,
  mintPlutusRefWithRedeemerFn,
  payToScriptInlineDatum,
 )

import Midgard.Contracts.Utils (nextOutIx)
import Midgard.ScriptUtils (mintingPolicyId, plutusVersion, validatorHash)
import Midgard.Scripts (
  MidgardRefScripts (MidgardRefScripts, retiredOperatorsPolicyRef),
  MidgardScripts (MidgardScripts, retiredOperatorsPolicy, retiredOperatorsValidator),
 )
import Midgard.Types.LinkedList qualified as LinkedList
import Midgard.Types.RetiredOperators qualified as RetiredOperators

initRetiredOperators ::
  ( C.HasScriptLanguageInEra C.PlutusScriptV3 era
  , MonadBuildTx era m
  , C.IsBabbageBasedEra era
  ) =>
  C.NetworkId ->
  MidgardScripts ->
  MidgardRefScripts ->
  m ()
initRetiredOperators
  netId
  MidgardScripts {retiredOperatorsValidator, retiredOperatorsPolicy}
  MidgardRefScripts {retiredOperatorsPolicyRef} = do
    let C.PolicyId policyId = mintingPolicyId retiredOperatorsPolicy
    addReference retiredOperatorsPolicyRef
    -- The registered operators token should be minted.
    mintPlutusRefWithRedeemerFn
      retiredOperatorsPolicyRef
      (plutusVersion retiredOperatorsPolicy)
      policyId
      (\txBody -> RetiredOperators.Init {outputIndex = toInteger $ nextOutIx txBody})
      RetiredOperators.rootAssetName
      1
    -- And sent to the registered operators validator.
    let datum :: RetiredOperators.Datum =
          LinkedList.Element
            { elementData = LinkedList.Root mempty
            , elementLink = Nothing
            }
    payToScriptInlineDatum
      netId
      (validatorHash retiredOperatorsValidator)
      datum
      C.NoStakeAddress
      (assetValue policyId RetiredOperators.rootAssetName 1)
