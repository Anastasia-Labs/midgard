module Midgard.Contracts.RetiredOperators (initRetiredOperators) where

import Cardano.Api qualified as C
import Convex.BuildTx (
  MonadBuildTx,
  assetValue,
  mintPlutusWithRedeemerFn,
  payToScriptInlineDatum,
 )

import Midgard.Contracts.Utils (nextOutIx)
import Midgard.ScriptUtils (mintingPolicyId, toMintingPolicy, validatorHash)
import Midgard.Scripts (MidgardScripts (MidgardScripts, retiredOperatorsPolicy, retiredOperatorsValidator))
import Midgard.Types.LinkedList qualified as LinkedList
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
    mintPlutusWithRedeemerFn
      (toMintingPolicy retiredOperatorsPolicy)
      (\txBody -> RetiredOperators.Init {outputIndex = toInteger $ nextOutIx txBody})
      RetiredOperators.rootKey
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
      -- Must manually add min ada deposit...
      (assetValue policyId RetiredOperators.rootKey 1 <> C.lovelaceToValue 3_000_000)
