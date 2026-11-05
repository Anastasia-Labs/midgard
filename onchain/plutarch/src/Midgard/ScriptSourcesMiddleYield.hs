{-# LANGUAGE OverloadedStrings #-}

-- | Authentication roles and dispatcher linkage for the eight middle yields.
module Midgard.ScriptSourcesMiddleYield (prole, pstage, pdispatch) where

import Midgard.ValidationResolution
import Midgard.ValidationSemanticYield qualified as Yield
import Midgard.ValidationMachine (PValidationOneStepWitnessV1)
import Midgard.ValidationTrace (PValidationMachineStateV1)
import Plutarch.LedgerApi.V3
import Plutarch.Prelude

prole :: forall s. Term s (PInteger :--> PTokenName)
prole = phoistAcyclic $ plam $ \index ->
  pif (index #>= 0 #&& index #< 8)
    (pcon $ PTokenName $ pelemAt # index # pconstant @(PBuiltinList PByteString)
      [ "V1VtSsStage2AdvanceYield", "V1VtSsStage3ReplayYield", "V1VtSsStage3FinishYield"
      , "V1VtSsStage4BeginYield", "V1VtSsStage4FinishYield", "V1VtSsStage6BeginPolicyYield"
      , "V1VtSsStage6FoldAssetYield", "V1VtSsStage6FinishYield"
      ]) perror

pstage :: forall s. Term s (PInteger :--> PInteger)
pstage = phoistAcyclic $ plam $ \index ->
  pif (index #>= 0 #&& index #< 8) (pelemAt # index # pconstant @(PBuiltinList PInteger) [2, 3, 3, 4, 4, 6, 6, 6]) perror

pdispatch :: forall s. Term s (PBuiltinList (PAsData PScriptHash)) -> Term s PTxInfo -> Term s PInteger -> Term s PInteger ->
  (Term s PValidationMachineStateV1 -> Term s PValidationOneStepWitnessV1 -> Term s (PBuiltinList PData) -> Term s PBool) -> Term s PBool
pdispatch hashes tx role auxiliaryTag semantic =
  pmatch (Yield.puniqueSemanticDispatchV1 # hashes # tx) $ \d ->
  plet (Yield.pdispatch'extra d) $ \extra ->
  pmatch (pasConstr # (pelemAt # 0 # extra)) $ \(PBuiltinPair tag fields) ->
  pmatch (Yield.pdispatch'state d) $ \state -> pmatch (pfromData $ pprepared'resolution state) $ \resolution ->
    pif (plength # extra #== 3 #&& pasInt # (pelemAt # 1 # extra) #== role #&& tag #== auxiliaryTag)
      (semantic (pfromData $ presolution'preState resolution) (Yield.pdispatch'transition d) fields) perror
