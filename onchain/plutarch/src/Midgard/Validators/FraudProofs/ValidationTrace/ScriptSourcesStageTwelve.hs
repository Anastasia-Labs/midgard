{- |
Module      : Midgard.Validators.FraudProofs.ValidationTrace.ScriptSourcesStageTwelve
Description : ScriptSources stage-twelve semantic validators.
-}
module Midgard.Validators.FraudProofs.ValidationTrace.ScriptSourcesStageTwelve (
  PScriptSourcesStageTwelveFinishActionV1 (..),
  PScriptSourcesStageTwelveRedeemerActionV1 (..),
  scriptSourcesStageTwelveFinishSemanticV1Validator,
  scriptSourcesStageTwelveRedeemerSemanticV1Validator,
) where

import GHC.Generics (Generic)
import Generics.SOP qualified as SOP

import Plutarch.LedgerApi.V3 (PCurrencySymbol, PScriptContext, PScriptHash)
import Plutarch.Prelude

import Midgard.CekSelection (bytesList)
import Midgard.ScriptSourcesDescriptor (PDescriptorStepClaim)
import Midgard.ScriptSourcesDescriptorWire (pdecodeClaim)
import Midgard.ScriptSourcesLateRawSemantics qualified as LateRaw
import Midgard.ScriptSourcesRedeemerItemStepYield (redeemerItemStepRole)
import Midgard.ValidationMachine (
  PValidationAuxiliaryWitnessV1 (..),
  PValidationOneStepWitnessV1,
  pdescriptorClaimAuxiliary,
  pverifyScriptSourcesStageTwelveFinishSemanticsV1,
 )
import Midgard.ValidationResolutionData (decodeTransition, integerField)
import Midgard.ValidationSemantic (pcontinueWinning, pvalidationSemanticPreState)
import Midgard.ValidationSemanticYield (prequireSemanticYieldV1)
import Midgard.ValidationTrace (PValidationPhase (PScriptSources))
import Midgard.Validators.FraudProofs.Step (pdispatch, pstep)

data PScriptSourcesStageTwelveFinishActionV1 (s :: S)
  = PVerifyFinish
      (Term s (PAsData PInteger))
      (Term s (PAsData PInteger))
      (Term s (PAsData PValidationOneStepWitnessV1))
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PScriptSourcesStageTwelveFinishActionV1)

data PScriptSourcesStageTwelveRedeemerActionV1 (s :: S)
  = PVerifyRedeemerItemStep
      (Term s (PAsData PInteger))
      (Term s (PAsData PInteger))
      (Term s (PAsData PValidationOneStepWitnessV1))
      (Term s (PAsData PDescriptorStepClaim))
      (Term s (PAsData PInteger))
  | PVerifyRedeemerScanBegin
      (Term s (PAsData PInteger))
      (Term s (PAsData PInteger))
      (Term s (PAsData PValidationOneStepWitnessV1))
      (Term s (PAsData PInteger))
      (Term s (PAsData PInteger))
      (Term s (PAsData PInteger))
      (Term s (PAsData PByteString))
      (Term s (PAsData (PBuiltinList (PAsData PByteString))))
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PScriptSourcesStageTwelveRedeemerActionV1)

scriptSourcesStageTwelveFinishSemanticV1Validator ::
  forall s.
  Term s (PAsData PScriptHash :--> PAsData PCurrencySymbol :--> PScriptContext :--> PUnit)
scriptSourcesStageTwelveFinishSemanticV1Validator = plam $ \awardScriptHash policyId ctx ->
  pstep ctx $ \datum redeemer ownOutRef txInfo ->
    pdispatch @_ @PScriptSourcesStageTwelveFinishActionV1 policyId datum redeemer ownOutRef txInfo $
      \action -> pmatch action $ \(PVerifyFinish inputIndex outputIndex transitionD) ->
        plet (pfromData transitionD) $ \transition ->
          plet (pcon PNoAuxiliaryWitness) $ \auxiliary ->
            pcontinueWinning
              (pcon PScriptSources)
              awardScriptHash
              policyId
              datum
              (pfromData inputIndex)
              (pfromData outputIndex)
              transition
              (pforgetData $ pdata auxiliary)
              (pverifyScriptSourcesStageTwelveFinishSemanticsV1 # pvalidationSemanticPreState datum # transition)
              ownOutRef
              txInfo

scriptSourcesStageTwelveRedeemerSemanticV1Validator ::
  forall s.
  Term s (PAsData PScriptHash :--> PAsData PCurrencySymbol :--> PAsData PCurrencySymbol :--> PScriptContext :--> PUnit)
scriptSourcesStageTwelveRedeemerSemanticV1Validator = plam $ \awardScriptHash policyId authPolicyId ctx ->
  pstep ctx $ \datum redeemer ownOutRef txInfo ->
    pdispatch @_ @PData policyId datum redeemer ownOutRef txInfo $ \rawAction ->
      pmatch (pasConstr # rawAction) $ \(PBuiltinPair tag fields) ->
        pif
          (tag #== 0 #&& plength # fields #== 5)
          ( plet (decodeTransition $ pelemAt # 2 # fields) $ \transition ->
              plet (pdecodeClaim # (pelemAt # 3 # fields)) $ \claim ->
                plet (pdescriptorClaimAuxiliary # claim) $ \auxiliary ->
                  plet
                    ( prequireSemanticYieldV1
                        # txInfo
                        # pfromData authPolicyId
                        # redeemerItemStepRole
                        # pfromData (integerField fields 4)
                    )
                    $ \yieldHash ->
                      pcontinueWinning
                        (pcon PScriptSources)
                        awardScriptHash
                        policyId
                        datum
                        (pfromData $ integerField fields 0)
                        (pfromData $ integerField fields 1)
                        transition
                        (pforgetData $ pdata auxiliary)
                        ( pforgetData
                            (pdata transition)
                            #== pelemAt
                            # 2
                            # fields
                            #&& pforgetData
                              (pdata claim)
                            #== pelemAt
                            # 3
                            # fields
                            #&& plengthBS
                            # pto yieldHash
                            #== 28
                            #&& LateRaw.pverifyDescriptorUsed
                            # pvalidationSemanticPreState datum
                            # transition
                            # claim
                        )
                        ownOutRef
                        txInfo
          )
          ( pif
              (tag #== 1 #&& plength # fields #== 8)
              ( plet (decodeTransition $ pelemAt # 2 # fields) $ \transition ->
                  plet (bytesList $ pelemAt # 7 # fields) $ \siblingsD ->
                    plet
                      ( pcon $
                          PRedeemerScanBeginWitness
                            (integerField fields 3)
                            (integerField fields 4)
                            (integerField fields 5)
                            (pdata $ pasByteStr # (pelemAt # 6 # fields))
                            siblingsD
                      )
                      $ \auxiliary ->
                        pcontinueWinning
                          (pcon PScriptSources)
                          awardScriptHash
                          policyId
                          datum
                          (pfromData $ integerField fields 0)
                          (pfromData $ integerField fields 1)
                          transition
                          (pforgetData $ pdata auxiliary)
                          ( pforgetData
                              (pdata transition)
                              #== pelemAt
                              # 2
                              # fields
                              #&& LateRaw.pverifyDescriptorBegin
                              # 12
                              # pvalidationSemanticPreState datum
                              # transition
                              # pfromData (integerField fields 3)
                              # pfromData (integerField fields 4)
                              # pfromData (integerField fields 5)
                              # (pasByteStr # (pelemAt # 6 # fields))
                              # pfromData siblingsD
                          )
                          ownOutRef
                          txInfo
              )
              perror
          )
