{- |
Module      : Midgard.Validators.FraudProofs.ValidationTrace.Resolution
Description : Shared aggregate validation-trace resolution.

Ports Aiken @validation_resolver_v1.validate@ for validators whose complete
one-step predicate fits in a single script.
-}
module Midgard.Validators.FraudProofs.ValidationTrace.Resolution (
  PResolveActionV1 (..),
  pvalidateResolutionValidator,
) where

import GHC.Generics (Generic)
import Generics.SOP qualified as SOP

import Plutarch.LedgerApi.V3 (
  PAddress,
  PCurrencySymbol,
  PScriptContext,
  PTxInfo (..),
 )
import Plutarch.Prelude

import Midgard.FraudProofs.Common (pfinalize)
import Midgard.ValidationMachine (
  PValidationOneStepEvidenceV1 (..),
  PValidationOneStepWitnessV1 (..),
 )
import Midgard.ValidationResolution (
  PValidationResolutionStateV1 (..),
  pchallengerWinsWithValidSuccessor,
 )
import Midgard.ValidationTrace (
  PValidationMachineStateV1 (..),
  PValidationPhase,
 )
import Midgard.Validators.FraudProofs.Step (
  pdispatch,
  pexpectDatum,
  pexpectStateAs,
  pstep,
 )

data PResolveActionV1 (s :: S)
  = PResolve
      { presolve'inputIndex :: Term s (PAsData PInteger)
      , presolve'outputIndex :: Term s (PAsData PInteger)
      , presolve'fraudProofMintRedeemerIndex :: Term s (PAsData PInteger)
      , presolve'challengerEvidence :: Term s (PAsData PValidationOneStepEvidenceV1)
      }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PResolveActionV1)

pvalidateResolutionValidator :: forall s.
  Term s PValidationPhase ->
  Term s (PValidationMachineStateV1 :--> PValidationOneStepEvidenceV1 :--> PBool) ->
  Term s
    ( PAsData PCurrencySymbol
        :--> PAsData PCurrencySymbol
        :--> PAsData PAddress
        :--> PScriptContext
        :--> PUnit
    )
pvalidateResolutionValidator expectedPhase verifyOneStep =
  plam $ \computationThreadPolicyId fraudProofPolicyId fraudProofAddress ctx ->
    pstep ctx $ \datum redeemer ownOutRef txInfo ->
    pdispatch @_ @PResolveActionV1 computationThreadPolicyId datum redeemer ownOutRef txInfo $
      \action -> pmatch action $ \(PResolve inputIndex outputIndex mintRedeemerIndex evidenceD) ->
        pmatch txInfo $ \PTxInfo {ptxInfo'inputs, ptxInfo'outputs, ptxInfo'redeemers} ->
        pfinalize
          computationThreadPolicyId
          fraudProofPolicyId
          fraudProofAddress
          (pexpectDatum datum)
          (pfromData inputIndex)
          (pfromData outputIndex)
          (pfromData mintRedeemerIndex)
          ownOutRef
          (pfromData ptxInfo'inputs)
          (pfromData ptxInfo'outputs)
          (pto $ pto $ pfromData ptxInfo'redeemers)
          $ \_inputScriptHash _assetName _fraudProver inputState ->
            plet (pexpectStateAs @PValidationResolutionStateV1 inputState) $ \state ->
            pmatch state $ \stateFields ->
            plet (pfromData $ presolution'preState stateFields) $ \pre ->
            pmatch pre $ \preState ->
            plet (pfromData evidenceD) $ \evidence ->
            pmatch evidence $ \evidenceFields ->
            plet (pfromData $ poneStepEvidence'transition evidenceFields) $ \transition ->
            pmatch transition $ \transitionFields ->
              pfromData (pmachineState'phase preState) #== expectedPhase
                #&& pchallengerWinsWithValidSuccessor
                  # state
                  # pfromData (poneStep'claimedSuccessor transitionFields)
                  # (verifyOneStep # pre # evidence)
