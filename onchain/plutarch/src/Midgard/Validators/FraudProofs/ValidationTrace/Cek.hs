{- |
Module      : Midgard.Validators.FraudProofs.ValidationTrace.Cek
Description : CEK validation-trace resolution validator.

Ports Aiken @validators/fraud-proofs/validation-trace/cek-v1.ak@ and the
CEK-specific arm of @validation_resolver_v1.validate_cek@.
-}
module Midgard.Validators.FraudProofs.ValidationTrace.Cek (
  PCekResolveActionV1 (..),
  cekV1Validator,
) where

import GHC.Generics (Generic)
import Generics.SOP qualified as SOP

import Plutarch.LedgerApi.V3 (
  PAddress,
  PCurrencySymbol,
  PScriptContext,
  PScriptHash,
  PTxInfo (..),
 )
import Plutarch.Prelude

import Midgard.FraudProofs.Common (pfinalize)
import Midgard.ValidationMachine (
  PValidationOneStepEvidenceV1 (..),
  PValidationOneStepWitnessV1 (..),
  pverifyCekOneStepV1,
 )
import Midgard.ValidationResolution (
  PValidationResolutionStateV1 (..),
  pchallengerWinsWithValidSuccessor,
 )
import Midgard.ValidationResolver (
  PCekMaterialRouteV1,
  pverifyCekRouteV1,
 )
import Midgard.ValidationTrace (
  PValidationMachineStateV1 (..),
  PValidationPhase (PCek),
 )
import Midgard.Validators.FraudProofs.Step (
  pdispatch,
  pexpectDatum,
  pexpectStateAs,
  pstep,
 )

data PCekResolveActionV1 (s :: S)
  = PResolveCek
      { pcekResolve'inputIndex :: Term s (PAsData PInteger)
      , pcekResolve'outputIndex :: Term s (PAsData PInteger)
      , pcekResolve'fraudProofMintRedeemerIndex :: Term s (PAsData PInteger)
      , pcekResolve'challengerEvidence :: Term s (PAsData PValidationOneStepEvidenceV1)
      , pcekResolve'materialRoute :: Term s (PAsData PCekMaterialRouteV1)
      }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PCekResolveActionV1)

cekV1Validator ::
  forall (s :: S).
  Term
    s
    ( PAsData PCurrencySymbol
        :--> PAsData PCurrencySymbol
        :--> PAsData PAddress
        :--> PAsData PScriptHash
        :--> PScriptContext
        :--> PUnit
    )
cekV1Validator =
  plam $ \computationThreadPolicyId fraudProofPolicyId fraudProofAddress materialScriptHash ctx ->
    pstep ctx $ \datum redeemer ownOutRef txInfo ->
    pdispatch @_ @PCekResolveActionV1 computationThreadPolicyId datum redeemer ownOutRef txInfo $
      \action -> pmatch action $ \(PResolveCek inputIndex outputIndex mintRedeemerIndex evidenceD materialRouteD) ->
        pmatch txInfo $ \PTxInfo {ptxInfo'inputs, ptxInfo'referenceInputs, ptxInfo'outputs, ptxInfo'redeemers} ->
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
              pfromData (pmachineState'phase preState) #== pcon PCek
                #&& pverifyCekRouteV1
                  # evidence
                  # pfromData materialRouteD
                  # pfromData ptxInfo'referenceInputs
                  # materialScriptHash
                #&& pchallengerWinsWithValidSuccessor
                  # state
                  # pfromData (poneStep'claimedSuccessor transitionFields)
                  # (pverifyCekOneStepV1 # pre # evidence)
