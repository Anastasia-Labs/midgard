{- |
Module      : Midgard.Validators.FraudProofs.ValidationTrace.ResolveInputs
Description : ResolveInputs validation-trace validators.
-}
module Midgard.Validators.FraudProofs.ValidationTrace.ResolveInputs (
  PResolveInputsInitialActionV1 (..),
  PResolveInputsFinishActionV1 (..),
  PResolveInputsMembershipBeginActionV1 (..),
  PResolveInputsMembershipStepActionV1 (..),
  PResolveInputsMembershipFinalizeActionV1 (..),
  PResolveInputsNonMembershipActionV1 (..),
  resolveInputsV1Validator,
  resolveInputsInitialSemanticV1Validator,
  resolveInputsFinishSemanticV1Validator,
  resolveInputsMembershipBeginSemanticV1Validator,
  resolveInputsMembershipStepSemanticV1Validator,
  resolveInputsMembershipFinalizeSemanticV1Validator,
  resolveInputsNonMembershipSemanticV1Validator,
) where

import GHC.Generics (Generic)
import Generics.SOP qualified as SOP

import Plutarch.LedgerApi.Utils (PMaybeData)
import Plutarch.LedgerApi.V3 (
  PCurrencySymbol,
  PScriptContext,
  PScriptHash,
  PTxInfo,
  PTxOutRef,
 )
import Plutarch.Prelude

import Midgard.ComputationThread (PStepDatum)
import Midgard.LedgerOutputProof (PLedgerOutputProofWitnessV1)
import Midgard.MpfProof.Types (PProof)
import Midgard.ValidationMachine (
  PSignerSetProofV1,
  PValidationAuxiliaryWitnessV1 (..),
  PValidationOneStepWitnessV1,
  pverifyResolveInputsFinishSemanticsV1,
  pverifyResolveInputsInitialSemanticsV1,
  pverifyResolveInputsMembershipBeginSemanticsV1,
  pverifyResolveInputsMembershipFinalizeSemanticsV1,
  pverifyResolveInputsMembershipStepSemanticsV1,
  pverifyResolveInputsNonMembershipSemanticsV1,
 )
import Midgard.ValidationSemantic (pcontinueWinning, pvalidationSemanticPreState)
import Midgard.ValidationTrace (PValidationPhase (PResolveInputs))
import Midgard.Validators.FraudProofs.Step (pdispatch, pstep)
import Midgard.Validators.FraudProofs.ValidationTrace.Preparation (
  pprepareSelectedValidator,
 )

data PResolveInputsInitialActionV1 (s :: S)
  = PVerifyInitial
      { pinitial'inputIndex :: Term s (PAsData PInteger)
      , pinitial'outputIndex :: Term s (PAsData PInteger)
      , pinitial'transition :: Term s (PAsData PValidationOneStepWitnessV1)
      }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PResolveInputsInitialActionV1)

data PResolveInputsFinishActionV1 (s :: S)
  = PVerifyFinish
      { pfinish'inputIndex :: Term s (PAsData PInteger)
      , pfinish'outputIndex :: Term s (PAsData PInteger)
      , pfinish'transition :: Term s (PAsData PValidationOneStepWitnessV1)
      }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PResolveInputsFinishActionV1)

data PResolveInputsMembershipBeginActionV1 (s :: S)
  = PVerifyMembershipBegin
      { pmembershipBegin'inputIndex :: Term s (PAsData PInteger)
      , pmembershipBegin'outputIndex :: Term s (PAsData PInteger)
      , pmembershipBegin'transition :: Term s (PAsData PValidationOneStepWitnessV1)
      , pmembershipBegin'sourceKind :: Term s (PAsData PInteger)
      , pmembershipBegin'key :: Term s (PAsData PByteString)
      , pmembershipBegin'nextScheduleHash :: Term s (PAsData PByteString)
      , pmembershipBegin'descriptorCbor :: Term s (PAsData PByteString)
      , pmembershipBegin'proof :: Term s (PAsData PProof)
      , pmembershipBegin'signerProof :: Term s (PAsData PSignerSetProofV1)
      }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PResolveInputsMembershipBeginActionV1)

data PResolveInputsMembershipStepActionV1 (s :: S)
  = PVerifyMembershipStep
      { pmembershipStep'inputIndex :: Term s (PAsData PInteger)
      , pmembershipStep'outputIndex :: Term s (PAsData PInteger)
      , pmembershipStep'transition :: Term s (PAsData PValidationOneStepWitnessV1)
      , pmembershipStep'proofWitness :: Term s (PAsData PLedgerOutputProofWitnessV1)
      }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PResolveInputsMembershipStepActionV1)

data PResolveInputsMembershipFinalizeActionV1 (s :: S)
  = PVerifyMembershipFinalize
      { pmembershipFinalize'inputIndex :: Term s (PAsData PInteger)
      , pmembershipFinalize'outputIndex :: Term s (PAsData PInteger)
      , pmembershipFinalize'transition :: Term s (PAsData PValidationOneStepWitnessV1)
      , pmembershipFinalize'descriptorCbor :: Term s (PAsData PByteString)
      , pmembershipFinalize'signerProof :: Term s (PAsData PSignerSetProofV1)
      }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PResolveInputsMembershipFinalizeActionV1)

data PResolveInputsNonMembershipActionV1 (s :: S)
  = PVerifyNonMembership
      { pnonMembership'inputIndex :: Term s (PAsData PInteger)
      , pnonMembership'outputIndex :: Term s (PAsData PInteger)
      , pnonMembership'transition :: Term s (PAsData PValidationOneStepWitnessV1)
      , pnonMembership'sourceKind :: Term s (PAsData PInteger)
      , pnonMembership'key :: Term s (PAsData PByteString)
      , pnonMembership'nextScheduleHash :: Term s (PAsData PByteString)
      , pnonMembership'proof :: Term s (PAsData PProof)
      }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PResolveInputsNonMembershipActionV1)

resolveInputsV1Validator :: forall s.
  Term s
    ( PAsData (PBuiltinList (PAsData PScriptHash))
        :--> PAsData PCurrencySymbol
        :--> PScriptContext
        :--> PUnit
    )
resolveInputsV1Validator = pprepareSelectedValidator (pcon PResolveInputs) 6

pcontinueNoAuxiliary :: forall s.
  Term s (PAsData PScriptHash) ->
  Term s (PAsData PCurrencySymbol) ->
  Term s (PMaybeData PStepDatum) ->
  Term s PInteger ->
  Term s PInteger ->
  Term s PValidationOneStepWitnessV1 ->
  Term s PBool ->
  Term s PTxOutRef ->
  Term s PTxInfo ->
  Term s PBool
pcontinueNoAuxiliary awardScriptHash policyId datum inputIndex outputIndex transition isValid ownOutRef txInfo =
  plet (pcon PNoAuxiliaryWitness) $ \auxiliary ->
    pcontinueWinning
      (pcon PResolveInputs)
      awardScriptHash policyId datum inputIndex outputIndex transition
      (pforgetData $ pdata auxiliary)
      isValid ownOutRef txInfo

resolveInputsInitialSemanticV1Validator :: forall s.
  Term s (PAsData PScriptHash :--> PAsData PCurrencySymbol :--> PScriptContext :--> PUnit)
resolveInputsInitialSemanticV1Validator = plam $ \awardScriptHash policyId ctx ->
  pstep ctx $ \datum redeemer ownOutRef txInfo ->
  pdispatch @_ @PResolveInputsInitialActionV1 policyId datum redeemer ownOutRef txInfo $
    \action -> pmatch action $ \(PVerifyInitial inputIndex outputIndex transitionD) ->
      plet (pfromData transitionD) $ \transition ->
        pcontinueNoAuxiliary
          awardScriptHash policyId datum
          (pfromData inputIndex) (pfromData outputIndex) transition
          (pverifyResolveInputsInitialSemanticsV1 # pvalidationSemanticPreState datum # transition)
          ownOutRef txInfo

resolveInputsFinishSemanticV1Validator :: forall s.
  Term s (PAsData PScriptHash :--> PAsData PCurrencySymbol :--> PScriptContext :--> PUnit)
resolveInputsFinishSemanticV1Validator = plam $ \awardScriptHash policyId ctx ->
  pstep ctx $ \datum redeemer ownOutRef txInfo ->
  pdispatch @_ @PResolveInputsFinishActionV1 policyId datum redeemer ownOutRef txInfo $
    \action -> pmatch action $ \(PVerifyFinish inputIndex outputIndex transitionD) ->
      plet (pfromData transitionD) $ \transition ->
        pcontinueNoAuxiliary
          awardScriptHash policyId datum
          (pfromData inputIndex) (pfromData outputIndex) transition
          (pverifyResolveInputsFinishSemanticsV1 # pvalidationSemanticPreState datum # transition)
          ownOutRef txInfo

resolveInputsMembershipBeginSemanticV1Validator :: forall s.
  Term s (PAsData PScriptHash :--> PAsData PCurrencySymbol :--> PScriptContext :--> PUnit)
resolveInputsMembershipBeginSemanticV1Validator = plam $ \awardScriptHash policyId ctx ->
  pstep ctx $ \datum redeemer ownOutRef txInfo ->
  pdispatch @_ @PResolveInputsMembershipBeginActionV1 policyId datum redeemer ownOutRef txInfo $
    \action -> pmatch action $ \(PVerifyMembershipBegin inputIndex outputIndex transitionD sourceKindD keyD nextScheduleHashD descriptorCborD proofD signerProofD) ->
      plet (pfromData transitionD) $ \transition ->
      plet
        ( pcon $
            PScheduledLedgerMembershipWitness
              sourceKindD keyD nextScheduleHashD descriptorCborD proofD signerProofD
        )
        $ \auxiliary ->
          pcontinueWinning
            (pcon PResolveInputs)
            awardScriptHash policyId datum
            (pfromData inputIndex) (pfromData outputIndex) transition
            (pforgetData $ pdata auxiliary)
            ( pverifyResolveInputsMembershipBeginSemanticsV1
                # pvalidationSemanticPreState datum # transition
                # pfromData sourceKindD # pfromData keyD
                # pfromData nextScheduleHashD # pfromData descriptorCborD
                # pfromData proofD # pfromData signerProofD
            )
            ownOutRef txInfo

resolveInputsMembershipStepSemanticV1Validator :: forall s.
  Term s (PAsData PScriptHash :--> PAsData PCurrencySymbol :--> PScriptContext :--> PUnit)
resolveInputsMembershipStepSemanticV1Validator = plam $ \awardScriptHash policyId ctx ->
  pstep ctx $ \datum redeemer ownOutRef txInfo ->
  pdispatch @_ @PResolveInputsMembershipStepActionV1 policyId datum redeemer ownOutRef txInfo $
    \action -> pmatch action $ \(PVerifyMembershipStep inputIndex outputIndex transitionD proofWitnessD) ->
      plet (pfromData transitionD) $ \transition ->
      plet (pcon $ PLedgerOutputProofStepWitness proofWitnessD) $ \auxiliary ->
        pcontinueWinning
          (pcon PResolveInputs)
          awardScriptHash policyId datum
          (pfromData inputIndex) (pfromData outputIndex) transition
          (pforgetData $ pdata auxiliary)
          ( pverifyResolveInputsMembershipStepSemanticsV1
              # pvalidationSemanticPreState datum # transition
              # pfromData proofWitnessD
          )
          ownOutRef txInfo

resolveInputsMembershipFinalizeSemanticV1Validator :: forall s.
  Term s (PAsData PScriptHash :--> PAsData PCurrencySymbol :--> PScriptContext :--> PUnit)
resolveInputsMembershipFinalizeSemanticV1Validator = plam $ \awardScriptHash policyId ctx ->
  pstep ctx $ \datum redeemer ownOutRef txInfo ->
  pdispatch @_ @PResolveInputsMembershipFinalizeActionV1 policyId datum redeemer ownOutRef txInfo $
    \action -> pmatch action $ \(PVerifyMembershipFinalize inputIndex outputIndex transitionD descriptorCborD signerProofD) ->
      plet (pfromData transitionD) $ \transition ->
      plet (pcon $ PLedgerOutputProofFinalizeWitness descriptorCborD signerProofD) $ \auxiliary ->
        pcontinueWinning
          (pcon PResolveInputs)
          awardScriptHash policyId datum
          (pfromData inputIndex) (pfromData outputIndex) transition
          (pforgetData $ pdata auxiliary)
          ( pverifyResolveInputsMembershipFinalizeSemanticsV1
              # pvalidationSemanticPreState datum # transition
              # pfromData descriptorCborD # pfromData signerProofD
          )
          ownOutRef txInfo

resolveInputsNonMembershipSemanticV1Validator :: forall s.
  Term s (PAsData PScriptHash :--> PAsData PCurrencySymbol :--> PScriptContext :--> PUnit)
resolveInputsNonMembershipSemanticV1Validator = plam $ \awardScriptHash policyId ctx ->
  pstep ctx $ \datum redeemer ownOutRef txInfo ->
  pdispatch @_ @PResolveInputsNonMembershipActionV1 policyId datum redeemer ownOutRef txInfo $
    \action -> pmatch action $ \(PVerifyNonMembership inputIndex outputIndex transitionD sourceKindD keyD nextScheduleHashD proofD) ->
      plet (pfromData transitionD) $ \transition ->
      plet
        (pcon $ PScheduledLedgerNonMembershipWitness sourceKindD keyD nextScheduleHashD proofD)
        $ \auxiliary ->
          pcontinueWinning
            (pcon PResolveInputs)
            awardScriptHash policyId datum
            (pfromData inputIndex) (pfromData outputIndex) transition
            (pforgetData $ pdata auxiliary)
            ( pverifyResolveInputsNonMembershipSemanticsV1
                # pvalidationSemanticPreState datum # transition
                # pfromData sourceKindD # pfromData keyD
                # pfromData nextScheduleHashD # pfromData proofD
            )
            ownOutRef txInfo
