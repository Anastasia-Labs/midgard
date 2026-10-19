{- |
Module      : Midgard.Validators.FraudProofs.ValidationTrace.Signatures
Description : Signatures validation-trace validators.

Ports the phase router and four @signatures-*-semantic-v1.ak@ scripts.
-}
module Midgard.Validators.FraudProofs.ValidationTrace.Signatures (
  PSignaturesAdvanceActionV1 (..),
  PSignaturesHandoffActionV1 (..),
  PSignaturesAddressItemActionV1 (..),
  PSignaturesRequiredItemActionV1 (..),
  signaturesV1Validator,
  signaturesAdvanceSemanticV1Validator,
  signaturesHandoffSemanticV1Validator,
  signaturesAddressItemSemanticV1Validator,
  signaturesRequiredItemSemanticV1Validator,
) where

import GHC.Generics (Generic)
import Generics.SOP qualified as SOP

import Plutarch.LedgerApi.Utils (PMaybeData)
import Plutarch.LedgerApi.V3 (
  PCurrencySymbol,
  PScriptContext,
  PScriptHash,
  PTxInfo (..),
  PTxOutRef,
 )
import Plutarch.Prelude

import Midgard.ComputationThread (PStepDatum)
import Midgard.NativeTxFieldAccess (PFieldCarriageV1)
import Midgard.ValidationMachine (
  PSignerSetProofV1,
  PValidationAuxiliaryWitnessV1 (..),
  PValidationOneStepWitnessV1,
  pverifyRequiredSignerItemSemanticsV1,
  pverifySignatureAddressItemSemanticsV1,
  pverifySignaturesAdvanceSemanticsV1,
  pverifySignaturesHandoffSemanticsV1,
 )
import Midgard.ValidationSemantic (pcontinueWinning, pvalidationSemanticPreState)
import Midgard.ValidationTrace (PValidationPhase (PSignatures))
import Midgard.ValidationMachineFieldDoor (PMachineFieldDoorV1 (..))
import Midgard.Validators.FraudProofs.Step (pdispatch, pstep)
import Midgard.Validators.FraudProofs.ValidationTrace.Preparation (
  pprepareSelectedValidator,
 )

data PSignaturesAdvanceActionV1 (s :: S)
  = PVerifyAdvance
      { padvance'inputIndex :: Term s (PAsData PInteger)
      , padvance'outputIndex :: Term s (PAsData PInteger)
      , padvance'transition :: Term s (PAsData PValidationOneStepWitnessV1)
      }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PSignaturesAdvanceActionV1)

data PSignaturesHandoffActionV1 (s :: S)
  = PVerifyHandoff
      { phandoff'inputIndex :: Term s (PAsData PInteger)
      , phandoff'outputIndex :: Term s (PAsData PInteger)
      , phandoff'transition :: Term s (PAsData PValidationOneStepWitnessV1)
      }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PSignaturesHandoffActionV1)

data PSignaturesAddressItemActionV1 (s :: S)
  = PVerifyAddressItem
      { paddressItem'inputIndex :: Term s (PAsData PInteger)
      , paddressItem'outputIndex :: Term s (PAsData PInteger)
      , paddressItem'transition :: Term s (PAsData PValidationOneStepWitnessV1)
      , paddressItem'fieldIndex :: Term s (PAsData PInteger)
      , paddressItem'itemIndex :: Term s (PAsData PInteger)
      , paddressItem'carriage :: Term s (PAsData PFieldCarriageV1)
      }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PSignaturesAddressItemActionV1)

data PSignaturesRequiredItemActionV1 (s :: S)
  = PVerifyRequiredItem
      { prequiredItem'inputIndex :: Term s (PAsData PInteger)
      , prequiredItem'outputIndex :: Term s (PAsData PInteger)
      , prequiredItem'transition :: Term s (PAsData PValidationOneStepWitnessV1)
      , prequiredItem'carriage :: Term s (PAsData PFieldCarriageV1)
      , prequiredItem'signerProof :: Term s (PAsData PSignerSetProofV1)
      }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PSignaturesRequiredItemActionV1)

signaturesV1Validator :: forall s.
  Term s
    ( PAsData (PBuiltinList (PAsData PScriptHash))
        :--> PAsData PCurrencySymbol
        :--> PScriptContext
        :--> PUnit
    )
signaturesV1Validator = pprepareSelectedValidator (pcon PSignatures) 4

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
      (pcon PSignatures)
      awardScriptHash policyId datum inputIndex outputIndex transition
      (pforgetData $ pdata auxiliary)
      isValid ownOutRef txInfo

signaturesAdvanceSemanticV1Validator :: forall s.
  Term s (PAsData PScriptHash :--> PAsData PCurrencySymbol :--> PScriptContext :--> PUnit)
signaturesAdvanceSemanticV1Validator = plam $ \awardScriptHash policyId ctx ->
  pstep ctx $ \datum redeemer ownOutRef txInfo ->
  pdispatch @_ @PSignaturesAdvanceActionV1 policyId datum redeemer ownOutRef txInfo $
    \action -> pmatch action $ \(PVerifyAdvance inputIndex outputIndex transitionD) ->
      plet (pfromData transitionD) $ \transition ->
        pcontinueNoAuxiliary
          awardScriptHash policyId datum
          (pfromData inputIndex) (pfromData outputIndex) transition
          (pverifySignaturesAdvanceSemanticsV1 # pvalidationSemanticPreState datum # transition)
          ownOutRef txInfo

signaturesHandoffSemanticV1Validator :: forall s.
  Term s (PAsData PScriptHash :--> PAsData PCurrencySymbol :--> PScriptContext :--> PUnit)
signaturesHandoffSemanticV1Validator = plam $ \awardScriptHash policyId ctx ->
  pstep ctx $ \datum redeemer ownOutRef txInfo ->
  pdispatch @_ @PSignaturesHandoffActionV1 policyId datum redeemer ownOutRef txInfo $
    \action -> pmatch action $ \(PVerifyHandoff inputIndex outputIndex transitionD) ->
      plet (pfromData transitionD) $ \transition ->
        pcontinueNoAuxiliary
          awardScriptHash policyId datum
          (pfromData inputIndex) (pfromData outputIndex) transition
          (pverifySignaturesHandoffSemanticsV1 # pvalidationSemanticPreState datum # transition)
          ownOutRef txInfo

signaturesAddressItemSemanticV1Validator :: forall s.
  Term s
    ( PAsData PScriptHash :--> PAsData PCurrencySymbol
        :--> PAsData PCurrencySymbol :--> PScriptContext :--> PUnit
    )
signaturesAddressItemSemanticV1Validator = plam $ \awardScriptHash policyId certificatePolicyId ctx ->
  pstep ctx $ \datum redeemer ownOutRef txInfo ->
  pdispatch @_ @PSignaturesAddressItemActionV1 policyId datum redeemer ownOutRef txInfo $
    \action -> pmatch action $ \(PVerifyAddressItem inputIndex outputIndex transitionD fieldIndexD itemIndexD carriageD) ->
      plet (pfromData transitionD) $ \transition ->
      plet (pcon $ PTransactionFieldChunkWitness fieldIndexD itemIndexD carriageD) $ \auxiliary ->
      pmatch txInfo $ \PTxInfo {ptxInfo'referenceInputs} ->
      plet (pcon $ PMachineFieldDoorV1 (pfromData ptxInfo'referenceInputs) certificatePolicyId) $ \door ->
        pcontinueWinning
          (pcon PSignatures) awardScriptHash policyId datum
          (pfromData inputIndex) (pfromData outputIndex) transition
          (pforgetData $ pdata auxiliary)
          ( pverifySignatureAddressItemSemanticsV1
              # pvalidationSemanticPreState datum
              # transition # door # pfromData fieldIndexD # pfromData itemIndexD
              # pfromData carriageD
          )
          ownOutRef txInfo

signaturesRequiredItemSemanticV1Validator :: forall s.
  Term s
    ( PAsData PScriptHash :--> PAsData PCurrencySymbol
        :--> PAsData PCurrencySymbol :--> PScriptContext :--> PUnit
    )
signaturesRequiredItemSemanticV1Validator = plam $ \awardScriptHash policyId certificatePolicyId ctx ->
  pstep ctx $ \datum redeemer ownOutRef txInfo ->
  pdispatch @_ @PSignaturesRequiredItemActionV1 policyId datum redeemer ownOutRef txInfo $
    \action -> pmatch action $ \(PVerifyRequiredItem inputIndex outputIndex transitionD carriageD signerProofD) ->
      plet (pfromData transitionD) $ \transition ->
      plet (pfromData signerProofD) $ \signerProof ->
      plet
        (pcon $ PRequiredSignerItemWitness carriageD signerProofD)
        $ \auxiliary ->
          pmatch txInfo $ \PTxInfo {ptxInfo'referenceInputs} ->
          plet (pcon $ PMachineFieldDoorV1 (pfromData ptxInfo'referenceInputs) certificatePolicyId) $ \door ->
            pcontinueWinning
              (pcon PSignatures) awardScriptHash policyId datum
              (pfromData inputIndex) (pfromData outputIndex) transition
              (pforgetData $ pdata auxiliary)
              ( pverifyRequiredSignerItemSemanticsV1
                  # pvalidationSemanticPreState datum
                  # transition # door # pfromData carriageD # signerProof
              )
              ownOutRef txInfo
