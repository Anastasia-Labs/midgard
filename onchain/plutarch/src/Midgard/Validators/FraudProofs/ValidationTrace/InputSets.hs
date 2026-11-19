{- |
Module      : Midgard.Validators.FraudProofs.ValidationTrace.InputSets
Description : InputSets validation-trace validators.

Ports @input-sets-v1.ak@, @input-sets-empty-semantic-v1.ak@, and
@input-sets-item-semantic-v1.ak@.
-}
module Midgard.Validators.FraudProofs.ValidationTrace.InputSets (
  PVerifyInputSetsEmptyActionV1 (..),
  PVerifyInputSetsItemActionV1 (..),
  inputSetsV1Validator,
  inputSetsEmptySemanticV1Validator,
  inputSetsItemSemanticV1Validator,
) where

import GHC.Generics (Generic)
import Generics.SOP qualified as SOP

import Plutarch.LedgerApi.V3 (PCurrencySymbol, PScriptContext, PScriptHash, PTxInfo (..))
import Plutarch.Prelude

import Midgard.NativeTxFieldAccess (PFieldCarriageV1)
import Midgard.ValidationMachine (
  PValidationAuxiliaryWitnessV1 (..),
  PValidationOneStepWitnessV1,
  pverifyInputSetsEmptySemanticsV1,
  pverifyInputSetsItemSemanticsV1,
 )
import Midgard.ValidationSemantic (pcontinueWinning, pvalidationSemanticPreState)
import Midgard.ValidationTrace (PValidationPhase (PInputSets))
import Midgard.ValidationMachineFieldDoor (PMachineFieldDoorV1 (..))
import Midgard.Validators.FraudProofs.Step (pdispatch, pstep)
import Midgard.Validators.FraudProofs.ValidationTrace.Preparation (
  pprepareSelectedValidator,
 )

data PVerifyInputSetsEmptyActionV1 (s :: S)
  = PVerifyEmpty
      { pverifyEmpty'inputIndex :: Term s (PAsData PInteger)
      , pverifyEmpty'outputIndex :: Term s (PAsData PInteger)
      , pverifyEmpty'transition :: Term s (PAsData PValidationOneStepWitnessV1)
      }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PVerifyInputSetsEmptyActionV1)

data PVerifyInputSetsItemActionV1 (s :: S)
  = PVerifyItem
      { pverifyItem'inputIndex :: Term s (PAsData PInteger)
      , pverifyItem'outputIndex :: Term s (PAsData PInteger)
      , pverifyItem'transition :: Term s (PAsData PValidationOneStepWitnessV1)
      , pverifyItem'fieldIndex :: Term s (PAsData PInteger)
      , pverifyItem'itemIndex :: Term s (PAsData PInteger)
      , pverifyItem'carriage :: Term s (PAsData PFieldCarriageV1)
      }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PVerifyInputSetsItemActionV1)

inputSetsV1Validator :: forall s.
  Term s
    ( PAsData (PBuiltinList (PAsData PScriptHash))
        :--> PAsData PCurrencySymbol
        :--> PScriptContext
        :--> PUnit
    )
inputSetsV1Validator = pprepareSelectedValidator (pcon PInputSets) 2

inputSetsEmptySemanticV1Validator :: forall s.
  Term s
    ( PAsData PScriptHash
        :--> PAsData PCurrencySymbol
        :--> PScriptContext
        :--> PUnit
    )
inputSetsEmptySemanticV1Validator = plam $ \awardScriptHash policyId ctx ->
  pstep ctx $ \datum redeemer ownOutRef txInfo ->
  pdispatch @_ @PVerifyInputSetsEmptyActionV1 policyId datum redeemer ownOutRef txInfo $
    \action -> pmatch action $ \(PVerifyEmpty inputIndex outputIndex transitionD) ->
      plet (pfromData transitionD) $ \transition ->
      plet (pcon PNoAuxiliaryWitness) $ \auxiliary ->
        pcontinueWinning
          (pcon PInputSets)
          awardScriptHash
          policyId
          datum
          (pfromData inputIndex)
          (pfromData outputIndex)
          transition
          (pforgetData $ pdata auxiliary)
          (pverifyInputSetsEmptySemanticsV1 # pvalidationSemanticPreState datum # transition)
          ownOutRef
          txInfo

inputSetsItemSemanticV1Validator :: forall s.
  Term s
    ( PAsData PScriptHash
        :--> PAsData PCurrencySymbol
        :--> PAsData PCurrencySymbol
        :--> PScriptContext
        :--> PUnit
    )
inputSetsItemSemanticV1Validator = plam $ \awardScriptHash policyId certificatePolicyId ctx ->
  pstep ctx $ \datum redeemer ownOutRef txInfo ->
  pdispatch @_ @PVerifyInputSetsItemActionV1 policyId datum redeemer ownOutRef txInfo $
    \action -> pmatch action $ \(PVerifyItem inputIndex outputIndex transitionD fieldIndexD itemIndexD carriageD) ->
      plet (pfromData transitionD) $ \transition ->
      plet
        (pcon $ PTransactionFieldChunkWitness fieldIndexD itemIndexD carriageD)
        $ \auxiliary ->
          pmatch txInfo $ \PTxInfo {ptxInfo'referenceInputs} ->
            plet
              (pcon $ PMachineFieldDoorV1 (pfromData ptxInfo'referenceInputs) certificatePolicyId)
              $ \door ->
                pcontinueWinning
                  (pcon PInputSets)
                  awardScriptHash
                  policyId
                  datum
                  (pfromData inputIndex)
                  (pfromData outputIndex)
                  transition
                  (pforgetData $ pdata auxiliary)
                  ( pverifyInputSetsItemSemanticsV1
                      # pvalidationSemanticPreState datum
                      # transition
                      # door
                      # pfromData fieldIndexD
                      # pfromData itemIndexD
                      # pfromData carriageD
                  )
                  ownOutRef
                  txInfo
