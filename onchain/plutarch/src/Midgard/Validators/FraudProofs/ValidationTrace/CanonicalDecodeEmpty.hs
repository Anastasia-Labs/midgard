{- |
Module      : Midgard.Validators.FraudProofs.ValidationTrace.CanonicalDecodeEmpty
Description : Plutarch port of @canonical-decode-empty-semantic-v1.ak@.
-}
module Midgard.Validators.FraudProofs.ValidationTrace.CanonicalDecodeEmpty (
  PCanonicalDecodeEmptyActionV1 (..),
  canonicalDecodeEmptySemanticV1Validator,
) where

import GHC.Generics (Generic)
import Generics.SOP qualified as SOP

import Plutarch.LedgerApi.Utils (PMaybeData (..))
import Plutarch.LedgerApi.V3 (PCurrencySymbol, PScriptContext, PScriptHash)
import Plutarch.Prelude
import Plutarch.Unsafe (punsafeCoerce)

import Midgard.ComputationThread (PStepDatum (..))
import Midgard.ValidationMachine (
  PValidationAuxiliaryWitnessV1 (PNoAuxiliaryWitness),
  PValidationOneStepWitnessV1,
  pverifyCanonicalDecodeEmptySemanticsV1,
 )
import Midgard.ValidationResolution (
  PPreparedValidationResolutionStateV1 (..),
  PValidationResolutionStateV1 (..),
 )
import Midgard.ValidationSemantic (pcontinueWinning)
import Midgard.ValidationTrace (PValidationMachineStateV1, PValidationPhase (PCanonicalDecode))
import Midgard.Validators.FraudProofs.Step (pdispatch, pstep)

-- | Aiken @canonical-decode-empty-semantic-v1.ActionV1@.
data PCanonicalDecodeEmptyActionV1 (s :: S)
  = PVerifyEmpty
      { pcanonicalEmpty'inputIndex :: Term s (PAsData PInteger)
      , pcanonicalEmpty'outputIndex :: Term s (PAsData PInteger)
      , pcanonicalEmpty'transition :: Term s (PAsData PValidationOneStepWitnessV1)
      }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PCanonicalDecodeEmptyActionV1)

psemanticPreState :: forall s.
  Term s (PMaybeData PStepDatum) -> Term s PValidationMachineStateV1
psemanticPreState datum =
  pmatch datum $ \case
    PDNothing -> perror
    PDJust stepDatum ->
      pmatch (pfromData stepDatum) $ \step ->
      pmatch (pstep'data step) $ \case
        PDNothing -> perror
        PDJust stateData ->
          pmatch
            (punsafeCoerce @PPreparedValidationResolutionStateV1 $ pfromData stateData)
            $ \prepared ->
              pmatch (pfromData $ pprepared'resolution prepared) $ \resolution ->
                pfromData $ presolution'preState resolution

-- | Aiken @canonical-decode-empty-semantic-v1.main@.
canonicalDecodeEmptySemanticV1Validator :: forall s.
  Term s
    ( PAsData PScriptHash
        :--> PAsData PCurrencySymbol
        :--> PScriptContext
        :--> PUnit
    )
canonicalDecodeEmptySemanticV1Validator = plam $
  \awardScriptHash computationThreadPolicyId ctx ->
    pstep ctx $ \datum redeemer ownOutRef txInfo ->
      pdispatch @_ @PCanonicalDecodeEmptyActionV1
        computationThreadPolicyId datum redeemer ownOutRef txInfo
        $ \action ->
          pmatch action $ \(PVerifyEmpty inputIndex outputIndex transitionD) ->
          plet (pfromData transitionD) $ \transition ->
          plet (pforgetData $ pdata $ pcon PNoAuxiliaryWitness) $ \auxiliaryData ->
            pcontinueWinning
              (pcon PCanonicalDecode)
              awardScriptHash
              computationThreadPolicyId
              datum
              (pfromData inputIndex)
              (pfromData outputIndex)
              transition
              auxiliaryData
              ( pverifyCanonicalDecodeEmptySemanticsV1
                  # psemanticPreState datum
                  # transition
              )
              ownOutRef
              txInfo
