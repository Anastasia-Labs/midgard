{- |
Module      : Midgard.Validators.FraudProofs.ValidationTrace.Preparation
Description : Shared phase-level semantic resolver preparation.

Ports Aiken @validation_resolver_v1.prepare_selected@ for the validation-trace
validator family.
-}
module Midgard.Validators.FraudProofs.ValidationTrace.Preparation (
  PPrepareSelectedActionV1 (..),
  ppreparedSelectionIsValid,
  pprepareSelectedValidator,
) where

import GHC.Generics (Generic)
import Generics.SOP qualified as SOP

import Plutarch.LedgerApi.V3 (
  PCurrencySymbol,
  PScriptContext,
  PScriptHash,
  PTxInfo (..),
 )
import Plutarch.Prelude

import Midgard.FraudProofs.Common (pcontinue)
import Midgard.ValidationMachine (PValidationOneStepWitnessV1)
import Midgard.ValidationResolution (
  PValidationResolutionStateV1 (..),
  pprepareSemanticResolution,
 )
import Midgard.ValidationResolver (pselectSemanticResolver)
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

data PPrepareSelectedActionV1 (s :: S)
  = PPrepareSelected
      { pprepareSelected'inputIndex :: Term s (PAsData PInteger)
      , pprepareSelected'outputIndex :: Term s (PAsData PInteger)
      , pprepareSelected'semanticResolverIndex :: Term s (PAsData PInteger)
      , pprepareSelected'transition :: Term s (PAsData PValidationOneStepWitnessV1)
      , pprepareSelected'auxiliary :: Term s PData
      }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PPrepareSelectedActionV1)

-- | The phase-specific part of Aiken @validation_resolver_v1.prepare_selected@.
ppreparedSelectionIsValid :: forall s.
  Term s PValidationPhase ->
  Term s PInteger ->
  Term s
    ( PBuiltinList (PAsData PScriptHash)
        :--> PValidationResolutionStateV1
        :--> PInteger
        :--> PValidationOneStepWitnessV1
        :--> PData
        :--> PScriptHash
        :--> PData
        :--> PBool
    )
ppreparedSelectionIsValid expectedPhase expectedResolverCount =
  plam $ \resolverHashes state resolverIndex transition auxiliary outputScriptHash outputState ->
    pmatch
      ( pselectSemanticResolver
          # resolverHashes
          # expectedResolverCount
          # resolverIndex
      )
      $ \case
        PNothing -> perror
        PJust resolverScriptHash ->
          pmatch state $ \stateFields ->
          pmatch (pfromData $ presolution'preState stateFields) $ \pre ->
          plet
            ( pforgetData $ pdata $
                pprepareSemanticResolution
                  # state
                  # transition
                  # auxiliary
            )
            $ \expectedOutputState ->
              pfromData (pmachineState'phase pre) #== expectedPhase
                #&& outputScriptHash #== resolverScriptHash
                #&& outputState #== expectedOutputState

pprepareSelectedValidator :: forall s.
  Term s PValidationPhase ->
  Term s PInteger ->
  Term s
    ( PAsData (PBuiltinList (PAsData PScriptHash))
        :--> PAsData PCurrencySymbol
        :--> PScriptContext
        :--> PUnit
    )
pprepareSelectedValidator expectedPhase expectedResolverCount =
  plam $ \resolverHashes policyId ctx ->
    pstep ctx $ \datum redeemer ownOutRef txInfo ->
    pdispatch @_ @PPrepareSelectedActionV1 policyId datum redeemer ownOutRef txInfo $
      \action -> pmatch action $ \(PPrepareSelected inputIndex outputIndex resolverIndex transitionD auxiliary) ->
        pmatch txInfo $ \PTxInfo {ptxInfo'inputs, ptxInfo'outputs} ->
        pcontinue
          policyId
          (pexpectDatum datum)
          (pfromData inputIndex)
          (pfromData outputIndex)
          ownOutRef
          (pfromData ptxInfo'inputs)
          (pfromData ptxInfo'outputs)
          $ \_inputScriptHash _assetName _fraudProver inputState outputScriptHash outputState ->
            ppreparedSelectionIsValid expectedPhase expectedResolverCount
              # pfromData resolverHashes
              # pexpectStateAs @PValidationResolutionStateV1 inputState
              # pfromData resolverIndex
              # pfromData transitionD
              # auxiliary
              # pfromData outputScriptHash
              # outputState
