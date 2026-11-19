{- |
Module      : Midgard.Validators.FraudProofs.ValidationTrace.CanonicalDecodePrepare
Description : Plutarch port of @canonical-decode-v1.ak@.
-}
module Midgard.Validators.FraudProofs.ValidationTrace.CanonicalDecodePrepare (
  PCanonicalDecodePrepareActionV1 (..),
  canonicalDecodeV1Validator,
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
import Midgard.FraudProofs.Common (pcontinue)
import Midgard.ValidationMachine (
  PValidationAuxiliaryWitnessV1 (PNoAuxiliaryWitness),
  PValidationOneStepWitnessV1 (..),
  pstructuralTransitionIsValid,
 )
import Midgard.ValidationResolution (
  PPreparedValidationResolutionStateV1 (..),
  PValidationResolutionStateV1 (..),
  phashOneStepEvidence,
  ppreparedResolutionVersion,
  pvalidationResolutionStateIsWellFormed,
 )
import Midgard.ValidationResolver (pselectSemanticResolver)
import Midgard.ValidationTrace (
  PValidationMachineStateV1 (..),
  PValidationPhase (PCanonicalDecode),
  phashMachineState,
 )
import Midgard.Validators.FraudProofs.Step (
  pdispatch,
  pexpectDatum,
  pexpectStateAs,
  pstep,
 )

data PCanonicalDecodePrepareActionV1 (s :: S)
  = PPrepareSelected
  { pprepareSelected'inputIndex :: Term s (PAsData PInteger)
  , pprepareSelected'outputIndex :: Term s (PAsData PInteger)
  , pprepareSelected'semanticResolverIndex :: Term s (PAsData PInteger)
  , pprepareSelected'transition :: Term s (PAsData PValidationOneStepWitnessV1)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PCanonicalDecodePrepareActionV1)

pprepareSelected ::
  forall s.
  Term s (PAsData (PBuiltinList (PAsData PScriptHash))) ->
  Term s (PAsData PCurrencySymbol) ->
  Term s (PMaybeData PStepDatum) ->
  Term s PInteger ->
  Term s PInteger ->
  Term s PInteger ->
  Term s PValidationOneStepWitnessV1 ->
  Term s PData ->
  Term s PTxOutRef ->
  Term s PTxInfo ->
  Term s PBool
pprepareSelected resolverHashes policyId datum inputIndex outputIndex resolverIndex transition transitionD ownOutRef txInfo =
  pmatch
    (pselectSemanticResolver # pfromData resolverHashes # 2 # resolverIndex)
    $ \case
      PNothing -> perror
      PJust resolverScriptHash ->
        pmatch txInfo $ \PTxInfo{ptxInfo'inputs, ptxInfo'outputs} ->
          pcontinue
            policyId
            (pexpectDatum datum)
            inputIndex
            outputIndex
            ownOutRef
            (pfromData ptxInfo'inputs)
            (pfromData ptxInfo'outputs)
            $ \_inputScriptHash _assetName _fraudProver inputState outputScriptHash outputState ->
              plet (pexpectStateAs @PValidationResolutionStateV1 inputState) $ \state ->
                pmatch state $ \stateFields ->
                  plet (pfromData $ presolution'preState stateFields) $ \pre ->
                    pmatch pre $ \preFields ->
                      pmatch transition $ \transitionFields ->
                        plet
                          ( phashOneStepEvidence
                              # transitionD
                              # pforgetData (pdata $ pcon PNoAuxiliaryWitness)
                          )
                          $ \evidenceHash ->
                            plet
                              ( pcon $
                                  PPreparedValidationResolutionStateV1
                                    (pdata ppreparedResolutionVersion)
                                    (pdata state)
                                    (pdata evidenceHash)
                              )
                              $ \expectedOutputState ->
                                pvalidationResolutionStateIsWellFormed
                                  # state
                                  #&& phashMachineState
                                  # pfromData (poneStep'claimedSuccessor transitionFields)
                                  #== pfromData (presolution'challengerSuccessorHash stateFields)
                                  #&& pstructuralTransitionIsValid
                                  # pre
                                  # transition
                                  #&& pfromData (pmachineState'phase preFields)
                                  #== pcon PCanonicalDecode
                                  #&& pfromData outputScriptHash
                                  #== resolverScriptHash
                                  #&& outputState
                                  #== pforgetData (pdata expectedOutputState)

canonicalDecodeV1Validator ::
  forall s.
  Term
    s
    ( PAsData (PBuiltinList (PAsData PScriptHash))
        :--> PAsData PCurrencySymbol
        :--> PScriptContext
        :--> PUnit
    )
canonicalDecodeV1Validator = plam $ \resolverHashes policyId ctx ->
  pstep ctx $ \datum redeemer ownOutRef txInfo ->
    pdispatch @_ @PCanonicalDecodePrepareActionV1 policyId datum redeemer ownOutRef txInfo $
      \action -> pmatch action $ \case
        PPrepareSelected inputIndex outputIndex resolverIndex transitionD ->
          pprepareSelected
            resolverHashes
            policyId
            datum
            (pfromData inputIndex)
            (pfromData outputIndex)
            (pfromData resolverIndex)
            (pfromData transitionD)
            (pforgetData transitionD)
            ownOutRef
            txInfo
