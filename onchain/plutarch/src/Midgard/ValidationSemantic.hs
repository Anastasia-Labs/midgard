module Midgard.ValidationSemantic (
  PValidationSemanticDatum,
  pvalidationSemanticPreState,
  psemanticHandoffIsValid,
  pcontinueWinning,
) where

import Plutarch.LedgerApi.Utils (PMaybeData (..))
import Plutarch.LedgerApi.V3 (
  PCurrencySymbol,
  PScriptHash,
  PTxInfo (..),
  PTxOutRef,
 )
import Plutarch.Prelude
import Plutarch.Unsafe (punsafeCoerce)

import Midgard.ComputationThread (PStepDatum (..))
import Midgard.FraudProofs.Common (pcontinue)
import Midgard.ValidationMachine (PValidationOneStepWitnessV1)
import Midgard.ValidationResolution (
  PPreparedValidationResolutionStateV1 (..),
  PValidationResolutionStateV1 (..),
  phashOneStepEvidence,
  ppreparedResolutionIsWellFormed,
  pwinningResolution,
 )
import Midgard.ValidationTrace (PValidationMachineStateV1 (..), PValidationPhase)

type PValidationSemanticDatum = PStepDatum

-- | Aiken validation semantic validators' shared @semantic_pre_state@ helper.
pvalidationSemanticPreState :: forall s.
  Term s (PMaybeData PStepDatum) -> Term s PValidationMachineStateV1
pvalidationSemanticPreState datum =
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

-- | The semantic-evidence checks inside Aiken @continue_winning@.
psemanticHandoffIsValid :: forall s.
  Term
    s
    ( PValidationPhase
        :--> PPreparedValidationResolutionStateV1
        :--> PValidationOneStepWitnessV1
        :--> PData
        :--> PBool
        :--> PScriptHash
        :--> PScriptHash
        :--> PData
        :--> PBool
    )
psemanticHandoffIsValid = phoistAcyclic $
  plam $ \expectedPhase state transition auxiliary semanticTransitionIsValid outputScriptHash awardScriptHash outputState ->
    pmatch state $ \prepared ->
    plet (pfromData $ pprepared'resolution prepared) $ \resolution ->
    pmatch resolution $ \resolved ->
    plet (pfromData $ presolution'preState resolved) $ \pre ->
    pmatch pre $ \preState ->
      ppreparedResolutionIsWellFormed # state
        #&& pfromData (pmachineState'phase preState) #== expectedPhase
        #&& phashOneStepEvidence # pforgetData (pdata transition) # auxiliary
          #== pfromData (pprepared'evidenceHash prepared)
        #&& semanticTransitionIsValid
        #&& outputScriptHash #== awardScriptHash
        #&& outputState #== pforgetData (pdata pwinningResolution)

pcontinueWinning ::
  forall (s :: S).
  Term s PValidationPhase ->
  Term s (PAsData PScriptHash) ->
  Term s (PAsData PCurrencySymbol) ->
  Term s (PMaybeData PStepDatum) ->
  Term s PInteger ->
  Term s PInteger ->
  Term s PValidationOneStepWitnessV1 ->
  Term s PData ->
  Term s PBool ->
  Term s PTxOutRef ->
  Term s PTxInfo ->
  Term s PBool
pcontinueWinning
  expectedPhase
  awardScriptHash
  computationThreadPolicyId
  datum
  inputIndex
  outputIndex
  transition
  auxiliary
  semanticTransitionIsValid
  ownOutRef
  txInfo =
    pmatch txInfo $ \PTxInfo {ptxInfo'inputs, ptxInfo'outputs} ->
    pmatch datum $ \case
      PDNothing -> perror
      PDJust stepDatum ->
        pcontinue
          computationThreadPolicyId
          (pfromData stepDatum)
          inputIndex
          outputIndex
          ownOutRef
          (pfromData ptxInfo'inputs)
          (pfromData ptxInfo'outputs)
          $ \_inputScriptHash _assetName _fraudProver inputState outputScriptHash outputState ->
            pmatch inputState $ \case
              PDNothing -> perror
              PDJust stateData ->
                plet (punsafeCoerce @PPreparedValidationResolutionStateV1 $ pfromData stateData) $ \state ->
                  psemanticHandoffIsValid
                    # expectedPhase
                    # state
                    # transition
                    # auxiliary
                    # semanticTransitionIsValid
                    # pfromData outputScriptHash
                    # pfromData awardScriptHash
                    # outputState
