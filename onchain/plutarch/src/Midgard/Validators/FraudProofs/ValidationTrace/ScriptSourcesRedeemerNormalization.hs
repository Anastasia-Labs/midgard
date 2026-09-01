{- |
Module      : Midgard.Validators.FraudProofs.ValidationTrace.ScriptSourcesRedeemerNormalization
Description : Validators for the ScriptSources redeemer-normalization pipeline.
-}
module Midgard.Validators.FraudProofs.ValidationTrace.ScriptSourcesRedeemerNormalization (
  PScriptSourcesRedeemerEnvelopeActionV1 (..),
  PScriptSourcesRedeemerTraversalNormalizerActionV1 (..),
  PScriptSourcesRedeemerOuterNormalizerActionV1 (..),
  PScriptSourcesRedeemerFoldMapExecutorActionV1 (..),
  PScriptSourcesRedeemerFinalizeFrameExecutorActionV1 (..),
  PScriptSourcesRedeemerExecutionSettlementActionV1 (..),
  scriptSourcesRedeemerEnvelopeV1Validator,
  scriptSourcesRedeemerTraversalNormalizerV1Validator,
  scriptSourcesRedeemerOuterNormalizerV1Validator,
  scriptSourcesRedeemerFoldMapExecutorV1Validator,
  scriptSourcesRedeemerFinalizeFrameExecutorV1Validator,
  scriptSourcesRedeemerExecutionSettlementV1Validator,
) where

import GHC.Generics (Generic)
import Generics.SOP qualified as SOP

import Plutarch.Core.Utils (pand'List)
import Plutarch.LedgerApi.Utils (PMaybeData (..))
import Plutarch.LedgerApi.V3 (
  PCurrencySymbol,
  PScriptContext,
  PScriptHash,
  PTxInfo (..),
 )
import Plutarch.Prelude

import Midgard.CekDataTraverse (
  PDataTraverseActionV1 (..),
  PDataTraverseControlV1 (..),
  PFinalizeFrameTransitionV1 (..),
  pcontrolIsWellFormed,
  pencodeControlV1,
  pencodeOptionalSummaryV1,
  pprevalidatedFinalizeFrameTransitionV1,
  pprevalidatedFoldMapNextFrameRootV1,
  pstageFold,
 )
import Midgard.FraudProofs.Common (pcontinue)
import Midgard.FraudProofs.NativeTx.Codec (pcborInt, pencodeDefiniteBytes)
import Midgard.RedeemerItemProof (phashStageDataFromAuthenticatedPrefixV1)
import Midgard.RedeemerItemProof qualified as Redeemer
import Midgard.ScriptSourcesRedeemerNormalization (
  PFamilyTraversalSerializationTemplateV1 (..),
  POuterNormalizedScriptSourcesRedeemerActionV1 (..),
  PPreparedScriptSourcesRedeemerEnvelopeV1 (..),
  PScriptSourcesRedeemerEnvelopeFactsV1 (..),
  PScriptSourcesRedeemerExecutionAttestedStateV1 (..),
  PTraversalNormalizedScriptSourcesRedeemerActionV1 (..),
  PUnvalidatedRedeemerItemOuterFieldsV1 (..),
  pbaseProvenanceIdentityV1,
  pcanonicalActionHashV1,
  pcanonicalAuxiliaryHashV1,
  penvelopeCommitmentV1,
  penvelopeDomain,
  penvelopeStateIsBoundV1,
  pexecutionAttestationSettlementIsExactV1,
  pexecutionAttestedStateV1,
  pfinalizeFrameFamily,
  pfoldMapFamily,
  pnarrowActionIsBoundV1,
  pouterNormalizedDomain,
  pouterNormalizerRouteIsExactV1,
  pouterNormalizedStateIsBoundV1,
  presolutionIdentityV1,
  psemanticExecutorRouteIsExactV1,
  ptraversalActionIdentityV1,
  ptraversalActionIdentityIsBoundV1,
  ptraversalNormalizedDomain,
  ptraversalNormalizedStateIsBoundV1,
  ptraversalNormalizerRouteIsExactV1,
  ptraversalSerializationTemplateV1,
  pverifyRawEnvelopeV1,
  pversion,
 )
import Midgard.ValidationMachine (
  PValidationOneStepWitnessV1,
  pstructuralTransitionIsValid,
 )
import Midgard.ValidationResolution (
  PPreparedValidationResolutionStateV1 (..),
  PValidationResolutionStateV1 (..),
  phashOneStepEvidence,
  ppreparedResolutionIsWellFormed,
 )
import Midgard.ValidationTrace (
  PValidationMachineStateV1 (..),
  PValidationPhase (PScriptSources),
 )
import Midgard.Validators.FraudProofs.Step (
  pdispatch,
  pexpectDatum,
  pexpectStateAs,
  pstep,
 )

data PScriptSourcesRedeemerEnvelopeActionV1 (s :: S)
  = PBindEnvelope
      (Term s (PAsData PInteger))
      (Term s (PAsData PInteger))
      (Term s (PAsData PValidationOneStepWitnessV1))
      (Term s PData)
      (Term s (PAsData PByteString))
      (Term s (PAsData PInteger))
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PScriptSourcesRedeemerEnvelopeActionV1)

data PScriptSourcesRedeemerTraversalNormalizerActionV1 (s :: S)
  = PNormalizeTraversal
      (Term s (PAsData PInteger))
      (Term s (PAsData PInteger))
      (Term s PData)
      (Term s (PAsData Redeemer.PRedeemerItemProofControlV1))
      (Term s (PAsData PDataTraverseActionV1))
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PScriptSourcesRedeemerTraversalNormalizerActionV1)

data PScriptSourcesRedeemerOuterNormalizerActionV1 (s :: S)
  = PNormalizeOuter
      (Term s (PAsData PInteger))
      (Term s (PAsData PInteger))
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PScriptSourcesRedeemerOuterNormalizerActionV1)

data PScriptSourcesRedeemerFoldMapExecutorActionV1 (s :: S)
  = PExecuteFoldMap
      (Term s (PAsData PInteger))
      (Term s (PAsData PInteger))
      (Term s (PAsData PDataTraverseActionV1))
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PScriptSourcesRedeemerFoldMapExecutorActionV1)

data PScriptSourcesRedeemerFinalizeFrameExecutorActionV1 (s :: S)
  = PExecuteFinalizeFrame
      (Term s (PAsData PInteger))
      (Term s (PAsData PInteger))
      (Term s (PAsData PDataTraverseActionV1))
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PScriptSourcesRedeemerFinalizeFrameExecutorActionV1)

data PScriptSourcesRedeemerExecutionSettlementActionV1 (s :: S)
  = PSettleExecution
      (Term s (PAsData PInteger))
      (Term s (PAsData PInteger))
      (Term s (PAsData PPreparedScriptSourcesRedeemerEnvelopeV1))
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PScriptSourcesRedeemerExecutionSettlementActionV1)

scriptSourcesRedeemerEnvelopeV1Validator :: forall s.
  Term s
    ( PAsData PByteString
        :--> PAsData PScriptHash
        :--> PAsData PScriptHash
        :--> PAsData PScriptHash
        :--> PAsData PScriptHash
        :--> PAsData PScriptHash
        :--> PAsData PCurrencySymbol
        :--> PScriptContext
        :--> PUnit
    )
scriptSourcesRedeemerEnvelopeV1Validator = plam $
  \deploymentIdD
   traversalNormalizerD
   outerNormalizerD
   foldMapExecutorD
   finalizeFrameExecutorD
   settlementD
   policyId
   ctx ->
    pstep ctx $ \datum redeemer ownOutRef txInfo ->
    pdispatch @_ @PScriptSourcesRedeemerEnvelopeActionV1 policyId datum redeemer ownOutRef txInfo $
      \action -> pmatch action $ \(PBindEnvelope inputIndex outputIndex transitionD auxiliary expectedNextHashD actionFamilyD) ->
        pmatch txInfo $ \PTxInfo {ptxInfo'inputs, ptxInfo'outputs} ->
        pcontinue
          policyId
          (pexpectDatum datum)
          (pfromData inputIndex)
          (pfromData outputIndex)
          ownOutRef
          (pfromData ptxInfo'inputs)
          (pfromData ptxInfo'outputs)
          $ \inputScriptHash _threadTokenAssetName _fraudProver inputState outputScriptHash outputState ->
            plet (pexpectStateAs @PPreparedValidationResolutionStateV1 inputState) $ \base ->
            pmatch base $ \baseFields ->
            plet (pfromData $ pprepared'resolution baseFields) $ \resolution ->
            pmatch resolution $ \resolutionFields ->
            plet (pfromData $ presolution'preState resolutionFields) $ \pre ->
            pmatch pre $ \preFields ->
            plet (pfromData transitionD) $ \transition ->
            plet (pfromData expectedNextHashD) $ \expectedNextHash ->
            plet (pfromData actionFamilyD) $ \actionFamily ->
            plet (pfromData deploymentIdD) $ \deploymentId ->
            plet (pto $ pfromData inputScriptHash) $ \inputScript ->
            plet (pto $ pfromData outputScriptHash) $ \outputScript ->
            plet (pto $ pfromData traversalNormalizerD) $ \traversalNormalizer ->
            plet (pto $ pfromData outerNormalizerD) $ \outerNormalizer ->
            plet (pto $ pfromData foldMapExecutorD) $ \foldMapExecutor ->
            plet (pto $ pfromData finalizeFrameExecutorD) $ \finalizeFrameExecutor ->
            plet (pto $ pfromData settlementD) $ \settlement ->
            plet
              ( pif
                  (actionFamily #== pfoldMapFamily)
                  foldMapExecutor
                  finalizeFrameExecutor
              )
              $ \semanticExecutor ->
              pmatch
                ( pverifyRawEnvelopeV1
                    # pre # transition # auxiliary # expectedNextHash # actionFamily
                )
                $ \case
                  PNothing -> pconstant False
                  PJust facts ->
                    pmatch facts $ \factFields ->
                    plet (presolutionIdentityV1 # base) $ \resolutionIdentity ->
                    plet
                      ( penvelopeCommitmentV1
                          # deploymentId
                          # pfromData (pprepared'evidenceHash baseFields)
                          # resolutionIdentity
                          # actionFamily
                          # pfromData (penvelopeFacts'canonicalAuxiliaryHash factFields)
                          # pfromData (penvelopeFacts'canonicalActionHash factFields)
                          # pfromData (penvelopeFacts'currentPendingItemControlHash factFields)
                          # expectedNextHash
                          # pfromData (penvelopeFacts'redeemerCount factFields)
                          # pfromData (penvelopeFacts'redeemerTotalCount factFields)
                          # inputScript
                          # traversalNormalizer
                          # outerNormalizer
                          # semanticExecutor
                          # settlement
                          # pfromData (pmachineState'transactionCommitment preFields)
                          # pfromData (pmachineState'validationContextHash preFields)
                      )
                      $ \envelopeCommitment ->
                      plet
                        ( pcon $ PPreparedScriptSourcesRedeemerEnvelopeV1
                            { penvelope'version = pdata pversion
                            , penvelope'domain = pdata penvelopeDomain
                            , penvelope'deploymentId = deploymentIdD
                            , penvelope'base = pdata base
                            , penvelope'resolutionIdentity = pdata resolutionIdentity
                            , penvelope'actionFamily = actionFamilyD
                            , penvelope'canonicalAuxiliaryHash = penvelopeFacts'canonicalAuxiliaryHash factFields
                            , penvelope'canonicalActionHash = penvelopeFacts'canonicalActionHash factFields
                            , penvelope'currentPendingItemControlHash = penvelopeFacts'currentPendingItemControlHash factFields
                            , penvelope'expectedNextItemControlHash = expectedNextHashD
                            , penvelope'redeemerCount = penvelopeFacts'redeemerCount factFields
                            , penvelope'redeemerTotalCount = penvelopeFacts'redeemerTotalCount factFields
                            , penvelope'envelopeBinderScriptHash = pdata inputScript
                            , penvelope'traversalNormalizerScriptHash = pdata traversalNormalizer
                            , penvelope'outerNormalizerScriptHash = pdata outerNormalizer
                            , penvelope'semanticExecutorScriptHash = pdata semanticExecutor
                            , penvelope'settlementScriptHash = pdata settlement
                            , penvelope'envelopeCommitment = pdata envelopeCommitment
                            }
                        )
                        $ \expectedOutputState ->
                          pand'List
                            [ ppreparedResolutionIsWellFormed # base
                            , pfromData (pmachineState'phase preFields) #== pcon PScriptSources
                            , phashOneStepEvidence
                                # pforgetData transitionD # auxiliary
                                #== pfromData (pprepared'evidenceHash baseFields)
                            , pstructuralTransitionIsValid # pre # transition
                            , plengthBS # deploymentId #== 32
                            , plengthBS # inputScript #== 28
                            , plengthBS # traversalNormalizer #== 28
                            , plengthBS # outerNormalizer #== 28
                            , plengthBS # foldMapExecutor #== 28
                            , plengthBS # finalizeFrameExecutor #== 28
                            , plengthBS # settlement #== 28
                            , outputScript #== traversalNormalizer
                            , outputState #== pforgetData (pdata expectedOutputState)
                            ]

scriptSourcesRedeemerTraversalNormalizerV1Validator :: forall s.
  Term s
    ( PAsData PByteString
        :--> PAsData PCurrencySymbol
        :--> PScriptContext
        :--> PUnit
    )
scriptSourcesRedeemerTraversalNormalizerV1Validator = plam $ \deploymentIdD policyId ctx ->
  pstep ctx $ \datum redeemer ownOutRef txInfo ->
  pdispatch @_ @PScriptSourcesRedeemerTraversalNormalizerActionV1 policyId datum redeemer ownOutRef txInfo $
    \action -> pmatch action $ \(PNormalizeTraversal inputIndex outputIndex auxiliary currentItemControlD traversalActionD) ->
      pmatch txInfo $ \PTxInfo {ptxInfo'inputs, ptxInfo'outputs} ->
      pcontinue
        policyId
        (pexpectDatum datum)
        (pfromData inputIndex)
        (pfromData outputIndex)
        ownOutRef
        (pfromData ptxInfo'inputs)
        (pfromData ptxInfo'outputs)
        $ \inputScriptHash _threadTokenAssetName _fraudProver inputState outputScriptHash outputState ->
          plet (pexpectStateAs @PPreparedScriptSourcesRedeemerEnvelopeV1 inputState) $ \state ->
          pmatch state $ \stateFields ->
          plet (pfromData currentItemControlD) $ \currentItemControl ->
          pmatch currentItemControl $ \currentFields ->
          pmatch (pfromData $ Redeemer.predeemerControl'traversal currentFields) $ \case
            PDNothing -> perror
            PDJust traversalD ->
              plet (pfromData traversalD) $ \traversal ->
              pmatch traversal $ \traversalFields ->
              plet (pfromData traversalActionD) $ \traversalAction ->
              plet (pencodeControlV1 # traversal) $ \checkedTraversalControlCbor ->
              plet
                ( pcon $ PUnvalidatedRedeemerItemOuterFieldsV1
                    { pouterFields'version = Redeemer.predeemerControl'version currentFields
                    , pouterFields'mode = Redeemer.predeemerControl'mode currentFields
                    , pouterFields'stage = Redeemer.predeemerControl'stage currentFields
                    , pouterFields'itemIndex = Redeemer.predeemerControl'itemIndex currentFields
                    , pouterFields'itemCount = Redeemer.predeemerControl'itemCount currentFields
                    , pouterFields'totalLength = Redeemer.predeemerControl'totalLength currentFields
                    , pouterFields'itemCommitment = Redeemer.predeemerControl'itemCommitment currentFields
                    , pouterFields'expectedPurposeTag = Redeemer.predeemerControl'expectedPurposeTag currentFields
                    , pouterFields'expectedPointerIndex = Redeemer.predeemerControl'expectedPointerIndex currentFields
                    , pouterFields'purposeTag = Redeemer.predeemerControl'purposeTag currentFields
                    , pouterFields'pointerIndex = Redeemer.predeemerControl'pointerIndex currentFields
                    , pouterFields'dataOffset = Redeemer.predeemerControl'dataOffset currentFields
                    , pouterFields'dataLength = Redeemer.predeemerControl'dataLength currentFields
                    , pouterFields'executionMemory = Redeemer.predeemerControl'executionMemory currentFields
                    , pouterFields'executionSteps = Redeemer.predeemerControl'executionSteps currentFields
                    }
                )
                $ \outerFields ->
                plet
                  ( pcon $ PTraversalNormalizedScriptSourcesRedeemerActionV1
                      { ptraversalNormalized'version = pdata pversion
                      , ptraversalNormalized'domain = pdata ptraversalNormalizedDomain
                      , ptraversalNormalized'deploymentId = deploymentIdD
                      , ptraversalNormalized'baseProvenanceIdentity = pdata $ pbaseProvenanceIdentityV1 # state
                      , ptraversalNormalized'envelopeBinderScriptHash = penvelope'envelopeBinderScriptHash stateFields
                      , ptraversalNormalized'traversalNormalizerScriptHash = penvelope'traversalNormalizerScriptHash stateFields
                      , ptraversalNormalized'outerNormalizerScriptHash = penvelope'outerNormalizerScriptHash stateFields
                      , ptraversalNormalized'semanticExecutorScriptHash = penvelope'semanticExecutorScriptHash stateFields
                      , ptraversalNormalized'settlementScriptHash = penvelope'settlementScriptHash stateFields
                      , ptraversalNormalized'actionFamily = penvelope'actionFamily stateFields
                      , ptraversalNormalized'canonicalActionHash = penvelope'canonicalActionHash stateFields
                      , ptraversalNormalized'authenticatedTraversalActionIdentity = pdata $ ptraversalActionIdentityV1 # traversalAction
                      , ptraversalNormalized'currentPendingItemControlHash = penvelope'currentPendingItemControlHash stateFields
                      , ptraversalNormalized'expectedNextItemControlHash = penvelope'expectedNextItemControlHash stateFields
                      , ptraversalNormalized'redeemerCount = penvelope'redeemerCount stateFields
                      , ptraversalNormalized'redeemerTotalCount = penvelope'redeemerTotalCount stateFields
                      , ptraversalNormalized'unvalidatedOuterFields = pdata outerFields
                      , ptraversalNormalized'validatedTraversalControl = traversalD
                      , ptraversalNormalized'checkedTraversalControlCbor = pdata checkedTraversalControlCbor
                      }
                  )
                  $ \expectedOutputState ->
                  plet (pfromData deploymentIdD) $ \deploymentId ->
                  plet (pto $ pfromData inputScriptHash) $ \inputScript ->
                  plet (pto $ pfromData outputScriptHash) $ \outputScript ->
                    pand'List
                      [ penvelopeStateIsBoundV1
                          # state # deploymentId
                          # pfromData (penvelope'envelopeBinderScriptHash stateFields)
                      , plengthBS # deploymentId #== 32
                      , ptraversalNormalizerRouteIsExactV1
                          # state # inputScript # outputScript
                      , pcanonicalAuxiliaryHashV1 # auxiliary
                          #== pfromData (penvelope'canonicalAuxiliaryHash stateFields)
                      , pcanonicalActionHashV1 # auxiliary # pfromData (penvelope'actionFamily stateFields)
                          #== pfromData (penvelope'canonicalActionHash stateFields)
                      , pnarrowActionIsBoundV1
                          # pforgetData currentItemControlD # traversalAction
                          # pfromData (penvelope'actionFamily stateFields)
                          # pfromData (penvelope'canonicalActionHash stateFields)
                      , pcontrolIsWellFormed # traversal
                      , pfromData (ptraverse'stage traversalFields) #== pstageFold
                      , outputState #== pforgetData (pdata expectedOutputState)
                      ]

scriptSourcesRedeemerOuterNormalizerV1Validator :: forall s.
  Term s
    ( PAsData PByteString
        :--> PAsData PCurrencySymbol
        :--> PScriptContext
        :--> PUnit
    )
scriptSourcesRedeemerOuterNormalizerV1Validator = plam $ \deploymentIdD policyId ctx ->
  pstep ctx $ \datum redeemer ownOutRef txInfo ->
  pdispatch @_ @PScriptSourcesRedeemerOuterNormalizerActionV1 policyId datum redeemer ownOutRef txInfo $
    \action -> pmatch action $ \(PNormalizeOuter inputIndex outputIndex) ->
      pmatch txInfo $ \PTxInfo {ptxInfo'inputs, ptxInfo'outputs} ->
      pcontinue
        policyId
        (pexpectDatum datum)
        (pfromData inputIndex)
        (pfromData outputIndex)
        ownOutRef
        (pfromData ptxInfo'inputs)
        (pfromData ptxInfo'outputs)
        $ \inputScriptHash _threadTokenAssetName _fraudProver inputState outputScriptHash outputState ->
          plet (pexpectStateAs @PTraversalNormalizedScriptSourcesRedeemerActionV1 inputState) $ \state ->
          pmatch state $ \stateFields ->
          plet (pfromData $ ptraversalNormalized'unvalidatedOuterFields stateFields) $ \outer ->
          pmatch outer $ \outerFields ->
          plet (pfromData $ ptraversalNormalized'validatedTraversalControl stateFields) $ \traversal ->
          plet
            ( pcon $ Redeemer.PRedeemerItemProofControlV1
                { Redeemer.predeemerControl'version = pouterFields'version outerFields
                , Redeemer.predeemerControl'mode = pouterFields'mode outerFields
                , Redeemer.predeemerControl'stage = pouterFields'stage outerFields
                , Redeemer.predeemerControl'itemIndex = pouterFields'itemIndex outerFields
                , Redeemer.predeemerControl'itemCount = pouterFields'itemCount outerFields
                , Redeemer.predeemerControl'totalLength = pouterFields'totalLength outerFields
                , Redeemer.predeemerControl'itemCommitment = pouterFields'itemCommitment outerFields
                , Redeemer.predeemerControl'expectedPurposeTag = pouterFields'expectedPurposeTag outerFields
                , Redeemer.predeemerControl'expectedPointerIndex = pouterFields'expectedPointerIndex outerFields
                , Redeemer.predeemerControl'purposeTag = pouterFields'purposeTag outerFields
                , Redeemer.predeemerControl'pointerIndex = pouterFields'pointerIndex outerFields
                , Redeemer.predeemerControl'dataOffset = pouterFields'dataOffset outerFields
                , Redeemer.predeemerControl'dataLength = pouterFields'dataLength outerFields
                , Redeemer.predeemerControl'executionMemory = pouterFields'executionMemory outerFields
                , Redeemer.predeemerControl'executionSteps = pouterFields'executionSteps outerFields
                , Redeemer.predeemerControl'traversal = pdata $ pcon $ PDJust $ pdata traversal
                }
            )
            $ \currentItemControl ->
            pmatch currentItemControl $ \currentFields ->
            plet (Redeemer.pstageDataSomeTraversalHashPrefixV1 # currentItemControl) $ \nextHashPrefix ->
            plet
              ( ptraversalSerializationTemplateV1
                  # traversal # pfromData (ptraversalNormalized'actionFamily stateFields)
              )
              $ \serializationTemplate ->
              plet
                ( pcon $ POuterNormalizedScriptSourcesRedeemerActionV1
                    { pouterNormalized'version = pdata pversion
                    , pouterNormalized'domain = pdata pouterNormalizedDomain
                    , pouterNormalized'deploymentId = deploymentIdD
                    , pouterNormalized'baseProvenanceIdentity = ptraversalNormalized'baseProvenanceIdentity stateFields
                    , pouterNormalized'envelopeBinderScriptHash = ptraversalNormalized'envelopeBinderScriptHash stateFields
                    , pouterNormalized'traversalNormalizerScriptHash = ptraversalNormalized'traversalNormalizerScriptHash stateFields
                    , pouterNormalized'outerNormalizerScriptHash = ptraversalNormalized'outerNormalizerScriptHash stateFields
                    , pouterNormalized'semanticExecutorScriptHash = ptraversalNormalized'semanticExecutorScriptHash stateFields
                    , pouterNormalized'settlementScriptHash = ptraversalNormalized'settlementScriptHash stateFields
                    , pouterNormalized'actionFamily = ptraversalNormalized'actionFamily stateFields
                    , pouterNormalized'canonicalActionHash = ptraversalNormalized'canonicalActionHash stateFields
                    , pouterNormalized'authenticatedTraversalActionIdentity = ptraversalNormalized'authenticatedTraversalActionIdentity stateFields
                    , pouterNormalized'currentPendingItemControlHash = ptraversalNormalized'currentPendingItemControlHash stateFields
                    , pouterNormalized'expectedNextItemControlHash = ptraversalNormalized'expectedNextItemControlHash stateFields
                    , pouterNormalized'redeemerCount = ptraversalNormalized'redeemerCount stateFields
                    , pouterNormalized'redeemerTotalCount = ptraversalNormalized'redeemerTotalCount stateFields
                    , pouterNormalized'nextItemControlHashPrefixCbor = pdata nextHashPrefix
                    , pouterNormalized'validatedTraversalControl = ptraversalNormalized'validatedTraversalControl stateFields
                    , pouterNormalized'traversalSerializationTemplate = pdata serializationTemplate
                    }
                )
                $ \expectedOutputState ->
                plet (pfromData deploymentIdD) $ \deploymentId ->
                plet (pto $ pfromData inputScriptHash) $ \inputScript ->
                plet (pto $ pfromData outputScriptHash) $ \outputScript ->
                  pand'List
                    [ ptraversalNormalizedStateIsBoundV1
                        # state # deploymentId
                        # pfromData (ptraversalNormalized'envelopeBinderScriptHash stateFields)
                        # pfromData (ptraversalNormalized'traversalNormalizerScriptHash stateFields)
                    , plengthBS # deploymentId #== 32
                    , pouterNormalizerRouteIsExactV1 # state # inputScript # outputScript
                    , Redeemer.pstageDataOuterFieldsAreWellFormedV1 # currentItemControl
                    , pfromData (Redeemer.predeemerControl'itemIndex currentFields)
                        #== pfromData (ptraversalNormalized'redeemerCount stateFields)
                    , pfromData (Redeemer.predeemerControl'itemCount currentFields)
                        #== pfromData (ptraversalNormalized'redeemerTotalCount stateFields)
                    , Redeemer.phashStageDataOuterWithCheckedTraversalV1
                        # currentItemControl
                        # pfromData (ptraversalNormalized'checkedTraversalControlCbor stateFields)
                        #== pfromData (ptraversalNormalized'currentPendingItemControlHash stateFields)
                    , outputState #== pforgetData (pdata expectedOutputState)
                    ]

scriptSourcesRedeemerFoldMapExecutorV1Validator :: forall s.
  Term s
    ( PAsData PByteString
        :--> PAsData PCurrencySymbol
        :--> PScriptContext
        :--> PUnit
    )
scriptSourcesRedeemerFoldMapExecutorV1Validator = plam $ \deploymentIdD policyId ctx ->
  pstep ctx $ \datum redeemer ownOutRef txInfo ->
  pdispatch @_ @PScriptSourcesRedeemerFoldMapExecutorActionV1 policyId datum redeemer ownOutRef txInfo $
    \action -> pmatch action $ \(PExecuteFoldMap inputIndex outputIndex traversalActionD) ->
      plet (pfromData traversalActionD) $ \traversalAction ->
      pmatch traversalAction $ \case
        PFoldMap frameD pairIndexD keyD valueD keySiblingsD valueSiblingsD ->
          pmatch txInfo $ \PTxInfo {ptxInfo'inputs, ptxInfo'outputs} ->
          pcontinue
            policyId
            (pexpectDatum datum)
            (pfromData inputIndex)
            (pfromData outputIndex)
            ownOutRef
            (pfromData ptxInfo'inputs)
            (pfromData ptxInfo'outputs)
            $ \inputScriptHash _threadTokenAssetName _fraudProver inputState outputScriptHash outputState ->
              plet (pexpectStateAs @POuterNormalizedScriptSourcesRedeemerActionV1 inputState) $ \state ->
              pmatch state $ \stateFields ->
              plet (pfromData $ pouterNormalized'validatedTraversalControl stateFields) $ \control ->
              pmatch control $ \controlFields ->
              pmatch
                ( pprevalidatedFoldMapNextFrameRootV1
                    # pfromData (ptraverse'frameRoot controlFields)
                    # pfromData frameD
                    # pfromData pairIndexD
                    # pfromData keyD
                    # pfromData valueD
                    # pfromData keySiblingsD
                    # pfromData valueSiblingsD
                )
                $ \case
                  PNothing -> pconstant False
                  PJust nextFrameRoot ->
                    pmatch (pfromData $ pouterNormalized'traversalSerializationTemplate stateFields) $ \case
                      PFoldMapFrameRootTemplate prefixD suffixD ->
                        plet
                          ( pfromData prefixD
                              <> (pencodeDefiniteBytes # nextFrameRoot)
                              <> pfromData suffixD
                          )
                          $ \checkedNextControlCbor ->
                          plet
                            ( phashStageDataFromAuthenticatedPrefixV1
                                # pfromData (pouterNormalized'nextItemControlHashPrefixCbor stateFields)
                                # checkedNextControlCbor
                            )
                            $ \actualNextItemControlHash ->
                            plet (pfromData deploymentIdD) $ \deploymentId ->
                            plet (pto $ pfromData inputScriptHash) $ \inputScript ->
                            plet (pto $ pfromData outputScriptHash) $ \outputScript ->
                              pand'List
                                [ plengthBS # deploymentId #== 32
                                , pouterNormalizedStateIsBoundV1
                                    # state # pfoldMapFamily # deploymentId
                                    # pfromData (pouterNormalized'envelopeBinderScriptHash stateFields)
                                    # pfromData (pouterNormalized'traversalNormalizerScriptHash stateFields)
                                    # pfromData (pouterNormalized'outerNormalizerScriptHash stateFields)
                                    # inputScript
                                    # pfromData (pouterNormalized'settlementScriptHash stateFields)
                                , psemanticExecutorRouteIsExactV1
                                    # state # pfoldMapFamily # inputScript # outputScript
                                    # pfromData (pouterNormalized'settlementScriptHash stateFields)
                                , ptraversalActionIdentityIsBoundV1
                                    # traversalAction # pfoldMapFamily
                                    # pfromData (pouterNormalized'authenticatedTraversalActionIdentity stateFields)
                                , pfromData (ptraverse'stage controlFields) #== pstageFold
                                , actualNextItemControlHash
                                    #== pfromData (pouterNormalized'expectedNextItemControlHash stateFields)
                                , outputState
                                    #== pforgetData
                                      (pdata $ pexecutionAttestedStateV1 # state # actualNextItemControlHash)
                                ]
                      _ -> perror
        _ -> perror

scriptSourcesRedeemerFinalizeFrameExecutorV1Validator :: forall s.
  Term s
    ( PAsData PByteString
        :--> PAsData PCurrencySymbol
        :--> PScriptContext
        :--> PUnit
    )
scriptSourcesRedeemerFinalizeFrameExecutorV1Validator = plam $ \deploymentIdD policyId ctx ->
  pstep ctx $ \datum redeemer ownOutRef txInfo ->
  pdispatch @_ @PScriptSourcesRedeemerFinalizeFrameExecutorActionV1 policyId datum redeemer ownOutRef txInfo $
    \action -> pmatch action $ \(PExecuteFinalizeFrame inputIndex outputIndex traversalActionD) ->
      plet (pfromData traversalActionD) $ \traversalAction ->
      pmatch traversalAction $ \case
        PFinalizeFrame frameD parentD ->
          pmatch txInfo $ \PTxInfo {ptxInfo'inputs, ptxInfo'outputs} ->
          pcontinue
            policyId
            (pexpectDatum datum)
            (pfromData inputIndex)
            (pfromData outputIndex)
            ownOutRef
            (pfromData ptxInfo'inputs)
            (pfromData ptxInfo'outputs)
            $ \inputScriptHash _threadTokenAssetName _fraudProver inputState outputScriptHash outputState ->
              plet (pexpectStateAs @POuterNormalizedScriptSourcesRedeemerActionV1 inputState) $ \state ->
              pmatch state $ \stateFields ->
              plet (pfromData $ pouterNormalized'validatedTraversalControl stateFields) $ \control ->
              pmatch control $ \controlFields ->
              pmatch
                ( pprevalidatedFinalizeFrameTransitionV1
                    # pfromData (ptraverse'frameRoot controlFields)
                    # pfromData (ptraverse'offset controlFields)
                    # pfromData (ptraverse'sourceLength controlFields)
                    # pfromData frameD
                    # pfromData parentD
                )
                $ \case
                  PNothing -> pconstant False
                  PJust transition ->
                    pmatch (pfromData $ pouterNormalized'traversalSerializationTemplate stateFields) $ \case
                      PFinalizeFrameTemplate prefixD sourceFieldsD suffixD ->
                        pmatch transition $ \transitionFields ->
                        plet
                          ( pfromData prefixD
                              <> (pcborInt $ ptransition'nextStage transitionFields)
                              <> pfromData sourceFieldsD
                              <> (pencodeDefiniteBytes # ptransition'nextFrameRoot transitionFields)
                              <> pfromData suffixD
                              <> (pencodeOptionalSummaryV1 # ptransition'nextResult transitionFields)
                          )
                          $ \checkedNextControlCbor ->
                          plet
                            ( phashStageDataFromAuthenticatedPrefixV1
                                # pfromData (pouterNormalized'nextItemControlHashPrefixCbor stateFields)
                                # checkedNextControlCbor
                            )
                            $ \actualNextItemControlHash ->
                            plet (pfromData deploymentIdD) $ \deploymentId ->
                            plet (pto $ pfromData inputScriptHash) $ \inputScript ->
                            plet (pto $ pfromData outputScriptHash) $ \outputScript ->
                              pand'List
                                [ plengthBS # deploymentId #== 32
                                , pouterNormalizedStateIsBoundV1
                                    # state # pfinalizeFrameFamily # deploymentId
                                    # pfromData (pouterNormalized'envelopeBinderScriptHash stateFields)
                                    # pfromData (pouterNormalized'traversalNormalizerScriptHash stateFields)
                                    # pfromData (pouterNormalized'outerNormalizerScriptHash stateFields)
                                    # inputScript
                                    # pfromData (pouterNormalized'settlementScriptHash stateFields)
                                , psemanticExecutorRouteIsExactV1
                                    # state # pfinalizeFrameFamily # inputScript # outputScript
                                    # pfromData (pouterNormalized'settlementScriptHash stateFields)
                                , ptraversalActionIdentityIsBoundV1
                                    # traversalAction # pfinalizeFrameFamily
                                    # pfromData (pouterNormalized'authenticatedTraversalActionIdentity stateFields)
                                , pfromData (ptraverse'stage controlFields) #== pstageFold
                                , actualNextItemControlHash
                                    #== pfromData (pouterNormalized'expectedNextItemControlHash stateFields)
                                , outputState
                                    #== pforgetData
                                      (pdata $ pexecutionAttestedStateV1 # state # actualNextItemControlHash)
                                ]
                      _ -> perror
        _ -> perror

scriptSourcesRedeemerExecutionSettlementV1Validator :: forall s.
  Term s
    ( PAsData PByteString
        :--> PAsData PScriptHash
        :--> PAsData PScriptHash
        :--> PAsData PScriptHash
        :--> PAsData PScriptHash
        :--> PAsData PScriptHash
        :--> PAsData PCurrencySymbol
        :--> PScriptContext
        :--> PUnit
    )
scriptSourcesRedeemerExecutionSettlementV1Validator = plam $
  \deploymentIdD
   expectedTraversalNormalizerD
   expectedOuterNormalizerD
   expectedFoldMapExecutorD
   expectedFinalizeFrameExecutorD
   expectedAwardD
   policyId
   ctx ->
    pstep ctx $ \datum redeemer ownOutRef txInfo ->
    pdispatch @_ @PScriptSourcesRedeemerExecutionSettlementActionV1 policyId datum redeemer ownOutRef txInfo $
      \action -> pmatch action $ \(PSettleExecution inputIndex outputIndex envelopeD) ->
        pmatch txInfo $ \PTxInfo {ptxInfo'inputs, ptxInfo'outputs} ->
        pcontinue
          policyId
          (pexpectDatum datum)
          (pfromData inputIndex)
          (pfromData outputIndex)
          ownOutRef
          (pfromData ptxInfo'inputs)
          (pfromData ptxInfo'outputs)
          $ \inputScriptHash _threadTokenAssetName _fraudProver inputState outputScriptHash outputState ->
            plet (pexpectStateAs @PScriptSourcesRedeemerExecutionAttestedStateV1 inputState) $ \state ->
            plet (pfromData envelopeD) $ \envelope ->
            plet (pfromData deploymentIdD) $ \deploymentId ->
            plet (pto $ pfromData expectedTraversalNormalizerD) $ \expectedTraversalNormalizer ->
            plet (pto $ pfromData expectedOuterNormalizerD) $ \expectedOuterNormalizer ->
            plet (pto $ pfromData expectedFoldMapExecutorD) $ \expectedFoldMapExecutor ->
            plet (pto $ pfromData expectedFinalizeFrameExecutorD) $ \expectedFinalizeFrameExecutor ->
            plet (pto $ pfromData expectedAwardD) $ \expectedAward ->
            pmatch state $ \stateFields ->
              pand'List
                [ plengthBS # deploymentId #== 32
                , plengthBS # expectedTraversalNormalizer #== 28
                , plengthBS # expectedOuterNormalizer #== 28
                , plengthBS # expectedFoldMapExecutor #== 28
                , plengthBS # expectedFinalizeFrameExecutor #== 28
                , plengthBS # expectedAward #== 28
                , pexecutionAttestationSettlementIsExactV1
                    # state
                    # envelope
                    # deploymentId
                    # pfromData (pattested'envelopeBinderScriptHash stateFields)
                    # expectedTraversalNormalizer
                    # expectedOuterNormalizer
                    # expectedFoldMapExecutor
                    # expectedFinalizeFrameExecutor
                    # (pto $ pfromData inputScriptHash)
                    # expectedAward
                    # (pto $ pfromData outputScriptHash)
                    # outputState
                ]
