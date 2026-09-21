-- | The shared ScriptSources and CEK item proof pipeline at the fixed Aiken target.
module Midgard.Validators.FraudProofs.ValidationTrace.ScriptSourcesRedeemerNormalization where

import Midgard.CekContextItem qualified as Carrier
import Midgard.CekContextItemWire qualified as ItemWire
import Midgard.RedeemerItemProof qualified as Item
import Midgard.ScriptSourcesItemData qualified as Wire
import Midgard.ScriptSourcesItemWire qualified as ActionWire
import Midgard.ScriptSourcesItemNormalization qualified as Normalized
import Midgard.ScriptSourcesItemSemantics qualified as Semantics
import Midgard.ScriptSourcesRedeemerNormalization qualified as Envelope
import Midgard.FraudProofs.Common qualified as Common
import Midgard.ValidationMachine qualified as VM
import Midgard.ValidationResolution qualified as Resolution
import Midgard.ValidationResolutionData (recordFields, decodePrepared, decodeTransition)
import Midgard.ValidationTrace qualified as Trace
import Midgard.Validators.FraudProofs.Step (pdispatch, pexpectDatum, pexpectState, pstep)
import Plutarch.Core.Utils (pand'List)
import Plutarch.LedgerApi.Utils (PMaybeData (..))
import Plutarch.LedgerApi.V3
import Plutarch.Prelude

type Stage s = Term s (PAsData PByteString :--> PAsData PCurrencySymbol :--> PScriptContext :--> PUnit)
type EnvelopeStage s = Term s (PAsData PByteString :--> PAsData PScriptHash :--> PAsData PScriptHash :--> PAsData (PBuiltinList (PAsData PScriptHash)) :--> PAsData PScriptHash :--> PAsData PCurrencySymbol :--> PScriptContext :--> PUnit)

run :: forall s. Term s (PAsData PCurrencySymbol) -> Term s PScriptContext -> (Term s PData -> Term s (PBuiltinList PData)) -> (Term s (PBuiltinList PData) -> Term s PByteString -> Term s PData -> Term s PByteString -> Term s PData -> Term s PBool) -> Term s PUnit
run policy ctx open evaluate = pstep ctx $ \datum redeemer ownRef tx ->
  pdispatch @_ @PData policy datum redeemer ownRef tx $ \raw -> plet (open raw) $ \f -> pmatch tx $ \t ->
    Common.pcontinue policy (pexpectDatum datum) (pasInt # (pelemAt # 0 # f)) (pasInt # (pelemAt # 1 # f)) ownRef
      (pfromData $ ptxInfo'inputs t) (pfromData $ ptxInfo'outputs t)
      (\inputHash _ _ state outputHash outputState -> evaluate f (pto $ pfromData inputHash) (pexpectState state) (pto $ pfromData outputHash) outputState)

scriptSourcesRedeemerTraversalNormalizerV1Validator :: forall s. Stage s
scriptSourcesRedeemerTraversalNormalizerV1Validator = plam $ \deployment policy ctx ->
  run policy ctx (\raw -> pmatch (pasConstr # raw) $ \(PBuiltinPair tag f) ->
    pif (tag #== 0 #&& plength # f #== 4 #|| tag #== 1 #&& plength # f #== 2) f perror) $ \f inputHash raw outputHash outputState ->
    plet (pif (plength # f #== 4)
      (plet (Wire.pdecodeEnvelope # raw) $ \state -> pmatch state $ \s ->
        plet (pelemAt # 2 # f) $ \auxiliary -> plet (pelemAt # 3 # f) $ \claimedNext ->
        pmatch (Normalized.pauxiliaryItemFields # auxiliary) $ \(PPair currentRaw witnessRaw) ->
        plet (ItemWire.pdecodeRedeemerItemProofControl # currentRaw) $ \current ->
        pif (pand'List
          [ Envelope.penvelopeStateIsBoundV1 # state # pfromData deployment # pfromData (Envelope.penvelope'envelopeBinderScriptHash s)
          , inputHash #== pfromData (Envelope.penvelope'traversalNormalizerScriptHash s)
          , Envelope.pcanonicalAuxiliaryHashV1 # auxiliary #== pfromData (Envelope.penvelope'canonicalAuxiliaryHash s)
          , Envelope.pcanonicalActionHashV1 # auxiliary # pfromData (Envelope.penvelope'actionFamily s) #== pfromData (Envelope.penvelope'canonicalActionHash s)
          , pfromData (Envelope.penvelope'carrier s) #== Envelope.pscriptSourcesCarrier #|| Normalized.prawDataHash # claimedNext #== pfromData (Envelope.penvelope'expectedNextControlDataHash s)
          ])
          (pcon $ Normalized.PTraversalChecked (pdata $ pconstant False) (pdata state) (pdata current) claimedNext
            (pdata $ Normalized.prawDataHash # witnessRaw) (pdata $ Normalized.pcheckedOptionalTraversalCbor # current)) perror)
      (pmatch (Wire.pdecodeCurrentChecked # raw) $ \s -> pmatch (pfromData $ Normalized.pcurrent'envelope s) $ \env ->
        plet (ItemWire.pdecodeRedeemerItemProofControl # Normalized.pcurrent'claimedNext s) $ \next ->
        pif (Envelope.penvelope'deploymentId env #== deployment #&& inputHash #== pfromData (Envelope.penvelope'traversalNormalizerScriptHash env))
          (pcon $ Normalized.PTraversalChecked (pdata $ pconstant True) (Normalized.pcurrent'envelope s) (Normalized.pcurrent'control s)
            (Normalized.pcurrent'claimedNext s) (Normalized.pcurrent'witnessHash s) (pdata $ Normalized.pcheckedOptionalTraversalCbor # next)) perror)) $ \checked ->
      pmatch checked $ \c -> pmatch (pfromData $ Normalized.ptraversal'envelope c) $ \env ->
        outputHash #== pfromData (Envelope.penvelope'outerNormalizerScriptHash env) #&& outputState #== pforgetData (pdata checked)

scriptSourcesRedeemerOuterNormalizerV1Validator :: forall s. Term s (PAsData PByteString :--> PAsData PCurrencySymbol :--> PAsData PScriptHash :--> PScriptContext :--> PUnit)
scriptSourcesRedeemerOuterNormalizerV1Validator = plam $ \deployment policy sourceHash ctx ->
  run policy ctx (recordFields 2) $ \_ inputHash raw outputHash outputState -> pmatch (Wire.pdecodeTraversalChecked # raw) $ \s ->
    pmatch (pfromData $ Normalized.ptraversal'envelope s) $ \env -> pmatch (pfromData $ Normalized.ptraversal'currentControl s) $ \current ->
      pif (Envelope.penvelope'deploymentId env #== deployment #&& inputHash #== pfromData (Envelope.penvelope'outerNormalizerScriptHash env))
        (pif (pfromData $ Normalized.ptraversal'checkingNext s)
          (plet (ItemWire.pdecodeRedeemerItemProofControl # Normalized.ptraversal'claimedNext s) $ \next ->
            Item.phashOuterWithCheckedOptionalTraversal # next # pfromData (Normalized.ptraversal'checkedOptionalTraversalCbor s) #== pfromData (Envelope.penvelope'expectedNextItemControlHash env)
              #&& outputHash #== pto (pfromData sourceHash)
              #&& outputState #== pforgetData (pdata $ pcon $ Normalized.PControlsChecked (Normalized.ptraversal'envelope s) (Normalized.ptraversal'currentControl s) (pdata next) (Normalized.ptraversal'witnessHash s)))
          (pand'List
            [ pif (pfromData (Envelope.penvelope'carrier env) #== Envelope.pscriptSourcesCarrier)
                (pfromData (Item.predeemerControl'mode current) #== Item.pmodeData)
                (pfromData (Envelope.penvelope'carrier env) #== Envelope.pcekContextCarrier #&& (pfromData (Item.predeemerControl'mode current) #== Item.pmodeDescriptor #|| pfromData (Item.predeemerControl'mode current) #== Item.pmodeData))
            , Item.predeemerControl'itemIndex current #== Envelope.penvelope'redeemerCount env
            , Item.predeemerControl'itemCount current #== Envelope.penvelope'redeemerTotalCount env
            , Item.phashOuterWithCheckedOptionalTraversal # pcon current # pfromData (Normalized.ptraversal'checkedOptionalTraversalCbor s) #== pfromData (Envelope.penvelope'currentPendingItemControlHash env)
            , pif (pfromData (Envelope.penvelope'actionFamily env) #== Envelope.pinvalidFamily)
                (pfromData (Envelope.penvelope'carrier env) #== Envelope.pscriptSourcesCarrier
                  #&& Normalized.ptraversal'claimedNext s #== pforgetData (pconstrBuiltin # 1 # pnil)
                  #&& outputHash #== pto (pfromData sourceHash)
                  #&& outputState #== pforgetData (pdata $ pcon $ Normalized.PControlsChecked (Normalized.ptraversal'envelope s) (Normalized.ptraversal'currentControl s) (Normalized.ptraversal'currentControl s) (Normalized.ptraversal'witnessHash s)))
                (outputHash #== pfromData (Envelope.penvelope'traversalNormalizerScriptHash env)
                  #&& outputState #== pforgetData (pdata $ pcon $ Normalized.PCurrentChecked (Normalized.ptraversal'envelope s) (Normalized.ptraversal'currentControl s) (Normalized.ptraversal'claimedNext s) (Normalized.ptraversal'witnessHash s)))
            ])) perror

scriptSourcesRedeemerSourceAuthenticatorValidator :: forall s. Stage s
scriptSourcesRedeemerSourceAuthenticatorValidator = plam $ \deployment policy ctx ->
  run policy ctx (recordFields 3) $ \f _ raw outputHash outputState -> pmatch (Wire.pdecodeControlsChecked # raw) $ \s ->
    pmatch (pfromData $ Normalized.pcontrols'envelope s) $ \env ->
    plet (ActionWire.pdecodeWitness # (pelemAt # 2 # f)) $ \witness -> pmatch witness $ \w ->
      pif (Envelope.penvelope'deploymentId env #== deployment #&& outputHash #== pfromData (Envelope.penvelope'semanticExecutorScriptHash env)
        #&& Normalized.prawDataHash # pforgetData (pdata witness) #== pfromData (Normalized.pcontrols'witnessHash s))
        (pmatch (Item.pauthenticateSourceWindow # pfromData (Normalized.pcontrols'current s) # (Item.pprevalidatedNextSourceSpan # pfromData (Normalized.pcontrols'current s)) # witness) $ \case
          PNothing -> perror
          PJust source -> plet (pmatch source $ \case PNothing -> pcon PDNothing; PJust bytes -> pcon $ PDJust $ pdata bytes) $ \sourceData ->
            plet (Normalized.pexecutionOutput # pcon env # pfromData (Normalized.pcontrols'witnessHash s)) $ \output ->
            pmatch (pfromData $ Item.predeemerWitness'action w) $ \case
              Item.PRedeemerItemTraverseData action -> pmatch (pfromData $ Normalized.pcontrols'current s) $ \current -> pmatch (pfromData $ Normalized.pcontrols'next s) $ \next ->
                pmatch (pfromData $ Item.predeemerControl'traversal current) $ \case
                  PDNothing -> perror
                  PDJust inner -> pmatch (pfromData $ Item.predeemerControl'traversal next) $ \case
                    PDNothing -> perror
                    PDJust nextInner -> pfromData (Normalized.pcontrols'next s) #== pcon current { Item.predeemerControl'traversal = Item.predeemerControl'traversal next }
                      #&& outputState #== pforgetData (pdata $ pcon $ Normalized.PTraversalExecution (pdata output) inner nextInner action (pdata sourceData))
              action -> outputState #== pforgetData (pdata $ pcon $ Normalized.POuterExecution (pdata output) (Normalized.pcontrols'current s) (Normalized.pcontrols'next s) (pdata $ pcon action) (pdata sourceData))) perror

makeEnvelope :: forall s. Term s (PAsData PByteString) -> Term s PInteger -> Term s PData -> Term s PByteString -> Term s PInteger -> Term s PByteString -> Term s PByteString -> Term s PByteString -> Term s PByteString -> Term s (PAsData PInteger) -> Term s (PAsData PInteger) -> Term s PByteString -> Term s (PAsData PScriptHash) -> Term s (PAsData PScriptHash) -> Term s (PAsData PScriptHash) -> Term s (PAsData PScriptHash) -> Term s Envelope.PPreparedScriptSourcesRedeemerEnvelopeV1
makeEnvelope deployment carrier base nextDataHash family auxiliaryHash actionHash currentHash nextHash count total binder traversal outer executor settlement =
  plet (Envelope.pcarrierIdentity # base) $ \identity ->
  plet (Envelope.penvelopeCommitmentV1 # pfromData deployment # carrier # identity # family # auxiliaryHash # actionHash # currentHash # nextHash
    # pfromData count # pfromData total # binder # pto (pfromData traversal) # pto (pfromData outer) # pto (pfromData executor) # pto (pfromData settlement) # nextDataHash) $ \commitment ->
    pcon $ Envelope.PPreparedScriptSourcesRedeemerEnvelopeV1
      (pdata Envelope.pversion) (pdata Envelope.penvelopeDomain) deployment (pdata carrier) base (pdata identity) (pdata nextDataHash) (pdata family)
      (pdata auxiliaryHash) (pdata actionHash) (pdata currentHash) (pdata nextHash) count total (pdata binder)
      (pdata $ pto $ pfromData traversal) (pdata $ pto $ pfromData outer) (pdata $ pto $ pfromData executor) (pdata $ pto $ pfromData settlement) (pdata commitment)

scriptSourcesRedeemerEnvelopeV1Validator :: forall s. EnvelopeStage s
scriptSourcesRedeemerEnvelopeV1Validator = plam $ \deployment traversal outer executors settlement policy ctx ->
  run policy ctx (recordFields 6) $ \f inputHash raw outputHash outputState ->
    plet (decodePrepared raw) $ \base -> pmatch base $ \b -> pmatch (pfromData $ Resolution.pprepared'resolution b) $ \resolution ->
    plet (pfromData $ Resolution.presolution'preState resolution) $ \pre -> pmatch pre $ \before ->
    plet (decodeTransition $ pelemAt # 2 # f) $ \transition -> plet (pelemAt # 3 # f) $ \auxiliary ->
    plet (pasByteStr # (pelemAt # 4 # f)) $ \nextHash -> plet (pasInt # (pelemAt # 5 # f)) $ \family ->
    pmatch (Normalized.pauxiliaryItemFields # auxiliary) $ \(PPair current witness) ->
    plet (pelemAt # (Normalized.pexecutorIndex # current # witness # family) # pfromData executors) $ \executor ->
    pmatch (Envelope.pverifyRawEnvelopeV1 # pre # transition # auxiliary # nextHash # family) $ \case
      PNothing -> pconstant False
      PJust facts -> pmatch facts $ \fact ->
        plet (makeEnvelope deployment Envelope.pscriptSourcesCarrier (pforgetData $ pdata base) (pconstant "") family
          (pfromData $ Envelope.penvelopeFacts'canonicalAuxiliaryHash fact) (pfromData $ Envelope.penvelopeFacts'canonicalActionHash fact)
          (pfromData $ Envelope.penvelopeFacts'currentPendingItemControlHash fact) nextHash
          (Envelope.penvelopeFacts'redeemerCount fact) (Envelope.penvelopeFacts'redeemerTotalCount fact) inputHash traversal outer executor settlement) $ \expected ->
        pand'List
          [ Resolution.ppreparedResolutionIsWellFormed # base
          , pfromData (Trace.pmachineState'phase before) #== pcon Trace.PScriptSources
          , Resolution.phashOneStepEvidence # pforgetData (pdata transition) # auxiliary #== pfromData (Resolution.pprepared'evidenceHash b)
          , VM.pstructuralTransitionIsValid # pre # transition
          , plengthBS # inputHash #== 28
          , outputHash #== pto (pfromData traversal)
          , outputState #== pforgetData (pdata expected)
          ]

scriptSourcesRedeemerCekEnvelopeValidator :: forall s. EnvelopeStage s
scriptSourcesRedeemerCekEnvelopeValidator = plam $ \deployment traversal outer executors settlement policy ctx ->
  run policy ctx (recordFields 6) $ \f inputHash raw outputHash outputState ->
    plet (Carrier.pdecodePending # raw) $ \pending -> pmatch pending $ \p -> pmatch (pfromData $ Carrier.ppending'control p) $ \current ->
    plet (ActionWire.pdecodeWitness # (pelemAt # 2 # f)) $ \witness -> plet (pforgetData $ pdata witness) $ \witnessData ->
    plet (pforgetData $ Carrier.ppending'control p) $ \currentData ->
    plet (pasByteStr # (pelemAt # 3 # f)) $ \currentHash -> plet (pasByteStr # (pelemAt # 4 # f)) $ \nextHash ->
    plet (pasInt # (pelemAt # 5 # f)) $ \family ->
    pif (family #>= Envelope.pfoldMapFamily #&& family #<= Envelope.pfinishDataFamily
      #&& Normalized.prawDataHash # witnessData #== pfromData (Carrier.ppending'witnessHash p))
      (plet (pforgetData $ pconstrBuiltin # 18 # (pcons # (pforgetData $ pconstrBuiltin # 1 # pnil) # (pcons # currentData # (pcons # witnessData # pnil)))) $ \auxiliary ->
        plet (pelemAt # (Normalized.pexecutorIndex # currentData # witnessData # family) # pfromData executors) $ \executor ->
        plet (makeEnvelope deployment Envelope.pcekContextCarrier (pforgetData $ pdata pending)
          (Normalized.prawDataHash # pforgetData (Carrier.ppending'claimedNext p)) family
          (Envelope.pcanonicalAuxiliaryHashV1 # auxiliary) (Envelope.pcanonicalItemActionHash # currentData # witnessData # family)
          currentHash nextHash (Item.predeemerControl'itemIndex current) (Item.predeemerControl'itemCount current) inputHash traversal outer executor settlement) $ \expected ->
          outputHash #== pto (pfromData traversal) #&& outputState #== pforgetData (pdata expected)) perror

executorRoster :: forall s. Term s (PAsData (PBuiltinList (PAsData PScriptHash))) -> Term s (PBuiltinList (PAsData PByteString))
executorRoster hashes = pmap # plam (\hash -> pdata $ pto $ pfromData hash) # pfromData hashes

scriptSourcesRedeemerExecutionSettlementV1Validator :: forall s. EnvelopeStage s
scriptSourcesRedeemerExecutionSettlementV1Validator = plam $ \deployment traversal outer executors award policy ctx ->
  run policy ctx (recordFields 3) $ \f inputHash raw outputHash outputState ->
    plet (Wire.pdecodeAttestation # raw) $ \state -> pmatch state $ \s ->
      Envelope.pexecutionAttestationSettlementIsExactV1 # state # (Wire.pdecodeEnvelope # (pelemAt # 2 # f)) # pfromData deployment
        # pfromData (Envelope.pattested'envelopeBinderScriptHash s) # pto (pfromData traversal) # pto (pfromData outer)
        # executorRoster executors # inputHash # pto (pfromData award) # outputHash # outputState

scriptSourcesRedeemerCekSettlementValidator :: forall s. EnvelopeStage s
scriptSourcesRedeemerCekSettlementValidator = plam $ \deployment traversal outer executors returnHash policy ctx ->
  run policy ctx (recordFields 3) $ \f inputHash raw outputHash outputState ->
    plet (Wire.pdecodeAttestation # raw) $ \state -> pmatch state $ \s ->
    plet (Wire.pdecodeEnvelope # (pelemAt # 2 # f)) $ \envelope -> pmatch envelope $ \env ->
    pmatch (Carrier.pdecodePending # Envelope.penvelope'base env) $ \pending ->
      pand'List
        [ pfromData (Envelope.penvelope'carrier env) #== Envelope.pcekContextCarrier
        , pfromData (Envelope.penvelope'actionFamily env) #<= Envelope.pfinishDataFamily
        , Normalized.prawDataHash # pforgetData (Carrier.ppending'claimedNext pending) #== pfromData (Envelope.penvelope'expectedNextControlDataHash env)
        , Envelope.pexecutionAttestationIsBoundToEnvelopeV1 # state # envelope # pfromData deployment
            # pfromData (Envelope.pattested'envelopeBinderScriptHash s) # pto (pfromData traversal) # pto (pfromData outer) # executorRoster executors # inputHash
        , outputHash #== pto (pfromData returnHash)
        , outputState #== pforgetData (pdata $ pcon $ Carrier.PVerified (Carrier.ppending'staged pending) (Carrier.ppending'claimedNext pending))
        ]

scriptSourcesRedeemerFoldMapExecutorV1Validator :: forall s. Stage s
scriptSourcesRedeemerFoldMapExecutorV1Validator = plam $ \deployment policy ctx ->
  run policy ctx (recordFields 2) $ \_ inputHash raw outputHash outputState ->
    plet (Wire.pdecodeTraversalExecution # raw) $ \state -> pmatch state $ \s ->
      Normalized.pexecutionOutputIsExact # pfromData (Normalized.pexecution'output s) # pfromData deployment # 0 # inputHash # outputHash # outputState
        #&& Semantics.pfoldMap # state

scriptSourcesRedeemerFinalizeFrameExecutorV1Validator :: forall s. Stage s
scriptSourcesRedeemerFinalizeFrameExecutorV1Validator = plam $ \deployment policy ctx ->
  run policy ctx (recordFields 2) $ \_ inputHash raw outputHash outputState ->
    plet (Wire.pdecodeTraversalExecution # raw) $ \state -> pmatch state $ \s ->
      Normalized.pexecutionOutputIsExact # pfromData (Normalized.pexecution'output s) # pfromData deployment # 1 # inputHash # outputHash # outputState
        #&& Semantics.pfinalizeFrame # state

scriptSourcesRedeemerOpenHeaderExecutorValidator :: forall s. Stage s
scriptSourcesRedeemerOpenHeaderExecutorValidator = plam $ \deployment policy ctx ->
  run policy ctx (recordFields 2) $ \_ inputHash raw outputHash outputState ->
    plet (Wire.pdecodeOuterExecution # raw) $ \state -> pmatch state $ \s ->
      Normalized.pexecutionOutputIsExact # pfromData (Normalized.pouter'output s) # pfromData deployment # 2 # inputHash # outputHash # outputState
        #&& Semantics.popenHeader # state

scriptSourcesRedeemerOpenTailExecutorValidator :: forall s. Stage s
scriptSourcesRedeemerOpenTailExecutorValidator = plam $ \deployment policy ctx ->
  run policy ctx (recordFields 2) $ \_ inputHash raw outputHash outputState ->
    plet (Wire.pdecodeOuterExecution # raw) $ \state -> pmatch state $ \s ->
      Normalized.pexecutionOutputIsExact # pfromData (Normalized.pouter'output s) # pfromData deployment # 3 # inputHash # outputHash # outputState
        #&& Semantics.popenTail # state

scriptSourcesRedeemerHeadScalarExecutorValidator :: forall s. Stage s
scriptSourcesRedeemerHeadScalarExecutorValidator = plam $ \deployment policy ctx ->
  run policy ctx (recordFields 2) $ \_ inputHash raw outputHash outputState ->
    plet (Wire.pdecodeTraversalExecution # raw) $ \state -> pmatch state $ \s ->
      Normalized.pexecutionOutputIsExact # pfromData (Normalized.pexecution'output s) # pfromData deployment # 4 # inputHash # outputHash # outputState
        #&& Semantics.pheadScalar # state

scriptSourcesRedeemerHeadSequenceExecutorValidator :: forall s. Stage s
scriptSourcesRedeemerHeadSequenceExecutorValidator = plam $ \deployment policy ctx ->
  run policy ctx (recordFields 2) $ \_ inputHash raw outputHash outputState ->
    plet (Wire.pdecodeTraversalExecution # raw) $ \state -> pmatch state $ \s ->
      Normalized.pexecutionOutputIsExact # pfromData (Normalized.pexecution'output s) # pfromData deployment # 4 # inputHash # outputHash # outputState
        #&& Semantics.pheadSequence # state

scriptSourcesRedeemerHeadMapExecutorValidator :: forall s. Stage s
scriptSourcesRedeemerHeadMapExecutorValidator = plam $ \deployment policy ctx ->
  run policy ctx (recordFields 2) $ \_ inputHash raw outputHash outputState ->
    plet (Wire.pdecodeTraversalExecution # raw) $ \state -> pmatch state $ \s ->
      Normalized.pexecutionOutputIsExact # pfromData (Normalized.pexecution'output s) # pfromData deployment # 4 # inputHash # outputHash # outputState
        #&& Semantics.pheadMap # state

scriptSourcesRedeemerHeadLargeConstructorExecutorValidator :: forall s. Stage s
scriptSourcesRedeemerHeadLargeConstructorExecutorValidator = plam $ \deployment policy ctx ->
  run policy ctx (recordFields 2) $ \_ inputHash raw outputHash outputState ->
    plet (Wire.pdecodeTraversalExecution # raw) $ \state -> pmatch state $ \s ->
      Normalized.pexecutionOutputIsExact # pfromData (Normalized.pexecution'output s) # pfromData deployment # 4 # inputHash # outputHash # outputState
        #&& Semantics.pheadLargeConstructor # state

scriptSourcesRedeemerAttachIntegerExecutorValidator :: forall s. Stage s
scriptSourcesRedeemerAttachIntegerExecutorValidator = plam $ \deployment policy ctx ->
  run policy ctx (recordFields 2) $ \_ inputHash raw outputHash outputState ->
    plet (Wire.pdecodeTraversalExecution # raw) $ \state -> pmatch state $ \s ->
      Normalized.pexecutionOutputIsExact # pfromData (Normalized.pexecution'output s) # pfromData deployment # 5 # inputHash # outputHash # outputState
        #&& Semantics.pattachInteger # state

scriptSourcesRedeemerAttachBytesExecutorValidator :: forall s. Stage s
scriptSourcesRedeemerAttachBytesExecutorValidator = plam $ \deployment policy ctx ->
  run policy ctx (recordFields 2) $ \_ inputHash raw outputHash outputState ->
    plet (Wire.pdecodeTraversalExecution # raw) $ \state -> pmatch state $ \s ->
      Normalized.pexecutionOutputIsExact # pfromData (Normalized.pexecution'output s) # pfromData deployment # 5 # inputHash # outputHash # outputState
        #&& Semantics.pattachBytes # state

scriptSourcesRedeemerFoldListExecutorValidator :: forall s. Stage s
scriptSourcesRedeemerFoldListExecutorValidator = plam $ \deployment policy ctx ->
  run policy ctx (recordFields 2) $ \_ inputHash raw outputHash outputState ->
    plet (Wire.pdecodeTraversalExecution # raw) $ \state -> pmatch state $ \s ->
      Normalized.pexecutionOutputIsExact # pfromData (Normalized.pexecution'output s) # pfromData deployment # 6 # inputHash # outputHash # outputState
        #&& Semantics.pfoldList # state

scriptSourcesRedeemerAdvanceIntegerExecutorValidator :: forall s. Stage s
scriptSourcesRedeemerAdvanceIntegerExecutorValidator = plam $ \deployment policy ctx ->
  run policy ctx (recordFields 2) $ \_ inputHash raw outputHash outputState ->
    plet (Wire.pdecodeTraversalExecution # raw) $ \state -> pmatch state $ \s ->
      Normalized.pexecutionOutputIsExact # pfromData (Normalized.pexecution'output s) # pfromData deployment # 7 # inputHash # outputHash # outputState
        #&& Semantics.padvanceInteger # state

scriptSourcesRedeemerAdvanceBytesExecutorValidator :: forall s. Stage s
scriptSourcesRedeemerAdvanceBytesExecutorValidator = plam $ \deployment policy ctx ->
  run policy ctx (recordFields 2) $ \_ inputHash raw outputHash outputState ->
    plet (Wire.pdecodeTraversalExecution # raw) $ \state -> pmatch state $ \s ->
      Normalized.pexecutionOutputIsExact # pfromData (Normalized.pexecution'output s) # pfromData deployment # 7 # inputHash # outputHash # outputState
        #&& Semantics.padvanceBytes # state

scriptSourcesRedeemerAdvanceLargeConstructorExecutorValidator :: forall s. Stage s
scriptSourcesRedeemerAdvanceLargeConstructorExecutorValidator = plam $ \deployment policy ctx ->
  run policy ctx (recordFields 2) $ \_ inputHash raw outputHash outputState ->
    plet (Wire.pdecodeTraversalExecution # raw) $ \state -> pmatch state $ \s ->
      Normalized.pexecutionOutputIsExact # pfromData (Normalized.pexecution'output s) # pfromData deployment # 7 # inputHash # outputHash # outputState
        #&& Semantics.padvanceLargeConstructor # state

scriptSourcesRedeemerAdvanceLargeFieldsExecutorValidator :: forall s. Stage s
scriptSourcesRedeemerAdvanceLargeFieldsExecutorValidator = plam $ \deployment policy ctx ->
  run policy ctx (recordFields 2) $ \_ inputHash raw outputHash outputState ->
    plet (Wire.pdecodeTraversalExecution # raw) $ \state -> pmatch state $ \s ->
      Normalized.pexecutionOutputIsExact # pfromData (Normalized.pexecution'output s) # pfromData deployment # 7 # inputHash # outputHash # outputState
        #&& Semantics.padvanceLargeFields # state

scriptSourcesRedeemerCloseExecutorValidator :: forall s. Stage s
scriptSourcesRedeemerCloseExecutorValidator = plam $ \deployment policy ctx ->
  run policy ctx (recordFields 2) $ \_ inputHash raw outputHash outputState ->
    plet (Wire.pdecodeTraversalExecution # raw) $ \state -> pmatch state $ \s ->
      Normalized.pexecutionOutputIsExact # pfromData (Normalized.pexecution'output s) # pfromData deployment # 7 # inputHash # outputHash # outputState
        #&& Semantics.pclose # state

scriptSourcesRedeemerFinishDataExecutorValidator :: forall s. Stage s
scriptSourcesRedeemerFinishDataExecutorValidator = plam $ \deployment policy ctx ->
  run policy ctx (recordFields 2) $ \_ inputHash raw outputHash outputState ->
    plet (Wire.pdecodeOuterExecution # raw) $ \state -> pmatch state $ \s ->
      Normalized.pexecutionOutputIsExact # pfromData (Normalized.pouter'output s) # pfromData deployment # 8 # inputHash # outputHash # outputState
        #&& Semantics.pfinishData # state

scriptSourcesRedeemerInvalidHeaderExecutorValidator :: forall s. Stage s
scriptSourcesRedeemerInvalidHeaderExecutorValidator = plam $ \deployment policy ctx ->
  run policy ctx (recordFields 2) $ \_ inputHash raw outputHash outputState ->
    plet (Wire.pdecodeOuterExecution # raw) $ \state -> pmatch state $ \s ->
      Normalized.pexecutionOutputIsExact # pfromData (Normalized.pouter'output s) # pfromData deployment # 9 # inputHash # outputHash # outputState
        #&& Semantics.pinvalidHeader # state

scriptSourcesRedeemerInvalidTailExecutorValidator :: forall s. Stage s
scriptSourcesRedeemerInvalidTailExecutorValidator = plam $ \deployment policy ctx ->
  run policy ctx (recordFields 2) $ \_ inputHash raw outputHash outputState ->
    plet (Wire.pdecodeOuterExecution # raw) $ \state -> pmatch state $ \s ->
      Normalized.pexecutionOutputIsExact # pfromData (Normalized.pouter'output s) # pfromData deployment # 9 # inputHash # outputHash # outputState
        #&& Semantics.pinvalidTail # state
