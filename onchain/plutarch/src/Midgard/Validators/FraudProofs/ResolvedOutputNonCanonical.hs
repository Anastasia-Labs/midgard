module Midgard.Validators.FraudProofs.ResolvedOutputNonCanonical (
  resolvedOutputNonCanonicalStep01Validator,
  resolvedOutputNonCanonicalStep02Validator,
  resolvedOutputNonCanonicalStep03Validator,
  resolvedOutputNonCanonicalStep04Validator,
  resolvedOutputNonCanonicalStep05Validator,
) where

import Plutarch.LedgerApi.V3 (PAddress, PCurrencySymbol, PScriptContext, PScriptHash, PTxInfo (..), PTxOutRef (..))
import Plutarch.Monadic qualified as P
import Plutarch.Prelude
import Plutarch.Unsafe (punsafeCoerce)

import Midgard.FraudProofs.Common (pcontinue, pfinalize, ppassNativeTxToNextStepCarried, pverifyMembershipCarried)
import Midgard.FraudProofs.FieldOpening (PNativeTxAnchorV1 (..), popenedFieldView, preferenceInputsFieldIndex, pspendInputsFieldIndex)
import Midgard.FraudProofs.ProofThreadSubstrate qualified as Subject
import Midgard.FraudProofs.ResolvedOutputNonCanonical
import Midgard.FraudProofs.TransitionTrace.Proof qualified as Proof
import Midgard.LedgerOutputCommitment (PLedgerOutputCommitmentV1 (..), pdecodeLedgerOutputCommitment)
import Midgard.LedgerOutputScan (pinitialControlV1)
import Midgard.LedgerState (PHeaderV1 (..))
import Midgard.NativeTxMachineWalk (pspendInputAt)
import Midgard.Validators.FraudProofs.Step (pdispatch, pexpectDatum, pexpectStateAs, pstep)

resolvedOutputNonCanonicalStep01Validator :: forall s. Term s (PAsData PScriptHash :--> PAsData PCurrencySymbol :--> PAsData PScriptHash :--> PScriptContext :--> PUnit)
resolvedOutputNonCanonicalStep01Validator = plam $ \step02Hash threadPolicy hubOracle ctx ->
  pstep ctx $ \datum redeemer ownRef tx -> pdispatch @_ @PStep01Args threadPolicy datum redeemer ownRef tx $ \args -> P.do
    PStep01Args{..} <- pmatch args
    PTxInfo{ptxInfo'inputs, ptxInfo'outputs, ptxInfo'referenceInputs, ptxInfo'redeemers} <- pmatch tx
    pmatch (pfromData pstep01Args'source) $ \case
      PAcceptedSource inclusion ->
        ppassNativeTxToNextStepCarried threadPolicy hubOracle datum (pfromData inclusion) ownRef (pfromData ptxInfo'inputs) (pfromData ptxInfo'referenceInputs) (pfromData ptxInfo'outputs) (pto $ pto $ pfromData ptxInfo'redeemers) $ \_ _ _ _ outputHash outputState header _ verified -> P.do
          PHeaderV1{pheader'prevUtxosRoot} <- pmatch $ pfromData header
          let expected = pbindInputV1 # (Subject.pbindAcceptedSubject # verified) # pfromData pstep01Args'sourceKind # pfromData pstep01Args'inputIndex # pfromData pheader'prevUtxosRoot
          outputHash #== step02Hash #&& outputState #== pforgetData (pdata expected)
      PForcedSource inputIndex outputIndex header membership direction ->
        pcontinue threadPolicy (pexpectDatum datum) (pfromData inputIndex) (pfromData outputIndex) ownRef (pfromData ptxInfo'inputs) (pfromData ptxInfo'outputs) $ \_ threadName _ _ outputHash outputState -> P.do
          subject <- plet $ Subject.pbindForcedSubjectToThread # pto (pfromData threadName) # pfromData header # pfromData membership # pfromData direction
          PHeaderV1{pheader'prevUtxosRoot} <- pmatch $ pfromData header
          let expected = pbindInputV1 # subject # pfromData pstep01Args'sourceKind # pfromData pstep01Args'inputIndex # pfromData pheader'prevUtxosRoot
          outputHash #== step02Hash #&& outputState #== pforgetData (pdata expected)

resolvedOutputNonCanonicalStep02Validator :: forall s. Term s (PAsData PScriptHash :--> PAsData PCurrencySymbol :--> PAsData PCurrencySymbol :--> PScriptContext :--> PUnit)
resolvedOutputNonCanonicalStep02Validator = plam $ \step03Hash threadPolicy certificatePolicy ctx ->
  pstep ctx $ \datum redeemer ownRef tx -> pdispatch @_ @PStep02Args threadPolicy datum redeemer ownRef tx $ \args -> P.do
    PStep02Args{..} <- pmatch args
    PTxInfo{ptxInfo'inputs, ptxInfo'outputs, ptxInfo'referenceInputs} <- pmatch tx
    pcontinue threadPolicy (pexpectDatum datum) (pfromData pstep02Args'inputIndex) (pfromData pstep02Args'outputIndex) ownRef (pfromData ptxInfo'inputs) (pfromData ptxInfo'outputs) $ \_ _ _ inputState outputHash outputState -> P.do
      bound@PBoundInputV1{pboundInput'subject, pboundInput'sourceKind, pboundInput'inputIndex} <- pmatch $ pexpectStateAs @PBoundInputV1 inputState
      Subject.PVerdictSubject{Subject.psubject'transactionId} <- pmatch $ pfromData pboundInput'subject
      let fieldIndex = pif (pfromData pboundInput'sourceKind #== 0) pspendInputsFieldIndex preferenceInputsFieldIndex
      view <- plet $ popenedFieldView # pfromData pstep02Args'opening # pcon (PBodyAnchor psubject'transactionId) # fieldIndex # pfromData ptxInfo'referenceInputs # certificatePolicy
      selected <- plet $ pspendInputAt # view # pfromData pboundInput'inputIndex
      let outRef = pfromData $ punsafeCoerce @(PAsData PTxOutRef) (pdata selected)
          expected = pauthenticateOutRefV1 # pcon bound # outRef
      outputHash #== step03Hash #&& outputState #== pforgetData (pdata expected)

resolvedOutputNonCanonicalStep03Validator :: forall s. Term s (PAsData PScriptHash :--> PAsData PCurrencySymbol :--> PScriptContext :--> PUnit)
resolvedOutputNonCanonicalStep03Validator = plam $ \step04Hash threadPolicy ctx ->
  pstep ctx $ \datum redeemer ownRef tx -> pdispatch @_ @PStep03Args threadPolicy datum redeemer ownRef tx $ \args -> P.do
    PStep03Args{..} <- pmatch args
    PTxInfo{ptxInfo'inputs, ptxInfo'outputs, ptxInfo'referenceInputs, ptxInfo'redeemers} <- pmatch tx
    pcontinue threadPolicy (pexpectDatum datum) (pfromData pstep03Args'inputIndex) (pfromData pstep03Args'outputIndex) ownRef (pfromData ptxInfo'inputs) (pfromData ptxInfo'outputs) $ \_ _ _ inputState outputHash outputState -> P.do
      PAuthenticatedOutRefV1{..} <- pmatch $ pexpectStateAs @PAuthenticatedOutRefV1 inputState
      let outRef = pfromData pauthenticatedOutRef'outRef
          key = Proof.pledgerOutrefKey (pforgetData pauthenticatedOutRef'outRef)
      descriptor <- plet $ pdecodeLedgerOutputCommitment # pfromData pstep03Args'descriptorCbor
      PLedgerOutputCommitmentV1{poutputCommitment'outputIndex, poutputCommitment'totalLength} <- pmatch descriptor
      PTxOutRef{ptxOutRef'idx} <- pmatch outRef
      let expected = pcon $ PReconstructionV1 pauthenticatedOutRef'subject pstep03Args'descriptorCbor (pdata pinitialControlV1)
      pverifyMembershipCarried (pfromData pstep03Args'membership) (pfromData pauthenticatedOutRef'priorRoot) key (pfromData pstep03Args'descriptorCbor) (pfromData ptxInfo'referenceInputs) (pto $ pto $ pfromData ptxInfo'redeemers)
        #&& pfromData poutputCommitment'outputIndex
        #== pfromData ptxOutRef'idx
        #&& pfromData poutputCommitment'totalLength
        #> 0
        #&& outputHash
        #== step04Hash
        #&& outputState
        #== pforgetData (pdata expected)

resolvedOutputNonCanonicalStep04Validator :: forall s. Term s (PAsData PScriptHash :--> PAsData PCurrencySymbol :--> PScriptContext :--> PUnit)
resolvedOutputNonCanonicalStep04Validator = plam $ \step05Hash threadPolicy ctx ->
  pstep ctx $ \datum redeemer ownRef tx -> pdispatch @_ @PStep04Args threadPolicy datum redeemer ownRef tx $ \args -> P.do
    PStep04Args{..} <- pmatch args
    PTxInfo{ptxInfo'inputs, ptxInfo'outputs} <- pmatch tx
    pcontinue threadPolicy (pexpectDatum datum) (pfromData pstep04Args'inputIndex) (pfromData pstep04Args'outputIndex) ownRef (pfromData ptxInfo'inputs) (pfromData ptxInfo'outputs) $ \ownHash _ _ inputState outputHash outputState -> P.do
      PReconstructionV1{..} <- pmatch $ pexpectStateAs @PReconstructionV1 inputState
      descriptor <- plet $ pdecodeLedgerOutputCommitment # pfromData preconstruction'descriptorCbor
      pmatch (pfromData pstep04Args'action) $ \case
        PAdvance chunkProof nextChunkProof ->
          pmatch (padvanceReconstructionV1 # descriptor # pfromData preconstruction'control # pfromData chunkProof # nextChunkProof) $ \case
            PJust nextControl ->
              let expected = pcon $ PReconstructionV1 preconstruction'subject preconstruction'descriptorCbor (pdata nextControl)
               in outputHash #== ownHash #&& outputState #== pforgetData (pdata expected)
            PNothing ->
              let expected = pcon $ PCanonicalVerdictV1 preconstruction'subject (pdata $ pconstant True)
               in outputHash #== step05Hash #&& outputState #== pforgetData (pdata expected)
        PFinalizeCanonical ->
          let expected = pcon $ PCanonicalVerdictV1 preconstruction'subject (pdata $ pconstant False)
           in pfinalizeCanonicalV1
                # descriptor
                # pfromData preconstruction'control
                #&& outputHash
                #== step05Hash
                #&& outputState
                #== pforgetData (pdata expected)

resolvedOutputNonCanonicalStep05Validator :: forall s. Term s (PAsData PCurrencySymbol :--> PAsData PAddress :--> PAsData PCurrencySymbol :--> PScriptContext :--> PUnit)
resolvedOutputNonCanonicalStep05Validator = plam $ \fraudPolicy fraudAddress threadPolicy ctx ->
  pstep ctx $ \datum redeemer ownRef tx -> pdispatch @_ @PStep05Args threadPolicy datum redeemer ownRef tx $ \args -> P.do
    PStep05Args{..} <- pmatch args
    PTxInfo{ptxInfo'inputs, ptxInfo'outputs, ptxInfo'redeemers} <- pmatch tx
    pfinalize threadPolicy fraudPolicy fraudAddress (pexpectDatum datum) (pfromData pstep05Args'inputIndex) (pfromData pstep05Args'outputIndex) (pfromData pstep05Args'fraudProofMintRedeemerIndex) ownRef (pfromData ptxInfo'inputs) (pfromData ptxInfo'outputs) (pto $ pto $ pfromData ptxInfo'redeemers) $ \_ _ _ inputState ->
      pterminalContradictionV1 # pexpectStateAs @PCanonicalVerdictV1 inputState
