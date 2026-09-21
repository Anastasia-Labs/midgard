module Midgard.Validators.FraudProofs.MissingScriptSource (
  missingScriptSourceStep01Validator,
  missingScriptSourceStep02Validator,
  missingScriptSourceStep03Validator,
  missingScriptSourceStep04Validator,
  missingScriptSourceStep05Validator,
  missingScriptSourceStep06Validator,
) where

import Plutarch.LedgerApi.V3 (PAddress, PCurrencySymbol, PScriptContext, PScriptHash, PTxInfo (..))
import Plutarch.Monadic qualified as P
import Plutarch.Prelude
import Plutarch.Unsafe (punsafeCoerce)

import Midgard.FraudProofs.Common (pcontinue, pfinalize, ppassNativeTxToNextStepCarried)
import Midgard.FraudProofs.MissingScriptSource
import Midgard.FraudProofs.NativeTx.Compact (pverifyNativeTxProofSourceV1)
import Midgard.FraudProofs.NativeTx.Types (PNativeTxCompact (..), PVerifiedMidgardNativeTxCompact (..))
import Midgard.FraudProofs.ProofThreadSubstrate qualified as Subject
import Midgard.LedgerState (PForcedInclusionTxV1 (..), PHeaderV1 (..), PNativeTxProofSourceV1 (..))
import Midgard.TransitionTrace (PRootMembershipProof (..))
import Midgard.Validators.FraudProofs.Step (pdispatch, pexpectDatum, pexpectStateAs, pexpecting, pstep)

missingScriptSourceStep01Validator :: forall s. Term s (PAsData PScriptHash :--> PAsData PCurrencySymbol :--> PAsData PScriptHash :--> PScriptContext :--> PUnit)
missingScriptSourceStep01Validator = plam $ \step02Hash threadPolicy hubOracle ctx ->
  pstep ctx $ \datum redeemer ownRef tx -> pdispatch @_ @PStep01Args threadPolicy datum redeemer ownRef tx $ \args -> P.do
    PStep01Args{..} <- pmatch args
    PTxInfo{ptxInfo'inputs, ptxInfo'outputs, ptxInfo'referenceInputs, ptxInfo'redeemers} <- pmatch tx
    pmatch (pfromData pstep01Args'source) $ \case
      PAcceptedSource inclusion ->
        ppassNativeTxToNextStepCarried threadPolicy hubOracle datum (pfromData inclusion) ownRef (pfromData ptxInfo'inputs) (pfromData ptxInfo'referenceInputs) (pfromData ptxInfo'outputs) (pto $ pto $ pfromData ptxInfo'redeemers) $ \_ _ _ _ outputHash outputState header _ verified -> P.do
          PHeaderV1{pheader'validationTracesRoot, pheader'validationTraceCount} <- pmatch $ pfromData header
          PVerifiedMidgardNativeTxCompact{pverified'txCompact} <- pmatch verified
          PNativeTxCompact{pcompact'witnessSetHash} <- pmatch pverified'txCompact
          let expected = pbindPurposeV1 # (Subject.pbindAcceptedSubject # verified) # pfromData pheader'validationTracesRoot # pfromData pheader'validationTraceCount # pcompact'witnessSetHash # pfromData pstep01Args'purposeKind # pfromData pstep01Args'purposeIndex
          outputHash #== step02Hash #&& outputState #== pforgetData (pdata expected)
      PForcedSource inputIndex outputIndex header membership direction ->
        pcontinue threadPolicy (pexpectDatum datum) (pfromData inputIndex) (pfromData outputIndex) ownRef (pfromData ptxInfo'inputs) (pfromData ptxInfo'outputs) $ \_ threadName _ _ outputHash outputState -> P.do
          subject <- plet $ Subject.pbindForcedSubjectToThread # pto (pfromData threadName) # pfromData header # pfromData membership # pfromData direction
          Subject.PVerdictSubject{Subject.psubject'transactionId} <- pmatch subject
          PRootMembershipProof{prootMembership'value} <- pmatch $ pfromData membership
          PForcedInclusionTxV1{pforcedTx'txId, pforcedTx'source} <- pmatch $ pfromData $ punsafeCoerce @(PAsData PForcedInclusionTxV1) prootMembership'value
          PNativeTxProofSourceV1{..} <- pmatch $ pfromData pforcedTx'source
          PPair verified _ <- pmatch $ pverifyNativeTxProofSourceV1 # pfromData pforcedTx'txId # pfromData pnativeSource'compactCbor # pfromData pnativeSource'witnessSetCompactCbor # pfromData pnativeSource'fieldPreimageLengthsCbor
          PVerifiedMidgardNativeTxCompact{pverified'txId, pverified'txCompact} <- pmatch verified
          PNativeTxCompact{pcompact'witnessSetHash} <- pmatch pverified'txCompact
          PHeaderV1{pheader'validationTracesRoot, pheader'validationTraceCount} <- pmatch $ pfromData header
          let expected = pbindPurposeV1 # subject # pfromData pheader'validationTracesRoot # pfromData pheader'validationTraceCount # pcompact'witnessSetHash # pfromData pstep01Args'purposeKind # pfromData pstep01Args'purposeIndex
          pverified'txId #== pfromData psubject'transactionId #&& outputHash #== step02Hash #&& outputState #== pforgetData (pdata expected)

missingScriptSourceStep02Validator :: forall s. Term s (PAsData PScriptHash :--> PAsData PCurrencySymbol :--> PScriptContext :--> PUnit)
missingScriptSourceStep02Validator = plam $ \step03Hash threadPolicy ctx ->
  pstep ctx $ \datum redeemer ownRef tx -> pdispatch @_ @PStep02Args threadPolicy datum redeemer ownRef tx $ \args -> P.do
    PStep02Args{..} <- pmatch args
    PTxInfo{ptxInfo'inputs, ptxInfo'outputs} <- pmatch tx
    pcontinue threadPolicy (pexpectDatum datum) (pfromData pstep02Args'inputIndex) (pfromData pstep02Args'outputIndex) ownRef (pfromData ptxInfo'inputs) (pfromData ptxInfo'outputs) $ \_ _ _ inputState outputHash outputState ->
      let expected = pauthenticateTraceStateV1 # pexpectStateAs @PBoundPurposeV1 inputState # pfromData pstep02Args'traceMembership # pfromData pstep02Args'machineState # pfromData pstep02Args'traceProof
       in outputHash #== step03Hash #&& outputState #== pforgetData (pdata expected)

missingScriptSourceStep03Validator :: forall s. Term s (PAsData PScriptHash :--> PAsData PCurrencySymbol :--> PScriptContext :--> PUnit)
missingScriptSourceStep03Validator = plam $ \step04Hash threadPolicy ctx ->
  pstep ctx $ \datum redeemer ownRef tx -> pdispatch @_ @PStep03Args threadPolicy datum redeemer ownRef tx $ \args -> P.do
    PStep03Args{..} <- pmatch args
    PTxInfo{ptxInfo'inputs, ptxInfo'outputs} <- pmatch tx
    pcontinue threadPolicy (pexpectDatum datum) (pfromData pstep03Args'inputIndex) (pfromData pstep03Args'outputIndex) ownRef (pfromData ptxInfo'inputs) (pfromData ptxInfo'outputs) $ \_ _ _ inputState outputHash outputState ->
      let purpose = pauthenticatePurposeControlV1 # pexpectStateAs @PAuthenticatedTraceStateV1 inputState # pfromData pstep03Args'control # pfromData pstep03Args'absolutePurposeIndex # pfromData pstep03Args'requiredScriptHash # pfromData pstep03Args'purposeSubject # pfromData pstep03Args'purposeSiblings
          expected = pauthenticateTransactionSourcesV1 # purpose # pfromData pstep03Args'transactionSourceCount
       in outputHash #== step04Hash #&& outputState #== pforgetData (pdata expected)

missingScriptSourceStep04Validator :: forall s. Term s (PAsData PScriptHash :--> PAsData PCurrencySymbol :--> PScriptContext :--> PUnit)
missingScriptSourceStep04Validator = plam $ \step05Hash threadPolicy ctx ->
  pstep ctx $ \datum redeemer ownRef tx -> pdispatch @_ @PStep04Args threadPolicy datum redeemer ownRef tx $ \args -> P.do
    PStep04Args{..} <- pmatch args
    PTxInfo{ptxInfo'inputs, ptxInfo'outputs} <- pmatch tx
    pcontinue threadPolicy (pexpectDatum datum) (pfromData pstep04Args'inputIndex) (pfromData pstep04Args'outputIndex) ownRef (pfromData ptxInfo'inputs) (pfromData ptxInfo'outputs) $ \_ _ _ inputState outputHash outputState ->
      let authenticated = pauthenticateResolvedSourcesV1 # pexpectStateAs @PAuthenticatedTransactionSourcesV1 inputState # pfromData pstep04Args'resolvedReferenceSourceCount
          expected = pinitialScanV1 # authenticated # pto (pfromData step05Hash)
       in outputHash #== step05Hash #&& outputState #== pforgetData (pdata expected)

missingScriptSourceStep05Validator :: forall s. Term s (PAsData PScriptHash :--> PAsData PCurrencySymbol :--> PScriptContext :--> PUnit)
missingScriptSourceStep05Validator = plam $ \step06Hash threadPolicy ctx ->
  pstep ctx $ \datum redeemer ownRef tx -> pdispatch @_ @PStep05Args threadPolicy datum redeemer ownRef tx $ \args -> P.do
    PStep05Args{..} <- pmatch args
    PTxInfo{ptxInfo'inputs, ptxInfo'outputs} <- pmatch tx
    let sources = pfromData pstep05Args'sources
        budget = pfromData pstep05Args'itemBudget
    pexpecting (budget #>= 0 #&& budget #<= pstagedSourceBudget #&& plength # sources #<= budget) $
      pcontinue threadPolicy (pexpectDatum datum) (pfromData pstep05Args'inputIndex) (pfromData pstep05Args'outputIndex) ownRef (pfromData ptxInfo'inputs) (pfromData ptxInfo'outputs) $ \ownHash _ _ inputState outputHash outputState -> P.do
        state <- plet $ pexpectStateAs @PSourceScanStateV1 inputState
        PSourceScanStateV1{psourceScan'nextExpectedScriptHash} <- pmatch state
        let advanced = padvanceScanV1 # state # sources # pto (pfromData ownHash) # pto (pfromData step06Hash)
            complete = pscanCompleteV1 # advanced
            expectedHash = pif complete step06Hash ownHash
        pif
          (pstateIsAuthenticV1 # state #&& pfromData psourceScan'nextExpectedScriptHash #== pto (pfromData ownHash))
          (outputHash #== expectedHash #&& outputState #== pforgetData (pdata advanced) #&& (pnot # (pnull # sources) #|| complete))
          perror

missingScriptSourceStep06Validator :: forall s. Term s (PAsData PCurrencySymbol :--> PAsData PCurrencySymbol :--> PAsData PAddress :--> PScriptContext :--> PUnit)
missingScriptSourceStep06Validator = plam $ \threadPolicy fraudPolicy fraudAddress ctx ->
  pstep ctx $ \datum redeemer ownRef tx -> pdispatch @_ @PStep06Args threadPolicy datum redeemer ownRef tx $ \args -> P.do
    PStep06Args{..} <- pmatch args
    PTxInfo{ptxInfo'inputs, ptxInfo'outputs, ptxInfo'redeemers} <- pmatch tx
    pfinalize threadPolicy fraudPolicy fraudAddress (pexpectDatum datum) (pfromData pstep06Args'inputIndex) (pfromData pstep06Args'outputIndex) (pfromData pstep06Args'fraudProofMintRedeemerIndex) ownRef (pfromData ptxInfo'inputs) (pfromData ptxInfo'outputs) (pto $ pto $ pfromData ptxInfo'redeemers) $ \_ _ _ inputState ->
      pterminalContradictionV1 # pexpectStateAs @PSourceScanStateV1 inputState
