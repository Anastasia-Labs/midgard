module Midgard.Validators.FraudProofs.UnusedRedeemer (
  unusedRedeemerStep01Validator,
  unusedRedeemerStep02Validator,
  unusedRedeemerStep02aValidator,
  unusedRedeemerStep02bValidator,
  unusedRedeemerStep02cValidator,
  unusedRedeemerStep03Validator,
  unusedRedeemerStep04Validator,
  unusedRedeemerStep05Validator,
  unusedRedeemerStep06Validator,
) where

import Plutarch.LedgerApi.V3 (PAddress, PCurrencySymbol, PScriptContext, PScriptHash, PTxInfo (..))
import Plutarch.Monadic qualified as P
import Plutarch.Prelude
import Plutarch.Unsafe (punsafeCoerce)

import Midgard.FraudProofs.Common (pcontinue, pfinalize, ppassNativeTxToNextStepCarried)
import Midgard.FraudProofs.NativeTx.Compact (pverifyNativeTxProofSourceV1)
import Midgard.FraudProofs.NativeTx.Types (PVerifiedMidgardNativeTxCompact (..))
import Midgard.FraudProofs.ProofThreadSubstrate qualified as Subject
import Midgard.FraudProofs.UnusedRedeemer
import Midgard.LedgerState (PForcedInclusionTxV1 (..), PHeaderV1 (..), PNativeTxProofSourceV1 (..))
import Midgard.TransitionTrace (PRootMembershipProof (..))
import Midgard.Validators.FraudProofs.Step (pdispatch, pexpectDatum, pexpectStateAs, pexpecting, pstep)

unusedRedeemerStep01Validator :: forall s. Term s (PAsData PScriptHash :--> PAsData PCurrencySymbol :--> PAsData PScriptHash :--> PScriptContext :--> PUnit)
unusedRedeemerStep01Validator = plam $ \step02Hash threadPolicy hubOracle ctx ->
  pstep ctx $ \datum redeemer ownRef tx -> pdispatch @_ @PStep01Args threadPolicy datum redeemer ownRef tx $ \args -> P.do
    PStep01Args{..} <- pmatch args
    PTxInfo{ptxInfo'inputs, ptxInfo'outputs, ptxInfo'referenceInputs, ptxInfo'redeemers} <- pmatch tx
    pmatch (pfromData pstep01Args'source) $ \case
      PAcceptedSource inclusion ->
        ppassNativeTxToNextStepCarried threadPolicy hubOracle datum (pfromData inclusion) ownRef (pfromData ptxInfo'inputs) (pfromData ptxInfo'referenceInputs) (pfromData ptxInfo'outputs) (pto $ pto $ pfromData ptxInfo'redeemers) $ \_ _ _ _ outputHash outputState header _ verified -> P.do
          PHeaderV1{pheader'validationTracesRoot, pheader'validationTraceCount} <- pmatch $ pfromData header
          let expected =
                pbindRedeemerV1
                  # (Subject.pbindAcceptedSubject # verified)
                  # pfromData pheader'validationTracesRoot
                  # pfromData pheader'validationTraceCount
                  # pfromData pstep01Args'redeemerIndex
          outputHash #== step02Hash #&& outputState #== pforgetData (pdata expected)
      PForcedSource inputIndex outputIndex header membership direction ->
        pcontinue threadPolicy (pexpectDatum datum) (pfromData inputIndex) (pfromData outputIndex) ownRef (pfromData ptxInfo'inputs) (pfromData ptxInfo'outputs) $ \_ threadName _ _ outputHash outputState -> P.do
          subject <- plet $ Subject.pbindForcedSubjectToThread # pto (pfromData threadName) # pfromData header # pfromData membership # pfromData direction
          PRootMembershipProof{prootMembership'value} <- pmatch $ pfromData membership
          PForcedInclusionTxV1{pforcedTx'txId, pforcedTx'source} <- pmatch $ pfromData $ punsafeCoerce @(PAsData PForcedInclusionTxV1) prootMembership'value
          PNativeTxProofSourceV1{..} <- pmatch $ pfromData pforcedTx'source
          PPair verified _ <- pmatch $ pverifyNativeTxProofSourceV1 # pfromData pforcedTx'txId # pfromData pnativeSource'compactCbor # pfromData pnativeSource'witnessSetCompactCbor # pfromData pnativeSource'fieldPreimageLengthsCbor
          PVerifiedMidgardNativeTxCompact{pverified'txId} <- pmatch verified
          PHeaderV1{pheader'validationTracesRoot, pheader'validationTraceCount} <- pmatch $ pfromData header
          Subject.PVerdictSubject{Subject.psubject'transactionId} <- pmatch subject
          let expected = pbindRedeemerV1 # subject # pfromData pheader'validationTracesRoot # pfromData pheader'validationTraceCount # pfromData pstep01Args'redeemerIndex
          pverified'txId #== pfromData psubject'transactionId #&& outputHash #== step02Hash #&& outputState #== pforgetData (pdata expected)

unusedRedeemerStep02Validator :: forall s. Term s (PAsData PScriptHash :--> PAsData PCurrencySymbol :--> PScriptContext :--> PUnit)
unusedRedeemerStep02Validator = plam $ \nextHash threadPolicy ctx ->
  pstep ctx $ \datum redeemer ownRef tx -> pdispatch @_ @PStep02Args threadPolicy datum redeemer ownRef tx $ \args -> P.do
    PStep02Args{..} <- pmatch args
    PTxInfo{ptxInfo'inputs, ptxInfo'outputs} <- pmatch tx
    pcontinue threadPolicy (pexpectDatum datum) (pfromData pstep02Args'inputIndex) (pfromData pstep02Args'outputIndex) ownRef (pfromData ptxInfo'inputs) (pfromData ptxInfo'outputs) $ \_ _ _ inputState outputHash outputState ->
      let expected = pauthenticateDescriptorV1 # pexpectStateAs @PBoundRedeemerV1 inputState # pfromData pstep02Args'traceMembership
       in outputHash #== nextHash #&& outputState #== pforgetData (pdata expected)

unusedRedeemerStep02aValidator :: forall s. Term s (PAsData PScriptHash :--> PAsData PCurrencySymbol :--> PScriptContext :--> PUnit)
unusedRedeemerStep02aValidator = plam $ \nextHash threadPolicy ctx ->
  pstep ctx $ \datum redeemer ownRef tx -> pdispatch @_ @PStep02aArgs threadPolicy datum redeemer ownRef tx $ \args -> P.do
    PStep02aArgs{..} <- pmatch args
    PTxInfo{ptxInfo'inputs, ptxInfo'outputs} <- pmatch tx
    pcontinue threadPolicy (pexpectDatum datum) (pfromData pstep02aArgs'inputIndex) (pfromData pstep02aArgs'outputIndex) ownRef (pfromData ptxInfo'inputs) (pfromData ptxInfo'outputs) $ \_ _ _ inputState outputHash outputState ->
      let expected = pauthenticateControlV1 # pexpectStateAs @PAuthenticatedDescriptorV1 inputState # pfromData pstep02aArgs'machineState # pfromData pstep02aArgs'traceProof # pfromData pstep02aArgs'control
       in outputHash #== nextHash #&& outputState #== pforgetData (pdata expected)

unusedRedeemerStep02bValidator :: forall s. Term s (PAsData PScriptHash :--> PAsData PCurrencySymbol :--> PScriptContext :--> PUnit)
unusedRedeemerStep02bValidator = plam $ \nextHash threadPolicy ctx ->
  pstep ctx $ \datum redeemer ownRef tx -> pdispatch @_ @PStep02bArgs threadPolicy datum redeemer ownRef tx $ \args -> P.do
    PStep02bArgs{..} <- pmatch args
    PTxInfo{ptxInfo'inputs, ptxInfo'outputs} <- pmatch tx
    pcontinue threadPolicy (pexpectDatum datum) (pfromData pstep02bArgs'inputIndex) (pfromData pstep02bArgs'outputIndex) ownRef (pfromData ptxInfo'inputs) (pfromData ptxInfo'outputs) $ \_ _ _ inputState outputHash outputState ->
      let expected = pauthenticateItemHeaderV1 # pexpectStateAs @PAuthenticatedControlV1 inputState # pfromData pstep02bArgs'itemControl # pfromData pstep02bArgs'chunkProof # pfromData pstep02bArgs'nextChunkProof
       in outputHash #== nextHash #&& outputState #== pforgetData (pdata expected)

unusedRedeemerStep02cValidator :: forall s. Term s (PAsData PScriptHash :--> PAsData PCurrencySymbol :--> PScriptContext :--> PUnit)
unusedRedeemerStep02cValidator = plam $ \nextHash threadPolicy ctx ->
  pstep ctx $ \datum redeemer ownRef tx -> pdispatch @_ @PStep02cArgs threadPolicy datum redeemer ownRef tx $ \args -> P.do
    PStep02cArgs{..} <- pmatch args
    PTxInfo{ptxInfo'inputs, ptxInfo'outputs} <- pmatch tx
    pcontinue threadPolicy (pexpectDatum datum) (pfromData pstep02cArgs'inputIndex) (pfromData pstep02cArgs'outputIndex) ownRef (pfromData ptxInfo'inputs) (pfromData ptxInfo'outputs) $ \_ _ _ inputState outputHash outputState ->
      let expected = pauthenticateItemTailV1 # pexpectStateAs @PAuthenticatedItemHeaderV1 inputState # pfromData pstep02cArgs'chunkProof # pfromData pstep02cArgs'nextChunkProof
       in outputHash #== nextHash #&& outputState #== pforgetData (pdata expected)

unusedRedeemerStep03Validator :: forall s. Term s (PAsData PScriptHash :--> PAsData PCurrencySymbol :--> PScriptContext :--> PUnit)
unusedRedeemerStep03Validator = plam $ \nextHash threadPolicy ctx ->
  pstep ctx $ \datum redeemer ownRef tx -> pdispatch @_ @PLinearArgs threadPolicy datum redeemer ownRef tx $ \args -> P.do
    PLinearArgs{..} <- pmatch args
    PTxInfo{ptxInfo'inputs, ptxInfo'outputs} <- pmatch tx
    pcontinue threadPolicy (pexpectDatum datum) (pfromData plinearArgs'inputIndex) (pfromData plinearArgs'outputIndex) ownRef (pfromData ptxInfo'inputs) (pfromData ptxInfo'outputs) $ \_ _ _ inputState outputHash outputState ->
      let expected = pinitialReverseScanV1 # pexpectStateAs @PAuthenticatedRedeemerV1 inputState
       in outputHash #== nextHash #&& outputState #== pforgetData (pdata expected)

unusedRedeemerStep04Validator :: forall s. Term s (PAsData PScriptHash :--> PAsData PCurrencySymbol :--> PScriptContext :--> PUnit)
unusedRedeemerStep04Validator = plam $ \nextHash threadPolicy ctx ->
  pstep ctx $ \datum redeemer ownRef tx -> pdispatch @_ @PLinearArgs threadPolicy datum redeemer ownRef tx $ \args -> P.do
    PLinearArgs{..} <- pmatch args
    PTxInfo{ptxInfo'inputs, ptxInfo'outputs} <- pmatch tx
    pcontinue threadPolicy (pexpectDatum datum) (pfromData plinearArgs'inputIndex) (pfromData plinearArgs'outputIndex) ownRef (pfromData ptxInfo'inputs) (pfromData ptxInfo'outputs) $ \_ _ _ inputState outputHash outputState ->
      let expected = pexpectStateAs @PReverseScanV1 inputState
       in outputHash #== nextHash #&& outputState #== pforgetData (pdata expected)

unusedRedeemerStep05Validator :: forall s. Term s (PAsData PScriptHash :--> PAsData PCurrencySymbol :--> PScriptContext :--> PUnit)
unusedRedeemerStep05Validator = plam $ \step06Hash threadPolicy ctx ->
  pstep ctx $ \datum redeemer ownRef tx -> pdispatch @_ @PStep05Args threadPolicy datum redeemer ownRef tx $ \args -> P.do
    PStep05Args{..} <- pmatch args
    PTxInfo{ptxInfo'inputs, ptxInfo'outputs} <- pmatch tx
    let budget = pfromData pstep05Args'itemBudget
        openings = pfromData pstep05Args'openings
    pexpecting (budget #> 0 #&& budget #<= pmaximumScanBatch) $
      pcontinue threadPolicy (pexpectDatum datum) (pfromData pstep05Args'inputIndex) (pfromData pstep05Args'outputIndex) ownRef (pfromData ptxInfo'inputs) (pfromData ptxInfo'outputs) $ \ownHash _ _ inputState outputHash outputState -> P.do
        state <- plet $ pexpectStateAs @PReverseScanV1 inputState
        scanned <- plet $ padvance # state # openings # budget
        pif
          (preverseScanCompleteV1 # scanned)
          (let expected = pdecisionV1 # scanned in outputHash #== step06Hash #&& outputState #== pforgetData (pdata expected))
          (outputHash #== ownHash #&& plength # openings #== budget #&& outputState #== pforgetData (pdata scanned))

unusedRedeemerStep06Validator :: forall s. Term s (PAsData PCurrencySymbol :--> PAsData PCurrencySymbol :--> PAsData PAddress :--> PScriptContext :--> PUnit)
unusedRedeemerStep06Validator = plam $ \threadPolicy fraudPolicy fraudAddress ctx ->
  pstep ctx $ \datum redeemer ownRef tx -> pdispatch @_ @PStep06Args threadPolicy datum redeemer ownRef tx $ \args -> P.do
    PStep06Args{..} <- pmatch args
    PTxInfo{ptxInfo'inputs, ptxInfo'outputs, ptxInfo'redeemers} <- pmatch tx
    pfinalize threadPolicy fraudPolicy fraudAddress (pexpectDatum datum) (pfromData pstep06Args'inputIndex) (pfromData pstep06Args'outputIndex) (pfromData pstep06Args'fraudProofMintRedeemerIndex) ownRef (pfromData ptxInfo'inputs) (pfromData ptxInfo'outputs) (pto $ pto $ pfromData ptxInfo'redeemers) $ \_ _ _ inputState ->
      pterminalContradictionV1 # pexpectStateAs @PDecisionV1 inputState

padvance :: forall s. Term s (PReverseScanV1 :--> PBuiltinList (PAsData PSelectionOpeningV1) :--> PInteger :--> PReverseScanV1)
padvance = phoistAcyclic $ pfix $ \self -> plam $ \state openings budget ->
  pif
    (budget #== 0 #|| preverseScanCompleteV1 # state)
    state
    (pelimList (\opening rest -> self # (pscanSelectionV1 # state # pfromData opening) # rest # (budget - 1)) perror openings)
