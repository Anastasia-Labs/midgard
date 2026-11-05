module Midgard.Validators.FraudProofs.UnusedScriptWitness (
  unusedScriptWitnessStep01Validator,
  unusedScriptWitnessStep02Validator,
  unusedScriptWitnessStep03Validator,
  unusedScriptWitnessStep04Validator,
  unusedScriptWitnessStep05Validator,
  unusedScriptWitnessStep06Validator,
) where

import Plutarch.LedgerApi.V3 (PAddress, PCurrencySymbol, PScriptContext, PScriptHash, PTxInfo (..))
import Plutarch.Monadic qualified as P
import Plutarch.Prelude
import Plutarch.Unsafe (punsafeCoerce)

import Midgard.FraudProofs.Common (pcontinue, pfinalize, ppassNativeTxToNextStepCarried)
import Midgard.FraudProofs.NativeTx.Compact (pverifyNativeTxProofSourceV1)
import Midgard.FraudProofs.NativeTx.Types (PVerifiedMidgardNativeTxCompact (..))
import Midgard.FraudProofs.ProofThreadSubstrate qualified as Subject
import Midgard.FraudProofs.UnusedScriptWitness
import Midgard.LedgerState (PForcedInclusionTxV1 (..), PHeaderV1 (..), PNativeTxProofSourceV1 (..))
import Midgard.TransitionTrace (PRootMembershipProof (..))
import Midgard.Validators.FraudProofs.Step (pdispatch, pexpectDatum, pexpectStateAs, pexpecting, pstep)

unusedScriptWitnessStep01Validator :: forall s. Term s (PAsData PScriptHash :--> PAsData PCurrencySymbol :--> PAsData PScriptHash :--> PScriptContext :--> PUnit)
unusedScriptWitnessStep01Validator = plam $ \step02Hash threadPolicy hubOracle ctx ->
  pstep ctx $ \datum redeemer ownRef tx -> pdispatch @_ @PStep01Args threadPolicy datum redeemer ownRef tx $ \args -> P.do
    PStep01Args{..} <- pmatch args
    PTxInfo{ptxInfo'inputs, ptxInfo'outputs, ptxInfo'referenceInputs, ptxInfo'redeemers} <- pmatch tx
    pmatch (pfromData pstep01Args'source) $ \case
      PAcceptedSource inclusion ->
        ppassNativeTxToNextStepCarried threadPolicy hubOracle datum (pfromData inclusion) ownRef (pfromData ptxInfo'inputs) (pfromData ptxInfo'referenceInputs) (pfromData ptxInfo'outputs) (pto $ pto $ pfromData ptxInfo'redeemers) $ \_ _ _ _ outputHash outputState header _ verified -> P.do
          PHeaderV1{pheader'validationTracesRoot, pheader'validationTraceCount} <- pmatch $ pfromData header
          let expected = pbindWitnessV1 # (Subject.pbindAcceptedSubject # verified) # pfromData pheader'validationTracesRoot # pfromData pheader'validationTraceCount # pfromData pstep01Args'scriptIndex
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
          let expected = pbindWitnessV1 # subject # pfromData pheader'validationTracesRoot # pfromData pheader'validationTraceCount # pfromData pstep01Args'scriptIndex
          pverified'txId #== pfromData psubject'transactionId #&& outputHash #== step02Hash #&& outputState #== pforgetData (pdata expected)

unusedScriptWitnessStep02Validator :: forall s. Term s (PAsData PScriptHash :--> PAsData PCurrencySymbol :--> PScriptContext :--> PUnit)
unusedScriptWitnessStep02Validator = plam $ \nextHash threadPolicy ctx ->
  pstep ctx $ \datum redeemer ownRef tx -> pdispatch @_ @PStep02Args threadPolicy datum redeemer ownRef tx $ \args -> P.do
    PStep02Args{..} <- pmatch args
    PTxInfo{ptxInfo'inputs, ptxInfo'outputs} <- pmatch tx
    pcontinue threadPolicy (pexpectDatum datum) (pfromData pstep02Args'inputIndex) (pfromData pstep02Args'outputIndex) ownRef (pfromData ptxInfo'inputs) (pfromData ptxInfo'outputs) $ \_ _ _ inputState outputHash outputState ->
      let expected =
            pauthenticateWitnessV1
              # pexpectStateAs @PBoundWitnessV1 inputState
              # pfromData pstep02Args'traceMembership
              # pfromData pstep02Args'machineState
              # pfromData pstep02Args'traceProof
              # pfromData pstep02Args'control
              # pfromData pstep02Args'languageTag
              # pfromData pstep02Args'scriptHash
              # pfromData pstep02Args'totalLength
              # pfromData pstep02Args'itemCommitment
              # pfromData pstep02Args'sourceSiblings
       in outputHash #== nextHash #&& outputState #== pforgetData (pdata expected)

unusedScriptWitnessStep03Validator :: forall s. Term s (PAsData PScriptHash :--> PAsData PCurrencySymbol :--> PScriptContext :--> PUnit)
unusedScriptWitnessStep03Validator = plam $ \nextHash threadPolicy ctx ->
  pstep ctx $ \datum redeemer ownRef tx -> pdispatch @_ @PLinearArgs threadPolicy datum redeemer ownRef tx $ \args -> P.do
    PLinearArgs{..} <- pmatch args
    PTxInfo{ptxInfo'inputs, ptxInfo'outputs} <- pmatch tx
    pcontinue threadPolicy (pexpectDatum datum) (pfromData plinearArgs'inputIndex) (pfromData plinearArgs'outputIndex) ownRef (pfromData ptxInfo'inputs) (pfromData ptxInfo'outputs) $ \_ _ _ inputState outputHash outputState ->
      let expected = pinitialReverseScanV1 # pexpectStateAs @PAuthenticatedWitnessV1 inputState
       in outputHash #== nextHash #&& outputState #== pforgetData (pdata expected)

unusedScriptWitnessStep04Validator :: forall s. Term s (PAsData PScriptHash :--> PAsData PCurrencySymbol :--> PScriptContext :--> PUnit)
unusedScriptWitnessStep04Validator = plam $ \step05Hash threadPolicy ctx ->
  pstep ctx $ \datum redeemer ownRef tx -> pdispatch @_ @PStep04Args threadPolicy datum redeemer ownRef tx $ \args -> P.do
    PStep04Args{..} <- pmatch args
    PTxInfo{ptxInfo'inputs, ptxInfo'outputs} <- pmatch tx
    let budget = pfromData pstep04Args'itemBudget
        openings = pfromData pstep04Args'openings
    pexpecting (budget #> 0 #&& budget #<= pmaximumScanBatch) $
      pcontinue threadPolicy (pexpectDatum datum) (pfromData pstep04Args'inputIndex) (pfromData pstep04Args'outputIndex) ownRef (pfromData ptxInfo'inputs) (pfromData ptxInfo'outputs) $ \ownHash _ _ inputState outputHash outputState -> P.do
        state <- plet $ pexpectStateAs @PReverseScanV1 inputState
        scanned <- plet $ padvanceSources # state # openings # budget
        pif
          (palternateSourcesCompleteV1 # scanned)
          (outputHash #== step05Hash #&& outputState #== pforgetData (pdata scanned))
          (outputHash #== ownHash #&& plength # openings #== budget #&& outputState #== pforgetData (pdata scanned))

unusedScriptWitnessStep05Validator :: forall s. Term s (PAsData PScriptHash :--> PAsData PCurrencySymbol :--> PScriptContext :--> PUnit)
unusedScriptWitnessStep05Validator = plam $ \step06Hash threadPolicy ctx ->
  pstep ctx $ \datum redeemer ownRef tx -> pdispatch @_ @PStep05Args threadPolicy datum redeemer ownRef tx $ \args -> P.do
    PStep05Args{..} <- pmatch args
    PTxInfo{ptxInfo'inputs, ptxInfo'outputs} <- pmatch tx
    let budget = pfromData pstep05Args'itemBudget
        openings = pfromData pstep05Args'openings
    pexpecting (budget #> 0 #&& budget #<= pmaximumScanBatch) $
      pcontinue threadPolicy (pexpectDatum datum) (pfromData pstep05Args'inputIndex) (pfromData pstep05Args'outputIndex) ownRef (pfromData ptxInfo'inputs) (pfromData ptxInfo'outputs) $ \ownHash _ _ inputState outputHash outputState -> P.do
        state <- plet $ pexpectStateAs @PReverseScanV1 inputState
        scanned <- plet $ padvancePurposes # state # openings # budget
        pif
          (preverseScanCompleteV1 # scanned)
          (let expected = pdecisionV1 # scanned in outputHash #== step06Hash #&& outputState #== pforgetData (pdata expected))
          (outputHash #== ownHash #&& plength # openings #== budget #&& outputState #== pforgetData (pdata scanned))

unusedScriptWitnessStep06Validator :: forall s. Term s (PAsData PCurrencySymbol :--> PAsData PCurrencySymbol :--> PAsData PAddress :--> PScriptContext :--> PUnit)
unusedScriptWitnessStep06Validator = plam $ \threadPolicy fraudPolicy fraudAddress ctx ->
  pstep ctx $ \datum redeemer ownRef tx -> pdispatch @_ @PStep06Args threadPolicy datum redeemer ownRef tx $ \args -> P.do
    PStep06Args{..} <- pmatch args
    PTxInfo{ptxInfo'inputs, ptxInfo'outputs, ptxInfo'redeemers} <- pmatch tx
    pfinalize threadPolicy fraudPolicy fraudAddress (pexpectDatum datum) (pfromData pstep06Args'inputIndex) (pfromData pstep06Args'outputIndex) (pfromData pstep06Args'fraudProofMintRedeemerIndex) ownRef (pfromData ptxInfo'inputs) (pfromData ptxInfo'outputs) (pto $ pto $ pfromData ptxInfo'redeemers) $ \_ _ _ inputState ->
      pterminalContradictionV1 # pexpectStateAs @PDecisionV1 inputState

padvanceSources :: forall s. Term s (PReverseScanV1 :--> PBuiltinList (PAsData PSourceOpeningV1) :--> PInteger :--> PReverseScanV1)
padvanceSources = phoistAcyclic $ pfix $ \self -> plam $ \state openings budget ->
  pif
    (budget #== 0 #|| palternateSourcesCompleteV1 # state)
    state
    (pelimList (\opening rest -> self # (pauthenticateEarlierSourceV1 # state # pfromData opening) # rest # (budget - 1)) perror openings)

padvancePurposes :: forall s. Term s (PReverseScanV1 :--> PBuiltinList (PAsData PPurposeOpeningV1) :--> PInteger :--> PReverseScanV1)
padvancePurposes = phoistAcyclic $ pfix $ \self -> plam $ \state openings budget ->
  pif
    (budget #== 0 #|| preverseScanCompleteV1 # state)
    state
    (pelimList (\opening rest -> self # (pscanPurposeV1 # state # pfromData opening) # rest # (budget - 1)) perror openings)
