module Midgard.Validators.FraudProofs.DistinctAssetAccumulationLimit (
  distinctAssetAccumulationLimitStep01Validator,
  distinctAssetAccumulationLimitStep02Validator,
  distinctAssetAccumulationLimitStep03Validator,
  distinctAssetAccumulationLimitStep04Validator,
  distinctAssetAccumulationLimitStep05Validator,
  distinctAssetAccumulationLimitStep06Validator,
) where

import Plutarch.LedgerApi.V3 (PAddress, PCurrencySymbol, PScriptContext, PScriptHash, PTxInfo (..))
import Plutarch.Monadic qualified as P
import Plutarch.Prelude
import Plutarch.Unsafe (punsafeCoerce)

import Midgard.FraudProofs.Common (pcontinue, pfinalize, ppassNativeTxToNextStepCarried)
import Midgard.FraudProofs.DistinctAssetAccumulationLimit
import Midgard.FraudProofs.NativeTx.Compact (pverifyNativeTxProofSourceV1)
import Midgard.FraudProofs.NativeTx.Types (PVerifiedMidgardNativeTxCompact (..))
import Midgard.FraudProofs.ProofThreadSubstrate qualified as Subject
import Midgard.LedgerState (PForcedInclusionTxV1 (..), PHeaderV1 (..), PNativeTxProofSourceV1 (..))
import Midgard.TransitionTrace (PRootMembershipProof (..))
import Midgard.Validators.FraudProofs.Step (pdispatch, pexpectDatum, pexpectStateAs, pstep)

distinctAssetAccumulationLimitStep01Validator :: forall s. Term s (PAsData PScriptHash :--> PAsData PCurrencySymbol :--> PAsData PScriptHash :--> PScriptContext :--> PUnit)
distinctAssetAccumulationLimitStep01Validator = plam $ \step02Hash threadPolicy hubOracle ctx ->
  pstep ctx $ \datum redeemer ownRef tx ->
    pdispatch @_ @PStep01Args threadPolicy datum redeemer ownRef tx $ \args -> P.do
      PStep01Args{..} <- pmatch args
      PTxInfo{ptxInfo'inputs, ptxInfo'outputs, ptxInfo'referenceInputs, ptxInfo'redeemers} <- pmatch tx
      pmatch (pfromData pstep01Args'source) $ \case
        PAcceptedSource inclusion ->
          ppassNativeTxToNextStepCarried threadPolicy hubOracle datum (pfromData inclusion) ownRef (pfromData ptxInfo'inputs) (pfromData ptxInfo'referenceInputs) (pfromData ptxInfo'outputs) (pto $ pto $ pfromData ptxInfo'redeemers) $ \_ _ _ _ outputHash outputState header _ verified -> P.do
            PHeaderV1{pheader'validationTracesRoot, pheader'validationTraceCount} <- pmatch $ pfromData header
            let expected = pbindCoordinateV1 # (Subject.pbindAcceptedSubject # verified) # pfromData pheader'validationTracesRoot # pfromData pheader'validationTraceCount # pfromData pstep01Args'coordinate
            outputHash #== step02Hash #&& outputState #== pforgetData (pdata expected)
        PForcedSource inputIndex outputIndex header membership direction ->
          pcontinue threadPolicy (pexpectDatum datum) (pfromData inputIndex) (pfromData outputIndex) ownRef (pfromData ptxInfo'inputs) (pfromData ptxInfo'outputs) $ \_ threadName _ _ outputHash outputState -> P.do
            subject <- plet $ Subject.pbindForcedSubjectToThread # pto (pfromData threadName) # pfromData header # pfromData membership # pfromData direction
            Subject.PVerdictSubject{Subject.psubject'transactionId} <- pmatch subject
            PRootMembershipProof{prootMembership'value} <- pmatch $ pfromData membership
            PForcedInclusionTxV1{pforcedTx'txId, pforcedTx'source} <- pmatch $ pfromData $ punsafeCoerce @(PAsData PForcedInclusionTxV1) prootMembership'value
            PNativeTxProofSourceV1{..} <- pmatch $ pfromData pforcedTx'source
            PPair verified _ <- pmatch $ pverifyNativeTxProofSourceV1 # pfromData pforcedTx'txId # pfromData pnativeSource'compactCbor # pfromData pnativeSource'witnessSetCompactCbor # pfromData pnativeSource'fieldPreimageLengthsCbor
            PVerifiedMidgardNativeTxCompact{pverified'txId} <- pmatch verified
            PHeaderV1{pheader'validationTracesRoot, pheader'validationTraceCount} <- pmatch $ pfromData header
            let expected = pbindCoordinateV1 # subject # pfromData pheader'validationTracesRoot # pfromData pheader'validationTraceCount # pfromData pstep01Args'coordinate
            pverified'txId #== pfromData psubject'transactionId #&& outputHash #== step02Hash #&& outputState #== pforgetData (pdata expected)

distinctAssetAccumulationLimitStep02Validator :: forall s. Term s (PAsData PScriptHash :--> PAsData PCurrencySymbol :--> PScriptContext :--> PUnit)
distinctAssetAccumulationLimitStep02Validator = plam $ \step03Hash threadPolicy ctx ->
  pstep ctx $ \datum redeemer ownRef tx -> pdispatch @_ @PStep02Args threadPolicy datum redeemer ownRef tx $ \args -> P.do
    PStep02Args{..} <- pmatch args
    PTxInfo{ptxInfo'inputs, ptxInfo'outputs} <- pmatch tx
    pcontinue threadPolicy (pexpectDatum datum) (pfromData pstep02Args'inputIndex) (pfromData pstep02Args'outputIndex) ownRef (pfromData ptxInfo'inputs) (pfromData ptxInfo'outputs) $ \_ _ _ inputState outputHash outputState ->
      let expected = pinitializeAccumulatorV1 # pexpectStateAs @PBoundV1 inputState # pfromData pstep02Args'traceMembership # pfromData pstep02Args'pre # pfromData pstep02Args'traceProof # pfromData pstep02Args'control
       in outputHash #== step03Hash #&& outputState #== pforgetData (pdata expected)

distinctAssetAccumulationLimitStep03Validator :: forall s. Term s (PAsData PScriptHash :--> PAsData PCurrencySymbol :--> PScriptContext :--> PUnit)
distinctAssetAccumulationLimitStep03Validator = plam $ \step04Hash threadPolicy ctx ->
  pstep ctx $ \datum redeemer ownRef tx -> pdispatch @_ @PInputActionV1 threadPolicy datum redeemer ownRef tx $ \action -> P.do
    PTxInfo{ptxInfo'inputs, ptxInfo'outputs} <- pmatch tx
    let inputIndex = pmatch action $ \case PInputSkip i _ -> pfromData i; PInputAuthenticate i _ _ -> pfromData i
        outputIndex = pmatch action $ \case PInputSkip _ o -> pfromData o; PInputAuthenticate _ o _ -> pfromData o
    pcontinue threadPolicy (pexpectDatum datum) inputIndex outputIndex ownRef (pfromData ptxInfo'inputs) (pfromData ptxInfo'outputs) $ \_ _ _ inputState outputHash outputState ->
      let state = pexpectStateAs @PFoldStateV1 inputState
          advanced = pmatch action $ \case
            PInputSkip _ _ -> pskipFoldV1 # state # 0
            PInputAuthenticate _ _ evidence -> pmatch (pfromData evidence) $ \PInputEvidenceV1{..} -> pauthenticateInputFoldV1 # state # pfromData pinputEvidence'sourceKind # pfromData pinputEvidence'key # pfromData pinputEvidence'nextScheduleHash # pfromData pinputEvidence'descriptorCbor # pfromData pinputEvidence'assetIndex # pfromData pinputEvidence'policyId # pfromData pinputEvidence'assetName # pfromData pinputEvidence'quantity # pfromData pinputEvidence'assetPeaks # pfromData pinputEvidence'assetSiblings # pfromData pinputEvidence'mutation
       in outputHash #== step04Hash #&& outputState #== pforgetData (pdata advanced)

distinctAssetAccumulationLimitStep04Validator :: forall s. Term s (PAsData PScriptHash :--> PAsData PCurrencySymbol :--> PScriptContext :--> PUnit)
distinctAssetAccumulationLimitStep04Validator = plam $ \step05Hash threadPolicy ctx ->
  pstep ctx $ \datum redeemer ownRef tx -> pdispatch @_ @POutputActionV1 threadPolicy datum redeemer ownRef tx $ \action -> P.do
    PTxInfo{ptxInfo'inputs, ptxInfo'outputs} <- pmatch tx
    let inputIndex = pmatch action $ \case POutputSkip i _ -> pfromData i; POutputAuthenticate i _ _ -> pfromData i
        outputIndex = pmatch action $ \case POutputSkip _ o -> pfromData o; POutputAuthenticate _ o _ -> pfromData o
    pcontinue threadPolicy (pexpectDatum datum) inputIndex outputIndex ownRef (pfromData ptxInfo'inputs) (pfromData ptxInfo'outputs) $ \_ _ _ inputState outputHash outputState ->
      let state = pexpectStateAs @PFoldStateV1 inputState
          advanced = pmatch action $ \case
            POutputSkip _ _ -> pskipFoldV1 # state # 1
            POutputAuthenticate _ _ evidence -> pmatch (pfromData evidence) $ \POutputEvidenceV1{..} -> pauthenticateOutputFoldV1 # state # pfromData poutputEvidence'outputIndex # pfromData poutputEvidence'descriptorCbor # pfromData poutputEvidence'assetIndex # pfromData poutputEvidence'policyId # pfromData poutputEvidence'assetName # pfromData poutputEvidence'quantity # pfromData poutputEvidence'assetPeaks # pfromData poutputEvidence'assetSiblings # pfromData poutputEvidence'mutation
       in outputHash #== step05Hash #&& outputState #== pforgetData (pdata advanced)

distinctAssetAccumulationLimitStep05Validator :: forall s. Term s (PAsData PScriptHash :--> PAsData PCurrencySymbol :--> PScriptContext :--> PUnit)
distinctAssetAccumulationLimitStep05Validator = plam $ \step06Hash threadPolicy ctx ->
  pstep ctx $ \datum redeemer ownRef tx -> pdispatch @_ @PMintActionV1 threadPolicy datum redeemer ownRef tx $ \action -> P.do
    PTxInfo{ptxInfo'inputs, ptxInfo'outputs} <- pmatch tx
    let inputIndex = pmatch action $ \case PMintSkip i _ -> pfromData i; PMintAuthenticate i _ _ -> pfromData i
        outputIndex = pmatch action $ \case PMintSkip _ o -> pfromData o; PMintAuthenticate _ o _ -> pfromData o
    pcontinue threadPolicy (pexpectDatum datum) inputIndex outputIndex ownRef (pfromData ptxInfo'inputs) (pfromData ptxInfo'outputs) $ \_ _ _ inputState outputHash outputState ->
      let state = pexpectStateAs @PFoldStateV1 inputState
          advanced = pmatch action $ \case
            PMintSkip _ _ -> pskipFoldV1 # state # 2
            PMintAuthenticate _ _ evidence -> pmatch (pfromData evidence) $ \PMintEvidenceV1{..} -> pauthenticateMintFoldV1 # state # pfromData pmintEvidence'mintIndex # pfromData pmintEvidence'policyId # pfromData pmintEvidence'assetName # pfromData pmintEvidence'quantity # pfromData pmintEvidence'siblings # pfromData pmintEvidence'mutation
       in outputHash #== step06Hash #&& outputState #== pforgetData (pdata advanced)

distinctAssetAccumulationLimitStep06Validator :: forall s. Term s (PAsData PCurrencySymbol :--> PAsData PCurrencySymbol :--> PAsData PAddress :--> PScriptContext :--> PUnit)
distinctAssetAccumulationLimitStep06Validator = plam $ \threadPolicy fraudPolicy fraudAddress ctx ->
  pstep ctx $ \datum redeemer ownRef tx -> pdispatch @_ @PStep06Args threadPolicy datum redeemer ownRef tx $ \args -> P.do
    PStep06Args{..} <- pmatch args
    PTxInfo{ptxInfo'inputs, ptxInfo'outputs, ptxInfo'redeemers} <- pmatch tx
    pfinalize threadPolicy fraudPolicy fraudAddress (pexpectDatum datum) (pfromData pstep06Args'inputIndex) (pfromData pstep06Args'outputIndex) (pfromData pstep06Args'fraudProofMintRedeemerIndex) ownRef (pfromData ptxInfo'inputs) (pfromData ptxInfo'outputs) (pto $ pto $ pfromData ptxInfo'redeemers) $ \_ _ _ inputState -> pterminalContradictionV1 # pexpectStateAs @PFoldStateV1 inputState
