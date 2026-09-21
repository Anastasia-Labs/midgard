module Midgard.Validators.FraudProofs.ExecutionSourceScriptDecoding (
    executionSourceScriptDecodingStep01Validator,
    executionSourceScriptDecodingStep02Validator,
    executionSourceScriptDecodingStep03Validator,
    executionSourceScriptDecodingStep04Validator,
    executionSourceScriptDecodingStep05Validator,
) where

import Plutarch.Core.Utils ((#/=))
import Plutarch.LedgerApi.Utils (PMaybeData (..))
import Plutarch.LedgerApi.V3 (PAddress, PCurrencySymbol, PScriptContext, PScriptHash, PTxInfo (..))
import Plutarch.Monadic qualified as P
import Plutarch.Prelude
import Plutarch.Unsafe (punsafeCoerce)

import Midgard.FraudProofs.Common (pcontinue, pfinalize, ppassNativeTxToNextStepCarried)
import Midgard.FraudProofs.ExecutionSourceScriptDecoding
import Midgard.FraudProofs.NativeScriptDecoding.Engine qualified as Engine
import Midgard.FraudProofs.NativeTx.Compact (pverifyNativeTxProofSourceV1)
import Midgard.FraudProofs.NativeTx.Types (PVerifiedMidgardNativeTxCompact (..))
import Midgard.FraudProofs.ProofThreadSubstrate qualified as Subject
import Midgard.LedgerState (PForcedInclusionTxV1 (..), PHeaderV1 (..), PNativeTxProofSourceV1 (..))
import Midgard.NativeScriptScan qualified as Scan
import Midgard.TransitionTrace (PRootMembershipProof (..))
import Midgard.Validators.FraudProofs.Step (pdispatch, pexpectDatum, pexpectStateAs, pstep)

executionSourceScriptDecodingStep01Validator :: forall s. Term s (PAsData PScriptHash :--> PAsData PCurrencySymbol :--> PAsData PScriptHash :--> PScriptContext :--> PUnit)
executionSourceScriptDecodingStep01Validator = plam $ \step02Hash threadPolicy hubOracle ctx ->
    pstep ctx $ \datum redeemer ownRef tx -> pdispatch @_ @PStep01Args threadPolicy datum redeemer ownRef tx $ \args -> P.do
        PStep01Args{pstep01Args'source, pstep01Args'executionIndex} <- pmatch args
        PTxInfo{ptxInfo'inputs, ptxInfo'outputs, ptxInfo'referenceInputs, ptxInfo'redeemers} <- pmatch tx
        pmatch (pfromData pstep01Args'source) $ \case
            PAcceptedSource inclusion ->
                ppassNativeTxToNextStepCarried threadPolicy hubOracle datum (pfromData inclusion) ownRef (pfromData ptxInfo'inputs) (pfromData ptxInfo'referenceInputs) (pfromData ptxInfo'outputs) (pto $ pto $ pfromData ptxInfo'redeemers) $ \_ _ _ _ outputHash outputState header _ verified -> P.do
                    PHeaderV1{pheader'validationTracesRoot, pheader'validationTraceCount} <- pmatch $ pfromData header
                    let expected = pbindExecutionV1 # (Subject.pbindAcceptedSubject # verified) # pfromData pheader'validationTracesRoot # pfromData pheader'validationTraceCount # pfromData pstep01Args'executionIndex
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
                    let expected = pbindExecutionV1 # subject # pfromData pheader'validationTracesRoot # pfromData pheader'validationTraceCount # pfromData pstep01Args'executionIndex
                    pverified'txId #== pfromData psubject'transactionId #&& outputHash #== step02Hash #&& outputState #== pforgetData (pdata expected)

executionSourceScriptDecodingStep02Validator :: forall s. Term s (PAsData PScriptHash :--> PAsData PCurrencySymbol :--> PScriptContext :--> PUnit)
executionSourceScriptDecodingStep02Validator = plam $ \step03Hash threadPolicy ctx ->
    pstep ctx $ \datum redeemer ownRef tx -> pdispatch @_ @PStep02Args threadPolicy datum redeemer ownRef tx $ \args -> P.do
        PStep02Args{..} <- pmatch args
        PTxInfo{ptxInfo'inputs, ptxInfo'outputs} <- pmatch tx
        pcontinue threadPolicy (pexpectDatum datum) (pfromData pstep02Args'inputIndex) (pfromData pstep02Args'outputIndex) ownRef (pfromData ptxInfo'inputs) (pfromData ptxInfo'outputs) $ \_ _ _ inputState outputHash outputState ->
            let expected =
                    pauthenticateExecutionSourceV1
                        # pexpectStateAs @PBoundExecutionV1 inputState
                        # pfromData pstep02Args'traceMembership
                        # pfromData pstep02Args'machineState
                        # pfromData pstep02Args'traceProof
                        # pfromData pstep02Args'control
                        # pfromData pstep02Args'purposeKind
                        # pfromData pstep02Args'purposeIndex
                        # pfromData pstep02Args'scriptHash
                        # pfromData pstep02Args'purposeSubject
                        # pfromData pstep02Args'purposeSiblings
                        # pfromData pstep02Args'sourceIndex
                        # pfromData pstep02Args'originKind
                        # pfromData pstep02Args'sourceKey
                        # pfromData pstep02Args'languageTag
                        # pfromData pstep02Args'totalLength
                        # pfromData pstep02Args'itemCommitment
                        # pfromData pstep02Args'sourceSiblings
                        # pfromData pstep02Args'redeemerLeaf
                        # pfromData pstep02Args'executionSiblings
             in outputHash #== step03Hash #&& outputState #== pforgetData (pdata expected)

executionSourceScriptDecodingStep03Validator :: forall s. Term s (PAsData PScriptHash :--> PAsData PCurrencySymbol :--> PScriptContext :--> PUnit)
executionSourceScriptDecodingStep03Validator = plam $ \step04Hash threadPolicy ctx ->
    pstep ctx $ \datum redeemer ownRef tx -> pdispatch @_ @PStep03Args threadPolicy datum redeemer ownRef tx $ \args -> P.do
        PStep03Args{..} <- pmatch args
        PTxInfo{ptxInfo'inputs, ptxInfo'outputs} <- pmatch tx
        pcontinue threadPolicy (pexpectDatum datum) (pfromData pstep03Args'inputIndex) (pfromData pstep03Args'outputIndex) ownRef (pfromData ptxInfo'inputs) (pfromData ptxInfo'outputs) $ \_ _ _ inputState outputHash outputState ->
            let expected = pbindExactItemV1 # pexpectStateAs @PAuthenticatedExecutionSourceV1 inputState # pfromData pstep03Args'firstChunk # pto (pfromData step04Hash)
             in outputHash #== step04Hash #&& outputState #== pforgetData (pdata expected)

executionSourceScriptDecodingStep04Validator :: forall s. Term s (PAsData PScriptHash :--> PAsData PCurrencySymbol :--> PScriptContext :--> PUnit)
executionSourceScriptDecodingStep04Validator = plam $ \step05Hash threadPolicy ctx ->
    pstep ctx $ \datum redeemer ownRef tx -> pdispatch @_ @PStep04Args threadPolicy datum redeemer ownRef tx $ \args -> P.do
        PStep04Args{..} <- pmatch args
        PTxInfo{ptxInfo'inputs, ptxInfo'outputs} <- pmatch tx
        pcontinue threadPolicy (pexpectDatum datum) (pfromData pstep04Args'inputIndex) (pfromData pstep04Args'outputIndex) ownRef (pfromData ptxInfo'inputs) (pfromData ptxInfo'outputs) $ \ownHash _ _ inputState outputHash outputState -> P.do
            state <- plet $ pexpectStateAs @PExecutionSourceScanStateV1 inputState
            PExecutionSourceScanStateV1{..} <- pmatch state
            let closeWith resultClass =
                    pif
                        (outputHash #== step05Hash)
                        (pclosedStateV1 # state # resultClass # pto (pfromData step05Hash))
                        perror
                expected =
                    pif
                        (pfromData pscanState'resultClass #/= presultPending)
                        ( pif
                            ( pfromData pstep04Args'controlCbor
                                #== pconstant ""
                                #&& pfromData pstep04Args'chunkProof
                                #== pcon PDNothing
                                #&& pfromData pstep04Args'nextChunkProof
                                #== pcon PDNothing
                                #&& pnull
                                # pfromData pstep04Args'frames
                                #&& pfromData pstep04Args'stepBudget
                                #== 0
                            )
                            (closeWith $ pfromData pscanState'resultClass)
                            perror
                        )
                        ( pif
                            (pfromData pstep04Args'stepBudget #> 0 #&& pfromData pstep04Args'controlCbor #== pfromData pscanState'controlCbor)
                            ( let control = Scan.pdecodeStructureControlV1 # pfromData pstep04Args'controlCbor
                                  window = pmatch (pfromData pstep04Args'chunkProof) $ \case
                                    PDNothing -> pif (pfromData pstep04Args'nextChunkProof #== pcon PDNothing) (pcon PNothing) perror
                                    PDJust proof -> pcon $ PJust $ pauthenticatedWindowV1 # state # control # pfromData proof # pfromData pstep04Args'nextChunkProof
                               in pmatch (Engine.pbudgetedScanV1 # control # window # pfromData pstep04Args'frames # pfromData pstep04Args'stepBudget) $ \case
                                    Engine.PScanRefusedV1 refusal -> closeWith $ pmappedRefusalClassV1 # pfromData refusal
                                    Engine.PScanAdvancedV1 nextControlData ->
                                        let nextControl = pfromData nextControlData
                                         in pif
                                                (Scan.pstructureTerminalIsExactV1 # nextControl)
                                                (closeWith presultNoFault)
                                                ( pif
                                                    (outputHash #== ownHash)
                                                    ( plet (padvancedStateV1 # state # nextControl # pto (pfromData ownHash)) $ \advanced ->
                                                        pmatch advanced $ \PExecutionSourceScanStateV1{pscanState'checkpointHash = advancedCheckpoint} ->
                                                            pif (advancedCheckpoint #/= pscanState'checkpointHash) advanced perror
                                                    )
                                                    perror
                                                )
                            )
                            perror
                        )
            pif
                (pstateIsAuthenticV1 # state #&& pfromData pscanState'nextExpectedScriptHash #== pto (pfromData ownHash))
                (outputState #== pforgetData (pdata expected))
                perror

executionSourceScriptDecodingStep05Validator :: forall s. Term s (PAsData PCurrencySymbol :--> PAsData PCurrencySymbol :--> PAsData PAddress :--> PScriptContext :--> PUnit)
executionSourceScriptDecodingStep05Validator = plam $ \threadPolicy fraudPolicy fraudAddress ctx ->
    pstep ctx $ \datum redeemer ownRef tx -> pdispatch @_ @PStep05Args threadPolicy datum redeemer ownRef tx $ \args -> P.do
        PStep05Args{..} <- pmatch args
        PTxInfo{ptxInfo'inputs, ptxInfo'outputs, ptxInfo'redeemers} <- pmatch tx
        pfinalize threadPolicy fraudPolicy fraudAddress (pexpectDatum datum) (pfromData pstep05Args'inputIndex) (pfromData pstep05Args'outputIndex) (pfromData pstep05Args'fraudProofMintRedeemerIndex) ownRef (pfromData ptxInfo'inputs) (pfromData ptxInfo'outputs) (pto $ pto $ pfromData ptxInfo'redeemers) $ \_ _ _ inputState ->
            pterminalContradictionV1 # pexpectStateAs @PExecutionSourceScanStateV1 inputState
