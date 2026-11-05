module Midgard.Validators.FraudProofs.OutputReferenceScriptDecoding (
    outputReferenceScriptDecodingStep01Validator,
    outputReferenceScriptDecodingStep02Validator,
    outputReferenceScriptDecodingStep03Validator,
    outputReferenceScriptDecodingStep04Validator,
    outputReferenceScriptDecodingStep05Validator,
    outputReferenceScriptDecodingStep06Validator,
) where

import Plutarch.Core.Utils ((#/=))
import Plutarch.LedgerApi.Utils (PMaybeData (..))
import Plutarch.LedgerApi.V3 (PAddress, PCurrencySymbol, PScriptContext, PScriptHash, PTxInfo (..))
import Plutarch.Monadic qualified as P
import Plutarch.Prelude

import Midgard.FraudProofs.Common (pcontinue, pfinalize, ppassNativeTxToNextStepCarried)
import Midgard.FraudProofs.FieldOpening (PNativeTxAnchorV1 (PBodyAnchor), popenedFieldView)
import Midgard.FraudProofs.NativeScriptDecoding.Engine qualified as Engine
import Midgard.FraudProofs.OutputReferenceScriptDecoding
import Midgard.FraudProofs.ProofThreadSubstrate qualified as Subject
import Midgard.NativeScriptScan qualified as Scan
import Midgard.NativeTxFieldAccess (pfieldItemAt)
import Midgard.Validators.FraudProofs.Step (pdispatch, pexpectDatum, pexpectStateAs, pexpecting, pstep)

outputReferenceScriptDecodingStep01Validator :: forall s. Term s (PAsData PScriptHash :--> PAsData PCurrencySymbol :--> PAsData PScriptHash :--> PScriptContext :--> PUnit)
outputReferenceScriptDecodingStep01Validator = plam $ \step02Hash threadPolicy hubOracle ctx ->
    pstep ctx $ \datum redeemer ownRef tx -> pdispatch @_ @PStep01Args threadPolicy datum redeemer ownRef tx $ \args -> P.do
        PStep01Args{pstep01Args'source, pstep01Args'outputIndex} <- pmatch args
        PTxInfo{ptxInfo'inputs, ptxInfo'outputs, ptxInfo'referenceInputs, ptxInfo'redeemers} <- pmatch tx
        pmatch (pfromData pstep01Args'source) $ \case
            PAcceptedSource inclusion ->
                ppassNativeTxToNextStepCarried threadPolicy hubOracle datum (pfromData inclusion) ownRef (pfromData ptxInfo'inputs) (pfromData ptxInfo'referenceInputs) (pfromData ptxInfo'outputs) (pto $ pto $ pfromData ptxInfo'redeemers) $ \_ _ _ _ outputHash outputState _ _ verified ->
                    let expected = pbindOutputV1 # (Subject.pbindAcceptedSubject # verified) # pfromData pstep01Args'outputIndex
                     in outputHash #== step02Hash #&& outputState #== pforgetData (pdata expected)
            PForcedSource inputIndex outputIndex header membership direction ->
                pcontinue threadPolicy (pexpectDatum datum) (pfromData inputIndex) (pfromData outputIndex) ownRef (pfromData ptxInfo'inputs) (pfromData ptxInfo'outputs) $ \_ threadName _ _ outputHash outputState -> P.do
                    subject <- plet $ Subject.pbindForcedSubjectToThread # pto (pfromData threadName) # pfromData header # pfromData membership # pfromData direction
                    let expected = pbindOutputV1 # subject # pfromData pstep01Args'outputIndex
                    outputHash #== step02Hash #&& outputState #== pforgetData (pdata expected)

outputReferenceScriptDecodingStep02Validator :: forall s. Term s (PAsData PScriptHash :--> PAsData PCurrencySymbol :--> PAsData PCurrencySymbol :--> PScriptContext :--> PUnit)
outputReferenceScriptDecodingStep02Validator = plam $ \step03Hash threadPolicy certificatePolicy ctx ->
    pstep ctx $ \datum redeemer ownRef tx -> pdispatch @_ @PStep02Args threadPolicy datum redeemer ownRef tx $ \args -> P.do
        PStep02Args{..} <- pmatch args
        PTxInfo{ptxInfo'inputs, ptxInfo'outputs, ptxInfo'referenceInputs} <- pmatch tx
        pcontinue threadPolicy (pexpectDatum datum) (pfromData pstep02Args'inputIndex) (pfromData pstep02Args'outputIndex) ownRef (pfromData ptxInfo'inputs) (pfromData ptxInfo'outputs) $ \_ _ _ inputState outputHash outputState -> P.do
            bound@PBoundOutputV1{pboundOutput'subject, pboundOutput'outputIndex} <- pmatch $ pexpectStateAs @PBoundOutputV1 inputState
            Subject.PVerdictSubject{Subject.psubject'transactionId} <- pmatch $ pfromData pboundOutput'subject
            view <- plet $ popenedFieldView # pfromData pstep02Args'opening # pcon (PBodyAnchor psubject'transactionId) # poutputsFieldIndex # pfromData ptxInfo'referenceInputs # certificatePolicy
            let expected = pinitialOutputScanV1 # pcon bound # (pfieldItemAt # view # pfromData pboundOutput'outputIndex)
            outputHash #== step03Hash #&& outputState #== pforgetData (pdata expected)

outputReferenceScriptDecodingStep03Validator :: forall s. Term s (PAsData PScriptHash :--> PAsData PCurrencySymbol :--> PScriptContext :--> PUnit)
outputReferenceScriptDecodingStep03Validator = plam $ \step04Hash threadPolicy ctx ->
    pstep ctx $ \datum redeemer ownRef tx -> pdispatch @_ @PStep03Args threadPolicy datum redeemer ownRef tx $ \args -> P.do
        PStep03Args{..} <- pmatch args
        PTxInfo{ptxInfo'inputs, ptxInfo'outputs} <- pmatch tx
        pcontinue threadPolicy (pexpectDatum datum) (pfromData pstep03Args'inputIndex) (pfromData pstep03Args'outputIndex) ownRef (pfromData ptxInfo'inputs) (pfromData ptxInfo'outputs) $ \ownHash _ _ inputState outputHash outputState -> P.do
            advanced <- plet $ padvanceOutputScanV1 # pexpectStateAs @POutputDescriptorStateV1 inputState # pfromData pstep03Args'window
            POutputDescriptorStateV1{poutputDescriptor'outcome} <- pmatch advanced
            let outcome = pfromData poutputDescriptor'outcome
            (outcome #== poutputScanning #|| outcome #== poutputCanonical)
                #&& outputHash
                #== pif (outcome #== poutputCanonical) step04Hash ownHash
                #&& outputState
                #== pforgetData (pdata advanced)

outputReferenceScriptDecodingStep04Validator :: forall s. Term s (PAsData PScriptHash :--> PAsData PCurrencySymbol :--> PAsData PCurrencySymbol :--> PScriptContext :--> PUnit)
outputReferenceScriptDecodingStep04Validator = plam $ \step05Hash threadPolicy certificatePolicy ctx ->
    pstep ctx $ \datum redeemer ownRef tx -> pdispatch @_ @PStep04Args threadPolicy datum redeemer ownRef tx $ \args -> P.do
        PStep04Args{..} <- pmatch args
        PTxInfo{ptxInfo'inputs, ptxInfo'outputs, ptxInfo'referenceInputs} <- pmatch tx
        pcontinue threadPolicy (pexpectDatum datum) (pfromData pstep04Args'inputIndex) (pfromData pstep04Args'outputIndex) ownRef (pfromData ptxInfo'inputs) (pfromData ptxInfo'outputs) $ \_ _ _ inputState outputHash outputState -> P.do
            state@POutputDescriptorStateV1{poutputDescriptor'bound, poutputDescriptor'outcome} <- pmatch $ pexpectStateAs @POutputDescriptorStateV1 inputState
            pexpecting (pfromData poutputDescriptor'outcome #== poutputCanonical) $ P.do
                PBoundOutputV1{pboundOutput'subject, pboundOutput'outputIndex} <- pmatch $ pfromData poutputDescriptor'bound
                Subject.PVerdictSubject{Subject.psubject'transactionId} <- pmatch $ pfromData pboundOutput'subject
                view <- plet $ popenedFieldView # pfromData pstep04Args'opening # pcon (PBodyAnchor psubject'transactionId) # poutputsFieldIndex # pfromData ptxInfo'referenceInputs # certificatePolicy
                let expected = pbindReferenceScriptV1 # pcon state # (pfieldItemAt # view # pfromData pboundOutput'outputIndex) # pto (pfromData step05Hash)
                outputHash #== step05Hash #&& outputState #== pforgetData (pdata expected)

outputReferenceScriptDecodingStep05Validator :: forall s. Term s (PAsData PScriptHash :--> PAsData PCurrencySymbol :--> PScriptContext :--> PUnit)
outputReferenceScriptDecodingStep05Validator = plam $ \step06Hash threadPolicy ctx ->
    pstep ctx $ \datum redeemer ownRef tx -> pdispatch @_ @PStep05Args threadPolicy datum redeemer ownRef tx $ \args -> P.do
        PStep05Args{..} <- pmatch args
        PTxInfo{ptxInfo'inputs, ptxInfo'outputs} <- pmatch tx
        pcontinue threadPolicy (pexpectDatum datum) (pfromData pstep05Args'inputIndex) (pfromData pstep05Args'outputIndex) ownRef (pfromData ptxInfo'inputs) (pfromData ptxInfo'outputs) $ \ownHash _ _ inputState outputHash outputState -> P.do
            state <- plet $ pexpectStateAs @PReferenceScriptScanStateV1 inputState
            PReferenceScriptScanStateV1{..} <- pmatch state
            pexpecting (pscanStateIsAuthenticV1 # state #&& pfromData preferenceScan'nextExpectedScriptHash #== pto (pfromData ownHash)) $
                let closeWith resultClass =
                        pexpecting (outputHash #== step06Hash) $
                            pclosedScanStateV1 # state # resultClass # pto (pfromData step06Hash)
                    expected =
                        pif
                            (pfromData preferenceScan'resultClass #/= presultPending)
                            ( pexpecting
                                ( pfromData pstep05Args'controlCbor
                                    #== pconstant ""
                                    #&& pfromData pstep05Args'chunkProof
                                    #== pcon PDNothing
                                    #&& pfromData pstep05Args'nextChunkProof
                                    #== pcon PDNothing
                                    #&& pnull
                                    # pfromData pstep05Args'frames
                                    #&& pfromData pstep05Args'stepBudget
                                    #== 0
                                )
                                (closeWith $ pfromData preferenceScan'resultClass)
                            )
                            ( pexpecting (pfromData pstep05Args'stepBudget #> 0 #&& pfromData pstep05Args'controlCbor #== pfromData preferenceScan'controlCbor) $
                                let control = Scan.pdecodeStructureControlV1 # pfromData pstep05Args'controlCbor
                                    window = pmatch (pfromData pstep05Args'chunkProof) $ \case
                                        PDNothing -> pexpecting (pfromData pstep05Args'nextChunkProof #== pcon PDNothing) (pcon PNothing)
                                        PDJust proof -> pcon $ PJust $ pauthenticatedWindowV1 # state # control # pfromData proof # pfromData pstep05Args'nextChunkProof
                                 in pmatch (Engine.pbudgetedScanV1 # control # window # pfromData pstep05Args'frames # pfromData pstep05Args'stepBudget) $ \case
                                        Engine.PScanRefusedV1 refusalClass -> closeWith $ pmappedRefusalClassV1 # pfromData refusalClass
                                        Engine.PScanAdvancedV1 nextControl ->
                                            let next = pfromData nextControl
                                             in pif
                                                    (Scan.pstructureTerminalIsExactV1 # next)
                                                    (closeWith presultNoFault)
                                                    ( pexpecting (outputHash #== ownHash) $
                                                        let advanced = padvancedScanStateV1 # state # next # pto (pfromData ownHash)
                                                         in pexpecting
                                                                ( pmatch advanced $ \PReferenceScriptScanStateV1{preferenceScan'checkpointHash = nextCheckpointHash} ->
                                                                    nextCheckpointHash #/= preferenceScan'checkpointHash
                                                                )
                                                                advanced
                                                    )
                            )
                 in outputState #== pforgetData (pdata expected)

outputReferenceScriptDecodingStep06Validator :: forall s. Term s (PAsData PCurrencySymbol :--> PAsData PAddress :--> PAsData PCurrencySymbol :--> PScriptContext :--> PUnit)
outputReferenceScriptDecodingStep06Validator = plam $ \fraudPolicy fraudAddress threadPolicy ctx ->
    pstep ctx $ \datum redeemer ownRef tx -> pdispatch @_ @PStep06Args threadPolicy datum redeemer ownRef tx $ \args -> P.do
        PStep06Args{..} <- pmatch args
        PTxInfo{ptxInfo'inputs, ptxInfo'outputs, ptxInfo'redeemers} <- pmatch tx
        pfinalize threadPolicy fraudPolicy fraudAddress (pexpectDatum datum) (pfromData pstep06Args'inputIndex) (pfromData pstep06Args'outputIndex) (pfromData pstep06Args'fraudProofMintRedeemerIndex) ownRef (pfromData ptxInfo'inputs) (pfromData ptxInfo'outputs) (pto $ pto $ pfromData ptxInfo'redeemers) $ \_ _ _ inputState ->
            pterminalContradictionV1 # pexpectStateAs @PReferenceScriptScanStateV1 inputState
