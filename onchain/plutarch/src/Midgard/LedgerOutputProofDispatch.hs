{-# LANGUAGE OverloadedStrings #-}

-- | Spending-side carrier binding for the shared output-proof yield graph.
module Midgard.LedgerOutputProofDispatch (
    pmembershipStep,
    poutputStep,
    pmembershipFinalize,
    poutputFinalize,
) where

import Midgard.FraudProofs.NativeTx.Codec (pcborInt)
import Midgard.FraudProofs.NativeTx.Types (PMidgardAddress (..))
import Midgard.LedgerOutput qualified as Output
import Midgard.LedgerOutputCommitment qualified as Descriptor
import Midgard.LedgerOutputProofRaw qualified as Lop
import Midgard.LedgerOutputProofRoles qualified as Roles
import Midgard.LedgerOutputProofStages qualified as Stages
import Midgard.RejectionReason qualified as Rejection
import Midgard.ResolveInputsControl qualified as Carrier
import Midgard.ScriptProof qualified as Script
import Midgard.ScriptSourcesRawFrame qualified as Raw
import Midgard.ValidationMachine qualified as VM
import Midgard.ValidationMerkle qualified as Merkle
import Midgard.ValidationSemanticYield qualified as Yield
import Midgard.ValidationTrace qualified as Trace
import Plutarch.LedgerApi.V3 (PCurrencySymbol, PTxInfo)
import Plutarch.Prelude

prequireStage :: forall s. Term s PTxInfo -> Term s PCurrencySymbol -> Term s PInteger -> Term s (PBuiltinList (PAsData PInteger)) -> Term s PBool
prequireStage tx policy role indices = Yield.prequireSemanticYieldsV1 # tx # policy # (pcons # pdata (Roles.pstageRole # role) # (Roles.pstageAttestationRoles # role)) # indices

presolveSuccessor :: forall s. Term s Trace.PValidationMachineStateV1 -> Term s VM.PValidationOneStepWitnessV1 -> Term s PByteString -> Term s PBool
presolveSuccessor pre transition next = pmatch pre $ \p -> pmatch transition $ \w -> pmatch (pfromData $ VM.poneStep'claimedSuccessor w) $ \post ->
    pfromData (Trace.pmachineState'phase post)
        #== pcon Trace.PResolveInputs
        #&& pfromData (Trace.pmachineState'workRoot post)
        #== Trace.phashWorkWitness
        # pcon Trace.PResolveInputs
        # (pfromData (Trace.pmachineState'programCounter p) + 1)
        # next

pmembershipStep, poutputStep :: forall s. Term s (Trace.PValidationMachineStateV1 :--> VM.PValidationOneStepWitnessV1 :--> PByteString :--> PByteString :--> PInteger :--> PBuiltinList (PAsData PInteger) :--> PCurrencySymbol :--> PTxInfo :--> PBool)
pmembershipStep = phoistAcyclic $ plam $ \pre transition controlCbor next role indices policy tx -> pmatch transition $ \w ->
    plet (Carrier.pcontrolRawFromWitness # pfromData (VM.poneStep'workWitnessCbor w)) $ \control -> pmatch control $ \c ->
        plet (Carrier.ppendingRawFromCbor # Carrier.pcontrol'pendingCbor c) $ \pending -> pmatch pending $ \pd ->
            plet (Lop.popen # controlCbor) $ \frame ->
                plet (Descriptor.pdecodeLedgerOutputCommitment # Carrier.ppending'descriptorCbor pd) $ \descriptor ->
                    prequireStage tx policy role indices
                        #&& controlCbor
                        #== Carrier.ppending'outputProofCbor pd
                        #&& Carrier.pcontrolRawIsBoundWithDescriptor
                        # pre
                        # transition
                        # control
                        # pending
                        # descriptor
                        #&& Carrier.pcontrolRawLopIsPinnedWithDescriptor
                        # frame
                        # descriptor
                        #&& pif
                            (next #== pconstant "")
                            (pmatch (pfromData $ VM.poneStep'claimedSuccessor w) $ \post -> pfromData (Trace.pmachineState'phase post) #== pcon Trace.PTerminal)
                            (presolveSuccessor pre transition $ Carrier.psplicePendingSuccessorV1 # pfromData (VM.poneStep'workWitnessCbor w) # Carrier.pcontrol'pendingCbor c # Carrier.ppending'outputProofCbor pd # next)
poutputStep = phoistAcyclic $ plam $ \pre transition controlCbor next role indices policy tx -> pmatch transition $ \w ->
    plet (Raw.popenFrameV1 # pre # transition # 31 # 5) $ \frame ->
        plet (Lop.popen # controlCbor) $ \_ ->
            prequireStage tx policy role indices
                #&& Raw.pitemBytesV1
                # frame
                # 30
                #== controlCbor
                #&& pif
                    (next #== pconstant "")
                    (pmatch (pfromData $ VM.poneStep'claimedSuccessor w) $ \post -> pfromData (Trace.pmachineState'phase post) #== pcon Trace.PTerminal)
                    (Raw.psuccessorIsExactV1 # pre # transition # (Raw.preplaceExtensionV1 # pfromData (VM.poneStep'workWitnessCbor w) # controlCbor # next))

prequireDescriptors :: forall s. Term s PTxInfo -> Term s PCurrencySymbol -> Term s (PBuiltinList (PAsData PInteger)) -> Term s (PBuiltinList (PAsData PInteger)) -> Term s PBool
prequireDescriptors tx policy roles indices = Yield.prequireSemanticYieldsV1 # tx # policy # (pmap # plam (\role -> pdata $ Roles.pdescriptorRole # pfromData role) # roles) # indices

pmembershipFinalize, poutputFinalize :: forall s. Term s (Trace.PValidationMachineStateV1 :--> VM.PValidationOneStepWitnessV1 :--> PByteString :--> VM.PSignerSetProofV1 :--> PByteString :--> PData :--> PData :--> PBuiltinList (PAsData PInteger) :--> PBuiltinList (PAsData PInteger) :--> PCurrencySymbol :--> PTxInfo :--> PBool)
pmembershipFinalize = phoistAcyclic $ plam $ \pre transition descriptorCbor signer controlCbor value datum roles indices policy tx -> pmatch transition $ \w ->
    plet (Carrier.pcontrolRawFromWitness # pfromData (VM.poneStep'workWitnessCbor w)) $ \control -> pmatch control $ \c ->
        plet (Carrier.ppendingRawFromCbor # Carrier.pcontrol'pendingCbor c) $ \pending -> pmatch pending $ \pd ->
            plet (Lop.popen # controlCbor) $ \frame ->
                plet (Descriptor.pdecodeLedgerOutputCommitment # descriptorCbor) $ \descriptor ->
                    pif
                        (pnot # (pnull # roles))
                        ( prequireDescriptors tx policy roles indices
                            #&& descriptorCbor
                            #== Carrier.ppending'descriptorCbor pd
                            #&& controlCbor
                            #== Carrier.ppending'outputProofCbor pd
                            #&& Carrier.pcontrolRawIsBoundWithDescriptor
                            # pre
                            # transition
                            # control
                            # pending
                            # descriptor
                            #&& Carrier.pcontrolRawLopIsPinnedWithDescriptor
                            # frame
                            # descriptor
                            #&& ( pmatch (Stages.pfactAttach # frame # (pmap # plam pfromData # roles) # descriptorCbor # value # datum) $ \case
                                    PNothing -> pconstant False
                                    PJust next -> presolveSuccessor pre transition $ Carrier.pencodeControlRaw # pcon c{Carrier.pcontrol'pendingCbor = Carrier.pencodePending # pcon pd{Carrier.ppending'outputProofCbor = Lop.pencode # next}}
                                )
                        )
                        ( pnull
                            # indices
                            #&& Stages.pfactsAreExact
                            # frame
                            # descriptorCbor
                            # value
                            # datum
                            #&& descriptorCbor
                            #== Carrier.ppending'descriptorCbor pd
                            #&& controlCbor
                            #== Carrier.ppending'outputProofCbor pd
                            #&& Carrier.pcontrolRawIsBoundWithDescriptor
                            # pre
                            # transition
                            # control
                            # pending
                            # descriptor
                            #&& Carrier.pcontrolRawLopIsPinnedWithDescriptor
                            # frame
                            # descriptor
                            #&& ( pmatch descriptor $ \d ->
                                    pmatch (Output.pdecodeCanonicalAddressBytes # pfromData (Descriptor.poutputCommitment'address d)) $ \case
                                        PNothing -> perror
                                        PJust address -> pmatch (VM.pinputSignerAuthorization # Carrier.ppending'sourceKind pd # address # Carrier.pcontrol'signerCount c # Carrier.pcontrol'signerFrontierCommitment c # signer) $ \case
                                            VM.PInputSignerMissing -> VM.prejectedSuccessorIsExact # pre # pfromData (VM.poneStep'claimedSuccessor w) # Rejection.prejectMissingRequiredWitness
                                            VM.PInputSignerProofMalformed -> pconstant False
                                            VM.PInputSignerAuthorized ->
                                                presolveSuccessor pre transition $
                                                    Carrier.pencodeControlRaw
                                                        # pcon
                                                            c
                                                                { Carrier.pcontrol'cursor = Carrier.pcontrol'cursor c + 1
                                                                , Carrier.pcontrol'accumulator = VM.presolvedInputAccumulatorSuccessor # Carrier.pcontrol'accumulator c # Carrier.ppending'sourceKind pd # Carrier.ppending'key pd # descriptorCbor
                                                                , Carrier.pcontrol'remainingScheduleHash = Carrier.ppending'nextScheduleHash pd
                                                                , Carrier.pcontrol'pendingCbor = pconstant "\x00"
                                                                }
                                )
                        )
poutputFinalize = phoistAcyclic $ plam $ \pre transition descriptorCbor signer controlCbor value datum roles indices policy tx -> pmatch transition $ \w ->
    plet (Raw.popenFrameV1 # pre # transition # 31 # 5) $ \frame ->
        plet (Lop.popen # controlCbor) $ \control ->
            pif
                (pnot # (pnull # roles))
                ( prequireDescriptors tx policy roles indices
                    #&& Raw.pitemBytesV1
                    # frame
                    # 30
                    #== controlCbor
                    #&& ( pmatch (Stages.pfactAttach # control # (pmap # plam pfromData # roles) # descriptorCbor # value # datum) $ \case
                            PNothing -> pconstant False
                            PJust next -> Raw.psuccessorIsExactV1 # pre # transition # (Raw.preplaceExtensionV1 # pfromData (VM.poneStep'workWitnessCbor w) # controlCbor # (Lop.pencode # next))
                        )
                )
                (poutputTerminal pre transition descriptorCbor signer controlCbor control value datum indices frame)

ppeaks :: forall s. Term s PData -> Term s (PBuiltinList (PAsData Merkle.PFrontierPeak))
ppeaks dat =
    pmap
        # plam
            ( \item -> plet (pasList # item) $ \fields ->
                pif
                    (plength # fields #== 2)
                    (pdata $ pcon $ Merkle.PFrontierPeak (pdata $ pasInt # (pelemAt # 0 # fields)) (pdata $ pasByteStr # (pelemAt # 1 # fields)))
                    perror
            )
        # (pasList # dat)

poutputTerminal :: forall s. Term s Trace.PValidationMachineStateV1 -> Term s VM.PValidationOneStepWitnessV1 -> Term s PByteString -> Term s VM.PSignerSetProofV1 -> Term s PByteString -> Term s Lop.PFrame -> Term s PData -> Term s PData -> Term s (PBuiltinList (PAsData PInteger)) -> Term s Raw.PFrame -> Term s PBool
poutputTerminal pre transition descriptorCbor signer controlCbor control value datum indices frame = pmatch transition $ \w -> pmatch frame $ \f ->
    plet (Raw.pitemIntV1 # frame # 20) $ \cursor -> pmatch (Descriptor.pdecodeLedgerOutputCommitment # descriptorCbor) $ \d ->
        pnull
            # indices
            #&& Stages.pfactsAreExact
            # control
            # descriptorCbor
            # value
            # datum
            #&& Raw.pitemBytesV1
            # frame
            # 30
            #== controlCbor
            #&& cursor
            #< Raw.pitemIntV1
            # frame
            # 21
            #&& pfromData (Descriptor.poutputCommitment'outputIndex d)
            #== cursor
            #&& ( pmatch (Output.pdecodeCanonicalAddressBytes # pfromData (Descriptor.poutputCommitment'address d)) $ \case
                    PNothing -> perror
                    PJust address -> pmatch address $ \a -> pmatch (VM.pdecodeValidationContext # Raw.pframe'contextCbor f) $ \context ->
                        pif
                            (pnot # (pfromData (paddress'networkId a) #== pfromData (VM.pvalidationContext'expectedNetworkId context)))
                            (VM.prejectedSuccessorIsExact # pre # pfromData (VM.poneStep'claimedSuccessor w) # Rejection.prejectNetworkIdMismatch)
                            ( pmatch (VM.pprotectedOutputAuthorization # address # (Raw.pitemIntV1 # frame # 6) # (Raw.pitemBytesV1 # frame # 7) # signer) $ \case
                                VM.PInputSignerMissing -> VM.prejectedSuccessorIsExact # pre # pfromData (VM.poneStep'claimedSuccessor w) # Rejection.prejectMissingRequiredWitness
                                VM.PInputSignerProofMalformed -> pconstant False
                                VM.PInputSignerAuthorized -> plet (pasList # (pelemAt # 24 # Raw.pframe'items f)) $ \fields ->
                                    pif
                                        (plength # fields #== 6)
                                        ( plet
                                            ( pcon $
                                                VM.PReceivePurposeScanControlV1
                                                    (pdata $ pasInt # (pelemAt # 0 # fields))
                                                    (pdata $ ppeaks $ pelemAt # 1 # fields)
                                                    (pdata $ pasInt # (pelemAt # 2 # fields))
                                                    (pdata $ pasByteStr # (pelemAt # 3 # fields))
                                                    (pdata $ pasByteStr # (pelemAt # 4 # fields))
                                                    (pdata $ Merkle.pappendLeaf # cursor # ppeaks (pelemAt # 5 # fields) # (Script.poutputDescriptorLeafHash # cursor # descriptorCbor))
                                            )
                                            $ \scan ->
                                                plet (Raw.preplaceItemsV1 # frame # pfromData (VM.poneStep'workWitnessCbor w) # 24 # 1 # (VM.pencodeReceivePurposeScanControl # (VM.preceiveSourceSuccessor # scan # address))) $ \scanSpliced ->
                                                    plet (Raw.preplaceItemsV1 # frame # scanSpliced # 20 # 1 # pcborInt (cursor + 1)) $ \cursorSpliced ->
                                                        Raw.psuccessorIsExactV1 # pre # transition # (Raw.pdropExtensionV1 # cursorSpliced # controlCbor)
                                        )
                                        perror
                            )
                )
