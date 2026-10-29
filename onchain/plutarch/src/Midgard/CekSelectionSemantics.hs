-- | Execution selection authentication and successor checks in separate yields.
module Midgard.CekSelectionSemantics (pauthenticate, pverifySuccessor) where

import Midgard.BoundedItem qualified as Item
import Midgard.CekProof qualified as Proof
import Midgard.NativeScriptScan qualified as Scan
import Midgard.ScriptProof qualified as Script
import Midgard.ValidationMachine qualified as VM
import Midgard.ValidationMerkle qualified as Merkle
import Midgard.ValidationTrace (PValidationMachineStateV1)
import Plutarch.Core.Utils (pand'List)
import Plutarch.Prelude

pauthenticate :: forall s. Term s (VM.PValidationAuxiliaryWitnessV1 :--> VM.PNativeScriptsControlV1 :--> PInteger :--> PBool)
pauthenticate = phoistAcyclic $ plam $ \auxiliary control cursor -> pmatch control $ \c -> pmatch auxiliary $ \case
    VM.PNativeExecutionScanWitness executionIndex languageTag purposeKind purposeIndex scriptHash subject purposeSiblings sourceIndex originKind sourceKey totalLength commitment sourceSiblings redeemerLeaf executionSiblings firstProof ->
        plet (pfromData firstProof) $ \proof -> pmatch proof $ \p ->
            plet (Script.ppurposeLeafHash # pfromData purposeKind # pfromData purposeIndex # pfromData scriptHash # pfromData subject) $ \purpose ->
                plet (Script.psourceDescriptorLeafHash # pfromData originKind # pfromData sourceKey # pfromData languageTag # pfromData scriptHash # pfromData totalLength # pfromData commitment) $ \source ->
                    plet (Script.pexecutionLeafHash # pfromData languageTag # purpose # source # pfromData redeemerLeaf) $ \execution ->
                        pmatch (Scan.pversionedScriptHeaderV1 # pfromData (Item.pchunkProof'chunk p) # pfromData totalLength) $ \case
                            PNothing -> perror
                            PJust header -> pmatch header $ \h ->
                                pand'List
                                    [ pfromData executionIndex #== cursor
                                    , cursor #< pfromData (VM.pnativeControl'executionCount c)
                                    , VM.pnativeControl'executionCount c #== VM.pnativeControl'purposeCount c
                                    , pfromData totalLength #> 0
                                    , pfromData totalLength #<= VM.pmaxAggregateFieldPreimageBytes
                                    , plengthBS # pfromData commitment #== 32
                                    , pfromData (Item.pchunkProof'chunkIndex p) #== 0
                                    , Item.pchunkProof'totalLength p #== totalLength
                                    , VM.pfirstSourceChunkIdentityMatches # pfromData originKind # pfromData sourceKey # proof
                                    , Item.pverifyChunk # pfromData commitment # proof
                                    , Scan.pheader'languageTag h #== languageTag
                                    , Merkle.pverifyMembership # pfromData (VM.pnativeControl'purposeCount c) # pfromData (VM.pnativeControl'purposePeaks c) # pfromData executionIndex # purpose # pfromData purposeSiblings
                                    , Merkle.pverifyMembership # pfromData (VM.pnativeControl'sourceCount c) # pfromData (VM.pnativeControl'sourcePeaks c) # pfromData sourceIndex # source # pfromData sourceSiblings
                                    , Merkle.pverifyMembership # pfromData (VM.pnativeControl'executionCount c) # pfromData (VM.pnativeControl'executionPeaks c) # pfromData executionIndex # execution # pfromData executionSiblings
                                    ]
    _ -> perror

pverifySuccessor :: forall s. Term s (PValidationMachineStateV1 :--> VM.PValidationOneStepWitnessV1 :--> VM.PValidationAuxiliaryWitnessV1 :--> VM.PNativeScriptsControlV1 :--> PInteger :--> PInteger :--> PInteger :--> PBool)
pverifySuccessor = phoistAcyclic $ plam $ \pre witness auxiliary control cursor cpu memory -> pmatch auxiliary $ \case
    VM.PNativeExecutionScanWitness _ languageTag purposeKind purposeIndex scriptHash subject _ _ _ _ totalLength _ _ redeemerLeaf _ firstProof ->
        pmatch (pfromData firstProof) $ \p ->
            pmatch (Scan.pversionedScriptHeaderV1 # pfromData (Item.pchunkProof'chunk p) # pfromData totalLength) $ \case
                PNothing -> perror
                PJust header -> pmatch header $ \h ->
                    let successor root envelope =
                            VM.pcekSelectionSuccessorIsExact
                                # pre
                                # witness
                                # control
                                # cursor
                                # cpu
                                # memory
                                # pfromData languageTag
                                # pfromData purposeKind
                                # pfromData purposeIndex
                                # pfromData scriptHash
                                # pfromData subject
                                # pfromData redeemerLeaf
                                # root
                                # envelope
                     in pif
                            (pfromData languageTag #== 0)
                            (pfromData redeemerLeaf #== pconstant "" #&& successor (pconstant "") (pconstant ""))
                            $ pif
                                ( (pfromData languageTag #== 3 #|| pfromData languageTag #== 128)
                                    #&& plengthBS
                                    # pfromData redeemerLeaf
                                    #== 32
                                    #&& pfromData (Scan.pheader'payloadLength h)
                                    #<= Proof.pmaxProgramEnvelopeCborBytes
                                    #&& pfromData (Scan.pheader'payloadOffset h)
                                    + pfromData (Scan.pheader'payloadLength h) #<= plengthBS # pfromData (Item.pchunkProof'chunk p)
                                )
                                ( pmatch (Proof.pinspectProgramEnvelopeV1 # (psliceBS # pfromData (Scan.pheader'payloadOffset h) # pfromData (Scan.pheader'payloadLength h) # pfromData (Item.pchunkProof'chunk p))) $ \case
                                    PNothing -> pconstant False
                                    PJust envelope -> pmatch envelope $ \e ->
                                        successor
                                            (pfromData $ Proof.penvelope'termRoot e)
                                            (Proof.phashProgramEnvelopeV1 # 1 # 1 # 0 # pfromData (Proof.penvelope'termRoot e) # pfromData (Proof.penvelope'nodeCount e) # pfromData (Proof.penvelope'materialByteLength e))
                                )
                                (pconstant False)
    _ -> perror
