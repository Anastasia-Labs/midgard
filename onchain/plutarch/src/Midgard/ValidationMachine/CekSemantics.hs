{- |
Module      : Midgard.ValidationMachine.CekSemantics
Description : Branch-local CEK semantic resolvers.

This is the Plutarch port of the four per-kind CEK resolvers introduced by R5.
Keeping them outside the aggregate machine module also prevents a split script
from retaining the other three CEK branches during compilation.
-}
module Midgard.ValidationMachine.CekSemantics (
    PCekWitnessControlV1 (..),
    pcekWitnessControlV1,
    pcekWitnessIsWellFormedV1,
    pcekControlIsCoreStepV1,
    pcekControlIsExecutionSelectionV1,
    pcekControlIsContextStepV1,
    pverifyCekCoreFrame,
    pverifyCekCoreSuccessor,
    pverifyCekFinishSemanticsV1,
    pverifyCekExecutionSelectionSemanticsV1,
    pverifyCekContextStepSemanticsV1,
    pverifyCekCoreStepSemanticsV1,
) where

import GHC.Generics (Generic)
import Generics.SOP qualified as SOP

import Aiken.Cbor (pdeserialise)
import Plutarch.Core.Utils (pand'List, (#/=))
import Plutarch.Prelude
import Plutarch.Repr.Scott (DeriveAsScottRec (..))

import Midgard.CekMachine qualified as CekMachine
import Midgard.FraudProofs.NativeTx.Compact qualified as NativeCompact
import Midgard.FraudProofs.NativeTx.Types (PVerifiedMidgardNativeTxCompact (..))
import Midgard.RejectionReason (prejectPlutusScriptInvalid)
import Midgard.ValidationMachine
import Midgard.ValidationMachineFieldDoor (PMachineFieldDoorV1)
import Midgard.ValidationTrace (PValidationMachineStateV1 (..), PValidationPhase (..), phashValidationContext, phashWorkWitness)

data PCekWitnessControlV1 (s :: S)
    = PCekWitnessControlV1
        (Term s PNativeScriptsControlV1)
        (Term s PByteString)
        (Term s PInteger)
        (Term s PInteger)
        (Term s PInteger)
        (Term s PByteString)
        (Term s PByteString)
        (Term s PInteger)
        (Term s PInteger)
    deriving stock (Generic)
    deriving anyclass (SOP.Generic)
    deriving (PlutusType) via (DeriveAsScottRec PCekWitnessControlV1)

pcekWitnessControlV1 ::
    forall s.
    Term s (PValidationOneStepWitnessV1 :--> PCekWitnessControlV1)
pcekWitnessControlV1 = phoistAcyclic $ plam $ \witness ->
    pmatch witness $ \stepWitness ->
        pmatch (pdeserialise # pfromData (poneStep'workWitnessCbor stepWitness)) $ \case
            PNothing -> perror
            PJust dat ->
                plet (pasList # dat) $ \items ->
                    pif
                        (plength # items #== 9)
                        ( plet (pnativeScriptsControlFromWitness # (pasByteStr # (pelemAt # 0 # items))) $ \native -> pcon $
                            PCekWitnessControlV1
                                native
                                (pasByteStr # (pelemAt # 1 # items))
                                (pasInt # (pelemAt # 2 # items))
                                (pasInt # (pelemAt # 3 # items))
                                (pasInt # (pelemAt # 4 # items))
                                (pasByteStr # (pelemAt # 5 # items))
                                (pasByteStr # (pelemAt # 6 # items))
                                (pasInt # (pelemAt # 7 # items))
                                (pasInt # (pelemAt # 8 # items))
                        )
                        perror

pcekWitnessIsWellFormedV1 ::
    forall s.
    Term
        s
        ( PValidationMachineStateV1
            :--> PValidationOneStepWitnessV1
            :--> PCekWitnessControlV1
            :--> PBool
        )
pcekWitnessIsWellFormedV1 = phoistAcyclic $ plam $ \pre witness cek ->
    pmatch pre $ \preState ->
        pmatch witness $ \stepWitness ->
            pmatch cek $ \(PCekWitnessControlV1 nativeControl contextControlCbor executionCursor completedCpu completedMemory activeStateHash programEnvelopeHash executionCpuLimit executionMemoryLimit) ->
                pmatch nativeControl $ \native ->
                    pmatch
                        ( NativeCompact.pverifyNativeTxProofSourceV1
                            # pfromData (pmachineState'transactionId preState)
                            # pfromData (pnativeControl'compactCbor native)
                            # pfromData (pnativeControl'witnessSetCompactCbor native)
                            # pfromData (pnativeControl'fieldPreimageLengthsCbor native)
                        )
                        $ \(PPair verifiedSource _) ->
                            pmatch verifiedSource $ \verified ->
                                pand'List
                                    [ pverified'version verified #== 1
                                    , NativeCompact.pnativeTxProofCommitmentV1
                                        # pfromData (pnativeControl'compactCbor native)
                                        # pfromData (pnativeControl'witnessSetCompactCbor native)
                                        # pfromData (pnativeControl'fieldPreimageLengthsCbor native)
                                        #== pfromData (pmachineState'transactionCommitment preState)
                                    , phashValidationContext
                                        # pfromData (pnativeControl'contextCbor native)
                                        #== pfromData (pmachineState'validationContextHash preState)
                                    , pnativeScriptsControlIsWellFormed # nativeControl
                                    , pfromData (pnativeControl'executionCursor native)
                                        #== pfromData (pnativeControl'executionCount native)
                                    , executionCursor #>= 0
                                    , executionCursor #<= pfromData (pnativeControl'executionCount native)
                                    , completedCpu #>= 0
                                    , completedMemory #>= 0
                                    , pfromData (poneStep'workWitnessCbor stepWitness)
                                        #== pencodeCekWitnessV1
                                        # nativeControl
                                        # contextControlCbor
                                        # executionCursor
                                        # completedCpu
                                        # completedMemory
                                        # activeStateHash
                                        # executionCpuLimit
                                        # executionMemoryLimit
                                        # programEnvelopeHash
                                    ]

pcekControlIsSelectingV1 :: forall s. Term s (PCekWitnessControlV1 :--> PBool)
pcekControlIsSelectingV1 = phoistAcyclic $ plam $ \cek ->
    pmatch cek $ \(PCekWitnessControlV1 _ contextControlCbor _ _ _ activeStateHash _ _ _) ->
        activeStateHash #== pconstant "" #&& contextControlCbor #== pconstant ""

pcekControlIsFinishV1 :: forall s. Term s (PCekWitnessControlV1 :--> PBool)
pcekControlIsFinishV1 = phoistAcyclic $ plam $ \cek ->
    pmatch cek $ \(PCekWitnessControlV1 nativeControl _ executionCursor _ _ _ _ _ _) ->
        pmatch nativeControl $ \native ->
            pcekControlIsSelectingV1
                # cek
                #&& executionCursor
                #== pfromData (pnativeControl'executionCount native)

pcekControlIsExecutionSelectionV1 :: forall s. Term s (PCekWitnessControlV1 :--> PBool)
pcekControlIsExecutionSelectionV1 = phoistAcyclic $ plam $ \cek ->
    pmatch cek $ \(PCekWitnessControlV1 nativeControl _ executionCursor _ _ _ _ _ _) ->
        pmatch nativeControl $ \native ->
            pcekControlIsSelectingV1
                # cek
                #&& executionCursor
                #/= pfromData (pnativeControl'executionCount native)

pcekControlIsContextStepV1 :: forall s. Term s (PCekWitnessControlV1 :--> PBool)
pcekControlIsContextStepV1 = phoistAcyclic $ plam $ \cek ->
    pmatch cek $ \(PCekWitnessControlV1 _ contextControlCbor _ _ _ activeStateHash _ _ _) ->
        activeStateHash #== pconstant "" #&& contextControlCbor #/= pconstant ""

pcekControlIsCoreStepV1 :: forall s. Term s (PCekWitnessControlV1 :--> PBool)
pcekControlIsCoreStepV1 = phoistAcyclic $ plam $ \cek ->
    pmatch cek $ \(PCekWitnessControlV1 _ _ _ _ _ activeStateHash _ _ _) ->
        activeStateHash #/= pconstant ""

pcekFinishV1 ::
    forall s.
    Term
        s
        ( PValidationMachineStateV1
            :--> PValidationOneStepWitnessV1
            :--> PValidationAuxiliaryWitnessV1
            :--> PCekWitnessControlV1
            :--> PBool
        )
pcekFinishV1 = phoistAcyclic $ plam $ \pre witness auxiliary cek ->
    pmatch cek $ \(PCekWitnessControlV1 nativeControl _ _ _ _ _ programEnvelopeHash executionCpuLimit executionMemoryLimit) ->
        pand'List
            [ pcekControlIsFinishV1 # cek
            , executionCpuLimit #== 0
            , executionMemoryLimit #== 0
            , programEnvelopeHash #== pconstant ""
            , pverifyCekCompleted # pre # witness # auxiliary # nativeControl
            ]

pcekExecutionSelectionV1 ::
    forall s.
    Term
        s
        ( PValidationMachineStateV1
            :--> PValidationOneStepWitnessV1
            :--> PValidationAuxiliaryWitnessV1
            :--> PCekWitnessControlV1
            :--> PBool
        )
pcekExecutionSelectionV1 = phoistAcyclic $ plam $ \pre witness auxiliary cek ->
    pmatch cek $ \(PCekWitnessControlV1 nativeControl _ executionCursor completedCpu completedMemory _ programEnvelopeHash executionCpuLimit executionMemoryLimit) ->
        pand'List
            [ pcekControlIsExecutionSelectionV1 # cek
            , executionCpuLimit #== 0
            , executionMemoryLimit #== 0
            , programEnvelopeHash #== pconstant ""
            , pmatch auxiliary $ \case
                PNativeExecutionScanWitness executionIndex languageTag purposeKind purposeIndex scriptHash subject purposeSiblings sourceIndex originKind sourceKey scriptTotalLength scriptItemCommitment sourceSiblings redeemerLeaf executionSiblings firstChunkProof ->
                    pverifyCekExecutionSelection
                        # pre
                        # witness
                        # nativeControl
                        # executionCursor
                        # completedCpu
                        # completedMemory
                        # pfromData executionIndex
                        # pfromData languageTag
                        # pfromData purposeKind
                        # pfromData purposeIndex
                        # pfromData scriptHash
                        # pfromData subject
                        # pfromData purposeSiblings
                        # pfromData sourceIndex
                        # pfromData originKind
                        # pfromData sourceKey
                        # pfromData scriptTotalLength
                        # pfromData scriptItemCommitment
                        # pfromData sourceSiblings
                        # pfromData redeemerLeaf
                        # pfromData executionSiblings
                        # pfromData firstChunkProof
                _ -> pconstant False
            ]

pcekContextV1 ::
    forall s.
    Term
        s
        ( PValidationMachineStateV1
            :--> PValidationOneStepWitnessV1
            :--> PValidationAuxiliaryWitnessV1
            :--> PCekWitnessControlV1
            :--> PMachineFieldDoorV1
            :--> PBool
        )
pcekContextV1 = phoistAcyclic $ plam $ \pre witness auxiliary cek door ->
    pmatch cek $ \(PCekWitnessControlV1 nativeControl contextControlCbor executionCursor completedCpu completedMemory _ programEnvelopeHash executionCpuLimit executionMemoryLimit) ->
        pmatch nativeControl $ \native ->
            pif
                (pcekControlIsContextStepV1 # cek)
                ( plet (pcekContextControlFromCbor # contextControlCbor) $ \contextControl ->
                    pmatch contextControl $ \context ->
                        pand'List
                            [ executionCursor #< pfromData (pnativeControl'executionCount native)
                            , executionCpuLimit #== 0
                            , executionMemoryLimit #== 0
                            , programEnvelopeHash #== pfromData (pcekContext'programEnvelopeHash context)
                            , contextControlCbor #== pencodeCekContextControlV1 # contextControl
                            , pcekContextControlIsWellFormed # nativeControl # contextControl
                            , pverifyCekContextStep
                                # pre
                                # witness
                                # auxiliary
                                # nativeControl
                                # contextControl
                                # executionCursor
                                # completedCpu
                                # completedMemory
                                # door
                            ]
                )
                (pconstant False)

pcekCoreV1 ::
    forall s.
    Term
        s
        ( PValidationMachineStateV1
            :--> PValidationOneStepWitnessV1
            :--> PValidationAuxiliaryWitnessV1
            :--> PCekWitnessControlV1
            :--> PBool
        )
pcekCoreV1 = phoistAcyclic $ plam $ \pre witness auxiliary cek ->
    pmatch cek $ \(PCekWitnessControlV1 nativeControl contextControlCbor executionCursor completedCpu completedMemory activeStateHash programEnvelopeHash executionCpuLimit executionMemoryLimit) ->
        pcekControlIsCoreStepV1
            # cek
            #&& contextControlCbor
            #== pconstant ""
            #&& pmatch
                auxiliary
                ( \case
                    PCekCoreStepWitness evidence ->
                        pmatch (pfromData evidence) $ \(CekMachine.PCoreStepEvidenceV1 stepPre stepPost coreWitness) ->
                            pverifyCekCoreStep
                                # pre
                                # witness
                                # nativeControl
                                # executionCursor
                                # completedCpu
                                # completedMemory
                                # activeStateHash
                                # executionCpuLimit
                                # executionMemoryLimit
                                # programEnvelopeHash
                                # pfromData stepPre
                                # pfromData stepPost
                                # pfromData coreWitness
                    _ -> pconstant False
                )

pverifyCekFinishSemanticsV1 ::
    forall s.
    Term s (PValidationMachineStateV1 :--> PValidationOneStepWitnessV1 :--> PBool)
pverifyCekFinishSemanticsV1 = phoistAcyclic $ plam $ \pre witness ->
    plet (pcekWitnessControlV1 # witness) $ \cek ->
        pcekWitnessIsWellFormedV1
            # pre
            # witness
            # cek
            #&& pcekFinishV1
            # pre
            # witness
            # pcon PNoAuxiliaryWitness
            # cek

pverifyCekExecutionSelectionSemanticsV1 ::
    forall s.
    Term
        s
        ( PValidationMachineStateV1
            :--> PValidationOneStepWitnessV1
            :--> PValidationAuxiliaryWitnessV1
            :--> PBool
        )
pverifyCekExecutionSelectionSemanticsV1 = phoistAcyclic $ plam $ \pre witness auxiliary ->
    plet (pcekWitnessControlV1 # witness) $ \cek ->
        pcekWitnessIsWellFormedV1
            # pre
            # witness
            # cek
            #&& pcekExecutionSelectionV1
            # pre
            # witness
            # auxiliary
            # cek

pverifyCekContextStepSemanticsV1 ::
    forall s.
    Term
        s
        ( PValidationMachineStateV1
            :--> PValidationOneStepWitnessV1
            :--> PValidationAuxiliaryWitnessV1
            :--> PMachineFieldDoorV1
            :--> PBool
        )
pverifyCekContextStepSemanticsV1 = phoistAcyclic $ plam $ \pre witness auxiliary door ->
    plet (pcekWitnessControlV1 # witness) $ \cek ->
        pcekWitnessIsWellFormedV1
            # pre
            # witness
            # cek
            #&& pcekControlIsContextStepV1
            # cek
            #&& pcekContextV1
            # pre
            # witness
            # auxiliary
            # cek
            # door

pverifyCekCoreStepSemanticsV1 ::
    forall s.
    Term
        s
        ( PValidationMachineStateV1
            :--> PValidationOneStepWitnessV1
            :--> CekMachine.PCoreStepEvidenceV1
            :--> PBool
        )
pverifyCekCoreStepSemanticsV1 = phoistAcyclic $ plam $ \pre witness step ->
    plet (pcekWitnessControlV1 # witness) $ \cek ->
        pcekWitnessIsWellFormedV1
            # pre
            # witness
            # cek
            #&& pcekCoreV1
            # pre
            # witness
            # (pcon $ PCekCoreStepWitness $ pdata step)
            # cek

-- Authentication and successor checks are separate physical hops in the target.
pverifyCekCoreFrame :: forall s. Term s (PValidationMachineStateV1 :--> CekMachine.PMachineStateV1 :--> CekMachine.PMachineStateV1 :--> PCekWitnessControlV1 :--> PBool)
pverifyCekCoreFrame = phoistAcyclic $ plam $ \pre machinePre machinePost cek ->
    pmatch pre $ \p -> pmatch machinePre $ \before -> pmatch machinePost $ \after ->
        pmatch cek $ \(PCekWitnessControlV1 control _ cursor cpu memory active envelope cpuLimit memoryLimit) -> pmatch control $ \c ->
            pand'List
                [ plengthBS # active #== 32
                , plengthBS # envelope #== 32
                , cursor #>= 0
                , cursor #< pfromData (pnativeControl'executionCount c)
                , cpuLimit #> 0
                , memoryLimit #> 0
                , pfromData (CekMachine.pstate'executionIndex before) #== cursor
                , CekMachine.phashStateV1 # machinePre #== active
                , pfromData (pmachineState'executionCpu p) #== cpu + pfromData (CekMachine.pstate'cpu before)
                , pfromData (pmachineState'executionMemory p) #== memory + pfromData (CekMachine.pstate'memory before)
                , CekMachine.pstateIsWellFormed # machinePre
                , CekMachine.pstateIsWellFormed # machinePost
                , CekMachine.pstate'executionIndex before #== CekMachine.pstate'executionIndex after
                ]

pverifyCekCoreSuccessor :: forall s. Term s (PValidationMachineStateV1 :--> PValidationOneStepWitnessV1 :--> CekMachine.PMachineStateV1 :--> PCekWitnessControlV1 :--> PBool)
pverifyCekCoreSuccessor = phoistAcyclic $ plam $ \pre witness stepPost cek ->
    pmatch pre $ \preState -> pmatch witness $ \validationWitness ->
        pmatch (pfromData $ poneStep'claimedSuccessor validationWitness) $ \postState ->
            pmatch stepPost $ \corePost ->
                pmatch cek $ \(PCekWitnessControlV1 nativeControl _ executionCursor completedCpu completedMemory _ programEnvelopeHash executionCpuLimit executionMemoryLimit) ->
                    pmatch nativeControl $ \native ->
                        plet (completedCpu + pfromData (CekMachine.pstate'cpu corePost)) $ \nextCpu ->
                            plet (completedMemory + pfromData (CekMachine.pstate'memory corePost)) $ \nextMemory ->
                                plet (pfromData (CekMachine.pstate'cpu corePost) #> executionCpuLimit #|| pfromData (CekMachine.pstate'memory corePost) #> executionMemoryLimit) $ \budgetExceeded ->
                                    pif
                                        (budgetExceeded #|| pfromData (CekMachine.pstate'mode corePost) #== CekMachine.pmodeHaltError)
                                        ( pand'List
                                            [ pfromData (pmachineState'executionCpu postState) #== nextCpu
                                            , pfromData (pmachineState'executionMemory postState) #== nextMemory
                                            , prejectedSuccessorIsExact
                                                # pre
                                                # pfromData (poneStep'claimedSuccessor validationWitness)
                                                # prejectPlutusScriptInvalid
                                            ]
                                        )
                                        ( pif
                                            (pfromData (CekMachine.pstate'mode corePost) #== CekMachine.pmodeHaltSuccess)
                                            ( plet (executionCursor + 1) $ \nextCursor ->
                                                pand'List
                                                    [ pfromData (CekMachine.pstate'cpu corePost) #<= executionCpuLimit
                                                    , pfromData (CekMachine.pstate'memory corePost) #<= executionMemoryLimit
                                                    , pfromData (pmachineState'executionCpu postState) #== nextCpu
                                                    , pfromData (pmachineState'executionMemory postState) #== nextMemory
                                                    , pif
                                                        (nextCursor #== pfromData (pnativeControl'executionCount native))
                                                        ( pand'List
                                                            [ pfromData (pmachineState'phase postState) #== pcon PValueAndMint
                                                            , pfromData (pmachineState'workRoot postState)
                                                                #== phashWorkWitness
                                                                # pcon PValueAndMint
                                                                # (pfromData (pmachineState'programCounter preState) + 1)
                                                                # (pencodeValueAndMintWitnessV1 # nativeControl)
                                                            ]
                                                        )
                                                        ( pand'List
                                                            [ pfromData (pmachineState'phase postState) #== pcon PCek
                                                            , pfromData (pmachineState'workRoot postState)
                                                                #== phashWorkWitness
                                                                # pcon PCek
                                                                # (pfromData (pmachineState'programCounter preState) + 1)
                                                                # ( pencodeCekWitnessV1
                                                                        # nativeControl
                                                                        # pconstant ""
                                                                        # nextCursor
                                                                        # nextCpu
                                                                        # nextMemory
                                                                        # pconstant ""
                                                                        # 0
                                                                        # 0
                                                                        # pconstant ""
                                                                  )
                                                            ]
                                                        )
                                                    ]
                                            )
                                            ( pand'List
                                                [ pfromData (pmachineState'phase postState) #== pcon PCek
                                                , pfromData (pmachineState'executionCpu postState) #== nextCpu
                                                , pfromData (pmachineState'executionMemory postState) #== nextMemory
                                                , pfromData (pmachineState'workRoot postState)
                                                    #== phashWorkWitness
                                                    # pcon PCek
                                                    # (pfromData (pmachineState'programCounter preState) + 1)
                                                    # ( pencodeCekWitnessV1
                                                            # nativeControl
                                                            # pconstant ""
                                                            # executionCursor
                                                            # completedCpu
                                                            # completedMemory
                                                            # (CekMachine.phashStateV1 # stepPost)
                                                            # executionCpuLimit
                                                            # executionMemoryLimit
                                                            # programEnvelopeHash
                                                      )
                                                ]
                                            )
                                        )
