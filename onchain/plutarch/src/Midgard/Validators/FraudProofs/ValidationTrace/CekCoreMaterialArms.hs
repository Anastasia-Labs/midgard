-- | Typed material and machine-control hops in the CEK core chain.
module Midgard.Validators.FraudProofs.ValidationTrace.CekCoreMaterialArms (
    machineValidator,
    mapConversionValidator,
    semanticPairValidator,
    semanticListConstructValidator,
    semanticListSelectValidator,
    semanticChooseValidator,
    semanticDataConstructValidator,
    semanticDataScalarValidator,
    semanticDataMiscValidator,
    failureKnownValidator,
    failureBudgetValidator,
    semanticFailureRootsValidator,
    semanticFailureMaterialValidator,
    typeFailureRootsValidator,
    typeFailureKindsValidator,
    blsBudgetValidator,
    blsRootsValidator,
    blsFinalValidator,
    mapStartRootsValidator,
    mapStartBudgetValidator,
    mapStartNodesValidator,
) where

import Midgard.CekBuiltin qualified as Builtin
import Midgard.CekCoreWitness qualified as Core
import Midgard.CekCost qualified as Cost
import Midgard.CekMachine qualified as Machine
import Midgard.CekMaterialWitness qualified as Witness
import Midgard.CekProof qualified as Proof
import Midgard.CekSemanticChain qualified as Chain
import Midgard.Common.Utils (pheadSingleton)
import Midgard.Validators.FraudProofs.ValidationTrace.CekCoreArms (arm)
import Plutarch.LedgerApi.V3
import Plutarch.Prelude

type Hop s = Term s (PAsData PScriptHash :--> PAsData PCurrencySymbol :--> PScriptContext :--> PUnit)

machineValidator, mapConversionValidator :: forall s. Hop s
machineValidator = plam $ \next policy ctx -> arm policy ctx $ \bound action continue -> pmatch bound $ \b -> pmatch action $ \a ->
    plet (pfromData $ Chain.pbound'pre b) $ \pre -> plet (pfromData $ Chain.pbound'post b) $ \post -> pmatch pre $ \p ->
        plet (Witness.pdecodeMachineArm # Chain.parm'witness a) $ \witness ->
            pif
                ( pfromData (Chain.pbound'group b)
                    #== 1
                    #&& ( pif (pfromData (Machine.pstate'mode p) #== Machine.pmodeLookup) (Machine.pverifyLookupStep # pre # post # witness) $
                            pif (pfromData (Machine.pstate'mode p) #== Machine.pmodeReturn) (Machine.pverifyReturnStep # pre # post # witness) $
                                pif (pfromData (Machine.pstate'mode p) #== Machine.pmodeCaseSelect) (Machine.pverifyCaseSelectStep # pre # post # witness) $
                                    pif (pfromData (Machine.pstate'mode p) #== Machine.pmodeCaseApply) (Machine.pverifyCaseApplyStep # pre # post # witness) (pconstant False)
                        )
                )
                (continue next 0 $ Chain.pbound'facts b)
                perror
mapConversionValidator = plam $ \next policy ctx -> arm policy ctx $ \bound action continue -> pmatch bound $ \b -> pmatch action $ \a ->
    pmatch (pfromData $ Chain.pbound'pre b) $ \pre ->
        pif
            ( pfromData (Chain.pbound'group b)
                #== 2
                #&& pfromData (Machine.pstate'mode pre)
                #== Machine.pmodeSemanticBuiltin
                #&& Machine.pverifySemanticBuiltinControlStep
                # pfromData (Chain.pbound'pre b)
                # pfromData (Chain.pbound'post b)
                # (Witness.pdecodeMapControlArm # Chain.parm'witness a)
            )
            (continue next 0 $ Chain.pbound'facts b)
            perror

-- These checks are common to the target builtin-material arms, including those
-- that intentionally leave some raw witness fields opaque until the next hop.
builtinHop ::
    forall s.
    Integer ->
    Integer ->
    Integer ->
    (Term s Chain.PCoreBound -> Term s PData -> (Term s PData -> Term s PBool) -> Term s PBool) ->
    Hop s
builtinHop group armTag progress verify = plam $ \next policy ctx -> arm policy ctx $ \bound action continue -> pmatch bound $ \b -> pmatch action $ \a ->
    pmatch (pfromData $ Chain.pbound'pre b) $ \pre ->
        pif
            ( pfromData (Chain.pbound'group b)
                #== pconstant group
                #&& pfromData (Chain.pbound'arm b)
                #== pconstant armTag
                #&& pfromData (Machine.pstate'mode pre)
                #== Machine.pmodeBuiltin
            )
            (verify bound (Chain.parm'witness a) $ continue next (pconstant progress))
            perror

semanticPairValidator, semanticListConstructValidator, semanticListSelectValidator, semanticChooseValidator, semanticDataConstructValidator, semanticDataScalarValidator, semanticDataMiscValidator :: forall s. Hop s
semanticPairValidator = semanticValidator $ \tag args result material -> Builtin.pverifySemanticPairV1 tag (pfromData $ pheadSingleton # args) result material
semanticChooseValidator = semanticValidator $ \_ args result material -> Builtin.pverifySemanticChooseDataV1 args result material
semanticListConstructValidator = semanticValidator $ \tag args result material -> Builtin.pverifySemanticListConstruct # tag # args # result # material
semanticListSelectValidator = semanticValidator $ \tag args result material -> Builtin.pverifySemanticListSelect # tag # args # result # material
semanticDataConstructValidator = semanticValidator $ \tag args result material -> Builtin.pverifySemanticDataConstruct # tag # args # result # material
semanticDataScalarValidator = semanticValidator $ \tag args result material -> Builtin.pverifySemanticDataScalar # tag # args # result # material
semanticDataMiscValidator = semanticValidator $ \tag args result material -> Builtin.pverifySemanticDataMisc # tag # args # result # material

semanticValidator :: forall s. (Term s PInteger -> Term s (PBuiltinList (PAsData Builtin.PValueWitnessV1)) -> Term s Builtin.PValueWitnessV1 -> Term s Builtin.PSemanticBuiltinWitnessV1 -> Term s PBool) -> Hop s
semanticValidator verify = builtinHop 5 31 3 $ \bound raw continue -> pmatch bound $ \b ->
    pmatch (Chain.psemanticItem # bound # raw) $ \item ->
        pif
            (verify (pfromData $ Chain.psuccess'tag item) (pfromData $ Chain.psuccess'arguments item) (pfromData $ Chain.psuccess'result item) (Witness.pdecodeSemanticBuiltinWitness # Chain.psuccess'material item))
            (continue $ Chain.pbound'facts b)
            perror

fields :: forall s. Term s PInteger -> Term s PInteger -> Term s PData -> Term s (PBuiltinList PData)
fields tag count raw = pmatch (pasConstr # raw) $ \(PBuiltinPair actual values) -> pif (actual #== tag #&& plength # values #== count) values perror
values :: forall s. Term s PData -> Term s (PBuiltinList (PAsData Builtin.PValueWitnessV1))
values raw = pmap # plam (\item -> pdata $ Core.pdecodeValue # item) # (pasList # raw)
runtimeValues :: forall s. Term s PData -> Term s (PBuiltinList (PAsData Builtin.PRuntimeValueWitnessV1))
runtimeValues raw = pmap # plam (\item -> pdata $ Witness.pdecodeRuntimeValueWitness # item) # (pasList # raw)

failureKnownValidator, failureBudgetValidator, semanticFailureRootsValidator, semanticFailureMaterialValidator, typeFailureRootsValidator, typeFailureKindsValidator :: forall s. Hop s
failureKnownValidator = builtinHop 9 38 0 $ \bound raw continue -> pmatch bound $ \b -> pmatch (pfromData $ Chain.pbound'pre b) $ \pre ->
    plet (fields 38 2 raw) $ \f -> pif (Builtin.pverifyDirectBuiltinFailureV1 # (pasInt # (phead # f)) # pfromData (Machine.pstate'focusRoot pre) # values (pelemAt # 1 # f)) (continue $ Chain.pbound'facts b) perror
failureBudgetValidator = builtinHop 9 38 1 $ \bound raw continue -> pmatch bound $ \b ->
    plet (fields 38 2 raw) $ \f -> pmatch (Builtin.pauthenticatedBuiltinFailureBudget # (pasInt # (phead # f)) # values (pelemAt # 1 # f)) $ \budget ->
        pif (failurePost bound (pfromData $ Cost.pbudget'cpu budget) (pfromData $ Cost.pbudget'memory budget)) (continue $ Chain.pbound'facts b) perror
semanticFailureRootsValidator = builtinHop 7 36 0 $ \bound raw continue -> pmatch bound $ \b ->
    plet (fields 36 3 raw) $ \f -> plet (pasInt # (phead # f)) $ \tag ->
        pmatch (Builtin.pargumentsRootV1 # values (pelemAt # 1 # f)) $ \(PPair root count) ->
            pif (argumentsBound bound tag root count #&& failurePost bound 0 0) (continue $ Chain.pbound'facts b) perror
semanticFailureMaterialValidator = builtinHop 7 36 1 $ \bound raw continue -> pmatch bound $ \b ->
    plet (fields 36 3 raw) $ \f -> pif (Builtin.pverifySemanticFailureMaterial # (pasInt # (phead # f)) # values (pelemAt # 1 # f) # (Witness.pdecodeSemanticBuiltinWitness # (pelemAt # 2 # f))) (continue $ Chain.pbound'facts b) perror
typeFailureRootsValidator = builtinHop 10 39 0 $ \bound raw continue -> pmatch bound $ \b ->
    plet (fields 39 2 raw) $ \f -> plet (pasInt # (phead # f)) $ \tag -> plet (runtimeValues $ pelemAt # 1 # f) $ \args ->
        pmatch (Builtin.pruntimeArgumentsRootV1 # args) $ \(PPair root count) ->
            -- The target's max_direct_builtin_revealed_payload_bytes is 9215.
            pif (Builtin.prevealedRuntimePayloadBytesV1 # args #<= 9215 #&& argumentsBound bound tag root count #&& failurePost bound 0 0) (continue $ Chain.pbound'facts b) perror
typeFailureKindsValidator = builtinHop 10 39 1 $ \bound raw continue -> pmatch bound $ \b ->
    plet (fields 39 2 raw) $ \f -> pif (pnot # (Builtin.pbuiltinArgumentsAreWellTypedV1 # (pasInt # (phead # f)) # runtimeValues (pelemAt # 1 # f))) (continue $ Chain.pbound'facts b) perror

argumentsBound :: forall s. Term s Chain.PCoreBound -> Term s PInteger -> Term s PByteString -> Term s PInteger -> Term s PBool
argumentsBound bound tag root count = pmatch bound $ \b -> pmatch (pfromData $ Chain.pbound'pre b) $ \pre ->
    count #== Cost.pbuiltinArgumentCountV1 # tag #&& pfromData (Machine.pstate'focusRoot pre) #== Proof.phashBuiltinValueV1 # tag # 0 # count # root
failurePost :: forall s. Term s Chain.PCoreBound -> Term s PInteger -> Term s PInteger -> Term s PBool
failurePost bound cpu memory = pmatch bound $ \b -> pfromData (Chain.pbound'post b) #== Machine.pexactState (pfromData $ Chain.pbound'pre b) Machine.pmodeHaltError Proof.phashErrorTermV1 Proof.pemptyEnvironmentRootV1 Proof.pemptyContinuationRootV1 Machine.perrorBuiltinFailure cpu memory

blsBudgetValidator, blsRootsValidator, blsFinalValidator :: forall s. Hop s
blsBudgetValidator = builtinHop 8 37 0 $ \bound raw continue -> pmatch bound $ \b -> pmatch (pfromData $ Chain.pbound'pre b) $ \pre ->
    plet (fields 37 5 raw) $ \f ->
        plet
            ( pcons
                # pdata (pcon $ Builtin.PBlsMillerLoopValue $ pdata $ pasByteStr # (pelemAt # 0 # f))
                # (pcons # pdata (pcon $ Builtin.PBlsMillerLoopValue $ pdata $ pasByteStr # (pelemAt # 1 # f)) # pnil)
            )
            $ \args ->
                pmatch (Builtin.pargumentsRootV1 # args) $ \(PPair root count) -> pmatch (Builtin.pdirectBuiltinBudgetV1 # 70 # args) $ \budget ->
                    pif
                        ( pfromData (Machine.pstate'focusRoot pre)
                            #== Proof.phashBuiltinValueV1
                            # 70
                            # 0
                            # count
                            # root
                            #&& pfromData (Chain.pbound'post b)
                            #== Machine.pexactState
                                (pfromData $ Chain.pbound'pre b)
                                Machine.pmodeReturn
                                (Builtin.presultRootV1 # (Core.pdecodeValue # (pelemAt # 4 # f)))
                                Proof.pemptyEnvironmentRootV1
                                (pfromData $ Machine.pstate'continuationRoot pre)
                                0
                                (pfromData $ Cost.pbudget'cpu budget)
                                (pfromData $ Cost.pbudget'memory budget)
                        )
                        (continue $ Chain.pbound'facts b)
                        perror
blsRootsValidator = builtinHop 8 37 1 $ \bound raw continue -> pmatch bound $ \b -> plet (fields 37 5 raw) $ \f ->
    pif (Builtin.pverifyBlsExpressionRoots # (Witness.pdecodeBlsExpressionWitness # (pelemAt # 2 # f)) # (Witness.pdecodeBlsExpressionWitness # (pelemAt # 3 # f)) # (pasByteStr # (pelemAt # 0 # f)) # (pasByteStr # (pelemAt # 1 # f))) (continue $ Chain.pbound'facts b) perror
blsFinalValidator = builtinHop 8 37 2 $ \bound raw continue -> pmatch bound $ \b -> plet (fields 37 5 raw) $ \f ->
    pif (Builtin.pverifyAuthenticatedBlsFinal # (Witness.pdecodeBlsExpressionWitness # (pelemAt # 2 # f)) # (Witness.pdecodeBlsExpressionWitness # (pelemAt # 3 # f)) # (Core.pdecodeValue # (pelemAt # 4 # f))) (continue $ Chain.pbound'facts b) perror

mapStartRootsValidator, mapStartBudgetValidator, mapStartNodesValidator :: forall s. Hop s
mapStartRootsValidator = builtinHop 6 32 0 $ \bound raw continue ->
    pmatch (Chain.pdecodeBuiltinSuccess # 32 # raw) $ \item ->
        plet (pmap # plam (\value -> pdata $ Builtin.pcompactSemanticValue # pfromData value) # pfromData (Chain.psuccess'arguments item)) $ \args ->
            plet (Builtin.pcompactSemanticValue # pfromData (Chain.psuccess'result item)) $ \result ->
                pmatch (Builtin.pargumentsRootV1 # args) $ \(PPair root count) ->
                    pif
                        (argumentsBound bound (pfromData $ Chain.psuccess'tag item) root count)
                        (continue $ pforgetData $ pdata $ pcon $ Chain.PMapProjection (pdata args) (pdata result) (pforgetData $ pconstrBuiltin # 0 # pnil))
                        perror
mapStartBudgetValidator = builtinHop 6 32 1 $ \bound raw continue ->
    pmatch (Chain.pmapItem # bound # raw) $ \item ->
        plet (Builtin.pdirectBuiltinBudgetV1 # pfromData (Chain.psuccess'tag item) # pfromData (Chain.psuccess'arguments item)) $ \budget ->
            continue $ pforgetData $ pdata $ pcon $ Chain.PMapProjection (Chain.psuccess'arguments item) (Chain.psuccess'result item) (pforgetData $ pdata budget)
mapStartNodesValidator = builtinHop 6 32 2 $ \bound raw continue -> pmatch bound $ \b ->
    pmatch (Chain.pmapItem # bound # raw) $ \item -> pmatch (Chain.pdecodeMapProjection # Chain.pbound'facts b) $ \projection ->
        plet (fields 0 2 $ Chain.pmap'budget projection) $ \budget ->
            pif
                ( Machine.pverifyAuthenticatedMapConversionNodes
                    (pfromData $ Chain.pbound'pre b)
                    (pfromData $ Chain.pbound'post b)
                    (pfromData $ Chain.psuccess'tag item)
                    (pfromData $ Chain.psuccess'arguments item)
                    (pfromData $ Chain.psuccess'result item)
                    (Witness.pdecodeMapConversionStartWitness # Chain.psuccess'material item)
                    (pcon $ Cost.PBuiltinBudgetV1 (pdata $ pasInt # (pelemAt # 0 # budget)) (pdata $ pasInt # (pelemAt # 1 # budget)))
                )
                (continue $ Chain.pbound'facts b)
                perror
