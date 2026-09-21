{-# LANGUAGE OverloadedStrings #-}

module Testing.CekMaterialTraversal (tests) where

import Midgard.CekMaterialTraversal qualified as Material
import Midgard.CekProof qualified as Proof
import Midgard.LedgerState (PCekProgramMaterialDatumV1 (..))
import Midgard.MpfProof.Types (PProof (..))
import Plutarch.Prelude
import Test.Tasty
import Test.Tasty.HUnit
import Testing.Eval (passertEvalNoTraceWithoutHoistChecks, pfailsNoTraceWithoutHoistChecks)

tests :: TestTree
tests =
    testGroup
        "CEK material traversal"
        [ testCase "accounts a unique node once and admits its repeated membership" $
            passertEvalNoTraceWithoutHoistChecks $
                plet (Material.pinitial # Proof.phashErrorTermV1 # 1 # 2) $ \initial ->
                    plet (Material.padvance # initial # action) $ \first -> pmatch initial $ \i -> pmatch first $ \f -> pmatch action $ \a ->
                        pfromData (Material.pstate'nodeCount f)
                            #== 1
                            #&& pfromData (Material.pstate'byteLength f)
                            #== 2
                            #&& pfromData (Material.pstate'pendingRoot f)
                            #== Material.pemptyStack
                            #&& Material.padvance
                            # pcon f{Material.pstate'pendingRoot = Material.pstate'pendingRoot i}
                            # pcon a{Material.pvisit'alreadySeen = pdata $ pconstant True}
                            #== first
        , testCase "forged visited membership fails" $
            pfailsNoTraceWithoutHoistChecks $
                pmatch action $
                    \a -> Material.padvance # (Material.pinitial # Proof.phashErrorTermV1 # 1 # 2) # pcon a{Material.pvisit'alreadySeen = pdata $ pconstant True}
        , testCase "a duplicate cannot be inserted as new" $
            pfailsNoTraceWithoutHoistChecks $
                plet (Material.pinitial # Proof.phashErrorTermV1 # 2 # 4) $ \initial -> pmatch initial $ \i ->
                    pmatch (Material.padvance # initial # action) $ \first -> Material.padvance # pcon first{Material.pstate'pendingRoot = Material.pstate'pendingRoot i} # action
        , testCase "substituted stack tail fails" $
            pfailsNoTraceWithoutHoistChecks $
                pmatch action $
                    \a -> Material.padvance # (Material.pinitial # Proof.phashErrorTermV1 # 1 # 2) # pcon a{Material.pvisit'tailRoot = pdata $ pconstant "wrong"}
        , testCase "substituted entry root fails" $
            pfailsNoTraceWithoutHoistChecks $
                pmatch action $ \a -> pmatch (pfromData $ Material.pvisit'entry a) $ \e ->
                    Material.padvance
                        # (Material.pinitial # Proof.phashErrorTermV1 # 1 # 2)
                        # pcon a{Material.pvisit'entry = pdata $ pcon e{pcekProgramMaterial'root = pdata $ Proof.phashVariableTermV1 # 0}}
        , testCase "node count cannot exceed the declared total" $ pfailsNoTraceWithoutHoistChecks $ Material.padvance # (Material.pinitial # Proof.phashErrorTermV1 # 0 # 2) # action
        , testCase "byte count cannot exceed the declared total" $ pfailsNoTraceWithoutHoistChecks $ Material.padvance # (Material.pinitial # Proof.phashErrorTermV1 # 1 # 1) # action
        , testCase "stack order and every child are committed" $
            passertEvalNoTraceWithoutHoistChecks $
                plet (pcon $ Proof.PProgramMaterialTaskV1 0 Proof.phashErrorTermV1 (-1)) $ \first ->
                    plet (pcon $ Proof.PProgramMaterialTaskV1 0 (Proof.phashVariableTermV1 # 0) (-1)) $ \second ->
                        plet (Material.ppushChildren # (pcons # first # (pcons # second # pnil)) # Material.pemptyStack) $ \stack ->
                            plengthBS
                                # stack
                                #== 32
                                #&& pnot
                                # (stack #== Material.ppushChildren # (pcons # second # (pcons # first # pnil)) # Material.pemptyStack)
                                #&& pnot
                                # (stack #== Material.ppush # first # Material.pemptyStack)
                                #&& pnot
                                # (stack #== Material.ppush # second # Material.pemptyStack)
                                #&& Material.ppushChildren
                                # pnil
                                # Material.pemptyStack
                                #== Material.pemptyStack
        , testCase "visit uses the exact nine-field action layout" $
            passertEvalNoTraceWithoutHoistChecks $
                pforgetData (pdata $ Material.pdecodeAction # pforgetData (pdata action)) #== pforgetData (pdata action)
        , testCase "state uses the exact six-field layout" $
            passertEvalNoTraceWithoutHoistChecks $
                plet (Material.pinitial # Proof.phashErrorTermV1 # 1 # 2) $
                    \state -> Material.pdecodeState # pforgetData (pdata state) #== state
        ]

action :: forall s. Term s Material.PAction
action =
    pcon $
        Material.PVisit
            (pdata 0)
            (pdata 0)
            taskData
            (pdata Material.pemptyStack)
            (pdata $ pcon $ PCekProgramMaterialDatumV1 (pdata 0) (pdata Proof.phashErrorTermV1) (pdata $ pconstant "\x81\x06"))
            (pdata $ pconstant False)
            (pdata $ pcon $ PProof pnil)
            (pdata 0)
            (pdata Material.pemptyStack)

taskData :: forall s. Term s PData
taskData =
    pforgetData $
        pconstrBuiltin
            # 0
            # (pcons # pforgetData (pdata $ pconstant @PInteger 0) # (pcons # pforgetData (pdata Proof.phashErrorTermV1) # (pcons # pforgetData (pdata $ pconstant @PInteger (-1)) # pnil)))
