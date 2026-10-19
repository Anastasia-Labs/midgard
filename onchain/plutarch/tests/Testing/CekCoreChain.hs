{-# LANGUAGE OverloadedStrings #-}

module Testing.CekCoreChain (tests) where

import Data.ByteString qualified as BS
import Midgard.CekCoreWitness qualified as Witness
import Midgard.CekSemanticChain qualified as Chain
import Plutarch.Builtin.Crypto (pblake2b_256)
import Plutarch.Prelude
import PlutusCore.Data qualified as D
import Test.Tasty
import Test.Tasty.HUnit
import Testing.Eval (passertEvalNoTraceWithoutHoistChecks, pfailsNoTraceWithoutHoistChecks)

tests :: TestTree
tests =
    testGroup
        "CEK core chain"
        [ testGroup
            "compute witness wire"
            [ testCase (show tag) $
                passertEvalNoTraceWithoutHoistChecks $
                    pforgetData (pdata $ Witness.pdecodeCompute # pconstant (D.Constr tag fields)) #== pconstant (D.Constr tag fields)
            | (tag, fields) <- computeVectors
            ]
        , testGroup
            "compute witness refuses"
            [ testCase name $ pfailsNoTraceWithoutHoistChecks $ Witness.pdecodeCompute # pconstant raw
            | (name, raw) <-
                [ ("machine-only arm", D.Constr 11 [D.B "x", D.B "y", D.I 0])
                , ("unknown arm", D.Constr 41 [])
                , ("extra field", D.Constr 0 [D.I 0, D.I 1])
                , ("wrong scalar type", D.Constr 0 [D.B "0"])
                , ("wrong constant constructor", D.Constr 1 [D.Constr 1 [D.B "type", D.B "payload"]])
                , ("malformed constant field", D.Constr 1 [D.Constr 0 [D.I 0, D.B "payload"]])
                , ("context constant missing root", D.Constr 40 [])
                ]
            ]
        , testGroup
            "core mode routing"
            [ testCase (show (mode, arm)) $
                passertEvalNoTraceWithoutHoistChecks $
                    Chain.pcoreGroup # pconstant mode # pconstant (D.Constr arm []) #== pconstant group
            | (mode, arms, group) <- [(0, [0 .. 10] ++ [40], 0), (2, [11, 12], 1), (1, [13 .. 27], 1), (6, [28], 1), (7, [29], 1), (8, [33 .. 35], 2), (3, [31], 5), (3, [32], 6), (3, [36], 7), (3, [37], 8), (3, [38], 9), (3, [39], 10)]
            , arm <- arms
            ]
        , testCase "direct success routing keeps signatures scalar" $
            passertEvalNoTraceWithoutHoistChecks $
                Chain.pcoreGroup
                    # 3
                    # pconstant (D.Constr 30 [D.I 52, D.List [], D.Constr 0 []])
                    #== 3
                    #&& Chain.pcoreGroup
                    # 3
                    # pconstant (D.Constr 30 [D.I 53, D.List [], D.Constr 0 []])
                    #== 3
                    #&& Chain.pcoreGroup
                    # 3
                    # pconstant (D.Constr 30 [D.I 68, D.List [], D.Constr 0 []])
                    #== 4
        , testCase "routing rejects a halted mode" $ pfailsNoTraceWithoutHoistChecks $ Chain.pcoreGroup # 4 # pconstant (D.Constr 0 [])
        , testCase "routing rejects an arm of another mode" $ pfailsNoTraceWithoutHoistChecks $ Chain.pcoreGroup # 0 # pconstant (D.Constr 11 [])
        , testCase "direct routing checks its tag type" $ pfailsNoTraceWithoutHoistChecks $ Chain.pcoreGroup # 3 # pconstant (D.Constr 30 [D.B "tag", D.List [], D.Constr 0 []])
        , testGroup
            "semantic tag routing"
            [ testCase (show tag) $ passertEvalNoTraceWithoutHoistChecks $ Chain.psemanticGroup # pconstant tag #== pconstant group
            | (tags, group) <- [([29, 30], 0), ([31, 32, 35], 1), ([33, 34], 2), ([36], 3), ([37, 42], 4), ([39, 40, 41, 44, 45, 46], 5), ([47 .. 51], 6)]
            , tag <- tags
            ]
        , testCase "map conversion has its own route" $ pfailsNoTraceWithoutHoistChecks $ Chain.psemanticGroup # 38
        , testCase "witness binds both hash and arm" $
            passertEvalNoTraceWithoutHoistChecks $
                Chain.pwitnessIsBound
                    # bound
                    # pconstant witnessData
                    #&& pnot
                    # (Chain.pwitnessIsBound # bound # pconstant (D.Constr 7 [D.I 1]))
                    #&& (pmatch bound $ \b -> pnot # (Chain.pwitnessIsBound # pcon b{Chain.pbound'arm = pdata 6} # pconstant witnessData))
        , testCase "bound layout keeps prepared and facts opaque" $
            passertEvalNoTraceWithoutHoistChecks $
                pforgetData (pdata bound) #== boundData
        , testCase "builtin opening checks its claimed arm" $ pfailsNoTraceWithoutHoistChecks $ Chain.pdecodeBuiltinSuccess # 31 # pconstant builtinData
        , testCase "direct builtin opening gives empty material" $
            passertEvalNoTraceWithoutHoistChecks $
                pmatch (Chain.pdecodeBuiltinSuccess # 30 # pconstant builtinData) $ \success ->
                    pfromData (Chain.psuccess'tag success) #== 0 #&& Chain.psuccess'material success #== pconstant (D.Constr 0 [])
        , testCase "builtin opening rejects extra arguments fields" $
            pfailsNoTraceWithoutHoistChecks $
                Chain.pdecodeBuiltinSuccess # 30 # pconstant (D.Constr 30 [D.I 0, D.List [], D.Constr 2 [D.B "opaque"], D.I 0])
        , testCase "builtin opening rejects malformed nested value" $
            pfailsNoTraceWithoutHoistChecks $
                Chain.pdecodeBuiltinSuccess # 30 # pconstant (D.Constr 30 [D.I 0, D.List [D.Constr 0 [D.Constr 0 [D.I 0, D.B ""]]], D.Constr 2 [D.B "opaque"]])
        , testCase "semantic facts replace values but preserve raw material" $
            passertEvalNoTraceWithoutHoistChecks $
                pmatch bound $ \b ->
                    pmatch (Chain.psemanticItem # pcon b{Chain.pbound'arm = pdata 31, Chain.pbound'facts = pconstant semanticFacts} # pconstant semanticData) $ \item ->
                        pforgetData (Chain.psuccess'arguments item)
                            #== pconstant (D.List [D.Constr 2 [D.B "pinned"]])
                            #&& pforgetData (Chain.psuccess'result item)
                            #== pconstant (D.Constr 2 [D.B "pinned result"])
                            #&& Chain.psuccess'material item
                            #== pconstant (D.I 99)
        ]

computeVectors :: [(Integer, [D.Data])]
computeVectors = [(0, [D.I 0]), (1, [D.Constr 0 [D.B "type", D.B "payload"]]), (2, [D.B "body"]), (3, [D.B "body"]), (4, [D.B "f", D.B "a"]), (5, [D.B "t"]), (6, []), (7, [D.I 0]), (8, [D.I 1]), (9, [D.I 0, D.I 2, D.B "head", D.B "tail"]), (10, [D.B "scrutinee", D.I 2, D.B "branches"]), (40, [D.B "root"])]
witnessData, machineData, builtinData, semanticData, semanticFacts :: D.Data
witnessData = D.Constr 7 [D.I 0]
machineData = D.Constr 0 [D.I 0, D.I 0, D.B "focus", D.B "env", D.B "continuation", D.I 0, D.I 0, D.I 0]
builtinData = D.Constr 30 [D.I 0, D.List [], D.Constr 2 [D.B "opaque"]]
semanticData = D.Constr 31 [D.I 33, D.List [], D.Constr 2 [D.B "raw result"], D.I 99]
semanticFacts = D.Constr 0 [D.Constr 0 [D.B "args root", D.I 1, D.B "result root", D.B "builtin root"], D.List [D.Constr 2 [D.B "pinned"]], D.Constr 2 [D.B "pinned result"]]

boundData :: forall s. Term s PData
boundData =
    pforgetData $
        pconstrBuiltin
            # 0
            # ( pcons
                    # pconstant (D.I 123)
                    # ( pcons
                            # pconstant machineData
                            # ( pcons
                                    # pconstant machineData
                                    # ( pcons
                                            # pforgetData (pdata $ pblake2b_256 # (pserialiseData # pconstant witnessData))
                                            # (pcons # pconstant (D.I 7) # (pcons # pconstant (D.I 0) # (pcons # pconstant (D.I 0) # (pcons # pconstant (D.B $ BS.pack [0, 1, 2]) # pnil))))
                                      )
                              )
                      )
              )
bound :: forall s. Term s Chain.PCoreBound
bound = Chain.pdecodeBound # boundData
