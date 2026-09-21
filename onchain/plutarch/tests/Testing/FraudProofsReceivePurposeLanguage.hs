{-# LANGUAGE OverloadedStrings #-}

module Testing.FraudProofsReceivePurposeLanguage (tests) where

import Data.ByteString qualified as BS
import Plutarch.LedgerApi.Utils (PMaybeData (..))
import Plutarch.Prelude
import Test.Tasty
import Test.Tasty.HUnit

import Midgard.FraudProofs.ProofThreadSubstrate (PVerdictSubject (..))
import Midgard.FraudProofs.ReceivePurposeLanguage
import Midgard.RejectionReason (PRejectionReasonV1 (POutputNonCanonical, PReceivePurposePlutusV3Forbidden))
import Testing.Eval (passertEvalNoTrace, pfails)

txId, h32 :: BS.ByteString
txId = BS.pack [0 .. 31]
h32 = BS.replicate 32 0x11

h28 :: BS.ByteString
h28 = BS.replicate 28 0x22

subjectAt :: forall s. Bool -> Integer -> Term s PVerdictSubject
subjectAt forced executionIndex =
    pcon $
        PVerdictSubject
            (pdata 1)
            (pdata $ pconstant $ if forced then 1 else 0)
            (pdata $ pconstant $ if forced then 1 else 0)
            (pdata $ pconstant txId)
            (pdata $ pconstant $ if forced then "\x01" else "")
            ( pdata $
                if forced
                    then pcon $ PDJust $ pdata $ pcon $ PReceivePurposePlutusV3Forbidden $ pdata $ pconstant executionIndex
                    else pcon PDNothing
            )

bound :: forall s. Bool -> Integer -> Term s PBoundExecutionV1
bound forced executionIndex =
    pbindExecutionV1
        # subjectAt forced executionIndex
        # pconstant h32
        # 1
        # pconstant executionIndex

authenticated :: forall s. Bool -> Integer -> Term s PAuthenticatedReceiveLanguageV1
authenticated forced languageTag =
    pcon $
        PAuthenticatedReceiveLanguageV1
            (pdata $ bound forced 2)
            (pdata $ pconstant h32)
            (pdata 3)
            (pdata 2)
            (pdata 0)
            (pdata 0)
            (pdata $ phexByteStr "00")
            (pdata $ pconstant languageTag)
            (pdata $ pconstant h28)

tests :: TestTree
tests =
    testGroup
        "Receive purpose language"
        [ testCase "Plutus V3 convicts wrongful acceptance" $
            passertEvalNoTrace $
                pterminalContradictionV1 # authenticated False 3
        , testCase "native contradicts wrongful rejection" $
            passertEvalNoTrace $
                pterminalContradictionV1 # authenticated True 0
        , testCase "Midgard V1 contradicts wrongful rejection" $
            passertEvalNoTrace $
                pterminalContradictionV1 # authenticated True 128
        , testCase "Plutus V3 refuses wrongful rejection" $
            passertEvalNoTrace $
                pnot # (pterminalContradictionV1 # authenticated True 3)
        , testCase "native refuses wrongful acceptance" $
            passertEvalNoTrace $
                pnot # (pterminalContradictionV1 # authenticated False 0)
        , testCase "Midgard V1 refuses wrongful acceptance" $
            passertEvalNoTrace $
                pnot # (pterminalContradictionV1 # authenticated False 128)
        , testCase "reason coordinate substitution refuses" $
            pfails $
                pbindExecutionV1 # subjectAt True 2 # pconstant h32 # 1 # 1
        , testCase "reason constructor substitution refuses" $
            pfails $
                pbindExecutionV1
                    # pmatch
                        (subjectAt True 2)
                        ( \value ->
                            pcon
                                value
                                    { psubject'rejectionReason =
                                        pdata $ pcon $ PDJust $ pdata $ pcon $ POutputNonCanonical (pdata 2)
                                    }
                        )
                    # pconstant h32
                    # 1
                    # 2
        , testCase "bind refuses empty trace root" $
            pfails $
                pbindExecutionV1 # subjectAt False 2 # phexByteStr "0001" # 1 # 2
        , testCase "bind refuses zero trace count" $
            pfails $
                pbindExecutionV1 # subjectAt False 2 # pconstant h32 # 0 # 2
        , testCase "bind refuses negative execution index" $
            pfails $
                pbindExecutionV1 # subjectAt False (-1) # pconstant h32 # 1 # (-1)
        , testCase "invalid language tag is not forbidden" $
            passertEvalNoTrace $
                pnot # (pforbiddenReceiveLanguageHoldsV1 # 3 # 128)
        ]
