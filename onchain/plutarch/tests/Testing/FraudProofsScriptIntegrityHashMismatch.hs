{-# LANGUAGE OverloadedStrings #-}

module Testing.FraudProofsScriptIntegrityHashMismatch (tests) where

import Data.ByteString qualified as BS
import Plutarch.LedgerApi.Utils (PMaybeData (..))
import Plutarch.Prelude
import Test.Tasty
import Test.Tasty.HUnit

import Midgard.FraudProofs.ProofThreadSubstrate (PVerdictSubject (..))
import Midgard.FraudProofs.ScriptIntegrityHashMismatch
import Midgard.RejectionReason (PRejectionReasonV1 (POutputNonCanonical, PScriptIntegrityHashMismatch))
import Testing.Eval (passertEvalNoTrace, pfails)

txId, h32, redeemerHash :: BS.ByteString
txId = BS.pack [0 .. 31]
h32 = BS.replicate 32 0x11
redeemerHash = h32

subject :: forall s. Bool -> Term s PVerdictSubject
subject forced =
  pcon $
    PVerdictSubject
      (pdata 1)
      (pdata $ pconstant $ if forced then 1 else 0)
      (pdata $ pconstant $ if forced then 1 else 0)
      (pdata $ pconstant txId)
      (pdata $ pconstant $ if forced then "\x01" else "")
      ( pdata $
          if forced
            then pcon $ PDJust $ pdata $ pcon PScriptIntegrityHashMismatch
            else pcon PDNothing
      )

bound :: forall s. Bool -> BS.ByteString -> Term s PBoundIntegrityV1
bound forced committedHash =
  pbindIntegrityV1
    # subject forced
    # pconstant h32
    # 1
    # pconstant committedHash

authenticated :: forall s. Bool -> Integer -> BS.ByteString -> Term s PAuthenticatedIntegrityV1
authenticated forced bitmap committedHash =
  pcon $
    PAuthenticatedIntegrityV1
      (pdata $ bound forced committedHash)
      (pdata $ pconstant h32)
      (pdata $ pconstant redeemerHash)
      (pdata $ pconstant bitmap)
      (pdata 2)

decision :: forall s. Bool -> Integer -> BS.ByteString -> Term s PDecisionV1
decision forced bitmap committedHash =
  pdecideIntegrityV1
    # ( pfoldNextLanguageV1
          # ( pfoldNextLanguageV1
                # (pinitializeLanguageFoldV1 # authenticated forced bitmap committedHash)
            )
      )

tests :: TestTree
tests =
  testGroup
    "Script integrity hash mismatch"
    [ expectedHashCase "folds empty language view vector" 0 "\x01\xf4\xb7\x88\x59\x3d\x4f\x70\xde\x2a\x45\xc2\xe1\xe8\x70\x88\xbf\xbd\xfa\x29\x57\x7a\xe1\xb6\x2a\xba\x60\xe0\x95\xe3\xab\x53"
    , expectedHashCase "folds Plutus V3 language view vector" 1 "\xd7\x23\x9e\xb1\xbd\x8b\x73\x76\xde\xdf\xbf\x7e\x62\x01\x81\x5b\x22\x5c\x02\x3d\x11\xc9\x75\xcd\x99\xd2\x5d\x52\x36\xb1\x99\xa1"
    , expectedHashCase "folds Midgard V1 language view vector" 2 "\x71\x20\x1d\x25\xea\x11\xe4\x10\x4e\xda\x10\x87\x82\xa7\xd6\x7b\x37\xb4\xae\x97\xdf\x6d\xc3\x25\x8b\x06\xd9\xc9\x8e\x58\xbb\xcb"
    , expectedHashCase "folds dual language view vector" 3 "\x6d\x49\xb4\xf2\x4c\x60\xbe\xc1\xcb\x34\xa2\x53\x82\x78\x25\x20\x59\xec\x06\x01\xb7\xf6\x75\xef\x73\xfe\x2b\x48\xe2\x43\x17\xd8"
    , testCase "mismatch convicts wrongful acceptance" $
        passertEvalNoTrace $
          pterminalContradictionV1 # decision False 3 h32
    , testCase "equality contradicts wrongful rejection" $
        passertEvalNoTrace $
          pterminalContradictionV1
            # decision True 1 "\xd7\x23\x9e\xb1\xbd\x8b\x73\x76\xde\xdf\xbf\x7e\x62\x01\x81\x5b\x22\x5c\x02\x3d\x11\xc9\x75\xcd\x99\xd2\x5d\x52\x36\xb1\x99\xa1"
    , testCase "equality refuses wrongful acceptance" $
        passertEvalNoTrace $
          pnot
            # ( pterminalContradictionV1
                  # decision False 2 "\x71\x20\x1d\x25\xea\x11\xe4\x10\x4e\xda\x10\x87\x82\xa7\xd6\x7b\x37\xb4\xae\x97\xdf\x6d\xc3\x25\x8b\x06\xd9\xc9\x8e\x58\xbb\xcb"
              )
    , testCase "mismatch refuses wrongful rejection" $
        passertEvalNoTrace $
          pnot # (pterminalContradictionV1 # decision True 0 h32)
    , testCase "wrong reason constructor refuses" $
        pfails $
          pbindIntegrityV1
            # pmatch
              (subject True)
              ( \value ->
                  pcon
                    value
                      { psubject'rejectionReason =
                          pdata $ pcon $ PDJust $ pdata $ pcon $ POutputNonCanonical (pdata 0)
                      }
              )
            # pconstant h32
            # 1
            # pconstant h32
    , testCase "bitmap above canonical domain refuses" $
        pfails $
          pinitializeLanguageFoldV1 # authenticated False 4 h32
    , testCase "premature fold finalization refuses" $
        pfails $
          pdecideIntegrityV1
            # ( pfoldNextLanguageV1
                  # (pinitializeLanguageFoldV1 # authenticated False 3 h32)
              )
    , testCase "bind refuses zero trace count" $
        pfails $
          pbindIntegrityV1 # subject False # pconstant h32 # 0 # pconstant h32
    , testCase "bind refuses non-32-byte hashes" $
        pfails $
          pbindIntegrityV1 # subject False # phexByteStr "00" # 1 # pconstant h32
    ]

expectedHashCase :: String -> Integer -> BS.ByteString -> TestTree
expectedHashCase name bitmap expected =
  testCase name $
    passertEvalNoTrace $
      pmatch (decision False bitmap h32) $ \PDecisionV1{pdecision'expectedHash} ->
        pfromData pdecision'expectedHash #== pconstant expected
