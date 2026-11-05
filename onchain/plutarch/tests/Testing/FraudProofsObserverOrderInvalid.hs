{-# LANGUAGE OverloadedStrings #-}

module Testing.FraudProofsObserverOrderInvalid (tests) where

import Data.ByteString qualified as BS
import Data.ByteString.Base16 qualified as Base16
import Plutarch.LedgerApi.Utils (PMaybeData (..))
import Plutarch.Prelude
import Test.Tasty
import Test.Tasty.HUnit

import Midgard.FraudProofs.ObserverOrderInvalid
import Midgard.FraudProofs.ProofThreadSubstrate (PVerdictSubject (..))
import Midgard.RejectionReason (PRejectionReasonV1 (PObserverOrderInvalid, POutputNonCanonical))
import Testing.Eval (passertEvalNoTrace, pfails)

txId, checkpoint, observerA, observerB, observerC :: BS.ByteString
txId = BS.pack [0 .. 31]
checkpoint = BS.replicate 32 0xaa
observerA = BS.replicate 28 0x00
observerB = BS.replicate 28 0x01
observerC = BS.replicate 28 0x02

subject :: forall s. Bool -> Integer -> Term s PVerdictSubject
subject forced index =
  pcon $
    PVerdictSubject
      (pdata 1)
      (pdata $ pconstant $ if forced then 1 else 0)
      (pdata $ pconstant $ if forced then 1 else 0)
      (pdata $ pconstant txId)
      (pdata $ pconstant $ if forced then "\x01" else "")
      ( pdata $
          if forced
            then pcon $ PDJust $ pdata $ pcon $ PObserverOrderInvalid (pdata $ pconstant index)
            else pcon PDNothing
      )

bound :: forall s. Bool -> Integer -> Term s PBoundObserverV1
bound forced index = pbindObserverV1 # subject forced index # pconstant index

initial :: forall s. Bool -> Integer -> Term s PScanStateV1
initial forced index = pinitialScanV1 # bound forced index # pconstant checkpoint

scan :: forall s. Bool -> Integer -> [BS.ByteString] -> Term s PScanStateV1
scan forced target =
  foldl
    (\state (index, observer) -> pscanItemV1 # state # pconstant index # pconstant observer)
    (initial forced target)
    . zip [0 :: Integer ..]

decision :: forall s. Bool -> Integer -> [BS.ByteString] -> Term s PDecisionStateV1
decision forced target observers = pdecisionV1 # scan forced target observers

tests :: TestTree
tests =
  testGroup
    "Observer order invalid"
    [ testCase "convicts accepted first descending pair" $
        passertEvalNoTrace $
          pterminalContradictionV1 # decision False 1 [observerB, observerA]
    , testCase "convicts accepted middle descending pair" $
        passertEvalNoTrace $
          pterminalContradictionV1 # decision False 2 [observerA, observerC, observerB]
    , testCase "convicts accepted duplicate pair" $
        passertEvalNoTrace $
          pterminalContradictionV1 # decision False 1 [observerA, observerA]
    , testCase "convicts wrongful rejection of ordered pair" $
        passertEvalNoTrace $
          pterminalContradictionV1 # decision True 2 [observerA, observerB, observerC]
    , testCase "refuses honest acceptance" $
        passertEvalNoTrace $
          pnot # (pterminalContradictionV1 # decision False 1 [observerA, observerB])
    , testCase "refuses honest rejection" $
        passertEvalNoTrace $
          pnot # (pterminalContradictionV1 # decision True 1 [observerB, observerA])
    , testCase "refuses forced reason coordinate substitution" $
        pfails $
          pbindObserverV1 # subject True 2 # 1
    , testCase "refuses another typed reason" $
        pfails $
          pbindObserverV1
            # pmatch
              (subject True 1)
              ( \value ->
                  pcon
                    value
                      { psubject'rejectionReason =
                          pdata $ pcon $ PDJust $ pdata $ pcon $ POutputNonCanonical (pdata 1)
                      }
              )
            # 1
    , testCase "refuses accepted zero coordinate" $
        pfails $
          pbindObserverV1 # subject False 0 # 0
    , testCase "forced zero coordinate is admissible and ordered" $
        passertEvalNoTrace $
          pmatch (scan True 0 [observerB]) $ \PScanStateV1{pscanState'outcome} ->
            pfromData pscanState'outcome #== poutcomeOrdered
    , testCase "refuses a non-32-byte checkpoint hash" $
        pfails $
          pinitialScanV1 # bound False 1 # phexByteStr "aa"
    , testCase "refuses substituted observer width" $
        pfails $
          pscanItemV1 # initial False 1 # 0 # phexByteStr "00"
    , testCase "refuses skipped coordinate" $
        pfails $
          pscanItemV1 # initial False 2 # 1 # pconstant observerA
    , testCase "refuses an earlier violation before the named ordinal" $
        pfails $
          scan False 2 [observerB, observerA, observerC]
    , testCase "exhausted ordered field contradicts out-of-range rejection" $
        passertEvalNoTrace $
          pterminalContradictionV1 # (pdecisionV1 # (pexhaustScanV1 # scan True 5 [observerA, observerB, observerC]))
    , testCase "empty field contradicts forced rejection" $
        passertEvalNoTrace $
          pterminalContradictionV1 # (pdecisionV1 # (pexhaustScanV1 # initial True 1))
    , testCase "exhausted field refuses acceptance" $
        passertEvalNoTrace $
          pnot # (pterminalContradictionV1 # (pdecisionV1 # (pexhaustScanV1 # scan False 4 [observerA, observerB])))
    , testCase "refuses to exhaust a decided scan" $
        pfails $
          pexhaustScanV1 # scan True 1 [observerA, observerB]
    , testCase "refuses to decide an active scan" $
        pfails $
          pdecisionV1 # initial False 1
    , testCase "bound Data ABI is stable" $
        passertEvalNoTrace $
          pserialiseData
            # pforgetData (pdata $ bound False 1)
            #== pconstant
              (hex "d8799fd8799f0100005820000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f40d87a80ff01ff")
    , testCase "initial scan Data ABI is stable" $
        passertEvalNoTrace $
          pserialiseData
            # pforgetData (pdata $ initial False 1)
            #== pconstant
              (hex "d8799fd8799f0100005820000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f40d87a80ff015820aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa004000ff")
    ]

hex :: BS.ByteString -> BS.ByteString
hex = either error id . Base16.decode
