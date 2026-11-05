{-# LANGUAGE OverloadedStrings #-}

module Testing.FraudProofsRedeemerCanonicity (tests) where

import Data.ByteString qualified as BS
import Data.ByteString.Base16 qualified as Base16
import Plutarch.LedgerApi.Utils (PMaybeData (..))
import Plutarch.Prelude
import Test.Tasty
import Test.Tasty.HUnit

import Midgard.FraudProofs.ProofThreadSubstrate (PVerdictSubject (..))
import Midgard.FraudProofs.RedeemerCanonicity
import Midgard.RejectionReason (PRejectionReasonV1 (PRedeemerMalformed, PRedeemerMissing))
import Testing.Eval (passertEvalNoTrace, pfails)

txId :: BS.ByteString
txId = BS.pack [0 .. 31]

witnessSetHash :: BS.ByteString
witnessSetHash = BS.pack [32 .. 63]

subject :: forall s. Bool -> Integer -> Term s PVerdictSubject
subject forced redeemerIndex =
  pcon $
    PVerdictSubject
      (pdata 1)
      (pdata $ pconstant $ if forced then 1 else 0)
      (pdata $ pconstant $ if forced then 1 else 0)
      (pdata $ pconstant txId)
      (pdata $ pconstant $ if forced then "\x01" else "")
      ( pdata $
          if forced
            then pcon $ PDJust $ pdata $ pcon $ PRedeemerMalformed (pdata $ pconstant redeemerIndex)
            else pcon PDNothing
      )

bound :: forall s. Bool -> Integer -> Term s PBoundRedeemerV1
bound forced redeemerIndex =
  pbindRedeemerV1
    # subject forced redeemerIndex
    # pconstant witnessSetHash
    # pconstant redeemerIndex

terminal :: forall s. Bool -> Integer -> Bool -> Term s PTerminalStateV1
terminal forced redeemerIndex canonical =
  pcon $
    PTerminalStateV1
      (pdata $ bound forced redeemerIndex)
      (pdata $ pconstant canonical)

tests :: TestTree
tests =
  testGroup
    "Redeemer canonicity"
    [ testCase "binds exact forced coordinate" $
        passertEvalNoTrace $
          pmatch (bound True 7) $ \PBoundRedeemerV1{pboundRedeemer'redeemerIndex} ->
            pfromData pboundRedeemer'redeemerIndex #== 7
    , testCase "refuses forced coordinate substitution" $
        pfails $
          pbindRedeemerV1 # subject True 7 # pconstant witnessSetHash # 8
    , testCase "refuses another typed reason" $
        pfails $
          pbindRedeemerV1
            # pmatch
              (subject True 0)
              ( \value ->
                  pcon
                    value
                      { psubject'rejectionReason =
                          pdata $ pcon $ PDJust $ pdata $ pcon $ PRedeemerMissing (pdata 0) (pdata 0)
                      }
              )
            # pconstant witnessSetHash
            # 0
    , testCase "refuses negative coordinate" $
        pfails $
          pbindRedeemerV1 # subject False 0 # pconstant witnessSetHash # (-1)
    , testCase "refuses a non-32-byte witness-set hash" $
        pfails $
          pbindRedeemerV1 # subject False 0 # phexByteStr "00" # 0
    , testCase "wrongful acceptance convicts malformed item" $
        passertEvalNoTrace $
          pterminalContradictionV1 # terminal False 0 False
    , testCase "wrongful rejection convicts canonical item" $
        passertEvalNoTrace $
          pterminalContradictionV1 # terminal True 0 True
    , testCase "honest acceptance refuses canonical item" $
        passertEvalNoTrace $
          pnot # (pterminalContradictionV1 # terminal False 0 True)
    , testCase "honest rejection refuses malformed item" $
        passertEvalNoTrace $
          pnot # (pterminalContradictionV1 # terminal True 0 False)
    , testCase "total decoder accepts the canonical target vector" $
        passertEvalNoTrace $
          pitemIsCanonicalV1 # phexByteStr "8400004100820102"
    , testCase "total decoder accepts every supported purpose tag" $
        passertEvalNoTrace $
          pitemIsCanonicalV1
            # phexByteStr "8401004100820102"
            #&& pitemIsCanonicalV1
            # phexByteStr "8403004100820102"
            #&& pitemIsCanonicalV1
            # phexByteStr "8406004100820102"
    , testCase "total decoder rejects nonminimal Plutus Data" $
        passertEvalNoTrace $
          pnot # (pitemIsCanonicalV1 # phexByteStr "840000421800820102")
    , testCase "total decoder rejects malformed envelope" $
        passertEvalNoTrace $
          pnot # (pitemIsCanonicalV1 # phexByteStr "8300004100820102")
    , testCase "total decoder rejects trailing bytes" $
        passertEvalNoTrace $
          pnot # (pitemIsCanonicalV1 # phexByteStr "840000410082010200")
    , testCase "total decoder rejects unsupported purpose" $
        passertEvalNoTrace $
          pnot # (pitemIsCanonicalV1 # phexByteStr "8402004100820102")
    , testCase "total decoder rejects malformed and truncated bytes without aborting" $
        passertEvalNoTrace $
          pnot
            # (pitemIsCanonicalV1 # phexByteStr "")
            #&& pnot
            # (pitemIsCanonicalV1 # phexByteStr "84000041008201")
    , testCase "authentication derives the canonical bit" $
        passertEvalNoTrace $
          pmatch
            (pauthenticateItemV1 # bound False 0 # phexByteStr "8400004100820102")
            (\PTerminalStateV1{pterminalState'canonical} -> pfromData pterminalState'canonical)
    , testCase "bound Data ABI matches target" $
        passertEvalNoTrace $
          pserialiseData
            # pforgetData (pdata $ bound False 0)
            #== pconstant
              (hex "d8799fd8799f0100005820000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f40d87a80ff5820202122232425262728292a2b2c2d2e2f303132333435363738393a3b3c3d3e3f00ff")
    , testCase "terminal Data ABI matches target" $
        passertEvalNoTrace $
          pserialiseData
            # pforgetData (pdata $ terminal False 0 True)
            #== pconstant
              (hex "d8799fd8799fd8799f0100005820000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f40d87a80ff5820202122232425262728292a2b2c2d2e2f303132333435363738393a3b3c3d3e3f00ffd87a80ff")
    ]

hex :: BS.ByteString -> BS.ByteString
hex = either error id . Base16.decode
