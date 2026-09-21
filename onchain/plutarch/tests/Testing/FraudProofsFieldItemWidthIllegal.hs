{-# LANGUAGE OverloadedStrings #-}

module Testing.FraudProofsFieldItemWidthIllegal (tests) where

import Data.ByteString qualified as BS
import Data.ByteString.Base16 qualified as Base16
import Plutarch.LedgerApi.Utils (PMaybeData (..))
import Plutarch.Prelude
import Test.Tasty
import Test.Tasty.HUnit

import Midgard.FraudProofs.FieldItemWidthIllegal
import Midgard.FraudProofs.ProofThreadSubstrate (PVerdictSubject (..))
import Midgard.RejectionReason (PRejectionReasonV1 (PFieldItemWidthIllegal, PNetworkIdMismatch))
import Testing.Eval (passertEvalNoTrace, pfails)

subject :: forall s. Bool -> Integer -> Integer -> Term s PVerdictSubject
subject forced fieldIndex itemIndex =
  pcon $
    PVerdictSubject
      (pdata 1)
      (pdata $ pconstant $ if forced then 1 else 0)
      (pdata $ pconstant $ if forced then 1 else 0)
      (pdata $ pconstant $ BS.pack [0 .. 31])
      (pdata $ pconstant $ if forced then "source" else "")
      ( pdata $
          if forced
            then pcon $ PDJust $ pdata $ pcon $ PFieldItemWidthIllegal (pdata $ pconstant fieldIndex) (pdata $ pconstant itemIndex)
            else pcon PDNothing
      )

widthState :: forall s. Term s PVerdictSubject -> Integer -> Integer -> Integer -> Term s PAuthenticatedWidth
widthState verdict fieldIndex itemIndex itemWidth =
  pcon $
    PAuthenticatedWidth
      (pdata verdict)
      (pdata $ pconstant fieldIndex)
      (pdata $ pconstant itemIndex)
      (pdata $ pconstant itemWidth)

tests :: TestTree
tests =
  testGroup
    "Field item width illegal"
    [ testCase "supports only output and mint coordinates" $
        passertEvalNoTrace $
          pcoordinateIsSupported
            # 2
            # 0
            #&& pcoordinateIsSupported
            # 5
            # 7
            #&& pnot
            # (pcoordinateIsSupported # 4 # 0)
            #&& pnot
            # (pcoordinateIsSupported # 2 # (-1))
    , testCase "binds accepted coordinate" $
        passertEvalNoTrace $
          pmatch (pbindCoordinate # subject False 2 0 # 2 # 0) $ \PBoundCoordinate{pboundCoordinate'itemIndex} ->
            pfromData pboundCoordinate'itemIndex #== 0
    , testCase "binds exact forced coordinate" $
        passertEvalNoTrace $
          pmatch (pbindCoordinate # subject True 5 7 # 5 # 7) $ \PBoundCoordinate{pboundCoordinate'fieldIndex, pboundCoordinate'itemIndex} ->
            pfromData pboundCoordinate'fieldIndex #== 5 #&& pfromData pboundCoordinate'itemIndex #== 7
    , testCase "refuses mutated forced item coordinate" $
        pfails $
          pbindCoordinate # subject True 2 7 # 2 # 8
    , testCase "refuses mutated forced field coordinate" $
        pfails $
          pbindCoordinate # subject True 2 7 # 5 # 7
    , testCase "refuses another forced reason" $
        pfails $
          pbindCoordinate
            # pmatch
              (subject True 2 0)
              ( \value ->
                  pcon
                    value
                      { psubject'rejectionReason =
                          pdata $ pcon $ PDJust $ pdata $ pcon PNetworkIdMismatch
                      }
              )
            # 2
            # 0
    , testCase "authentication derives width from bytes"
        $ passertEvalNoTrace
        $ pmatch
          (pauthenticateItemWidth # (pbindCoordinate # subject False 5 0 # 5 # 0) # phexByteStr "000102")
        $ \PAuthenticatedWidth{pauthenticatedWidth'itemWidth} ->
          pfromData pauthenticatedWidth'itemWidth #== 3
    , testCase "classifies exact output and mint boundaries" $
        passertEvalNoTrace $
          pitemWidthIsIllegal
            # 2
            # 16385
            #&& pnot
            # (pitemWidthIsIllegal # 2 # 16384)
            #&& pitemWidthIsIllegal
            # 5
            # 0
            #&& pnot
            # (pitemWidthIsIllegal # 5 # 1)
    , testCase "refuses negative width" $ pfails $ pitemWidthIsIllegal # 2 # (-1)
    , testCase "convicts accepted oversize output" $
        passertEvalNoTrace $
          pterminalContradiction # widthState (subject False 2 0) 2 0 16385
    , testCase "convicts accepted empty mint item" $
        passertEvalNoTrace $
          pterminalContradiction # widthState (subject False 5 3) 5 3 0
    , testCase "convicts wrongfully rejected legal output" $
        passertEvalNoTrace $
          pterminalContradiction # widthState (subject True 2 0) 2 0 16384
    , testCase "convicts wrongfully rejected nonempty mint item" $
        passertEvalNoTrace $
          pterminalContradiction # widthState (subject True 5 3) 5 3 1
    , testCase "refuses honestly accepted boundary output" $
        passertEvalNoTrace $
          pnot # (pterminalContradiction # widthState (subject False 2 0) 2 0 16384)
    , testCase "refuses honestly rejected oversize output" $
        passertEvalNoTrace $
          pnot # (pterminalContradiction # widthState (subject True 2 0) 2 0 16385)
    , testCase "terminal refuses unrelated field" $
        pfails $
          pterminalContradiction # widthState (subject False 2 0) 4 0 0
    , testCase "bound-coordinate Data ABI matches target" $
        passertEvalNoTrace $
          pserialiseData
            # pforgetData (pdata $ pbindCoordinate # subject False 5 0 # 5 # 0)
            #== pconstant
              (hex "d8799fd8799f0100005820000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f40d87a80ff0500ff")
    , testCase "authenticated-width Data ABI matches target" $
        passertEvalNoTrace $
          pserialiseData
            # pforgetData
              ( pdata $
                  pauthenticateItemWidth
                    # (pbindCoordinate # subject False 5 0 # 5 # 0)
                    # phexByteStr "000102"
              )
            #== pconstant
              (hex "d8799fd8799f0100005820000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f40d87a80ff050003ff")
    ]

hex :: BS.ByteString -> BS.ByteString
hex = either error id . Base16.decode
