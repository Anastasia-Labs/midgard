{-# LANGUAGE OverloadedStrings #-}

module Testing.FraudProofsDistinctAssetAccumulationLimit (tests) where

import Data.ByteString qualified as BS
import Plutarch.LedgerApi.Utils (PMaybeData (..))
import Plutarch.Prelude
import Test.Tasty
import Test.Tasty.HUnit

import Midgard.FraudProofs.DistinctAssetAccumulationLimit
import Midgard.FraudProofs.ProofThreadSubstrate (PVerdictSubject (..))
import Midgard.RejectionReason (PRejectionReasonV1 (..))
import Testing.Eval (passertEvalNoTrace, pfails)

txId, root :: BS.ByteString
txId = BS.pack [0 .. 31]
root = BS.replicate 32 0x11

accepted :: forall s. Term s PVerdictSubject
accepted = pcon $ PVerdictSubject (pdata 1) (pdata 0) (pdata 0) (pdata $ pconstant txId) (pdata $ pconstant "") (pdata $ pcon PDNothing)

forced :: forall s. Term s PRejectionReasonV1 -> Term s PVerdictSubject
forced reason = pcon $ PVerdictSubject (pdata 1) (pdata 1) (pdata 1) (pdata $ pconstant txId) (pdata $ phexByteStr "01") (pdata $ pcon $ PDJust $ pdata reason)

coordinate :: forall s. Integer -> Integer -> Integer -> Term s PCoordinateV1
coordinate fold primary asset = pcon $ PCoordinateV1 (pdata $ pconstant fold) (pdata $ pconstant primary) (pdata $ pconstant asset)

bound :: forall s. Term s PVerdictSubject -> Term s PCoordinateV1 -> Term s PBoundV1
bound subject point = pbindCoordinateV1 # subject # pconstant root # 1 # point

state :: forall s. Term s PVerdictSubject -> Term s PCoordinateV1 -> Integer -> Maybe Bool -> Term s PFoldStateV1
state subject point stage decision =
  pcon $ PFoldStateV1 (pdata $ bound subject point) (pdata $ pcon PDNothing) (pdata $ pconstant stage) (pdata $ case decision of Nothing -> pcon PDNothing; Just value -> pcon $ PDJust $ pdata $ pconstant value)

tests :: TestTree
tests =
  testGroup
    "Distinct asset accumulation limit"
    [ testCase "input coordinate binds forced reason" $
        passertEvalNoTrace $
          pmatch (bound (forced $ pcon $ PInputAssetAccumulationLimit (pdata 2) (pdata 7)) (coordinate 0 2 7)) $ \PBoundV1{pbound'coordinate} ->
            pmatch (pfromData pbound'coordinate) $ \PCoordinateV1{pcoordinate'assetIndex} -> pfromData pcoordinate'assetIndex #== 7
    , testCase "output coordinate binds forced reason" $
        passertEvalNoTrace $
          pmatch (bound (forced $ pcon $ POutputAssetAccumulationLimit (pdata 3) (pdata 8)) (coordinate 1 3 8)) $ \PBoundV1{pbound'coordinate} ->
            pmatch (pfromData pbound'coordinate) $ \PCoordinateV1{pcoordinate'primaryIndex} -> pfromData pcoordinate'primaryIndex #== 3
    , testCase "mint coordinate binds forced reason" $
        passertEvalNoTrace $
          pmatch (bound (forced $ pcon $ PMintAssetAccumulationLimit (pdata 9)) (coordinate 2 9 0)) $ \PBoundV1{pbound'coordinate} ->
            pmatch (pfromData pbound'coordinate) $ \PCoordinateV1{pcoordinate'primaryIndex} -> pfromData pcoordinate'primaryIndex #== 9
    , testCase "accepted first crossing convicts" $ passertEvalNoTrace $ pterminalContradictionV1 # state accepted (coordinate 0 0 16_384) 3 (Just True)
    , testCase "forced exact boundary convicts" $ passertEvalNoTrace $ pterminalContradictionV1 # state (forced $ pcon $ POutputAssetAccumulationLimit (pdata 0) (pdata 16_383)) (coordinate 1 0 16_383) 3 (Just False)
    , testCase "accepted boundary refuses" $ passertEvalNoTrace $ pnot # (pterminalContradictionV1 # state accepted (coordinate 1 0 16_383) 3 (Just False))
    , testCase "forced crossing refuses" $ passertEvalNoTrace $ pnot # (pterminalContradictionV1 # state (forced $ pcon $ PMintAssetAccumulationLimit (pdata 16_384)) (coordinate 2 16_384 0) 3 (Just True))
    , testCase "reason coordinate mutation refuses" $ pfails $ bound (forced $ pcon $ PInputAssetAccumulationLimit (pdata 2) (pdata 7)) (coordinate 0 2 8)
    , testCase "reason constructor mutation refuses" $ pfails $ bound (forced $ pcon $ POutputNonCanonical (pdata 2)) (coordinate 1 2 0)
    , testCase "mint nonzero secondary coordinate refuses" $ pfails $ bound accepted (coordinate 2 0 1)
    , testCase "skips only non-target folds" $
        passertEvalNoTrace $
          pmatch (pskipFoldV1 # (pskipFoldV1 # state accepted (coordinate 2 0 0) 0 Nothing # 0) # 1) $
            \PFoldStateV1{pfoldState'stage} -> pfromData pfoldState'stage #== 2
    , testCase "target fold cannot be skipped" $ pfails $ pskipFoldV1 # state accepted (coordinate 0 0 0) 0 Nothing # 0
    , testCase "preserves an input decision through later folds" $
        passertEvalNoTrace $
          pterminalContradictionV1 # (pskipFoldV1 # (pskipFoldV1 # state accepted (coordinate 0 0 0) 1 (Just True) # 1) # 2)
    , testCase "refuses a missing decision after target fold" $ pfails $ pskipFoldV1 # state accepted (coordinate 0 0 0) 1 Nothing # 1
    ]
