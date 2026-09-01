{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Testing.CanonicalCborScan
Description : Behavioural tests for the Plutarch port of
              @lib/midgard/canonical-cbor-scan-v1.ak@.

Two readers, and one property doing most of the work: __minimality__. Canonical
CBOR admits one spelling per value, and Midgard hashes these bytes, so a length
written wider than it needs would give one logical value several commitments.
Every width below is therefore tested from both sides — the smallest value that
belongs in it is accepted, and the largest value that belonged in the narrower
one is rejected when written here.

The reference is a minimal-head encoder written from RFC 8949 §3, not from the
port. The positive direction runs it over each width's boundaries and requires
the reader to recover what it wrote; the negative direction is a table of
hand-written heads, because a minimal encoder cannot produce a non-minimal one.
-}
module Testing.CanonicalCborScan (tests) where

import Data.ByteString qualified as BS
import Test.Tasty
import Test.Tasty.HUnit

import Plutarch.Prelude

import Midgard.CanonicalCborScan (
  PCborBytesV1 (..),
  PCborHeadV1 (..),
  pbytesAtV1,
  pheadAtV1,
 )
import Testing.Eval (passertEval)

--------------------------------------------------------------------------------
-- The suite
--------------------------------------------------------------------------------

tests :: TestTree
tests =
  testGroup
    "Canonical CBOR Scan Tests"
    [ testGroup "head_at_v1 against a minimal encoder" encoderTests
    , testGroup "head_at_v1 fails closed" headRejectionTests
    , testGroup "bytes_at_v1" bytesTests
    ]

--------------------------------------------------------------------------------
-- The reference encoder
--------------------------------------------------------------------------------

{- | RFC 8949 §3's head, in the shortest form that fits.

Written from the RFC rather than taken from the port, and deliberately not
sharing the port's slice-and-convert: this builds the bytes big-endian by
division, so the two agree only if both are right about byte order.
-}
minimalHead :: Integer -> Integer -> BS.ByteString
minimalHead major value
  | value < 24 = BS.pack [fromIntegral (major * 32 + value)]
  | value < 0x100 = BS.pack [fromIntegral (major * 32 + 24)] <> bigEndian 1 value
  | value < 0x10000 = BS.pack [fromIntegral (major * 32 + 25)] <> bigEndian 2 value
  | value < 0x100000000 = BS.pack [fromIntegral (major * 32 + 26)] <> bigEndian 4 value
  | otherwise = BS.pack [fromIntegral (major * 32 + 27)] <> bigEndian 8 value

bigEndian :: Int -> Integer -> BS.ByteString
bigEndian width value =
  BS.pack [fromIntegral (value `div` (256 ^ i) `mod` 256) | i <- reverse [0 .. width - 1]]

{- | Every width's boundaries, on both sides.

The value one below each threshold belongs in the narrower form; the value at it
is the first that needs this one. A reader that got a threshold wrong by one
fails here rather than on some value nobody would have thought to try.
-}
boundaryValues :: [Integer]
boundaryValues =
  [ 0
  , 1
  , 23
  , 24
  , 255
  , 256
  , 65535
  , 65536
  , 4294967295
  , 4294967296
  , 0xfedcba9876543210
  ]

encoderTests :: [TestTree]
encoderTests =
  [ testCase ("reads back " <> show value <> " under major " <> show major) $
    passertEval $
      headIs (minimalHead major value) 0 major (encodedLength value, value)
  | major <- [0, 2, 4, 5]
  , value <- boundaryValues
  ]
    <> [ testCase "reads a head that does not start at zero" $
          passertEval $
            headIs ("\xff\xff" <> minimalHead 4 300) 2 4 (2 + encodedLength 300, 300)
       , testCase "a head is refused under any major but its own" $
          passertEval $
            pall'
              [ headFails (minimalHead 2 value) 0 other
              | value <- [0, 24, 300]
              , other <- [0, 1, 3, 4, 5, 6, 7]
              ]
       ]

-- | How many bytes 'minimalHead' spends on a value, argument included.
encodedLength :: Integer -> Integer
encodedLength = fromIntegral . BS.length . minimalHead 0

--------------------------------------------------------------------------------
-- Failing closed
--------------------------------------------------------------------------------

{- | The non-minimal and malformed heads, spelled by hand.

A minimal encoder cannot produce these, which is why they are a table rather than
generated: the wider form of a value that fits a narrower one, the indefinite
marker, the three reserved additional-info values, and the truncations.
-}
headRejectionTests :: [TestTree]
headRejectionTests =
  [ testCase (why <> " is refused") (passertEval (headFails bytes 0 major))
  | (why, bytes, major) <-
      [ ("23 written in the one-byte form", "\x18\x17", 0)
      , ("255 written in the two-byte form", "\x19\x00\xff", 0)
      , ("65535 written in the four-byte form", "\x1a\x00\x00\xff\xff", 0)
      , ("4294967295 written in the eight-byte form", "\x1b\x00\x00\x00\x00\xff\xff\xff\xff", 0)
      , ("zero written in the one-byte form", "\x18\x00", 0)
      , ("the indefinite marker", "\x5f", 2)
      , ("reserved additional info 28", "\x1c", 0)
      , ("reserved additional info 29", "\x1d", 0)
      , ("reserved additional info 30", "\x1e", 0)
      , ("a one-byte argument with no byte", "\x18", 0)
      , ("a two-byte argument with one byte", "\x19\x01", 0)
      , ("a four-byte argument with three bytes", "\x1a\x00\x01\x00", 0)
      , ("an eight-byte argument with seven bytes", "\x1b\x00\x00\x00\x01\x00\x00\x00", 0)
      , ("empty input", "", 0)
      ]
  ]
    <> [ testCase "an offset past the end is refused" $
          passertEval $ headFails "\x00" 1 0
       , testCase "…and a negative one" $
          passertEval $ headFails "\x00" (-1) 0
       ]

--------------------------------------------------------------------------------
-- bytes_at_v1
--------------------------------------------------------------------------------

bytesTests :: [TestTree]
bytesTests =
  [ testCase "reads an empty byte string" $
      passertEval $ bytesAre "\x40" 0 (1, "")
  , testCase "reads a short one" $
      passertEval $ bytesAre "\x43\x01\x02\x03" 0 (4, "\x01\x02\x03")
  , testCase "reads one whose length needs a one-byte argument" $
      passertEval $ bytesAre (minimalHead 2 24 <> BS.replicate 24 0x07) 0 (26, BS.replicate 24 0x07)
  , testCase "stops at the payload's end, not the input's" $
      passertEval $ bytesAre "\x41\xaa\xbb\xcc" 0 (2, "\xaa")
  , testCase "reads one that does not start at zero" $
      passertEval $ bytesAre "\xff\x42\xaa\xbb" 1 (4, "\xaa\xbb")
  , testCase "refuses a payload that runs past the end" $
      passertEval $ bytesFail "\x43\x01\x02" 0
  , testCase "refuses a payload one byte short" $
      passertEval $ bytesFail (minimalHead 2 64 <> BS.replicate 63 0x01) 0
  , testCase "refuses a non-minimal length" $
      passertEval $ bytesFail "\x58\x01\xaa" 0
  , testCase "refuses the indefinite form" $
      passertEval $ bytesFail "\x5f\x41\xaa\xff" 0
  , -- Every other major is somebody else's, and this reader must not take them.
    testCase "refuses a head that is not a byte string" $
      passertEval $
        pall' [bytesFail (minimalHead major 2 <> "\xaa\xbb") 0 | major <- [0, 1, 3, 4, 5, 6, 7]]
  , testCase "refuses an offset past the end" $
      passertEval $ bytesFail "\x40" 1
  ]

--------------------------------------------------------------------------------
-- Plumbing
--------------------------------------------------------------------------------

headIs ::
  forall (s :: S).
  BS.ByteString ->
  Integer ->
  Integer ->
  (Integer, Integer) ->
  Term s PBool
headIs bytes offset major (expectedNext, expectedValue) =
  pmatch (pheadAtV1 # pconstant bytes # pconstant offset # pconstant major) $ \case
    PNothing -> pconstant False
    PJust h -> pmatch h $ \(PCborHeadV1 next value) ->
      next #== pconstant expectedNext #&& value #== pconstant expectedValue

headFails :: forall (s :: S). BS.ByteString -> Integer -> Integer -> Term s PBool
headFails bytes offset major =
  pmatch (pheadAtV1 # pconstant bytes # pconstant offset # pconstant major) $ \case
    PNothing -> pconstant True
    PJust _ -> pconstant False

bytesAre ::
  forall (s :: S).
  BS.ByteString ->
  Integer ->
  (Integer, BS.ByteString) ->
  Term s PBool
bytesAre bytes offset (expectedNext, expectedValue) =
  pmatch (pbytesAtV1 # pconstant bytes # pconstant offset) $ \case
    PNothing -> pconstant False
    PJust b -> pmatch b $ \(PCborBytesV1 next value) ->
      next #== pconstant expectedNext #&& value #== pconstant expectedValue

bytesFail :: forall (s :: S). BS.ByteString -> Integer -> Term s PBool
bytesFail bytes offset =
  pmatch (pbytesAtV1 # pconstant bytes # pconstant offset) $ \case
    PNothing -> pconstant True
    PJust _ -> pconstant False

pall' :: forall (s :: S). [Term s PBool] -> Term s PBool
pall' = foldr (#&&) (pconstant True)
