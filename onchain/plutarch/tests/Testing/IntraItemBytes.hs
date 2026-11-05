{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Testing.IntraItemBytes
Description : Behavioural tests for the Plutarch port of
              @lib/midgard/intra-item-bytes-v1.ak@.

The same head grammar as "Testing.CanonicalCborScan", read by a caller who has
already decided the bytes are canonical — so every violation is an /abort/ rather
than a @None@. Both modules are tested against the same minimal-head encoder,
written from RFC 8949 §3, because the whole point of having two of them is that
they agree about the grammar and disagree only about the failure mode.

__The clamp hazard is the thing worth testing.__ @sliceByteString@ clamps: asked
for more bytes than exist, it silently returns fewer. Two clamped reads of
different ranges can be byte-equal, so evidence built that way is
indistinguishable from the real thing. Every out-of-range case below therefore
asserts an abort, and one pins the specific pair of ranges that would collide
under clamping.
-}
module Testing.IntraItemBytes (tests) where

import Data.ByteString qualified as BS
import Test.Tasty
import Test.Tasty.HUnit

import Plutarch.Prelude

import Midgard.IntraItemBytes (pbyteIn, pheadAt, psliceExact)
import Testing.Eval (passertEval, pfails)

tests :: TestTree
tests =
  testGroup
    "Intra-Item Bytes Tests"
    [ testGroup "headAt" headTests
    , testGroup "byteIn" byteTests
    , testGroup "sliceExact" sliceTests
    ]

--------------------------------------------------------------------------------
-- headAt
--------------------------------------------------------------------------------

headTests :: [TestTree]
headTests =
  [ testCase ("reads " <> show value <> " under major " <> show major) $
    holds $ headIs (minimalHead major value) 0 major (encodedLength value, value)
  | major <- [0, 2, 4, 5]
  , value <- boundaryValues
  ]
    <> [ testCase "reads a head that does not start at zero" $
          holds $ headIs ("\xff\xff" <> minimalHead 4 300) 2 4 (2 + encodedLength 300, 300)
       , -- The major type is pinned by the caller, so a map head cannot be read
         -- where a byte string belongs.
         testCase "a head under the wrong major aborts" $
          pfails $ headAt (minimalHead 2 24) 0 5
       , testCase "…and under every other major" $ do
          sequence_ [pfails (headAt (minimalHead 2 300) 0 m) | m <- [0, 1, 3, 4, 5, 6, 7]]
       ]
    <> [ testCase (why <> " aborts") (pfails (headAt bytes 0 0))
       | (why, bytes) <-
           [ ("23 written in the one-byte form", "\x18\x17")
           , ("255 written in the two-byte form", "\x19\x00\xff")
           , ("65535 written in the four-byte form", "\x1a\x00\x00\xff\xff")
           , ("4294967295 in the eight-byte form", "\x1b\x00\x00\x00\x00\xff\xff\xff\xff")
           , ("the indefinite marker", "\x1f")
           , ("reserved additional info 28", "\x1c")
           , ("reserved additional info 29", "\x1d")
           , ("reserved additional info 30", "\x1e")
           , ("a one-byte argument with no byte", "\x18")
           , ("a two-byte argument with one byte", "\x19\x01")
           , ("a four-byte argument with three bytes", "\x1a\x00\x01\x00")
           , ("an eight-byte argument with seven bytes", "\x1b\x00\x00\x00\x01\x00\x00\x00")
           , ("empty input", "")
           ]
       ]
    <> [ testCase "an offset past the end aborts" $ pfails $ headAt "\x00" 1 0
       , testCase "…and a negative one" $ pfails $ headAt "\x00" (-1) 0
       ]

--------------------------------------------------------------------------------
-- byteIn
--------------------------------------------------------------------------------

byteTests :: [TestTree]
byteTests =
  [ testCase "reads every byte of a string" $
      holds $
        pall'
          [ (pbyteIn # pconstant sample # pconstant (fromIntegral i))
            #== pconstant (fromIntegral (BS.index sample i))
          | i <- [0 .. BS.length sample - 1]
          ]
  , testCase "one past the end aborts" $
      pfails $ pbyteIn # pconstant sample # pconstant (fromIntegral (BS.length sample))
  , testCase "a negative index aborts" $ pfails $ pbyteIn # pconstant sample # (-1)
  , testCase "any index into the empty string aborts" $ pfails $ pbyteIn # pconstant "" # 0
  ]
  where
    sample = "\x00\x01\x7f\x80\xff" :: BS.ByteString

--------------------------------------------------------------------------------
-- sliceExact
--------------------------------------------------------------------------------

sliceTests :: [TestTree]
sliceTests =
  [ testCase "takes a slice from the middle" $
      holds $ (psliceExact # pconstant sample # 1 # 3) #== pconstant "\x01\x02\x03"
  , testCase "takes the whole string" $
      holds $ (psliceExact # pconstant sample # 0 # 5) #== pconstant sample
  , testCase "a zero length is the empty string, not an error" $
      holds $ (psliceExact # pconstant sample # 2 # 0) #== pconstant ""
  , testCase "…including at the very end" $
      holds $ (psliceExact # pconstant sample # 5 # 0) #== pconstant ""
  , {- The clamp hazard, stated as the collision it would cause. Under
       `sliceByteString`'s clamping both of these return the last two bytes, so a
       reader that clamped could be handed either range and could not tell them
       apart. Both must abort. -}
    testCase "a slice running one byte past the end aborts" $
      pfails $ psliceExact # pconstant sample # 4 # 2
  , testCase "…and one running far past it" $
      pfails $ psliceExact # pconstant sample # 3 # 99
  , testCase "a negative offset aborts" $ pfails $ psliceExact # pconstant sample # (-1) # 1
  , testCase "a negative length aborts" $ pfails $ psliceExact # pconstant sample # 0 # (-1)
  ]
  where
    sample = "\x00\x01\x02\x03\x04" :: BS.ByteString

--------------------------------------------------------------------------------
-- The reference encoder, written from RFC 8949 §3
--------------------------------------------------------------------------------

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

encodedLength :: Integer -> Integer
encodedLength = fromIntegral . BS.length . minimalHead 0

boundaryValues :: [Integer]
boundaryValues =
  [0, 1, 23, 24, 255, 256, 65535, 65536, 4294967295, 4294967296, 0xfedcba9876543210]

--------------------------------------------------------------------------------
-- Plumbing
--------------------------------------------------------------------------------

headAt ::
  forall (s :: S). BS.ByteString -> Integer -> Integer -> Term s (PPair PInteger PInteger)
headAt bytes offset major =
  pheadAt # pconstant bytes # pconstant offset # pconstant major

headIs ::
  forall (s :: S). BS.ByteString -> Integer -> Integer -> (Integer, Integer) -> Term s PBool
headIs bytes offset major (expectedNext, expectedValue) =
  pmatch (headAt bytes offset major) $ \(PPair next value) ->
    next #== pconstant expectedNext #&& value #== pconstant expectedValue

holds :: (forall (s :: S). Term s PBool) -> Assertion
holds = passertEval

pall' :: forall (s :: S). [Term s PBool] -> Term s PBool
pall' = foldr (#&&) (pconstant True)
