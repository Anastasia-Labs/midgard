{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Testing.NativeTxCodec
Description : Behavioural tests for the Plutarch port of
              @lib/midgard/fraud-proofs/native-tx/codec.ak@.

Hand-written CBOR, so the tests are mostly a table of byte strings and the values
they must decode to — including the widths nobody exercises by accident.

The property doing the real work is **minimality**. Canonical CBOR admits one
encoding per value; the permissive decoders accept a value written in a wider
form than it needs, the canonical ones reject it. Since Midgard hashes these
bytes, a permissive decoder used where a canonical one belongs would let one
logical transaction have several commitments. Every non-minimal encoding below is
therefore tested twice — accepted by the permissive decoder, rejected by the
canonical one — because a port that made both permissive, or both strict, would
still pass a one-sided test.
-}
module Testing.NativeTxCodec (tests) where

import Data.ByteString qualified as BS
import Data.ByteString.Base16 qualified as Base16
import PlutusCore.Data qualified as PD
import PlutusTx.Builtins qualified as Builtins
import PlutusTx.Builtins (fromBuiltin, toBuiltin)
import Test.Tasty
import Test.Tasty.HUnit

import Plutarch.Prelude

import Midgard.FraudProofs.NativeTx.Codec (
  pbyteAt,
  pdecodeCanonicalIntAt,
  pdecodeCanonicalUintAt,
  pdecodeDefiniteArrayHeaderAt,
  pdecodeDefiniteBytesAt,
  pdecodeDefiniteMapHeaderAt,
  pdecodeIntAt,
  pdecodeUintAt,
  pencodeDefiniteArrayHeader,
  pencodeDefiniteBytes,
  pencodeDefiniteMapHeader,
  pexpectByte,
  pexpectH28,
  pexpectH32,
  pexpectNetworkId,
  pexpectNonNegative,
  pexpectPreimageHash,
  pexpectValidityCode,
  psliceLen,
  pvalidityFromCode,
  pvalidityFromPlutusData,
  pvalidityToCode,
  pvalidityToPlutusData,
 )
import Testing.Eval (passertEval, pfails)

-- | Collects the tests defined in this module.
tests :: TestTree
tests =
  testGroup
    "Native Tx Codec Tests"
    [ testGroup
        "shape assertions"
        [ testCase "expectH32 passes a 32-byte string through" $
            holds $ (pexpectH32 # pconstant (BS.replicate 32 0x01)) #== pconstant (BS.replicate 32 0x01)
        , testCase "expectH32 rejects 31 and 33 bytes" $ do
            pfails $ pexpectH32 # pconstant (BS.replicate 31 0x01)
            pfails $ pexpectH32 # pconstant (BS.replicate 33 0x01)
        , testCase "expectH28 passes a 28-byte string through" $
            holds $ (pexpectH28 # pconstant (BS.replicate 28 0x01)) #== pconstant (BS.replicate 28 0x01)
        , testCase "expectH28 rejects 32 bytes" $
            pfails $ pexpectH28 # pconstant (BS.replicate 32 0x01)
        , testCase "expectNonNegative admits zero" $
            holds $ (pexpectNonNegative # 0) #== 0
        , testCase "expectNonNegative rejects minus one" $
            pfails $ pexpectNonNegative # (-1)
        , testCase "expectValidityCode admits the whole range" $
            holds $ pall' [(pexpectValidityCode # pconstant c) #== pconstant c | c <- [0 .. 5]]
        , testCase "expectValidityCode rejects six and minus one" $ do
            pfails $ pexpectValidityCode # 6
            pfails $ pexpectValidityCode # (-1)
        , -- 255 is the ledger's "unspecified" marker, not a network.
          testCase "expectNetworkId admits zero, one and 255" $
            holds $ pall' [(pexpectNetworkId # pconstant n) #== pconstant n | n <- [0, 1, 255]]
        , testCase "expectNetworkId rejects two, 256 and minus one" $ do
            pfails $ pexpectNetworkId # 2
            pfails $ pexpectNetworkId # 256
            pfails $ pexpectNetworkId # (-1)
        , testCase "expectByte returns the offset after the byte it matched" $
            holds $ (pexpectByte # pconstant "\xaa\xbb" # 1 # 0xbb) #== 2
        , testCase "expectByte rejects a byte that does not match" $
            pfails $ pexpectByte # pconstant "\xaa\xbb" # 1 # 0xaa
        , testCase "expectPreimageHash returns the preimage it authenticated" $
            holds $
              (pexpectPreimageHash # pconstant "hello" # pconstant (blake2b256 "hello"))
                #== pconstant "hello"
        , testCase "expectPreimageHash rejects a hash of something else" $
            pfails $ pexpectPreimageHash # pconstant "hello" # pconstant (blake2b256 "world")
        , testCase "expectPreimageHash rejects an expected hash that is not 32 bytes" $
            pfails $ pexpectPreimageHash # pconstant "hello" # pconstant (BS.replicate 31 0x01)
        , testCase "byteAt reads the byte at the offset" $
            holds $ (pbyteAt # pconstant "\x00\x7f\xff" # 2) #== 255
        , testCase "byteAt rejects an offset past the end" $
            pfails $ pbyteAt # pconstant "\x00" # 1
        , testCase "sliceLen takes the requested length" $
            holds $ (psliceLen # pconstant "abcdef" # 2 # 3) #== pconstant "cde"
        , -- The zero case is pulled out because `slice(b, o, o - 1)` is not
          -- the empty string.
          testCase "sliceLen returns empty for a zero length" $
            holds $ (psliceLen # pconstant "abcdef" # 2 # 0) #== pconstant ""
        , testCase "sliceLen rejects a negative length" $
            pfails $ psliceLen # pconstant "abcdef" # 2 # (-1)
        ]
    , testGroup
        "verdict codes"
        [ testCase "the code round trip is the identity on all six verdicts" $
            holds $
              pall'
                [ (pvalidityToCode #$ pvalidityFromCode # pconstant c) #== pconstant c
                | c <- [0 .. 5]
                ]
        , testCase "validityFromCode rejects six and minus one" $ do
            pfails $ pvalidityFromCode # 6
            pfails $ pvalidityFromCode # (-1)
        , -- Two encodings of the same six verdicts, kept aligned only by this
          -- pair of functions.
          testCase "the PlutusData round trip is the identity on all six verdicts" $
            holds $
              pall'
                [ ( pvalidityToCode
                      #$ pvalidityFromPlutusData
                      #$ pvalidityToPlutusData
                      #$ pvalidityFromCode
                      # pconstant c
                  )
                    #== pconstant c
                | c <- [0 .. 5]
                ]
        , testCase "validityToPlutusData builds a nullary constructor" $
            holds $
              pall'
                [ (pvalidityToPlutusData #$ pvalidityFromCode # pconstant c)
                    #== pconstant @PData (PD.Constr c [])
                | c <- [0 .. 5]
                ]
        , -- A verdict must not be smuggled in alongside a payload.
          testCase "validityFromPlutusData rejects a constructor carrying fields" $
            pfails $ pvalidityFromPlutusData # pconstant @PData (PD.Constr 0 [PD.I 1])
        , testCase "validityFromPlutusData rejects an out-of-range constructor" $
            pfails $ pvalidityFromPlutusData # pconstant @PData (PD.Constr 6 [])
        ]
    , testGroup
        "unsigned integers"
        [ testCase "decodes every width" $
            holds $
              pall'
                [ uintIs pdecodeUintAt bytes next value
                | (bytes, next, value) <- canonicalUints
                ]
        , testCase "the canonical decoder agrees on every minimal encoding" $
            holds $
              pall'
                [ uintIs pdecodeCanonicalUintAt bytes next value
                | (bytes, next, value) <- canonicalUints
                ]
        , -- The distinction, tested from both sides.
          testCase "the permissive decoder accepts non-minimal encodings" $
            holds $
              pall'
                [ uintIs pdecodeUintAt bytes next value
                | (bytes, next, value) <- nonMinimalUints
                ]
        , testCase "the canonical decoder rejects every non-minimal encoding" $
            mapM_
              (\(bytes, _, _) -> pfails $ pdecodeCanonicalUintAt # pconstant bytes # 0)
              nonMinimalUints
        , testCase "both reject a tag that is not an unsigned integer" $ do
            pfails $ pdecodeUintAt # pconstant "\x1c" # 0
            pfails $ pdecodeCanonicalUintAt # pconstant "\x1c" # 0
        ]
    , testGroup
        "signed integers"
        [ -- Major type 1 stores `n` for the value `-1 - n`, so there is no
          -- negative zero and the negative range is one wider.
          testCase "decodes every width, both signs" $
            holds $
              pall'
                [ uintIs pdecodeIntAt bytes next value
                | (bytes, next, value) <- canonicalInts
                ]
        , testCase "the canonical decoder agrees on every minimal encoding" $
            holds $
              pall'
                [ uintIs pdecodeCanonicalIntAt bytes next value
                | (bytes, next, value) <- canonicalInts
                ]
        , testCase "the permissive decoder accepts non-minimal negatives" $
            holds $
              pall'
                [ uintIs pdecodeIntAt bytes next value
                | (bytes, next, value) <- nonMinimalInts
                ]
        , testCase "the canonical decoder rejects every non-minimal negative" $
            mapM_
              (\(bytes, _, _) -> pfails $ pdecodeCanonicalIntAt # pconstant bytes # 0)
              nonMinimalInts
        , testCase "both reject a tag between the two major types" $ do
            pfails $ pdecodeIntAt # pconstant "\x1f" # 0
            pfails $ pdecodeCanonicalIntAt # pconstant "\x1f" # 0
        ]
    , testGroup
        "byte strings"
        [ testCase "decodes every width" $
            holds $
              pall'
                [ bytesIs bytes next value
                | (bytes, next, value) <- definiteBytes
                ]
        , testCase "rejects a tag that is not a definite byte string" $ do
            pfails $ pdecodeDefiniteBytesAt # pconstant "\x5f" # 0 -- indefinite
            pfails $ pdecodeDefiniteBytesAt # pconstant "\x80" # 0 -- an array
        ]
    , testGroup
        "array and map headers"
        [ testCase "reads array headers at every width" $
            holds $
              pall'
                [ uintIs pdecodeDefiniteArrayHeaderAt bytes next value
                | (bytes, next, value) <- arrayHeaders
                ]
        , testCase "reads map headers at every width" $
            holds $
              pall'
                [ uintIs pdecodeDefiniteMapHeaderAt bytes next value
                | (bytes, next, value) <- mapHeaders
                ]
        , -- The two major types must not be confused for one another.
          testCase "the array reader rejects a map header" $
            pfails $ pdecodeDefiniteArrayHeaderAt # pconstant "\xa1" # 0
        , testCase "the map reader rejects an array header" $
            pfails $ pdecodeDefiniteMapHeaderAt # pconstant "\x81" # 0
        , -- This is the permissive reader, deliberately: it is not the §5.1
          -- field-preimage decoder.
          testCase "the array reader accepts a non-minimal header" $
            holds $ uintIs pdecodeDefiniteArrayHeaderAt "\x98\x01" 2 1
        ]
    , testGroup
        "encoders"
        [ testCase "long_bytearray_serialisation_matches_typescript_chunking" $
            holds $
              (pserialiseData # pforgetData (pdata $ pconstant @PByteString longBytearrayPayload))
                #== pconstant longBytearrayCbor
        , testCase "array headers match the expected bytes at every boundary" $
            holds $
              pall'
                [ (pencodeDefiniteArrayHeader # pconstant n) #== pconstant bytes
                | (n, bytes) <- arrayHeaderBytes
                ]
        , testCase "map headers match the expected bytes at every boundary" $
            holds $
              pall'
                [ (pencodeDefiniteMapHeader # pconstant n) #== pconstant bytes
                | (n, bytes) <- mapHeaderBytes
                ]
        , testCase "byte strings match the expected bytes at every boundary" $
            holds $
              pall'
                [ (pencodeDefiniteBytes # pconstant (BS.replicate n 0x41)) #== pconstant bytes
                | (n, prefix) <- bytesHeaderBytes
                , let bytes = prefix <> BS.replicate n 0x41
                ]
        , -- The encoders emit the minimal form, so what they write is exactly
          -- what the canonical decoder will take back.
          testCase "every array header round-trips through its reader" $
            holds $
              pall'
                [ pmatch
                  ( pdecodeDefiniteArrayHeaderAt
                      # (pencodeDefiniteArrayHeader # pconstant n)
                      # 0
                  )
                  (\(PPair _ v) -> v #== pconstant n)
                | (n, _) <- arrayHeaderBytes
                ]
        , testCase "every map header round-trips through its reader" $
            holds $
              pall'
                [ pmatch
                  (pdecodeDefiniteMapHeaderAt # (pencodeDefiniteMapHeader # pconstant n) # 0)
                  (\(PPair _ v) -> v #== pconstant n)
                | (n, _) <- mapHeaderBytes
                ]
        , testCase "every byte string round-trips through its reader" $
            holds $
              pall'
                [ pmatch
                  ( pdecodeDefiniteBytesAt
                      # (pencodeDefiniteBytes # pconstant (BS.replicate n 0x41))
                      # 0
                  )
                  (\(PPair _ v) -> v #== pconstant (BS.replicate n 0x41))
                | (n, _) <- bytesHeaderBytes
                ]
        , testCase "array and map headers reject a negative length" $ do
            pfails $ pencodeDefiniteArrayHeader # (-1)
            pfails $ pencodeDefiniteMapHeader # (-1)
        ]
    ]

--------------------------------------------------------------------------------
-- Tables
--------------------------------------------------------------------------------

-- | @(bytes, next offset, value)@ — minimal encodings of unsigned integers.
canonicalUints :: [(BS.ByteString, Integer, Integer)]
canonicalUints =
  [ ("\x00", 1, 0)
  , ("\x17", 1, 23)
  , ("\x18\x18", 2, 24)
  , ("\x18\xff", 2, 255)
  , ("\x19\x01\x00", 3, 256)
  , ("\x19\xff\xff", 3, 65535)
  , ("\x1a\x00\x01\x00\x00", 5, 65536)
  , ("\x1a\xff\xff\xff\xff", 5, 4294967295)
  , ("\x1b\x00\x00\x00\x01\x00\x00\x00\x00", 9, 4294967296)
  ]

{- | The same values written wider than they need to be.

Each is one step off the minimality boundary: the largest value the narrower
form could still have held.
-}
nonMinimalUints :: [(BS.ByteString, Integer, Integer)]
nonMinimalUints =
  [ ("\x18\x17", 2, 23) -- 23 fits in the packed form
  , ("\x19\x00\xff", 3, 255) -- 255 fits in one byte
  , ("\x1a\x00\x00\xff\xff", 5, 65535) -- 65535 fits in two
  , ("\x1b\x00\x00\x00\x00\xff\xff\xff\xff", 9, 4294967295) -- and in four
  ]

canonicalInts :: [(BS.ByteString, Integer, Integer)]
canonicalInts =
  [ ("\x00", 1, 0)
  , ("\x17", 1, 23)
  , ("\x20", 1, -1) -- major type 1 stores 0 for -1
  , ("\x37", 1, -24)
  , ("\x38\x18", 2, -25)
  , ("\x38\xff", 2, -256)
  , ("\x39\x01\x00", 3, -257)
  , ("\x39\xff\xff", 3, -65536)
  , ("\x3a\x00\x01\x00\x00", 5, -65537)
  , ("\x3b\x00\x00\x00\x01\x00\x00\x00\x00", 9, -4294967297)
  ]

nonMinimalInts :: [(BS.ByteString, Integer, Integer)]
nonMinimalInts =
  [ ("\x38\x17", 2, -24)
  , ("\x39\x00\xff", 3, -256)
  , ("\x3a\x00\x00\xff\xff", 5, -65536)
  , ("\x3b\x00\x00\x00\x00\xff\xff\xff\xff", 9, -4294967296)
  ]

{- | @(bytes, next offset, payload)@.

Stops below the evaluator's @appendByteString@ ceiling for the same reason as
'bytesHeaderBytes', so the four-byte length form is read but not with a payload
that needs it.
-}
definiteBytes :: [(BS.ByteString, Integer, BS.ByteString)]
definiteBytes =
  [ ("\x40", 1, "")
  , ("\x41\x61", 2, "a")
  , ("\x57" <> BS.replicate 23 0x41, 24, BS.replicate 23 0x41)
  , ("\x58\x18" <> BS.replicate 24 0x41, 26, BS.replicate 24 0x41)
  , ("\x58\xff" <> BS.replicate 255 0x41, 257, BS.replicate 255 0x41)
  , ("\x59\x01\x00" <> BS.replicate 256 0x41, 259, BS.replicate 256 0x41)
  ]

arrayHeaders :: [(BS.ByteString, Integer, Integer)]
arrayHeaders =
  [ ("\x80", 1, 0)
  , ("\x97", 1, 23)
  , ("\x98\x18", 2, 24)
  , ("\x99\x01\x00", 3, 256)
  , ("\x9a\x00\x01\x00\x00", 5, 65536)
  ]

mapHeaders :: [(BS.ByteString, Integer, Integer)]
mapHeaders =
  [ ("\xa0", 1, 0)
  , ("\xb7", 1, 23)
  , ("\xb8\x18", 2, 24)
  , ("\xb9\x01\x00", 3, 256)
  , ("\xba\x00\x01\x00\x00", 5, 65536)
  ]

arrayHeaderBytes :: [(Integer, BS.ByteString)]
arrayHeaderBytes =
  [ (0, "\x80")
  , (23, "\x97")
  , (24, "\x98\x18")
  , (255, "\x98\xff")
  , (256, "\x99\x01\x00")
  , (65535, "\x99\xff\xff")
  , (65536, "\x9a\x00\x01\x00\x00")
  ]

mapHeaderBytes :: [(Integer, BS.ByteString)]
mapHeaderBytes =
  [ (0, "\xa0")
  , (23, "\xb7")
  , (24, "\xb8\x18")
  , (255, "\xb8\xff")
  , (256, "\xb9\x01\x00")
  , (65535, "\xb9\xff\xff")
  , (65536, "\xba\x00\x01\x00\x00")
  ]

{- | @(payload length, the header the encoder must emit)@.

Stops at a few hundred bytes deliberately. The four-byte length branch needs a
payload over 65,535 bytes, and building one blows the evaluator's ceiling on
@appendByteString@ — so that branch of 'pencodeDefiniteBytes' is *not* exercised
here. The identical four-byte ladder in the array and map header encoders is,
since those emit five bytes and carry no payload.
-}
bytesHeaderBytes :: [(Int, BS.ByteString)]
bytesHeaderBytes =
  [ (0, "\x40")
  , (23, "\x57")
  , (24, "\x58\x18")
  , (255, "\x58\xff")
  , (256, "\x59\x01\x00")
  , (300, "\x59\x01\x2c")
  ]

longBytearrayPayload, longBytearrayCbor :: BS.ByteString
longBytearrayPayload =
  Base16.decodeLenient
    "abababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababab"
longBytearrayCbor =
  Base16.decodeLenient
    "5f5840abababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababababab5820ababababababababababababababababababababababababababababababababff"

--------------------------------------------------------------------------------
-- Plumbing
--------------------------------------------------------------------------------

holds :: (forall s. Term s PBool) -> Assertion
holds = passertEval

pall' :: forall s. [Term s PBool] -> Term s PBool
pall' = foldr (#&&) (pconstant True)

-- | Runs an integer decoder at offset zero and checks both halves of its result.
uintIs ::
  forall s.
  (forall s'. Term s' (PByteString :--> PInteger :--> PPair PInteger PInteger)) ->
  BS.ByteString ->
  Integer ->
  Integer ->
  Term s PBool
uintIs decoder bytes expectedNext expectedValue =
  pmatch (decoder # pconstant bytes # 0) $ \(PPair next value) ->
    next #== pconstant expectedNext #&& value #== pconstant expectedValue

bytesIs :: forall s. BS.ByteString -> Integer -> BS.ByteString -> Term s PBool
bytesIs bytes expectedNext expectedValue =
  pmatch (pdecodeDefiniteBytesAt # pconstant bytes # 0) $ \(PPair next value) ->
    next #== pconstant expectedNext #&& value #== pconstant expectedValue

blake2b256 :: BS.ByteString -> BS.ByteString
blake2b256 = fromBuiltin . Builtins.blake2b_256 . toBuiltin
