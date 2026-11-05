{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Testing.AikenCbor
Description : Behavioural tests for the Plutarch port of
              @aiken/cbor.deserialise@.

Two oracles, and neither is a reimplementation of the decoder.

__The builtin.__ @serialiseData@ is a Plutus builtin with no inverse, so it is a
reference the decoder cannot have been fitted to: for any @Data@, the bytes it
emits must decode back to that @Data@ and to nothing else. That single property
covers most of the grammar for free, and it covers the parts of it that are
easiest to get wrong — @serialiseData@ emits __indefinite-length__ lists,
__chunked__ byte strings above 64 bytes, and the 121/1280 constructor tags, so a
round-trip over a wide enough spread of values drives every one of those paths
without a single hand-written byte.

__RFC 8949.__ The forms @serialiseData@ never emits — definite-length arrays,
definite byte strings, indefinite maps, and the four multi-byte length headers —
are written out by hand from the specification, because a decoder that only ever
saw its own encoder's output would accept a strictly smaller language than the
one it claims to.

=== Round-tripping is not an identity, and the gaps are the original's

The stdlib documents @deserialise(serialise(x)) == Some(x)@. Three families of
value break it, all of them the original's behaviour and all reproduced here:

  * an __empty byte string that ends the input__, because @take@ refuses at an
    exhausted cursor before it looks at the width;
  * a __constructor index above 127__, which Plutus encodes as tag 102 and the
    decoder refuses by name;
  * an __integer past 64 bits__, which Plutus encodes as a tagged bignum and the
    decoder reads as a constructor with a negative index — which does not
    decline, it __aborts the machine__.

The last one generalises past bignums: every tag below 121 except 102 produces a
negative index, so four bytes an adversary writes by hand abort a script that
calls @deserialise@ on them. The tests below pin all four facts, and separate
"declines" from "aborts" throughout, because a caller matching on @None@ never
sees the second.

Each of the four was confirmed against the Aiken toolchain directly, by running
the same claims as @aiken check@ probes over the stdlib: the two that decline
pass there, and the two that abort make @aiken check@ /panic/ —
@TryFromBigIntError@ at @crates/uplc/src/machine/runtime.rs:879@ — because its
evaluator does not handle a negative @constrData@ index the way the ledger does.
The port produces the ledger's behaviour, which is a clean script failure.
-}
module Testing.AikenCbor (tests) where

import Data.ByteString qualified as BS
import PlutusCore.Data qualified as PD
import PlutusTx.Builtins (dataToBuiltinData, fromBuiltin, serialiseData)
import Test.Tasty
import Test.Tasty.HUnit

import Plutarch.Prelude

import Aiken.Cbor (pdeserialise)
import Testing.Eval (passertEval, pfails)

tests :: TestTree
tests =
  testGroup
    "Aiken CBOR Tests"
    [ testGroup "round-tripping the builtin's own output" roundTripTests
    , testGroup "forms the builtin never emits" handWrittenTests
    , testGroup "refusals" refusalTests
    , testGroup "where round-tripping stops" gapTests
    ]

--------------------------------------------------------------------------------
-- Round-tripping serialiseData
--------------------------------------------------------------------------------

{- | For every value here, @deserialise (serialise v) == Some v@.

The spread is chosen for the /encodings/ it forces rather than for the values:
each group names the CBOR form it exercises.
-}
roundTripTests :: [TestTree]
roundTripTests =
  [ testGroup "integers across every header width" $
      roundTrips
        [ PD.I 0
        , PD.I 1
        , PD.I 23
        , PD.I 24
        , PD.I 255
        , PD.I 256
        , PD.I 65535
        , PD.I 65536
        , PD.I 4294967295
        , PD.I 4294967296
        , PD.I 18446744073709551615
        ]
  , testGroup "negative integers across every header width" $
      roundTrips
        [ PD.I (-1)
        , PD.I (-24)
        , PD.I (-25)
        , PD.I (-256)
        , PD.I (-257)
        , PD.I (-65537)
        , PD.I (-4294967297)
        , PD.I (-18446744073709551616)
        ]
  , -- Above 64 bytes the builtin switches to the indefinite chunked form, so
    -- these are the cases that drive `decode_chunks` rather than `decode_bytes`.
    testGroup "byte strings on both sides of the chunking threshold" $
      roundTrips
        [ PD.B "\x01"
        , PD.B (BS.replicate 23 0xaa)
        , PD.B (BS.replicate 24 0xbb)
        , PD.B (BS.replicate 64 0xcc)
        , PD.B (BS.replicate 65 0xdd)
        , PD.B (BS.replicate 200 0xee)
        , PD.B (filler 1000)
        ]
  , -- The builtin emits `80` for the empty list and the indefinite `9f … ff`
    -- for every non-empty one.
    testGroup "lists, empty and nested" $
      roundTrips
        [ PD.List []
        , PD.List [PD.I 1]
        , PD.List [PD.I 1, PD.B "\x02", PD.List []]
        , PD.List [PD.List [PD.List [PD.I 7]]]
        , PD.List (map PD.I [0 .. 40])
        ]
  , testGroup "maps, empty and nested" $
      roundTrips
        [ PD.Map []
        , PD.Map [(PD.I 1, PD.B "\x01")]
        , PD.Map [(PD.B "k", PD.Map [(PD.I 0, PD.List [PD.I 9])])]
        , PD.Map [(PD.I k, PD.I (k * 2)) | k <- [0 .. 30]]
        ]
  , -- Constructors 0–6 are tags 121–127; 7 upwards are 1280 upwards. Both sides
    -- of that boundary are driven, and so is an index far past it.
    testGroup "constructors across the 121/1280 tag boundary" $
      roundTrips
        [ PD.Constr 0 []
        , PD.Constr 6 [PD.I 1]
        , PD.Constr 7 [PD.I 1, PD.I 2]
        , PD.Constr 8 []
        , PD.Constr 127 [PD.B "\xff"]
        ]
  , testGroup "a deeply mixed structure" $
      roundTrips [mixed]
  ]

-- | A value with every constructor of @Data@ in it, nested four deep.
mixed :: PD.Data
mixed =
  PD.Constr
    3
    [ PD.Map
        [ (PD.I 0, PD.List [PD.B (BS.replicate 70 0x11), PD.I (-1)])
        , (PD.B "key", PD.Constr 9 [PD.Map [], PD.List []])
        ]
    , PD.I 65536
    , PD.B (filler 300)
    ]

--------------------------------------------------------------------------------
-- Hand-written CBOR
--------------------------------------------------------------------------------

{- | The forms @serialiseData@ does not produce, written from RFC 8949 §3.

Each is a byte string this decoder must accept and a @Data@ it must produce.
Note what that means for canonicity: several of these decode to the same value as
a shorter encoding does, which is exactly why every consumer in this tree
re-encodes and compares rather than trusting a successful decode.
-}
handWrittenTests :: [TestTree]
handWrittenTests =
  [ testCase "a definite-length array" $
      decodes "\x83\x01\x02\x03" (PD.List [PD.I 1, PD.I 2, PD.I 3])
  , testCase "an empty definite array" $
      decodes "\x80" (PD.List [])
  , testCase "a definite byte string" $
      decodes "\x43\x01\x02\x03" (PD.B "\x01\x02\x03")
  , testCase "an indefinite map" $
      decodes "\xbf\x01\x02\xff" (PD.Map [(PD.I 1, PD.I 2)])
  , testCase "an empty indefinite map" $
      decodes "\xbf\xff" (PD.Map [])
  , testCase "an empty indefinite list" $
      decodes "\x9f\xff" (PD.List [])
  , testCase "an empty indefinite byte string" $
      decodes "\x5f\xff" (PD.B "")
  , testCase "a chunked byte string with two chunks" $
      decodes "\x5f\x42\x01\x02\x41\x03\xff" (PD.B "\x01\x02\x03")
  , -- The four multi-byte length headers, each carrying a value that would fit
    -- in a shorter one — non-canonical, and accepted, by design.
    testCase "a one-byte length header" $
      decodes "\x18\x2a" (PD.I 42)
  , testCase "a two-byte length header" $
      decodes "\x19\x00\x2a" (PD.I 42)
  , testCase "a four-byte length header" $
      decodes "\x1a\x00\x00\x00\x2a" (PD.I 42)
  , testCase "an eight-byte length header" $
      decodes "\x1b\x00\x00\x00\x00\x00\x00\x00\x2a" (PD.I 42)
  , testCase "a definite array behind a one-byte length header" $
      decodes "\x98\x02\x01\x02" (PD.List [PD.I 1, PD.I 2])
  , -- A constructor whose fields are a definite array rather than the
    -- indefinite one the builtin emits.
    testCase "a constructor with definite fields" $
      decodes "\xd8\x79\x82\x01\x02" (PD.Constr 0 [PD.I 1, PD.I 2])
  , testCase "a constructor at tag 1280" $
      decodes "\xd9\x05\x00\x80" (PD.Constr 7 [])
  ]

--------------------------------------------------------------------------------
-- Refusals
--------------------------------------------------------------------------------

{- | Everything the decoder answers @None@ for.

The two that matter most are the last-listed: __trailing bytes__ and
__truncation__. A decoder that accepted a prefix would let two different byte
strings claim the same datum, and one that ran off the end would let a truncated
witness claim a shorter one.
-}
refusalTests :: [TestTree]
refusalTests =
  [ testCase "empty input" $ declines ""
  , testCase "a text string" $ declines "\x61\x61"
  , testCase "an empty text string" $ declines "\x60"
  , testCase "a simple value" $ declines "\xf6"
  , testCase "a float" $ declines "\xfb\x40\x09\x21\xfb\x54\x44\x2d\x18"
  , testCase "tag 102" $ declines "\xd8\x66\x80"
  , testCase "a reserved header, 28" $ declines "\x1c"
  , testCase "a reserved header, 30" $ declines "\x1e"
  , testCase "a bare break byte" $ declines "\xff"
  , testCase "trailing bytes after a complete item" $ declines "\x01\x02"
  , testCase "trailing bytes after a complete array" $ declines "\x80\x00"
  , testCase "a truncated one-byte header" $ declines "\x18"
  , testCase "a truncated two-byte header" $ declines "\x19\x00"
  , testCase "a truncated byte string" $ declines "\x43\x01\x02"
  , testCase "an unterminated indefinite list" $ declines "\x9f\x01"
  , testCase "an array claiming more items than it has" $ declines "\x83\x01\x02"
  , testCase "a map with an odd number of items" $ declines "\xbf\x01\xff"
  ]

--------------------------------------------------------------------------------
-- The bignum gap
--------------------------------------------------------------------------------

{- | Three values @serialiseData@ emits that @deserialise@ does not read back.

All three are the original's behaviour, reproduced by the port, and all three
matter to a caller who assumed @deserialise ∘ serialise@ is the identity — the
stdlib's own documentation says it is, and for these three it is not.

They are also not the same /kind/ of failure, which is the part worth knowing:
two answer @None@ and one __aborts the script__.
-}
gapTests :: [TestTree]
gapTests =
  [ testGroup "an empty byte string that ends the input" emptyBytesTests
  , testGroup "constructor indices above 127" highConstrTests
  , testGroup "integers that do not fit in 64 bits" bignumTests
  , testGroup "tags below 121" lowTagTests
  ]

{- | @take@ refuses at an exhausted cursor even for a zero-length read.

The guard is @0 >= cursor@ and it is checked before the width is looked at, so a
@B ""@ whose header byte is the last byte of the input fails — there is nothing
left to take, even though nothing needs to be taken. Move it anywhere that leaves
a byte behind it and it decodes.
-}
emptyBytesTests :: [TestTree]
emptyBytesTests =
  [ testCase "on its own it does not decode" $
      declines (serialised (PD.B ""))
  , testCase "…and the encoding it declines is the one-byte header" $
      assertEqual "0x40" (BS.unpack (serialised (PD.B ""))) [0x40]
  , testCase "but inside a list, where bytes follow it, it does" $
      decodes (serialised (PD.List [PD.B "", PD.I 1])) (PD.List [PD.B "", PD.I 1])
  , testCase "…including as the last item, because the break byte follows it" $
      decodes (serialised (PD.List [PD.I 1, PD.B ""])) (PD.List [PD.I 1, PD.B ""])
  ]

{- | Plutus encodes a constructor index above 127 as tag 102, and tag 102 is the
one tag the decoder refuses by name.

Indices 0–6 are tags 121–127 and 7–127 are 1280 upwards; past that the encoding
changes shape to @102([index, fields])@, and the decoder has no case for it. So
the boundary is exactly 127: at 127 a round-trip works, at 128 it does not.
-}
highConstrTests :: [TestTree]
highConstrTests =
  [ testCase "127 round-trips" $
      decodes (serialised (PD.Constr 127 [])) (PD.Constr 127 [])
  , testCase "128 does not" $
      declines (serialised (PD.Constr 128 []))
  , testCase "…and the encoding it declines is tag 102" $
      assertEqual
        "0xd8 0x66"
        (BS.unpack (BS.take 2 (serialised (PD.Constr 128 []))))
        [0xd8, 0x66]
  , testCase "nor does a much larger index" $
      declines (serialised (PD.Constr 1000 [PD.List [PD.I 1]]))
  ]

{- | A bignum does not fail closed. It __aborts__.

@serialiseData@ emits tag 2 for an integer past 64 bits (tag 3 for the negative
side). The decoder reads every tag as a constructor index, so tag 2 becomes index
@2 - 121 = -119@, and @constrData@ rejects a negative index by terminating the
machine. The caller does not get @None@ to branch on; the script fails.

That is fail-closed in the sense that no wrong value is produced, but it is a
different failure mode from every other refusal in this module, and a consumer
that wraps @deserialise@ in a @when … is None@ will never see it.
-}
bignumTests :: [TestTree]
bignumTests =
  [ testCase "the boundary value still round-trips" $
      decodes (serialised (PD.I 18446744073709551615)) (PD.I 18446744073709551615)
  , testCase "one past it aborts" $
      aborts (serialised (PD.I 18446744073709551616))
  , testCase "…and the encoding it aborts on is a tag-2 bignum" $
      assertEqual
        "0xc2"
        (BS.unpack (BS.take 1 (serialised (PD.I 18446744073709551616))))
        [0xc2]
  , testCase "the negative side is a tag-3 bignum, and aborts too" $ do
      assertEqual
        "0xc3"
        (BS.unpack (BS.take 1 (serialised (PD.I (-18446744073709551617)))))
        [0xc3]
      aborts (serialised (PD.I (-18446744073709551617)))
  ]

{- | The same abort, reachable from bytes nobody encoded.

Every tag below 121 except 102 maps to a negative constructor index, so an input
carrying one aborts rather than declining. Tag 0 is the cheapest witness. This is
worth stating separately from the bignum case because it needs no encoder to
produce it: it is four bytes an adversary can write down.
-}
lowTagTests :: [TestTree]
lowTagTests =
  [ testCase "tag 0 aborts" $ aborts "\xc0\x80"
  , testCase "tag 120 aborts" $ aborts "\xd8\x78\x80"
  , testCase "tag 121 is constructor 0 and decodes" $
      decodes "\xd8\x79\x80" (PD.Constr 0 [])
  , testCase "tag 102 declines rather than aborting" $
      declines "\xd8\x66\x80"
  ]

--------------------------------------------------------------------------------
-- Driving the decoder
--------------------------------------------------------------------------------

{- | One case per value, labelled by the value.

A round-trip that failed as a batch would say only that one of eight encodings is
wrong; labelled individually it says which.
-}
roundTrips :: [PD.Data] -> [TestTree]
roundTrips values =
  [ testCase (summarise value) $ passertEval (pdecodesTo (serialised value) value)
  | value <- values
  ]

-- | A short label for a test case: the value, or its shape when it is large.
summarise :: PD.Data -> String
summarise = \case
  PD.I n -> "I " <> show n
  PD.B b -> "B, " <> show (BS.length b) <> " bytes"
  PD.List xs -> "List, " <> show (length xs) <> " items"
  PD.Map xs -> "Map, " <> show (length xs) <> " entries"
  PD.Constr n xs -> "Constr " <> show n <> ", " <> show (length xs) <> " fields"

decodes :: BS.ByteString -> PD.Data -> Assertion
decodes bytes value = passertEval (pdecodesTo bytes value)

-- | The decoder terminates the machine rather than answering.
aborts :: BS.ByteString -> Assertion
aborts bytes = pfails (pdeserialise # pconstant bytes)

declines :: BS.ByteString -> Assertion
declines bytes =
  passertEval $
    pmatch (pdeserialise # pconstant bytes) $ \case
      PNothing -> pconstant @PBool True
      PJust _ -> pconstant @PBool False

pdecodesTo :: forall (s :: S). BS.ByteString -> PD.Data -> Term s PBool
pdecodesTo bytes value =
  pmatch (pdeserialise # pconstant bytes) $ \case
    PNothing -> pconstant False
    PJust decoded -> decoded #== pconstant value

-- | The builtin's encoding of a value — the oracle, not a reimplementation of it.
serialised :: PD.Data -> BS.ByteString
serialised = fromBuiltin . serialiseData . dataToBuiltinData

filler :: Int -> BS.ByteString
filler n = BS.pack [fromIntegral (i `mod` 251) | i <- [0 .. n - 1]]
