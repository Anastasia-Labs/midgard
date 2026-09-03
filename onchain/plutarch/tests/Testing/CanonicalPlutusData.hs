{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Testing.CanonicalPlutusData
Description : Behavioural tests for the Plutarch port of
              @lib/midgard/canonical-plutus-data-v1.ak@.

The predicate's definition is "membership in the image of @serialiseData@", so
the strongest available reference is the definition itself. @serialiseData@ is a
Plutus builtin with a Haskell implementation, and the first group below runs it
over a spread of @Data@ values and requires every byte string it emits to be
accepted. That reference is genuinely independent of the port — it is the
builtin, not a second copy of the grammar — and it is the only one that can catch
a walk which is self-consistently wrong.

What the builtin cannot supply is the /negative/ direction, since it emits
nothing outside its own image. The tables further down are therefore written from
§6.2: for each rule, the smallest byte string that breaks it. They include the
two classes the retired stdlib round-trip pin rejected — canonical bignums and
tag-102 constructors — which are datums L1 accepts and Midgard used to refuse.

=== Two predicates, and the tests must keep them apart

'pisMaterialisablePlutusDataV1' is strictly narrower than
'pisCanonicalPlutusDataV1', and the gap is exactly the bignums and the tag-102
constructors. Every case in the materialisability group asserts /both/, because a
port that collapsed the two into one predicate would still pass a suite that only
ever asked one of them. The nesting cases matter for the same reason: the screen
this replaced read byte zero only, so a bignum one level down slipped past it.
-}
module Testing.CanonicalPlutusData (tests) where

import Data.ByteString qualified as BS
import Data.ByteString.Base16 qualified as Base16
import Data.Word (Word8)
import PlutusCore.Data qualified as PD
import PlutusTx.Builtins (dataToBuiltinData, fromBuiltin, serialiseData)
import Test.Tasty
import Test.Tasty.HUnit

import Plutarch.Prelude

import Midgard.CanonicalPlutusData (
  pcanonicalDataEndAtV1,
  pisCanonicalPlutusDataV1,
  pisMaterialisablePlutusDataV1,
 )
import Testing.Eval (passertEval)

--------------------------------------------------------------------------------
-- The suite
--------------------------------------------------------------------------------

tests :: TestTree
tests =
  testGroup
    "Canonical Plutus Data Tests"
    [ testGroup "the definition: serialiseData's image" imageTests
    , testGroup "the L1-parity re-pin" repinTests
    , testGroup "what §6.2 still rejects" rejectionTests
    , testGroup "materialisability" materialisableTests
    , testGroup "skipping one item" endAtTests
    ]

--------------------------------------------------------------------------------
-- The definition
--------------------------------------------------------------------------------

{- | Everything the builtin emits is canonical, by definition.

The spread covers each major the grammar admits and each head width inside it:
integers on both sides of every minimal-encoding boundary, byte strings on both
sides of the 64-byte chunking threshold, empty and non-empty lists and maps, and
constructor alternatives in all three tag spellings.
-}
imageTests :: [TestTree]
imageTests =
  [ testCase (name <> " is canonical") (accepts (serialised value))
  | (name, value) <- imageCorpus
  ]
    <> [ testCase "…and every one of them consumes exactly its own bytes" $
          passertEval $
            pall'
              [ endAt (serialised value) 0 #== pcon (PJust (byteLength (serialised value)))
              | (_, value) <- imageCorpus
              ]
       ]

{- | The @Data@ values the builtin is run over.

Named rather than anonymous so a failure says which shape broke, and kept as one
list so the round-trip group and the offset group cannot drift apart.
-}
imageCorpus :: [(String, PD.Data)]
imageCorpus =
  [ ("zero", PD.I 0)
  , ("the largest single-byte integer", PD.I 23)
  , ("the smallest one-byte-argument integer", PD.I 24)
  , ("255", PD.I 255)
  , ("65536", PD.I 65536)
  , ("minus one", PD.I (-1))
  , ("minus 24", PD.I (-24))
  , ("a five-byte negative", PD.I (-4294967297))
  , ("the empty byte string", PD.B "")
  , ("a one-byte string", PD.B "\x00")
  , ("a byte string at the chunk size", PD.B (BS.take 64 long65))
  , ("a byte string one byte over it", PD.B long65)
  , ("a byte string over two chunks", PD.B (long65 <> long65))
  , ("the empty list", PD.List [])
  , ("a two-item list", PD.List [PD.I 1, PD.I 2])
  , ("the empty map", PD.Map [])
  , ("a one-entry map", PD.Map [(PD.I 1, PD.B "\xaa")])
  , ("a two-entry map", PD.Map [(PD.I 1, PD.B "\xaa"), (PD.B "", PD.List [])])
  , ("constructor zero with no arguments", PD.Constr 0 [])
  , ("the last small-tag alternative", PD.Constr 6 [PD.I 1])
  , ("the first large-tag alternative", PD.Constr 7 [])
  , ("the last large-tag alternative", PD.Constr 127 [PD.B "\xff"])
  , ("the first tag-102 alternative", PD.Constr 128 [])
  , ("a four-digit alternative", PD.Constr 1000 [PD.I 1])
  , ("the largest alternative", PD.Constr (twoPow64 - 1) [])
  , ("a bignum", PD.I twoPow64)
  , ("a negative bignum", PD.I (-twoPow64 - 1))
  , ("a very wide bignum", PD.I (twoPow64 * twoPow64))
  , -- Nesting, because a walk that is right at depth zero and wrong below it
    -- would pass every flat case above.
    ("a bignum nested three deep", PD.List [PD.Map [(PD.I 0, PD.Constr 1 [PD.I twoPow64])]])
  , ("a tag-102 constructor nested in a map value", PD.Map [(PD.I 0, PD.Constr 200 [PD.I 1])])
  ]

--------------------------------------------------------------------------------
-- The L1-parity re-pin
--------------------------------------------------------------------------------

{- | The byte forms the retired stdlib round-trip pin rejected.

Every one is something cardano-ledger's @decodeData@ accepts, so every one has to
be canonical here — that is the whole point of the re-pin. Spelled as literal
bytes rather than through the builtin so that the /encoding/ is pinned and not
merely the acceptance.
-}
repinTests :: [TestTree]
repinTests =
  [ testCase "2^64 exactly: nine magnitude bytes, no leading zero" $
      accepts (hex "c249010000000000000000")
  , testCase "…and its negative twin" $ accepts (hex "c349010000000000000000")
  , testCase "…and a wider magnitude" $ accepts (hex "c24affffffffffffffffffff")
  , testCase "the builtin emits exactly those bytes for 2^64" $
      serialised (PD.I twoPow64) @?= hex "c249010000000000000000"
  , testCase "a magnitude of exactly 64 bytes, the last definite width" $
      accepts (hex "c2" <> hex "5840" <> BS.replicate 64 0x01)
  , testCase "a chunked magnitude, two chunks and a break" $
      accepts (hex "c25f" <> chunk 64 0x01 <> chunk 1 0x02 <> hex "ff")
  , testCase "alternative 128 with an empty argument list" $ accepts (hex "d86682188080")
  , testCase "…with one argument" $ accepts (hex "d8668218809f00ff")
  , testCase "…and alternative 65,535" $ accepts (hex "d8668219ffff80")
  , testCase "the builtin emits exactly those bytes for alternative 128" $
      serialised (PD.Constr 128 []) @?= hex "d86682188080"
  ]

--------------------------------------------------------------------------------
-- What §6.2 still rejects
--------------------------------------------------------------------------------

{- | One byte string per rule, each the smallest thing that breaks it.

The bignum rules get the most attention because they are the newest: a magnitude
shorter than nine bytes has a plain integer spelling, a leading zero is never
minimal, and a chunked magnitude below the chunking threshold would have been
spelled definite.
-}
rejectionTests :: [TestTree]
rejectionTests =
  [ testCase (why <> " rejects") (refuses bytes)
  | (why, bytes) <-
      [ ("a non-minimal integer head", hex "1817")
      , ("a definite non-empty list", hex "8100")
      , ("an indefinite empty list", hex "9fff")
      , ("an empty indefinite byte string", hex "5fff")
      , ("a constructor with a definite argument list", hex "d8798100")
      , ("an indefinite map", hex "bf0000ff")
      , ("text, which is outside Plutus Data", hex "6161")
      , ("a simple value, likewise", hex "f5")
      , ("an unknown tag", hex "c11a514b67b0")
      , ("trailing bytes after a complete item", hex "0000")
      , ("a truncated head", hex "18")
      , ("empty input", "")
      , ("an empty bignum magnitude", hex "c240")
      , ("a one-byte magnitude", hex "c24101")
      , ("an eight-byte magnitude, which fits a plain integer", hex "c248ffffffffffffffff")
      , ("a nine-byte magnitude with a leading zero", hex "c249000100000000000000")
      , ("a chunked magnitude below the chunking threshold", hex "c25f49010000000000000000ff")
      , ("a 65-byte definite magnitude", hex "c25841" <> BS.replicate 65 0x01)
      , ("tag 102 at alternative zero", hex "d866820080")
      , ("tag 102 at alternative 127", hex "d86682187f80")
      , ("tag 102 with a definite argument list", hex "d8668218808100")
      , ("tag 102 with a non-minimal alternative head", hex "d8668219008080")
      , ("a 65-byte definite byte string", hex "5841" <> long65)
      , ("a single 64-byte chunk in indefinite form", hex "5f" <> chunk 64 0x01 <> hex "ff")
      , ("an indefinite byte string with a zero-length final chunk", hex "5f" <> chunk 64 0x01 <> hex "40ff")
      , ("a truncated definite byte string", hex "4300")
      , ("a map whose entry count outruns its entries", hex "a20001")
      , ("a nested non-canonical item", hex "9f1817ff")
      , ("a non-canonical map value", hex "a1008100")
      ]
  ]
    <> [ testCase "a 65-byte string is fine once the builtin chunks it" $
          accepts (serialised (PD.B long65))
       ]

--------------------------------------------------------------------------------
-- Materialisability
--------------------------------------------------------------------------------

{- | §11.2: canonical is not the same question as decodable.

Each case states both verdicts, because the interesting failure is the two
predicates agreeing when they should not.
-}
materialisableTests :: [TestTree]
materialisableTests =
  [ testCase (name <> ": " <> verdict expected) $
    passertEval $
      pisCanonicalPlutusDataV1
        # pconstant bytes
        #&& (pisMaterialisablePlutusDataV1 # pconstant bytes #== pconstant expected)
  | (name, bytes, expected) <-
      [ ("a plain integer", hex "00", True)
      , ("an ordinary constructor", hex "d87980", True)
      , ("a list of ordinary items", hex "9f00d87980ff", True)
      , ("a map of ordinary items", hex "a10000", True)
      , ("a bignum", hex "c249010000000000000000", False)
      , ("a negative bignum", hex "c349010000000000000000", False)
      , ("a tag-102 constructor", hex "d86682188080", False)
      , -- The screen this replaced read byte zero only, so these are the cases
        -- that used to slip through and abort the decoder at depth.
        ("a bignum inside a list", hex "9fc249010000000000000000ff", False)
      , ("a bignum as a map key", hex "a1c24901000000000000000000", False)
      , ("a bignum as a map value", hex "a100c249010000000000000000", False)
      , ("a bignum inside a constructor", hex "d8799fc249010000000000000000ff", False)
      , ("a tag-102 constructor inside a list", hex "9fd86682188080ff", False)
      , ("a bignum three levels down", serialised nestedBignum, False)
      ]
  ]

verdict :: Bool -> String
verdict True = "materialisable"
verdict False = "canonical but not materialisable"

nestedBignum :: PD.Data
nestedBignum = PD.List [PD.Map [(PD.I 0, PD.Constr 1 [PD.I twoPow64])]]

--------------------------------------------------------------------------------
-- Skipping one item
--------------------------------------------------------------------------------

{- | @canonical_data_end_at_v1@ is how §11.2 navigates to a datum's @k@-th child.

Reaching child @k@ means skipping @k@ siblings, so the test is a concatenation
walked one item at a time — with the last step landing exactly on the end.
-}
endAtTests :: [TestTree]
endAtTests =
  [ testCase "walks a concatenation one item at a time" $
      passertEval $
        pall'
          [ endAt concatenated (pconstant from) #== pcon (PJust (pconstant to))
          | (from, to) <- zip offsets (drop 1 offsets)
          ]
  , testCase "…and the last step lands on the end of the bytes" $
      passertEval $ pconstant (last offsets) #== byteLength concatenated
  , {- The skipper is not a boundary check: it scans from wherever it is pointed,
       and payload bytes can spell a canonical item of their own. Both cases
       below are one canonical three-byte string whose payload is read into
       instead of over — the first payload answers, the second declines, and
       neither says anything about the string that encloses it. A caller's
       guarantee that an offset /is/ a boundary comes from having walked to it
       from zero, which is what the first case in this group does. -}
    testCase "an offset inside another item's payload can still answer" $
      passertEval $
        pisCanonicalPlutusDataV1
          # pconstant (hex "43008001")
          #&& endAt (hex "43008001") 2
          #== pcon (PJust 3)
  , testCase "…and can decline there, without that being a verdict on the whole" $
      passertEval $
        pisCanonicalPlutusDataV1
          # pconstant (hex "43001f01")
          #&& endAt (hex "43001f01") 2
          #== pcon PNothing
  , testCase "an offset past the end is not either" $
      passertEval $ endAt concatenated (byteLength concatenated) #== pcon PNothing
  , testCase "a negative offset is not either" $
      passertEval $ endAt concatenated (-1) #== pcon PNothing
  , -- The predicate is stricter than the skipper by exactly the trailing bytes,
    -- so a concatenation of canonical items is skippable and not canonical.
    testCase "the concatenation itself is not one canonical item" $
      refuses concatenated
  ]
  where
    offsets = scanl (+) 0 (map (fromIntegral . BS.length . serialised) siblings) :: [Integer]
    concatenated = BS.concat (map serialised siblings)
    siblings =
      [ PD.I 0
      , PD.B long65
      , PD.Constr 128 [PD.I twoPow64]
      , PD.Map [(PD.I 1, PD.List [PD.I 2, PD.B "\xaa"])]
      , PD.List []
      ]

--------------------------------------------------------------------------------
-- Plumbing
--------------------------------------------------------------------------------

accepts, refuses :: BS.ByteString -> Assertion
accepts bytes = passertEval $ pisCanonicalPlutusDataV1 # pconstant bytes
refuses bytes = passertEval $ pnot #$ pisCanonicalPlutusDataV1 # pconstant bytes

endAt ::
  forall (s :: S). BS.ByteString -> Term s PInteger -> Term s (PMaybe PInteger)
endAt bytes offset = pcanonicalDataEndAtV1 # pconstant bytes # offset

byteLength :: forall (s :: S). BS.ByteString -> Term s PInteger
byteLength = pconstant . fromIntegral . BS.length

-- | The builtin itself, which is what canonicity is defined as the image of.
serialised :: PD.Data -> BS.ByteString
serialised = fromBuiltin . serialiseData . dataToBuiltinData

pall' :: forall (s :: S). [Term s PBool] -> Term s PBool
pall' = foldr (#&&) (pconstant True)

hex :: BS.ByteString -> BS.ByteString
hex = either error id . Base16.decode

-- | A definite CBOR byte-string chunk of @n@ bytes, all the same.
chunk :: Int -> Word8 -> BS.ByteString
chunk n b
  | n <= 23 = BS.cons (0x40 + fromIntegral n) (BS.replicate n b)
  | otherwise = hex "58" <> BS.singleton (fromIntegral n) <> BS.replicate n b

-- | 2^64 — the smallest integer that needs a bignum.
twoPow64 :: Integer
twoPow64 = 18446744073709551616

long65 :: BS.ByteString
long65 = BS.pack [0 .. 64]
