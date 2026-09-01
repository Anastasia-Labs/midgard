{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Testing.CekConstant
Description : Behavioural tests for the Plutarch port of
              @lib/midgard/cek-constant-v1.ak@.

=== Five roots from a third implementation

The Aiken suite pins @constant_root_v1@ on five exact 32-byte values produced by
a TypeScript implementation on a different runtime. They are asserted here
verbatim, and they are the strongest check this module has: a shared
misunderstanding of the preimage would have to be shared with the TypeScript
too. Between them they cover an integer, a boolean, a list, a 4,095-byte byte
string and an 8,800-byte one — which is one, two and three blob chunks, so the
whole of the bounded-blob tree is pinned on absolute bytes.

=== The invariant the module exists for

A constant has one root whether a proof reveals its whole payload or only the
nodes a builtin touched. 'Midgard.CekConstant.pconstantRootV1' takes the
payload; 'Midgard.CekConstant.psemanticConstantRootV1' takes a
"Midgard.CekData" summary of it. The group below asserts they agree, across
every payload shape — including the ones where the two arrive at the memory
number by different routes.

=== Two memory numbers that must not be confused

@data_memory_size_v1@ is what Plutus charges a @Data@ value: four words a node
plus the leaf. @semantic_memory_size_v1@ is what the machine charges a /typed/
constant: the payload alone, with no per-node overhead. A list of two integers
costs 3 under the second and 15 under the first, and the tests pin both — a port
that used one where the other belongs would misprice every constant step and
still pass a round-trip suite.
-}
module Testing.CekConstant (tests) where

import Data.ByteString qualified as BS
import Data.ByteString.Base16 qualified as Base16
import PlutusCore.Data qualified as PD
import PlutusTx.Builtins qualified as Builtins
import PlutusTx.Builtins.Internal qualified as BI
import Test.Tasty
import Test.Tasty.HUnit

import Plutarch.Core.Utils (pand'List)
import Plutarch.Prelude

import Midgard.CekConstant (
  PConstantWitnessV1 (..),
  pconstantMemorySizeV1,
  pconstantPayloadMemorySizeV1,
  pconstantPayloadV1,
  pconstantRootV1,
  pconstantTypeIsKnownV1,
  pconstantTypeV1,
  pdataMemorySizeV1,
  pdecodeConstantPayloadV1,
  pdecodeConstantTypeV1,
  pintegerMemorySizeV1,
  ppayloadMatchesTypeV1,
  psemanticConstantRootV1,
  psemanticDataConstantRootV1,
  pverifyConstantWitnessV1,
 )
import Midgard.CekData (psemanticDataSummaryV1)
import Testing.Eval (passertEval, pfails)

--------------------------------------------------------------------------------
-- The suite
--------------------------------------------------------------------------------

tests :: TestTree
tests =
  testGroup
    "Midgard.CekConstant"
    [ goldenRootTests
    , typeDecodingTests
    , payloadDecodingTests
    , typeCheckingTests
    , memoryTests
    , agreementTests
    , semanticRootTests
    ]

--------------------------------------------------------------------------------
-- The golden roots
--------------------------------------------------------------------------------

{- | The five witnesses the Aiken suite pins against a TypeScript implementation.

Copied verbatim from @cek-constant-v1.test.ak@. Nothing in this tree produced
them.
-}
goldenRootTests :: TestTree
goldenRootTests =
  testGroup
    "the roots a third implementation produced"
    [ testCase "the integer 41" $
        passertEval $ rootIs "9f00ff" "1829" integerGolden
    , testCase "the boolean False" $
        passertEval $ rootIs "9f04ff" "d87a80" booleanGolden
    , testCase "the list [1, 2]" $
        passertEval $ rootIs "9f0500ff" "9f0102ff" listGolden
    , testCase "a 4,095-byte byte string, which is one blob chunk" $
        passertEval $
          rootIsBytes "9f01ff" (ser (PD.B (BS.replicate 4095 0))) bytes4095Golden
    , testCase "an 8,800-byte byte string, which is three" $
        passertEval $
          rootIsBytes "9f01ff" (ser (PD.B (BS.replicate 8800 0))) bytes8800Golden
    , testCase "and a wrong root simply fails to verify, without aborting" $
        passertEval $
          pnot
            #$ pverifyConstantWitnessV1
              # pconstant (hexOf booleanGolden)
              # witness (hexOf "9f00ff") (hexOf "1829")
    , testCase "a non-canonical payload aborts rather than failing" $
        pfails $
          pverifyConstantWitnessV1
            # pconstant (hexOf integerGolden)
            # witness (hexOf "9f00ff") (hexOf "190017")
    , testCase "a payload past the direct bound aborts" $
        pfails $
          pconstantRootV1 # witness (hexOf "9f01ff") (ser (PD.B (BS.replicate 9216 0)))
    ]
  where
    integerGolden = "35930512fad9db8f38585195d5363af8826bb2b002028fe85af5874a85ab305c"
    booleanGolden = "b9d299a51cb8e6181262d67d07662451930d812d4d79348a86d64361f93a889d"
    listGolden = "455063152b571b24bc7ba2e98d14120484eea1fe9a4b7210b639fb413f27f0ba"
    bytes4095Golden = "39286d9f63929ce56cdd18c4b40524a0c1ecf7909825a3ed3b60f3afcd299d3a"
    bytes8800Golden = "83ee73cbaad1ceaad96adde815cb0cd758faa8846e9cf06ab966889df1750a87"
    rootIs t p expected = rootIsBytes t (hexOf p) expected
    rootIsBytes t p expected =
      pconstantRootV1 # witness (hexOf t) p #== pconstant (hexOf expected)

witness :: forall (s :: S). BS.ByteString -> BS.ByteString -> Term s PConstantWitnessV1
witness typeCbor payloadCbor =
  pcon $ PConstantWitnessV1 (pdata (pconstant typeCbor)) (pdata (pconstant payloadCbor))

--------------------------------------------------------------------------------
-- Type decoding
--------------------------------------------------------------------------------

{- | Every wire tag, and the fact that 7 is not one.

The prefix encoding skips 7, so the tags run 0–6 then 8–11 while the
constructors run 0–10. A port that read the wire tag as a constructor index
would accept @[7]@ and misname everything above it.
-}
typeDecodingTests :: TestTree
typeDecodingTests =
  testGroup
    "the constant type is a prefix expression"
    [ testGroup
        "every wire tag decodes"
        [ testCase name $ passertEval $ typeIsKnown (typeCbor tags)
        | (name, tags) <-
            [ ("integer", [0])
            , ("byte string", [1])
            , ("string", [2])
            , ("unit", [3])
            , ("boolean", [4])
            , ("a list of integers", [5, 0])
            , ("a pair of an integer and a byte string", [6, 0, 1])
            , ("data", [8])
            , ("BLS G1", [9])
            , ("BLS G2", [10])
            , ("a Miller-loop result", [11])
            ]
        ]
    , testCase "a list of pairs" $ passertEval $ typeIsKnown (typeCbor [5, 6, 0, 1])
    , testCase "a pair of lists" $ passertEval $ typeIsKnown (typeCbor [6, 5, 0, 5, 1])
    , testCase "a pair of a pair and an integer" $
        passertEval $ typeIsKnown (typeCbor [6, 6, 0, 1, 0])
    , testGroup
        "and the decoder aborts on"
        [ testCase "tag 7, which the wire skips" $ pfails (decodeType (typeCbor [7]))
        , testCase "tag 12" $ pfails (decodeType (typeCbor [12]))
        , testCase "a negative tag" $ pfails (decodeType (typeCbor [-1]))
        , testCase "an empty type expression" $ pfails (decodeType (typeCbor []))
        , testCase "a list with no element type" $ pfails (decodeType (typeCbor [5]))
        , testCase "a pair with only one half" $ pfails (decodeType (typeCbor [6, 0]))
        , testCase "an expression that leaves an item over" $
            pfails (decodeType (typeCbor [0, 0]))
        , testCase "…even when the leftover is a legal type" $
            pfails (decodeType (typeCbor [1, 5, 0]))
        , testCase "a type that is not an array at all" $
            pfails (decodeType (ser (PD.I 0)))
        , testCase "a definite-length array, which is not what serialiseData emits" $
            pfails (decodeType (BS.concat ["\x81", cborI 0]))
        , testCase "a non-minimal integer header inside the array" $
            pfails (decodeType (BS.concat ["\x9f", "\x18\x00", "\xff"]))
        , testCase "a type expression past 64 bytes" $
            pfails (decodeType (typeCbor (replicate 63 5 <> [0])))
        , testCase "…where 62 nestings still fit" $
            passertEval $ typeIsKnown (typeCbor (replicate 61 5 <> [0]))
        , testCase "an empty type payload" $ pfails (decodeType "")
        ]
    ]

typeIsKnown :: forall (s :: S). BS.ByteString -> Term s PBool
typeIsKnown t = pconstantTypeIsKnownV1 # pconstant t

decodeType :: forall (s :: S). BS.ByteString -> Term s PBool
decodeType t = pconstantTypeIsKnownV1 # pconstant t

--------------------------------------------------------------------------------
-- Payload decoding
--------------------------------------------------------------------------------

payloadDecodingTests :: TestTree
payloadDecodingTests =
  testGroup
    "the payload is decoded canonically"
    [ testGroup
        "round-trips"
        [ testCase name $
            passertEval $
              pdecodeConstantPayloadV1 # pconstant (ser value) #== pconstant value
        | (name, value) <-
            [ ("an integer", PD.I 41)
            , ("a negative integer", PD.I (-41))
            , ("a byte string", PD.B "midgard")
            , ("a long byte string", PD.B (BS.replicate 200 0x11))
            , ("a list", PD.List [PD.I 1, PD.I 2])
            , ("the empty list", PD.List [])
            , ("a map", PD.Map [(PD.I 1, PD.B "x")])
            , ("a constructor", PD.Constr 1 [])
            ]
        ]
    , testCase "the empty byte string, which the CBOR decoder cannot read back" $
        passertEval $
          pdecodeConstantPayloadV1 # pconstant "\x40" #== pconstant (PD.B "")
    , testCase "…and 0x40 is what serialiseData emits for it" $
        ser (PD.B "") @?= "\x40"
    , testCase "a non-canonical integer header aborts" $
        pfails $ pdecodeConstantPayloadV1 # pconstant (hexOf "190017")
    , testCase "trailing bytes abort" $
        pfails $ pdecodeConstantPayloadV1 # pconstant (ser (PD.I 41) <> "\x00")
    , testCase "a payload past 9,215 bytes aborts" $
        pfails $ pdecodeConstantPayloadV1 # pconstant (BS.replicate 9216 0x00)
    , testCase "an empty payload aborts" $
        pfails $ pdecodeConstantPayloadV1 # pconstant ""
    ]

--------------------------------------------------------------------------------
-- Type checking the payload
--------------------------------------------------------------------------------

typeCheckingTests :: TestTree
typeCheckingTests =
  testGroup
    "the payload must fit the type"
    [ testGroup
        "accepts"
        [ testCase name $ passertEval $ matches tags value
        | (name, tags, value) <-
            [ ("an integer under integer", [0], PD.I 41)
            , ("bytes under byte string", [1], PD.B "midgard")
            , ("the empty byte string", [1], PD.B "")
            , ("ASCII under string", [2], PD.B "midgard")
            , ("valid multi-byte UTF-8 under string", [2], PD.B "\xc3\xa9")
            , ("the unit constructor", [3], PD.Constr 0 [])
            , ("boolean False", [4], PD.Constr 0 [])
            , ("boolean True", [4], PD.Constr 1 [])
            , ("a list of integers", [5, 0], PD.List [PD.I 1, PD.I 2])
            , ("the empty list of anything", [5, 0], PD.List [])
            , ("a pair", [6, 0, 1], PD.Constr 0 [PD.I 1, PD.B "x"])
            , ("anything at all under data", [8], PD.Map [(PD.I 1, PD.B "x")])
            , ("48 bytes under G1", [9], PD.B (BS.replicate 48 0x01))
            , ("96 bytes under G2", [10], PD.B (BS.replicate 96 0x01))
            ]
        ]
    , testGroup
        "refuses"
        [ testCase name $ passertEval $ pnot #$ matches tags value
        | (name, tags, value) <-
            [ ("bytes under integer", [0], PD.B "x")
            , ("an integer under byte string", [1], PD.I 1)
            , ("an integer under string", [2], PD.I 1)
            , ("a non-constructor under unit", [3], PD.I 0)
            , ("a unit with a field", [3], PD.Constr 0 [PD.I 1])
            , ("a unit at tag 1", [3], PD.Constr 1 [])
            , ("a boolean at tag 2", [4], PD.Constr 2 [])
            , ("a boolean with a field", [4], PD.Constr 0 [PD.I 1])
            , ("a list with one wrong element", [5, 0], PD.List [PD.I 1, PD.B "x"])
            , ("a map under list", [5, 0], PD.Map [])
            , ("a pair of the wrong halves", [6, 0, 1], PD.Constr 0 [PD.B "x", PD.I 1])
            , ("a pair at tag 1", [6, 0, 1], PD.Constr 1 [PD.I 1, PD.B "x"])
            , ("a pair with three fields", [6, 0, 1], PD.Constr 0 [PD.I 1, PD.B "x", PD.I 2])
            , ("a pair with one field", [6, 0, 1], PD.Constr 0 [PD.I 1])
            , ("47 bytes under G1", [9], PD.B (BS.replicate 47 0x01))
            , ("49 bytes under G1", [9], PD.B (BS.replicate 49 0x01))
            , ("48 bytes under G2", [10], PD.B (BS.replicate 48 0x01))
            , ("anything at all under a Miller-loop result", [11], PD.B "")
            , ("even a 192-byte string under a Miller-loop result", [11], PD.B (BS.replicate 192 0))
            ]
        ]
    , testCase "invalid UTF-8 under string aborts rather than refusing" $
        pfails $ matches [2] (PD.B "\xff\xfe")
    , testCase "a witness whose payload does not fit its type aborts" $
        pfails $ pconstantRootV1 # witness (typeCbor [0]) (ser (PD.B "x"))
    , testCase "reading a witness's type checks the payload too" $
        pfails $ pconstantTypeV1 # witness (typeCbor [0]) (ser (PD.B "x"))
    , testCase "…and so does reading its payload" $
        pfails $ pconstantPayloadV1 # witness (typeCbor [0]) (ser (PD.B "x"))
    , testCase "a well-typed witness yields its payload" $
        passertEval $
          pconstantPayloadV1 # witness (typeCbor [0]) (ser (PD.I 41)) #== pconstant (PD.I 41)
    ]

matches :: forall (s :: S). [Integer] -> PD.Data -> Term s PBool
matches tags value =
  ppayloadMatchesTypeV1
    # (pdecodeConstantTypeV1 # pconstant (typeCbor tags))
    # pconstant value

--------------------------------------------------------------------------------
-- The two memory numbers
--------------------------------------------------------------------------------

memoryTests :: TestTree
memoryTests =
  testGroup
    "the two memory numbers"
    [ testCase "the signed integer sizes the Aiken suite pins" $
        passertEval $
          pand'List
            [ pintegerMemorySizeV1 # pconstant n #== pconstant expected
            | (n, expected) <- [(-129, 2), (-128, 1), (-1, 1), (0, 1), (127, 1), (128, 2)]
            ]
    , testGroup
        "integer memory against the reference"
        [ testCase (show n) $
            passertEval $
              pintegerMemorySizeV1 # pconstant n #== pconstant (integerMemorySize n)
        | n <-
            [ 0
            , 1
            , -1
            , 255
            , 256
            , -256
            , 65535
            , 65536
            , 9223372036854775807
            , -9223372036854775808
            , 340282366920938463463374607431768211456
            ]
        ]
    , testCase "a map of an integer to a list of bytes costs 19, as the Aiken pins" $
        passertEval $
          pdataMemorySizeV1 # pconstant goldenMapData #== pconstant 19
    , testGroup
        "data memory against the reference"
        [ testCase name $
            passertEval $
              pdataMemorySizeV1 # pconstant value #== pconstant (dataMemorySize value)
        | (name, value) <-
            [ ("an integer", PD.I 41)
            , ("the empty byte string, which still costs one word", PD.B "")
            , ("a byte string", PD.B (BS.replicate 7 0))
            , ("the empty list", PD.List [])
            , ("a nested list", PD.List [PD.List [PD.I 1], PD.I 2])
            , ("a map", goldenMapData)
            , ("a constructor", PD.Constr 3 [PD.I 1, PD.B "xy"])
            , ("the empty constructor", PD.Constr 0 [])
            ]
        ]
    , testCase "a list of two integers costs 3 to the machine, as the Aiken pins" $
        passertEval $
          pconstantMemorySizeV1 # witness (typeCbor [5, 0]) (hexOf "9f01190100ff")
            #== pconstant 3
    , testCase "…and 15 as a Data value, which is the number not to use" $
        passertEval $
          pdataMemorySizeV1 # pconstant (PD.List [PD.I 1, PD.I 256]) #== pconstant 15
    , testGroup
        "typed constant memory against the reference"
        [ testCase name $
            passertEval $
              pconstantPayloadMemorySizeV1
                # (pdecodeConstantTypeV1 # pconstant (typeCbor tags))
                # pconstant value
                #== pconstant (semanticMemorySize tags value)
        | (name, tags, value) <-
            [ ("an integer", [0], PD.I 41)
            , ("a large integer", [0], PD.I 340282366920938463463374607431768211456)
            , ("a byte string", [1], PD.B (BS.replicate 7 0))
            , ("the empty byte string", [1], PD.B "")
            , ("a string", [2], PD.B "midgard")
            , ("unit", [3], PD.Constr 0 [])
            , ("a boolean", [4], PD.Constr 1 [])
            , ("a list of integers", [5, 0], PD.List [PD.I 1, PD.I 256])
            , ("an empty list", [5, 0], PD.List [])
            , ("a list of lists", [5, 5, 0], PD.List [PD.List [PD.I 1], PD.List []])
            , ("a pair", [6, 0, 1], PD.Constr 0 [PD.I 1, PD.B "xyz"])
            , ("a data constant, where the two numbers coincide", [8], goldenMapData)
            , ("G1", [9], PD.B (BS.replicate 48 0x01))
            , ("G2", [10], PD.B (BS.replicate 96 0x01))
            ]
        ]
    , testCase "a payload that does not fit its type aborts before it is sized" $
        pfails $
          pconstantPayloadMemorySizeV1
            # (pdecodeConstantTypeV1 # pconstant (typeCbor [0]))
            # pconstant (PD.B "x")
    ]
  where
    goldenMapData = PD.Map [(PD.I 1, PD.List [PD.B "\x00\x00"])]

--------------------------------------------------------------------------------
-- One root, two ways of reaching it
--------------------------------------------------------------------------------

{- | The whole point of the module.

For each payload, the witness form and the summary form must produce the same
root. They compute the memory by different routes — the witness form walks the
typed payload, the summary form is handed the number — so this is not a
tautology; it is the rule that lets a builtin proof touch part of a value
without changing the value's identity.
-}
agreementTests :: TestTree
agreementTests =
  testGroup
    "the direct and semantic roots agree"
    [ testCase name $
        passertEval $
          pconstantRootV1 # witness (typeCbor tags) (ser value)
            #== psemanticConstantRootV1
              # pconstant (typeCbor tags)
              # (psemanticDataSummaryV1 # pconstant value)
              # pconstant (semanticMemorySize tags value)
    | (name, tags, value) <-
        [ ("an integer", [0], PD.I 41)
        , ("a negative integer", [0], PD.I (-1000))
        , ("a byte string", [1], PD.B "midgard")
        , ("a 200-byte string", [1], PD.B (BS.replicate 200 0x22))
        , ("a string", [2], PD.B "midgard")
        , ("unit", [3], PD.Constr 0 [])
        , ("a boolean", [4], PD.Constr 1 [])
        , ("a list of integers", [5, 0], PD.List [PD.I 1, PD.I 2])
        , ("an empty list", [5, 0], PD.List [])
        , ("a pair", [6, 0, 1], PD.Constr 0 [PD.I 1, PD.B "x"])
        , ("a data constant", [8], PD.Map [(PD.I 1, PD.B "x")])
        , ("G1", [9], PD.B (BS.replicate 48 0x01))
        ]
    ]

--------------------------------------------------------------------------------
-- The semantic root's own guards
--------------------------------------------------------------------------------

semanticRootTests :: TestTree
semanticRootTests =
  testGroup
    "the semantic root"
    [ testCase "a Data constant seeded from its summary alone" $
        passertEval $
          psemanticDataConstantRootV1 # (psemanticDataSummaryV1 # pconstant dataValue)
            #== pconstantRootV1 # witness (typeCbor [8]) (ser dataValue)
    , testCase "…and 0x9f08ff is the one-item type expression naming Data" $
        typeCbor [8] @?= hexOf "9f08ff"
    , testCase "an unknown type aborts" $
        pfails $
          psemanticConstantRootV1
            # pconstant (typeCbor [7])
            # (psemanticDataSummaryV1 # pconstant dataValue)
            # pconstant 0
    , testCase "a type past 64 bytes aborts" $
        pfails $
          psemanticConstantRootV1
            # pconstant (typeCbor (replicate 63 5 <> [0]))
            # (psemanticDataSummaryV1 # pconstant dataValue)
            # pconstant 0
    , testCase "a negative memory aborts" $
        pfails $
          psemanticConstantRootV1
            # pconstant (typeCbor [8])
            # (psemanticDataSummaryV1 # pconstant dataValue)
            # pconstant (-1)
    , testCase "a memory of zero is allowed, since the guard is a sign check" $
        passertEval $
          ( plengthBS
              #$ psemanticConstantRootV1
                # pconstant (typeCbor [8])
                # (psemanticDataSummaryV1 # pconstant dataValue)
                # pconstant 0
          )
            #== 32
    ]
  where
    dataValue = PD.Map [(PD.I 1, PD.B "x")]

--------------------------------------------------------------------------------
-- The reference
--------------------------------------------------------------------------------

integerMemorySize :: Integer -> Integer
integerMemorySize v = unsignedByteSize (if v < 0 then (negate v - 1) * 2 else v * 2)
  where
    unsignedByteSize n
      | n < 256 = 1
      | otherwise = 1 + unsignedByteSize (n `div` 256)

bytearrayMemorySize :: BS.ByteString -> Integer
bytearrayMemorySize b = if BS.null b then 1 else fromIntegral (BS.length b)

-- | Aiken @data_memory_size_v1@ — four words a node, plus the leaf.
dataMemorySize :: PD.Data -> Integer
dataMemorySize = \case
  PD.Constr _ fields -> 4 + sum (map dataMemorySize fields)
  PD.Map entries -> 4 + sum [dataMemorySize k + dataMemorySize v | (k, v) <- entries]
  PD.List items -> 4 + sum (map dataMemorySize items)
  PD.I n -> 4 + integerMemorySize n
  PD.B b -> 4 + bytearrayMemorySize b

{- | Aiken @semantic_memory_size_v1@, driven by the same prefix tag list the
wire carries.

Written over the tags rather than over a decoded type so that the reference does
not share the port's decoder: if 'Midgard.CekConstant.pdecodeConstantTypeV1'
mapped a wire tag to the wrong constructor, the sizes would part company here.
-}
semanticMemorySize :: [Integer] -> PD.Data -> Integer
semanticMemorySize tags value = case go tags value of
  (size, []) -> size
  (_, leftover) -> error ("reference semanticMemorySize: leftover " <> show leftover)
  where
    go :: [Integer] -> PD.Data -> (Integer, [Integer])
    go [] _ = error "reference semanticMemorySize: empty type"
    go (tag : rest) payload = case tag of
      0 -> (integerMemorySize (unI payload), rest)
      1 -> (bytearrayMemorySize (unB payload), rest)
      2 -> (bytearrayMemorySize (unB payload), rest)
      3 -> (1, rest)
      4 -> (1, rest)
      5 ->
        let items = unList payload
            elementTags = rest
            remaining = skip rest
         in (sum [fst (go elementTags item) | item <- items], remaining)
      6 ->
        let fields = unConstrFields payload
            afterFirst = skip rest
            remaining = skip afterFirst
         in case fields of
              [a, b] -> (fst (go rest a) + fst (go afterFirst b), remaining)
              _ -> error "reference semanticMemorySize: pair arity"
      8 -> (dataMemorySize payload, rest)
      9 -> (48, rest)
      10 -> (96, rest)
      11 -> (192, rest)
      _ -> error ("reference semanticMemorySize: unknown tag " <> show tag)

    -- | Drop one complete type expression from the front of a tag list.
    skip :: [Integer] -> [Integer]
    skip [] = error "reference semanticMemorySize: truncated type"
    skip (tag : rest)
      | tag == 5 = skip rest
      | tag == 6 = skip (skip rest)
      | otherwise = rest

    unI (PD.I n) = n
    unI d = error ("reference: not an integer: " <> show d)
    unB (PD.B b) = b
    unB d = error ("reference: not bytes: " <> show d)
    unList (PD.List xs) = xs
    unList d = error ("reference: not a list: " <> show d)
    unConstrFields (PD.Constr _ fs) = fs
    unConstrFields d = error ("reference: not a constructor: " <> show d)

--------------------------------------------------------------------------------
-- Plumbing
--------------------------------------------------------------------------------

-- | Aiken's @cbor.serialise@, which is @serialiseData@ on the value read as @Data@.
ser :: PD.Data -> BS.ByteString
ser = Builtins.fromBuiltin . Builtins.serialiseData . BI.BuiltinData

cborI :: Integer -> BS.ByteString
cborI = ser . PD.I

-- | A type expression, in the indefinite-array form @serialiseData@ emits.
typeCbor :: [Integer] -> BS.ByteString
typeCbor = ser . PD.List . map PD.I

hexOf :: BS.ByteString -> BS.ByteString
hexOf = Base16.decodeLenient
