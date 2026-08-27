{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Testing.CekData
Description : Behavioural tests for the Plutarch port of
              @lib/midgard/cek-data-v1.ak@.

The authenticated @Data@ node format, checked against a Haskell reference
written here from @cek-data-v1.ak@'s preimage layouts rather than from the port.
The reference borrows only 'blake2b256', 'arrayHeader' and 'definiteBytes' from
"Testing.BoundedItem" — the shared CBOR primitives, already pinned there against
a different port. Every domain string, field order and field range below is
transcribed from the Aiken.

=== The two numbers are the point

A node carries its subject's CBOR length and its @ExMemory@ so a replay can
charge a step without materialising the value. Both numbers therefore have to be
right, and both have an oracle that is not this tree: 'plistCborLength',
'pmapCborLength' and 'pbytesDataCborLengthV1' are checked against the /actual
length of @serialiseData@ applied to a real value of that shape/, so an
arithmetic error in the header sizes shows up as a mismatch against the Plutus
builtin rather than against a second transcription of the same formula.

=== Where the canonicity lives

"Aiken.Cbor" reads indefinite-length arrays, non-minimal headers and reordered
maps, all deliberately. The inspectors reject them, and they do it in one place:
the re-encoding comparison at the end. The declining group below feeds each of
those forms to 'pinspectDataNodePreimageV1' as a preimage that decodes to the
right fields, so nothing but the re-encoding can be what turns them away.

=== One node shape cannot be inspected at all

@BytesData@ encodes as kind, /root/, length — the only shape that puts its root
before its count — while @inspect_data_node_preimage_v1@'s five-item branch reads
kind, /count/, root and demands an integer where the byte node has 32 bytes. No
valid byte node therefore survives its own inspector. That is the Aiken's
behaviour, confirmed against @aiken check@ on a hand-written round-trip, not an
artefact of the port; the group below pins it as a refusal so a later fix to the
field order has to be a deliberate one, on both sides.

=== The semantic commitment is checked against its own pieces

'phashSemanticDataV1' walks a value and 'plistDataSummaryV1' and friends build a
node from a sequence summary. They are separate code paths that must agree, and
the last group asserts they do — on top of both being checked against the
reference.
-}
module Testing.CekData (
  tests,

  -- * Reference pieces, shared with the modules built on this one
  RefSummary (..),
  semantic,
) where

import Data.ByteString qualified as BS
import Data.Kind (Type)
import PlutusCore.Data qualified as PD
import PlutusTx.Builtins qualified as Builtins
import PlutusTx.Builtins.Internal qualified as BI
import Test.Tasty
import Test.Tasty.HUnit

import Plutarch.Core.Utils (pand'List)
import Plutarch.LedgerApi.Utils (PMaybeData (..))
import Plutarch.Prelude

import Midgard.CekData (
  PDataListNodeV1 (..),
  PDataNodeV1 (..),
  PDataPairNodeV1 (..),
  PDataSequenceSummaryV1 (..),
  PDataSummaryV1 (..),
  pbytesDataCborLengthV1,
  pbytesDataSummaryV1,
  pdataNodeCborLengthV1,
  pdataNodeChildRootsV1,
  pdataNodeMemoryV1,
  pdataListNodeChildRootsV1,
  pdataPairNodeChildRootsV1,
  pemptyDataListRootV1,
  pemptyDataListSummaryV1,
  pemptyDataPairRootV1,
  pemptyDataPairSummaryV1,
  pencodeDataListNodePreimageV1,
  pencodeDataNodePreimageV1,
  pencodeDataPairNodePreimageV1,
  phashDataListNodeV1,
  phashDataNodeV1,
  phashDataPairNodeV1,
  phashSemanticDataV1,
  pinspectDataListNodePreimageV1,
  pinspectDataNodePreimageV1,
  pinspectDataPairNodePreimageV1,
  pintegerDataSummaryV1,
  plargeConstrDataSummaryFromCborV1,
  plargeConstrDataSummaryV1,
  plistDataSummaryV1,
  pmapDataSummaryV1,
  pprependDataListSummaryV1,
  pprependDataPairSummaryV1,
  psemanticDataSummaryV1,
  psmallConstrDataSummaryV1,
  pverifyDataListLinkV1,
  pverifyDataNodeV1,
 )
import Testing.BoundedItem (arrayHeader, blake2b256, definiteBytes)
import Testing.Eval (passertEval, pfails)

--------------------------------------------------------------------------------
-- The suite
--------------------------------------------------------------------------------

tests :: TestTree
tests =
  testGroup
    "Midgard.CekData"
    [ encodingTests
    , encodingRefusalTests
    , accessorTests
    , lengthArithmeticTests
    , integerSizingTests
    , aikenParityTests
    , inspectionTests
    , decliningTests
    , summaryTests
    , semanticTests
    ]

--------------------------------------------------------------------------------
-- Fixtures
--------------------------------------------------------------------------------

rootA, rootB, rootC :: BS.ByteString
rootA = BS.replicate 32 0xa1
rootB = BS.replicate 32 0xb2
rootC = BS.replicate 32 0xc3

-- | One node of each of the six shapes, with distinguishable numbers.
smallNode, largeNode, mapNode, listNode, integerNode, bytesNode :: RefNode
smallNode = ConstrSmall 3 2 rootA 41 17
largeNode = ConstrLarge rootB 5 9 2 rootA 61 23
mapNode = MapNode 4 rootA 55 29
listNode = ListNode 4 rootA 57 31
integerNode = IntegerNode rootA 9 12
bytesNode = BytesNode rootA 200 202 204

everyNode :: [(String, RefNode)]
everyNode =
  [ ("a small constructor", smallNode)
  , ("a large constructor", largeNode)
  , ("a map", mapNode)
  , ("a list", listNode)
  , ("an integer", integerNode)
  , ("a byte string", bytesNode)
  ]

listLink :: RefListNode
listLink = RefListNode rootA 7 11 rootB 3 40 60

pairLink :: RefPairNode
pairLink = RefPairNode rootA 7 11 rootB 13 17 rootC 3 70 90

--------------------------------------------------------------------------------
-- The preimage encoding
--------------------------------------------------------------------------------

encodingTests :: TestTree
encodingTests =
  testGroup
    "the preimage encoding"
    [ testGroup
        "each node shape encodes as the format says"
        [ testCase name $
            passertEval $
              pencodeDataNodePreimageV1 # nodeTerm node #== pconstant (encodeNode node)
        | (name, node) <- everyNode
        ]
    , testGroup
        "…and hashes under the node domain"
        [ testCase name $
            passertEval $
              phashDataNodeV1 # nodeTerm node #== pconstant (hashNode node)
        | (name, node) <- everyNode
        ]
    , testCase "an item link encodes as the format says" $
        passertEval $
          pencodeDataListNodePreimageV1 # listNodeTerm listLink
            #== pconstant (encodeListNode listLink)
    , testCase "…and hashes under the list domain" $
        passertEval $
          phashDataListNodeV1 # listNodeTerm listLink #== pconstant (hashListNode listLink)
    , testCase "an entry link encodes as the format says" $
        passertEval $
          pencodeDataPairNodePreimageV1 # pairNodeTerm pairLink
            #== pconstant (encodePairNode pairLink)
    , testCase "…and hashes under the pair domain" $
        passertEval $
          phashDataPairNodeV1 # pairNodeTerm pairLink #== pconstant (hashPairNode pairLink)
    , testCase "the empty item sequence is a bare 0x80 under its own domain" $
        passertEval $
          pemptyDataListRootV1 #== pconstant (blake2b256 (listDomain <> "\x80"))
    , testCase "the empty entry sequence likewise" $
        passertEval $
          pemptyDataPairRootV1 #== pconstant (blake2b256 (pairDomain <> "\x80"))
    , testCase "and the three domains are distinct, so no preimage crosses over" $
        passertEval $
          pand'List
            [ pemptyDataListRootV1 #/= pemptyDataPairRootV1
            , pemptyDataListRootV1 #/= pconstant (blake2b256 (nodeDomain <> "\x80"))
            ]
    ]

--------------------------------------------------------------------------------
-- The ranges the encoder enforces
--------------------------------------------------------------------------------

encodingRefusalTests :: TestTree
encodingRefusalTests =
  testGroup
    "the encoding refuses"
    [ testCase "a small constructor above 127" $
        pfails $ pencodeDataNodePreimageV1 # nodeTerm (ConstrSmall 128 0 rootA 1 4)
    , testCase "a negative constructor" $
        pfails $ pencodeDataNodePreimageV1 # nodeTerm (ConstrSmall (-1) 0 rootA 1 4)
    , testCase "a field count past uint32" $
        pfails $ pencodeDataNodePreimageV1 # nodeTerm (ConstrSmall 0 4294967296 rootA 1 4)
    , testCase "a fields root that is not 32 bytes" $
        pfails $ pencodeDataNodePreimageV1 # nodeTerm (ConstrSmall 0 0 (BS.replicate 31 1) 1 4)
    , testCase "a negative cbor length" $
        pfails $ pencodeDataNodePreimageV1 # nodeTerm (ConstrSmall 0 0 rootA (-1) 4)
    , testCase "a memory past uint64" $
        pfails $
          pencodeDataNodePreimageV1
            # nodeTerm (ConstrSmall 0 0 rootA 1 18446744073709551616)
    , testCase "a large constructor's root that is not 32 bytes" $
        pfails $
          pencodeDataNodePreimageV1 # nodeTerm (ConstrLarge (BS.replicate 33 1) 5 9 0 rootA 1 4)
    , testCase "a byte node whose cbor length passes uint32" $
        pfails $ pencodeDataNodePreimageV1 # nodeTerm (BytesNode rootA 1 4294967296 4)
    , testCase "an item link with a short head" $
        pfails $
          pencodeDataListNodePreimageV1 # listNodeTerm listLink {rlHead = BS.replicate 31 1}
    , testCase "an item link whose head cbor length passes uint32" $
        pfails $
          pencodeDataListNodePreimageV1 # listNodeTerm listLink {rlHeadCborLength = 4294967296}
    , testCase "an entry link with a short value" $
        pfails $
          pencodeDataPairNodePreimageV1 # pairNodeTerm pairLink {rpValue = BS.replicate 31 1}
    , testCase "an entry link with a negative length" $
        pfails $
          pencodeDataPairNodePreimageV1 # pairNodeTerm pairLink {rpLength = -1}
    ]

--------------------------------------------------------------------------------
-- The accessors
--------------------------------------------------------------------------------

accessorTests :: TestTree
accessorTests =
  testGroup
    "the accessors"
    [ testGroup
        "cbor length, at a different position in every shape"
        [ testCase name $
            passertEval $
              pdataNodeCborLengthV1 # nodeTerm node #== pconstant (nodeCborLength node)
        | (name, node) <- everyNode
        ]
    , testGroup
        "memory, likewise"
        [ testCase name $
            passertEval $
              pdataNodeMemoryV1 # nodeTerm node #== pconstant (nodeMemory node)
        | (name, node) <- everyNode
        ]
    , testGroup
        "the child roots a proof must open"
        [ testCase name $
            passertEval $ rootsMatch (pdataNodeChildRootsV1 # nodeTerm node) (nodeChildRoots node)
        | (name, node) <- everyNode
        ]
    , testCase "an item link opens two" $
        passertEval $
          rootsMatch (pdataListNodeChildRootsV1 # listNodeTerm listLink) [rlHead listLink, rlTail listLink]
    , testCase "an entry link opens three" $
        passertEval $
          rootsMatch
            (pdataPairNodeChildRootsV1 # pairNodeTerm pairLink)
            [rpKey pairLink, rpValue pairLink, rpTail pairLink]
    ]

--------------------------------------------------------------------------------
-- The length arithmetic, against serialiseData itself
--------------------------------------------------------------------------------

{- | Sample payloads whose serialisations span every header width the arithmetic
branches on.
-}
sampleItems :: [[PD.Data]]
sampleItems =
  [ []
  , [PD.I 0]
  , [PD.I 1, PD.I 2, PD.I 3]
  , replicate 23 (PD.I 7)
  , replicate 24 (PD.I 7)
  , replicate 40 (PD.B "abc")
  , replicate 300 (PD.I 0)
  ]

lengthArithmeticTests :: TestTree
lengthArithmeticTests =
  testGroup
    "the length arithmetic agrees with serialiseData"
    [ testGroup
        "a list's cbor length"
        [ testCase (show (length items) <> " items") $
            serialisedLength (PD.List items)
              @?= listCborLength (fromIntegral (length items)) (payloadLength items)
        | items <- sampleItems
        ]
    , testGroup
        "a map's cbor length"
        [ testCase (show (length items) <> " entries") $
            serialisedLength (PD.Map (map (\x -> (x, x)) items))
              @?= mapCborLength (fromIntegral (length items)) (2 * payloadLength items)
        | items <- sampleItems
        ]
    , testGroup
        "a byte string's cbor length, across the 64-byte chunk boundary"
        [ testCase (show n <> " bytes") $
            passertEval $
              pbytesDataCborLengthV1 # pconstant (fromIntegral n)
                #== pconstant (serialisedLength (PD.B (BS.replicate n 0x5a)))
        | n <- [0, 1, 23, 24, 63, 64, 65, 127, 128, 129, 191, 192, 193, 255, 256, 1000, 4095]
        ]
    , testCase "the byte length past uint32 aborts" $
        pfails $ pbytesDataCborLengthV1 # pconstant 4294967296
    , testCase "a negative byte length aborts" $
        pfails $ pbytesDataCborLengthV1 # pconstant (-1)
    ]

headerLength :: Int -> Integer
headerLength n
  | n < 24 = 1
  | n <= 255 = 2
  | n <= 65535 = 3
  | otherwise = 5

payloadLength :: [PD.Data] -> Integer
payloadLength = sum . map serialisedLength

serialisedLength :: PD.Data -> Integer
serialisedLength = fromIntegral . BS.length . ser

--------------------------------------------------------------------------------
-- Integer sizing
--------------------------------------------------------------------------------

integerSizingTests :: TestTree
integerSizingTests =
  testGroup
    "an integer node's memory"
    [ testCase (show n) $
        passertEval $
          summaryMatches
            (pintegerDataSummaryV1 # pconstant n # pconstant rootA)
            (integerSummary n rootA)
    | n <-
        [ 0
        , 1
        , -1
        , 127
        , 128
        , -128
        , 255
        , 256
        , 65535
        , 65536
        , 9223372036854775807
        , -9223372036854775808
        , 340282366920938463463374607431768211456
        ]
    ]

--------------------------------------------------------------------------------
-- The five tests in cek-data-v1.test.ak
--------------------------------------------------------------------------------

aikenParityTests :: TestTree
aikenParityTests =
  testGroup
    "the Aiken test vectors"
    [ testCase "canonical_signed_integer_boundaries_match_the_evidence_constructor" $
        passertEval $
          pand'List
            [ pserialiseData # pconstant (PD.I 18446744073709551615)
                #== phexByteStr "1bffffffffffffffff"
            , pserialiseData # pconstant (PD.I 18446744073709551616)
                #== phexByteStr "c249010000000000000000"
            , pserialiseData # pconstant (PD.I (-18446744073709551616))
                #== phexByteStr "3bffffffffffffffff"
            , pserialiseData # pconstant (PD.I (-18446744073709551617))
                #== phexByteStr "c349010000000000000000"
            ]
    , testCase "offchain_semantic_data_root_vector" $
        passertEval offchainSemanticDataRootVector
    , testCase "semantic_data_summary_drift_is_rejected" $
        passertEval $
          pnot
            #$ pverifyDataNodeV1
              # driftedAikenBytesNode
              # pcon PDNothing
              # pcon PDNothing
    , testCase "data_material_preimage_inspectors_reject_noncanonical_and_extract_edges" $
        passertEval aikenInspectorVector
    , testCase "offchain_ordered_map_root_vector" $
        passertEval $
          pmatch
            ( psemanticDataSummaryV1
                # pconstant
                  ( PD.Map
                      [ (PD.B "\x11", PD.I 1)
                      , (PD.B "", PD.I 2)
                      ]
                  )
            )
            $ \(PDataSummaryV1 root cborLength memory) ->
              pand'List
                [ pfromData root
                    #== phexByteStr "96cf66f3acdee22a9661894a6fdae1deb78e5b434338a19cdc23df2d73fabb51"
                , pfromData cborLength #== 6
                , pfromData memory #== 24
                ]
    ]

offchainSemanticDataRootVector :: forall (s :: S). Term s PBool
offchainSemanticDataRootVector =
  pand'List
    [ phashDataNodeV1 # aikenBytesNode
        #== phexByteStr "8d961cac2f0554e64621a1874febaaedcc89a40bb882f2ea869725ee57505191"
    , phashDataNodeV1 # aikenIntegerNode
        #== phexByteStr "045b6d8572fcb30c6ac06398f3dda84638c1f44e43df6acce9ec954870dfdc37"
    , phashDataListNodeV1 # aikenLastField
        #== phexByteStr "aa85e721cfa08a22e6590847a80f8e24d890e88f7d539ba1601c5fad1cc492a9"
    , phashDataListNodeV1 # aikenFields
        #== phexByteStr "89ba3f57e131f81546f69405455db99d51a76825a7e41aa33a806e90bea03632"
    , phashDataNodeV1 # aikenRootNode
        #== phexByteStr "9ce9a6db13fa610a6efad613e5266cefe3740f2de1dd4a014884fa3f717d69de"
    , pverifyDataListLinkV1 # aikenLastField # aikenBytesNode # pcon PDNothing
    , pverifyDataListLinkV1
        # aikenFields
        # aikenIntegerNode
        # pcon (PDJust (pdata aikenLastField))
    , pverifyDataNodeV1
        # aikenRootNode
        # pcon (PDJust (pdata aikenFields))
        # pcon PDNothing
    ]

aikenInspectorVector :: forall (s :: S). Term s PBool
aikenInspectorVector =
  plet (pencodeDataNodePreimageV1 # aikenIntegerNode) $ \headPreimage ->
    plet (pencodeDataListNodePreimageV1 # aikenInspectorList) $ \listPreimage ->
      plet (pencodeDataPairNodePreimageV1 # aikenInspectorPair) $ \pairPreimage ->
        pand'List
          [ pinspectDataNodePreimageV1 # headPreimage #== pcon (PJust aikenIntegerNode)
          , pinspectDataListNodePreimageV1 # listPreimage #== pcon (PJust aikenInspectorList)
          , pinspectDataPairNodePreimageV1 # pairPreimage #== pcon (PJust aikenInspectorPair)
          , pdataNodeChildRootsV1 # aikenIntegerNode
              #== (pcons # pdata aikenIntegerCborRoot # pcon PNil)
          , pdataListNodeChildRootsV1 # aikenInspectorList
              #== ( pcons
                      # pdata (phashDataNodeV1 # aikenIntegerNode)
                      # (pcons # pdata pemptyDataListRootV1 # pcon PNil)
                  )
          , pdataPairNodeChildRootsV1 # aikenInspectorPair
              #== ( pcons
                      # pdata (phashDataNodeV1 # aikenIntegerNode)
                      # ( pcons
                            # pdata (phashDataNodeV1 # aikenIntegerNode)
                            # (pcons # pdata pemptyDataPairRootV1 # pcon PNil)
                        )
                  )
          , pinspectDataNodePreimageV1 # (headPreimage <> phexByteStr "00")
              #== pcon PNothing
          ]

aikenBytesNode, driftedAikenBytesNode, aikenIntegerNode, aikenRootNode ::
  forall (s :: S). Term s PDataNodeV1
aikenBytesNode =
  pcon $
    PBytesDataNode
      (pdata $ phexByteStr "27bffb36b7607480b9af8e0abd0a28042d953fb5791792e2c0e9c28015cad8f1")
      (pdata 65)
      (pdata 70)
      (pdata 69)
driftedAikenBytesNode =
  pcon $
    PBytesDataNode
      (pdata $ phexByteStr "27bffb36b7607480b9af8e0abd0a28042d953fb5791792e2c0e9c28015cad8f1")
      (pdata 65)
      (pdata 69)
      (pdata 69)
aikenIntegerNode =
  pcon $
    PIntegerDataNode
      (pdata aikenIntegerCborRoot)
      (pdata 1)
      (pdata 5)
aikenRootNode =
  pcon $
    PConstrLargeData
      (pdata $ phexByteStr "3ba6e86f178af94b2662ab108e98320a100ccd6b2c517f0eee2ab72a2c562fcf")
      (pdata 2)
      (pdata 6)
      (pdata 2)
      (pdata $ phashDataListNodeV1 # aikenFields)
      (pdata 78)
      (pdata 78)

aikenLastField, aikenFields, aikenInspectorList ::
  forall (s :: S). Term s PDataListNodeV1
aikenLastField =
  pcon $
    PDataListNodeV1
      (pdata $ phashDataNodeV1 # aikenBytesNode)
      (pdata 70)
      (pdata 69)
      (pdata pemptyDataListRootV1)
      (pdata 1)
      (pdata 70)
      (pdata 69)
aikenFields =
  pcon $
    PDataListNodeV1
      (pdata $ phashDataNodeV1 # aikenIntegerNode)
      (pdata 1)
      (pdata 5)
      (pdata $ phashDataListNodeV1 # aikenLastField)
      (pdata 2)
      (pdata 71)
      (pdata 74)
aikenInspectorList =
  pcon $
    PDataListNodeV1
      (pdata $ phashDataNodeV1 # aikenIntegerNode)
      (pdata 1)
      (pdata 5)
      (pdata pemptyDataListRootV1)
      (pdata 1)
      (pdata 1)
      (pdata 5)

aikenInspectorPair :: forall (s :: S). Term s PDataPairNodeV1
aikenInspectorPair =
  pcon $
    PDataPairNodeV1
      (pdata $ phashDataNodeV1 # aikenIntegerNode)
      (pdata 1)
      (pdata 5)
      (pdata $ phashDataNodeV1 # aikenIntegerNode)
      (pdata 1)
      (pdata 5)
      (pdata pemptyDataPairRootV1)
      (pdata 1)
      (pdata 2)
      (pdata 10)

aikenIntegerCborRoot :: forall (s :: S). Term s PByteString
aikenIntegerCborRoot =
  phexByteStr "e727db15461df0c95814206c4df3ca7397b8bdeab92da97eb2e87141dd3cd178"

--------------------------------------------------------------------------------
-- Inspection
--------------------------------------------------------------------------------

inspectionTests :: TestTree
inspectionTests =
  testGroup
    "inspection round-trips a canonical preimage"
    [ testGroup
        "every node shape but one"
        [ testCase name $ passertEval $ inspectsTo (encodeNode node) node
        | (name, node) <- everyNode
        , name /= "a byte string"
        ]
    , testCase "a byte node does not, and cannot: the Aiken agrees" $
        passertEval $ declines (encodeNode bytesNode)
    , testCase "a small constructor at the top of its range" $
        passertEval $ inspectsTo (encodeNode n) n
    , testCase "a large constructor with a zero field count" $
        passertEval $ inspectsTo (encodeNode m) m
    , testCase "an item link" $
        passertEval $
          pmatch (pinspectDataListNodePreimageV1 # pconstant (encodeListNode listLink)) $ \case
            PNothing -> pconstant @PBool False
            PJust node -> phashDataListNodeV1 # node #== pconstant (hashListNode listLink)
    , testCase "an entry link" $
        passertEval $
          pmatch (pinspectDataPairNodePreimageV1 # pconstant (encodePairNode pairLink)) $ \case
            PNothing -> pconstant @PBool False
            PJust node -> phashDataPairNodeV1 # node #== pconstant (hashPairNode pairLink)
    ]
  where
    n = ConstrSmall 127 4294967295 rootA 18446744073709551615 18446744073709551615
    m = ConstrLarge rootB 4294967295 1 0 rootA 2 3

--------------------------------------------------------------------------------
-- What inspection turns away
--------------------------------------------------------------------------------

decliningTests :: TestTree
decliningTests =
  testGroup
    "inspection declines"
    [ testCase "an empty preimage" $ passertEval $ declines ""
    , testCase "bytes that are not CBOR at all" $ passertEval $ declines "\xff\xff"
    , testCase "a preimage with a byte left over" $
        passertEval $ declines (encodeNode smallNode <> "\x00")
    , testCase "a truncated preimage" $
        passertEval $ declines (BS.init (encodeNode smallNode))
    , testCase "an arity no shape uses" $
        passertEval $
          declines (BS.concat [arrayHeader 3, cborI 0, cborI 1, cborI 2])
    , testCase "a small-constructor body under an integer kind that is not 0" $
        passertEval $
          declines $
            BS.concat [arrayHeader 6, cborI 9, cborI 3, cborI 2, definiteBytes rootA, cborI 41, cborI 17]
    , testCase "a large-constructor body under a kind that is not 1" $
        passertEval $
          declines $
            BS.concat
              [ arrayHeader 8
              , cborI 0
              , definiteBytes rootB
              , cborI 5
              , cborI 9
              , cborI 2
              , definiteBytes rootA
              , cborI 61
              , cborI 23
              ]
    , testCase "a counted body under kind 4, which is the integer arity" $
        passertEval $
          declines $
            BS.concat [arrayHeader 5, cborI 4, cborI 4, definiteBytes rootA, cborI 55, cborI 29]
    , testCase "an integer body under kind 2, which is the map kind" $
        passertEval $
          declines $ BS.concat [arrayHeader 4, cborI 2, definiteBytes rootA, cborI 9, cborI 12]
    , testCase "a constructor index of 128 in the small shape" $
        passertEval $
          declines $
            BS.concat [arrayHeader 6, cborI 0, cborI 128, cborI 2, definiteBytes rootA, cborI 41, cborI 17]
    , testCase "a root that is 31 bytes" $
        passertEval $
          declines $
            BS.concat
              [arrayHeader 6, cborI 0, cborI 3, cborI 2, definiteBytes (BS.replicate 31 1), cborI 41, cborI 17]
    , testCase "a field count one past uint32" $
        passertEval $
          declines $
            BS.concat [arrayHeader 6, cborI 0, cborI 3, cborI 4294967296, definiteBytes rootA, cborI 41, cborI 17]
    , testCase "a byte node whose cbor length one past uint32" $
        passertEval $
          declines $
            BS.concat [arrayHeader 5, cborI 5, definiteBytes rootA, cborI 200, cborI 4294967296, cborI 204]
    , testCase "a field where an integer was expected" $
        passertEval $
          declines $
            BS.concat [arrayHeader 6, cborI 0, definiteBytes rootB, cborI 2, definiteBytes rootA, cborI 41, cborI 17]
    , testCase "a field where bytes were expected" $
        passertEval $
          declines $
            BS.concat [arrayHeader 6, cborI 0, cborI 3, cborI 2, cborI 99, cborI 41, cborI 17]
    , testGroup
        "…and the non-canonical forms, which only the re-encoding catches"
        [ testCase "an indefinite-length outer array" $
            passertEval $
              declines $
                BS.concat
                  ["\x9f", cborI 0, cborI 3, cborI 2, definiteBytes rootA, cborI 41, cborI 17, "\xff"]
        , testCase "a non-minimal integer header" $
            passertEval $
              declines $
                BS.concat
                  [arrayHeader 6, cborI 0, "\x18\x03", cborI 2, definiteBytes rootA, cborI 41, cborI 17]
        , testCase "a non-minimal byte-string header" $
            passertEval $
              declines $
                BS.concat
                  [ arrayHeader 6
                  , cborI 0
                  , cborI 3
                  , cborI 2
                  , "\x59\x00\x20" <> rootA
                  , cborI 41
                  , cborI 17
                  ]
        ]
    , testCase "an item link at the wrong arity" $
        passertEval $
          pmatch (pinspectDataListNodePreimageV1 # pconstant (encodeNode smallNode)) $ \case
            PNothing -> pconstant @PBool True
            PJust _ -> pconstant @PBool False
    , testCase "an item link of length zero, which has no link to be" $
        passertEval $
          pmatch
            ( pinspectDataListNodePreimageV1
                # pconstant (encodeListNode listLink {rlLength = 0})
            )
            $ \case
              PNothing -> pconstant @PBool True
              PJust _ -> pconstant @PBool False
    , testCase "an entry link of length zero, likewise" $
        passertEval $
          pmatch
            ( pinspectDataPairNodePreimageV1
                # pconstant (encodePairNode pairLink {rpLength = 0})
            )
            $ \case
              PNothing -> pconstant @PBool True
              PJust _ -> pconstant @PBool False
    , testCase "an entry link at the item link's arity" $
        passertEval $
          pmatch (pinspectDataPairNodePreimageV1 # pconstant (encodeListNode listLink)) $ \case
            PNothing -> pconstant @PBool True
            PJust _ -> pconstant @PBool False
    ]

--------------------------------------------------------------------------------
-- The summary constructors
--------------------------------------------------------------------------------

summaryTests :: TestTree
summaryTests =
  testGroup
    "the summary constructors"
    [ testCase "the empty item sequence" $
        passertEval $ seqMatches pemptyDataListSummaryV1 emptyListSeq
    , testCase "the empty entry sequence" $
        passertEval $ seqMatches pemptyDataPairSummaryV1 emptyPairSeq
    , testCase "prepending one item" $
        passertEval $
          seqMatches
            (pprependDataListSummaryV1 # summaryTerm sA # seqTerm emptyListSeq)
            (prependList sA emptyListSeq)
    , testCase "prepending three, which must fold right" $
        passertEval $
          seqMatches (threeItemsT) (prependList sA (prependList sB (prependList sC emptyListSeq)))
    , testCase "prepending one entry" $
        passertEval $
          seqMatches
            (pprependDataPairSummaryV1 # summaryTerm sA # summaryTerm sB # seqTerm emptyPairSeq)
            (prependPair sA sB emptyPairSeq)
    , testCase "a small constructor over a sequence" $
        passertEval $
          summaryMatches
            (psmallConstrDataSummaryV1 # pconstant 3 # seqTerm threeItems)
            (smallConstrSummary 3 threeItems)
    , testCase "…and at alternative 6, the last one-byte tag" $
        passertEval $
          summaryMatches
            (psmallConstrDataSummaryV1 # pconstant 6 # seqTerm threeItems)
            (smallConstrSummary 6 threeItems)
    , testCase "…and at alternative 7, where the tag grows a byte" $
        passertEval $
          summaryMatches
            (psmallConstrDataSummaryV1 # pconstant 7 # seqTerm threeItems)
            (smallConstrSummary 7 threeItems)
    , testCase "a small constructor above 127 aborts" $
        pfails $ psmallConstrDataSummaryV1 # pconstant 128 # seqTerm threeItems
    , testCase "a large constructor over a sequence" $
        passertEval $
          summaryMatches
            (plargeConstrDataSummaryV1 # pconstant 1000 # pconstant rootB # seqTerm threeItems)
            (largeConstrSummary 1000 rootB threeItems)
    , testCase "a large constructor at or below 127 aborts" $
        pfails $ plargeConstrDataSummaryV1 # pconstant 127 # pconstant rootB # seqTerm threeItems
    , testCase "the from-cbor form with a short root aborts" $
        pfails $
          plargeConstrDataSummaryFromCborV1
            # pconstant (BS.replicate 31 1)
            # pconstant 2
            # pconstant 6
            # seqTerm threeItems
    , testCase "…with a zero cbor length aborts" $
        pfails $
          plargeConstrDataSummaryFromCborV1 # pconstant rootB # pconstant 0 # pconstant 6 # seqTerm threeItems
    , testCase "…with a constructor memory below five aborts" $
        pfails $
          plargeConstrDataSummaryFromCborV1 # pconstant rootB # pconstant 2 # pconstant 4 # seqTerm threeItems
    , testCase "a list over a sequence" $
        passertEval $
          summaryMatches (plistDataSummaryV1 # seqTerm threeItems) (listSummary threeItems)
    , testCase "a map over a sequence" $
        passertEval $
          summaryMatches (pmapDataSummaryV1 # seqTerm threeEntries) (mapSummary threeEntries)
    , testCase "an integer leaf" $
        passertEval $
          summaryMatches (pintegerDataSummaryV1 # pconstant 4242 # pconstant rootA) (integerSummary 4242 rootA)
    , testCase "an integer leaf with a short root aborts" $
        pfails $ pintegerDataSummaryV1 # pconstant 1 # pconstant (BS.replicate 31 1)
    , testCase "a byte leaf" $
        passertEval $
          summaryMatches (pbytesDataSummaryV1 # pconstant 200 # pconstant rootA) (bytesSummary 200 rootA)
    , testCase "the empty byte leaf, which still costs one word" $
        passertEval $
          summaryMatches (pbytesDataSummaryV1 # pconstant 0 # pconstant rootA) (bytesSummary 0 rootA)
    , testCase "a negative byte length aborts" $
        pfails $ pbytesDataSummaryV1 # pconstant (-1) # pconstant rootA
    ]
  where
    sA = RefSummary rootA 3 12
    sB = RefSummary rootB 5 16
    sC = RefSummary rootC 7 20
    threeItems = prependList sA (prependList sB (prependList sC emptyListSeq))
    threeEntries = prependPair sA sB (prependPair sB sC emptyPairSeq)
    threeItemsT :: forall (s :: S). Term s PDataSequenceSummaryV1
    threeItemsT =
      pprependDataListSummaryV1
        # summaryTerm sA
        # ( pprependDataListSummaryV1
              # summaryTerm sB
              # (pprependDataListSummaryV1 # summaryTerm sC # seqTerm emptyListSeq)
          )

--------------------------------------------------------------------------------
-- The semantic commitment
--------------------------------------------------------------------------------

sampleValues :: [(String, PD.Data)]
sampleValues =
  [ ("zero", PD.I 0)
  , ("a small positive integer", PD.I 42)
  , ("a negative integer", PD.I (-9001))
  , ("an integer past 64 bits", PD.I 340282366920938463463374607431768211456)
  , ("the empty byte string", PD.B "")
  , ("a short byte string", PD.B "midgard")
  , ("a byte string across the 64-byte chunk boundary", PD.B (BS.replicate 65 0x11))
  , ("a 200-byte string", PD.B (BS.replicate 200 0x22))
  , ("the empty list", PD.List [])
  , ("a flat list", PD.List [PD.I 1, PD.I 2, PD.I 3])
  , ("a nested list", PD.List [PD.List [PD.I 1], PD.List []])
  , ("the empty map", PD.Map [])
  , ("a map", PD.Map [(PD.B "k", PD.I 1), (PD.B "j", PD.I 2)])
  , ("the empty constructor", PD.Constr 0 [])
  , ("a constructor with fields", PD.Constr 2 [PD.I 7, PD.B "x"])
  , ("a constructor at alternative 6", PD.Constr 6 [PD.I 1])
  , ("a constructor at alternative 7", PD.Constr 7 [PD.I 1])
  , ("a constructor at alternative 127", PD.Constr 127 [PD.I 1])
  , ("a constructor at alternative 128", PD.Constr 128 [PD.I 1])
  , ("a constructor at a large alternative", PD.Constr 100000 [PD.I 1])
  , ( "a value with all five shapes in it"
    , PD.Constr
        1
        [ PD.I (-5)
        , PD.B "abc"
        , PD.List [PD.Map [(PD.I 0, PD.B "")], PD.Constr 3 []]
        ]
    )
  ]

semanticTests :: TestTree
semanticTests =
  testGroup
    "the semantic commitment"
    [ testGroup
        "matches the reference"
        [ testCase name $
            passertEval $ phashSemanticDataV1 # pconstant value #== pconstant (sRoot (semantic value))
        | (name, value) <- sampleValues
        ]
    , testGroup
        "…including the two replay numbers"
        [ testCase name $
            passertEval $ summaryMatches (psemanticDataSummaryV1 # pconstant value) (semantic value)
        | (name, value) <- sampleValues
        ]
    , testCase "a list's commitment equals the list node built over its items" $
        passertEval $
          summaryMatches
            (plistDataSummaryV1 # seqTerm (commitList items))
            (semantic (PD.List items))
    , testCase "a map's commitment equals the map node built over its entries" $
        passertEval $
          summaryMatches
            (pmapDataSummaryV1 # seqTerm (commitPairs entries))
            (semantic (PD.Map entries))
    , testCase "a small constructor's commitment equals the node built over its fields" $
        passertEval $
          summaryMatches
            (psmallConstrDataSummaryV1 # pconstant 2 # seqTerm (commitList items))
            (semantic (PD.Constr 2 items))
    , testCase "a large constructor's commitment equals the node built over its fields" $
        passertEval $
          summaryMatches
            ( plargeConstrDataSummaryV1
                # pconstant 1000
                # pconstant (blobRoot (cborI 1000))
                # seqTerm (commitList items)
            )
            (semantic (PD.Constr 1000 items))
    , testCase "two values that differ only in shape do not collide" $
        passertEval $
          phashSemanticDataV1 # pconstant (PD.List [])
            #/= phashSemanticDataV1 # pconstant (PD.Map [])
    , testCase "…nor do a constructor with no fields and an empty list" $
        passertEval $
          phashSemanticDataV1 # pconstant (PD.Constr 0 [])
            #/= phashSemanticDataV1 # pconstant (PD.List [])
    ]
  where
    items = [PD.I 1, PD.B "two", PD.List [PD.I 3]]
    entries = [(PD.B "k", PD.I 1), (PD.I 2, PD.B "v")]

--------------------------------------------------------------------------------
-- Assertion helpers
--------------------------------------------------------------------------------

inspectsTo :: forall (s :: S). BS.ByteString -> RefNode -> Term s PBool
inspectsTo preimage node =
  pmatch (pinspectDataNodePreimageV1 # pconstant preimage) $ \case
    PNothing -> pconstant @PBool False
    PJust got ->
      pand'List
        [ phashDataNodeV1 # got #== pconstant (hashNode node)
        , pdataNodeCborLengthV1 # got #== pconstant (nodeCborLength node)
        , pdataNodeMemoryV1 # got #== pconstant (nodeMemory node)
        , rootsMatch (pdataNodeChildRootsV1 # got) (nodeChildRoots node)
        ]

declines :: forall (s :: S). BS.ByteString -> Term s PBool
declines preimage =
  pmatch (pinspectDataNodePreimageV1 # pconstant preimage) $ \case
    PNothing -> pconstant @PBool True
    PJust _ -> pconstant @PBool False

rootsMatch ::
  forall (s :: S).
  Term s (PBuiltinList (PAsData PByteString)) ->
  [BS.ByteString] ->
  Term s PBool
rootsMatch got expected =
  pand'List $
    (plength # got #== pconstant (fromIntegral (length expected)))
      : [ pfromData (pelemAt # pconstant (fromIntegral i) # got) #== pconstant root
        | (i, root) <- zip [0 :: Int ..] expected
        ]

summaryMatches :: forall (s :: S). Term s PDataSummaryV1 -> RefSummary -> Term s PBool
summaryMatches got (RefSummary root cborLength memory) =
  pmatch got $ \(PDataSummaryV1 gotRoot gotCborLength gotMemory) ->
    pand'List
      [ pfromData gotRoot #== pconstant root
      , pfromData gotCborLength #== pconstant cborLength
      , pfromData gotMemory #== pconstant memory
      ]

seqMatches :: forall (s :: S). Term s PDataSequenceSummaryV1 -> RefSeq -> Term s PBool
seqMatches got (RefSeq root len payload memory) =
  pmatch got $ \(PDataSequenceSummaryV1 gotRoot gotLength gotPayload gotMemory) ->
    pand'List
      [ pfromData gotRoot #== pconstant root
      , pfromData gotLength #== pconstant len
      , pfromData gotPayload #== pconstant payload
      , pfromData gotMemory #== pconstant memory
      ]

--------------------------------------------------------------------------------
-- Term builders
--------------------------------------------------------------------------------

nodeTerm :: forall (s :: S). RefNode -> Term s PDataNodeV1
nodeTerm = \case
  ConstrSmall c fc fr cl m ->
    pcon $
      PConstrSmallData (pd c) (pd fc) (pb fr) (pd cl) (pd m)
  ConstrLarge cr ccl cm fc fr cl m ->
    pcon $
      PConstrLargeData (pb cr) (pd ccl) (pd cm) (pd fc) (pb fr) (pd cl) (pd m)
  MapNode ec er cl m -> pcon $ PMapDataNode (pd ec) (pb er) (pd cl) (pd m)
  ListNode ic ir cl m -> pcon $ PListDataNode (pd ic) (pb ir) (pd cl) (pd m)
  IntegerNode cr cl m -> pcon $ PIntegerDataNode (pb cr) (pd cl) (pd m)
  BytesNode br bl cl m -> pcon $ PBytesDataNode (pb br) (pd bl) (pd cl) (pd m)

listNodeTerm :: forall (s :: S). RefListNode -> Term s PDataListNodeV1
listNodeTerm (RefListNode h hcl hm t l p m) =
  pcon $ PDataListNodeV1 (pb h) (pd hcl) (pd hm) (pb t) (pd l) (pd p) (pd m)

pairNodeTerm :: forall (s :: S). RefPairNode -> Term s PDataPairNodeV1
pairNodeTerm (RefPairNode k kcl km v vcl vm t l p m) =
  pcon $
    PDataPairNodeV1 (pb k) (pd kcl) (pd km) (pb v) (pd vcl) (pd vm) (pb t) (pd l) (pd p) (pd m)

summaryTerm :: forall (s :: S). RefSummary -> Term s PDataSummaryV1
summaryTerm (RefSummary r cl m) = pcon $ PDataSummaryV1 (pb r) (pd cl) (pd m)

seqTerm :: forall (s :: S). RefSeq -> Term s PDataSequenceSummaryV1
seqTerm (RefSeq r l p m) = pcon $ PDataSequenceSummaryV1 (pb r) (pd l) (pd p) (pd m)

pd :: forall (s :: S). Integer -> Term s (PAsData PInteger)
pd = pdata . pconstant

pb :: forall (s :: S). BS.ByteString -> Term s (PAsData PByteString)
pb = pdata . pconstant

--------------------------------------------------------------------------------
-- The reference: node shapes
--------------------------------------------------------------------------------

data RefNode
  = ConstrSmall Integer Integer BS.ByteString Integer Integer
  | ConstrLarge BS.ByteString Integer Integer Integer BS.ByteString Integer Integer
  | MapNode Integer BS.ByteString Integer Integer
  | ListNode Integer BS.ByteString Integer Integer
  | IntegerNode BS.ByteString Integer Integer
  | BytesNode BS.ByteString Integer Integer Integer
  deriving stock (Show, Eq)

data RefListNode = RefListNode
  { rlHead :: BS.ByteString
  , rlHeadCborLength :: Integer
  , rlHeadMemory :: Integer
  , rlTail :: BS.ByteString
  , rlLength :: Integer
  , rlPayloadCborLength :: Integer
  , rlMemory :: Integer
  }
  deriving stock (Show, Eq)

data RefPairNode = RefPairNode
  { rpKey :: BS.ByteString
  , rpKeyCborLength :: Integer
  , rpKeyMemory :: Integer
  , rpValue :: BS.ByteString
  , rpValueCborLength :: Integer
  , rpValueMemory :: Integer
  , rpTail :: BS.ByteString
  , rpLength :: Integer
  , rpPayloadCborLength :: Integer
  , rpMemory :: Integer
  }
  deriving stock (Show, Eq)

data RefSummary = RefSummary BS.ByteString Integer Integer
  deriving stock (Show, Eq)

data RefSeq = RefSeq BS.ByteString Integer Integer Integer
  deriving stock (Show, Eq)

sRoot :: RefSummary -> BS.ByteString
sRoot (RefSummary r _ _) = r

--------------------------------------------------------------------------------
-- The reference: encoding and hashing
--------------------------------------------------------------------------------

nodeDomain, listDomain, pairDomain :: BS.ByteString
nodeDomain = "MidgardCekDataNodeV1"
listDomain = "MidgardCekDataListNodeV1"
pairDomain = "MidgardCekDataPairNodeV1"

chunkDomain, branchDomain :: BS.ByteString
chunkDomain = "MidgardCekBlobChunkV1"
branchDomain = "MidgardCekBlobBranchV1"

encodeNode :: RefNode -> BS.ByteString
encodeNode = \case
  ConstrSmall c fc fr cl m ->
    BS.concat [arrayHeader 6, cborI 0, cborI c, cborI fc, definiteBytes fr, cborI cl, cborI m]
  ConstrLarge cr ccl cm fc fr cl m ->
    BS.concat
      [ arrayHeader 8
      , cborI 1
      , definiteBytes cr
      , cborI ccl
      , cborI cm
      , cborI fc
      , definiteBytes fr
      , cborI cl
      , cborI m
      ]
  MapNode ec er cl m ->
    BS.concat [arrayHeader 5, cborI 2, cborI ec, definiteBytes er, cborI cl, cborI m]
  ListNode ic ir cl m ->
    BS.concat [arrayHeader 5, cborI 3, cborI ic, definiteBytes ir, cborI cl, cborI m]
  IntegerNode cr cl m ->
    BS.concat [arrayHeader 4, cborI 4, definiteBytes cr, cborI cl, cborI m]
  BytesNode br bl cl m ->
    BS.concat [arrayHeader 5, cborI 5, definiteBytes br, cborI bl, cborI cl, cborI m]

encodeListNode :: RefListNode -> BS.ByteString
encodeListNode (RefListNode h hcl hm t l p m) =
  BS.concat
    [arrayHeader 7, definiteBytes h, cborI hcl, cborI hm, definiteBytes t, cborI l, cborI p, cborI m]

encodePairNode :: RefPairNode -> BS.ByteString
encodePairNode (RefPairNode k kcl km v vcl vm t l p m) =
  BS.concat
    [ arrayHeader 10
    , definiteBytes k
    , cborI kcl
    , cborI km
    , definiteBytes v
    , cborI vcl
    , cborI vm
    , definiteBytes t
    , cborI l
    , cborI p
    , cborI m
    ]

hashNode :: RefNode -> BS.ByteString
hashNode = blake2b256 . (nodeDomain <>) . encodeNode

hashListNode :: RefListNode -> BS.ByteString
hashListNode = blake2b256 . (listDomain <>) . encodeListNode

hashPairNode :: RefPairNode -> BS.ByteString
hashPairNode = blake2b256 . (pairDomain <>) . encodePairNode

--------------------------------------------------------------------------------
-- The reference: accessors
--------------------------------------------------------------------------------

nodeCborLength :: RefNode -> Integer
nodeCborLength = \case
  ConstrSmall _ _ _ cl _ -> cl
  ConstrLarge _ _ _ _ _ cl _ -> cl
  MapNode _ _ cl _ -> cl
  ListNode _ _ cl _ -> cl
  IntegerNode _ cl _ -> cl
  BytesNode _ _ cl _ -> cl

nodeMemory :: RefNode -> Integer
nodeMemory = \case
  ConstrSmall _ _ _ _ m -> m
  ConstrLarge _ _ _ _ _ _ m -> m
  MapNode _ _ _ m -> m
  ListNode _ _ _ m -> m
  IntegerNode _ _ m -> m
  BytesNode _ _ _ m -> m

nodeChildRoots :: RefNode -> [BS.ByteString]
nodeChildRoots = \case
  ConstrSmall _ _ fr _ _ -> [fr]
  ConstrLarge cr _ _ _ fr _ _ -> [cr, fr]
  MapNode _ er _ _ -> [er]
  ListNode _ ir _ _ -> [ir]
  IntegerNode cr _ _ -> [cr]
  BytesNode br _ _ _ -> [br]

--------------------------------------------------------------------------------
-- The reference: the bounded blob root
--------------------------------------------------------------------------------

maxChunk, maxBlob :: Int
maxChunk = 4095
maxBlob = 9215

hashBlobChunk :: BS.ByteString -> BS.ByteString
hashBlobChunk chunk
  | BS.length chunk > maxChunk = error "reference blob: chunk too long"
  | otherwise = blake2b256 (chunkDomain <> definiteBytesLong chunk)

hashBlobBranch :: BS.ByteString -> BS.ByteString -> Integer -> BS.ByteString
hashBlobBranch left right byteLength =
  blake2b256 $
    BS.concat [branchDomain, "\x83", definiteBytes left, definiteBytes right, cborI byteLength]

blobRoot :: BS.ByteString -> BS.ByteString
blobRoot bytes
  | len > maxBlob = error "reference blob: too long"
  | len <= maxChunk = hashBlobChunk bytes
  | remaining <= maxChunk = left
  | otherwise = hashBlobBranch left (hashBlobChunk third) (fromIntegral len)
  where
    len = BS.length bytes
    first = BS.take maxChunk bytes
    remaining = len - maxChunk
    secondLength = min remaining maxChunk
    second = BS.take secondLength (BS.drop maxChunk bytes)
    left =
      hashBlobBranch
        (hashBlobChunk first)
        (hashBlobChunk second)
        (fromIntegral (maxChunk + secondLength))
    third = BS.drop (maxChunk + secondLength) bytes

{- | 'definiteBytes' with the 4,095-byte chunk in range.

"Testing.BoundedItem" stops at 65,535, which covers every chunk, but its own
fixtures never reach two-byte headers; this spells the same rule out for the
sizes the blob root actually uses.
-}
definiteBytesLong :: BS.ByteString -> BS.ByteString
definiteBytesLong bytes
  | len <= 23 = BS.pack [fromIntegral (64 + len)] <> bytes
  | len <= 255 = BS.pack [0x58, fromIntegral len] <> bytes
  | otherwise =
      BS.pack [0x59, fromIntegral (len `div` 256), fromIntegral (len `mod` 256)] <> bytes
  where
    len = BS.length bytes

--------------------------------------------------------------------------------
-- The reference: lengths and sizes
--------------------------------------------------------------------------------

listCborLength :: Integer -> Integer -> Integer
listCborLength 0 _ = 1
listCborLength _ payload = 2 + payload

mapCborLength :: Integer -> Integer -> Integer
mapCborLength len payload = headerLength (fromIntegral len) + payload

unsignedByteSize :: Integer -> Integer
unsignedByteSize v
  | v < 256 = 1
  | otherwise = 1 + unsignedByteSize (v `div` 256)

integerMemorySize :: Integer -> Integer
integerMemorySize v = unsignedByteSize (if v < 0 then (negate v - 1) * 2 else v * 2)

bytesMemory :: Integer -> Integer
bytesMemory n = 4 + if n == 0 then 1 else n

--------------------------------------------------------------------------------
-- The reference: sequences and summaries
--------------------------------------------------------------------------------

emptyListSeq, emptyPairSeq :: RefSeq
emptyListSeq = RefSeq (blake2b256 (listDomain <> "\x80")) 0 0 0
emptyPairSeq = RefSeq (blake2b256 (pairDomain <> "\x80")) 0 0 0

prependList :: RefSummary -> RefSeq -> RefSeq
prependList (RefSummary hr hcl hm) (RefSeq tr tl tp tm) =
  RefSeq (hashListNode node) (tl + 1) (hcl + tp) (hm + tm)
  where
    node = RefListNode hr hcl hm tr (tl + 1) (hcl + tp) (hm + tm)

prependPair :: RefSummary -> RefSummary -> RefSeq -> RefSeq
prependPair (RefSummary kr kcl km) (RefSummary vr vcl vm) (RefSeq tr tl tp tm) =
  RefSeq (hashPairNode node) (tl + 1) (kcl + vcl + tp) (km + vm + tm)
  where
    node = RefPairNode kr kcl km vr vcl vm tr (tl + 1) (kcl + vcl + tp) (km + vm + tm)

summaryOf :: RefNode -> RefSummary
summaryOf node = RefSummary (hashNode node) (nodeCborLength node) (nodeMemory node)

smallConstrSummary :: Integer -> RefSeq -> RefSummary
smallConstrSummary constructor (RefSeq r l p m) =
  summaryOf $
    ConstrSmall constructor l r (if constructor <= 6 then 2 + fieldsLen else 3 + fieldsLen) (4 + m)
  where
    fieldsLen = listCborLength l p

largeConstrSummary :: Integer -> BS.ByteString -> RefSeq -> RefSummary
largeConstrSummary constructor cborRoot (RefSeq r l p m) =
  summaryOf $
    ConstrLarge
      cborRoot
      (fromIntegral (BS.length (cborI constructor)))
      (4 + integerMemorySize constructor)
      l
      r
      (3 + fromIntegral (BS.length (cborI constructor)) + listCborLength l p)
      (4 + m)

listSummary :: RefSeq -> RefSummary
listSummary (RefSeq r l p m) = summaryOf (ListNode l r (listCborLength l p) (4 + m))

mapSummary :: RefSeq -> RefSummary
mapSummary (RefSeq r l p m) = summaryOf (MapNode l r (mapCborLength l p) (4 + m))

integerSummary :: Integer -> BS.ByteString -> RefSummary
integerSummary n cborRoot =
  RefSummary (hashNode node) cborLength (nodeMemory node)
  where
    cborLength = fromIntegral (BS.length (cborI n))
    node = IntegerNode cborRoot cborLength (4 + integerMemorySize n)

bytesSummary :: Integer -> BS.ByteString -> RefSummary
bytesSummary n bytesRoot =
  summaryOf (BytesNode bytesRoot n (bytesDataCborLength n) (bytesMemory n))

{- | The reference for @bytes_data_cbor_length_v1@.

Not used to check the port — the group above checks that against @serialiseData@
directly — but needed here, where a summary's length field has to be predicted
without a byte string to serialise.
-}
bytesDataCborLength :: Integer -> Integer
bytesDataCborLength n
  | n <= 64 = headerLength (fromIntegral n) + n
  | otherwise =
      2
        + fullChunks * 66
        + (if remainder == 0 then 0 else headerLength (fromIntegral remainder) + remainder)
  where
    fullChunks = n `div` 64
    remainder = n `mod` 64

--------------------------------------------------------------------------------
-- The reference: the semantic commitment
--------------------------------------------------------------------------------

commitList :: [PD.Data] -> RefSeq
commitList = foldr (\d acc -> prependList (semantic d) acc) emptyListSeq

commitPairs :: [(PD.Data, PD.Data)] -> RefSeq
commitPairs = foldr (\(k, v) acc -> prependPair (semantic k) (semantic v) acc) emptyPairSeq

semantic :: PD.Data -> RefSummary
semantic value = RefSummary (hashNode node) cborLength (nodeMemory node)
  where
    cborLength = serialisedLength value
    node = case value of
      PD.Constr c fields ->
        let RefSeq r l _ m = commitList fields
            memory = 4 + m
         in if c <= 127
              then ConstrSmall c l r cborLength memory
              else
                let constructorCbor = cborI c
                 in ConstrLarge
                      (blobRoot constructorCbor)
                      (fromIntegral (BS.length constructorCbor))
                      (4 + integerMemorySize c)
                      l
                      r
                      cborLength
                      memory
      PD.Map entries ->
        let RefSeq r l _ m = commitPairs entries in MapNode l r cborLength (4 + m)
      PD.List xs ->
        let RefSeq r l _ m = commitList xs in ListNode l r cborLength (4 + m)
      PD.I n -> IntegerNode (blobRoot (ser value)) cborLength (4 + integerMemorySize n)
      PD.B b ->
        BytesNode
          (blobRoot b)
          (fromIntegral (BS.length b))
          cborLength
          (bytesMemory (fromIntegral (BS.length b)))

--------------------------------------------------------------------------------
-- Plumbing
--------------------------------------------------------------------------------

-- | Plutarch has no disequality operator; every module that wants one spells it.
(#/=) :: forall (s :: S) (a :: S -> Type). PEq a => Term s a -> Term s a -> Term s PBool
a #/= b = pnot # (a #== b)

infix 4 #/=

-- | @cbor.serialise@, which is @serialiseData@ on the value read as @Data@.
ser :: PD.Data -> BS.ByteString
ser = Builtins.fromBuiltin . Builtins.serialiseData . BI.BuiltinData

-- | @cbor.serialise@ of an integer, which is how every number in a preimage travels.
cborI :: Integer -> BS.ByteString
cborI = ser . PD.I
