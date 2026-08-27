{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Testing.CekDataFrame
Description : Behavioural tests for the Plutarch port of
              @lib/midgard/cek-data-frame-v1.ak@.

=== The test that matters is a whole traversal

A frame exists so that a proof can walk a @Data@ node one child at a time and
arrive at the /same root/ a whole-value commitment would produce. So the
headline group here does exactly that: it takes a real @Data@ value, builds the
frame for its top node, appends each child's summary, folds them back right to
left with a real Merkle membership path for each, finalises — and asserts the
result equals @semantic_data_summary_v1@ of the original value.

That single assertion spans "Midgard.CekData", "Midgard.ValidationMerkle" and
this module. If the fold order were reversed, if a child leaf omitted its index,
if a map's key and value were paired across entries, or if the sequence
summaries were built by appending rather than prepending, it would fail.

=== Two phases that must not overlap

A frame is filled and then folded. The tests pin both directions of that: no
child may be appended once folding has begun, and no child may be folded before
every one has arrived. Neither is a redundant check — they are what stop a proof
from committing to a node whose children it chose after the fact.

=== The reference is shared, deliberately

The frontier and membership paths come from "Testing.ValidationMerkle" and the
semantic summaries from "Testing.CekData". Both are independent references
already pinned against their own ports — and, through
"Testing.CekConstant", against five roots from a TypeScript implementation. A
third transcription of either would test nothing this module cares about. What
is written here is only what is specific to frames: the child leaf preimage, the
frame encoding, and the phase rules.
-}
module Testing.CekDataFrame (tests) where

import Data.ByteString qualified as BS
import PlutusCore.Data qualified as PD
import PlutusTx.Builtins qualified as Builtins
import PlutusTx.Builtins.Internal qualified as BI
import Test.Tasty
import Test.Tasty.HUnit

import Plutarch.Prelude

import Midgard.CekData (
  PDataSummaryV1 (..),
  PDataSequenceSummaryV1 (..),
  pemptyDataListSummaryV1,
  plargeConstrDataSummaryFromCborV1,
  pprependDataListSummaryV1,
 )
import Midgard.CekDataFrame (
  PDataFrameV1 (..),
  pappendChildV1,
  pchildLeafHashV1,
  pencodeFrameV1,
  pfinalizedSummaryV1,
  pfoldListChildV1,
  pfoldMapPairV1,
  pframeIsWellFormedV1,
  phashFrameV1,
  pinitialLargeConstrFrameV1,
  pinitialListFrameV1,
  pinitialMapFrameV1,
  pinitialSmallConstrFrameV1,
 )
import Testing.CekData (RefSummary (..), semantic)
import Testing.Eval (passertEval, pfails)
import Testing.ValidationMerkle (blake2b256, proofForLeaves, siblingsT)

--------------------------------------------------------------------------------
-- The suite
--------------------------------------------------------------------------------

tests :: TestTree
tests =
  testGroup
    "Midgard.CekDataFrame"
    [ aikenLargeConstructorTests
    , testCase "forged_or_unbounded_frames_fail_closed" $
        passertEval forgedOrUnboundedFramesFailClosed
    , traversalTests
    , childLeafTests
    , phaseTests
    , wellFormednessTests
    , encodingTests
    ]

--------------------------------------------------------------------------------
-- A whole traversal
--------------------------------------------------------------------------------

tailRoot :: BS.ByteString
tailRoot = BS.replicate 32 0xaa

--------------------------------------------------------------------------------
-- The missing large-constructor cases from cek-data-frame-v1.test.ak
--------------------------------------------------------------------------------

aikenLargeConstructorTests :: TestTree
aikenLargeConstructorTests =
  testGroup
    "the Aiken large-constructor vectors"
    [ testCase "large_constructor_frame_is_fixed_size_and_matches_typescript" $
        passertEval $
          plet aikenLargeConstructorFrame $ \frame ->
            (pencodeFrameV1 # frame #== pconstant aikenLargeConstructorFrameEncoding)
              #&& (plengthBS # pconstant aikenLargeConstructorFrameEncoding #< 256)
              #&& ( phashFrameV1 # frame
                      #== phexByteStr "f1f01b15e143b47b513a5be7c071a57709fa88b183a9220a9a81a1307b5334db"
                  )
    , testCase "folds_large_constructor_children_in_authenticated_reverse_order" $
        passertEval aikenLargeConstructorFold
    ]

aikenLargeConstructorFrame :: forall (s :: S). Term s PDataFrameV1
aikenLargeConstructorFrame =
  pinitialLargeConstrFrameV1
    # pconstant (BS.replicate 32 199)
    # 16_384
    # 16_388
    # pconstant (BS.replicate 32 66)
    # 2

aikenLargeConstructorFrameEncoding :: BS.ByteString
aikenLargeConstructorFrameEncoding =
  BS.pack [0x8b, 0x01, 0x00, 0x58, 0x20]
    <> BS.replicate 32 199
    <> BS.pack [0x19, 0x40, 0x00, 0x19, 0x40, 0x04, 0x58, 0x20]
    <> BS.replicate 32 66
    <> BS.pack
      [ 0x02
      , 0x00
      , 0x80
      , 0x00
      , 0x84
      , 0x58
      , 0x20
      , 0x8c
      , 0x44
      , 0x6a
      , 0x90
      , 0x3f
      , 0x12
      , 0x59
      , 0x39
      , 0xfd
      , 0x6e
      , 0x03
      , 0x6b
      , 0x31
      , 0x3c
      , 0x52
      , 0x34
      , 0x0c
      , 0x9a
      , 0xc0
      , 0x53
      , 0x9e
      , 0x67
      , 0x30
      , 0xf0
      , 0x8e
      , 0x95
      , 0xea
      , 0xec
      , 0x90
      , 0x52
      , 0xfa
      , 0x56
      , 0x00
      , 0x00
      , 0x00
      ]

aikenLargeConstructorFold :: forall (s :: S). Term s PBool
aikenLargeConstructorFold =
  plet (summaryT child0) $ \child0Term ->
    plet (summaryT child1) $ \child1Term ->
      plet (appendAll aikenLargeConstructorFrame [child0, child1]) $ \full ->
        plet (foldAll full [child0, child1]) $ \folded ->
          plet
            ( pprependDataListSummaryV1
                # child0Term
                # ( pprependDataListSummaryV1
                      # child1Term
                      # pemptyDataListSummaryV1
                  )
            )
            $ \fields ->
              pmatch (pfinalizedSummaryV1 # folded) $ \case
                PNothing -> pconstant False
                PJust actual ->
                  actual
                    #== ( plargeConstrDataSummaryFromCborV1
                            # pconstant (BS.replicate 32 199)
                            # 16_384
                            # 16_388
                            # fields
                        )
  where
    child0 = RefSummary (BS.replicate 32 17) 3 5
    child1 = RefSummary (BS.replicate 32 34) 67 68

forgedOrUnboundedFramesFailClosed :: forall (s :: S). Term s PBool
forgedOrUnboundedFramesFailClosed =
  plet (pinitialListFrameV1 # pconstant "" # 2) $ \initial ->
    pmatch initial $ \i ->
      pmatch (pfromData $ pframe'sequence i) $ \sequence ->
        plet
          ( pcon
              i
                { pframe'sequence =
                    pdata $
                      pcon sequence {pseq'root = pdata $ pconstant (BS.replicate 32 0xff)}
                }
          )
          $ \forgedEmpty ->
            plet
              (pcon i {pframe'expectedChildren = pdata 4_294_967_296})
              $ \unbounded ->
                pnot # (pframeIsWellFormedV1 # forgedEmpty)
                  #&& pnot # (pframeIsWellFormedV1 # unbounded)
                  #&& pnot
                    # ( isJust
                          (pappendChildV1 # initial # summaryT (RefSummary (BS.replicate 32 68) 0 4))
                      )
                  #&& noSummary (pfinalizedSummaryV1 # initial)

{- | Values whose top node is a constructor or a list, walked child by child.

Each is committed twice — once by this module's traversal and once by
@semantic_data_summary_v1@ — and the two roots must agree.
-}
listShapedValues :: [(String, PD.Data)]
listShapedValues =
  [ ("a constructor of two fields", PD.Constr 2 [PD.I 1, PD.B "x"])
  , ("a constructor of one field", PD.Constr 0 [PD.I 7])
  , ("a constructor at alternative 6", PD.Constr 6 [PD.I 1, PD.I 2, PD.I 3])
  , ("a constructor at alternative 7", PD.Constr 7 [PD.I 1])
  , ("a constructor at alternative 127", PD.Constr 127 [PD.I 1, PD.I 2])
  , ("a list of three", PD.List [PD.I 1, PD.I 2, PD.I 3])
  , ("a list of one", PD.List [PD.B "only"])
  , ("a list of five", PD.List [PD.I 1, PD.I 2, PD.I 3, PD.I 4, PD.I 5])
  , ("a list of nested values", PD.List [PD.List [PD.I 1], PD.Map [(PD.I 0, PD.I 1)]])
  , ("a constructor of nested values", PD.Constr 1 [PD.List [], PD.Constr 3 [PD.I 9]])
  ]

mapShapedValues :: [(String, [(PD.Data, PD.Data)])]
mapShapedValues =
  [ ("a map of one entry", [(PD.B "k", PD.I 1)])
  , ("a map of two entries", [(PD.B "k", PD.I 1), (PD.I 2, PD.B "v")])
  , ("a map of three entries", [(PD.I 1, PD.I 1), (PD.I 2, PD.I 2), (PD.I 3, PD.I 3)])
  , ("a map of nested values", [(PD.List [PD.I 1], PD.Map [(PD.I 0, PD.I 0)])])
  ]

traversalTests :: TestTree
traversalTests =
  testGroup
    "a traversal arrives at the whole-value root"
    [ testGroup
        "list-shaped nodes"
        [ testCase name $ passertEval $ traversalMatches value
        | (name, value) <- listShapedValues
        ]
    , testGroup
        "map-shaped nodes"
        [ testCase name $ passertEval $ mapTraversalMatches entries
        | (name, entries) <- mapShapedValues
        ]
    , testCase "an empty list needs no children and finalises straight away" $
        passertEval $
          summaryMatches
            (pfinalizedSummaryV1 # (pinitialListFrameV1 # pconstant tailRoot # pconstant 0))
            (semantic (PD.List []))
    , testCase "an empty map likewise" $
        passertEval $
          summaryMatches
            (pfinalizedSummaryV1 # (pinitialMapFrameV1 # pconstant tailRoot # pconstant 0))
            (semantic (PD.Map []))
    , testCase "an empty constructor likewise" $
        passertEval $
          summaryMatches
            ( pfinalizedSummaryV1
                #$ pinitialSmallConstrFrameV1 # pconstant 4 # pconstant tailRoot # pconstant 0
            )
            (semantic (PD.Constr 4 []))
    , testCase "a frame that has not been folded has no summary" $
        passertEval $
          noSummary $
            pfinalizedSummaryV1 # appendAll (frameFor value) (childrenOf value)
    , testCase "…nor has one whose children have not all arrived" $
        passertEval $
          noSummary $
            pfinalizedSummaryV1 # appendAll (frameFor value) (init (childrenOf value))
    ]
  where
    value = PD.Constr 2 [PD.I 1, PD.B "x"]

-- | Append every child, then fold every child, then finalise.
traversalMatches :: forall (s :: S). PD.Data -> Term s PBool
traversalMatches value =
  summaryMatches (pfinalizedSummaryV1 # foldAll (appendAll (frameFor value) kids) kids) (semantic value)
  where
    kids = childrenOf value

mapTraversalMatches :: forall (s :: S). [(PD.Data, PD.Data)] -> Term s PBool
mapTraversalMatches entries =
  summaryMatches
    (pfinalizedSummaryV1 # foldAllPairs (appendAll frame kids) entries)
    (semantic (PD.Map entries))
  where
    kids = concat [[semantic k, semantic v] | (k, v) <- entries]
    frame =
      pinitialMapFrameV1
        # pconstant tailRoot
        # pconstant (fromIntegral (length kids))

--------------------------------------------------------------------------------
-- The child leaf
--------------------------------------------------------------------------------

childLeafTests :: TestTree
childLeafTests =
  testGroup
    "the child leaf"
    [ testCase "hashes as the format says" $
        passertEval $
          pchildLeafHashV1 # pconstant 3 # summaryT s1 #== pconstant (childLeafHash 3 s1)
    , testCase "commits to the index, so the same child at two positions differs" $
        passertEval $
          pnot
            #$ (pchildLeafHashV1 # pconstant 0 # summaryT s1)
            #== (pchildLeafHashV1 # pconstant 1 # summaryT s1)
    , testCase "a negative index aborts" $
        pfails $ pchildLeafHashV1 # pconstant (-1) # summaryT s1
    , testCase "an index past uint32 aborts" $
        pfails $ pchildLeafHashV1 # pconstant 4294967296 # summaryT s1
    , testCase "a summary with a 31-byte root aborts" $
        pfails $ pchildLeafHashV1 # pconstant 0 # summaryT (RefSummary (BS.replicate 31 1) 5 9)
    , testCase "a summary of zero cbor length aborts: nothing serialises to nothing" $
        pfails $ pchildLeafHashV1 # pconstant 0 # summaryT (RefSummary root32 0 9)
    , testCase "a summary below four words of memory aborts" $
        pfails $ pchildLeafHashV1 # pconstant 0 # summaryT (RefSummary root32 5 3)
    , testCase "…and exactly four is accepted" $
        passertEval $
          pchildLeafHashV1 # pconstant 0 # summaryT (RefSummary root32 5 4)
            #== pconstant (childLeafHash 0 (RefSummary root32 5 4))
    ]
  where
    root32 = BS.replicate 32 0x11
    s1 = RefSummary root32 5 9

--------------------------------------------------------------------------------
-- The two phases
--------------------------------------------------------------------------------

phaseTests :: TestTree
phaseTests =
  testGroup
    "filling and folding do not overlap"
    [ testCase "a child may be appended while the cursor is zero" $
        passertEval $ isJust (pappendChildV1 # frame0 # summaryT (childAt 0))
    , testCase "no child may be appended once one has been folded" $
        passertEval $
          pnot #$ isJust (pappendChildV1 # foldOne # summaryT (childAt 0))
    , testCase "no more children than expected may be appended" $
        passertEval $ pnot #$ isJust (pappendChildV1 # filled # summaryT (childAt 0))
    , testCase "a malformed child summary is refused" $
        passertEval $
          pnot #$ isJust (pappendChildV1 # frame0 # summaryT (RefSummary root31 5 9))
    , testCase "no child may be folded before every one has arrived" $
        passertEval $
          pnot
            #$ isJust
              ( pfoldListChildV1
                  # (unsafeAppend frame0 (childAt 0))
                  # pconstant 1
                  # summaryT (childAt 1)
                  # siblingsT (pathFor 1)
              )
    , testCase "the fold runs right to left" $
        passertEval $ isJust (foldStep filled (length kids - 1))
    , testCase "…and an index other than the expected one is refused" $
        passertEval $ pnot #$ isJust (foldStep filled 0)
    , testCase "…nor may the same index be folded twice" $
        passertEval $
          pnot #$ isJust (foldStep foldOne (length kids - 1))
    , testCase "a membership path for the wrong child is refused" $
        passertEval $
          pnot
            #$ isJust
              ( pfoldListChildV1
                  # filled
                  # pconstant (fromIntegral (length kids - 1))
                  # summaryT (childAt 0)
                  # siblingsT (pathFor (fromIntegral (length kids - 1)))
              )
    , testCase "a corrupted membership path is refused" $
        passertEval $
          pnot #$ isJust (foldWithPath (map blake2b256 lastPath))
    , testCase "…and so is one with a sibling too many" $
        passertEval $
          pnot #$ isJust (foldWithPath (lastPath <> [BS.replicate 32 0x77]))
    , testCase "…or too few" $
        passertEval $ pnot #$ isJust (foldWithPath (drop 1 lastPath))
    , testCase "the last child's path is not empty, so those three test something" $
        assertBoolT (not (null lastPath))
    , testCase "a list frame refuses the map fold" $
        passertEval $
          pnot
            #$ isJustPair
              ( pfoldMapPairV1
                  # filled
                  # pconstant 0
                  # summaryT (childAt 0)
                  # summaryT (childAt 1)
                  # siblingsT (pathFor 0)
                  # siblingsT (pathFor 1)
              )
    , testCase "a map frame refuses the list fold" $
        passertEval $
          pnot
            #$ isJust
              ( pfoldListChildV1
                  # mapFilled
                  # pconstant 1
                  # summaryT (mapChildAt 1)
                  # siblingsT (mapPathFor 1)
              )
    , testCase "a map's key and value must come from the same entry" $
        passertEval $
          pnot
            #$ isJustPair
              ( pfoldMapPairV1
                  # mapFilled
                  # pconstant 1
                  # summaryT (mapChildAt 2)
                  # summaryT (mapChildAt 1)
                  # siblingsT (mapPathFor 2)
                  # siblingsT (mapPathFor 1)
              )
    ]
  where
    -- Four items, not three: with three, the last leaf sits alone in its own
    -- peak and its membership path is empty, so a corrupted path would be
    -- indistinguishable from the real one.
    value = PD.List [PD.I 1, PD.I 2, PD.I 3, PD.I 4]
    kids = childrenOf value
    lastPath = pathFor (fromIntegral (length kids - 1))
    foldWithPath path =
      pfoldListChildV1
        # filled
        # pconstant (fromIntegral (length kids - 1))
        # summaryT (childAt (length kids - 1))
        # siblingsT path
    childAt i = kids !! i
    pathFor = proofForLeaves [childLeafHash (fromIntegral i) k | (i, k) <- zip [0 :: Int ..] kids]
    frame0 = frameFor value
    filled = appendAll frame0 kids
    foldOne = unsafeFold filled (length kids - 1) kids
    foldStep f i =
      pfoldListChildV1
        # f
        # pconstant (fromIntegral i)
        # summaryT (childAt i)
        # siblingsT (pathFor (fromIntegral i))
    root31 = BS.replicate 31 0x11

    mapEntries = [(PD.B "k", PD.I 1), (PD.I 2, PD.B "v")]
    mapKids = concat [[semantic k, semantic v] | (k, v) <- mapEntries]
    mapChildAt i = mapKids !! i
    mapPathFor =
      proofForLeaves [childLeafHash (fromIntegral i) k | (i, k) <- zip [0 :: Int ..] mapKids]
    mapFilled =
      appendAll
        (pinitialMapFrameV1 # pconstant tailRoot # pconstant (fromIntegral (length mapKids)))
        mapKids

--------------------------------------------------------------------------------
-- Well-formedness
--------------------------------------------------------------------------------

wellFormednessTests :: TestTree
wellFormednessTests =
  testGroup
    "a frame is well formed only when"
    [ testCase "it is a fresh list frame" $
        passertEval $
          pframeIsWellFormedV1 # (pinitialListFrameV1 # pconstant tailRoot # pconstant 3)
    , testCase "it is a fresh map frame with an even child count" $
        passertEval $
          pframeIsWellFormedV1 # (pinitialMapFrameV1 # pconstant tailRoot # pconstant 4)
    , testCase "a map frame with an odd child count aborts at construction" $
        pfails $ pinitialMapFrameV1 # pconstant tailRoot # pconstant 3
    , testCase "a small constructor above 127 aborts at construction" $
        pfails $
          pinitialSmallConstrFrameV1 # pconstant 128 # pconstant tailRoot # pconstant 1
    , testCase "a negative constructor aborts at construction" $
        pfails $
          pinitialSmallConstrFrameV1 # pconstant (-1) # pconstant tailRoot # pconstant 1
    , testCase "a tail that is neither empty nor 32 bytes aborts" $
        pfails $
          pinitialListFrameV1 # pconstant (BS.replicate 31 0x01) # pconstant 1
    , testCase "an empty tail is allowed: it is the bottom of the stack" $
        passertEval $ pframeIsWellFormedV1 # (pinitialListFrameV1 # pconstant "" # pconstant 1)
    , testCase "an expected-child count past uint32 aborts" $
        pfails $ pinitialListFrameV1 # pconstant tailRoot # pconstant 4294967296
    , testCase "a negative expected-child count aborts" $
        pfails $ pinitialListFrameV1 # pconstant tailRoot # pconstant (-1)
    ]

--------------------------------------------------------------------------------
-- Encoding
--------------------------------------------------------------------------------

encodingTests :: TestTree
encodingTests =
  testGroup
    "encoding"
    [ testCase "a fresh list frame encodes as the format says" $
        passertEval $
          pencodeFrameV1 # (pinitialListFrameV1 # pconstant tailRoot # pconstant 2)
            #== pconstant (encodeFreshFrame 2 tailRoot 2 emptyListSequence)
    , testCase "a fresh map frame" $
        passertEval $
          pencodeFrameV1 # (pinitialMapFrameV1 # pconstant tailRoot # pconstant 2)
            #== pconstant (encodeFreshFrame 3 tailRoot 2 emptyPairSequence)
    , testCase "…and the two differ, because their empty sequences do" $
        passertEval $
          pnot
            #$ (phashFrameV1 # (pinitialListFrameV1 # pconstant tailRoot # pconstant 2))
            #== (phashFrameV1 # (pinitialMapFrameV1 # pconstant tailRoot # pconstant 2))
    , testCase "a fresh small-constructor frame" $
        passertEval $
          pencodeFrameV1
            # (pinitialSmallConstrFrameV1 # pconstant 5 # pconstant tailRoot # pconstant 2)
            #== pconstant (encodeSmallConstrFrame 5 tailRoot 2)
    , testCase "the frame hash is the encoding under the frame domain" $
        passertEval $
          phashFrameV1 # (pinitialListFrameV1 # pconstant tailRoot # pconstant 2)
            #== pconstant
              (blake2b256 (frameDomain <> encodeFreshFrame 2 tailRoot 2 emptyListSequence))
    , testCase "two frames differing only in their tail are different hashes" $
        passertEval $
          pnot
            #$ (phashFrameV1 # (pinitialListFrameV1 # pconstant tailRoot # pconstant 2))
            #== (phashFrameV1 # (pinitialListFrameV1 # pconstant "" # pconstant 2))
    ]

--------------------------------------------------------------------------------
-- Driving the port
--------------------------------------------------------------------------------

frameFor :: forall (s :: S). PD.Data -> Term s PDataFrameV1
frameFor value = case value of
  PD.Constr tag fields
    | tag <= 127 ->
        pinitialSmallConstrFrameV1
          # pconstant tag
          # pconstant tailRoot
          # pconstant (fromIntegral (length fields))
  PD.List items ->
    pinitialListFrameV1 # pconstant tailRoot # pconstant (fromIntegral (length items))
  _ -> error "frameFor: only small constructors and lists"

childrenOf :: PD.Data -> [RefSummary]
childrenOf = \case
  PD.Constr _ fields -> map semantic fields
  PD.List items -> map semantic items
  _ -> error "childrenOf: only constructors and lists"

appendAll :: forall (s :: S). Term s PDataFrameV1 -> [RefSummary] -> Term s PDataFrameV1
appendAll = foldl unsafeAppend

unsafeAppend ::
  forall (s :: S). Term s PDataFrameV1 -> RefSummary -> Term s PDataFrameV1
unsafeAppend frame child =
  pmatch (pappendChildV1 # frame # summaryT child) $ \case
    PNothing -> perror
    PJust next -> next

-- | Fold every child, right to left, with a real membership path for each.
foldAll :: forall (s :: S). Term s PDataFrameV1 -> [RefSummary] -> Term s PDataFrameV1
foldAll frame kids =
  foldl (\f i -> unsafeFold f i kids) frame (reverse [0 .. length kids - 1])

unsafeFold ::
  forall (s :: S). Term s PDataFrameV1 -> Int -> [RefSummary] -> Term s PDataFrameV1
unsafeFold frame i kids =
  pmatch
    ( pfoldListChildV1
        # frame
        # pconstant (fromIntegral i)
        # summaryT (kids !! i)
        # siblingsT (pathOver kids (fromIntegral i))
    )
    $ \case
      PNothing -> perror
      PJust next -> next

foldAllPairs ::
  forall (s :: S). Term s PDataFrameV1 -> [(PD.Data, PD.Data)] -> Term s PDataFrameV1
foldAllPairs frame entries =
  foldl step frame (reverse [0 .. length entries - 1])
  where
    kids = concat [[semantic k, semantic v] | (k, v) <- entries]
    step f i =
      pmatch
        ( pfoldMapPairV1
            # f
            # pconstant (fromIntegral i)
            # summaryT (kids !! (2 * i))
            # summaryT (kids !! (2 * i + 1))
            # siblingsT (pathOver kids (fromIntegral (2 * i)))
            # siblingsT (pathOver kids (fromIntegral (2 * i + 1)))
        )
        $ \case
          PNothing -> perror
          PJust next -> next

pathOver :: [RefSummary] -> Integer -> [BS.ByteString]
pathOver kids =
  proofForLeaves [childLeafHash (fromIntegral i) k | (i, k) <- zip [0 :: Int ..] kids]

summaryT :: forall (s :: S). RefSummary -> Term s PDataSummaryV1
summaryT (RefSummary root cborLength memory) =
  pcon $
    PDataSummaryV1
      (pdata (pconstant root))
      (pdata (pconstant cborLength))
      (pdata (pconstant memory))

summaryMatches ::
  forall (s :: S). Term s (PMaybe PDataSummaryV1) -> RefSummary -> Term s PBool
summaryMatches got expected =
  pmatch got $ \case
    PNothing -> pconstant @PBool False
    PJust summary -> summary #== summaryT expected

noSummary :: forall (s :: S). Term s (PMaybe PDataSummaryV1) -> Term s PBool
noSummary t = pmatch t $ \case
  PNothing -> pconstant @PBool True
  PJust _ -> pconstant @PBool False

isJust :: forall (s :: S). Term s (PMaybe PDataFrameV1) -> Term s PBool
isJust t = pmatch t $ \case
  PNothing -> pconstant @PBool False
  PJust _ -> pconstant @PBool True

isJustPair :: forall (s :: S). Term s (PMaybe PDataFrameV1) -> Term s PBool
isJustPair = isJust

--------------------------------------------------------------------------------
-- The reference: what is specific to frames
--------------------------------------------------------------------------------

frameDomain, childDomain :: BS.ByteString
frameDomain = "MidgardCekDataFrameV1"
childDomain = "MidgardCekDataFrameChildV1"

childLeafHash :: Integer -> RefSummary -> BS.ByteString
childLeafHash childIndex (RefSummary root cborLength memory) =
  blake2b256 $
    BS.concat
      [ childDomain
      , arrayHeader 4
      , cborInt childIndex
      , definiteBytes root
      , cborInt cborLength
      , cborInt memory
      ]

-- | The sequence summary a fresh frame carries, as four encoded fields.
emptyListSequence, emptyPairSequence :: (BS.ByteString, Integer, Integer, Integer)
emptyListSequence = (blake2b256 ("MidgardCekDataListNodeV1" <> "\x80"), 0, 0, 0)
emptyPairSequence = (blake2b256 ("MidgardCekDataPairNodeV1" <> "\x80"), 0, 0, 0)

encodeSequence :: (BS.ByteString, Integer, Integer, Integer) -> BS.ByteString
encodeSequence (root, len, payload, memory) =
  BS.concat
    [arrayHeader 4, definiteBytes root, cborInt len, cborInt payload, cborInt memory]

{- | A frame that has had nothing appended: empty frontier, zero cursor.

Only the fresh shape is written out. Every later shape is reached by driving the
port itself, and is checked by the traversal group rather than by a second
transcription of the encoder.
-}
encodeFreshFrame ::
  Integer ->
  BS.ByteString ->
  Integer ->
  (BS.ByteString, Integer, Integer, Integer) ->
  BS.ByteString
encodeFreshFrame kind tail expectedChildren sequence =
  BS.concat
    [ arrayHeader 11
    , cborInt kind
    , cborInt 0
    , definiteBytes ""
    , cborInt 0
    , cborInt 0
    , definiteBytes tail
    , cborInt expectedChildren
    , cborInt 0
    , arrayHeader 0
    , cborInt 0
    , encodeSequence sequence
    ]

encodeSmallConstrFrame :: Integer -> BS.ByteString -> Integer -> BS.ByteString
encodeSmallConstrFrame constructor tail expectedChildren =
  BS.concat
    [ arrayHeader 11
    , cborInt 0
    , cborInt constructor
    , definiteBytes ""
    , cborInt 0
    , cborInt 0
    , definiteBytes tail
    , cborInt expectedChildren
    , cborInt 0
    , arrayHeader 0
    , cborInt 0
    , encodeSequence emptyListSequence
    ]

--------------------------------------------------------------------------------
-- Plumbing
--------------------------------------------------------------------------------

ser :: PD.Data -> BS.ByteString
ser = Builtins.fromBuiltin . Builtins.serialiseData . BI.BuiltinData

cborInt :: Integer -> BS.ByteString
cborInt = ser . PD.I

arrayHeader :: Int -> BS.ByteString
arrayHeader n
  | n <= 23 = BS.pack [fromIntegral (128 + n)]
  | n <= 255 = BS.pack [0x98, fromIntegral n]
  | otherwise = error "reference arrayHeader: out of fixture range"

definiteBytes :: BS.ByteString -> BS.ByteString
definiteBytes bytes
  | len <= 23 = BS.pack [fromIntegral (64 + len)] <> bytes
  | len <= 255 = BS.pack [0x58, fromIntegral len] <> bytes
  | otherwise = error "reference definiteBytes: out of fixture range"
  where
    len = BS.length bytes

-- | An ordinary HUnit assertion, phrased so it reads beside the Plutarch ones.
assertBoolT :: Bool -> Assertion
assertBoolT = assertBool "expected to hold"
