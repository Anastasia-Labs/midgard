{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Testing.MpfProofFold
Description : Behavioural tests for the Plutarch port of
              @lib/midgard/mpf-proof-fold-v1.ak@.

An MPF proof folded one frame per transaction. Two things need testing and they
need different oracles.

__The frame preimage__ is a byte format, so it gets a Haskell encoder written from
the Aiken module and a hash comparison. That catches a field in the wrong order or
a missing kind tag, neither of which the fold itself would notice — every frame
would simply hash consistently wrongly.

__The fold__ is arithmetic over a real trie, so a reference would only re-derive
the port's own assumptions. Instead the whole golden fixture from
@mpf-proof-fold-v1.test.ak@ is reused: four frames of a proof against a trie built
by the TypeScript implementation, the four leaf hashes it produces, and the two
roots the completed fold must reach. Nothing in it was computed by this repository
at all, which is exactly why it is worth having.

=== The fold runs backwards, and the tests do too

Frames are folded from @frame_count - 1@ down to @0@. The sequence below therefore
starts with the fourth frame and ends with the first, and each step's siblings are
the frontier path for /that/ frame. Offering them in any other order is refused,
and one test does exactly that.
-}
module Testing.MpfProofFold (tests) where

import Data.ByteString qualified as BS
import Data.ByteString.Base16 qualified as Base16
import Data.Kind (Type)
import Test.Tasty
import Test.Tasty.HUnit

import Plutarch.Prelude

import Midgard.MpfProofFold (
  PProofDescriptorV1 (..),
  PProofFoldControlV1 (..),
  PProofFrameV1 (..),
  pdescriptorIsWellFormedV1,
  pencodeProofDescriptorV1,
  pfoldIsCompleteV1,
  pfoldProofFrameV1,
  pframeIsWellFormed,
  pinitialFoldControlV1,
  pproofFrameLeafHashV1,
 )
import Midgard.MpfProof.Types (PNeighbor (..), PProofStep (..))
import Midgard.ValidationMerkle (PFrontierPeak (..))
import Testing.BoundedItem (arrayHeader, blake2b256, cborInt, definiteBytes)
import Testing.Eval (passertEval, pfails)

--------------------------------------------------------------------------------
-- The suite
--------------------------------------------------------------------------------

tests :: TestTree
tests =
  testGroup
    "MPF Proof Fold Tests"
    [ testGroup "Aiken parity" aikenParityTests
    , testGroup "the frame preimage" preimageTests
    , testGroup "the golden fold" goldenTests
    , testGroup "descriptors" descriptorTests
    , testGroup "frames" frameTests
    , testGroup "the fold fails closed" failureTests
    ]

-- | Direct ports of the three tests in @mpf-proof-fold-v1.test.ak@.
aikenParityTests :: [TestTree]
aikenParityTests =
  [ testCase "proof_frame_encoding_matches_typescript" $
      holds $
        pall'
          [ (pproofFrameLeafHashV1 # frame) #== pconstant expected
          | (frame, expected) <-
              [ (firstFrame, firstLeaf)
              , (secondFrame, secondLeaf)
              , (thirdFrame, thirdLeaf)
              , (fourthFrame, fourthLeaf)
              ]
          ]
  , testCase "proof_fold_matches_real_typescript_trie_roots" $
      holds $
        (pfoldIsCompleteV1 # goldenFold)
          #&& ((excludingRootOf goldenFold) #== pconstant goldenExcludingRoot)
          #&& ((includingRootOf goldenFold) #== pconstant goldenIncludingRoot)
  , testCase "malformed_or_reordered_frame_fails_closed" $
      isNothing $ foldOne initialControl thirdFrame []
  ]

--------------------------------------------------------------------------------
-- The frame preimage
--------------------------------------------------------------------------------

{- | Every step kind, against an encoder written from the Aiken module.

The kind tag and the item count are both part of the preimage, and the three
kinds declare 7, 9 and 8 items — so a branch and a leaf carrying the same payload
still hash apart. The fork case is the one the golden fixture does not reach.
-}
preimageTests :: [TestTree]
preimageTests =
  [ testCase "hashes a branch frame as the reference does" $
      holds $
        (pproofFrameLeafHashV1 # frameT 0 0 1 (branchStep 0 firstNeighbors))
          #== pconstant (frameHash 0 0 1 (RefBranch 0 firstNeighbors))
  , testCase "…a fork frame" $
      holds $
        (pproofFrameLeafHashV1 # frameT 1 3 5 (forkStep 1 7 forkPrefix forkRoot))
          #== pconstant (frameHash 1 3 5 (RefFork 1 7 forkPrefix forkRoot))
  , testCase "…and a leaf frame" $
      holds $
        (pproofFrameLeafHashV1 # frameT 2 2 3 (leafStep 0 thirdKey thirdValue))
          #== pconstant (frameHash 2 2 3 (RefLeaf 0 thirdKey thirdValue))
  , -- Each coordinate moved once. A preimage that dropped one would still be
    -- self-consistent, so this is the only thing that catches it.
    testCase "the frame index is inside the hash" $
      differ (frameT 0 0 1 (branchStep 0 firstNeighbors)) (frameT 1 0 1 (branchStep 0 firstNeighbors))
  , testCase "…as is the cursor" $
      differ (frameT 0 0 1 (branchStep 0 firstNeighbors)) (frameT 0 1 2 (branchStep 0 firstNeighbors))
  , testCase "…and the step kind, at equal payloads" $
      differ (frameT 0 0 1 (leafStep 0 thirdKey thirdValue)) (frameT 0 0 1 (leafStep 0 thirdValue thirdKey))
  ]

-- | Two frames whose leaf hashes must not coincide.
differ ::
  (forall (s :: S). Term s PProofFrameV1) ->
  (forall (s :: S). Term s PProofFrameV1) ->
  Assertion
differ a b = holds $ pnot #$ (pproofFrameLeafHashV1 # a) #== (pproofFrameLeafHashV1 # b)

--------------------------------------------------------------------------------
-- The golden fold
--------------------------------------------------------------------------------

{- | The fixture from @mpf-proof-fold-v1.test.ak@, folded end to end.

Key @"absent-252"@ against a four-frame proof over a trie the TypeScript
implementation built. The two roots at the end are that implementation's, not
this one's.
-}
goldenTests :: [TestTree]
goldenTests =
  [ testCase "the four frames hash to the fixture's leaves" $
      holds $
        pall'
          [ (pproofFrameLeafHashV1 # frame) #== pconstant expected
          | (frame, expected) <-
              [ (firstFrame, firstLeaf)
              , (secondFrame, secondLeaf)
              , (thirdFrame, thirdLeaf)
              , (fourthFrame, fourthLeaf)
              ]
          ]
  , testCase "the descriptor is well formed" $
      holds $ pdescriptorIsWellFormedV1 # descriptorT
  , testCase "the completed fold reaches the fixture's including root" $
      holds $ (includingRootOf goldenFold) #== pconstant goldenIncludingRoot
  , testCase "…and its excluding root" $
      holds $ (excludingRootOf goldenFold) #== pconstant goldenExcludingRoot
  , testCase "…and it is complete" $
      holds $ pfoldIsCompleteV1 # goldenFold
  , -- The intermediate state is checked too: a fold that landed on the right
    -- roots by accident would still have the wrong cursor on the way.
    testCase "the initial control starts at the last frame" $
      holds $
        (nextFrameIndexOf (expectJust (pinitialFoldControlV1 # pconstant goldenKey # pconstant goldenValue # descriptorT)) #== 3)
          #&& (expectedCursorOf (expectJust (pinitialFoldControlV1 # pconstant goldenKey # pconstant goldenValue # descriptorT)) #== 4)
  , testCase "…and the completed one at minus one and cursor zero" $
      holds $ (nextFrameIndexOf goldenFold #== -1) #&& (expectedCursorOf goldenFold #== 0)
  ]

-- | The four frames folded in order, from the last to the first.
goldenFold :: forall (s :: S). Term s PProofFoldControlV1
goldenFold =
  foldl
    step
    (expectJust (pinitialFoldControlV1 # pconstant goldenKey # pconstant goldenValue # descriptorT))
    [ (fourthFrame, [thirdLeaf, firstTwoRoot])
    , (thirdFrame, [fourthLeaf, firstTwoRoot])
    , (secondFrame, [firstLeaf, lastTwoRoot])
    , (firstFrame, [secondLeaf, lastTwoRoot])
    ]
  where
    step control (frame, siblings) = expectJust (foldOne control frame siblings)

foldOne ::
  forall (s :: S).
  Term s PProofFoldControlV1 ->
  Term s PProofFrameV1 ->
  [BS.ByteString] ->
  Term s (PMaybe PProofFoldControlV1)
foldOne = foldOneUnder goldenKey

foldOneUnder ::
  forall (s :: S).
  BS.ByteString ->
  Term s PProofFoldControlV1 ->
  Term s PProofFrameV1 ->
  [BS.ByteString] ->
  Term s (PMaybe PProofFoldControlV1)
foldOneUnder key control frame siblings =
  pfoldProofFrameV1
    # pconstant key
    # descriptorT
    # control
    # frame
    # byteList siblings

--------------------------------------------------------------------------------
-- Descriptors
--------------------------------------------------------------------------------

descriptorTests :: [TestTree]
descriptorTests =
  [ testCase "the empty descriptor is well formed" $
      holds $ pdescriptorIsWellFormedV1 # descriptorOf 1 0 0 []
  , {- The clause tying the two counters together. Without it a descriptor could
       claim frames and a terminal cursor of zero, and the completeness check
       would accept a fold that never consumed them. -}
    testCase "…but not with a terminal cursor it cannot have reached" $
      refuses $ pdescriptorIsWellFormedV1 # descriptorOf 1 0 1 []
  , testCase "a descriptor with frames must end past the root" $
      refuses $ pdescriptorIsWellFormedV1 # descriptorOf 1 4 0 [(2, peakHash)]
  , testCase "a version that is not this one is refused" $
      refuses $ pdescriptorIsWellFormedV1 # descriptorOf 2 4 4 [(2, peakHash)]
  , testCase "a negative frame count is refused" $
      refuses $ pdescriptorIsWellFormedV1 # descriptorOf 1 (-1) 4 [(2, peakHash)]
  , testCase "a frame count past the path length is refused" $
      refuses $ pdescriptorIsWellFormedV1 # descriptorOf 1 65 4 [(2, peakHash)]
  , testCase "a terminal cursor past the path length is refused" $
      refuses $ pdescriptorIsWellFormedV1 # descriptorOf 1 4 65 [(2, peakHash)]
  , testCase "peaks that do not match the frame count are refused" $
      refuses $ pdescriptorIsWellFormedV1 # descriptorOf 1 4 4 [(1, peakHash)]
  , testCase "encoding a well-formed descriptor is the header, three ints and the frontier" $
      holds $
        (pencodeProofDescriptorV1 # descriptorT)
          #== pconstant
            ( BS.concat
                [ arrayHeader 4
                , cborInt 1
                , cborInt 4
                , cborInt 4
                , arrayHeader 1
                , "\x82" <> cborInt 2 <> definiteBytes peakHash
                ]
            )
  , -- An `expect` in the original, so this aborts rather than refusing.
    testCase "encoding a malformed descriptor aborts" $
      pfails $ pencodeProofDescriptorV1 # descriptorOf 1 4 0 [(2, peakHash)]
  ]

--------------------------------------------------------------------------------
-- Frames
--------------------------------------------------------------------------------

{- | The per-frame guards, against the golden key's path.

@next_cursor == cursor + 1 + skip@ is what keeps the nibble reads total, and the
two inequality clauses are what make a step a step: a fork's neighbour and a
leaf's key have to branch away from the path here.
-}
frameTests :: [TestTree]
frameTests =
  [ testCase "the fixture's first frame is well formed" $
      holds $ pframeIsWellFormed # pconstant goldenPath # firstFrame
  , testCase "…and its third, a leaf that branches away" $
      holds $ pframeIsWellFormed # pconstant goldenPath # thirdFrame
  , testCase "a cursor pair that does not account for the skip is refused" $
      refuses $ pframeIsWellFormed # pconstant goldenPath # frameT 0 0 3 (branchStep 0 firstNeighbors)
  , testCase "…and one that overshoots the path is refused" $
      refuses $ pframeIsWellFormed # pconstant goldenPath # frameT 0 63 65 (branchStep 0 firstNeighbors)
  , testCase "a version that is not this one is refused" $
      refuses $
        pframeIsWellFormed
          # pconstant goldenPath
          # frameWith 2 0 0 1 (branchStep 0 firstNeighbors)
  , testCase "a branch whose neighbours are not four digests is refused" $
      refuses $
        pframeIsWellFormed # pconstant goldenPath # frameT 0 0 1 (branchStep 0 (BS.take 96 firstNeighbors))
  , testCase "a leaf whose key is not a digest is refused" $
      refuses $
        pframeIsWellFormed
          # pconstant goldenPath
          # frameT 2 2 3 (leafStep 0 (BS.take 31 thirdKey) thirdValue)
  , {- The leaf that does *not* branch away. Its key agrees with the path at the
       nibble this step consumes, so the step claims a divergence that is not
       there — which would let a prover fold a leaf that is on the path. -}
    testCase "a leaf agreeing with the path at this nibble is refused" $
      refuses $ pframeIsWellFormed # pconstant goldenPath # frameT 2 2 3 (leafStep 0 goldenPath thirdValue)
  , testCase "a fork whose neighbour nibble is the path's is refused" $
      refuses $
        pframeIsWellFormed
          # pconstant goldenPath
          # frameT 0 0 1 (forkStep 0 (pathNibble 0) forkPrefix forkRoot)
  , testCase "…and one whose neighbour root is not a digest is refused" $
      refuses $
        pframeIsWellFormed
          # pconstant goldenPath
          # frameT 0 0 1 (forkStep 0 (otherNibble 0) forkPrefix (BS.take 31 forkRoot))
  ]

--------------------------------------------------------------------------------
-- Failing closed
--------------------------------------------------------------------------------

{- | Everything that must return nothing rather than abort.

The fold is a step in a multi-transaction protocol, so a prover who supplies the
wrong frame has to get no new control state — not a failed script that leaves the
thread stuck.
-}
failureTests :: [TestTree]
failureTests =
  [ testCase "a frame offered out of order is refused" $
      isNothing $ foldOne initialControl thirdFrame []
  , testCase "…even with its own siblings" $
      isNothing $ foldOne initialControl thirdFrame [fourthLeaf, firstTwoRoot]
  , testCase "the right frame with the wrong siblings is refused" $
      isNothing $ foldOne initialControl fourthFrame [firstLeaf, lastTwoRoot]
  , testCase "…and with no siblings at all" $
      isNothing $ foldOne initialControl fourthFrame []
  , testCase "a frame whose next cursor is not the expected one is refused" $
      isNothing $
        foldOne initialControl (frameWith 1 3 2 4 (branchStep 0 firstNeighbors)) [thirdLeaf, firstTwoRoot]
  , {- The key is *not* bound by a fold step, and it would be wrong to assert
       that it is. `path` enters only the well-formedness nibble checks and the
       root arithmetic, so folding the same frame under another key can succeed
       — and lands on a different root. What ties a proof to its key is the
       caller comparing the *completed* root against the trie's, not this
       function. -}
    testCase "a fold under a different key reaches a different root" $
      holds $
        pnot
          #$ includingRootOf (expectJust (foldOneUnder otherKey initialControl fourthFrame [thirdLeaf, firstTwoRoot]))
          #== includingRootOf (expectJust (foldOne initialControl fourthFrame [thirdLeaf, firstTwoRoot]))
  , testCase "a control whose including root is not a digest is refused" $
      isNothing $
        pfoldProofFrameV1
          # pconstant goldenKey
          # descriptorT
          # controlOf 3 4 (BS.take 31 goldenIncludingRoot) nullHash
          # fourthFrame
          # byteList [thirdLeaf, firstTwoRoot]
  , testCase "a control past the end of the descriptor is refused" $
      isNothing $
        pfoldProofFrameV1
          # pconstant goldenKey
          # descriptorT
          # controlOf 4 4 goldenIncludingRoot nullHash
          # fourthFrame
          # byteList [thirdLeaf, firstTwoRoot]
  , testCase "…and one already below zero" $
      isNothing $
        pfoldProofFrameV1
          # pconstant goldenKey
          # descriptorT
          # controlOf (-1) 4 goldenIncludingRoot nullHash
          # fourthFrame
          # byteList [thirdLeaf, firstTwoRoot]
  , testCase "the initial control of a malformed descriptor is refused" $
      isNothing $
        pinitialFoldControlV1 # pconstant goldenKey # pconstant goldenValue # descriptorOf 1 4 0 [(2, peakHash)]
  , testCase "an incomplete fold is not complete" $
      refuses $ pfoldIsCompleteV1 # initialControl
  , testCase "…nor is one whose roots are the wrong width" $
      refuses $ pfoldIsCompleteV1 # controlOf (-1) 0 (BS.take 31 goldenIncludingRoot) nullHash
  ]

--------------------------------------------------------------------------------
-- The frame preimage, reimplemented from mpf-proof-fold-v1.ak
--------------------------------------------------------------------------------

data RefStep
  = RefBranch Integer BS.ByteString
  | RefFork Integer Integer BS.ByteString BS.ByteString
  | RefLeaf Integer BS.ByteString BS.ByteString

frameHash :: Integer -> Integer -> Integer -> RefStep -> BS.ByteString
frameHash frameIndex cursor nextCursor step =
  blake2b256 ("MidgardMpfProofFrameV1" <> encodeFrame frameIndex cursor nextCursor step)

encodeFrame :: Integer -> Integer -> Integer -> RefStep -> BS.ByteString
encodeFrame frameIndex cursor nextCursor step = case step of
  RefBranch skip neighbors ->
    prefixOf 0 7 <> cborInt skip <> definiteBytes neighbors
  RefFork skip nibbleValue prefix root ->
    prefixOf 1 9
      <> cborInt skip
      <> cborInt nibbleValue
      <> definiteBytes prefix
      <> definiteBytes root
  RefLeaf skip key value ->
    prefixOf 2 8 <> cborInt skip <> definiteBytes key <> definiteBytes value
  where
    prefixOf kind itemCount =
      arrayHeader itemCount
        <> cborInt 1
        <> cborInt frameIndex
        <> cborInt cursor
        <> cborInt nextCursor
        <> cborInt kind

--------------------------------------------------------------------------------
-- The golden fixture
--------------------------------------------------------------------------------

firstNeighbors, secondNeighbors :: BS.ByteString
firstNeighbors =
  hex
    "fcb3a266af3765b1034ed5b3aa4b4207a3326fe6c8d819833330688f2c789576\
    \f78acc03c7a5dfb5cfa9fea66cf3fee9122c7674c71ada7cb7fa0a4d63e3628f\
    \acf6bc2599db2c5d6d299a15d5721ae11ee7fd7249ae8617b57509a74f2119bd\
    \756f6f99694666e954705f5a7b02b7750498dbeb4b889fb344e1d9bc59a9ed10"
secondNeighbors =
  hex
    "235b143ef050a1b6bbea33803c7077088871d2ecc2f098591c4c34c7616ccb86\
    \c9f40a6f0e8c1d1017d7527c6efe204e479e449a5fed9a88311c2998c7635296\
    \9ccc9dbea8cf853d71d324c571a99f08a9434bb3da9018c068e04e5c3f51841b\
    \0000000000000000000000000000000000000000000000000000000000000000"

thirdKey, thirdValue, fourthKey, fourthValue :: BS.ByteString
thirdKey = hex "38c04ff63fbdbb3fc408a19f3831335596c6d145008c4ef6c089f43eeffc114f"
thirdValue = hex "7dec4f4f919b8eb0fa49dc74dc302781e288ef3e558fb6f97e314d5850269a84"
fourthKey = hex "38d8f7fe941c2b69bd72d0a30a310a3f1d207043e5afabf6e07baeb3f927ea6c"
fourthValue = hex "3a9cba86616d4ef370f645ed0f512dd5c2065f8d2de8417d1d4706c67896e62f"

firstLeaf, secondLeaf, thirdLeaf, fourthLeaf :: BS.ByteString
firstLeaf = hex "b1d2e32017535fb2d185011ce5b79b96512ee28661c415c76559a149f8d4f22b"
secondLeaf = hex "dcb48b82764a667afbcf88c699636b9b7c16c8793b5707320df8130192265700"
thirdLeaf = hex "95f4251f61f4a1f2896f30b61a07852c2ba70c01af74211e252189ba46d05b5a"
fourthLeaf = hex "99ebc08dce5d1546a8628f8f4f5baec65fc0d85f7142002a7ca3d51fa7051b9e"

peakHash, firstTwoRoot, lastTwoRoot :: BS.ByteString
peakHash = hex "91c6884ab9f858a70432d5a302cad27c74e95c0470014c380204bcf55f5f4ac7"
firstTwoRoot = hex "bb4b8c9161ab8382666814ad76a901b73306efc458d83c94f949b6ab3f6a179f"
lastTwoRoot = hex "5cecfab2d235824274f6ce0ad22daf43e517a060827432ac58b6634837989c46"

goldenIncludingRoot, goldenExcludingRoot :: BS.ByteString
goldenIncludingRoot = hex "341d675c5ea37d8fa4eed271f4d62f4546a441102c30271281f7ffceb6ea6300"
goldenExcludingRoot = hex "a636554b7a642f6a7cf97a544d21b858c3dab7450c2aa178b8279a91fae41541"

-- | @"absent-252"@ and @"inserted-value"@, as the fixture spells them.
goldenKey, goldenValue, otherKey :: BS.ByteString
goldenKey = hex "616273656e742d323532"
goldenValue = hex "696e7365727465642d76616c7565"
otherKey = hex "616273656e742d323533"

-- | The path the fold walks: @blake2b_256(key)@.
goldenPath :: BS.ByteString
goldenPath = blake2b256 goldenKey

nullHash :: BS.ByteString
nullHash = BS.replicate 32 0x00

-- | The path's own nibble at an index, and one that is not it.
pathNibble :: Int -> Integer
pathNibble i =
  let byte = fromIntegral (BS.index goldenPath (i `div` 2))
   in if even i then byte `div` 16 else byte `mod` 16

otherNibble :: Int -> Integer
otherNibble i = (pathNibble i + 1) `mod` 16

forkPrefix, forkRoot :: BS.ByteString
forkPrefix = hex "0102"
forkRoot = hex "0f1e2d3c4b5a69788796a5b4c3d2e1f00f1e2d3c4b5a69788796a5b4c3d2e1f0"

--------------------------------------------------------------------------------
-- Building terms
--------------------------------------------------------------------------------

firstFrame, secondFrame, thirdFrame, fourthFrame :: forall (s :: S). Term s PProofFrameV1
firstFrame = frameT 0 0 1 (branchStep 0 firstNeighbors)
secondFrame = frameT 1 1 2 (branchStep 0 secondNeighbors)
thirdFrame = frameT 2 2 3 (leafStep 0 thirdKey thirdValue)
fourthFrame = frameT 3 3 4 (leafStep 0 fourthKey fourthValue)

frameT ::
  forall (s :: S). Integer -> Integer -> Integer -> Term s (PAsData PProofStep) -> Term s PProofFrameV1
frameT = frameWith 1

frameWith ::
  forall (s :: S).
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  Term s (PAsData PProofStep) ->
  Term s PProofFrameV1
frameWith version frameIndex cursor nextCursor step =
  pcon $
    PProofFrameV1
      { pproofFrame'version = pdata (pconstant version)
      , pproofFrame'frameIndex = pdata (pconstant frameIndex)
      , pproofFrame'cursor = pdata (pconstant cursor)
      , pproofFrame'nextCursor = pdata (pconstant nextCursor)
      , pproofFrame'step = step
      }

branchStep :: forall (s :: S). Integer -> BS.ByteString -> Term s (PAsData PProofStep)
branchStep skip neighbors =
  pdata . pcon $
    PBranch
      { pproofStep'skip = pdata (pconstant skip)
      , pproofStep'neighbors = pdata (pconstant neighbors)
      }

forkStep ::
  forall (s :: S).
  Integer ->
  Integer ->
  BS.ByteString ->
  BS.ByteString ->
  Term s (PAsData PProofStep)
forkStep skip nibbleValue prefix root =
  pdata . pcon $
    PFork
      { pproofStep'skip = pdata (pconstant skip)
      , pproofStep'neighbor =
          pdata . pcon $
            PNeighbor
              { pneighbor'nibble = pdata (pconstant nibbleValue)
              , pneighbor'prefix = pdata (pconstant prefix)
              , pneighbor'root = pdata (pconstant root)
              }
      }

leafStep ::
  forall (s :: S). Integer -> BS.ByteString -> BS.ByteString -> Term s (PAsData PProofStep)
leafStep skip key value =
  pdata . pcon $
    PLeaf
      { pproofStep'skip = pdata (pconstant skip)
      , pproofStep'key = pdata (pconstant key)
      , pproofStep'value = pdata (pconstant value)
      }

descriptorT :: forall (s :: S). Term s PProofDescriptorV1
descriptorT = descriptorOf 1 4 4 [(2, peakHash)]

descriptorOf ::
  forall (s :: S).
  Integer ->
  Integer ->
  Integer ->
  [(Integer, BS.ByteString)] ->
  Term s PProofDescriptorV1
descriptorOf version frameCount terminalCursor peaks =
  pcon $
    PProofDescriptorV1
      { pproofDescriptor'version = pdata (pconstant version)
      , pproofDescriptor'frameCount = pdata (pconstant frameCount)
      , pproofDescriptor'terminalCursor = pdata (pconstant terminalCursor)
      , pproofDescriptor'peaks = pdata (peaksT peaks)
      }

peaksT ::
  forall (s :: S). [(Integer, BS.ByteString)] -> Term s (PBuiltinList (PAsData PFrontierPeak))
peaksT ps =
  foldr
    (\p acc -> pcons # p # acc)
    pnil
    [ pdata . pcon $
      PFrontierPeak
        { pfrontierPeak'height = pdata (pconstant h)
        , pfrontierPeak'hash = pdata (pconstant hash)
        }
    | (h, hash) <- ps
    ]

controlOf ::
  forall (s :: S).
  Integer ->
  Integer ->
  BS.ByteString ->
  BS.ByteString ->
  Term s PProofFoldControlV1
controlOf nextFrameIndex expectedNextCursor includingRoot excludingRoot =
  pcon $
    PProofFoldControlV1
      { pfoldControl'nextFrameIndex = pdata (pconstant nextFrameIndex)
      , pfoldControl'expectedNextCursor = pdata (pconstant expectedNextCursor)
      , pfoldControl'includingRoot = pdata (pconstant includingRoot)
      , pfoldControl'excludingRoot = pdata (pconstant excludingRoot)
      }

initialControl :: forall (s :: S). Term s PProofFoldControlV1
initialControl =
  expectJust (pinitialFoldControlV1 # pconstant goldenKey # pconstant goldenValue # descriptorT)

byteList :: forall (s :: S). [BS.ByteString] -> Term s (PBuiltinList (PAsData PByteString))
byteList bs = foldr (\b acc -> pcons # pdata (pconstant b) # acc) pnil bs

--------------------------------------------------------------------------------
-- Plumbing
--------------------------------------------------------------------------------

expectJust :: forall (s :: S) (a :: S -> Type). Term s (PMaybe a) -> Term s a
expectJust m = pmatch m $ \case
  PJust x -> x
  PNothing -> perror

isNothing :: (forall (s :: S). Term s (PMaybe PProofFoldControlV1)) -> Assertion
isNothing m =
  passertEval $
    pmatch m $ \case
      PNothing -> pconstant @PBool True
      PJust _ -> pconstant False

nextFrameIndexOf, expectedCursorOf :: forall (s :: S). Term s PProofFoldControlV1 -> Term s PInteger
nextFrameIndexOf c =
  pmatch c $ \PProofFoldControlV1 {pfoldControl'nextFrameIndex} ->
    pfromData pfoldControl'nextFrameIndex
expectedCursorOf c =
  pmatch c $ \PProofFoldControlV1 {pfoldControl'expectedNextCursor} ->
    pfromData pfoldControl'expectedNextCursor

includingRootOf, excludingRootOf :: forall (s :: S). Term s PProofFoldControlV1 -> Term s PByteString
includingRootOf c =
  pmatch c $ \PProofFoldControlV1 {pfoldControl'includingRoot} ->
    pfromData pfoldControl'includingRoot
excludingRootOf c =
  pmatch c $ \PProofFoldControlV1 {pfoldControl'excludingRoot} ->
    pfromData pfoldControl'excludingRoot

holds :: (forall (s :: S). Term s PBool) -> Assertion
holds = passertEval

refuses :: (forall (s :: S). Term s PBool) -> Assertion
refuses p = passertEval (pnot # p)

pall' :: forall (s :: S). [Term s PBool] -> Term s PBool
pall' = foldr (#&&) (pconstant True)

hex :: BS.ByteString -> BS.ByteString
hex = either error id . Base16.decode
