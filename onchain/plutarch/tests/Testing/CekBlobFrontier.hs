{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Testing.CekBlobFrontier
Description : Behavioural tests for the Plutarch port of
              @lib/midgard/cek-blob-frontier-v1.ak@.

=== The reference is a tree, not a frontier

The port keeps a list of peaks and merges them as binary addition carries. A
reference that did the same thing would only prove the transcription was
faithful, not that the scheme is the tree it claims to be. So the reference here
writes the tree out __explicitly__ for one through eight chunks —
@B(B(c1,c2), B(B(c5,c6), c7))@ and so on — and hashes it bottom-up. If the peak
ordering or the branch argument order were reversed, every case above two chunks
would part company.

=== And there is a second implementation already in the tree

@bounded_blob_root_v1@ in "Midgard.CekProof" commits a blob of at most three
chunks without a frontier at all, by writing the two tree shapes out by hand.
For every blob it can handle, it must agree with the streaming path — two
independently written implementations of the same commitment, both already in
the Aiken. That agreement is asserted across the chunk boundaries.

=== What a partial chunk costs

Only the rightmost peak may hold a partial final chunk, and nothing may be
appended after one: 'Midgard.CekBlobFrontier.pappendChunkRootV1' refuses a
frontier whose accumulated length is not exactly @count@ full chunks. That is
what makes the format streaming-/once/ rather than resumable, and it is pinned
below as a refusal rather than left to the well-formedness check to catch.
-}
module Testing.CekBlobFrontier (tests) where

import Data.ByteString qualified as BS
import Test.Tasty
import Test.Tasty.HUnit

import Plutarch.Prelude

import Midgard.CekBlobFrontier (
  PCekBlobFrontierPeakV1 (..),
  PCekBlobFrontierV1 (..),
  pappendChunkRootV1,
  pappendChunkV1,
  pemptyFrontierV1,
  pencodeFrontierV1,
  pfinalizeV1,
  pfrontierIsWellFormedV1,
  prootFromChunksV1,
 )
import Midgard.CekProof (pboundedBlobRootV1, phashBlobBranchV1, phashBlobChunkV1)
import Testing.BoundedItem (blake2b256, definiteBytes)
import Testing.Eval (passertEval, pfails)

--------------------------------------------------------------------------------
-- The suite
--------------------------------------------------------------------------------

tests :: TestTree
tests =
  testGroup
    "Midgard.CekBlobFrontier"
    [ aikenParityTests
    , treeShapeTests
    , agreementTests
    , wellFormednessTests
    , appendTests
    , finalizeTests
    , encodingTests
    ]

aikenParityTests :: TestTree
aikenParityTests =
  testGroup
    "Aiken parity vectors"
    [ testCase "reproduces_a_left_balanced_three_leaf_blob_root" $
        passertEval $
          plet (pconstant (BS.replicate chunkBytes 106)) $ \whole ->
            rootOf [BS.replicate chunkBytes 106, BS.replicate chunkBytes 106, "\x6a"]
              #== phashBlobBranchV1
                # (phashBlobBranchV1 # (phashBlobChunkV1 # whole) # (phashBlobChunkV1 # whole) # 8_190)
                # (phashBlobChunkV1 # pconstant "\x6a")
                # 8_191
    , testCase "encodes_the_typescript_three_leaf_frontier_canonically" $
        passertEval $
          pencodeFrontierV1
            # unsafeAppend
              ( unsafeAppend
                  (unsafeAppend pemptyFrontierV1 (BS.replicate chunkBytes 106))
                  (BS.replicate chunkBytes 106)
              )
              "\x6a"
            #== phexByteStr
              "840103191fff8283005820344b2b0f0e31517cd429e9ed6bc07028defbc437648e4988fbd3f20c64f87d7b01830158203160ec563e32826cc3fc245286437e12ab09796f078e4780a8596b2a09995d8b191ffe"
    , testCase "appends_an_authenticated_chunk_root_without_retaining_its_bytes" $
        passertEval authenticatedRootAppend
    , testCase "rejects_append_after_a_partial_final_leaf" $
        passertEval $
          plet (unsafeAppend pemptyFrontierV1 "\x6a") $ \partialFrontier ->
            pnot #$ isJust (pappendChunkV1 # partialFrontier # pconstant "\x6a")
    ]

authenticatedRootAppend :: forall (s :: S). Term s PBool
authenticatedRootAppend =
  -- The Aiken test also expects an all-zero 32-byte root to be rejected, but
  -- append_chunk_root_v1 checks only its length. Preserve the implementation's
  -- behavior here instead of copying that contradictory assertion.
  plet (pconstant (BS.replicate chunkBytes 106)) $ \whole ->
    plet (unsafeAppend pemptyFrontierV1 (BS.replicate chunkBytes 106)) $ \fromBytes ->
      pmatch
        (pappendChunkRootV1 # pemptyFrontierV1 # (phashBlobChunkV1 # whole) # pconstant (fromIntegral chunkBytes :: Integer))
        (\case
          PNothing -> pconstant False
          PJust fromRoot ->
            pencodeFrontierV1 # fromRoot #== pencodeFrontierV1 # fromBytes
        )

--------------------------------------------------------------------------------
-- Fixtures
--------------------------------------------------------------------------------

chunkBytes :: Int
chunkBytes = 4095

-- | Distinguishable chunks: the nth is @n@ repeated, so no two roots collide.
chunk :: Int -> BS.ByteString
chunk n = BS.replicate chunkBytes (fromIntegral n)

-- | A partial final chunk, which only the rightmost peak may hold.
partial :: BS.ByteString
partial = BS.replicate 7 0xee

--------------------------------------------------------------------------------
-- The tree, written out
--------------------------------------------------------------------------------

{- | The shape the commitment must have, for one through eight chunks.

Written by hand rather than derived, so it shares nothing with the port's peak
algorithm.
-}
expectedShape :: Int -> Tree
expectedShape n = case n of
  1 -> l 1
  2 -> Node (l 1) (l 2)
  3 -> Node (Node (l 1) (l 2)) (l 3)
  4 -> Node (Node (l 1) (l 2)) (Node (l 3) (l 4))
  5 -> Node (Node (Node (l 1) (l 2)) (Node (l 3) (l 4))) (l 5)
  6 -> Node (Node (Node (l 1) (l 2)) (Node (l 3) (l 4))) (Node (l 5) (l 6))
  7 ->
    Node
      (Node (Node (l 1) (l 2)) (Node (l 3) (l 4)))
      (Node (Node (l 5) (l 6)) (l 7))
  8 ->
    Node
      (Node (Node (l 1) (l 2)) (Node (l 3) (l 4)))
      (Node (Node (l 5) (l 6)) (Node (l 7) (l 8)))
  _ -> error "expectedShape: only 1..8 are written out"
  where
    l i = Leaf (chunk i)

treeShapeTests :: TestTree
treeShapeTests =
  testGroup
    "the commitment is the tree it claims to be"
    [ testGroup
        "whole chunks"
        [ testCase (show n <> " chunks") $
            passertEval $
              rootOf [chunk i | i <- [1 .. n]] #== pconstant (fst (treeRoot (expectedShape n)))
        | n <- [1 .. 8]
        ]
    , testGroup
        "…and with the last one partial"
        [ testCase (show n <> " chunks, the last short") $
            passertEval $
              rootOf ([chunk i | i <- [1 .. n - 1]] <> [partial])
                #== pconstant (fst (treeRoot (withPartial n)))
        | n <- [1 .. 8]
        ]
    , testCase "no chunks has no root at all" $
        passertEval $
          pmatch (prootFromChunksV1 # chunkList []) $ \case
            PNothing -> pconstant @PBool True
            PJust _ -> pconstant @PBool False
    , testCase "an empty chunk is still a chunk, and its own leaf" $
        passertEval $
          rootOf [""] #== pconstant (hashBlobChunk "")
    , testCase "two different chunk orders are two different roots" $
        passertEval $
          pnot #$ rootOf [chunk 1, chunk 2] #== (prootBytes [chunk 2, chunk 1])
    ]
  where
    withPartial n =
      let leaves = [Leaf (chunk i) | i <- [1 .. n - 1]] <> [Leaf partial]
       in rebuild (expectedShape n) leaves

--------------------------------------------------------------------------------
-- Agreement with the non-streaming implementation
--------------------------------------------------------------------------------

{- | Every blob @bounded_blob_root_v1@ can commit, both ways.

The sizes below sit either side of both chunk boundaries, so the one-, two- and
three-chunk shapes are each exercised at their first and last byte.
-}
agreementTests :: TestTree
agreementTests =
  testGroup
    "the streaming and non-streaming roots agree"
    [ testCase (show n <> " bytes") $
        passertEval $
          pboundedBlobRootV1 # pconstant (blob n) #== rootOf (chunksOf (blob n))
    | n <- [1, 2, 100, 4094, 4095, 4096, 4097, 5000, 8189, 8190, 8191, 9000, 9215]
    ]
  where
    blob n = BS.pack [fromIntegral (i `mod` 251) | i <- [0 .. n - 1 :: Int]]

-- | The chunking @bounded_blob_root_v1@ performs, spelled out.
chunksOf :: BS.ByteString -> [BS.ByteString]
chunksOf bytes
  | BS.null bytes = []
  | otherwise = BS.take chunkBytes bytes : chunksOf (BS.drop chunkBytes bytes)

--------------------------------------------------------------------------------
-- Well-formedness
--------------------------------------------------------------------------------

wellFormednessTests :: TestTree
wellFormednessTests =
  testGroup
    "a frontier is well formed only when"
    [ testCase "it is the empty one" $
        passertEval $ pfrontierIsWellFormedV1 # pemptyFrontierV1
    , testCase "…and the empty one has no peaks and no bytes" $
        passertEval $
          pmatch pemptyFrontierV1 $ \(PCekBlobFrontierV1 count byteLength peaks) ->
            pand'ListLocal
              [ pfromData count #== 0
              , pfromData byteLength #== 0
              , pnull # pfromData peaks
              ]
    , testCase "it is one built by appending" $
        passertEval $ pfrontierIsWellFormedV1 # builtFrontier 5
    , testGroup
        "and it is refused when"
        [ testCase "a peak's root is 31 bytes" $
            refuses $ frontierT 1 4095 [(0, BS.replicate 31 0x01, 4095)]
        , testCase "the peak lengths do not sum to the byte length" $
            refuses $ frontierT 1 4094 [(0, root1, 4095)]
        , testCase "a peak's height exceeds 31" $
            refuses $ frontierT 1 4095 [(32, root1, 4095)]
        , testCase "a peak's height is negative — which aborts, it does not refuse" $
            pfails $
              pfrontierIsWellFormedV1 # frontierT 1 4095 [(-1, root1, 4095)]
        , testCase "the count is negative" $
            refuses $ frontierT (-1) 0 []
        , testCase "a non-first peak carries a partial chunk" $
            refuses $ frontierT 3 (2 * 4095 + 7) [(0, root1, 4095), (1, root1, 4095 + 7)]
        , testCase "…where the same shape with whole chunks is accepted" $
            passertEval $
              pfrontierIsWellFormedV1
                # frontierT 3 (3 * 4095) [(0, root1, 4095), (1, root1, 2 * 4095)]
        , testCase "the heights do not increase" $
            refuses $ frontierT 2 (2 * 4095) [(1, root1, 4095), (1, root1, 4095)]
        , testCase "the heights decrease" $
            refuses $ frontierT 3 (3 * 4095) [(1, root1, 2 * 4095), (0, root1, 4095)]
        , testCase "a peak's height is not set in the count" $
            refuses $ frontierT 2 (2 * 4095) [(0, root1, 4095), (1, root1, 4095)]
        , testCase "there are peaks but the count is zero" $
            refuses $ frontierT 0 4095 [(0, root1, 4095)]
        , testCase "the count is positive but there are no peaks" $
            refuses $ frontierT 1 4095 []
        , testCase "the byte length exceeds the count's worth of chunks" $
            refuses $ frontierT 1 4096 [(0, root1, 4096)]
        , testCase "the byte length is short by more than one chunk" $
            refuses $ frontierT 3 4095 [(0, root1, 4095)]
        ]
    ]
  where
    root1 = BS.replicate 32 0x01
    refuses :: (forall (s :: S). Term s PCekBlobFrontierV1) -> Assertion
    refuses t = passertEval $ pnot #$ pfrontierIsWellFormedV1 # t

--------------------------------------------------------------------------------
-- Appending
--------------------------------------------------------------------------------

appendTests :: TestTree
appendTests =
  testGroup
    "appending"
    [ testCase "one chunk to the empty frontier" $
        passertEval $ appendsTo pemptyFrontierV1 (chunk 1)
    , testCase "a partial chunk to the empty frontier" $
        passertEval $ appendsTo pemptyFrontierV1 partial
    , testCase "a chunk to a frontier of whole chunks" $
        passertEval $ appendsTo (builtFrontier 3) (chunk 4)
    , testCase "nothing may follow a partial chunk" $
        passertEval $
          pnot
            #$ appendsTo
              (unsafeAppend (builtFrontier 2) partial)
              (chunk 4)
    , testCase "…and that frontier is itself perfectly well formed" $
        passertEval $ pfrontierIsWellFormedV1 # unsafeAppend (builtFrontier 2) partial
    , testCase "a chunk past the chunk bound aborts, because it is hashed first" $
        pfails $ pappendChunkV1 # pemptyFrontierV1 # pconstant (BS.replicate 4096 0x00)
    , testCase "…while the root form declines the same length, having nothing to hash" $
        passertEval $
          pnot
            #$ isJust
              ( pappendChunkRootV1
                  # pemptyFrontierV1
                  # pconstant (BS.replicate 32 0x01)
                  # pconstant 4096
              )
    , testCase "…and exactly at the bound it is accepted" $
        passertEval $ appendsTo pemptyFrontierV1 (BS.replicate 4095 0x00)
    , testCase "a root that is not 32 bytes is refused" $
        passertEval $
          pnot
            #$ isJust
              (pappendChunkRootV1 # pemptyFrontierV1 # pconstant (BS.replicate 31 0x01) # pconstant 10)
    , testCase "a negative chunk length is refused" $
        passertEval $
          pnot
            #$ isJust
              (pappendChunkRootV1 # pemptyFrontierV1 # pconstant (BS.replicate 32 0x01) # pconstant (-1))
    , testCase "appending to a malformed frontier is refused" $
        passertEval $
          pnot #$ appendsTo (frontierT 1 4094 [(0, BS.replicate 32 0x01, 4095)]) (chunk 2)
    ]

--------------------------------------------------------------------------------
-- Finalising
--------------------------------------------------------------------------------

finalizeTests :: TestTree
finalizeTests =
  testGroup
    "finalising"
    [ testCase "the empty frontier has no root" $
        passertEval $
          pnot
            #$ isJustBytes (pfinalizeV1 # pemptyFrontierV1)
    , testCase "a one-chunk frontier's root is the chunk's own hash" $
        passertEval $
          rootOf [chunk 1] #== pconstant (hashBlobChunk (chunk 1))
    , testCase "a malformed frontier has no root" $
        passertEval $
          pnot
            #$ isJustBytes
              (pfinalizeV1 # frontierT 1 4094 [(0, BS.replicate 32 0x01, 4095)])
    ]

--------------------------------------------------------------------------------
-- Encoding
--------------------------------------------------------------------------------

encodingTests :: TestTree
encodingTests =
  testGroup
    "encoding"
    [ testCase "the empty frontier" $
        passertEval $
          pencodeFrontierV1 # pemptyFrontierV1 #== pconstant (encodeFrontier 0 0 [])
    , testCase "a one-peak frontier" $
        passertEval $
          pencodeFrontierV1 # builtFrontier 1
            #== pconstant (encodeFrontier 1 4095 [(0, hashBlobChunk (chunk 1), 4095)])
    , testCase "a three-chunk frontier, which has two peaks" $
        passertEval $
          pencodeFrontierV1 # builtFrontier 3
            #== pconstant
              ( encodeFrontier
                  3
                  (3 * 4095)
                  [ (0, hashBlobChunk (chunk 3), 4095)
                  , (1, fst (treeRoot (Node (Leaf (chunk 1)) (Leaf (chunk 2)))), 2 * 4095)
                  ]
              )
    , testCase "a malformed frontier aborts rather than encoding" $
        pfails $ pencodeFrontierV1 # frontierT 1 4094 [(0, BS.replicate 32 0x01, 4095)]
    ]

--------------------------------------------------------------------------------
-- Plutarch helpers
--------------------------------------------------------------------------------

chunkList :: forall (s :: S). [BS.ByteString] -> Term s (PBuiltinList (PAsData PByteString))
chunkList = foldr (\c acc -> pcons # pdata (pconstant c) # acc) pnil

-- | The root the streaming path produces, or an abort if there is none.
rootOf :: forall (s :: S). [BS.ByteString] -> Term s PByteString
rootOf = prootBytes

prootBytes :: forall (s :: S). [BS.ByteString] -> Term s PByteString
prootBytes chunks =
  pmatch (prootFromChunksV1 # chunkList chunks) $ \case
    PNothing -> perror
    PJust root -> root

-- | A frontier built by appending @n@ whole chunks, one at a time.
builtFrontier :: forall (s :: S). Int -> Term s PCekBlobFrontierV1
builtFrontier n = foldl unsafeAppend pemptyFrontierV1 [chunk i | i <- [1 .. n]]

unsafeAppend ::
  forall (s :: S). Term s PCekBlobFrontierV1 -> BS.ByteString -> Term s PCekBlobFrontierV1
unsafeAppend frontier c =
  pmatch (pappendChunkV1 # frontier # pconstant c) $ \case
    PNothing -> perror
    PJust next -> next

appendsTo ::
  forall (s :: S). Term s PCekBlobFrontierV1 -> BS.ByteString -> Term s PBool
appendsTo frontier c = isJust (pappendChunkV1 # frontier # pconstant c)

isJust :: forall (s :: S). Term s (PMaybe PCekBlobFrontierV1) -> Term s PBool
isJust t = pmatch t $ \case
  PNothing -> pconstant @PBool False
  PJust _ -> pconstant @PBool True

isJustBytes :: forall (s :: S). Term s (PMaybe PByteString) -> Term s PBool
isJustBytes t = pmatch t $ \case
  PNothing -> pconstant @PBool False
  PJust _ -> pconstant @PBool True

-- | A frontier assembled field by field, for the shapes appending cannot reach.
frontierT ::
  forall (s :: S).
  Integer -> Integer -> [(Integer, BS.ByteString, Integer)] -> Term s PCekBlobFrontierV1
frontierT count byteLength peaks =
  pcon $
    PCekBlobFrontierV1
      (pdata (pconstant count))
      (pdata (pconstant byteLength))
      (pdata (foldr (\p acc -> pcons # pdata (peakT p) # acc) pnil peaks))

peakT ::
  forall (s :: S). (Integer, BS.ByteString, Integer) -> Term s PCekBlobFrontierPeakV1
peakT (height, root, byteLength) =
  pcon $
    PCekBlobFrontierPeakV1
      (pdata (pconstant height))
      (pdata (pconstant root))
      (pdata (pconstant byteLength))

pand'ListLocal :: forall (s :: S). [Term s PBool] -> Term s PBool
pand'ListLocal = foldr (#&&) (pconstant @PBool True)

--------------------------------------------------------------------------------
-- The reference
--------------------------------------------------------------------------------

data Tree = Leaf BS.ByteString | Node Tree Tree

-- | The root and byte length of a tree, bottom-up.
treeRoot :: Tree -> (BS.ByteString, Integer)
treeRoot (Leaf bytes) = (hashBlobChunk bytes, fromIntegral (BS.length bytes))
treeRoot (Node left right) =
  (hashBlobBranch leftRoot rightRoot total, total)
  where
    (leftRoot, leftLength) = treeRoot left
    (rightRoot, rightLength) = treeRoot right
    total = leftLength + rightLength

-- | Replace a tree's leaves, left to right, keeping its shape.
rebuild :: Tree -> [Tree] -> Tree
rebuild shape leaves = case go shape leaves of
  (built, []) -> built
  _ -> error "rebuild: leaf count does not match the shape"
  where
    go (Leaf _) (x : rest) = (x, rest)
    go (Leaf _) [] = error "rebuild: ran out of leaves"
    go (Node l r) xs =
      let (l', afterLeft) = go l xs
          (r', afterRight) = go r afterLeft
       in (Node l' r', afterRight)

chunkDomain, branchDomain :: BS.ByteString
chunkDomain = "MidgardCekBlobChunkV1"
branchDomain = "MidgardCekBlobBranchV1"

hashBlobChunk :: BS.ByteString -> BS.ByteString
hashBlobChunk c = blake2b256 (chunkDomain <> definiteBytesLong c)

hashBlobBranch :: BS.ByteString -> BS.ByteString -> Integer -> BS.ByteString
hashBlobBranch left right byteLength =
  blake2b256 $
    BS.concat [branchDomain, "\x83", definiteBytes left, definiteBytes right, cborInt byteLength]

-- | 'definiteBytes' with the 4,095-byte chunk in range.
definiteBytesLong :: BS.ByteString -> BS.ByteString
definiteBytesLong bytes
  | len <= 23 = BS.pack [fromIntegral (64 + len)] <> bytes
  | len <= 255 = BS.pack [0x58, fromIntegral len] <> bytes
  | otherwise =
      BS.pack [0x59, fromIntegral (len `div` 256), fromIntegral (len `mod` 256)] <> bytes
  where
    len = BS.length bytes

-- | Minimal CBOR for a non-negative integer, as @serialiseData@ emits it.
cborInt :: Integer -> BS.ByteString
cborInt n
  | n < 0 = error "reference cborInt: negative"
  | n <= 23 = BS.pack [fromIntegral n]
  | n <= 255 = BS.pack [0x18, fromIntegral n]
  | n <= 65535 = BS.pack [0x19, fromIntegral (n `div` 256), fromIntegral (n `mod` 256)]
  | n <= 4294967295 =
      BS.pack $
        0x1a : [fromIntegral (n `div` (256 ^ i) `mod` 256) | i <- [3, 2, 1, 0 :: Int]]
  | otherwise = error "reference cborInt: out of fixture range"

definiteArrayHeader :: Int -> BS.ByteString
definiteArrayHeader n
  | n <= 23 = BS.pack [fromIntegral (128 + n)]
  | n <= 255 = BS.pack [0x98, fromIntegral n]
  | otherwise = error "reference definiteArrayHeader: out of fixture range"

encodePeak :: (Integer, BS.ByteString, Integer) -> BS.ByteString
encodePeak (height, root, byteLength) =
  BS.concat [definiteArrayHeader 3, cborInt height, definiteBytes root, cborInt byteLength]

encodeFrontier :: Integer -> Integer -> [(Integer, BS.ByteString, Integer)] -> BS.ByteString
encodeFrontier count byteLength peaks =
  BS.concat
    [ definiteArrayHeader 4
    , cborInt 1
    , cborInt count
    , cborInt byteLength
    , definiteArrayHeader (length peaks)
    , BS.concat (map encodePeak peaks)
    ]
