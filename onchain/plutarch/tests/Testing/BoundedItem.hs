{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Testing.BoundedItem
Description : Behavioural tests for the Plutarch port of
              @lib/midgard/bounded-item-v1.ak@.

An item of a Midgard transaction field is committed to in 4,095-byte chunks
folded into a Merkle frontier. Like "Testing.ValidationMerkle" this layer is
pure, so the tests carry an independent Haskell implementation of the same
scheme and check the port against it — including the chunk boundaries, which are
where an off-by-one would live: zero bytes, one byte, exactly one chunk, one
byte over, and two chunks plus a remainder.

The properties that carry the design are the /positional/ ones. A chunk hash
covers its field, item and chunk index, and an item's commitment covers its
field and item index and its total length, so nothing can be replayed at a
different position. Each of those is tested by moving one coordinate and
checking the result changes.

`verify_chunk`'s conjunction has three partial terms guarded by earlier ones, so
it has to short-circuit; the tests that pin that are marked.
-}
module Testing.BoundedItem (
  tests,

  -- * The reference frontier and CBOR, shared with "Testing.BoundedBlob"
  chunkBytes,
  buildFrontier,
  frontierCommitment,
  membershipPath,
  hashBranch,
  cborInt,
  arrayHeader,
  definiteBytes,
  blake2b256,
  peaksT,
) where

import Data.ByteString qualified as BS
import Data.ByteString.Base16 qualified as Base16
import PlutusCore.Data qualified as PD
import PlutusTx.Builtins qualified as Builtins
import PlutusTx.Builtins (fromBuiltin, toBuiltin)
import Test.Tasty
import Test.Tasty.HUnit

import Plutarch.Prelude
import Plutarch.Unsafe (punsafeCoerce)

import Midgard.ValidationMerkle (PFrontierPeak)
import Midgard.BoundedItem (
  PChunkProofV1,
  pchunkCount,
  pcommitment,
  pexpectedChunkLength,
  pfromBytes,
  phashChunk,
  pverifyChunk,
 )
import Testing.Eval (passertEval, pfails)

-- | Collects the tests defined in this module.
tests :: TestTree
tests =
  testGroup
    "Bounded Item Tests"
    [ testGroup "Aiken parity" aikenParityTests
    , testGroup
        "chunkCount and expectedChunkLength"
        [ -- The boundaries, stated as a table rather than prose.
          testCase "counts chunks at and around every boundary" $
            holds $
              pall'
                [ (pchunkCount # pconstant n) #== pconstant expected
                | (n, expected) <-
                    [ (0, 1) -- empty is one chunk, not none
                    , (1, 1)
                    , (4094, 1)
                    , (4095, 1)
                    , (4096, 2)
                    , (8189, 2)
                    , (8190, 2)
                    , (8191, 3)
                    ]
                ]
        , testCase "rejects a negative total length" $
            pfails $ pchunkCount # (-1)
        , testCase "gives every chunk but the last the full size" $
            holds $
              pall'
                [ (pexpectedChunkLength # pconstant n # pconstant i) #== pconstant expected
                | (n, i, expected) <-
                    [ (0, 0, 0) -- the empty item's single empty chunk
                    , (1, 0, 1)
                    , (4095, 0, 4095)
                    , (4096, 0, 4095)
                    , (4096, 1, 1)
                    , (8191, 0, 4095)
                    , (8191, 1, 4095)
                    , (8191, 2, 1)
                    ]
                ]
        , testCase "rejects a chunk index at the count" $
            pfails $ pexpectedChunkLength # 4096 # 2
        , testCase "rejects a negative chunk index" $
            pfails $ pexpectedChunkLength # 4096 # (-1)
        ]
    , testGroup
        "hashChunk"
        [ testCase "agrees with an independent recomputation" $
            holds $
              (phashChunk # 3 # 2 # 1 # pconstant sample)
                #== pconstant (hashChunk 3 2 1 sample)
        , -- Position is inside the hash, so the same bytes elsewhere hash
          -- differently. Without this, chunks could be permuted freely.
          testCase "distinguishes the same bytes at different positions" $
            holds $
              pallDistinctT
                [ phashChunk # pconstant f # pconstant i # pconstant c # pconstant sample
                | (f, i, c) <- [(3, 2, 1), (4, 2, 1), (3, 3, 1), (3, 2, 2)]
                ]
        , testCase "rejects a field index at the field count" $
            pfails $ phashChunk # 9 # 0 # 0 # pconstant sample
        , testCase "rejects a negative field index" $
            pfails $ phashChunk # (-1) # 0 # 0 # pconstant sample
        , testCase "rejects a negative item index" $
            pfails $ phashChunk # 0 # (-1) # 0 # pconstant sample
        , testCase "rejects a negative chunk index" $
            pfails $ phashChunk # 0 # 0 # (-1) # pconstant sample
        , testCase "rejects a chunk longer than the chunk size" $
            pfails $ phashChunk # 0 # 0 # 0 # pconstant (BS.replicate 4096 0x01)
        ]
    , testGroup
        "commitment and fromBytes"
        [ testCase "fromBytes agrees with an independent recomputation" $
            holds $
              pall'
                [ (pfromBytes # 3 # 2 # pconstant (item n)) #== pconstant (fromBytes 3 2 (item n))
                | n <- boundarySizes
                ]
        , -- The item's coordinates and length are inside the commitment.
          testCase "distinguishes the same bytes at different positions" $
            holds $
              pallDistinctT
                [ pfromBytes # pconstant f # pconstant i # pconstant (item 100)
                | (f, i) <- [(3, 2), (4, 2), (3, 3)]
                ]
        , -- Two items of different lengths inside the same chunk would share a
          -- frontier if the length were not committed separately.
          testCase "distinguishes lengths that share a chunk count" $
            holds $
              pnot
                #$ (pfromBytes # 3 # 2 # pconstant (item 100))
                #== (pfromBytes # 3 # 2 # pconstant (item 101))
        , testCase "rejects a commitment over a malformed frontier" $
            pfails $ pcommitment # 3 # 2 # 100 # peaksT [(1, leafOf 1)]
        , testCase "rejects a commitment at an out-of-range field index" $
            pfails $ pcommitment # 9 # 2 # 0 # peaksT (frontierFor 3 2 (item 0))
        ]
    , testGroup
        "verifyChunk"
        ( [ testCase ("accepts every chunk of a " <> show n <> "-byte item") $
            holds $
              pall'
                [ pverifyChunk
                  # pconstant (fromBytes 3 2 (item n))
                  # proofT (proofFor 3 2 (item n) c)
                | c <- [0 .. chunkCount n - 1]
                ]
          | n <- boundarySizes
          ]
            <> [ testCase "rejects a proof against another item's commitment" $
                  holds $
                    pnot
                      #$ pverifyChunk
                      # pconstant (fromBytes 3 2 (item 5000))
                      # proofT (proofFor 3 2 (item 4000) 0)
               , testCase "rejects a proof whose chunk bytes were altered" $
                  holds $
                    pnot
                      #$ pverifyChunk
                      # pconstant (fromBytes 3 2 (item 100))
                      # proofT (proofFor 3 2 (item 100) 0) {pChunk = item 100 <> "x"}
               , -- The chunk index in the proof has to be the one the chunk
                 -- actually sits at, since it is inside the leaf hash.
                 testCase "rejects a chunk presented at the wrong index" $
                  holds $
                    pnot
                      #$ pverifyChunk
                      # pconstant (fromBytes 3 2 (item 5000))
                      # (proofFor 3 2 (item 5000) 0 `atIndex` 1)
               , testCase "rejects a proof carrying the wrong version" $
                  holds $
                    pnot
                      #$ pverifyChunk
                      # pconstant (fromBytes 3 2 (item 100))
                      # proofT (proofFor 3 2 (item 100) 0) {pVersion = 2}
               , testCase "rejects a proof at an out-of-range field index" $
                  holds $
                    pnot
                      #$ pverifyChunk
                      # pconstant (fromBytes 3 2 (item 100))
                      # proofT (proofFor 3 2 (item 100) 0) {pFieldIndex = 9}
               , testCase "rejects an expected commitment that is not 32 bytes" $
                  holds $
                    pnot
                      #$ pverifyChunk
                      # pconstant (BS.replicate 31 0x01)
                      # proofT (proofFor 3 2 (item 100) 0)
               , -- SHORT-CIRCUIT. `expected_chunk_length` errors on an
                 -- out-of-range index, and the range checks in front of it are
                 -- what keep that from happening.
                 testCase "returns False for a chunk index at the count" $
                  holds $
                    pnot
                      #$ pverifyChunk
                      # pconstant (fromBytes 3 2 (item 100))
                      # proofT (proofFor 3 2 (item 100) 0) {pChunkIndex = 1}
               , testCase "returns False for a negative chunk index" $
                  holds $
                    pnot
                      #$ pverifyChunk
                      # pconstant (fromBytes 3 2 (item 100))
                      # proofT (proofFor 3 2 (item 100) 0) {pChunkIndex = -1}
               , -- SHORT-CIRCUIT. `hash_chunk` errors on an over-long chunk,
                 -- and the length check in front of it is what keeps that from
                 -- happening.
                 testCase "returns False for a chunk longer than the chunk size" $
                  holds $
                    pnot
                      #$ pverifyChunk
                      # pconstant (fromBytes 3 2 (item 100))
                      # proofT (proofFor 3 2 (item 100) 0) {pChunk = BS.replicate 4096 0x01}
               , -- SHORT-CIRCUIT. `commitment` errors on a malformed frontier,
                 -- and `verify_membership` in front of it returns False first.
                 testCase "returns False for a malformed frontier" $
                  holds $
                    pnot
                      #$ pverifyChunk
                      # pconstant (fromBytes 3 2 (item 100))
                      # proofT (proofFor 3 2 (item 100) 0) {pFrontier = [(1, leafOf 1)]}
               ]
        )
    ]

aikenParityTests :: [TestTree]
aikenParityTests =
  [ testCase "canonical_cross_language_item_vector" $
      holds $
        ((phashChunk # 2 # 7 # 0 # pconstant helloBytes) #== pconstant helloChunk)
          #&& ((pcommitment # 2 # 7 # 5 # peaksT [(0, helloChunk)]) #== pconstant helloCommitment)
          #&& ((pfromBytes # 2 # 7 # pconstant helloBytes) #== pconstant helloCommitment)
  , testCase "exact_chunk_membership_is_accepted" $
      holds $ pverifyChunk # pconstant helloCommitment # proofT helloProof
  , testCase "cross_item_replay_fails_closed" $
      holds $
        pnot
          #$ pverifyChunk
          # pconstant helloCommitment
          # proofT helloProof {pItemIndex = 8}
  ]

helloBytes, helloChunk, helloCommitment :: BS.ByteString
helloBytes = "hello"
helloChunk = hex "8c61c7975b3faa9d5248d9dfd3ebd992ec7b0679c723f26841557a040c422f55"
helloCommitment = hex "3df275a9639180b74b52f354e0d9f9cd81a409a19e7b9ef6889a1b719db98cf4"

helloProof :: Proof
helloProof = proofFor 2 7 helloBytes 0

--------------------------------------------------------------------------------
-- Fixtures
--------------------------------------------------------------------------------

holds :: (forall s. Term s PBool) -> Assertion
holds = passertEval

pall' :: forall s. [Term s PBool] -> Term s PBool
pall' = foldr (#&&) (pconstant True)

pallDistinctT :: forall s. [Term s PByteString] -> Term s PBool
pallDistinctT xs =
  pall' [pnot #$ a #== b | (i, a) <- zip [0 :: Int ..] xs, (j, b) <- zip [0 ..] xs, i < j]

{- | The sizes worth testing: both sides of every chunk boundary, plus a
multi-chunk item with a remainder.
-}
boundarySizes :: [Int]
boundarySizes = [0, 1, 4094, 4095, 4096, 8190, 8191, 12000]

-- | A deterministic item of @n@ bytes.
item :: Int -> BS.ByteString
item n = BS.take n (BS.concat (replicate (n `div` 32 + 1) (blake2b256 (BS.pack [fromIntegral n]))))

sample :: BS.ByteString
sample = item 100

leafOf :: Int -> BS.ByteString
leafOf n = blake2b256 (BS.pack [fromIntegral n])

-- | A proof of one chunk, as a record so a test can move one field at a time.
data Proof = Proof
  { pVersion :: Integer
  , pFieldIndex :: Integer
  , pItemIndex :: Integer
  , pTotalLength :: Integer
  , pChunkIndex :: Integer
  , pChunk :: BS.ByteString
  , pFrontier :: [(Integer, BS.ByteString)]
  , pSiblings :: [BS.ByteString]
  }

proofFor :: Integer -> Integer -> BS.ByteString -> Int -> Proof
proofFor fieldIndex itemIndex bytes chunkIndex =
  Proof
    { pVersion = 1
    , pFieldIndex = fieldIndex
    , pItemIndex = itemIndex
    , pTotalLength = fromIntegral (BS.length bytes)
    , pChunkIndex = fromIntegral chunkIndex
    , pChunk = chunkAt bytes chunkIndex
    , pFrontier = frontierFor fieldIndex itemIndex bytes
    , pSiblings = siblingsFor fieldIndex itemIndex bytes chunkIndex
    }

{- | Moves a proof to a different chunk index without changing anything else, so
the leaf hash it implies no longer matches the frontier.
-}
atIndex :: forall s. Proof -> Int -> Term s PChunkProofV1
atIndex p i = proofT p {pChunkIndex = fromIntegral i}

--------------------------------------------------------------------------------
-- The reference implementation
--------------------------------------------------------------------------------

chunkBytes :: Int
chunkBytes = 4095

chunkCount :: Int -> Int
chunkCount 0 = 1
chunkCount n = (n + chunkBytes - 1) `div` chunkBytes

expectedChunkLength :: Int -> Int -> Int
expectedChunkLength totalLength chunkIndex
  | chunkIndex + 1 < chunkCount totalLength = chunkBytes
  | otherwise = totalLength - chunkIndex * chunkBytes

chunkAt :: BS.ByteString -> Int -> BS.ByteString
chunkAt bytes i =
  BS.take (expectedChunkLength (BS.length bytes) i) (BS.drop (i * chunkBytes) bytes)

hashChunk :: Integer -> Integer -> Integer -> BS.ByteString -> BS.ByteString
hashChunk fieldIndex itemIndex chunkIndex chunk =
  blake2b256 $
    BS.concat
      [ "MidgardBoundedItemChunkV1"
      , arrayHeader 5
      , cborInt 1
      , cborInt fieldIndex
      , cborInt itemIndex
      , cborInt chunkIndex
      , definiteBytes chunk
      ]

-- | The chunk hashes of an item, in order.
chunkLeaves :: Integer -> Integer -> BS.ByteString -> [BS.ByteString]
chunkLeaves fieldIndex itemIndex bytes =
  [ hashChunk fieldIndex itemIndex (fromIntegral i) (chunkAt bytes i)
  | i <- [0 .. chunkCount (BS.length bytes) - 1]
  ]

frontierFor :: Integer -> Integer -> BS.ByteString -> [(Integer, BS.ByteString)]
frontierFor fieldIndex itemIndex bytes =
  snd (buildFrontier (chunkLeaves fieldIndex itemIndex bytes))

siblingsFor :: Integer -> Integer -> BS.ByteString -> Int -> [BS.ByteString]
siblingsFor fieldIndex itemIndex bytes =
  membershipPath (chunkLeaves fieldIndex itemIndex bytes)

fromBytes :: Integer -> Integer -> BS.ByteString -> BS.ByteString
fromBytes fieldIndex itemIndex bytes =
  commitment
    fieldIndex
    itemIndex
    (fromIntegral (BS.length bytes))
    (frontierFor fieldIndex itemIndex bytes)

commitment :: Integer -> Integer -> Integer -> [(Integer, BS.ByteString)] -> BS.ByteString
commitment fieldIndex itemIndex totalLength frontier =
  blake2b256 $
    BS.concat
      [ "MidgardBoundedItemCommitmentV1"
      , arrayHeader 5
      , cborInt 1
      , cborInt fieldIndex
      , cborInt itemIndex
      , cborInt totalLength
      , definiteBytes (frontierCommitment count frontier)
      ]
  where
    count = fromIntegral (chunkCount (fromIntegral totalLength))

--------------------------------------------------------------------------------
-- The frontier, again independently
--------------------------------------------------------------------------------

hashBranch :: BS.ByteString -> BS.ByteString -> BS.ByteString
hashBranch l r = blake2b256 ("MidgardValidationMerkleBranchV1" <> l <> r)

buildFrontier :: [BS.ByteString] -> (Integer, [(Integer, BS.ByteString)])
buildFrontier = go 0 []
  where
    go c ps [] = (c, ps)
    go c ps (l : ls) = go (c + 1) (appendCarry c 0 l ps) ls

    appendCarry oldCount height carry peaks
      | even oldCount = (height, carry) : peaks
      | otherwise = case peaks of
          ((h, hash) : rest)
            | h == height ->
                appendCarry (oldCount `div` 2) (height + 1) (hashBranch hash carry) rest
          _ -> error "reference frontier: malformed peaks"

frontierCommitment :: Integer -> [(Integer, BS.ByteString)] -> BS.ByteString
frontierCommitment count peaks =
  blake2b256 $
    "MidgardValidationMerkleFrontierV1"
      <> cborInt count
      <> ( arrayHeader (length peaks)
             <> BS.concat ["\x82" <> cborInt h <> definiteBytes hash | (h, hash) <- peaks]
         )

-- | The sibling path for a leaf, bottom up. See @Testing.ValidationMerkle@.
membershipPath :: [BS.ByteString] -> Int -> [BS.ByteString]
membershipPath leaves leafIndex = path subLeaves (leafIndex - offset) []
  where
    (_, peaks) = buildFrontier leaves
    -- Stored low bit first, but laid out tallest peak first.
    (height, offset) = locate (reverse peaks) 0

    locate [] _ = error "reference membershipPath: index outside the frontier"
    locate ((h, _) : rest) off
      | leafIndex < off + 2 ^ h = (fromIntegral h, off)
      | otherwise = locate rest (off + 2 ^ h)

    subLeaves = take (2 ^ height) (drop offset leaves)

    path [_] _ acc = reverse acc
    path level idx acc =
      let sibling = if even idx then level !! (idx + 1) else level !! (idx - 1)
       in path (pairUp level) (idx `div` 2) (sibling : acc)

    pairUp (a : b : rest) = hashBranch a b : pairUp rest
    pairUp _ = []

--------------------------------------------------------------------------------
-- CBOR, independently
--------------------------------------------------------------------------------

cborInt :: Integer -> BS.ByteString
cborInt n
  | n < 0 = error "reference cborInt: negative"
  | n <= 23 = BS.pack [fromIntegral n]
  | n <= 255 = BS.pack [0x18, fromIntegral n]
  | n <= 65535 = BS.pack [0x19, fromIntegral (n `div` 256), fromIntegral (n `mod` 256)]
  | otherwise = error "reference cborInt: out of fixture range"

arrayHeader :: Int -> BS.ByteString
arrayHeader n
  | n <= 23 = BS.pack [fromIntegral (128 + n)]
  | otherwise = error "reference arrayHeader: out of fixture range"

definiteBytes :: BS.ByteString -> BS.ByteString
definiteBytes bytes
  | len <= 23 = BS.pack [fromIntegral (64 + len)] <> bytes
  | len <= 255 = BS.pack [0x58, fromIntegral len] <> bytes
  | len <= 65535 = BS.pack [0x59, fromIntegral (len `div` 256), fromIntegral (len `mod` 256)] <> bytes
  | otherwise = error "reference definiteBytes: out of fixture range"
  where
    len = BS.length bytes

blake2b256 :: BS.ByteString -> BS.ByteString
blake2b256 = fromBuiltin . Builtins.blake2b_256 . toBuiltin

hex :: BS.ByteString -> BS.ByteString
hex = either error id . Base16.decode

--------------------------------------------------------------------------------
-- Plumbing
--------------------------------------------------------------------------------

proofT :: forall s. Proof -> Term s PChunkProofV1
proofT p =
  pfromData $
    punsafeCoerce @(PAsData PChunkProofV1) $
      pconstant @PData $
        PD.Constr
          0
          [ PD.I (pVersion p)
          , PD.I (pFieldIndex p)
          , PD.I (pItemIndex p)
          , PD.I (pTotalLength p)
          , PD.I (pChunkIndex p)
          , PD.B (pChunk p)
          , PD.List [PD.Constr 0 [PD.I h, PD.B hash] | (h, hash) <- pFrontier p]
          , PD.List (map PD.B (pSiblings p))
          ]

peaksT :: forall s. [(Integer, BS.ByteString)] -> Term s (PBuiltinList (PAsData PFrontierPeak))
peaksT ps =
  punsafeCoerce $
    pasList # pconstant @PData (PD.List [PD.Constr 0 [PD.I h, PD.B hash] | (h, hash) <- ps])
