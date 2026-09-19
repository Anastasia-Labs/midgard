{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Testing.BoundedBlob
Description : Behavioural tests for the Plutarch port of
              @lib/midgard/bounded-blob-v1.ak@.

A whole transaction field committed to in 4,095-byte chunks folded into a Merkle
frontier. The layer is pure, so the tests carry a Haskell implementation of the
same scheme and check the port against it — including the chunk boundaries, where
an off-by-one would live.

The frontier and CBOR halves of that reference are imported from
"Testing.BoundedItem" rather than copied. They are the /shared/ machinery, they
are already pinned there against a different port, and a second transcription of
them would test nothing this module cares about. Everything specific to blobs —
the chunk count, the two domain strings, both preimage layouts — is written here
from @bounded-blob-v1.ak@.

=== Three golden hashes

The Aiken suite pins @hash_chunk@ and @commitment@ on exact bytes, produced by a
different implementation on a different runtime. Those three values are asserted
below against both the port and this module's reference, which is the strongest
check available here: a shared misunderstanding of the preimage would have to be
shared with Aiken too.

=== It is not 'Testing.BoundedItem' with a field dropped

@chunk_count(0)@ is zero here and one there, and the difference propagates: an
empty blob has a commitment that no chunk proof can ever be offered against,
because @verify_chunk@'s outer guard demands a positive total length. Both halves
of that are pinned below, since a port that reused the item module's count would
pass every non-empty case.
-}
module Testing.BoundedBlob (tests) where

import Data.ByteString qualified as BS
import Data.ByteString.Base16 qualified as Base16
import PlutusCore.Data qualified as PD
import Test.Tasty
import Test.Tasty.HUnit

import Plutarch.Prelude
import Plutarch.Unsafe (punsafeCoerce)

import Midgard.BoundedBlob (
  PChunkProofV1,
  pchunkCount,
  pcommitment,
  pexpectedChunkLength,
  pfromPreimage,
  phashChunk,
  pverifyChunk,
 )
import Testing.BoundedItem (
  arrayHeader,
  blake2b256,
  buildFrontier,
  cborInt,
  chunkBytes,
  definiteBytes,
  frontierCommitment,
  membershipPath,
  peaksT,
 )
import Testing.Eval (passertEval, pfails)

--------------------------------------------------------------------------------
-- The suite
--------------------------------------------------------------------------------

tests :: TestTree
tests =
  testGroup
    "Bounded Blob Tests"
    [ testGroup "Aiken parity" aikenParityTests
    , testGroup "the chunking arithmetic" arithmeticTests
    , testGroup "the golden hashes" goldenTests
    , testGroup "hashChunk and commitment against the reference" referenceTests
    , testGroup "verifyChunk" verifyTests
    , testGroup "the empty blob" emptyTests
    ]

-- | Direct ports of the two tests in @bounded-blob-v1.test.ak@.
aikenParityTests :: [TestTree]
aikenParityTests =
  [ testCase "chunk_commitment_binds_exact_lengths_and_membership" $
      holds $
        (pchunkCount # 4098 #== 2)
          #&& ((phashChunk # 2 # 0 # pconstant firstChunk) #== pconstant firstHash)
          #&& ((phashChunk # 2 # 1 # pconstant secondChunk) #== pconstant secondHash)
          #&& ((pcommitment # 2 # 4098 # peaksT (frontierFor 2 twoChunkBlob)) #== pconstant blobCommitment)
          #&& (pverifyChunk # pconstant blobCommitment # proofT proof)
          #&& pnot # (pverifyChunk # pconstant blobCommitment # proofT proof {pFieldIndex = 3})
          #&& pnot # (pverifyChunk # pconstant blobCommitment # proofT proof {pTotalLength = 4099})
          #&& pnot # (pverifyChunk # pconstant blobCommitment # proofT proof {pChunk = "\x01\x02\x04"})
          #&& ((pfromPreimage # 2 # pconstant twoChunkBlob) #== pconstant blobCommitment)
  , testCase "empty_blob_has_a_commitment_but_no_chunk_proof" $
      holds $
        (plengthBS # emptyCommitment #== 32)
          #&& ((pfromPreimage # 0 # pconstant "") #== emptyCommitment)
          #&& pnot # (pverifyChunk # emptyCommitment # proofT emptyProof)
  ]
  where
    proof = proofFor 2 twoChunkBlob 1
    emptyCommitment = pcommitment # 0 # 0 # peaksT []
    emptyProof =
      Proof
        { pVersion = 1
        , pFieldIndex = 0
        , pTotalLength = 0
        , pChunkIndex = 0
        , pChunk = ""
        , pFrontier = []
        , pSiblings = []
        }

--------------------------------------------------------------------------------
-- The chunking arithmetic
--------------------------------------------------------------------------------

arithmeticTests :: [TestTree]
arithmeticTests =
  [ testCase "counts chunks at and around every boundary" $
      holds $
        pall'
          [ (pchunkCount # pconstant n) #== pconstant expected
          | (n, expected) <-
              [ (0, 0) -- the empty blob has no chunks at all
              , (1, 1)
              , (4094, 1)
              , (4095, 1)
              , (4096, 2)
              , (8189, 2)
              , (8190, 2)
              , (8191, 3)
              ]
          ]
  , testCase "…and agrees with the reference across a wider range" $
      holds $
        pall'
          [ (pchunkCount # pconstant (fromIntegral n))
            #== pconstant (fromIntegral (chunkCount n))
          | n <- [0 .. 3] <> [4093 .. 4098] <> [8188 .. 8193] <> [12285, 12286, 40950]
          ]
  , testCase "a negative length has no chunk count" $
      pfails $ pchunkCount # (-1)
  , testCase "every chunk is full except the last" $
      holds $
        pall'
          [ (pexpectedChunkLength # pconstant (fromIntegral total) # pconstant (fromIntegral i))
            #== pconstant (fromIntegral (expectedChunkLength total i))
          | total <- [1, 4094, 4095, 4096, 8190, 8191]
          , i <- [0 .. chunkCount total - 1]
          ]
  , testCase "an index at or past the count has no expected length" $
      pfails $ pexpectedChunkLength # 4096 # 2
  , -- The empty blob has no chunk zero, which is the count difference showing up.
    testCase "…and neither does chunk zero of an empty blob" $
      pfails $ pexpectedChunkLength # 0 # 0
  ]

--------------------------------------------------------------------------------
-- The golden hashes
--------------------------------------------------------------------------------

{- | The three values @bounded-blob-v1.test.ak@ pins.

Asserted against the port and against this module's reference separately, so a
disagreement says which of the two moved.
-}
goldenTests :: [TestTree]
goldenTests =
  [ testCase "the port hashes the first chunk to Aiken's value" $
      holds $ (phashChunk # 2 # 0 # pconstant firstChunk) #== pconstant firstHash
  , testCase "…and the reference agrees" $ hashChunk 2 0 firstChunk @?= firstHash
  , testCase "the port hashes the second chunk to Aiken's value" $
      holds $ (phashChunk # 2 # 1 # pconstant secondChunk) #== pconstant secondHash
  , testCase "…and the reference agrees" $ hashChunk 2 1 secondChunk @?= secondHash
  , testCase "the port commits the two-chunk blob to Aiken's value" $
      holds $
        (pcommitment # 2 # 4098 # peaksT (frontierFor 2 twoChunkBlob)) #== pconstant blobCommitment
  , testCase "…and the reference agrees" $ commitment 2 4098 (frontierFor 2 twoChunkBlob) @?= blobCommitment
  , testCase "…and fromPreimage reaches the same value from the bytes alone" $
      holds $ (pfromPreimage # 2 # pconstant twoChunkBlob) #== pconstant blobCommitment
  ]

firstChunk, secondChunk, twoChunkBlob :: BS.ByteString
firstChunk = BS.replicate 4095 90
secondChunk = "\x01\x02\x03"
twoChunkBlob = firstChunk <> secondChunk

firstHash, secondHash, blobCommitment :: BS.ByteString
firstHash = hex "0d56e8ba6350807a8760329ede34888c9c63884515d40ff9a809a40c9122198a"
secondHash = hex "08790d8ac85ae960107a2dcded04a2935110432c13f1ebbbf5a7c9de8eadc290"
blobCommitment = hex "61a3c28584495eef1a5c76661f772158a460356921e30d5ffbbdae23d4894959"

--------------------------------------------------------------------------------
-- Against the reference
--------------------------------------------------------------------------------

{- | The whole scheme over a spread of blob shapes, and the positional properties.

Position lives inside every hash rather than beside it, so each coordinate is
moved once and the result has to change. Without that a chunk could be replayed
at another index or under another field.
-}
referenceTests :: [TestTree]
referenceTests =
  [ testCase "fromPreimage matches the reference at every shape" $
      holds $
        pall'
          [ (pfromPreimage # 3 # pconstant blob) #== pconstant (fromPreimage 3 blob)
          | blob <- blobShapes
          ]
  , testCase "…and under a different field index" $
      holds $
        pall'
          [ (pfromPreimage # 0 # pconstant blob) #== pconstant (fromPreimage 0 blob)
          | blob <- blobShapes
          ]
  , testCase "the same bytes at a different chunk index hash differently" $
      holds $
        pnot #$ (phashChunk # 2 # 0 # pconstant secondChunk)
          #== (phashChunk # 2 # 1 # pconstant secondChunk)
  , testCase "…and under a different field index" $
      holds $
        pnot #$ (phashChunk # 2 # 0 # pconstant secondChunk)
          #== (phashChunk # 3 # 0 # pconstant secondChunk)
  , testCase "the same frontier under a different length commits differently" $
      holds $
        pnot #$ (pcommitment # 2 # 4098 # peaksT (frontierFor 2 twoChunkBlob))
          #== (pcommitment # 2 # 4097 # peaksT (frontierFor 2 twoChunkBlob))
  , testCase "…and under a different field index" $
      holds $
        pnot #$ (pcommitment # 2 # 4098 # peaksT (frontierFor 2 twoChunkBlob))
          #== (pcommitment # 3 # 4098 # peaksT (frontierFor 3 twoChunkBlob))
  , -- The domain strings are what keep the blob scheme apart from the item one,
    -- which is otherwise the same arithmetic over the same frontier.
    testCase "a blob chunk hash is not a bounded-item chunk hash" $
      holds $
        pnot #$ (phashChunk # 2 # 0 # pconstant secondChunk)
          #== pconstant (itemStyleChunkHash 2 0 secondChunk)
  , testCase "a field index outside the field count has no hash" $
      pfails $ phashChunk # 64 # 0 # pconstant secondChunk
  , testCase "…and no commitment" $
      pfails $ pcommitment # 64 # 4098 # peaksT (frontierFor 2 twoChunkBlob)
  , testCase "an over-long chunk has no hash" $
      pfails $ phashChunk # 2 # 0 # pconstant (BS.replicate (chunkBytes + 1) 0x01)
  , testCase "a frontier that does not match the count has no commitment" $
      pfails $ pcommitment # 2 # 4098 # peaksT (frontierFor 2 firstChunk)
  ]

{- | Blob lengths worth commiting to: nothing, a byte, both sides of one chunk,
both sides of two, and a three-chunk blob with a remainder.
-}
blobShapes :: [BS.ByteString]
blobShapes =
  [ ""
  , "\x01"
  , BS.replicate 4094 0x02
  , BS.replicate 4095 0x03
  , BS.replicate 4096 0x04
  , BS.replicate 8190 0x05
  , BS.replicate 8191 0x06
  , BS.replicate 9000 0x07
  ]

--------------------------------------------------------------------------------
-- verifyChunk
--------------------------------------------------------------------------------

verifyTests :: [TestTree]
verifyTests =
  [ testCase "accepts every chunk of every shape that has one" $
      holds $
        pall'
          [ pverifyChunk # pconstant (fromPreimage 3 blob) # proofT (proofFor 3 blob i)
          | blob <- blobShapes
          , i <- [0 .. chunkCount (BS.length blob) - 1]
          ]
  , testCase "refuses a chunk offered against another blob's commitment" $
      refuses $
        pverifyChunk
          # pconstant (fromPreimage 3 (BS.replicate 4096 0x04))
          # proofT (proofFor 3 twoChunkBlob 1)
  , testCase "refuses a proof filed under the wrong field index" $
      refuses $
        pverifyChunk
          # pconstant (fromPreimage 2 twoChunkBlob)
          # proofT (proofFor 2 twoChunkBlob 1) {pFieldIndex = 3}
  , testCase "refuses a proof claiming the wrong total length" $
      refuses $
        pverifyChunk
          # pconstant (fromPreimage 2 twoChunkBlob)
          # proofT (proofFor 2 twoChunkBlob 1) {pTotalLength = 4099}
  , testCase "refuses a proof whose chunk bytes have been changed" $
      refuses $
        pverifyChunk
          # pconstant (fromPreimage 2 twoChunkBlob)
          # proofT (proofFor 2 twoChunkBlob 1) {pChunk = "\x01\x02\x04"}
  , testCase "refuses a chunk moved to another index" $
      refuses $
        pverifyChunk
          # pconstant (fromPreimage 2 twoChunkBlob)
          # proofT (proofFor 2 twoChunkBlob 1) {pChunkIndex = 0}
  , testCase "refuses a proof at a version that is not this one" $
      refuses $
        pverifyChunk
          # pconstant (fromPreimage 2 twoChunkBlob)
          # proofT (proofFor 2 twoChunkBlob 1) {pVersion = 2}
  , testCase "refuses a commitment that is not 32 bytes" $
      refuses $ pverifyChunk # pconstant "\x00" # proofT (proofFor 2 twoChunkBlob 1)
  , {- Short-circuiting. Each of these would abort under a strict conjunction:
       the index is past the count, so `expectedChunkLength` errors; the chunk is
       longer than a chunk, so `hashChunk` errors; the frontier does not match the
       count, so `commitment` errors. Aiken returns False for all three. -}
    testCase "refuses a chunk index past the count without aborting" $
      refuses $
        pverifyChunk
          # pconstant (fromPreimage 2 twoChunkBlob)
          # proofT (proofFor 2 twoChunkBlob 1) {pChunkIndex = 2}
  , testCase "refuses an over-long chunk without aborting" $
      refuses $
        pverifyChunk
          # pconstant (fromPreimage 2 twoChunkBlob)
          # proofT (proofFor 2 twoChunkBlob 1) {pChunk = BS.replicate (chunkBytes + 1) 0x01}
  , testCase "refuses a malformed frontier without aborting" $
      refuses $
        pverifyChunk
          # pconstant (fromPreimage 2 twoChunkBlob)
          # proofT (proofFor 2 twoChunkBlob 1) {pFrontier = []}
  , testCase "refuses a field index outside the field count without aborting" $
      refuses $
        pverifyChunk
          # pconstant (fromPreimage 2 twoChunkBlob)
          # proofT (proofFor 2 twoChunkBlob 1) {pFieldIndex = 64}
  , testCase "refuses a sibling path that is not the chunk's" $
      refuses $
        pverifyChunk
          # pconstant (fromPreimage 2 twoChunkBlob)
          # proofT (proofFor 2 twoChunkBlob 1) {pSiblings = [BS.replicate 32 0x00]}
  ]

--------------------------------------------------------------------------------
-- The empty blob
--------------------------------------------------------------------------------

{- | It has a commitment, and it has no proof.

This is where the module parts company with 'Testing.BoundedItem', so both halves
are stated: the commitment exists and is 32 bytes, and every proof against it is
refused rather than aborting.
-}
emptyTests :: [TestTree]
emptyTests =
  [ testCase "an empty blob commits to the reference's value" $
      holds $ (pfromPreimage # 0 # pconstant "") #== pconstant (fromPreimage 0 "")
  , testCase "…which is the empty frontier's commitment at length zero" $
      holds $ (pfromPreimage # 0 # pconstant "") #== (pcommitment # 0 # 0 # peaksT [])
  , testCase "…and is 32 bytes" $
      BS.length (fromPreimage 0 "") @?= 32
  , testCase "…and differs between fields" $
      holds $ pnot #$ (pfromPreimage # 0 # pconstant "") #== (pfromPreimage # 1 # pconstant "")
  , testCase "no chunk proof holds against it" $
      refuses $
        pverifyChunk
          # pconstant (fromPreimage 0 "")
          # proofT
            Proof
              { pVersion = 1
              , pFieldIndex = 0
              , pTotalLength = 0
              , pChunkIndex = 0
              , pChunk = ""
              , pFrontier = []
              , pSiblings = []
              }
  ]

--------------------------------------------------------------------------------
-- The scheme, reimplemented from bounded-blob-v1.ak
--------------------------------------------------------------------------------

-- | Zero for zero, which is the whole difference from the bounded-item count.
chunkCount :: Int -> Int
chunkCount 0 = 0
chunkCount n = (n + chunkBytes - 1) `div` chunkBytes

expectedChunkLength :: Int -> Int -> Int
expectedChunkLength totalLength chunkIndex
  | chunkIndex + 1 < chunkCount totalLength = chunkBytes
  | otherwise = totalLength - chunkIndex * chunkBytes

chunkAt :: BS.ByteString -> Int -> BS.ByteString
chunkAt bytes i =
  BS.take (expectedChunkLength (BS.length bytes) i) (BS.drop (i * chunkBytes) bytes)

hashChunk :: Integer -> Integer -> BS.ByteString -> BS.ByteString
hashChunk fieldIndex chunkIndex chunk =
  blake2b256 $
    BS.concat
      [ "MidgardBoundedBlobChunkV1"
      , arrayHeader 4
      , cborInt 1
      , cborInt fieldIndex
      , cborInt chunkIndex
      , definiteBytes chunk
      ]

{- | The bounded-/item/ preimage for the same chunk.

Only used to show that the two schemes cannot collide: same arithmetic, same
frontier, different domain string and one more field.
-}
itemStyleChunkHash :: Integer -> Integer -> BS.ByteString -> BS.ByteString
itemStyleChunkHash fieldIndex chunkIndex chunk =
  blake2b256 $
    BS.concat
      [ "MidgardBoundedItemChunkV1"
      , arrayHeader 5
      , cborInt 1
      , cborInt fieldIndex
      , cborInt 0
      , cborInt chunkIndex
      , definiteBytes chunk
      ]

commitment :: Integer -> Integer -> [(Integer, BS.ByteString)] -> BS.ByteString
commitment fieldIndex totalLength frontier =
  blake2b256 $
    BS.concat
      [ "MidgardBoundedBlobCommitmentV1"
      , arrayHeader 4
      , cborInt 1
      , cborInt fieldIndex
      , cborInt totalLength
      , definiteBytes (frontierCommitment count frontier)
      ]
  where
    count = fromIntegral (chunkCount (fromIntegral totalLength))

chunkLeaves :: Integer -> BS.ByteString -> [BS.ByteString]
chunkLeaves fieldIndex bytes =
  [ hashChunk fieldIndex (fromIntegral i) (chunkAt bytes i)
  | i <- [0 .. chunkCount (BS.length bytes) - 1]
  ]

frontierFor :: Integer -> BS.ByteString -> [(Integer, BS.ByteString)]
frontierFor fieldIndex bytes = snd (buildFrontier (chunkLeaves fieldIndex bytes))

fromPreimage :: Integer -> BS.ByteString -> BS.ByteString
fromPreimage fieldIndex bytes =
  commitment fieldIndex (fromIntegral (BS.length bytes)) (frontierFor fieldIndex bytes)

--------------------------------------------------------------------------------
-- Proofs
--------------------------------------------------------------------------------

data Proof = Proof
  { pVersion :: Integer
  , pFieldIndex :: Integer
  , pTotalLength :: Integer
  , pChunkIndex :: Integer
  , pChunk :: BS.ByteString
  , pFrontier :: [(Integer, BS.ByteString)]
  , pSiblings :: [BS.ByteString]
  }

-- | The honest proof for one chunk of a blob.
proofFor :: Integer -> BS.ByteString -> Int -> Proof
proofFor fieldIndex bytes i =
  Proof
    { pVersion = 1
    , pFieldIndex = fieldIndex
    , pTotalLength = fromIntegral (BS.length bytes)
    , pChunkIndex = fromIntegral i
    , pChunk = chunkAt bytes i
    , pFrontier = frontierFor fieldIndex bytes
    , pSiblings = membershipPath (chunkLeaves fieldIndex bytes) i
    }

--------------------------------------------------------------------------------
-- Plumbing
--------------------------------------------------------------------------------

proofT :: forall (s :: S). Proof -> Term s PChunkProofV1
proofT p =
  pfromData $
    punsafeCoerce @(PAsData PChunkProofV1) $
      pconstant @PData $
        PD.Constr
          0
          [ PD.I (pVersion p)
          , PD.I (pFieldIndex p)
          , PD.I (pTotalLength p)
          , PD.I (pChunkIndex p)
          , PD.B (pChunk p)
          , PD.List [PD.Constr 0 [PD.I h, PD.B hash] | (h, hash) <- pFrontier p]
          , PD.List (map PD.B (pSiblings p))
          ]

holds :: (forall (s :: S). Term s PBool) -> Assertion
holds = passertEval

refuses :: (forall (s :: S). Term s PBool) -> Assertion
refuses p = passertEval (pnot # p)

pall' :: forall (s :: S). [Term s PBool] -> Term s PBool
pall' = foldr (#&&) (pconstant True)

hex :: BS.ByteString -> BS.ByteString
hex = either error id . Base16.decode
