{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Testing.DaAttestationSignatures
Description : Tests for the signature and bitmap layer of
              @validators/da-attestation.ak@.

The property this layer exists to guarantee is that a threshold counts
/distinct/ committee members. Two independent mechanisms enforce it — the bitmap
refuses an already-set bit, and signer indices must strictly ascend — so both
are tested separately as well as together. Either alone would be enough today;
testing them separately is what keeps a future simplification from silently
removing the only one that was load-bearing.

Signatures are real Ed25519, generated here with `cardano-crypto-class`, so the
verification step is exercised rather than stubbed.
-}
module Testing.DaAttestationSignatures (tests) where

import Data.ByteString qualified as BS
import Test.Tasty
import Test.Tasty.HUnit

import Cardano.Crypto.DSIGN.Class qualified as DSIGN
import Cardano.Crypto.DSIGN.Ed25519 (Ed25519DSIGN)
import Cardano.Crypto.Seed (mkSeedFromBytes)

import Plutarch.LedgerApi.V3 (PTokenName (..))
import Plutarch.Prelude

import Midgard.DaAttestation.Signatures (
  pattestationAssetName,
  pattestationMessage,
  psetAttestedSigner,
  psignerBitIsClear,
  psignerBitMask,
  pverifyIndexedSignatures,
 )
import Testing.Eval (passertEval, pfails)

-- | Collects the tests defined in this module.
tests :: TestTree
tests =
  testGroup
    "DA Attestation Signature Tests"
    [ nameTests
    , bitmapTests
    , verifyTests
    ]

--------------------------------------------------------------------------------
-- Names and messages
--------------------------------------------------------------------------------

nameTests :: TestTree
nameTests =
  testGroup
    "names and messages"
    [ testCase "the asset name is DAAT ++ header hash" $
        holds $
          pattestationAssetName # pconstant headerHash
            #== pdata (pcon (PTokenName (pconstant ("DAAT" <> headerHash))))
    , -- 4 + 28 exactly fills Cardano's 32-byte asset-name limit; anything else
      -- would either overflow it or let two headers collide under padding.
      testCase "rejects a header hash that is not 28 bytes" $
        pfails $ pattestationAssetName # pconstant (BS.replicate 32 0x01)
    , testCase "rejects a short header hash" $
        pfails $ pattestationAssetName # pconstant (BS.replicate 27 0x01)
    , -- The domain-separating prefix is what stops a signature over a bare hash
      -- being replayed anywhere else that asks a member to sign a hash.
      testCase "the signed message carries a domain-separating prefix" $
        holds $
          pattestationMessage # pconstant headerHash
            #== pconstant ("MidgardDAAttestationV1" <> headerHash)
    ]

--------------------------------------------------------------------------------
-- The bitmap
--------------------------------------------------------------------------------

bitmapTests :: TestTree
bitmapTests =
  testGroup
    "attested-signer bitmap"
    [ -- Most-significant-first within each byte.
      testCase "the mask selects the right bit within its byte" $
        holds $
          pand'ListT
            [ (psignerBitMask # 0) #== 128
            , (psignerBitMask # 1) #== 64
            , (psignerBitMask # 7) #== 1
            , (psignerBitMask # 8) #== 128
            , (psignerBitMask # 15) #== 1
            ]
    , testCase "a clear bit reads as clear and a set one does not" $
        holds $
          pand'ListT
            [ psignerBitIsClear # 0 # 128
            , pnot #$ psignerBitIsClear # 128 # 128
            , psignerBitIsClear # 128 # 64
            , pnot #$ psignerBitIsClear # 255 # 1
            ]
    , testCase "setting signer 0 sets the top bit of the first byte" $
        holds $ setBits [0] #== pconstant (bitmapWith [0])
    , testCase "setting signer 8 sets the top bit of the second byte" $
        holds $ setBits [8] #== pconstant (bitmapWith [8])
    , testCase "setting several signers accumulates" $
        holds $ setBits [0, 3, 9, 255] #== pconstant (bitmapWith [0, 3, 9, 255])
    , testCase "the bitmap keeps its width" $
        holds $ (plengthBS #$ setBits [0, 200]) #== 32
    , -- This is the check that makes a threshold count distinct members.
      testCase "refuses a bit that is already set" $
        pfails $ psetAttestedSigner # pconstant (bitmapWith [5]) # 5
    , testCase "rejects a bitmap of the wrong width" $
        pfails $ psetAttestedSigner # pconstant (BS.replicate 31 0x00) # 0
    ]
  where
    setBits :: forall s. [Integer] -> Term s PByteString
    setBits =
      foldl
        (\acc i -> psetAttestedSigner # acc # pconstant i)
        (pconstant emptyBitmap)

--------------------------------------------------------------------------------
-- verify_indexed_signatures
--------------------------------------------------------------------------------

verifyTests :: TestTree
verifyTests =
  testGroup
    "verifyIndexedSignatures"
    [ testCase "accepts one valid signature and records its signer" $
        holds $ verify (witness [0]) #== pconstant (bitmapWith [0])
    , testCase "accepts several in ascending order" $
        holds $ verify (witness [0, 1, 2]) #== pconstant (bitmapWith [0, 1, 2])
    , testCase "accepts a subset of the committee" $
        holds $ verify (witness [1]) #== pconstant (bitmapWith [1])
    , -- Guard one: indices must strictly ascend, so a repeat cannot be admitted
      -- even before the bitmap sees it.
      testCase "rejects a repeated signer index" $
        pfails $ verify (witness [1, 1])
    , testCase "rejects descending signer indices" $
        pfails $ verify (witness [2, 1])
    , -- Guard two, reached independently: a signer already in the incoming
      -- bitmap cannot be added again, which is what makes AddSignatures
      -- idempotent-proof across transactions.
      testCase "rejects a signer already recorded in the incoming bitmap" $
        pfails $
          pverifyIndexedSignatures
            # pconstant (witness [1])
            # (pattestationMessage # pconstant headerHash)
            # pconstant committee
            # pconstant (bitmapWith [1])
            # 0
            # 0
            # pconstant (fromIntegral (BS.length (witness [1])))
    , testCase "rejects an index outside the committee" $
        pfails $ verify (witnessFor [3] [0, 1, 2])
    , -- The signature itself has to be real; a wrong key is what a forged
      -- attestation would look like.
      testCase "rejects a signature made by another key" $
        pfails $ verify (witnessWrongKey 0)
    , testCase "rejects a signature over another message" $
        pfails $
          pverifyIndexedSignatures
            # pconstant (witness [0])
            # (pattestationMessage # pconstant otherHeaderHash)
            # pconstant committee
            # pconstant emptyBitmap
            # 0
            # 0
            # pconstant (fromIntegral (BS.length (witness [0])))
    , testCase "an empty witness sequence records nothing" $
        holds $ verify BS.empty #== pconstant emptyBitmap
    ]
  where
    verify :: forall s. BS.ByteString -> Term s PByteString
    verify w =
      pverifyIndexedSignatures
        # pconstant w
        # (pattestationMessage # pconstant headerHash)
        # pconstant committee
        # pconstant emptyBitmap
        # 0
        # 0
        # pconstant (fromIntegral (BS.length w))

--------------------------------------------------------------------------------
-- Assertions
--------------------------------------------------------------------------------

holds :: (forall s. Term s PBool) -> Assertion
holds = passertEval

pand'ListT :: forall s. [Term s PBool] -> Term s PBool
pand'ListT = foldr (#&&) (pconstant True)

--------------------------------------------------------------------------------
-- Fixtures
--------------------------------------------------------------------------------

headerHash, otherHeaderHash :: BS.ByteString
headerHash = BS.replicate 28 0xaa
otherHeaderHash = BS.replicate 28 0xbb

emptyBitmap :: BS.ByteString
emptyBitmap = BS.replicate 32 0x00

{- | The 32-byte bitmap with exactly these signer bits set.

Built independently of the on-chain code — most-significant bit first within
each byte — so that a change to either side's bit order fails a test rather than
two copies agreeing.
-}
bitmapWith :: [Int] -> BS.ByteString
bitmapWith indices =
  BS.pack [byteAt i | i <- [0 .. 31]]
  where
    byteAt i =
      foldr
        (\b acc -> acc + bitValue b)
        0
        [b | b <- indices, b `div` 8 == i]
    bitValue b = 2 ^ (7 - (b `mod` 8))

--------------------------------------------------------------------------------
-- Ed25519 keys and witnesses
--------------------------------------------------------------------------------

-- | A deterministic signing key per committee index.
signKeyFor :: Int -> DSIGN.SignKeyDSIGN Ed25519DSIGN
signKeyFor i = DSIGN.genKeyDSIGN (mkSeedFromBytes (BS.replicate 32 (fromIntegral i)))

verKeyFor :: Int -> BS.ByteString
verKeyFor = DSIGN.rawSerialiseVerKeyDSIGN . DSIGN.deriveVerKeyDSIGN . signKeyFor

-- | Three committee members, packed as 32-byte keys.
committee :: BS.ByteString
committee = BS.concat (map verKeyFor [0, 1, 2])

signWith :: Int -> BS.ByteString -> BS.ByteString
signWith i msg =
  DSIGN.rawSerialiseSigDSIGN (DSIGN.signDSIGN () msg (signKeyFor i))

-- | The message the committee signs for 'headerHash'.
attestedMessage :: BS.ByteString
attestedMessage = "MidgardDAAttestationV1" <> headerHash

{- | A packed witness sequence: one index byte then a 64-byte signature, each.

@witnessFor@ lets the declared index differ from the key that signed, which is
how the out-of-committee case is built.
-}
witness :: [Int] -> BS.ByteString
witness indices = witnessFor indices indices

witnessFor :: [Int] -> [Int] -> BS.ByteString
witnessFor declared signers =
  BS.concat
    [ BS.cons (fromIntegral d) (signWith s attestedMessage)
    | (d, s) <- zip declared signers
    ]

-- | An index claiming one member, signed by another.
witnessWrongKey :: Int -> BS.ByteString
witnessWrongKey i = BS.cons (fromIntegral i) (signWith (i + 1) attestedMessage)
