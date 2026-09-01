{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Testing.NativeTxCarriage
Description : Behavioural tests for the Plutarch port of
              @lib/midgard/native-tx-carriage-v1.ak@.

The producer half of the §8 carriage ladder: the deterministic split rule and the
§8.6 certification predicate.

__The split rule is tested as a rule, not as an example.__ Its whole value is that
two publishers who never met produce identical chunks, so the reference below
implements §8.4 independently and the port is checked against it at every
boundary around @K@ and around the aggregate cap — the places an off-by-one would
live and nowhere else.

__§2.4's transposition is driven in both directions.__ Wire position 6 is
/script/ witnesses and 7 is /address/ witnesses, the opposite of the record's
declaration order. A table that transposed only one of the pair passes a test
that certifies the script preimage under 6; a table that transposed both passes
one that certifies the address preimage under 7. Both are asserted, in both
polarities, so neither mistake survives.

__The transaction is real.__ The certification cases build a genuine compact
structure from "Testing.FraudProofsFixture", so the tx-id is re-derived by the
port through the §3 derivation rather than accepted, and the witness set is bound
by its own hash. A certificate naming another transaction is refused before any
byte of carriage is looked at, and one test pins exactly that ordering.
-}
module Testing.NativeTxCarriage (tests) where

import Data.ByteString qualified as BS
import Data.ByteString.Base16 qualified as Base16
import PlutusLedgerApi.V1.Address (scriptHashAddress)
import PlutusLedgerApi.V3 (
  Datum (..),
  OutputDatum (..),
  ScriptHash (..),
  TxId (..),
  TxInInfo (..),
  TxOut (..),
  TxOutRef (..),
 )
import PlutusTx.Builtins (dataToBuiltinData, toBuiltin)
import PlutusCore.Data qualified as PD
import PlutusTx.IsData qualified as PlutusTx
import Test.Tasty
import Test.Tasty.HUnit

import Plutarch.LedgerApi.V3 (PPubKeyHash, PTxInInfo)
import Plutarch.Prelude
import Plutarch.Unsafe (punsafeCoerce)

import Midgard.NativeTxCarriage (
  pfieldPreimageCertificateV1,
  pfieldPreimageChunkDigests,
  prawChunkBytes,
  psplitFieldPreimage,
  pverifyFieldPreimageCertificateV1,
 )
import Midgard.NativeTxFieldAccess (
  PFieldPreimageCertificateV1 (..),
  pfieldCommitment,
  pfieldPreimageCertificateAssetName,
 )
import Testing.Eval (passertEval, pfails)
import Testing.FraudProofsFixture (
  Tx,
  blake2b256,
  cborInt,
  compactWith,
  defBytes32,
  tx1,
  tx2,
  txIdOf,
  witnessSetCborFrom,
 )

tests :: TestTree
tests =
  testGroup
    "Native Tx Carriage Tests"
    [ testGroup "the §8.4 split rule" splitTests
    , testGroup "building a certificate" buildTests
    , testGroup "raw carriage" rawTests
    , testGroup "§8.6 certification" certifyTests
    , testGroup "§8.7 healing" healingTests
    , testGroup "§2.4's transposition" transpositionTests
    ]

--------------------------------------------------------------------------------
-- The split rule
--------------------------------------------------------------------------------

{- | Chunk @j@ is bytes @[j·K, (j+1)·K)@, ragged last chunk, minimum chunks.

The boundaries are @K@ itself, one either side of it, and the same around two and
three chunks — plus the aggregate cap, which is where the certificate's own bound
stops the ladder.
-}
splitTests :: [TestTree]
splitTests =
  [ testCase ("splits " <> show n <> " bytes as the reference does") $
    holds $ (psplitFieldPreimage # pconstant (filler n)) #== chunksT (splitRef (filler n))
  | n <- splitBoundaries
  ]
    <> [ testCase "…and the chunk counts are what the rule says" $
          map (length . splitRef . filler) splitBoundaries
            @?= [1, 1, 1, 2, 2, 2, 3, 3, 3]
       , testCase "every chunk but the last is exactly K" $
          all fullExceptLast (map (splitRef . filler) splitBoundaries)
            @? "a chunk before the last was not K bytes"
       , testCase "the chunks reassemble to the preimage" $
          all (\n -> BS.concat (splitRef (filler n)) == filler n) splitBoundaries
            @? "a split did not rejoin to its preimage"
       , testCase "an empty preimage has no split" $
          pfails $ psplitFieldPreimage # pconstant ""
       , testCase "…and one past the aggregate cap has none either" $
          pfails $ psplitFieldPreimage # pconstant (filler (aggregateCap + 1))
       , testCase "the digests are the chunks' hashes in order" $
          holds $
            (pfieldPreimageChunkDigests # pconstant preimage)
              #== chunksT (map blake2b256 (splitRef preimage))
       , testCase "split_matches_the_cross_language_straddle_vector" $
          holds straddleVectorMatches
       ]
  where
    fullExceptLast chunks = all ((== chunkK) . BS.length) (init chunks)

splitBoundaries :: [Int]
splitBoundaries =
  [ 1
  , chunkK - 1
  , chunkK
  , chunkK + 1
  , 2 * chunkK - 1
  , 2 * chunkK
  , 2 * chunkK + 1
  , aggregateCap - 1
  , aggregateCap
  ]

--------------------------------------------------------------------------------
-- Building a certificate
--------------------------------------------------------------------------------

buildTests :: [TestTree]
buildTests =
  [ testCase "the built certificate is the one certification accepts" $
      holds $ builtCertificate #== certificateT preimage
  , testCase "…and its digests are the split's" $
      holds $
        pmatch builtCertificate (\PFieldPreimageCertificateV1 {pcert'chunkDigests} -> pcert'chunkDigests)
          #== pdata (chunksT (map blake2b256 (splitRef preimage)))
  , {- The tier boundary is a partition, not a preference: a preimage that fits
       tier 1 or tier 2 cannot have a tier-3 certificate brought into existence
       at all. -}
    testCase "a preimage of exactly K has no certificate" $
      pfails $ buildWith owner (txIdOf tx1) 6 (filler chunkK)
  , testCase "…nor one below it" $
      pfails $ buildWith owner (txIdOf tx1) 6 (filler 1)
  , testCase "one byte over K does" $
      holds $ certificateIsBuilt (buildWith owner (txIdOf tx1) 6 (filler (chunkK + 1)))
  , -- The asset-name derivation is what enforces the field index and the tx-id.
    testCase "a field index outside 0..8 has no certificate" $
      pfails $ buildWith owner (txIdOf tx1) 9 preimage
  , testCase "…nor a negative one" $
      pfails $ buildWith owner (txIdOf tx1) (-1) preimage
  , testCase "a tx-id that is not 32 bytes has no certificate" $
      pfails $ buildWith owner (BS.take 31 (txIdOf tx1)) 6 preimage
  , testCase "an owner that is not a key hash has no certificate" $
      pfails $ buildWith (BS.take 27 owner) (txIdOf tx1) 6 preimage
  ]

--------------------------------------------------------------------------------
-- Raw carriage
--------------------------------------------------------------------------------

{- | §8.5 carriage is unauthenticated: any address, any key, anyone.

Nothing here trusts provenance, so the tests say so — the same bytes at a script
address with no owner in sight read back identically.
-}
rawTests :: [TestTree]
rawTests =
  [ testCase "reads a chunk out of an inline datum" $
      holds $ (prawChunkBytes # refInputsT # 0) #== pconstant (splitRef preimage !! 0)
  , testCase "…and the one after it" $
      holds $ (prawChunkBytes # refInputsT # 1) #== pconstant (splitRef preimage !! 1)
  , testCase "an index past the end aborts" $ pfails $ prawChunkBytes # refInputsT # 9
  , testCase "a negative index aborts" $ pfails $ prawChunkBytes # refInputsT # (-1)
  , testCase "a datum that is not bytes aborts" $
      pfails $ prawChunkBytes # inputList [chunkInput (PD.I 0)] # 0
  , testCase "an output with no inline datum aborts" $
      pfails $ prawChunkBytes # inputList [noDatumInput] # 0
  ]

--------------------------------------------------------------------------------
-- Certification
--------------------------------------------------------------------------------

certifyTests :: [TestTree]
certifyTests =
  [ testCase "an honest certificate for field 8 is accepted" $
      holds $ certify 8 preimage certifiedCase
  , testGroup
      "certification_binds_each_field_to_its_positional_commitment"
      [ testCase ("field " <> show fieldIndex) $
          holds (positionalFieldIsAccepted fieldIndex)
      | fieldIndex <- [0 .. 8]
      ]
  , testCase "…and for a three-chunk preimage" $
      holds $ certify 8 bigPreimage (caseFor 8 bigPreimage tx1)
  , {- The binding is checked before any byte of carriage. A certificate naming
       another transaction is refused whatever chunks accompany it. -}
    testCase "a certificate naming another transaction aborts" $
      pfails $
        pverifyFieldPreimageCertificateV1
          # certificateFor (txIdOf tx2) 8 preimage
          # pconstant (cCompact certifiedCase)
          # pconstant (cWitnessSet certifiedCase)
          # chunksT (splitRef preimage)
  , testCase "a witness set that is not the committed one aborts" $
      pfails $
        pverifyFieldPreimageCertificateV1
          # certificateT preimage
          # pconstant (cCompact certifiedCase)
          # pconstant (witnessSetCborFrom (h 0x01, h 0x02, h 0x03))
          # chunksT (splitRef preimage)
  , testCase "a preimage the field does not commit is refused" $
      refuses $ certify 8 otherPreimage certifiedCase
  , testCase "certification_rejects_a_reordered_chunk_vector" $
      refuses $
        verifyWith
          reorderedCertificate
          reorderedCase
          (reverse (splitRef reorderedPreimage))
  , {- From here down the guards are `expect` chains in the original, so they
       abort rather than answering False. Only the final field-commitment
       comparison is a Bool, which is why the preimage-mismatch case above
       refuses and these do not. -}
    testCase "chunks that do not match the certificate's digests abort" $
      pfails $ verifyWith (certificateT preimage) certifiedCase (splitRef otherPreimage)
  , {- The shape check. These chunks hash to the right digests only if the digests
       are recomputed from them, which is why the certificate keeps its own — a
       lopsided split whose boundaries no consumer could compute must be refused
       even when it reassembles correctly. -}
    testCase "a lopsided split of the right bytes aborts" $
      pfails $ verifyWith (lopsidedCertificate preimage) certifiedCase (lopsided preimage)
  , testCase "a certificate claiming the wrong total length aborts" $
      pfails $
        verifyWith
          (certificateWithLength preimage (fromIntegral (BS.length preimage) - 1))
          certifiedCase
          (splitRef preimage)
  , testCase "a certificate for a tier-2 preimage aborts" $
      pfails $
        verifyWith (certificateWithLength preimage (fromIntegral chunkK)) certifiedCase (splitRef preimage)
  , testCase "…and one past the aggregate cap" $
      pfails $
        verifyWith
          (certificateWithLength preimage (fromIntegral aggregateCap + 1))
          certifiedCase
          (splitRef preimage)
  , testCase "a chunk list of the wrong length aborts" $
      pfails $ verifyWith (certificateT preimage) certifiedCase (take 1 (splitRef preimage))
  ]

--------------------------------------------------------------------------------
-- Healing
--------------------------------------------------------------------------------

healingTests :: [TestTree]
healingTests =
  [ testCase "republished_carriage_is_byte_identical_and_certifies" $
      holds republishedCarriageCertifies
  ]

republishedCarriageCertifies :: forall (s :: S). Term s PBool
republishedCarriageCertifies =
  plet (pconstant healingPreimage) $ \preimage' ->
    plet (buildWith healingOwner (cTxId healingCase) 8 healingPreimage) $ \original ->
      plet (buildWith healer (cTxId healingCase) 8 healingPreimage) $ \healed ->
        plet
          ( pcons
              # pdata (psliceBS # 0 # 15_900 # preimage')
              #$ pcons
                # pdata
                  ( psliceBS
                      # 15_900
                      # (plengthBS # preimage' - 15_900)
                      # preimage'
                  )
                # pnil
          )
          $ \republishedChunks ->
            pmatch original $ \PFieldPreimageCertificateV1 {pcert'txId = originalTxId, pcert'fieldIndex = originalFieldIndex, pcert'totalLength = originalLength, pcert'chunkDigests = originalDigests} ->
              pmatch healed $ \PFieldPreimageCertificateV1 {pcert'txId = healedTxId, pcert'fieldIndex = healedFieldIndex, pcert'totalLength = healedLength, pcert'chunkDigests = healedDigests} ->
                originalDigests #== healedDigests
                  #&& originalLength #== healedLength
                  #&& ( pfieldPreimageCertificateAssetName
                          # pfromData originalTxId
                          # pfromData originalFieldIndex
                      )
                    #== ( pfieldPreimageCertificateAssetName
                            # pfromData healedTxId
                            # pfromData healedFieldIndex
                        )
                  #&& ( pverifyFieldPreimageCertificateV1
                          # original
                          # pconstant (cCompact healingCase)
                          # pconstant (cWitnessSet healingCase)
                          # republishedChunks
                      )
                  #&& ( pverifyFieldPreimageCertificateV1
                          # healed
                          # pconstant (cCompact healingCase)
                          # pconstant (cWitnessSet healingCase)
                          # republishedChunks
                      )

--------------------------------------------------------------------------------
-- §2.4's transposition
--------------------------------------------------------------------------------

{- | Wire 6 is script witnesses; wire 7 is address witnesses.

Each preimage is committed at exactly one witness slot, then offered under both
field indices. A table that transposed one of the pair fails the first pair; one
that transposed both fails the second.
-}
transpositionTests :: [TestTree]
transpositionTests =
  [ testCase "the script preimage certifies under field 6" $
      holds $ certify 6 preimage (scriptSlotCase preimage)
  , testCase "…and not under field 7" $
      refuses $ certify 7 preimage (scriptSlotCase preimage)
  , testCase "the address preimage certifies under field 7" $
      holds $ certify 7 preimage (addressSlotCase preimage)
  , testCase "…and not under field 6" $
      refuses $ certify 6 preimage (addressSlotCase preimage)
  , testCase "the redeemer preimage certifies under field 8" $
      holds $ certify 8 preimage (redeemerSlotCase preimage)
  , testCase "…and not under either witness slot below it" $
      refuses $ certify 6 preimage (redeemerSlotCase preimage)
  , -- The body half of the table must not alias the witness half.
    testCase "a witness preimage does not certify under a body index" $
      refuses $ certify 0 preimage (redeemerSlotCase preimage)
  , testCase "…at any body index" $
      holds $
        pall'
          [pnot #$ certify i preimage (redeemerSlotCase preimage) | i <- [0 .. 5]]
  , testCase "a field index outside 0..8 aborts" $
      pfails $ certify 9 preimage (redeemerSlotCase preimage)
  ]

--------------------------------------------------------------------------------
-- The §8.4 split, reimplemented
--------------------------------------------------------------------------------

-- | @native_tx_field_access_v1.chunk_bytes_k@.
chunkK :: Int
chunkK = 15900

-- | @max_transaction_aggregate_field_bytes@.
aggregateCap :: Int
aggregateCap = 32768

-- | §8.4: chunk @j@ is @[j·K, (j+1)·K)@, ragged last chunk, minimum chunks.
splitRef :: BS.ByteString -> [BS.ByteString]
splitRef preimage'
  | BS.length preimage' <= chunkK = [preimage']
  | otherwise = BS.take chunkK preimage' : splitRef (BS.drop chunkK preimage')

-- | A split of the same bytes that is not §8.4's: the first chunk is short.
lopsided :: BS.ByteString -> [BS.ByteString]
lopsided preimage' = [BS.take n preimage', BS.drop n preimage']
  where
    n = chunkK - 1

filler :: Int -> BS.ByteString
filler n = BS.pack [fromIntegral (i `mod` 251) | i <- [0 .. n - 1]]

straddleVectorMatches :: forall (s :: S). Term s PBool
straddleVectorMatches =
  plet (psplitFieldPreimage # pconstant straddlePreimage) $ \chunks ->
    pmatch chunks $ \case
      PNil -> pconstant False
      PCons first rest -> pmatch rest $ \case
        PNil -> pconstant False
        PCons second trailing ->
          plengthBS # pconstant straddlePreimage #== 16_003
            #&& pfieldCommitment # pconstant straddlePreimage #== pconstant straddleCommitment
            #&& plengthBS # pfromData first #== 15_900
            #&& plengthBS # pfromData second #== 103
            #&& pnull # trailing
            #&& pfieldPreimageChunkDigests # pconstant straddlePreimage
              #== chunksT straddleChunkDigests

straddleBlock, straddlePreimage, straddleCommitment :: BS.ByteString
straddleBlock = hex
  "58268258201f262d343b424950575e656c737a81888f969da4abb2b9c0c7ced5dce3eaf1f819000058268258203e454c535a61686f767d848b9299a0a7aeb5bcc3cad1d8dfe6edf4fb0209101719000158268258205d646b727980878e959ca3aab1b8bfc6cdd4dbe2e9f0f7fe050c131a21282f3619000258268258207c838a91989fa6adb4bbc2c9d0d7dee5ecf3fa01080f161d242b323940474e5519000358268258209ba2a9b0b7bec5ccd3dae1e8eff6fd040b121920272e353c434a51585f666d741900045826825820bac1c8cfd6dde4ebf2f900070e151c232a31383f464d545b626970777e858c931900055826825820d9e0e7eef5fc030a11181f262d343b424950575e656c737a81888f969da4abb21900065826825820f8ff060d141b222930373e454c535a61686f767d848b9299a0a7aeb5bcc3cad11900075826825820171e252c333a41484f565d646b727980878e959ca3aab1b8bfc6cdd4dbe2e9f01900085826825820363d444b525960676e757c838a91989fa6adb4bbc2c9d0d7dee5ecf3fa01080f190009"
straddlePreimage = hex "990190" <> BS.concat (replicate 40 straddleBlock)
straddleCommitment = hex "c33cac158cd252aeb86e3fafb6776fd03e7afacffa7923c05878afca870b4ef1"

straddleChunkDigests :: [BS.ByteString]
straddleChunkDigests = map hex
  [ "3d472a8b6608fd6572a3de26a9f95a37c86c9ba739c39b5b8314f860bc908806"
  , "7243f0ea84ad415d83212fc24569d54be7e81f50198a257f93de50cfcb3d7cb2"
  ]

preimage, otherPreimage, bigPreimage, healingPreimage :: BS.ByteString
preimage = filler 20_000
otherPreimage = BS.map (+ 1) (filler 20_000)
bigPreimage = filler 32_000
healingPreimage =
  BS.take (chunkK + 1) $ BS.concat (replicate 3976 (hex "12a5c30f"))

reorderedPreimage :: BS.ByteString
reorderedPreimage = BS.replicate chunkK 0x05 <> BS.replicate chunkK 0x50

owner, healingOwner, healer :: BS.ByteString
owner = BS.replicate 28 0xa1
healingOwner = BS.replicate 28 0x66
healer = BS.replicate 28 0x77

h :: Int -> BS.ByteString
h n = BS.replicate 32 (fromIntegral n)

--------------------------------------------------------------------------------
-- Certification cases
--------------------------------------------------------------------------------

{- | A transaction whose witness set commits @preimage@ at one of the three
witness slots, plus the compact bytes that bind it.
-}
data CertCase = CertCase
  { cCompact :: BS.ByteString
  , cWitnessSet :: BS.ByteString
  , cTxId :: BS.ByteString
  }

-- | The committed hash goes to the address, script or redeemer slot in turn.
caseWith :: Tx -> (BS.ByteString, BS.ByteString, BS.ByteString) -> CertCase
caseWith tx hashes =
  CertCase
    { cCompact = compactWith tx (blake2b256 wsCbor)
    , cWitnessSet = wsCbor
    , cTxId = txIdOf tx
    }
  where
    wsCbor = witnessSetCborFrom hashes

addressSlotCase, scriptSlotCase, redeemerSlotCase :: BS.ByteString -> CertCase
addressSlotCase p = caseWith tx1 (blake2b256 p, h 0x02, h 0x03)
scriptSlotCase p = caseWith tx1 (h 0x01, blake2b256 p, h 0x03)
redeemerSlotCase p = caseWith tx1 (h 0x01, h 0x02, blake2b256 p)

-- | The default case: field 8, the redeemer slot.
certifiedCase :: CertCase
certifiedCase = redeemerSlotCase preimage

healingCase :: CertCase
healingCase = redeemerSlotCase healingPreimage

reorderedCase :: CertCase
reorderedCase =
  CertCase
    { cCompact =
        BS.concat
          [ "\x84"
          , cborInt 1
          , reorderedBodyCbor
          , defBytes32 (blake2b256 reorderedWitnessSetCbor)
          , cborInt 0
          ]
    , cWitnessSet = reorderedWitnessSetCbor
    , cTxId = reorderedTxId
    }

positionalCase :: CertCase
positionalCase =
  CertCase
    { cCompact =
        BS.concat
          [ "\x84"
          , cborInt 1
          , positionalBodyCbor
          , defBytes32 (blake2b256 reorderedWitnessSetCbor)
          , cborInt 0
          ]
    , cWitnessSet = reorderedWitnessSetCbor
    , cTxId = positionalTxId
    }

positionalBodyCbor :: BS.ByteString
positionalBodyCbor =
  BS.concat
    [ "\x8c"
    , defBytes32 (blake2b256 (aikenFieldPreimage 0))
    , defBytes32 (blake2b256 (aikenFieldPreimage 1))
    , defBytes32 (blake2b256 (aikenFieldPreimage 2))
    , cborInt 0
    , cborInt 0
    , cborInt 0
    , defBytes32 (blake2b256 (aikenFieldPreimage 3))
    , defBytes32 (blake2b256 (aikenFieldPreimage 4))
    , defBytes32 (blake2b256 (aikenFieldPreimage 5))
    , defBytes32 (BS.replicate 32 0)
    , defBytes32 (BS.replicate 32 0)
    , cborInt 255
    ]

positionalTxId :: BS.ByteString
positionalTxId =
  blake2b256 ("MidgardNativeTxBodyV1" <> cborInt 1 <> positionalBodyCbor)

reorderedBodyCbor :: BS.ByteString
reorderedBodyCbor =
  BS.concat
    [ "\x8c"
    , defBytes32 (blake2b256 reorderedPreimage)
    , defBytes32 (blake2b256 (aikenFieldPreimage 1))
    , defBytes32 (blake2b256 (aikenFieldPreimage 2))
    , cborInt 0
    , cborInt 0
    , cborInt 0
    , defBytes32 (blake2b256 (aikenFieldPreimage 3))
    , defBytes32 (blake2b256 (aikenFieldPreimage 4))
    , defBytes32 (blake2b256 (aikenFieldPreimage 5))
    , defBytes32 (BS.replicate 32 0)
    , defBytes32 (BS.replicate 32 0)
    , cborInt 255
    ]

reorderedWitnessSetCbor :: BS.ByteString
reorderedWitnessSetCbor =
  BS.concat
    [ "\x83"
    , defBytes32 (blake2b256 (aikenFieldPreimage 7))
    , defBytes32 (blake2b256 (aikenFieldPreimage 6))
    , defBytes32 (blake2b256 (aikenFieldPreimage 8))
    ]

reorderedTxId :: BS.ByteString
reorderedTxId =
  blake2b256 ("MidgardNativeTxBodyV1" <> cborInt 1 <> reorderedBodyCbor)

aikenFieldPreimage :: Int -> BS.ByteString
aikenFieldPreimage fieldIndex =
  BS.take (chunkK + 1) $
    BS.concat (replicate 3976 (BS.cons (fromIntegral (16 + fieldIndex)) (hex "a5c30f")))

caseFor :: Integer -> BS.ByteString -> Tx -> CertCase
caseFor 6 p tx = caseWith tx (h 0x01, blake2b256 p, h 0x03)
caseFor 7 p tx = caseWith tx (blake2b256 p, h 0x02, h 0x03)
caseFor _ p tx = caseWith tx (h 0x01, h 0x02, blake2b256 p)

--------------------------------------------------------------------------------
-- Building terms
--------------------------------------------------------------------------------

certify :: forall (s :: S). Integer -> BS.ByteString -> CertCase -> Term s PBool
certify fieldIndex p c =
  verifyWith (certificateFor (cTxId c) fieldIndex p) c (splitRef p)

positionalFieldIsAccepted :: forall (s :: S). Int -> Term s PBool
positionalFieldIsAccepted fieldIndex =
  plet (pconstant (cCompact positionalCase)) $ \compact ->
    plet (pconstant (cWitnessSet positionalCase)) $ \witnessSet ->
      pverifyFieldPreimageCertificateV1
        # buildWith healingOwner positionalTxId (fromIntegral fieldIndex) fieldPreimage
        # compact
        # witnessSet
        # (psplitFieldPreimage # pconstant fieldPreimage)
  where
    fieldPreimage = aikenFieldPreimage fieldIndex

verifyWith ::
  forall (s :: S).
  Term s PFieldPreimageCertificateV1 ->
  CertCase ->
  [BS.ByteString] ->
  Term s PBool
verifyWith certificate c chunks =
  pverifyFieldPreimageCertificateV1
    # certificate
    # pconstant (cCompact c)
    # pconstant (cWitnessSet c)
    # chunksT chunks

certificateT :: forall (s :: S). BS.ByteString -> Term s PFieldPreimageCertificateV1
certificateT p = certificateFor (txIdOf tx1) 8 p

certificateFor ::
  forall (s :: S).
  BS.ByteString ->
  Integer ->
  BS.ByteString ->
  Term s PFieldPreimageCertificateV1
certificateFor txId fieldIndex p =
  certificateOf txId fieldIndex (fromIntegral (BS.length p)) (map blake2b256 (splitRef p))

certificateWithLength ::
  forall (s :: S). BS.ByteString -> Integer -> Term s PFieldPreimageCertificateV1
certificateWithLength p len = certificateOf (txIdOf tx1) 8 len (map blake2b256 (splitRef p))

lopsidedCertificate :: forall (s :: S). BS.ByteString -> Term s PFieldPreimageCertificateV1
lopsidedCertificate p =
  certificateOf (txIdOf tx1) 8 (fromIntegral (BS.length p)) (map blake2b256 (lopsided p))

reorderedCertificate :: forall (s :: S). Term s PFieldPreimageCertificateV1
reorderedCertificate =
  certificateOfWithOwner
    healingOwner
    reorderedTxId
    0
    (fromIntegral (BS.length reorderedPreimage))
    (map blake2b256 (reverse (splitRef reorderedPreimage)))

certificateOf ::
  forall (s :: S).
  BS.ByteString ->
  Integer ->
  Integer ->
  [BS.ByteString] ->
  Term s PFieldPreimageCertificateV1
certificateOf = certificateOfWithOwner owner

certificateOfWithOwner ::
  forall (s :: S).
  BS.ByteString ->
  BS.ByteString ->
  Integer ->
  Integer ->
  [BS.ByteString] ->
  Term s PFieldPreimageCertificateV1
certificateOfWithOwner certificateOwner txId fieldIndex totalLength digests =
  pcon $
    PFieldPreimageCertificateV1
      { pcert'owner = pdata (punsafeCoerce (pconstant @PByteString certificateOwner))
      , pcert'txId = pdata (pconstant txId)
      , pcert'fieldIndex = pdata (pconstant fieldIndex)
      , pcert'totalLength = pdata (pconstant totalLength)
      , pcert'chunkDigests = pdata (chunksT digests)
      }

builtCertificate :: forall (s :: S). Term s PFieldPreimageCertificateV1
builtCertificate = buildWith owner (txIdOf tx1) 8 preimage

buildWith ::
  forall (s :: S).
  BS.ByteString ->
  BS.ByteString ->
  Integer ->
  BS.ByteString ->
  Term s PFieldPreimageCertificateV1
buildWith o txId fieldIndex p =
  pfieldPreimageCertificateV1
    # punsafeCoerce @PPubKeyHash (pconstant @PByteString o)
    # pconstant txId
    # pconstant fieldIndex
    # pconstant p

certificateIsBuilt :: forall (s :: S). Term s PFieldPreimageCertificateV1 -> Term s PBool
certificateIsBuilt c =
  pmatch c $ \PFieldPreimageCertificateV1 {pcert'totalLength} ->
    pfromData pcert'totalLength #> 0

--------------------------------------------------------------------------------
-- Reference inputs
--------------------------------------------------------------------------------

refInputsT :: forall (s :: S). Term s (PBuiltinList (PAsData PTxInInfo))
refInputsT = inputList [chunkInput (PD.B c) | c <- splitRef preimage]

-- | Carriage sits wherever it was published; the outref is never read.
chunkRef :: TxOutRef
chunkRef = TxOutRef (TxId (toBuiltin (BS.replicate 32 0x01))) 0

chunkInput :: PD.Data -> TxInInfo
chunkInput datum =
  TxInInfo
    chunkRef
    ( TxOut
        (scriptHashAddress (ScriptHash (toBuiltin (BS.replicate 28 0xc1))))
        mempty
        (OutputDatum (Datum (dataToBuiltinData datum)))
        Nothing
    )

noDatumInput :: TxInInfo
noDatumInput =
  TxInInfo
    chunkRef
    ( TxOut
        (scriptHashAddress (ScriptHash (toBuiltin (BS.replicate 28 0xc1))))
        mempty
        NoOutputDatum
        Nothing
    )

--------------------------------------------------------------------------------
-- Plumbing
--------------------------------------------------------------------------------

chunksT :: forall (s :: S). [BS.ByteString] -> Term s (PBuiltinList (PAsData PByteString))
chunksT bs = foldr (\b acc -> pcons # pdata (pconstant b) # acc) pnil bs

{- | Reference inputs as the port sees them.

Coerced from @Data@ rather than lifted, which is what every other suite in the
tree does for a list of ledger records — the port reads fields positionally, so
the coercion is the faithful path and not a shortcut.
-}
inputList :: forall (s :: S). [TxInInfo] -> Term s (PBuiltinList (PAsData PTxInInfo))
inputList inputs =
  punsafeCoerce (pconstant @(PBuiltinList PData) (map PlutusTx.toData inputs))

holds :: (forall (s :: S). Term s PBool) -> Assertion
holds = passertEval

refuses :: (forall (s :: S). Term s PBool) -> Assertion
refuses p = passertEval (pnot # p)

pall' :: forall (s :: S). [Term s PBool] -> Term s PBool
pall' = foldr (#&&) (pconstant True)

hex :: BS.ByteString -> BS.ByteString
hex = Base16.decodeLenient
