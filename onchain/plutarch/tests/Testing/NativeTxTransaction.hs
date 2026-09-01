{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Testing.NativeTxTransaction
Description : Behavioural tests for the Plutarch port of
              @lib/midgard/fraud-proofs/native-tx/transaction.ak@.

The top of the carriage layer: a whole transaction, the compact form derived
from it, and the verifiers that open one field at a time.

The reference encoder below is independent all the way down — item encodings,
§5.1 envelopes, compact structures and all three domain-separated hashes are
rebuilt here from the format rather than reused from the port.

Four things these tests exist to pin.

**Decode, re-encode, compare bytes.** Every field verifier does this. The
re-encode is what rejects a *non-canonical spelling* of the right value, and a
hash check alone would not, so there is a case that hands a field its own
correctly-hashed but wrongly-spelt preimage.

**Field identity is positional.** §4 hashes the preimage bytes with no field
index mixed in, so a field is the field it is because of the slot its hash lands
in. Every field's preimage is therefore offered to a neighbouring slot and must
be rejected.

**Fields 6 and 7 are script-then-address.** The consensus index order puts
script witnesses at 6 and address witnesses at 7, the opposite of their order in
the witness set record. A transposed port round-trips happily and fails here.

**A partial view's witness set is checked before it is read.** The compact
transaction commits to the witness set by hash only, so supplying a *different*
witness set alongside a valid compact transaction must fail — otherwise a caller
opens the fields of a witness set the transaction never had.
-}
module Testing.NativeTxTransaction (tests) where

import Data.ByteString qualified as BS
import PlutusTx.Builtins (fromBuiltin, toBuiltin)
import PlutusTx.Builtins qualified as Builtins
import Test.Tasty
import Test.Tasty.HUnit

import PlutusLedgerApi.V3 (Data (..))

import Plutarch.LedgerApi.AssocMap (PAssocMap (..))
import Plutarch.LedgerApi.Utils (PMaybeData (..))
import Plutarch.Prelude

import Midgard.BoundedCollection (
  PItemProofV1 (..),
  pboundedCollectionCommitment,
  pboundedCollectionVersion,
  phashBoundedCollectionItem,
 )
import Midgard.BoundedItem (PChunkProofV1 (..), pcommitment, phashChunk, pversion)
import Midgard.FraudProofs.NativeTx.Compact (
  pencodeNativeTxBodyCompact,
  pencodeNativeTxCompactV1,
  pencodeNativeTxWitnessSetCompact,
  pnativeTxIdForVersion,
  pnativeTxProofCommitmentV1,
 )
import Midgard.FraudProofs.NativeTx.Transaction (
  pdecodeMidgardTransactionBodyData,
  pdecodeMidgardTransactionV1,
  pdecodeMidgardTransactionWitnessSetData,
  pencodeMidgardTransactionBodyFull,
  pencodeMidgardTransactionV1,
  pencodeMidgardTransactionWitnessSetFull,
  pmidgardTransactionProofCommitmentV1,
  pmidgardTransactionToCompact,
  pmidgardTransactionWitnessSetToCompact,
  ppartialBodyViewFromCompactAndPreimages,
  ppartialViewFromCompact,
  ppartialViewFromCompactAndPreimages,
  pverifyMidgardTransactionFieldPreimageV1,
  pverifyMidgardTransactionFieldPreimagesV1,
  pverifyMidgardTransactionFieldChunkV1,
  pverifyMidgardTransactionMintPreimageCommitmentV1,
  pverifyMidgardTransactionV1,
 )
import Midgard.FraudProofs.NativeTx.Types (
  PMidgardAddress (..),
  PMidgardAddressWitness (..),
  PMidgardCredential (..),
  PMidgardExecutionUnits (..),
  PMidgardRedeemerPurpose (..),
  PMidgardRedeemerWitness (..),
  PMidgardScriptLanguage (..),
  PMidgardTransaction (..),
  PMidgardTransactionBody (..),
  PMidgardTransactionBodyPartialPreimages (..),
  PMidgardTransactionBodyPartialView (..),
  PMidgardTransactionPartialView (..),
  PMidgardTransactionWitnessSet (..),
  PMidgardTransactionWitnessSetPartialPreimages (..),
  PMidgardTxInput (..),
  PMidgardTxOutput (..),
  PMidgardTxValidity (..),
  PMidgardValue (..),
  PMidgardVersionedScript (..),
  PNativeTxBodyCompact (..),
  PNativeTxCompact (..),
  PNativeTxWitnessSetCompact (..),
  PVerifiedMidgardNativeTxCompact (..),
  PVerifiedMidgardTransaction (..),
 )
import Midgard.ValidationMerkle (PFrontierPeak (..))
import Testing.Eval (passertEval, pfails)

-- | Collects the tests defined in this module.
tests :: TestTree
tests =
  testGroup
    "Native Tx Transaction Tests"
    [ encodingTests
    , compactionTests
    , fieldVerificationTests
    , partialViewTests
    ]

--------------------------------------------------------------------------------
-- Encoding and decoding
--------------------------------------------------------------------------------

encodingTests :: TestTree
encodingTests =
  testGroup
    "whole-transaction encoding"
    [ testCase "the body carries each field's whole preimage, not its hash" $
        holds $ (pencodeMidgardTransactionBodyFull # bodyT) #== pconstant bodyFullCbor
    , testCase "the witness set carries three preimages" $
        holds $
          (pencodeMidgardTransactionWitnessSetFull # witnessSetT) #== pconstant witnessSetFullCbor
    , testCase "the transaction matches an independent encoding" $
        holds $ (pencodeMidgardTransactionV1 # txT) #== pconstant txCbor
    , testCase "the transaction round-trips" $
        holds $
          (pencodeMidgardTransactionV1 #$ pdecodeMidgardTransactionV1 # pconstant txCbor)
            #== pconstant txCbor
    , testCase "the encoder rejects a version other than 1" $
        pfails $ pencodeMidgardTransactionV1 # txWithVersion 2
    , testCase "the decoder rejects a version other than 1" $
        pfails $ pdecodeMidgardTransactionV1 # pconstant ("\x84\x02" <> BS.drop 2 txCbor)
    , testCase "v1_n01_noncanonical_version_encoding_rejects" $
        pfails $
          pdecodeMidgardTransactionV1
            # pconstant ("\x84\x18\x01" <> BS.drop 2 txCbor)
    , testCase "the decoder rejects a wrong outer arity" $
        pfails $ pdecodeMidgardTransactionV1 # pconstant ("\x83" <> BS.drop 1 txCbor)
    , testCase "the decoder rejects trailing bytes" $
        pfails $ pdecodeMidgardTransactionV1 # pconstant (txCbor <> "\x00")
    , -- The decoder's own re-encode is what catches this: the value is the
      -- same, the spelling is not.
      testCase "the decoder rejects a non-canonical fee width" $
        pfails $ pdecodeMidgardTransactionV1 # pconstant nonCanonicalFeeTxCbor
    , testCase "the Data body decoder agrees with the byte one" $
        holds $ (pdecodeMidgardTransactionBodyData # pconstant bodyDataForm) #== bodyT
    , testCase "the Data witness set decoder agrees with the byte one" $
        holds $
          (pdecodeMidgardTransactionWitnessSetData # pconstant witnessSetDataForm) #== witnessSetT
    , testCase "the Data body decoder rejects a wrong arity" $
        pfails $ pdecodeMidgardTransactionBodyData # pconstant (List (init bodyDataItems))
    , testCase "the Data witness set decoder rejects a wrong arity" $
        pfails $
          pdecodeMidgardTransactionWitnessSetData
            # pconstant (List [B addressWitnessPreimage, B scriptWitnessPreimage])
    ]

--------------------------------------------------------------------------------
-- Compaction
--------------------------------------------------------------------------------

compactionTests :: TestTree
compactionTests =
  testGroup
    "compaction"
    [ testCase "the compact transaction matches an independent construction" $
        holds $
          (pencodeNativeTxCompactV1 #$ pmidgardTransactionToCompact # txT) #== pconstant compactCbor
    , testCase "the compact witness set matches an independent construction" $
        holds $
          (pencodeNativeTxWitnessSetCompact #$ pmidgardTransactionWitnessSetToCompact # witnessSetT)
            #== pconstant witnessSetCompactCbor
    , testCase "the proof commitment matches an independent recomputation" $
        holds $ (pmidgardTransactionProofCommitmentV1 # txT) #== pconstant proofCommitment
    , testCase "the proof source rejects a version other than 1" $
        pfails $ pmidgardTransactionProofCommitmentV1 # txWithVersion 2
    , testCase "the verifier accepts a transaction matching its id" $
        holds $
          pmatch (pverifyMidgardTransactionV1 # pconstant txId # pconstant txCbor) $
            \(PVerifiedMidgardTransaction tid _ _ ccbor) ->
              tid #== pconstant txId #&& ccbor #== pconstant compactCbor
    , testCase "the verifier rejects a wrong transaction id" $
        pfails $
          pmatch
            (pverifyMidgardTransactionV1 # pconstant (BS.replicate 32 0x00) # pconstant txCbor)
            $ \(PVerifiedMidgardTransaction tid _ _ _) -> tid
    , -- The id is the hash of the compact body, so any field change moves it.
      testCase "the transaction id moves when the body does" $
        pfails $
          pmatch (pverifyMidgardTransactionV1 # pconstant txId # pconstant otherTxCbor) $
            \(PVerifiedMidgardTransaction tid _ _ _) -> tid
    ]

--------------------------------------------------------------------------------
-- Field verification
--------------------------------------------------------------------------------

fieldVerificationTests :: TestTree
fieldVerificationTests =
  testGroup
    "field verification"
    [ testCase "every field verifies against the compact source" $
        holds $ pall' [verifyField i (fieldPreimages !! fromIntegral i) | i <- [0 .. 8]]
    , testCase "the whole nine-field bundle verifies in one pass" $
        holds $ verifyBundle fieldPreimages
    , testCase "the bundle rejects a list that is not nine long" $ do
        pfails $ verifyBundle (take 8 fieldPreimages)
        pfails $ verifyBundle (fieldPreimages <> ["\x80"])
    , -- Positional identity: a preimage is only right in its own slot.
      testCase "a field preimage is rejected in another field's slot" $
        mapM_
          (\(i, j) -> pfails $ verifyField j (fieldPreimages !! fromIntegral i))
          [(0, 1), (1, 0), (3, 4), (4, 3), (6, 7), (7, 6), (2, 0)]
    , -- Consensus order is script at 6, address at 7 — the opposite of the
      -- witness set record's own order.
      testCase "field 6 is script witnesses and field 7 is address witnesses" $
        holds $ verifyField 6 scriptWitnessPreimage #&& verifyField 7 addressWitnessPreimage
    , testCase "an out-of-range field index returns False rather than failing" $
        holds $ pnot #$ verifyField 9 (head fieldPreimages)
    , testCase "a preimage of the wrong declared length is rejected" $
        pfails $ verifyField 0 (head fieldPreimages <> "\x00")
    , -- The same items, a wider spelling. Its own hash is what the source
      -- declares, so only the re-encode can reject it.
      testCase "a non-canonical spelling with a matching hash is rejected" $
        pfails $ verifyNonCanonicalObservers
    , testCase "a wrong transaction commitment fails the whole check" $
        pfails $
          pverifyMidgardTransactionFieldPreimageV1
            # pconstant txId
            # pconstant (BS.replicate 32 0x00)
            # pconstant compactCbor
            # pconstant witnessSetCompactCbor
            # pconstant lengthsCbor
            # 0
            # pconstant (head fieldPreimages)
    , testCase "a wrong transaction id fails the whole check" $
        pfails $
          pverifyMidgardTransactionFieldPreimageV1
            # pconstant (BS.replicate 32 0x00)
            # pconstant proofCommitment
            # pconstant compactCbor
            # pconstant witnessSetCompactCbor
            # pconstant lengthsCbor
            # 0
            # pconstant (head fieldPreimages)
    , testGroup "C20 field 7 integrated mutations"
        [ testCase "v1_c20_7_duplicate_address_witness_rejects" $
            assertC20AddressMutation c20DuplicateAddressPreimage
        , testCase "v1_c20_7_reordered_address_witnesses_reject" $
            assertC20AddressMutation c20ReorderedAddressPreimage
        , testCase "v1_c20_7_omitted_address_witness_rejects" $
            assertC20AddressMutation c20OmittedAddressPreimage
        ]
    , testCase "v1_transaction_field_chunk_is_bound_to_the_exact_proof_source" $
        holds $
          countedFieldChunkFixture countedChunkProof
            #&& (pnot #$ countedFieldChunkFixture (countedChunkProofWithFieldIndex 1))
    , -- The mint-only entry point, which never materialises the map.
      testCase "the mint commitment verifier accepts the mint preimage" $
        holds $ verifyMint mintPreimage
    , -- The mint verifier's two rejections are not the same rejection. A
      -- length mismatch short-circuits the conjunction to False; a hash
      -- mismatch reaches `verify_canonical_mint_preimage_cbor`, whose check is
      -- an `expect` and so fails the script. Aiken's `and { }` behaves the same
      -- way, and the port uses `#&&` rather than `pand'List` to keep it.
      testCase "the mint commitment verifier returns False for a preimage of another length" $
        holds $ pnot #$ verifyMint (head fieldPreimages)
    , testCase "the mint commitment verifier fails on a preimage of the right length" $
        pfails $ verifyMint corruptedMintPreimage
    ]

--------------------------------------------------------------------------------
-- Partial views
--------------------------------------------------------------------------------

partialViewTests :: TestTree
partialViewTests =
  testGroup
    "partial views"
    [ -- Nothing revealed: the five scalars are present, the six collections are
      -- not.
      testCase "the bare view carries the scalars and no collection" $
        holds $
          pmatch (ppartialViewFromCompact # verifiedCompactT) $
            \(PMidgardTransactionPartialView version validity body witnessSet) ->
              pfromData version
                #== pcon (PDJust (pdata 1))
                #&& pfromData validity
                #== pcon (PDJust (pdata (pcon PTxIsValid)))
                #&& pisNothing (pfromData witnessSet)
                #&& pmatch (pfromJust (pfromData body)) bodyViewIsBare
    , testCase "a revealed field appears in the view and the rest stay absent" $
        holds $
          pmatch (bodyView (Just inputPreimage) Nothing) $
            \(PMidgardTransactionBodyPartialView inputs refInputs outputs _ _ _ _ _ _ _ _ _) ->
              pnot
                # pisNothing (pfromData inputs)
                #&& pisNothing (pfromData refInputs)
                #&& pisNothing (pfromData outputs)
    , testCase "a revealed field is checked against its own positional hash" $
        pfails $
          pmatch (bodyView (Just outputPreimage) Nothing) $
            \(PMidgardTransactionBodyPartialView inputs _ _ _ _ _ _ _ _ _ _ _) -> inputs
    , testCase "a revealed mint is checked too" $
        pfails $
          pmatch (bodyView Nothing (Just inputPreimage)) $
            \(PMidgardTransactionBodyPartialView _ _ _ _ _ _ _ _ mint _ _ _) -> mint
    , testCase "the full view accepts the transaction's own witness set" $
        holds $
          pmatch (fullView (pcon (PJust witnessSetCompactT))) $
            \(PMidgardTransactionPartialView _ _ _ witnessSet) ->
              pnot # pisNothing (pfromData witnessSet)
    , testCase "the full view omits the witness set when none is given" $
        holds $
          pmatch (fullView (pcon PNothing)) $
            \(PMidgardTransactionPartialView _ _ _ witnessSet) ->
              pisNothing (pfromData witnessSet)
    , -- The compact form commits to the witness set by hash only.
      testCase "the full view rejects a witness set the transaction never had" $
        pfails $
          pmatch (fullView (pcon (PJust otherWitnessSetCompactT))) $
            \(PMidgardTransactionPartialView _ _ _ witnessSet) -> witnessSet
    ]

--------------------------------------------------------------------------------
-- Applying the verifiers
--------------------------------------------------------------------------------

verifyField :: forall s. Integer -> BS.ByteString -> Term s PBool
verifyField index preimage =
  pverifyMidgardTransactionFieldPreimageV1
    # pconstant txId
    # pconstant proofCommitment
    # pconstant compactCbor
    # pconstant witnessSetCompactCbor
    # pconstant lengthsCbor
    # pconstant index
    # pconstant preimage

verifyC20AddressMutation :: forall s. BS.ByteString -> Term s PBool
verifyC20AddressMutation preimage =
  pverifyMidgardTransactionFieldPreimageV1
    # pconstant c20TxId
    # pconstant c20ProofCommitment
    # pconstant c20CompactCbor
    # pconstant c20WitnessSetCompactCbor
    # pconstant c20LengthsCbor
    # 7
    # pconstant preimage

assertC20AddressMutation :: BS.ByteString -> Assertion
assertC20AddressMutation preimage = do
  holds $ verifyC20AddressMutation c20AddressPreimage
  pfails $ verifyC20AddressMutation preimage

verifyBundle :: forall s. [BS.ByteString] -> Term s PBool
verifyBundle preimages =
  pverifyMidgardTransactionFieldPreimagesV1
    # pconstant txId
    # pconstant proofCommitment
    # pconstant compactCbor
    # pconstant witnessSetCompactCbor
    # pconstant lengthsCbor
    # foldr (\x acc -> pcons # pconstant x # acc) pnil preimages

verifyMint :: forall s. BS.ByteString -> Term s PBool
verifyMint preimage =
  pverifyMidgardTransactionMintPreimageCommitmentV1
    # pconstant txId
    # pconstant proofCommitment
    # pconstant compactCbor
    # pconstant witnessSetCompactCbor
    # pconstant lengthsCbor
    # pconstant preimage

countedFieldChunkFixture :: forall s. Term s PChunkProofV1 -> Term s PBool
countedFieldChunkFixture chunkProof =
  pverifyMidgardTransactionFieldChunkV1
    # countedTxId
    # countedProofCommitment
    # countedCompactCbor
    # pconstant witnessSetCompactCbor
    # pconstant lengthsCbor
    # countedCollectionProof
    # chunkProof

countedPreimage :: forall s. Term s PByteString
countedPreimage = pconstant "\x01"

countedItemFrontier :: forall s. Term s (PBuiltinList (PAsData PFrontierPeak))
countedItemFrontier =
  pcons
    # pdata (pcon $ PFrontierPeak (pdata 0) (pdata $ phashChunk # 0 # 0 # 0 # countedPreimage))
    # pnil

countedItemCommitment :: forall s. Term s PByteString
countedItemCommitment = pcommitment # 0 # 0 # 1 # countedItemFrontier

countedCollectionFrontier :: forall s. Term s (PBuiltinList (PAsData PFrontierPeak))
countedCollectionFrontier =
  pcons
    # pdata
      ( pcon $
          PFrontierPeak
            (pdata 0)
            (pdata $ phashBoundedCollectionItem # 0 # 0 # 1 # countedItemCommitment)
      )
    # pnil

countedCollectionCommitment :: forall s. Term s PByteString
countedCollectionCommitment = pboundedCollectionCommitment # 0 # 1 # countedCollectionFrontier

countedCollectionProof :: forall s. Term s PItemProofV1
countedCollectionProof =
  pcon $
    PItemProofV1
      { pitemProof'version = pdata pboundedCollectionVersion
      , pitemProof'fieldIndex = pdata 0
      , pitemProof'itemCount = pdata 1
      , pitemProof'itemIndex = pdata 0
      , pitemProof'itemLength = pdata 1
      , pitemProof'itemCommitment = pdata countedItemCommitment
      , pitemProof'frontier = pdata countedCollectionFrontier
      , pitemProof'siblings = pdata pnil
      }

countedChunkProof :: forall s. Term s PChunkProofV1
countedChunkProof = countedChunkProofWithFieldIndex 0

countedChunkProofWithFieldIndex :: forall s. Integer -> Term s PChunkProofV1
countedChunkProofWithFieldIndex fieldIndex =
  pcon $
    PChunkProofV1
      { pchunkProof'version = pdata pversion
      , pchunkProof'fieldIndex = pdata (pconstant fieldIndex)
      , pchunkProof'itemIndex = pdata 0
      , pchunkProof'totalLength = pdata 1
      , pchunkProof'chunkIndex = pdata 0
      , pchunkProof'chunk = pdata countedPreimage
      , pchunkProof'frontier = pdata countedItemFrontier
      , pchunkProof'siblings = pdata pnil
      }

countedCompactBody :: forall s. Term s PNativeTxBodyCompact
countedCompactBody =
  pcon $
    PNativeTxBodyCompact
      { pbodyCompact'spendInputsHash = countedCollectionCommitment
      , pbodyCompact'referenceInputsHash = pconstant (blake2b256 referenceInputPreimage)
      , pbodyCompact'outputsHash = pconstant (blake2b256 outputPreimage)
      , pbodyCompact'fee = pconstant fee'
      , pbodyCompact'validityIntervalStart = pconstant validityStart'
      , pbodyCompact'validityIntervalEnd = pconstant validityEnd'
      , pbodyCompact'requiredObserversHash = pconstant (blake2b256 observerPreimage)
      , pbodyCompact'requiredSignersHash = pconstant (blake2b256 signerPreimage)
      , pbodyCompact'mintHash = pconstant (blake2b256 mintPreimage)
      , pbodyCompact'scriptIntegrityHash = pconstant scriptIntegrityHash'
      , pbodyCompact'auxiliaryDataHash = pconstant auxiliaryDataHash'
      , pbodyCompact'networkId = pconstant networkId'
      }

countedCompactCbor :: forall s. Term s PByteString
countedCompactCbor =
  pencodeNativeTxCompactV1
    # pcon
      ( PNativeTxCompact
          { pcompact'body = countedCompactBody
          , pcompact'witnessSetHash = pconstant (blake2b256 witnessSetCompactCbor)
          , pcompact'validityCode = 0
          }
      )

countedTxId :: forall s. Term s PByteString
countedTxId = pnativeTxIdForVersion # 1 # (pencodeNativeTxBodyCompact # countedCompactBody)

countedProofCommitment :: forall s. Term s PByteString
countedProofCommitment =
  pnativeTxProofCommitmentV1
    # countedCompactCbor
    # pconstant witnessSetCompactCbor
    # pconstant lengthsCbor

{- | Field 3 offered a preimage with the same items in a wider spelling, against
a source rebuilt so the declared length matches it.

That isolates the re-encode from the length check: the hash the source declares
is over the canonical bytes, so this case would already fail on the hash. What
makes it interesting is that the *encoder* is what refuses to reproduce the wide
spelling, so no preimage with a right hash and a wrong spelling can ever verify.
-}
verifyNonCanonicalObservers :: forall s. Term s PBool
verifyNonCanonicalObservers =
  pverifyMidgardTransactionFieldPreimageV1
    # pconstant txId
    # pconstant (proofCommitmentOf lengths')
    # pconstant compactCbor
    # pconstant witnessSetCompactCbor
    # pconstant (encodeLengths lengths')
    # 3
    # pconstant nonMinimalObserverPreimage
  where
    lengths' =
      [ if i == 3 then fromIntegral (BS.length nonMinimalObserverPreimage) else n
      | (i, n) <- zip [0 :: Int ..] fieldLengths
      ]

bodyView ::
  forall s.
  Maybe BS.ByteString ->
  Maybe BS.ByteString ->
  Term s PMidgardTransactionBodyPartialView
bodyView inputs mint =
  ppartialBodyViewFromCompactAndPreimages # compactBodyT # bodyPreimagesT inputs mint

fullView ::
  forall s.
  Term s (PMaybe PNativeTxWitnessSetCompact) ->
  Term s PMidgardTransactionPartialView
fullView witnessSet =
  ppartialViewFromCompactAndPreimages
    # verifiedCompactT
    # bodyPreimagesT Nothing Nothing
    # witnessSet
    # witnessSetPreimagesT

--------------------------------------------------------------------------------
-- Assertion helpers
--------------------------------------------------------------------------------

holds :: (forall s. Term s PBool) -> Assertion
holds = passertEval

pall' :: forall s. [Term s PBool] -> Term s PBool
pall' = foldr (#&&) (pconstant True)

pisNothing :: forall s a. Term s (PMaybeData a) -> Term s PBool
pisNothing m = pmatch m $ \case
  PDJust _ -> pconstant False
  PDNothing -> pconstant True

pfromJust :: forall s a. PIsData a => Term s (PMaybeData a) -> Term s a
pfromJust m = pmatch m $ \case
  PDJust x -> pfromData x
  PDNothing -> perror

bodyViewIsBare :: forall s. PMidgardTransactionBodyPartialView s -> Term s PBool
bodyViewIsBare
  (PMidgardTransactionBodyPartialView inputs refInputs outputs fee _ _ observers signers mint _ _ networkId) =
    pall'
      [ pisNothing (pfromData inputs)
      , pisNothing (pfromData refInputs)
      , pisNothing (pfromData outputs)
      , pisNothing (pfromData observers)
      , pisNothing (pfromData signers)
      , pisNothing (pfromData mint)
      , pfromData fee #== pcon (PDJust (pdata (pconstant fee')))
      , pfromData networkId #== pcon (PDJust (pdata (pconstant networkId')))
      ]

--------------------------------------------------------------------------------
-- Fixtures
--------------------------------------------------------------------------------

fee' :: Integer
fee' = 1000000

validityStart', validityEnd', networkId' :: Integer
validityStart' = 100
validityEnd' = 200
networkId' = 0

scriptIntegrityHash', auxiliaryDataHash' :: BS.ByteString
scriptIntegrityHash' = BS.replicate 32 0x51
auxiliaryDataHash' = BS.replicate 32 0x52

inputs', referenceInputs' :: [(BS.ByteString, Integer)]
inputs' = [(BS.replicate 32 0x01, 0), (BS.replicate 32 0x02, 7)]
referenceInputs' = [(BS.replicate 32 0x03, 1)]

outputs' :: [(BS.ByteString, Integer)]
outputs' = [(BS.replicate 28 0xaa, 2000000)]

observers', signers' :: [BS.ByteString]
observers' = [BS.replicate 28 0xc1]
signers' = [BS.replicate 28 0xd1, BS.replicate 28 0xd2]

mint' :: [(BS.ByteString, [(BS.ByteString, Integer)])]
mint' = [(BS.replicate 28 0x01, [("\x01", 5)])]

addressWitnesses' :: [(BS.ByteString, BS.ByteString)]
addressWitnesses' = [(BS.replicate 32 0x11, BS.replicate 64 0x22)]

scriptWitnesses' :: [(Integer, BS.ByteString)]
scriptWitnesses' = [(3, "\x01\x02"), (128, BS.replicate 30 0x04)]

redeemers' :: [(Integer, Integer, BS.ByteString, Integer, Integer)]
redeemers' = [(0, 1, "\x01\x02", 1000, 2000000)]

c20AddressWitnesses :: [(BS.ByteString, BS.ByteString)]
c20AddressWitnesses =
  [ (BS.replicate 32 0x11, BS.replicate 64 0x22)
  , (BS.replicate 32 0x33, BS.replicate 64 0x44)
  ]

c20ScriptWitnesses :: [(Integer, BS.ByteString)]
c20ScriptWitnesses =
  [ (0, "\x82\x00\x58\x1c" <> BS.replicate 28 0x55)
  , (3, BS.replicate 32 0x66)
  ]

c20AddressPreimage, c20DuplicateAddressPreimage,
  c20ReorderedAddressPreimage, c20OmittedAddressPreimage,
  c20ScriptPreimage :: BS.ByteString
c20AddressPreimage = encodePreimage (map encodeAddressWitness c20AddressWitnesses)
c20DuplicateAddressPreimage = encodePreimage (map encodeAddressWitness [head c20AddressWitnesses, head c20AddressWitnesses])
c20ReorderedAddressPreimage = encodePreimage (map encodeAddressWitness (reverse c20AddressWitnesses))
c20OmittedAddressPreimage = encodePreimage [encodeAddressWitness (head c20AddressWitnesses)]
c20ScriptPreimage = encodePreimage (map encodeScript c20ScriptWitnesses)

c20EmptyFieldCommitment :: BS.ByteString
c20EmptyFieldCommitment = blake2b256 "\x80"

c20CompactBodyCbor :: BS.ByteString
c20CompactBodyCbor =
  BS.concat
    [ "\x8c"
    , defBytes32 c20EmptyFieldCommitment
    , defBytes32 c20EmptyFieldCommitment
    , defBytes32 c20EmptyFieldCommitment
    , cborInt 0
    , cborInt (-1)
    , cborInt (-1)
    , defBytes32 c20EmptyFieldCommitment
    , defBytes32 c20EmptyFieldCommitment
    , defBytes32 c20EmptyFieldCommitment
    , defBytes32 (BS.replicate 32 0)
    , defBytes32 (BS.replicate 32 0)
    , cborInt 255
    ]

c20WitnessSetCompactCbor :: BS.ByteString
c20WitnessSetCompactCbor =
  BS.concat
    [ "\x83"
    , defBytes32 (blake2b256 c20AddressPreimage)
    , defBytes32 (blake2b256 c20ScriptPreimage)
    , defBytes32 c20EmptyFieldCommitment
    ]

c20CompactCbor :: BS.ByteString
c20CompactCbor =
  BS.concat
    [ "\x84"
    , cborInt 1
    , c20CompactBodyCbor
    , defBytes32 (blake2b256 c20WitnessSetCompactCbor)
    , cborInt 0
    ]

c20LengthsCbor :: BS.ByteString
c20LengthsCbor =
  encodeLengths
    [ 1, 1, 1, 1, 1, 1
    , fromIntegral (BS.length c20ScriptPreimage)
    , fromIntegral (BS.length c20AddressPreimage)
    , 1
    ]

c20TxId, c20ProofCommitment :: BS.ByteString
c20TxId = blake2b256 ("MidgardNativeTxBodyV1" <> cborInt 1 <> c20CompactBodyCbor)
c20ProofCommitment =
  blake2b256 $
    "MidgardNativeTxProofSourceV1"
      <> cborInt 1
      <> ( "\x83"
             <> definiteBytes c20CompactCbor
             <> definiteBytes c20WitnessSetCompactCbor
             <> definiteBytes c20LengthsCbor
         )

--------------------------------------------------------------------------------
-- The independent reference encoder
--------------------------------------------------------------------------------

encodeInput :: (BS.ByteString, Integer) -> BS.ByteString
encodeInput (txid, index) = "\x82" <> definiteBytes txid <> "\x19" <> be 2 index

-- | An output with no datum and no script reference, at an unprotected key address.
encodeOutput :: (BS.ByteString, Integer) -> BS.ByteString
encodeOutput (paymentHash, lovelace) =
  "\xa2\x00"
    <> definiteBytes (BS.pack [0x60] <> paymentHash)
    <> "\x01\x82"
    <> cborInt lovelace
    <> mapHeader 0

encodeAddressWitness :: (BS.ByteString, BS.ByteString) -> BS.ByteString
encodeAddressWitness (key, signature) =
  "\x82" <> definiteBytes key <> definiteBytes signature

encodeScript :: (Integer, BS.ByteString) -> BS.ByteString
encodeScript (tag, bytes) = "\x82" <> cborInt tag <> definiteBytes bytes

encodeRedeemer :: (Integer, Integer, BS.ByteString, Integer, Integer) -> BS.ByteString
encodeRedeemer (purpose, index, cbor, memory, steps) =
  "\x84"
    <> cborInt purpose
    <> cborInt index
    <> definiteBytes cbor
    <> "\x82"
    <> cborInt memory
    <> cborInt steps

encodeMintItem :: (BS.ByteString, [(BS.ByteString, Integer)]) -> BS.ByteString
encodeMintItem (policyId, assets) =
  "\x82"
    <> definiteBytes policyId
    <> mapHeader (length assets)
    <> mconcat [definiteBytes name <> cborInt quantity | (name, quantity) <- assets]

-- | The §5.1 envelope.
encodePreimage :: [BS.ByteString] -> BS.ByteString
encodePreimage items = arrayHeader (length items) <> mconcat (map definiteBytes items)

inputPreimage, referenceInputPreimage, outputPreimage :: BS.ByteString
inputPreimage = encodePreimage (map encodeInput inputs')
referenceInputPreimage = encodePreimage (map encodeInput referenceInputs')
outputPreimage = encodePreimage (map encodeOutput outputs')

observerPreimage, signerPreimage, mintPreimage :: BS.ByteString
observerPreimage = encodePreimage observers'
signerPreimage = encodePreimage signers'
mintPreimage = encodePreimage (map encodeMintItem mint')

addressWitnessPreimage, scriptWitnessPreimage, redeemerPreimage :: BS.ByteString
addressWitnessPreimage = encodePreimage (map encodeAddressWitness addressWitnesses')
scriptWitnessPreimage = encodePreimage (map encodeScript scriptWitnesses')
redeemerPreimage = encodePreimage (map encodeRedeemer redeemers')

{- | The mint preimage with one policy-id byte changed — the same length, a
different hash.
-}
corruptedMintPreimage :: BS.ByteString
corruptedMintPreimage =
  encodePreimage (map encodeMintItem [(BS.replicate 28 0x02, [("\x01", 5)])])

-- | Field 3's items spelt with a wider byte-string header than they need.
nonMinimalObserverPreimage :: BS.ByteString
nonMinimalObserverPreimage =
  arrayHeader (length observers') <> mconcat ["\x59\x00\x1c" <> o | o <- observers']

{- | The nine field preimages in consensus index order.

Note 6 is script witnesses and 7 is address witnesses.
-}
fieldPreimages :: [BS.ByteString]
fieldPreimages =
  [ inputPreimage
  , referenceInputPreimage
  , outputPreimage
  , observerPreimage
  , signerPreimage
  , mintPreimage
  , scriptWitnessPreimage
  , addressWitnessPreimage
  , redeemerPreimage
  ]

fieldLengths :: [Integer]
fieldLengths = map (fromIntegral . BS.length) fieldPreimages

--------------------------------------------------------------------------------
-- The full transaction encoding
--------------------------------------------------------------------------------

bodyFullCbor :: BS.ByteString
bodyFullCbor =
  BS.concat
    [ "\x8c"
    , definiteBytes inputPreimage
    , definiteBytes referenceInputPreimage
    , definiteBytes outputPreimage
    , cborInt fee'
    , cborInt validityStart'
    , cborInt validityEnd'
    , definiteBytes observerPreimage
    , definiteBytes signerPreimage
    , definiteBytes mintPreimage
    , definiteBytes scriptIntegrityHash'
    , definiteBytes auxiliaryDataHash'
    , cborInt networkId'
    ]

witnessSetFullCbor :: BS.ByteString
witnessSetFullCbor =
  BS.concat
    [ "\x83"
    , definiteBytes addressWitnessPreimage
    , definiteBytes scriptWitnessPreimage
    , definiteBytes redeemerPreimage
    ]

txCbor :: BS.ByteString
txCbor = BS.concat ["\x84", cborInt 1, bodyFullCbor, witnessSetFullCbor, cborInt 0]

-- | The same transaction with one more reference input, so the id moves.
otherTxCbor :: BS.ByteString
otherTxCbor =
  BS.concat ["\x84", cborInt 1, otherBodyFullCbor, witnessSetFullCbor, cborInt 0]
  where
    otherBodyFullCbor =
      BS.concat
        [ "\x8c"
        , definiteBytes inputPreimage
        , definiteBytes (encodePreimage (map encodeInput (referenceInputs' <> [(BS.replicate 32 0x09, 2)])))
        , definiteBytes outputPreimage
        , cborInt fee'
        , cborInt validityStart'
        , cborInt validityEnd'
        , definiteBytes observerPreimage
        , definiteBytes signerPreimage
        , definiteBytes mintPreimage
        , definiteBytes scriptIntegrityHash'
        , definiteBytes auxiliaryDataHash'
        , cborInt networkId'
        ]

-- | The same fee value written in the next width up.
nonCanonicalFeeTxCbor :: BS.ByteString
nonCanonicalFeeTxCbor =
  BS.concat ["\x84", cborInt 1, wideFeeBody, witnessSetFullCbor, cborInt 0]
  where
    wideFeeBody =
      BS.concat
        [ "\x8c"
        , definiteBytes inputPreimage
        , definiteBytes referenceInputPreimage
        , definiteBytes outputPreimage
        , "\x1b" <> be 8 fee'
        , cborInt validityStart'
        , cborInt validityEnd'
        , definiteBytes observerPreimage
        , definiteBytes signerPreimage
        , definiteBytes mintPreimage
        , definiteBytes scriptIntegrityHash'
        , definiteBytes auxiliaryDataHash'
        , cborInt networkId'
        ]

bodyDataItems :: [Data]
bodyDataItems =
  [ B inputPreimage
  , B referenceInputPreimage
  , B outputPreimage
  , I fee'
  , I validityStart'
  , I validityEnd'
  , B observerPreimage
  , B signerPreimage
  , B mintPreimage
  , B scriptIntegrityHash'
  , B auxiliaryDataHash'
  , I networkId'
  ]

bodyDataForm :: Data
bodyDataForm = List bodyDataItems

witnessSetDataForm :: Data
witnessSetDataForm =
  List [B addressWitnessPreimage, B scriptWitnessPreimage, B redeemerPreimage]

--------------------------------------------------------------------------------
-- The compact form and its commitments
--------------------------------------------------------------------------------

compactBodyCbor :: BS.ByteString
compactBodyCbor =
  BS.concat
    [ "\x8c"
    , defBytes32 (blake2b256 inputPreimage)
    , defBytes32 (blake2b256 referenceInputPreimage)
    , defBytes32 (blake2b256 outputPreimage)
    , cborInt fee'
    , cborInt validityStart'
    , cborInt validityEnd'
    , defBytes32 (blake2b256 observerPreimage)
    , defBytes32 (blake2b256 signerPreimage)
    , defBytes32 (blake2b256 mintPreimage)
    , defBytes32 scriptIntegrityHash'
    , defBytes32 auxiliaryDataHash'
    , cborInt networkId'
    ]

witnessSetCompactCbor :: BS.ByteString
witnessSetCompactCbor =
  BS.concat
    [ "\x83"
    , defBytes32 (blake2b256 addressWitnessPreimage)
    , defBytes32 (blake2b256 scriptWitnessPreimage)
    , defBytes32 (blake2b256 redeemerPreimage)
    ]

compactCbor :: BS.ByteString
compactCbor =
  BS.concat
    ["\x84", cborInt 1, compactBodyCbor, defBytes32 (blake2b256 witnessSetCompactCbor), cborInt 0]

-- | Script before address, matching the wire and not the record.
encodeLengths :: [Integer] -> BS.ByteString
encodeLengths ls =
  BS.concat $ "\x89" : map cborInt [ls !! 0, ls !! 1, ls !! 2, ls !! 3, ls !! 4, ls !! 5, ls !! 6, ls !! 7, ls !! 8]

lengthsCbor :: BS.ByteString
lengthsCbor = encodeLengths fieldLengths

txId :: BS.ByteString
txId = blake2b256 ("MidgardNativeTxBodyV1" <> cborInt 1 <> compactBodyCbor)

proofCommitmentOf :: [Integer] -> BS.ByteString
proofCommitmentOf ls =
  blake2b256 $
    "MidgardNativeTxProofSourceV1"
      <> cborInt 1
      <> ( "\x83"
             <> definiteBytes compactCbor
             <> definiteBytes witnessSetCompactCbor
             <> definiteBytes (encodeLengths ls)
         )

proofCommitment :: BS.ByteString
proofCommitment = proofCommitmentOf fieldLengths

--------------------------------------------------------------------------------
-- CBOR primitives, independent of the port
--------------------------------------------------------------------------------

cborInt :: Integer -> BS.ByteString
cborInt n
  | n >= 0 = major 0 n
  | otherwise = major 1 (-1 - n)
  where
    major base v
      | v <= 23 = BS.pack [fromIntegral (base * 32 + v)]
      | v <= 255 = BS.pack [fromIntegral (base * 32 + 24), fromIntegral v]
      | v <= 65535 = BS.pack [fromIntegral (base * 32 + 25)] <> be 2 v
      | v <= 4294967295 = BS.pack [fromIntegral (base * 32 + 26)] <> be 4 v
      | otherwise = BS.pack [fromIntegral (base * 32 + 27)] <> be 8 v

definiteBytes :: BS.ByteString -> BS.ByteString
definiteBytes bytes
  | len <= 23 = BS.pack [fromIntegral (64 + len)] <> bytes
  | len <= 255 = BS.pack [0x58, fromIntegral len] <> bytes
  | len <= 65535 = BS.pack [0x59] <> be 2 (fromIntegral len) <> bytes
  | otherwise = error "reference definiteBytes: out of fixture range"
  where
    len = BS.length bytes

defBytes32 :: BS.ByteString -> BS.ByteString
defBytes32 h = "\x58\x20" <> h

arrayHeader :: Int -> BS.ByteString
arrayHeader n
  | n <= 23 = BS.pack [fromIntegral (128 + n)]
  | n <= 255 = BS.pack [0x98, fromIntegral n]
  | otherwise = error "reference arrayHeader: out of fixture range"

mapHeader :: Int -> BS.ByteString
mapHeader n
  | n <= 23 = BS.pack [fromIntegral (160 + n)]
  | otherwise = error "reference mapHeader: out of fixture range"

be :: Int -> Integer -> BS.ByteString
be width n =
  BS.pack [fromIntegral (n `div` (256 ^ i) `mod` 256) | i <- reverse [0 .. width - 1]]

blake2b256 :: BS.ByteString -> BS.ByteString
blake2b256 = fromBuiltin . Builtins.blake2b_256 . toBuiltin

--------------------------------------------------------------------------------
-- Building the Plutarch values
--------------------------------------------------------------------------------

plist :: forall s a. PIsListLike PBuiltinList a => [Term s a] -> Term s (PBuiltinList a)
plist = foldr (\x acc -> pcons # x # acc) pnil

inputsT :: forall s. [(BS.ByteString, Integer)] -> Term s (PBuiltinList (PAsData PMidgardTxInput))
inputsT = plist . map one
  where
    one (txid, index) =
      pdata . pcon $
        PMidgardTxInput
          { ptxInput'txId = pdata (pconstant txid)
          , ptxInput'outputIndex = pdata (pconstant index)
          }

outputsT :: forall s. [(BS.ByteString, Integer)] -> Term s (PBuiltinList (PAsData PMidgardTxOutput))
outputsT = plist . map one
  where
    one (paymentHash, lovelace) =
      pdata . pcon $
        PMidgardTxOutput
          { ptxOutput'address =
              pdata . pcon $
                PMidgardAddress
                  { paddress'protected = pdata (pconstant False)
                  , paddress'networkId = pdata (pconstant 0)
                  , paddress'paymentCredential =
                      pdata (pcon (PMidgardPubKeyCredential (pdata (pconstant paymentHash))))
                  , paddress'stakeCredential = pdata (pcon PDNothing)
                  }
          , ptxOutput'value =
              pdata . pcon $
                PMidgardValue
                  { pvalue'lovelace = pdata (pconstant lovelace)
                  , pvalue'assets = pdata (pcon (PAssocMap pnil))
                  }
          , ptxOutput'datumCbor = pdata (pcon PDNothing)
          , ptxOutput'scriptRef = pdata (pcon PDNothing)
          }

bytesT :: forall s. [BS.ByteString] -> Term s (PBuiltinList (PAsData PByteString))
bytesT = plist . map (pdata . pconstant)

mintT :: forall s. Term s PData
mintT =
  pconstant $
    Map [(B policyId, Map [(B name, I q) | (name, q) <- assets]) | (policyId, assets) <- mint']

addressWitnessesT ::
  forall s. [(BS.ByteString, BS.ByteString)] -> Term s (PBuiltinList (PAsData PMidgardAddressWitness))
addressWitnessesT = plist . map one
  where
    one (key, signature) =
      pdata . pcon $
        PMidgardAddressWitness
          { paddressWitness'verificationKey = pdata (pconstant key)
          , paddressWitness'signature = pdata (pconstant signature)
          }

scriptsT :: forall s. [(Integer, BS.ByteString)] -> Term s (PBuiltinList (PAsData PMidgardVersionedScript))
scriptsT = plist . map one
  where
    one (tag, bytes) =
      pdata . pcon $
        PMidgardVersionedScript
          { pversionedScript'language = pdata (langT tag)
          , pversionedScript'scriptBytes = pdata (pconstant bytes)
          }
    langT 0 = pcon PNativeCardanoScript
    langT 3 = pcon PPlutusV3Script
    langT 128 = pcon PMidgardV1Script
    langT n = error ("no such script language tag in fixtures: " <> show n)

redeemersT ::
  forall s.
  [(Integer, Integer, BS.ByteString, Integer, Integer)] ->
  Term s (PBuiltinList (PAsData PMidgardRedeemerWitness))
redeemersT = plist . map one
  where
    one (purpose, index, cbor, memory, steps) =
      pdata . pcon $
        PMidgardRedeemerWitness
          { predeemerWitness'purpose = pdata (purposeT purpose)
          , predeemerWitness'index = pdata (pconstant index)
          , predeemerWitness'redeemerCbor = pdata (pconstant cbor)
          , predeemerWitness'executionUnits =
              pdata . pcon $
                PMidgardExecutionUnits
                  { pexecutionUnits'memory = pdata (pconstant memory)
                  , pexecutionUnits'steps = pdata (pconstant steps)
                  }
          }
    purposeT 0 = pcon PSpendRedeemer
    purposeT n = error ("no such redeemer purpose in fixtures: " <> show n)

bodyT :: forall s. Term s PMidgardTransactionBody
bodyT =
  pcon $
    PMidgardTransactionBody
      { pbody'inputs = pdata (inputsT inputs')
      , pbody'referenceInputs = pdata (inputsT referenceInputs')
      , pbody'outputs = pdata (outputsT outputs')
      , pbody'fee = pdata (pconstant fee')
      , pbody'validityIntervalStart = pdata (pconstant validityStart')
      , pbody'validityIntervalEnd = pdata (pconstant validityEnd')
      , pbody'requiredObservers = pdata (bytesT observers')
      , pbody'requiredSigners = pdata (bytesT signers')
      , pbody'mint = mintT
      , pbody'scriptIntegrityHash = pdata (pconstant scriptIntegrityHash')
      , pbody'auxiliaryDataHash = pdata (pconstant auxiliaryDataHash')
      , pbody'networkId = pdata (pconstant networkId')
      }

witnessSetT :: forall s. Term s PMidgardTransactionWitnessSet
witnessSetT =
  pcon $
    PMidgardTransactionWitnessSet
      { pwitnessSet'addrTxWits = pdata (addressWitnessesT addressWitnesses')
      , pwitnessSet'scriptTxWits = pdata (scriptsT scriptWitnesses')
      , pwitnessSet'redeemerTxWits = pdata (redeemersT redeemers')
      }

txWithVersion :: forall s. Integer -> Term s PMidgardTransaction
txWithVersion version =
  pcon $
    PMidgardTransaction
      { ptransaction'version = pdata (pconstant version)
      , ptransaction'validity = pdata (pcon PTxIsValid)
      , ptransaction'body = pdata bodyT
      , ptransaction'witnessSet = pdata witnessSetT
      }

txT :: forall s. Term s PMidgardTransaction
txT = txWithVersion 1

compactBodyT :: forall s. Term s PNativeTxBodyCompact
compactBodyT =
  pcon $
    PNativeTxBodyCompact
      { pbodyCompact'spendInputsHash = pconstant (blake2b256 inputPreimage)
      , pbodyCompact'referenceInputsHash = pconstant (blake2b256 referenceInputPreimage)
      , pbodyCompact'outputsHash = pconstant (blake2b256 outputPreimage)
      , pbodyCompact'fee = pconstant fee'
      , pbodyCompact'validityIntervalStart = pconstant validityStart'
      , pbodyCompact'validityIntervalEnd = pconstant validityEnd'
      , pbodyCompact'requiredObserversHash = pconstant (blake2b256 observerPreimage)
      , pbodyCompact'requiredSignersHash = pconstant (blake2b256 signerPreimage)
      , pbodyCompact'mintHash = pconstant (blake2b256 mintPreimage)
      , pbodyCompact'scriptIntegrityHash = pconstant scriptIntegrityHash'
      , pbodyCompact'auxiliaryDataHash = pconstant auxiliaryDataHash'
      , pbodyCompact'networkId = pconstant networkId'
      }

witnessSetCompactT :: forall s. Term s PNativeTxWitnessSetCompact
witnessSetCompactT =
  pcon $
    PNativeTxWitnessSetCompact
      { pwitnessSetCompact'addrTxWitsHash = pdata (pconstant (blake2b256 addressWitnessPreimage))
      , pwitnessSetCompact'scriptTxWitsHash = pdata (pconstant (blake2b256 scriptWitnessPreimage))
      , pwitnessSetCompact'redeemerTxWitsHash = pdata (pconstant (blake2b256 redeemerPreimage))
      }

otherWitnessSetCompactT :: forall s. Term s PNativeTxWitnessSetCompact
otherWitnessSetCompactT =
  pcon $
    PNativeTxWitnessSetCompact
      { pwitnessSetCompact'addrTxWitsHash = pdata (pconstant (blake2b256 "\x80"))
      , pwitnessSetCompact'scriptTxWitsHash = pdata (pconstant (blake2b256 scriptWitnessPreimage))
      , pwitnessSetCompact'redeemerTxWitsHash = pdata (pconstant (blake2b256 redeemerPreimage))
      }

verifiedCompactT :: forall s. Term s PVerifiedMidgardNativeTxCompact
verifiedCompactT =
  pcon $
    PVerifiedMidgardNativeTxCompact
      { pverified'txId = pconstant txId
      , pverified'version = 1
      , pverified'txCompact =
          pcon $
            PNativeTxCompact
              { pcompact'body = compactBodyT
              , pcompact'witnessSetHash = pconstant (blake2b256 witnessSetCompactCbor)
              , pcompact'validityCode = 0
              }
      }

maybeBytesT :: forall s. Maybe BS.ByteString -> Term s (PAsData (PMaybeData PByteString))
maybeBytesT = pdata . maybe (pcon PDNothing) (pcon . PDJust . pdata . pconstant)

bodyPreimagesT ::
  forall s.
  Maybe BS.ByteString ->
  Maybe BS.ByteString ->
  Term s PMidgardTransactionBodyPartialPreimages
bodyPreimagesT inputs mint =
  pcon $
    PMidgardTransactionBodyPartialPreimages
      { pbodyPreimages'inputs = maybeBytesT inputs
      , pbodyPreimages'referenceInputs = maybeBytesT Nothing
      , pbodyPreimages'outputs = maybeBytesT Nothing
      , pbodyPreimages'requiredObservers = maybeBytesT Nothing
      , pbodyPreimages'requiredSigners = maybeBytesT Nothing
      , pbodyPreimages'mint = maybeBytesT mint
      }

witnessSetPreimagesT :: forall s. Term s PMidgardTransactionWitnessSetPartialPreimages
witnessSetPreimagesT =
  pcon $
    PMidgardTransactionWitnessSetPartialPreimages
      { pwitnessSetPreimages'addrTxWits = maybeBytesT Nothing
      , pwitnessSetPreimages'scriptTxWits = maybeBytesT Nothing
      , pwitnessSetPreimages'redeemerTxWits = maybeBytesT Nothing
      }
