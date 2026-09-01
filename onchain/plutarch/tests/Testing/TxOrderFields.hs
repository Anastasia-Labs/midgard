{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Testing.TxOrderFields
Description : Behavioural tests for the Plutarch port of the transaction-order
              field-carriage validators.

A forced transaction's material is published on L1 a chunk at a time: each chunk
as a preimage UTxO, each preimage acknowledged by a receipt UTxO. Two things
hold that scheme together, and the tests are split along them.

The first is the receipt asset name, which is a hash of every coordinate that
identifies a chunk — order, transaction, field, item, chunk. Because it is a
hash of all of them, a receipt cannot be reused for a different chunk and a chunk
cannot be receipted twice. These tests recompute that name independently in
Haskell rather than asking the validator what it thinks the name is, so an
encoding change on either side shows up as a failure.

The second is that neither a preimage nor a receipt may be spent except while
the order they belong to is being burnt. That is what keeps a forced
transaction's evidence available for as long as anyone could need it.
-}
module Testing.TxOrderFields (tests) where

import Data.Bits (shiftR, (.&.))
import Data.ByteString qualified as BS
import Data.Word (Word8)
import PlutusCore.Data qualified as PD
import PlutusLedgerApi.V1.Address (scriptHashAddress)
import PlutusLedgerApi.V1.Value (CurrencySymbol (..), TokenName (..), Value, flattenValue, singleton)
import PlutusLedgerApi.V3 (
  Address,
  ScriptContext,
  ScriptHash (..),
  ToData,
  TxId (..),
  TxOutRef (..),
  toBuiltinData,
 )
import PlutusTx.Builtins qualified as Builtins
import PlutusTx.Builtins (builtinDataToData, dataToBuiltinData, fromBuiltin, toBuiltin)
import Test.Tasty
import Test.Tasty.HUnit

import Plutarch.Builtin.Crypto (pblake2b_256)
import Plutarch.LedgerApi.Utils (PMaybeData (..))
import Plutarch.LedgerApi.V3 (PScriptContext, PTokenName, PTxInInfo, PTxOutRef)
import Plutarch.Prelude

import Midgard.BoundedCollection (
  PItemProofV1 (..),
  pboundedCollectionCommitment,
  pboundedCollectionVersion,
  phashBoundedCollectionItem,
 )
import Midgard.FraudProofs.NativeTx.Compact (
  pencodeNativeTxBodyCompact,
  pencodeNativeTxCompactV1,
  pencodeNativeTxFieldPreimageLengthsV1,
  pencodeNativeTxWitnessSetCompact,
  pnativeTxIdForVersion,
  pnativeTxProofCommitmentV1,
 )
import Midgard.FraudProofs.NativeTx.Types (
  PNativeTxBodyCompact (..),
  PNativeTxCompact (..),
  PNativeTxFieldPreimageLengthsV1 (..),
  PNativeTxWitnessSetCompact (..),
 )
import Midgard.LedgerState (
  PNativeTxProofSourceV1 (..),
  PTxFieldReceiptV1 (..),
  PTxOrderPayloadV1 (..),
 )
import Midgard.NativeTxFieldAccess (pemptyFieldCommitment)
import Midgard.UserEvents.TxOrder (
  pfieldReceiptAssetName,
  pverifyOrderReceipts,
  pverifyReceiptChainLink,
 )
import Midgard.ValidationMerkle (PFrontierPeak (..))
import Midgard.Validators.CekProgramMaterial (cekProgramMaterialSpendValidator)
import Midgard.Validators.TxOrderFields (
  txFieldPreimageSpendValidator,
  txFieldReceiptSpendValidator,
 )
import Testing.Eval (passertEval, pfails, psucceeds)
import Testing.ScriptContextBuilder (
  ScriptContextBuilder,
  buildScriptContext,
  currencySymbolFromHex,
  mkAdaValue,
  withAddress,
  withInlineDatum,
  withMint,
  withOutRef,
  withSpendingScript,
  withValue,
 )

-- | Collects the tests defined in this module.
tests :: TestTree
tests =
  testGroup
    "Tx Order Field Carriage Tests"
    [ testGroup
        "order receipt activation"
        [ testCase "empty_material_activates_without_a_terminal_receipt" $
            passertEval $ verifyReceiptlessOrder emptyMaterialBody
        , testCase "non_empty_material_without_a_terminal_receipt_fails_closed" $
            passertEval $ pnot #$ verifyReceiptlessOrder nonEmptyMaterialBody
        , testCase "first_receipt_authenticates_one_item_material" $
            passertEval firstReceiptChainIsValid
        , testCase "later_non_empty_field_links_to_the_authenticated_predecessor" $
            passertEval laterNonEmptyFieldLinksToAuthenticatedPredecessor
        , testCase "script_field_6_precedes_address_field_7_with_exact_receipt_sizes" $
            passertEval scriptFieldPrecedesAddressFieldWithExactSizes
        , testCase "later_field_without_its_predecessor_fails_closed" $
            passertEval $ pnot # laterFieldWithoutPredecessor
        , testCase "malformed_encoded_size_fails_closed" $
            passertEval $ pnot # malformedEncodedSize
        ]
    , testGroup
        "fieldReceiptAssetName"
        [ testCase "agrees with an independent recomputation" $
            passertEval $ pto (nameOf defaultCoords) #== pconstant (expectedName defaultCoords)
        , -- Every coordinate is inside the hash, so no two chunks anywhere in
          -- the protocol share a receipt name.
          testCase "a different field index gives a different name" $
            distinct defaultCoords defaultCoords {cField = 2}
        , testCase "a different item index gives a different name" $
            distinct defaultCoords defaultCoords {cItem = 4}
        , testCase "a different chunk index gives a different name" $
            distinct defaultCoords defaultCoords {cChunk = 8}
        , testCase "a different transaction commitment gives a different name" $
            distinct defaultCoords defaultCoords {cCommitment = BS.replicate 32 0xbb}
        , testCase "a different order output index gives a different name" $
            distinct defaultCoords defaultCoords {cOutRef = TxOutRef orderTxId 1}
        , -- The bounds are `expect`s in the original, so they error.
          testCase "rejects a policy id that is not 28 bytes" $
            pfails $ nameOf defaultCoords {cPolicy = CurrencySymbol (toBuiltin (BS.replicate 27 0x33))}
        , testCase "rejects a transaction commitment that is not 32 bytes" $
            pfails $ nameOf defaultCoords {cCommitment = BS.replicate 31 0xaa}
        , testCase "rejects a negative output index" $
            pfails $ nameOf defaultCoords {cOutRef = TxOutRef orderTxId (-1)}
        , -- A Midgard transaction has nine fields and the index is encoded in
          -- one byte, so a tenth would alias an existing name.
          testCase "rejects a field index of nine" $
            pfails $ nameOf defaultCoords {cField = 9}
        , testCase "rejects a negative field index" $
            pfails $ nameOf defaultCoords {cField = -1}
        , testCase "rejects a negative item index" $
            pfails $ nameOf defaultCoords {cItem = -1}
        , testCase "rejects a negative chunk index" $
            pfails $ nameOf defaultCoords {cChunk = -1}
        ]
    , testGroup
        "spend / tx-field-preimage-v1"
        [ testCase "accepts a spend that burns the order and the receipt" $
            psucceeds $ runPreimage defaultCoords (orderBurn <> receiptBurn defaultCoords)
        , -- Neither burn on its own releases the material.
          testCase "rejects a spend burning only the order NFT" $
            pfails $ runPreimage defaultCoords orderBurn
        , testCase "rejects a spend burning only the receipt NFT" $
            pfails $ runPreimage defaultCoords (receiptBurn defaultCoords)
        , testCase "rejects a spend burning nothing" $
            pfails $ runPreimage defaultCoords mempty
        , -- `== -1` is exact: minting is not burning, and burning twice over is
          -- not a quantity the ledger could reach for a one-off NFT anyway.
          testCase "rejects a spend minting the order NFT instead" $
            pfails $ runPreimage defaultCoords (orderMint <> receiptBurn defaultCoords)
        , testCase "rejects a spend burning two of the receipt NFT" $
            pfails $ runPreimage defaultCoords (orderBurn <> receiptBurnQty defaultCoords (-2))
        , -- The receipt burnt has to be *this chunk's*, which is the whole
          -- point of the name being a hash of the coordinates.
          testCase "rejects a receipt burn for a different chunk" $
            pfails $
              runPreimage defaultCoords (orderBurn <> receiptBurn defaultCoords {cChunk = 7})
        , testCase "rejects a receipt burn under a different policy" $
            pfails $
              runPreimage
                defaultCoords
                (orderBurn <> singleton otherPolicy (receiptName defaultCoords) (-1))
        , testCase "rejects a spend of a datum-less UTxO" $
            pfails $ runNoDatum txFieldPreimageSpendValidator
        ]
    , testGroup
        "spend / tx-field-receipt-spend-v1"
        [ testCase "accepts a spend that burns the order and the receipt" $
            psucceeds $ runReceipt defaultCoords (orderBurn <> receiptBurn defaultCoords)
        , testCase "rejects a spend burning only the order NFT" $
            pfails $ runReceipt defaultCoords orderBurn
        , testCase "rejects a spend burning only the receipt NFT" $
            pfails $ runReceipt defaultCoords (receiptBurn defaultCoords)
        , testCase "rejects a receipt burn for a different chunk" $
            pfails $
              runReceipt defaultCoords (orderBurn <> receiptBurn defaultCoords {cChunk = 7})
        , -- A receipt carries no chunk proof, so its chunk index is its own
          -- field while the field and item indices come off the collection
          -- proof. Every number in the datum is distinct (field 3, item 2,
          -- chunk 5, item_count 16, item_length 64, encoded size 128), so the
          -- name only comes out right if each is read from the right place.
          testCase "reads the chunk index from the receipt's own field" $
            psucceeds $
              runReceipt defaultCoords {cChunk = 5} (orderBurn <> receiptBurn defaultCoords {cChunk = 5})
        , -- The concrete positional slip that test rules out: reading the
          -- collection proof's item count where the chunk index belongs.
          testCase "rejects a receipt named with the item count in the chunk position" $
            pfails $
              runReceipt defaultCoords {cChunk = 5} (orderBurn <> receiptBurn defaultCoords {cChunk = 16})
        , testCase "rejects a spend of a datum-less UTxO" $
            pfails $ runNoDatum txFieldReceiptSpendValidator
        ]
    , testGroup
        "spend / cek-program-material-v1"
        [ -- The address has no spend path at all; published material is
          -- permanent by construction.
          testCase "rejects every spend" $
            pfails $ cekProgramMaterialSpendValidator # pconstant (spendCtx (PD.I 0) mempty)
        ]
    ]

verifyReceiptlessOrder ::
  forall s.
  Term s PNativeTxBodyCompact ->
  Term s PBool
verifyReceiptlessOrder body =
  pverifyOrderReceipts
    # (pnil :: Term s (PBuiltinList (PAsData PTxInInfo)))
    # pdata (pconstant receiptScriptHash)
    # pdata (pconstant receiptPolicy)
    # pdata (pconstant orderPolicy)
    # pdata (pconstant spentRef)
    # receiptlessPayload body

receiptlessPayload :: forall s. Term s PNativeTxBodyCompact -> Term s PTxOrderPayloadV1
receiptlessPayload body =
  pcon $
    PTxOrderPayloadV1
      { ptxOrderPayload'txId = pdata transactionId
      , ptxOrderPayload'transactionCommitment = pdata transactionCommitment
      , ptxOrderPayload'source =
          pdata $
            pcon $
              PNativeTxProofSourceV1
                { pnativeSource'compactCbor = pdata compactCbor'
                , pnativeSource'witnessSetCompactCbor = pdata receiptWitnessSetCbor
                , pnativeSource'fieldPreimageLengthsCbor = pdata receiptLengthsCbor
                }
      , ptxOrderPayload'terminalReceiptReference =
          pforgetData
            ( pdata
                (pcon PDNothing :: Term s (PMaybeData PTxOutRef))
            )
      }
  where
    compactCbor' = receiptCompactCbor body
    transactionId = pnativeTxIdForVersion # 1 # (pencodeNativeTxBodyCompact # body)
    transactionCommitment =
      pnativeTxProofCommitmentV1
        # compactCbor'
        # receiptWitnessSetCbor
        # receiptLengthsCbor

receiptCompactCbor :: forall s. Term s PNativeTxBodyCompact -> Term s PByteString
receiptCompactCbor body =
  pencodeNativeTxCompactV1
    # pcon
      ( PNativeTxCompact
          { pcompact'body = body
          , pcompact'witnessSetHash = pblake2b_256 # receiptWitnessSetCbor
          , pcompact'validityCode = 0
          }
      )

receiptWitnessSetCbor :: forall s. Term s PByteString
receiptWitnessSetCbor = pencodeNativeTxWitnessSetCompact # receiptWitnessSet

receiptWitnessSet :: forall s. Term s PNativeTxWitnessSetCompact
receiptWitnessSet =
  pcon $
    PNativeTxWitnessSetCompact
      { pwitnessSetCompact'addrTxWitsHash = pdata pemptyFieldCommitment
      , pwitnessSetCompact'scriptTxWitsHash = pdata pemptyFieldCommitment
      , pwitnessSetCompact'redeemerTxWitsHash = pdata pemptyFieldCommitment
      }

emptyMaterialBody :: forall s. Term s PNativeTxBodyCompact
emptyMaterialBody = materialBody pemptyFieldCommitment

nonEmptyMaterialBody :: forall s. Term s PNativeTxBodyCompact
nonEmptyMaterialBody = materialBody (pconstant $ BS.replicate 32 0xaa)

materialBody :: forall s. Term s PByteString -> Term s PNativeTxBodyCompact
materialBody spendInputsHash =
  pcon $
    PNativeTxBodyCompact
      { pbodyCompact'spendInputsHash = spendInputsHash
      , pbodyCompact'referenceInputsHash = pemptyFieldCommitment
      , pbodyCompact'outputsHash = pemptyFieldCommitment
      , pbodyCompact'fee = 0
      , pbodyCompact'validityIntervalStart = 0
      , pbodyCompact'validityIntervalEnd = 0
      , pbodyCompact'requiredObserversHash = pemptyFieldCommitment
      , pbodyCompact'requiredSignersHash = pemptyFieldCommitment
      , pbodyCompact'mintHash = pemptyFieldCommitment
      , pbodyCompact'scriptIntegrityHash = pconstant (BS.replicate 32 0x11)
      , pbodyCompact'auxiliaryDataHash = pconstant (BS.replicate 32 0x22)
      , pbodyCompact'networkId = 0
      }

receiptLengthsCbor :: forall s. Term s PByteString
receiptLengthsCbor = pconstant "\x89\x01\x01\x01\x01\x01\x01\x01\x01\x01"

firstReceiptChainIsValid :: forall s. Term s PBool
firstReceiptChainIsValid =
  pverifyReceiptChainLink
    # countedTransactionId
    # countedTransactionCommitment
    # countedSource
    # pdata (pconstant receiptPolicy)
    # pdata (pconstant orderPolicy)
    # pdata (pconstant spentRef)
    # pcon PNothing
    # firstReceipt

countedItemCommitment :: forall s. Term s PByteString
countedItemCommitment = pconstant (BS.replicate 32 0xab)

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
countedCollectionCommitment =
  pboundedCollectionCommitment # 0 # 1 # countedCollectionFrontier

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

countedBody :: forall s. Term s PNativeTxBodyCompact
countedBody = materialBody countedCollectionCommitment

countedCompactCbor :: forall s. Term s PByteString
countedCompactCbor = receiptCompactCbor countedBody

countedLengthsCbor :: forall s. Term s PByteString
countedLengthsCbor = pconstant "\x89\x03\x01\x01\x01\x01\x01\x01\x01\x01"

countedTransactionId :: forall s. Term s PByteString
countedTransactionId =
  pnativeTxIdForVersion # 1 # (pencodeNativeTxBodyCompact # countedBody)

countedTransactionCommitment :: forall s. Term s PByteString
countedTransactionCommitment =
  pnativeTxProofCommitmentV1
    # countedCompactCbor
    # receiptWitnessSetCbor
    # countedLengthsCbor

countedSource :: forall s. Term s PNativeTxProofSourceV1
countedSource =
  pcon $
    PNativeTxProofSourceV1
      { pnativeSource'compactCbor = pdata countedCompactCbor
      , pnativeSource'witnessSetCompactCbor = pdata receiptWitnessSetCbor
      , pnativeSource'fieldPreimageLengthsCbor = pdata countedLengthsCbor
      }

firstReceipt :: forall s. Term s PTxFieldReceiptV1
firstReceipt =
  pcon $
    PTxFieldReceiptV1
      { ptxFieldReceipt'fieldReceiptPolicyId = pdata (pconstant receiptPolicy)
      , ptxFieldReceipt'txOrderPolicyId = pdata (pconstant orderPolicy)
      , ptxFieldReceipt'txOrderId = pdata (pconstant spentRef)
      , ptxFieldReceipt'transactionCommitment = pdata countedTransactionCommitment
      , ptxFieldReceipt'collectionProof = pdata countedCollectionProof
      , ptxFieldReceipt'chunkIndex = pdata 0
      , ptxFieldReceipt'fieldReference = pdata (pconstant spentRef)
      , ptxFieldReceipt'predecessorReceiptReference =
          pforgetData
            ( pdata
                (pcon PDNothing :: Term s (PMaybeData PTxOutRef))
            )
      , ptxFieldReceipt'fieldEncodedSize = pdata 3
      }

--------------------------------------------------------------------------------
-- Multi-field receipt-chain routing
--------------------------------------------------------------------------------

laterNonEmptyFieldLinksToAuthenticatedPredecessor :: forall s. Term s PBool
laterNonEmptyFieldLinksToAuthenticatedPredecessor =
  let source = twoBodyFieldSource
      transactionId' = sourceTransactionId twoBodyFieldBody
      transactionCommitment' =
        sourceCommitment twoBodyFieldCompactCbor receiptWitnessSetCbor twoBodyFieldLengthsCbor
      previous =
        chainReceipt
          transactionCommitment'
          (singleItemProof 0 38 0xa0)
          (TxOutRef orderTxId 0)
          nothingReceiptReference
          41
      current =
        chainReceipt
          transactionCommitment'
          (singleItemProof 4 28 0xa4)
          (TxOutRef orderTxId 1)
          (justReceiptReference $ TxOutRef orderTxId 0)
          31
   in verifyChainLink transactionId' transactionCommitment' source (pcon PNothing) previous
        #&& verifyChainLink transactionId' transactionCommitment' source (pcon $ PJust previous) current

scriptFieldPrecedesAddressFieldWithExactSizes :: forall s. Term s PBool
scriptFieldPrecedesAddressFieldWithExactSizes =
  let source = witnessFieldSource
      transactionId' = sourceTransactionId emptyMaterialBody
      transactionCommitment' =
        sourceCommitment witnessFieldCompactCbor witnessFieldWitnessSetCbor witnessFieldLengthsCbor
      scriptReceipt =
        chainReceipt
          transactionCommitment'
          (singleItemProof 6 6 0xa6)
          (TxOutRef orderTxId 6)
          nothingReceiptReference
          7
      addressReceipt =
        chainReceipt
          transactionCommitment'
          (singleItemProof 7 101 0xa7)
          (TxOutRef orderTxId 7)
          (justReceiptReference $ TxOutRef orderTxId 0)
          104
   in verifyChainLink transactionId' transactionCommitment' source (pcon PNothing) scriptReceipt
        #&& verifyChainLink
          transactionId'
          transactionCommitment'
          source
          (pcon $ PJust scriptReceipt)
          addressReceipt

laterFieldWithoutPredecessor :: forall s. Term s PBool
laterFieldWithoutPredecessor =
  let transactionId' = sourceTransactionId twoBodyFieldBody
      transactionCommitment' =
        sourceCommitment twoBodyFieldCompactCbor receiptWitnessSetCbor twoBodyFieldLengthsCbor
      signerReceipt =
        chainReceipt
          transactionCommitment'
          (singleItemProof 4 28 0xa4)
          (TxOutRef orderTxId 1)
          nothingReceiptReference
          31
   in verifyChainLink
        transactionId'
        transactionCommitment'
        twoBodyFieldSource
        (pcon PNothing)
        signerReceipt

malformedEncodedSize :: forall s. Term s PBool
malformedEncodedSize =
  let transactionId' = sourceTransactionId twoBodyFieldBody
      transactionCommitment' =
        sourceCommitment twoBodyFieldCompactCbor receiptWitnessSetCbor twoBodyFieldLengthsCbor
      malformed =
        chainReceipt
          transactionCommitment'
          (singleItemProof 0 38 0xa0)
          (TxOutRef orderTxId 0)
          nothingReceiptReference
          42
   in verifyChainLink
        transactionId'
        transactionCommitment'
        twoBodyFieldSource
        (pcon PNothing)
        malformed

verifyChainLink ::
  forall s.
  Term s PByteString ->
  Term s PByteString ->
  Term s PNativeTxProofSourceV1 ->
  Term s (PMaybe PTxFieldReceiptV1) ->
  Term s PTxFieldReceiptV1 ->
  Term s PBool
verifyChainLink transactionId' transactionCommitment' source predecessor receipt =
  pverifyReceiptChainLink
    # transactionId'
    # transactionCommitment'
    # source
    # pdata (pconstant receiptPolicy)
    # pdata (pconstant orderPolicy)
    # pdata (pconstant spentRef)
    # predecessor
    # receipt

chainReceipt ::
  forall s.
  Term s PByteString ->
  Term s PItemProofV1 ->
  TxOutRef ->
  Term s PData ->
  Integer ->
  Term s PTxFieldReceiptV1
chainReceipt transactionCommitment' proof fieldRef predecessorReference encodedSize =
  pcon $
    PTxFieldReceiptV1
      { ptxFieldReceipt'fieldReceiptPolicyId = pdata (pconstant receiptPolicy)
      , ptxFieldReceipt'txOrderPolicyId = pdata (pconstant orderPolicy)
      , ptxFieldReceipt'txOrderId = pdata (pconstant spentRef)
      , ptxFieldReceipt'transactionCommitment = pdata transactionCommitment'
      , ptxFieldReceipt'collectionProof = pdata proof
      , ptxFieldReceipt'chunkIndex = pdata 0
      , ptxFieldReceipt'fieldReference = pdata (pconstant fieldRef)
      , ptxFieldReceipt'predecessorReceiptReference = predecessorReference
      , ptxFieldReceipt'fieldEncodedSize = pdata (pconstant encodedSize)
      }

nothingReceiptReference :: forall s. Term s PData
nothingReceiptReference =
  pforgetData $ pdata (pcon PDNothing :: Term s (PMaybeData PTxOutRef))

justReceiptReference :: forall s. TxOutRef -> Term s PData
justReceiptReference ref =
  pforgetData $
    pdata
      ( pcon (PDJust $ pdata $ pconstant ref) ::
          Term s (PMaybeData PTxOutRef)
      )

singleItemProof :: forall s. Integer -> Integer -> Word8 -> Term s PItemProofV1
singleItemProof fieldIndex itemLength seed =
  pcon $
    PItemProofV1
      { pitemProof'version = pdata pboundedCollectionVersion
      , pitemProof'fieldIndex = pdata (pconstant fieldIndex)
      , pitemProof'itemCount = pdata 1
      , pitemProof'itemIndex = pdata 0
      , pitemProof'itemLength = pdata (pconstant itemLength)
      , pitemProof'itemCommitment = pdata (pconstant $ BS.replicate 32 seed)
      , pitemProof'frontier = pdata (singleItemFrontier fieldIndex itemLength seed)
      , pitemProof'siblings = pdata pnil
      }

singleItemFrontier ::
  forall s.
  Integer ->
  Integer ->
  Word8 ->
  Term s (PBuiltinList (PAsData PFrontierPeak))
singleItemFrontier fieldIndex itemLength seed =
  pcons
    # pdata
      ( pcon $
          PFrontierPeak
            (pdata 0)
            ( pdata $
                phashBoundedCollectionItem
                  # pconstant fieldIndex
                  # 0
                  # pconstant itemLength
                  # pconstant (BS.replicate 32 seed)
            )
      )
    # pnil

singleItemCommitment :: forall s. Integer -> Integer -> Word8 -> Term s PByteString
singleItemCommitment fieldIndex itemLength seed =
  pboundedCollectionCommitment
    # pconstant fieldIndex
    # 1
    # singleItemFrontier fieldIndex itemLength seed

twoBodyFieldSource :: forall s. Term s PNativeTxProofSourceV1
twoBodyFieldSource = sourceFrom twoBodyFieldCompactCbor receiptWitnessSetCbor twoBodyFieldLengthsCbor

witnessFieldSource :: forall s. Term s PNativeTxProofSourceV1
witnessFieldSource = sourceFrom witnessFieldCompactCbor witnessFieldWitnessSetCbor witnessFieldLengthsCbor

sourceFrom ::
  forall s.
  Term s PByteString ->
  Term s PByteString ->
  Term s PByteString ->
  Term s PNativeTxProofSourceV1
sourceFrom compactCbor witnessSetCbor lengthsCbor =
  pcon $
    PNativeTxProofSourceV1
      { pnativeSource'compactCbor = pdata compactCbor
      , pnativeSource'witnessSetCompactCbor = pdata witnessSetCbor
      , pnativeSource'fieldPreimageLengthsCbor = pdata lengthsCbor
      }

sourceTransactionId :: forall s. Term s PNativeTxBodyCompact -> Term s PByteString
sourceTransactionId body =
  pnativeTxIdForVersion # 1 # (pencodeNativeTxBodyCompact # body)

sourceCommitment ::
  forall s.
  Term s PByteString ->
  Term s PByteString ->
  Term s PByteString ->
  Term s PByteString
sourceCommitment compactCbor witnessSetCbor lengthsCbor =
  pnativeTxProofCommitmentV1 # compactCbor # witnessSetCbor # lengthsCbor

twoBodyFieldCompactCbor :: forall s. Term s PByteString
twoBodyFieldCompactCbor = compactCborFor twoBodyFieldBody receiptWitnessSetCbor

witnessFieldCompactCbor :: forall s. Term s PByteString
witnessFieldCompactCbor = compactCborFor emptyMaterialBody witnessFieldWitnessSetCbor

compactCborFor ::
  forall s.
  Term s PNativeTxBodyCompact ->
  Term s PByteString ->
  Term s PByteString
compactCborFor body witnessSetCbor =
  pencodeNativeTxCompactV1
    # pcon
      ( PNativeTxCompact
          { pcompact'body = body
          , pcompact'witnessSetHash = pblake2b_256 # witnessSetCbor
          , pcompact'validityCode = 0
          }
      )

twoBodyFieldBody :: forall s. Term s PNativeTxBodyCompact
twoBodyFieldBody =
  pcon $
    PNativeTxBodyCompact
      { pbodyCompact'spendInputsHash = singleItemCommitment 0 38 0xa0
      , pbodyCompact'referenceInputsHash = pemptyFieldCommitment
      , pbodyCompact'outputsHash = pemptyFieldCommitment
      , pbodyCompact'fee = 0
      , pbodyCompact'validityIntervalStart = 0
      , pbodyCompact'validityIntervalEnd = 0
      , pbodyCompact'requiredObserversHash = pemptyFieldCommitment
      , pbodyCompact'requiredSignersHash = singleItemCommitment 4 28 0xa4
      , pbodyCompact'mintHash = pemptyFieldCommitment
      , pbodyCompact'scriptIntegrityHash = pconstant (BS.replicate 32 0x11)
      , pbodyCompact'auxiliaryDataHash = pconstant (BS.replicate 32 0x22)
      , pbodyCompact'networkId = 0
      }

witnessFieldWitnessSetCbor :: forall s. Term s PByteString
witnessFieldWitnessSetCbor =
  pencodeNativeTxWitnessSetCompact
    # pcon
      ( PNativeTxWitnessSetCompact
          { pwitnessSetCompact'addrTxWitsHash = pdata (singleItemCommitment 7 101 0xa7)
          , pwitnessSetCompact'scriptTxWitsHash = pdata (singleItemCommitment 6 6 0xa6)
          , pwitnessSetCompact'redeemerTxWitsHash = pdata pemptyFieldCommitment
          }
      )

twoBodyFieldLengthsCbor :: forall s. Term s PByteString
twoBodyFieldLengthsCbor = fieldLengthsCbor 41 1 1 1 31 1 1 1 1

witnessFieldLengthsCbor :: forall s. Term s PByteString
witnessFieldLengthsCbor = fieldLengthsCbor 1 1 1 1 1 1 7 104 1

fieldLengthsCbor ::
  forall s.
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  Term s PByteString
fieldLengthsCbor inputs references outputs observers signers mint scripts addresses redeemers =
  pencodeNativeTxFieldPreimageLengthsV1
    # pcon
      ( PNativeTxFieldPreimageLengthsV1
          { plengths'spendInputs = pconstant inputs
          , plengths'referenceInputs = pconstant references
          , plengths'outputs = pconstant outputs
          , plengths'requiredObservers = pconstant observers
          , plengths'requiredSigners = pconstant signers
          , plengths'mint = pconstant mint
          , plengths'addressWitnesses = pconstant addresses
          , plengths'scriptWitnesses = pconstant scripts
          , plengths'redeemers = pconstant redeemers
          }
      )

--------------------------------------------------------------------------------
-- Coordinates
--------------------------------------------------------------------------------

{- | Everything that identifies one published chunk. The receipt asset name is a
hash of exactly this record.
-}
data Coords = Coords
  { cPolicy :: CurrencySymbol
  , cOutRef :: TxOutRef
  , cCommitment :: BS.ByteString
  , cField :: Integer
  , cItem :: Integer
  , cChunk :: Integer
  }

defaultCoords :: Coords
defaultCoords =
  Coords
    { cPolicy = orderPolicy
    , cOutRef = TxOutRef orderTxId 0
    , cCommitment = BS.replicate 32 0xaa
    , cField = 3
    , cItem = 2
    , cChunk = 1
    }

-- | The name the validator computes.
nameOf :: forall s. Coords -> Term s PTokenName
nameOf c =
  pfieldReceiptAssetName
    # pdata (pconstant (cPolicy c))
    # pdata (pconstant (cOutRef c))
    # pconstant (cCommitment c)
    # pconstant (cField c)
    # pconstant (cItem c)
    # pconstant (cChunk c)

{- | The name recomputed here, from the spec rather than from the port. The
big-endian encoder is deliberately hand-written instead of reusing the builtin
the validator calls.
-}
expectedName :: Coords -> BS.ByteString
expectedName c =
  blake2b256 $
    BS.concat
      [ "MidgardTxFieldReceiptV1"
      , unCS (cPolicy c)
      , getTxId (txOutRefId (cOutRef c))
      , bigEndian 8 (txOutRefIdx (cOutRef c))
      , cCommitment c
      , bigEndian 1 (cField c)
      , bigEndian 8 (cItem c)
      , bigEndian 8 (cChunk c)
      ]
  where
    unCS = fromBuiltin . unCurrencySymbol
    getTxId (TxId b) = fromBuiltin b

receiptName :: Coords -> TokenName
receiptName = TokenName . toBuiltin . expectedName

-- | Asserts two coordinate sets hash to different receipt names.
distinct :: Coords -> Coords -> Assertion
distinct a b = passertEval $ pnot #$ pto (nameOf a) #== pto (nameOf b)

--------------------------------------------------------------------------------
-- Mint fixtures
--------------------------------------------------------------------------------

orderBurn, orderMint :: Value
orderBurn = singleton orderPolicy orderNonce (-1)
orderMint = singleton orderPolicy orderNonce 1

receiptBurn :: Coords -> Value
receiptBurn c = receiptBurnQty c (-1)

receiptBurnQty :: Coords -> Integer -> Value
receiptBurnQty c = singleton receiptPolicy (receiptName c)

{- | The order's event NFT name: blake2b-256 of the serialised output reference
that created it, per @user_events.out_ref_to_nonce@.
-}
orderNonce :: TokenName
orderNonce =
  TokenName . toBuiltin . blake2b256 . fromBuiltin . Builtins.serialiseData $
    toBuiltinData (cOutRef defaultCoords)

--------------------------------------------------------------------------------
-- Datums and contexts
--------------------------------------------------------------------------------

runPreimage :: forall s. Coords -> Value -> Term s PUnit
runPreimage c minted =
  txFieldPreimageSpendValidator # pconstant (spendCtx (preimageDatum c) minted)

runReceipt :: forall s. Coords -> Value -> Term s PUnit
runReceipt c minted =
  txFieldReceiptSpendValidator # pconstant (spendCtx (receiptDatum c) minted)

-- | A spend of a UTxO carrying no datum at all.
runNoDatum :: forall s. Term s (PScriptContext :--> PUnit) -> Term s PUnit
runNoDatum validator = validator # pconstant ctx
  where
    ctx =
      buildScriptContext $
        withSpendingScript
          (dataToBuiltinData (PD.I 0))
          (withOutRef spentRef <> withAddress fieldAddress <> withValue (mkAdaValue 2_000_000))

spendCtx :: PD.Data -> Value -> ScriptContext
spendCtx datum minted =
  buildScriptContext $
    withSpendingScript
      (dataToBuiltinData (PD.I 0))
      ( withOutRef spentRef
          <> withAddress fieldAddress
          <> withValue (mkAdaValue 2_000_000)
          <> withInlineDatum (dataToBuiltinData datum)
      )
      <> mintOf minted

-- | `withMint` reads a policy off the value, so it is applied per policy.
mintOf :: Value -> ScriptContextBuilder
mintOf v =
  mconcat
    [ withMint (singleton cs tn q) (dataToBuiltinData (PD.I 0))
    | (cs, tn, q) <- flattenValue v
    ]

preimageDatum :: Coords -> PD.Data
preimageDatum c =
  PD.Constr
    0
    [ PD.B (fromBuiltin (unCurrencySymbol receiptPolicy))
    , PD.B (fromBuiltin (unCurrencySymbol (cPolicy c)))
    , toPD (cOutRef c)
    , PD.B (cCommitment c)
    , itemProof c
    , chunkProof c
    ]

receiptDatum :: Coords -> PD.Data
receiptDatum c =
  PD.Constr
    0
    [ PD.B (fromBuiltin (unCurrencySymbol receiptPolicy))
    , PD.B (fromBuiltin (unCurrencySymbol (cPolicy c)))
    , toPD (cOutRef c)
    , PD.B (cCommitment c)
    , itemProof c
    , PD.I (cChunk c)
    , toPD spentRef
    , PD.Constr 1 [] -- predecessor_receipt_reference: None
    , PD.I 128 -- field_encoded_size
    ]

-- | @bounded_collection_v1.ItemProofV1@.
itemProof :: Coords -> PD.Data
itemProof c =
  PD.Constr
    0
    [ PD.I 1 -- version
    , PD.I (cField c)
    , PD.I 16 -- item_count
    , PD.I (cItem c)
    , PD.I 64 -- item_length
    , PD.B (BS.replicate 32 0xcc) -- item_commitment
    , PD.List [] -- frontier
    , PD.List [] -- siblings
    ]

-- | @bounded_item_v1.ChunkProofV1@.
chunkProof :: Coords -> PD.Data
chunkProof c =
  PD.Constr
    0
    [ PD.I 1 -- version
    , PD.I (cField c)
    , PD.I (cItem c)
    , PD.I 256 -- total_length
    , PD.I (cChunk c)
    , PD.B (BS.replicate 16 0xdd) -- chunk
    , PD.List [] -- frontier
    , PD.List [] -- siblings
    ]

--------------------------------------------------------------------------------
-- Identities and plumbing
--------------------------------------------------------------------------------

orderPolicy, receiptPolicy, otherPolicy :: CurrencySymbol
orderPolicy = currencySymbolFromHex (concat (replicate 28 "33"))
receiptPolicy = currencySymbolFromHex (concat (replicate 28 "44"))
otherPolicy = currencySymbolFromHex (concat (replicate 28 "55"))

fieldAddress :: Address
fieldAddress = scriptHashAddress (ScriptHash (toBuiltin (BS.replicate 28 0x66)))

receiptScriptHash :: ScriptHash
receiptScriptHash = ScriptHash (toBuiltin (BS.replicate 28 0x99))

orderTxId :: TxId
orderTxId = TxId (toBuiltin (BS.replicate 32 0x77))

spentRef :: TxOutRef
spentRef = TxOutRef (TxId (toBuiltin (BS.replicate 32 0x88))) 0

blake2b256 :: BS.ByteString -> BS.ByteString
blake2b256 = fromBuiltin . Builtins.blake2b_256 . toBuiltin

-- | Big-endian, fixed width, written out rather than delegating to the builtin.
bigEndian :: Int -> Integer -> BS.ByteString
bigEndian width n = BS.pack [fromIntegral ((n `shiftR` (8 * i)) .&. 0xff) | i <- [width - 1, width - 2 .. 0]]

toPD :: ToData a => a -> PD.Data
toPD = builtinDataToData . toBuiltinData
