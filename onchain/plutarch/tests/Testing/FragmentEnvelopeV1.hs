{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Testing.FragmentEnvelopeV1
Description : Maximum-profile parity for @proof-v1-fragment-envelope.test.ak@.

The JSON fixture is emitted beside the generated Aiken suite by its existing
TypeScript generator.  These tests feed those literal cross-language vectors
through the production receipt-chain and mint-policy functions.
-}
module Testing.FragmentEnvelopeV1 (tests) where

import Data.Aeson (FromJSON, eitherDecodeFileStrict')
import Data.Bits (shiftR, (.&.))
import Data.ByteString qualified as BS
import Data.ByteString.Base16 qualified as Base16
import Data.ByteString.Char8 qualified as BSC
import GHC.Generics (Generic)
import Plutarch.LedgerApi.V3 (PCurrencySymbol, PScriptHash, PTxInInfo, PTxOutRef)
import Plutarch.Prelude
import Plutarch.Unsafe (punsafeCoerce)
import PlutusCore.Data qualified as PD
import PlutusLedgerApi.V1.Address (scriptHashAddress)
import PlutusLedgerApi.V1.Value (
  CurrencySymbol (..),
  TokenName (..),
  singleton,
 )
import PlutusLedgerApi.V3 (
  Address,
  Datum (..),
  OutputDatum (..),
  Redeemer (..),
  ScriptContext (..),
  ScriptHash (..),
  ToData,
  TxId (..),
  TxInInfo (..),
  TxInfo (..),
  TxOut (..),
  TxOutRef (..),
  toBuiltinData,
 )
import PlutusTx.Builtins qualified as Builtins
import PlutusTx.Builtins (builtinDataToData, dataToBuiltinData, fromBuiltin, toBuiltin)
import Test.Tasty
import Test.Tasty.HUnit

import Midgard.LedgerState (PTxOrderPayloadV1)
import Midgard.UserEvents.TxOrder (pverifyOrderReceipts)
import Midgard.Validators.TxFieldReceipt (txFieldReceiptMintValidator)
import Testing.Eval (pfails, psucceeds)
import Testing.ScriptContextBuilder (buildScriptContext, mkAdaValue, withMintingScript)

data Fixture = Fixture
  { transactionId :: String
  , transactionCommitment :: String
  , compactCbor :: String
  , witnessSetCompactCbor :: String
  , fieldPreimageLengthsCbor :: String
  , canonicalTransactionBytes :: Integer
  , maximumCanonicalTransactionBytes :: Integer
  , terminalReceipt :: ReceiptFixture
  , scenarios :: [Scenario]
  }
  deriving stock (Generic)
  deriving anyclass (FromJSON)

data Scenario = Scenario
  { name :: String
  , collectionProof :: CollectionProof
  , chunkProof :: ChunkProof
  , fieldEncodedSize :: Integer
  , predecessor :: Maybe ReceiptFixture
  }
  deriving stock (Generic)
  deriving anyclass (FromJSON)

data ReceiptFixture = ReceiptFixture
  { collectionProof :: CollectionProof
  , chunkIndex :: Integer
  , fieldEncodedSize :: Integer
  }
  deriving stock (Generic)
  deriving anyclass (FromJSON)

data CollectionProof = CollectionProof
  { version :: Integer
  , fieldIndex :: Integer
  , itemCount :: Integer
  , itemIndex :: Integer
  , itemLength :: Integer
  , itemCommitment :: String
  , frontier :: [Peak]
  , siblings :: [String]
  }
  deriving stock (Generic)
  deriving anyclass (FromJSON)

data ChunkProof = ChunkProof
  { version :: Integer
  , fieldIndex :: Integer
  , itemIndex :: Integer
  , totalLength :: Integer
  , chunkIndex :: Integer
  , chunk :: String
  , frontier :: [Peak]
  , siblings :: [String]
  }
  deriving stock (Generic)
  deriving anyclass (FromJSON)

data Peak = Peak
  { height :: Integer
  , hash :: String
  }
  deriving stock (Generic)
  deriving anyclass (FromJSON)

tests :: TestTree
tests =
  testGroup "Proof V1 Fragment Envelope Aiken Parity" $
    [ testCase "maximum_profile_terminal_receipt_authenticates_complete_material_chain" $ do
        fixture <- loadFixture
        psucceeds $ terminalReceiptAuthenticates fixture
    , testCase "generated_profile_fixture_is_near_the_derived_transaction_maximum" $ do
        fixture <- loadFixture
        assertBool "a representative chunk exceeds 4,095 bytes" $
          all (\scenario -> BS.length (hex scenario.chunkProof.chunk) <= 4_095) fixture.scenarios
        fixture.canonicalTransactionBytes @?= 51_344
        fixture.maximumCanonicalTransactionBytes @?= 295_041
        assertBool "fixture is not near the generated maximum profile" $
          fixture.canonicalTransactionBytes > 50_000
            && fixture.canonicalTransactionBytes <= fixture.maximumCanonicalTransactionBytes
    ]
      <> map publicationCase publicationCaseNames
      <> [ testCase "receipt_publication_without_the_referenced_fragment_fails_closed" $ do
            fixture <- loadFixture
            let scenario = scenarioNamed (head publicationCaseNames) fixture
            pfails $ runPublication (withoutReferenceInputs $ publicationContext fixture scenario)
         ]

publicationCaseNames :: [String]
publicationCaseNames =
  [ "maximum_profile_field_0_item_0_chunk_0_verifies_independently_on_l1"
  , "maximum_profile_field_1_item_0_chunk_0_verifies_independently_on_l1"
  , "maximum_profile_field_2_item_1_chunk_0_verifies_independently_on_l1"
  , "maximum_profile_field_3_item_0_chunk_0_verifies_independently_on_l1"
  , "maximum_profile_field_4_item_0_chunk_0_verifies_independently_on_l1"
  , "maximum_profile_field_5_item_0_chunk_0_verifies_independently_on_l1"
  , "maximum_profile_field_6_item_0_chunk_0_verifies_independently_on_l1"
  , "maximum_profile_field_7_item_0_chunk_0_verifies_independently_on_l1"
  , "maximum_profile_field_8_item_15_chunk_0_verifies_independently_on_l1"
  , "maximum_profile_field_8_item_15_chunk_1_verifies_independently_on_l1"
  ]

publicationCase :: String -> TestTree
publicationCase caseName = testCase caseName $ do
  fixture <- loadFixture
  psucceeds $ runPublication (publicationContext fixture $ scenarioNamed caseName fixture)

loadFixture :: IO Fixture
loadFixture =
  either fail pure
    =<< eitherDecodeFileStrict' "tests/fixtures/proof-v1-fragment-envelope.json"

scenarioNamed :: String -> Fixture -> Scenario
scenarioNamed caseName fixture =
  case filter ((== caseName) . (.name)) fixture.scenarios of
    [scenario] -> scenario
    _ -> error $ "missing or duplicate fragment-envelope scenario: " <> caseName

runPublication :: forall s. ScriptContext -> Term s PUnit
runPublication ctx =
  txFieldReceiptMintValidator
    # pdata (pconstant fieldPreimageScriptHash)
    # pdata (pconstant receiptScriptHash)
    # pconstant ctx

publicationContext :: Fixture -> Scenario -> ScriptContext
publicationContext fixture scenario =
  case buildScriptContext (withMintingScript minted (dataToBuiltinData redeemer)) of
    ScriptContext txInfo _ scriptInfo ->
      ScriptContext
        txInfo
          { txInfoReferenceInputs = fieldInput : predecessorInputs
          , txInfoOutputs = [receiptOutput]
          }
        (Redeemer $ dataToBuiltinData redeemer)
        scriptInfo
  where
    proof = scenario.chunkProof
    fieldRef = publicationReference
    predecessorIndex = maybe (-1) (const 1) scenario.predecessor
    redeemer =
      PD.Constr
        0
        [ PD.I 0
        , PD.I predecessorIndex
        , PD.I 0
        , PD.B (hex fixture.transactionId)
        , sourceData fixture
        ]
    receiptName = fieldReceiptName fixture proof.fieldIndex proof.itemIndex proof.chunkIndex
    minted = singleton fieldReceiptPolicyId receiptName 1
    fieldInput =
      TxInInfo
        fieldRef
        ( TxOut
            fieldPreimageAddress
            (mkAdaValue 2_000_000)
            (inlineDatum $ fieldDatum fixture scenario)
            Nothing
        )
    predecessorInputs = maybe [] (pure . predecessorInput fixture) scenario.predecessor
    receiptOutput =
      TxOut
        receiptAddress
        (mkAdaValue 2_000_000 <> minted)
        (inlineDatum $ receiptDatum fixture scenario.collectionProof proof.chunkIndex fieldRef predecessorRef scenario.fieldEncodedSize)
        Nothing
    predecessorRef = predecessorReference <$ scenario.predecessor

withoutReferenceInputs :: ScriptContext -> ScriptContext
withoutReferenceInputs (ScriptContext txInfo redeemer scriptInfo) =
  ScriptContext txInfo {txInfoReferenceInputs = []} redeemer scriptInfo

terminalReceiptAuthenticates :: forall s. Fixture -> Term s PBool
terminalReceiptAuthenticates fixture =
  pverifyOrderReceipts
    # pconstant @(PBuiltinList (PAsData PTxInInfo)) [terminalReceiptInput fixture]
    # pconstant @(PAsData PScriptHash) receiptScriptHash
    # pconstant @(PAsData PCurrencySymbol) fieldReceiptPolicyId
    # pconstant @(PAsData PCurrencySymbol) txOrderPolicyId
    # pconstant @(PAsData PTxOutRef) orderId
    # pfromData
      ( punsafeCoerce @(PAsData PTxOrderPayloadV1) $
          pconstant @PData $
            PD.Constr
              0
              [ PD.B (hex fixture.transactionId)
              , PD.B (hex fixture.transactionCommitment)
              , sourceData fixture
              , PD.Constr 0 [toPD terminalReference]
              ]
      )

terminalReceiptInput :: Fixture -> TxInInfo
terminalReceiptInput fixture =
  TxInInfo
    terminalReference
    ( TxOut
        receiptAddress
        (mkAdaValue 2_000_000 <> singleton fieldReceiptPolicyId assetName 1)
        (inlineDatum datum)
        Nothing
    )
  where
    receipt = fixture.terminalReceipt
    proof = receipt.collectionProof
    assetName = fieldReceiptName fixture proof.fieldIndex proof.itemIndex receipt.chunkIndex
    datum =
      receiptDatum
        fixture
        proof
        receipt.chunkIndex
        publicationReference
        (Just predecessorReference)
        receipt.fieldEncodedSize

predecessorInput :: Fixture -> ReceiptFixture -> TxInInfo
predecessorInput fixture receipt =
  TxInInfo
    predecessorReference
    ( TxOut
        receiptAddress
        (mkAdaValue 2_000_000 <> singleton fieldReceiptPolicyId assetName 1)
        (inlineDatum datum)
        Nothing
    )
  where
    proof = receipt.collectionProof
    assetName = fieldReceiptName fixture proof.fieldIndex proof.itemIndex receipt.chunkIndex
    datum =
      receiptDatum
        fixture
        proof
        receipt.chunkIndex
        predecessorFieldReference
        Nothing
        receipt.fieldEncodedSize

fieldDatum :: Fixture -> Scenario -> PD.Data
fieldDatum fixture scenario =
  PD.Constr
    0
    [ PD.B fieldReceiptPolicyBytes
    , PD.B txOrderPolicyBytes
    , toPD orderId
    , PD.B (hex fixture.transactionCommitment)
    , collectionProofData scenario.collectionProof
    , chunkProofData scenario.chunkProof
    ]

receiptDatum :: Fixture -> CollectionProof -> Integer -> TxOutRef -> Maybe TxOutRef -> Integer -> PD.Data
receiptDatum fixture proof receiptChunkIndex fieldRef predecessorRef encodedSize =
  PD.Constr
    0
    [ PD.B fieldReceiptPolicyBytes
    , PD.B txOrderPolicyBytes
    , toPD orderId
    , PD.B (hex fixture.transactionCommitment)
    , collectionProofData proof
    , PD.I receiptChunkIndex
    , toPD fieldRef
    , maybe (PD.Constr 1 []) (PD.Constr 0 . pure . toPD) predecessorRef
    , PD.I encodedSize
    ]

sourceData :: Fixture -> PD.Data
sourceData fixture =
  PD.Constr
    0
    [ PD.B (hex fixture.compactCbor)
    , PD.B (hex fixture.witnessSetCompactCbor)
    , PD.B (hex fixture.fieldPreimageLengthsCbor)
    ]

collectionProofData :: CollectionProof -> PD.Data
collectionProofData proof =
  PD.Constr
    0
    [ PD.I proof.version
    , PD.I proof.fieldIndex
    , PD.I proof.itemCount
    , PD.I proof.itemIndex
    , PD.I proof.itemLength
    , PD.B (hex proof.itemCommitment)
    , PD.List (map peakData proof.frontier)
    , PD.List (map (PD.B . hex) proof.siblings)
    ]

chunkProofData :: ChunkProof -> PD.Data
chunkProofData proof =
  PD.Constr
    0
    [ PD.I proof.version
    , PD.I proof.fieldIndex
    , PD.I proof.itemIndex
    , PD.I proof.totalLength
    , PD.I proof.chunkIndex
    , PD.B (hex proof.chunk)
    , PD.List (map peakData proof.frontier)
    , PD.List (map (PD.B . hex) proof.siblings)
    ]

peakData :: Peak -> PD.Data
peakData peak = PD.Constr 0 [PD.I peak.height, PD.B (hex peak.hash)]

fieldReceiptName :: Fixture -> Integer -> Integer -> Integer -> TokenName
fieldReceiptName fixture field item receiptChunk =
  TokenName . toBuiltin . blake2b256 $
    BS.concat
      [ "MidgardTxFieldReceiptV1"
      , txOrderPolicyBytes
      , orderTransactionIdBytes
      , bigEndian 8 65_535
      , hex fixture.transactionCommitment
      , BS.singleton (fromIntegral field)
      , bigEndian 8 item
      , bigEndian 8 receiptChunk
      ]

bigEndian :: Int -> Integer -> BS.ByteString
bigEndian width value =
  BS.pack
    [ fromIntegral ((value `shiftR` (8 * index)) .&. 0xff)
    | index <- [width - 1, width - 2 .. 0]
    ]

inlineDatum :: PD.Data -> OutputDatum
inlineDatum = OutputDatum . Datum . dataToBuiltinData

toPD :: ToData a => a -> PD.Data
toPD = builtinDataToData . toBuiltinData

blake2b256 :: BS.ByteString -> BS.ByteString
blake2b256 = fromBuiltin . Builtins.blake2b_256 . toBuiltin

hex :: String -> BS.ByteString
hex = Base16.decodeLenient . BSC.pack

fieldReceiptPolicyBytes, txOrderPolicyBytes, orderTransactionIdBytes :: BS.ByteString
fieldReceiptPolicyBytes = BS.replicate 28 0x10
txOrderPolicyBytes = BS.replicate 28 0x20
orderTransactionIdBytes = BS.replicate 32 0x50

fieldReceiptPolicyId, txOrderPolicyId :: CurrencySymbol
fieldReceiptPolicyId = CurrencySymbol (toBuiltin fieldReceiptPolicyBytes)
txOrderPolicyId = CurrencySymbol (toBuiltin txOrderPolicyBytes)

fieldPreimageScriptHash, receiptScriptHash :: ScriptHash
fieldPreimageScriptHash = ScriptHash (toBuiltin $ BS.replicate 28 0x30)
receiptScriptHash = ScriptHash (toBuiltin $ BS.replicate 28 0x40)

fieldPreimageAddress, receiptAddress :: Address
fieldPreimageAddress = scriptHashAddress fieldPreimageScriptHash
receiptAddress = scriptHashAddress receiptScriptHash

orderId, publicationReference, predecessorReference, predecessorFieldReference, terminalReference :: TxOutRef
orderId = TxOutRef (TxId $ toBuiltin orderTransactionIdBytes) 65_535
publicationReference = TxOutRef (TxId $ toBuiltin $ BS.replicate 32 0x60) 0
predecessorReference = TxOutRef (TxId $ toBuiltin $ BS.replicate 32 0x70) 0
terminalReference = TxOutRef (TxId $ toBuiltin $ BS.replicate 32 0x70) 1
predecessorFieldReference = TxOutRef (TxId $ toBuiltin $ BS.replicate 32 0x71) 0
