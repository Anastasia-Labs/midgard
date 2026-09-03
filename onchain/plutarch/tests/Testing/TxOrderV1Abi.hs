{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Testing.TxOrderV1Abi
Description : Exact wire vectors from
              @lib/midgard/user-events/tx-order-v1.test.ak@.

This module stops at the public transaction-order ABI and its content-addressed
material. Receipt-chain behaviour is covered separately by "Testing.TxOrderFields".
-}
module Testing.TxOrderV1Abi (tests) where

import Data.ByteString qualified as BS
import Data.ByteString.Base16 qualified as Base16
import Plutarch.Core.Utils (pand'List)
import Plutarch.LedgerApi.Utils (PMaybeData (..))
import Plutarch.LedgerApi.V3 (PAddress, POutputDatum (..), PScriptHash, PTxOutRef)
import Plutarch.Prelude
import Plutarch.Unsafe (punsafeCoerce)
import PlutusLedgerApi.V1.Value (CurrencySymbol (..))
import PlutusLedgerApi.V3 (
  Address (..),
  Credential (..),
  PubKeyHash (..),
  ScriptHash (..),
  TxId (..),
  TxOutRef (..),
 )
import PlutusTx.Builtins (toBuiltin)
import Test.Tasty
import Test.Tasty.HUnit

import Aiken.Cbor (pdeserialise)
import Midgard.CekProof (phashBlobChunkV1)
import Midgard.LedgerState (
  PCekProgramMaterialDatumV1 (..),
  PForcedInclusionTxV1 (..),
  PMidgardTxValidity (..),
  PNativeTxProofSourceV1 (..),
  PTxOrderEventV1 (..),
  PTxOrderPayloadV1 (..),
 )
import Midgard.MpfProof.Types (PProof (..))
import Midgard.TransitionTrace (
  PRootDomain (..),
  PRootMembershipProof (..),
 )
import Midgard.UserEvents.TxFieldReceipt (PMintRedeemer (..))
import Midgard.UserEvents.TxOrder (
  PSpendRedeemer (..),
  PTxOrderDatum (..),
  pfieldReceiptAssetName,
  pforcedInclusionKeyValue,
 )
import Testing.Eval (passertEval, pfails)

tests :: TestTree
tests =
  testGroup
    "Tx Order V1 ABI Aiken Parity"
    [ testCase "tx_order_v1_matches_the_canonical_typescript_abi_vectors" $
        passertEval canonicalAbiVectors
    , testCase "forced_inclusion_key_value_matches_the_canonical_v1_vectors" $
        passertEval forcedInclusionKeyValueVectors
    , testCase "tx_field_receipt_redeemers_match_the_canonical_typescript_abi_vectors" $
        passertEval fieldReceiptRedeemerVectors
    , testCase "tx_order_payload_rejects_an_unknown_outer_constructor" $
        pfails $ strictPayloadHasExpectedTxId payloadUnknownConstructorCbor
    , testCase "tx_order_payload_rejects_an_extra_field" $
        pfails $ strictPayloadHasExpectedTxId payloadExtraFieldCbor
    , testCase "cek_program_material_v1_matches_the_typed_blob_chunk_hash_vector" $
        passertEval cekProgramMaterialVector
    , testCase "field_receipt_asset_name_matches_core_vector" $
        passertEval fieldReceiptAssetNameVector
    ]

canonicalAbiVectors :: forall s. Term s PBool
canonicalAbiVectors =
  pand'List
    [ serialisesTo vectorOrderId vectorOrderIdCbor
    , serialisesTo vectorPayload vectorPayloadCbor
    , serialisesTo vectorEvent vectorEventCbor
    , serialisesTo vectorDatum vectorDatumCbor
    , serialisesTo vectorForcedInclusion vectorForcedInclusionCbor
    , serialisesTo vectorSpendRedeemer vectorSpendRedeemerCbor
    ]

forcedInclusionKeyValueVectors :: forall s. Term s PBool
forcedInclusionKeyValueVectors =
  let (key, value) =
        pforcedInclusionKeyValue
          (pforgetData $ pdata vectorEvent)
          (pdata $ pcon PFailedScript)
   in pserialiseData # key #== pconstant vectorOrderIdCbor
        #&& pserialiseData # value #== pconstant vectorForcedInclusionCbor

fieldReceiptRedeemerVectors :: forall s. Term s PBool
fieldReceiptRedeemerVectors =
  serialisesTo publishFieldRedeemer publishFieldRedeemerCbor
    #&& serialisesTo burnReceiptsRedeemer burnReceiptsRedeemerCbor

cekProgramMaterialVector :: forall s. Term s PBool
cekProgramMaterialVector =
  plet (phashBlobChunkV1 # pconstant "material") $ \root ->
    root #== pconstant cekMaterialRoot
      #&& serialisesTo
        ( pcon $
            PCekProgramMaterialDatumV1
              { pcekProgramMaterial'kind = pdata 3
              , pcekProgramMaterial'root = pdata root
              , pcekProgramMaterial'preimage = pdata (pconstant "\x48material")
              }
        )
        cekMaterialDatumCbor

fieldReceiptAssetNameVector :: forall s. Term s PBool
fieldReceiptAssetNameVector =
  pto
    ( pfieldReceiptAssetName
        # pdata (pconstant txOrderPolicyId)
        # pdata (pconstant orderId)
        # pconstant zeroHash
        # 0
        # 0
        # 0
    )
    #== pconstant fieldReceiptAssetName

strictPayloadHasExpectedTxId :: forall s. BS.ByteString -> Term s PBool
strictPayloadHasExpectedTxId source =
  withDecoded source $ \raw ->
    pmatch (pasConstr # raw) $ \(PBuiltinPair tag fields) ->
      pif
        (tag #== 0 #&& plength # fields #== 4)
        ( plet
            (pfromData $ punsafeCoerce @(PAsData PTxOrderPayloadV1) raw)
            $ \payload ->
              pmatch payload $ \p@PTxOrderPayloadV1 {ptxOrderPayload'txId} ->
                pif
                  (pforgetData (pdata $ pcon p) #== raw)
                  (pfromData ptxOrderPayload'txId #== pconstant orderTransactionId)
                  perror
        )
        perror

withDecoded :: forall s. BS.ByteString -> (Term s PData -> Term s PBool) -> Term s PBool
withDecoded source continuation =
  pmatch (pdeserialise # pconstant source) $ \case
    PNothing -> perror
    PJust value -> continuation value

serialisesTo :: forall a s. PIsData a => Term s a -> BS.ByteString -> Term s PBool
serialisesTo value expected =
  pserialiseData # pforgetData (pdata value) #== pconstant expected

vectorSource :: forall s. Term s PNativeTxProofSourceV1
vectorSource =
  pcon $
    PNativeTxProofSourceV1
      { pnativeSource'compactCbor = pdata (pconstant "\x01")
      , pnativeSource'witnessSetCompactCbor = pdata (pconstant "\x02\x03")
      , pnativeSource'fieldPreimageLengthsCbor = pdata (pconstant "\x04")
      }

vectorOrderId :: forall s. Term s PTxOutRef
vectorOrderId = pconstant vectorOrderIdHost

vectorPayload :: forall s. Term s PTxOrderPayloadV1
vectorPayload =
  pcon $
    PTxOrderPayloadV1
      { ptxOrderPayload'txId = pdata (pconstant orderTransactionId)
      , ptxOrderPayload'transactionCommitment = pdata (pconstant fieldTransactionId)
      , ptxOrderPayload'source = pdata vectorSource
      , ptxOrderPayload'terminalReceiptReference =
          pforgetData (pdata (pcon PDNothing :: Term s (PMaybeData PTxOutRef)))
      }

vectorEvent :: forall s. Term s PTxOrderEventV1
vectorEvent =
  pcon $
    PTxOrderEventV1
      { ptxOrderEvent'id = pdata vectorOrderId
      , ptxOrderEvent'tx = pdata vectorPayload
      }

vectorDatum :: forall s. Term s PTxOrderDatum
vectorDatum =
  pcon $
    PTxOrderDatum
      { ptxOrderDatum'event = pdata vectorEvent
      , ptxOrderDatum'inclusionTime = pdata 123
      , ptxOrderDatum'witness = pdata (pconstant witnessScriptHash :: Term s PScriptHash)
      , ptxOrderDatum'refundAddress = pdata (pconstant refundAddress :: Term s PAddress)
      , ptxOrderDatum'refundDatum = pcon PNoOutputDatum
      }

vectorForcedInclusion :: forall s. Term s PForcedInclusionTxV1
vectorForcedInclusion =
  pcon $
    PForcedInclusionTxV1
      { pforcedTx'txId = pdata (pconstant orderTransactionId)
      , pforcedTx'source = pdata vectorSource
      , pforcedTx'operatorValidity = pdata (pcon PFailedScript)
      }

vectorSpendRedeemer :: forall s. Term s PSpendRedeemer
vectorSpendRedeemer =
  pcon $
    PSpendRedeemer
      { ptxOrderSpend'inputIndex = pdata 0
      , ptxOrderSpend'outputIndex = pdata 1
      , ptxOrderSpend'hubRefInputIndex = pdata 2
      , ptxOrderSpend'settlementRefInputIndex = pdata 3
      , ptxOrderSpend'burnRedeemerIndex = pdata 4
      , ptxOrderSpend'membershipProof = pdata vectorMembershipProof
      , ptxOrderSpend'inclusionProofScriptWithdrawRedeemerIndex = pdata 5
      , ptxOrderSpend'validityOverride = pdata (pcon PFailedScript)
      }

vectorMembershipProof :: forall s. Term s PRootMembershipProof
vectorMembershipProof =
  pcon $
    PRootMembershipProof
      { prootMembership'domain = pdata (pcon PForcedTransactionsV1RootDomain)
      , prootMembership'root = pdata (pconstant zeroHash)
      , prootMembership'phasRoot = pdata (pconstant phasRoot)
      , prootMembership'count = pdata 1
      , prootMembership'key = pforgetData (pdata $ pconstant @PByteString vectorOrderIdCbor)
      , prootMembership'value = pforgetData (pdata $ pconstant @PByteString vectorForcedInclusionCbor)
      , prootMembership'proof = pdata (pcon $ PProof pnil)
      }

publishFieldRedeemer :: forall s. Term s PMintRedeemer
publishFieldRedeemer =
  pcon $
    PPublishField
      { ppublishField'fieldReferenceInputIndex = pdata 0
      , ppublishField'predecessorReceiptReferenceInputIndex = pdata (-1)
      , ppublishField'receiptOutputIndex = pdata 1
      , ppublishField'transactionId = pdata (pconstant orderTransactionId)
      , ppublishField'source = pdata vectorSource
      }

burnReceiptsRedeemer :: forall s. Term s PMintRedeemer
burnReceiptsRedeemer =
  pcon $
    PBurnReceipts
      { pburnReceipts'receiptInputIndices =
          pdata $ pcons # pdata 0 #$ pcons # pdata 2 # pnil
      }

vectorOrderIdHost :: TxOutRef
vectorOrderIdHost = TxOutRef (TxId $ toBuiltin $ BS.replicate 32 0x33) 4

orderId :: TxOutRef
orderId = TxOutRef (TxId $ toBuiltin orderTransactionId) 7

refundAddress :: Address
refundAddress =
  Address
    (PubKeyCredential $ PubKeyHash $ toBuiltin $ BS.replicate 28 0x77)
    Nothing

witnessScriptHash :: ScriptHash
witnessScriptHash = ScriptHash $ toBuiltin $ BS.replicate 28 0x66

txOrderPolicyId :: CurrencySymbol
txOrderPolicyId = CurrencySymbol $ toBuiltin $ BS.replicate 28 0x11

orderTransactionId, fieldTransactionId, zeroHash, phasRoot :: BS.ByteString
orderTransactionId = BS.replicate 32 0x44
fieldTransactionId = BS.replicate 32 0x55
zeroHash = BS.replicate 32 0x00
phasRoot = BS.replicate 32 0x11

cekMaterialRoot, fieldReceiptAssetName :: BS.ByteString
cekMaterialRoot = hex "941de596141f044be570fb7b579b3fc520db7cacdbccd020cdb7618ba124380c"
fieldReceiptAssetName = hex "7bc0ae911756007cc44a1e956fedc08d67af38ce19778405ab040faa77b60123"

vectorOrderIdCbor, vectorPayloadCbor, vectorEventCbor, vectorDatumCbor :: BS.ByteString
vectorOrderIdCbor = hex "d8799f5820333333333333333333333333333333333333333333333333333333333333333304ff"
vectorPayloadCbor = hex "d8799f5820444444444444444444444444444444444444444444444444444444444444444458205555555555555555555555555555555555555555555555555555555555555555d8799f41014202034104ffd87a80ff"
vectorEventCbor = hex "d8799fd8799f5820333333333333333333333333333333333333333333333333333333333333333304ffd8799f5820444444444444444444444444444444444444444444444444444444444444444458205555555555555555555555555555555555555555555555555555555555555555d8799f41014202034104ffd87a80ffff"
vectorDatumCbor = hex "d8799fd8799fd8799f5820333333333333333333333333333333333333333333333333333333333333333304ffd8799f5820444444444444444444444444444444444444444444444444444444444444444458205555555555555555555555555555555555555555555555555555555555555555d8799f41014202034104ffd87a80ffff187b581c66666666666666666666666666666666666666666666666666666666d8799fd8799f581c77777777777777777777777777777777777777777777777777777777ffd87a80ffd87980ff"

vectorForcedInclusionCbor, vectorSpendRedeemerCbor :: BS.ByteString
vectorForcedInclusionCbor = hex "d8799f58204444444444444444444444444444444444444444444444444444444444444444d8799f41014202034104ffd87c80ff"
vectorSpendRedeemerCbor = hex "d8799f0001020304d8799fd87a805820000000000000000000000000000000000000000000000000000000000000000058201111111111111111111111111111111111111111111111111111111111111111015827d8799f5820333333333333333333333333333333333333333333333333333333333333333304ff5834d8799f58204444444444444444444444444444444444444444444444444444444444444444d8799f41014202034104ffd87c80ff80ff05d87c80ff"

publishFieldRedeemerCbor, burnReceiptsRedeemerCbor :: BS.ByteString
publishFieldRedeemerCbor = hex "d8799f00200158204444444444444444444444444444444444444444444444444444444444444444d8799f41014202034104ffff"
burnReceiptsRedeemerCbor = hex "d87a9f9f0002ffff"

payloadUnknownConstructorCbor, payloadExtraFieldCbor, cekMaterialDatumCbor :: BS.ByteString
payloadUnknownConstructorCbor = hex "d87a9f5820444444444444444444444444444444444444444444444444444444444444444458205555555555555555555555555555555555555555555555555555555555555555d8799f41014202034104ffd87a80ff"
payloadExtraFieldCbor = hex "d8799f5820444444444444444444444444444444444444444444444444444444444444444458205555555555555555555555555555555555555555555555555555555555555555d8799f41014202034104ffd87a8000ff"
cekMaterialDatumCbor = hex "d8799f035820941de596141f044be570fb7b579b3fc520db7cacdbccd020cdb7618ba124380c49486d6174657269616cff"

hex :: BS.ByteString -> BS.ByteString
hex = Base16.decodeLenient
