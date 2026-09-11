{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Testing.TxOrderV1Abi
Description : Exact wire vectors from
              @lib/midgard/user-events/tx-order-v1.test.ak@.

This module covers the public transaction-order ABI and its content-addressed
material carriage.
-}
module Testing.TxOrderV1Abi (tests) where

import Data.ByteString qualified as BS
import Data.ByteString.Base16 qualified as Base16
import Plutarch.Core.Utils (pand'List)
import Plutarch.LedgerApi.V3 (PAddress, POutputDatum (..), PScriptHash, PTxOutRef)
import Plutarch.Prelude
import Plutarch.Unsafe (punsafeCoerce)
import PlutusLedgerApi.V1.Value (TokenName (..))
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
  PNativeTxProofSourceV1 (..),
  PTxOrderEventV1 (..),
  PTxOrderPayloadV1 (..),
 )
import Midgard.MpfProof.Types (PProof (..))
import Midgard.RejectionReason (
  POperatorVerdictV1 (..),
  PRejectionReasonV1 (..),
 )
import Midgard.TransitionTrace (
  PRootDomain (..),
  PRootMembershipProof (..),
 )
import Midgard.NativeTxFieldAccess (PFieldCarriageV1 (..))
import Midgard.UserEvents qualified as UserEvents
import Midgard.UserEvents.TxOrder (
  PMintRedeemer (..),
  PSpendRedeemer (..),
  PTxOrderDatum (..),
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
    , testCase "tx_order_mint_redeemer_wire_form_is_the_event_plus_the_carriage_vector" $
        passertEval mintRedeemerVectors
    , testCase "tx_order_payload_rejects_an_unknown_outer_constructor" $
        pfails $ strictPayloadHasExpectedTxId payloadUnknownConstructorCbor
    , testCase "tx_order_payload_rejects_an_extra_field" $
        pfails $ strictPayloadHasExpectedTxId payloadExtraFieldCbor
    , testCase "cek_program_material_v1_matches_the_typed_blob_chunk_hash_vector" $
        passertEval cekProgramMaterialVector
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
          (pdata plutusFailureVerdict)
   in pserialiseData # key #== pconstant vectorOrderIdCbor
        #&& pserialiseData # value #== pconstant vectorForcedInclusionCbor

mintRedeemerVectors :: forall s. Term s PBool
mintRedeemerVectors =
  serialisesTo
    (pcon $ PMintRedeemer (pdata authenticateEvent) (pdata pnil))
    authenticateMintRedeemerCbor
    #&& serialisesTo
      (pcon $ PMintRedeemer (pdata burnEvent) (pdata pnil))
      burnMintRedeemerCbor
    #&& serialisesTo
      ( pcon $
          PMintRedeemer
            (pdata authenticateEvent)
            ( pdata $
                pcons # pdata (pcon $ PInline $ pdata $ pconstant "\x80")
                  #$ pcons # pdata (pcon $ PRawUtxo $ pdata 5)
                  #$ pcons
                    # pdata
                      ( pcon $
                          PCertified
                            (pdata 6)
                            (pdata $ pcons # pdata 7 #$ pcons # pdata 8 # pnil)
                      )
                    # pnil
            )
      )
      carriedMintRedeemerCbor

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

strictPayloadHasExpectedTxId :: forall s. BS.ByteString -> Term s PBool
strictPayloadHasExpectedTxId source =
  withDecoded source $ \raw ->
    pmatch (pasConstr # raw) $ \(PBuiltinPair tag fields) ->
      pif
        (tag #== 0 #&& plength # fields #== 3)
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
      , pforcedTx'verdict = pdata plutusFailureVerdict
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
      , ptxOrderSpend'validityOverride = pdata plutusFailureVerdict
      }

plutusFailureVerdict :: forall s. Term s POperatorVerdictV1
plutusFailureVerdict =
  pcon $ PForcedTxInvalid $ pforgetData $ pdata $ pcon $ PPlutusExecutionFailed $ pdata 0

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

authenticateEvent :: forall s. Term s UserEvents.PMintRedeemer
authenticateEvent =
  pcon $
    UserEvents.PAuthenticateEvent
      { UserEvents.pauthenticate'nonceInputIndex = pdata 0
      , UserEvents.pauthenticate'eventOutputIndex = pdata 1
      , UserEvents.pauthenticate'hubRefInputIndex = pdata 2
      , UserEvents.pauthenticate'witnessRegistrationRedeemerIndex = pdata 3
      }

burnEvent :: forall s. Term s UserEvents.PMintRedeemer
burnEvent =
  pcon $
    UserEvents.PBurnEventNFT
      { UserEvents.pburnEvent'nonceAssetName = pdata (pconstant $ TokenName "\xaa\xbb")
      , UserEvents.pburnEvent'witnessUnregistrationRedeemerIndex = pdata 4
      }

vectorOrderIdHost :: TxOutRef
vectorOrderIdHost = TxOutRef (TxId $ toBuiltin $ BS.replicate 32 0x33) 4

refundAddress :: Address
refundAddress =
  Address
    (PubKeyCredential $ PubKeyHash $ toBuiltin $ BS.replicate 28 0x77)
    Nothing

witnessScriptHash :: ScriptHash
witnessScriptHash = ScriptHash $ toBuiltin $ BS.replicate 28 0x66

orderTransactionId, fieldTransactionId, zeroHash, phasRoot :: BS.ByteString
orderTransactionId = BS.replicate 32 0x44
fieldTransactionId = BS.replicate 32 0x55
zeroHash = BS.replicate 32 0x00
phasRoot = BS.replicate 32 0x11

cekMaterialRoot :: BS.ByteString
cekMaterialRoot = hex "941de596141f044be570fb7b579b3fc520db7cacdbccd020cdb7618ba124380c"

vectorOrderIdCbor, vectorPayloadCbor, vectorEventCbor, vectorDatumCbor :: BS.ByteString
vectorOrderIdCbor = hex "d8799f5820333333333333333333333333333333333333333333333333333333333333333304ff"
vectorPayloadCbor = hex "d8799f5820444444444444444444444444444444444444444444444444444444444444444458205555555555555555555555555555555555555555555555555555555555555555d8799f41014202034104ffff"
vectorEventCbor = hex "d8799fd8799f5820333333333333333333333333333333333333333333333333333333333333333304ffd8799f5820444444444444444444444444444444444444444444444444444444444444444458205555555555555555555555555555555555555555555555555555555555555555d8799f41014202034104ffffff"
vectorDatumCbor = hex "d8799fd8799fd8799f5820333333333333333333333333333333333333333333333333333333333333333304ffd8799f5820444444444444444444444444444444444444444444444444444444444444444458205555555555555555555555555555555555555555555555555555555555555555d8799f41014202034104ffffff187b581c66666666666666666666666666666666666666666666666666666666d8799fd8799f581c77777777777777777777777777777777777777777777777777777777ffd87a80ffd87980ff"

vectorForcedInclusionCbor, vectorSpendRedeemerCbor :: BS.ByteString
vectorForcedInclusionCbor = hex "d8799f58204444444444444444444444444444444444444444444444444444444444444444d8799f41014202034104ffd87a9fd905229f00ffffff"
vectorSpendRedeemerCbor = hex "d8799f0001020304d8799fd87a805820000000000000000000000000000000000000000000000000000000000000000058201111111111111111111111111111111111111111111111111111111111111111015827d8799f5820333333333333333333333333333333333333333333333333333333333333333304ff583bd8799f58204444444444444444444444444444444444444444444444444444444444444444d8799f41014202034104ffd87a9fd905229f00ffffff80ff05d87a9fd905229f00ffffff"

authenticateMintRedeemerCbor, burnMintRedeemerCbor, carriedMintRedeemerCbor :: BS.ByteString
authenticateMintRedeemerCbor = hex "d8799fd8799f00010203ff80ff"
burnMintRedeemerCbor = hex "d8799fd87a9f42aabb04ff80ff"
carriedMintRedeemerCbor = hex "d8799fd8799f00010203ff9fd8799f4180ffd87a9f05ffd87b9f069f0708ffffffff"

payloadUnknownConstructorCbor, payloadExtraFieldCbor, cekMaterialDatumCbor :: BS.ByteString
payloadUnknownConstructorCbor = hex "d87a9f5820444444444444444444444444444444444444444444444444444444444444444458205555555555555555555555555555555555555555555555555555555555555555d8799f41014202034104ffff"
payloadExtraFieldCbor = hex "d8799f5820444444444444444444444444444444444444444444444444444444444444444458205555555555555555555555555555555555555555555555555555555555555555d8799f41014202034104ff00ff"
cekMaterialDatumCbor = hex "d8799f035820941de596141f044be570fb7b579b3fc520db7cacdbccd020cdb7618ba124380c49486d6174657269616cff"

hex :: BS.ByteString -> BS.ByteString
hex = Base16.decodeLenient
