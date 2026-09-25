{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

module Testing.TxOrderMaterial (tests) where

import Data.ByteString qualified as BS
import Plutarch.Builtin.Crypto (pblake2b_256)
import Plutarch.LedgerApi.V3 (PTxInInfo)
import Plutarch.Prelude
import PlutusLedgerApi.V1.Value (CurrencySymbol (..), TokenName (..))
import PlutusTx.Builtins (toBuiltin)
import Test.Tasty
import Test.Tasty.HUnit

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
import Midgard.LedgerState (PNativeTxProofSourceV1 (..), PTxOrderPayloadV1 (..))
import Midgard.NativeTxFieldAccess (PFieldCarriageV1 (..), pemptyFieldCommitment)
import Midgard.UserEvents qualified as UserEvents
import Midgard.UserEvents.TxOrder (
  pmaterialCarriageMatchesEvent,
  pverifyOrderMaterial,
 )
import Testing.Eval (passertEval, pfails)

tests :: TestTree
tests =
  testGroup
    "Tx Order Material Carriage Tests"
    [ testCase "empty_material_activates_the_order" $
        passertEval $ verify emptyBody emptyLengths []
    , testCase "empty_material_refuses_a_spare_carriage_entry" $
        passertEval $ pnot #$ verify emptyBody emptyLengths [inline emptyPreimage]
    , testCase "material_bearing_order_fails_closed_without_carriage" $
        pfails $ verify oneFieldBody oneFieldLengths []
    , testCase "inline_carriage_authenticates_the_non_empty_field" $
        passertEval $ verify oneFieldBody oneFieldLengths [inline spendPreimage]
    , testCase "inline_carriage_refuses_bytes_the_field_does_not_commit" $
        pfails $ verify oneFieldBody oneFieldLengths [inline signerPreimage]
    , testCase "two fields consume carriage in ascending positional order" $
        passertEval $
          verify twoFieldBody twoFieldLengths [inline spendPreimage, inline signerPreimage]
    , testCase "two fields refuse a transposed carriage vector" $
        pfails $
          verify twoFieldBody twoFieldLengths [inline signerPreimage, inline spendPreimage]
    , testCase "declared_field_length_disagreeing_with_material_is_refused" $
        pfails $ verify oneFieldBody wrongOneFieldLengths [inline spendPreimage]
    , testCase "burn_carries_no_material_carriage_vector" $
        passertEval $ pmaterialCarriageMatchesEvent # burnEvent # carriages []
    , testCase "burn_carrying_material_is_refused" $
        passertEval $
          pnot #$ pmaterialCarriageMatchesEvent # burnEvent # carriages [inline emptyPreimage]
    , testCase "authenticate_event_leaves_vector_consumption_to_the_material_walk" $
        passertEval $
          pmaterialCarriageMatchesEvent # authenticateEvent # carriages []
            #&& pmaterialCarriageMatchesEvent
              # authenticateEvent
              # carriages [inline spendPreimage]
    ]

verify ::
  forall s.
  Term s PNativeTxBodyCompact ->
  Term s PNativeTxFieldPreimageLengthsV1 ->
  [Term s PFieldCarriageV1] ->
  Term s PBool
verify body lengths material =
  pverifyOrderMaterial
    # payload body lengths
    # carriages material
    # (pnil :: Term s (PBuiltinList (PAsData PTxInInfo)))
    # pdata (pconstant certificatePolicy)

payload ::
  forall s.
  Term s PNativeTxBodyCompact ->
  Term s PNativeTxFieldPreimageLengthsV1 ->
  Term s PTxOrderPayloadV1
payload body lengths =
  pcon
    PTxOrderPayloadV1
      { ptxOrderPayload'txId = pdata txId
      , ptxOrderPayload'transactionCommitment = pdata commitment
      , ptxOrderPayload'source =
          pdata $
            pcon
              PNativeTxProofSourceV1
                { pnativeSource'compactCbor = pdata compactCbor
                , pnativeSource'witnessSetCompactCbor = pdata witnessSetCbor
                , pnativeSource'fieldPreimageLengthsCbor = pdata lengthsCbor
                }
      }
  where
    bodyCbor = pencodeNativeTxBodyCompact # body
    txId = pnativeTxIdForVersion # 1 # bodyCbor
    witnessSetCbor = pencodeNativeTxWitnessSetCompact # emptyWitnessSet
    compactCbor =
      pencodeNativeTxCompactV1
        # pcon
          PNativeTxCompact
            { pcompact'body = body
            , pcompact'witnessSetHash = pblake2b_256 # witnessSetCbor
            , pcompact'validityCode = 0
            }
    lengthsCbor = pencodeNativeTxFieldPreimageLengthsV1 # lengths
    commitment = pnativeTxProofCommitmentV1 # compactCbor # witnessSetCbor # lengthsCbor

inline :: forall s. BS.ByteString -> Term s PFieldCarriageV1
inline bytes = pcon (PInline (pdata (pconstant bytes)))

carriages :: forall s. [Term s PFieldCarriageV1] -> Term s (PBuiltinList (PAsData PFieldCarriageV1))
carriages = foldr (\carriage rest -> pcons # pdata carriage # rest) pnil

burnEvent :: forall s. Term s UserEvents.PMintRedeemer
burnEvent =
  pcon
    UserEvents.PBurnEventNFT
      { UserEvents.pburnEvent'nonceAssetName = pdata (pconstant nonceName)
      , UserEvents.pburnEvent'witnessUnregistrationRedeemerIndex = pdata 0
      }

authenticateEvent :: forall s. Term s UserEvents.PMintRedeemer
authenticateEvent =
  pcon
    UserEvents.PAuthenticateEvent
      { UserEvents.pauthenticate'nonceInputIndex = pdata 0
      , UserEvents.pauthenticate'eventOutputIndex = pdata 0
      , UserEvents.pauthenticate'hubRefInputIndex = pdata 0
      , UserEvents.pauthenticate'witnessRegistrationRedeemerIndex = pdata 0
      }

emptyBody, oneFieldBody, twoFieldBody :: forall s. Term s PNativeTxBodyCompact
emptyBody = body pemptyFieldCommitment pemptyFieldCommitment
oneFieldBody = body (pblake2b_256 # pconstant spendPreimage) pemptyFieldCommitment
twoFieldBody =
  body
    (pblake2b_256 # pconstant spendPreimage)
    (pblake2b_256 # pconstant signerPreimage)

body :: forall s. Term s PByteString -> Term s PByteString -> Term s PNativeTxBodyCompact
body spendHash signerHash =
  pcon
    PNativeTxBodyCompact
      { pbodyCompact'spendInputsHash = spendHash
      , pbodyCompact'referenceInputsHash = pemptyFieldCommitment
      , pbodyCompact'outputsHash = pemptyFieldCommitment
      , pbodyCompact'fee = 0
      , pbodyCompact'validityIntervalStart = 0
      , pbodyCompact'validityIntervalEnd = 0
      , pbodyCompact'requiredObserversHash = pemptyFieldCommitment
      , pbodyCompact'requiredSignersHash = signerHash
      , pbodyCompact'mintHash = pemptyFieldCommitment
      , pbodyCompact'scriptIntegrityHash = pconstant (BS.replicate 32 0x11)
      , pbodyCompact'auxiliaryDataHash = pconstant (BS.replicate 32 0x22)
      , pbodyCompact'networkId = 0
      }

emptyLengths, oneFieldLengths, twoFieldLengths, wrongOneFieldLengths ::
  forall s. Term s PNativeTxFieldPreimageLengthsV1
emptyLengths = lengths 1 1
oneFieldLengths = lengths (fromIntegral $ BS.length spendPreimage) 1
twoFieldLengths =
  lengths
    (fromIntegral $ BS.length spendPreimage)
    (fromIntegral $ BS.length signerPreimage)
wrongOneFieldLengths = lengths (fromIntegral $ BS.length spendPreimage + 1) 1

lengths :: forall s. Integer -> Integer -> Term s PNativeTxFieldPreimageLengthsV1
lengths spend signers =
  pcon
    PNativeTxFieldPreimageLengthsV1
      { plengths'spendInputs = pconstant spend
      , plengths'referenceInputs = 1
      , plengths'outputs = 1
      , plengths'requiredObservers = 1
      , plengths'requiredSigners = pconstant signers
      , plengths'mint = 1
      , plengths'addressWitnesses = 1
      , plengths'scriptWitnesses = 1
      , plengths'redeemers = 1
      }

emptyWitnessSet :: forall s. Term s PNativeTxWitnessSetCompact
emptyWitnessSet =
  pcon
    PNativeTxWitnessSetCompact
      { pwitnessSetCompact'addrTxWitsHash = pdata pemptyFieldCommitment
      , pwitnessSetCompact'scriptTxWitsHash = pdata pemptyFieldCommitment
      , pwitnessSetCompact'redeemerTxWitsHash = pdata pemptyFieldCommitment
      }

emptyPreimage, spendPreimage, signerPreimage :: BS.ByteString
emptyPreimage = "\x80"
spendPreimage = "\x81\x58\x26\x82\x58\x20" <> BS.replicate 32 0x44 <> "\x19\x00\x00"
signerPreimage = "\x81\x58\x1c" <> BS.replicate 28 0x66

certificatePolicy :: CurrencySymbol
certificatePolicy = CurrencySymbol (toBuiltin (BS.replicate 28 0x91))

nonceName :: TokenName
nonceName = TokenName (toBuiltin (BS.replicate 32 0x33))
