{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Testing.ValidationMachineFieldDoor
Description : Focused tests for the validation-machine whole-field door.
-}
module Testing.ValidationMachineFieldDoor (tests) where

import Data.ByteString qualified as BS
import Plutarch.Core.Utils (pand'List)
import Plutarch.Prelude
import PlutusLedgerApi.V1.Value (CurrencySymbol (..))
import PlutusTx.Builtins qualified as Builtins
import PlutusTx.Builtins (fromBuiltin, toBuiltin)
import Test.Tasty
import Test.Tasty.HUnit

import Midgard.BoundedItem qualified as BoundedItem
import Midgard.FraudProofs.NativeTx.Types (
  PNativeTxBodyCompact (..),
  PNativeTxCompact (..),
  PNativeTxWitnessSetCompact (..),
  PVerifiedMidgardNativeTxCompact (..),
 )
import Midgard.NativeTxFieldAccess (PFieldCarriageV1 (..))
import Midgard.ValidationMachineFieldDoor (
  PMachineFieldDoorV1 (..),
  PMachineFieldItemV1,
  pmachineFieldCount,
  pmachineFieldItemBytes,
  pmachineFieldItemBytesMatch,
  pmachineFieldItemChunk,
  pmachineFieldItemChunkCount,
  pmachineFieldItemCommitment,
  pmachineFieldItemCount,
  pmachineFieldItemLength,
  pmachineFieldNextItemOffset,
  popenMachineFieldItem,
  popenMachineFieldItemAt,
 )
import Testing.Eval (passertEval, pfails)

tests :: TestTree
tests =
  testGroup
    "Validation Machine Field Door Tests"
    [ testCase "open_machine_field_item derives the variable-width extent" $
        passertEval $
          pand'List
            [ pmachineFieldItemLength # item0 #== 5_000
            , pmachineFieldItemBytes # item0 #== pconstant largeItem
            , pmachineFieldNextItemOffset # item0 #== 5_004
            ]
    , testCase "open_machine_field_item_at uses the carried wrapper offset" $
        passertEval $
          pand'List
            [ pmachineFieldItemLength # item1At #== 3
            , pmachineFieldItemBytes # item1At #== pconstant smallItem
            , pmachineFieldNextItemOffset # item1At #== 5_008
            ]
    , testCase "open_machine_field_item_at rejects an offset outside the field" $
        pfails $ pmachineFieldItemLength # badOffsetItem
    , testCase "machine_field_count authenticates and materialises the field" $
        passertEval $
          pmachineFieldCount
            # doorT
            # verifiedT
            # witnessSetT
            # 2
            # carriageT
            #== 2
    , testCase "machine_field_item_count comes from the authenticated view" $
        passertEval $ pmachineFieldItemCount # item0 #== 2
    , testCase "machine_field_item_chunk slices bounded-item windows" $
        passertEval $
          pand'List
            [ pmachineFieldItemChunk # item0 # 0
                #== pconstant (BS.take 4_095 largeItem)
            , pmachineFieldItemChunk # item0 # 1
                #== pconstant (BS.drop 4_095 largeItem)
            ]
    , testCase "machine_field_item_chunk rejects a chunk past the item" $
        pfails $ plengthBS #$ pmachineFieldItemChunk # item0 # 2
    , testCase "machine_field_item_chunk_count is derived from the item length" $
        passertEval $ pmachineFieldItemChunkCount # item0 #== 2
    , testCase "machine_field_item_commitment is derived from authenticated bytes" $
        passertEval $
          pmachineFieldItemCommitment # item0
            #== BoundedItem.pfromBytes # 2 # 0 # pconstant largeItem
    , testCase "machine_field_item_bytes_match accepts only the exact payload" $
        passertEval $
          pand'List
            [ pmachineFieldItemBytesMatch # item0 # pconstant largeItem
            , pnot #$ pmachineFieldItemBytesMatch # item0 # pconstant (BS.drop 1 largeItem)
            , pnot #$ pmachineFieldItemBytesMatch # item0 # pconstant (BS.replicate 5_000 0x42)
            ]
    ]

item0 :: forall s. Term s PMachineFieldItemV1
item0 = popenMachineFieldItem # doorT # verifiedT # witnessSetT # 2 # 0 # carriageT

item1At :: forall s. Term s PMachineFieldItemV1
item1At = popenMachineFieldItemAt # doorT # verifiedT # witnessSetT # 2 # 1 # 5_004 # carriageT

badOffsetItem :: forall s. Term s PMachineFieldItemV1
badOffsetItem = popenMachineFieldItemAt # doorT # verifiedT # witnessSetT # 2 # 1 # 6_000 # carriageT

doorT :: forall s. Term s PMachineFieldDoorV1
doorT =
  pcon
    PMachineFieldDoorV1
      { pmachineDoor'referenceInputs = pnil
      , pmachineDoor'certificatePolicyId = pdata (pconstant certificatePolicy)
      }

carriageT :: forall s. Term s PFieldCarriageV1
carriageT = pcon (PInline (pdata (pconstant preimage)))

verifiedT :: forall s. Term s PVerifiedMidgardNativeTxCompact
verifiedT =
  pcon
    PVerifiedMidgardNativeTxCompact
      { pverified'txId = pconstant (BS.replicate 32 0x11)
      , pverified'version = 1
      , pverified'txCompact =
          pcon
            PNativeTxCompact
              { pcompact'body = bodyT
              , pcompact'witnessSetHash = pconstant (BS.replicate 32 0x22)
              , pcompact'validityCode = 1
              }
      }

bodyT :: forall s. Term s PNativeTxBodyCompact
bodyT =
  pcon
    PNativeTxBodyCompact
      { pbodyCompact'spendInputsHash = pconstant (hash32 0)
      , pbodyCompact'referenceInputsHash = pconstant (hash32 1)
      , pbodyCompact'outputsHash = pconstant (blake2b256 preimage)
      , pbodyCompact'fee = 1_000_000
      , pbodyCompact'validityIntervalStart = 0
      , pbodyCompact'validityIntervalEnd = 1
      , pbodyCompact'requiredObserversHash = pconstant (hash32 3)
      , pbodyCompact'requiredSignersHash = pconstant (hash32 4)
      , pbodyCompact'mintHash = pconstant (hash32 5)
      , pbodyCompact'scriptIntegrityHash = pconstant (hash32 6)
      , pbodyCompact'auxiliaryDataHash = pconstant (hash32 7)
      , pbodyCompact'networkId = 1
      }

witnessSetT :: forall s. Term s PNativeTxWitnessSetCompact
witnessSetT =
  pcon
    PNativeTxWitnessSetCompact
      { pwitnessSetCompact'addrTxWitsHash = pdata (pconstant (hash32 8))
      , pwitnessSetCompact'scriptTxWitsHash = pdata (pconstant (hash32 9))
      , pwitnessSetCompact'redeemerTxWitsHash = pdata (pconstant (hash32 10))
      }

largeItem, smallItem, preimage :: BS.ByteString
largeItem = BS.replicate 5_000 0x41
smallItem = "xyz"
preimage = "\x82\x59\x13\x88" <> largeItem <> "\x43" <> smallItem

certificatePolicy :: CurrencySymbol
certificatePolicy = CurrencySymbol (toBuiltin (BS.replicate 28 0x91))

blake2b256 :: BS.ByteString -> BS.ByteString
blake2b256 = fromBuiltin . Builtins.blake2b_256 . toBuiltin

hash32 :: Int -> BS.ByteString
hash32 = blake2b256 . BS.singleton . fromIntegral
