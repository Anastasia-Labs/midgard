{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Testing.TxOrderValidator
Description : Behavioural tests for the Plutarch port of the mint and spend sides of
              @validators/user-events/tx-order-v1.ak@.

An order has a single outcome — it is released once the block that included it
has settled — so what these vary is the claimed verdict and the shape of the
released UTxO. The Aiken file has no @test@ blocks, so none of these mirror one.
-}
module Testing.TxOrderValidator (tests) where

import Data.Bits (shiftR, (.&.))
import Data.ByteString qualified as BS
import Data.ByteString.Base16 qualified as Base16
import Data.ByteString.Char8 qualified as BS8
import PlutusCore.Data qualified as PD
import PlutusLedgerApi.V1.Address (scriptHashAddress)
import PlutusLedgerApi.V1.Interval (Extended (..), Interval (..), LowerBound (..), UpperBound (..))
import PlutusLedgerApi.V1.Value (CurrencySymbol (..), TokenName (..), getValue, singleton)
import PlutusLedgerApi.V3 (
  Credential (ScriptCredential),
  Datum (..),
  OutputDatum (..),
  POSIXTime (..),
  Redeemer (..),
  ScriptContext (..),
  ScriptHash (..),
  ScriptInfo (MintingScript, SpendingScript),
  ScriptPurpose (Certifying, Minting, Rewarding),
  TxCert (TxCertRegStaking),
  TxId (..),
  TxInInfo (..),
  TxOut (..),
  TxOutRef (..),
  scriptContextTxInfo,
  toBuiltinData,
  txInfoInputs,
  txInfoMint,
  txInfoOutputs,
  txInfoRedeemers,
  txInfoReferenceInputs,
  txInfoValidRange,
 )
import PlutusLedgerApi.V3.MintValue (MintValue (UnsafeMintValue))
import PlutusTx.AssocMap qualified as Map
import PlutusTx.Builtins (
  BuiltinData,
  blake2b_256,
  builtinDataToData,
  dataToBuiltinData,
  fromBuiltin,
  serialiseData,
  toBuiltin,
 )
import Test.Tasty
import Test.Tasty.HUnit

import Plutarch.Prelude

import Midgard.Validators.TxOrder (txOrderMintValidator, txOrderSpendValidator)
import Testing.DepositValidator qualified as UserEventFixture
import Testing.Eval (pfails, psucceeds)
import Testing.ScriptContextBuilder (buildScriptContext, currencySymbolFromHex, mkAdaValue)

-- | Collects the tests defined in this module.
tests :: TestTree
tests =
  testGroup
    "Transaction Order Validator Tests"
    [ testGroup
        "mint"
        [ testCase "authenticates an order whose transaction has no field material" $
            psucceeds $ runMint mintDefaults
        , testCase "authenticates non-empty material through its terminal receipt" $
            psucceeds $
              runMint mintDefaults {mEmptyMaterial = False, mTerminalReceipt = True}
        , testCase "rejects non-empty transaction material without a terminal receipt" $
            pfails $ runMint mintDefaults {mEmptyMaterial = False}
        , testCase "rejects a terminal receipt locked by another script" $
            pfails $
              runMint
                mintDefaults
                  { mEmptyMaterial = False
                  , mTerminalReceipt = True
                  , mReceiptScriptMatches = False
                  }
        , testCase "rejects an order output carrying an asset beyond its NFT" $
            pfails $ runMint mintDefaults {mExtraAsset = True}
        ]
    , testGroup
        "spend"
        [ testCase "releases an order the settlement recorded as valid" $
            psucceeds $ runSpend defaults
        , testCase "releases an order recorded with a failed script" $
            psucceeds $ runSpend defaults {sClaimed = txFailedScript, sCommitted = txFailedScript}
        , -- The verdict is rebuilt into the stored value before the proof, so
          -- one the operator never committed cannot corroborate.
          testCase "rejects a verdict the committed root does not carry" $
            pfails $ runSpend defaults {sClaimed = txFailedScript}
        , testCase "rejects an order the forced-transactions root omits" $
            pfails $ runSpend defaults {sCommittedTxId = Just "other"}
        , testCase "rejects a release to an address other than the refund one" $
            pfails $ runSpend defaults {sRefundAddressMatches = False}
        , testCase "rejects a release carrying a datum other than the refund one" $
            pfails $ runSpend defaults {sRefundDatumMatches = False}
        , testCase "rejects an output that keeps the order's NFT" $
            pfails $ runSpend defaults {sBurnNft = False}
        , testCase "rejects an output carrying a reference script" $
            pfails $ runSpend defaults {sRefScript = True}
        , testCase "rejects a mint redeemer that is not a burn" $
            pfails $ runSpend defaults {sBurnRedeemer = False}
        ]
    ]

--------------------------------------------------------------------------------
-- Fixtures
--------------------------------------------------------------------------------

hubOraclePolicy, txOrderPolicy, settlementPolicy, auxiliaryPolicy :: CurrencySymbol
hubOraclePolicy = repeatedByte 0x11
txOrderPolicy = repeatedByte 0x22
settlementPolicy = repeatedByte 0x33
auxiliaryPolicy = repeatedByte 0x55

repeatedByte :: Int -> CurrencySymbol
repeatedByte b = currencySymbolFromHex (concatMap hex (replicate 28 b))
  where
    hex n = let xs = "0123456789abcdef" in [xs !! (n `div` 16), xs !! (n `mod` 16)]

outRefN :: Integer -> TxOutRef
outRefN = TxOutRef (TxId "0101010101010101010101010101010101010101010101010101010101010101")

hubAssetName :: TokenName
hubAssetName = TokenName "MIDGARD_HUB_ORACLE"

nonceRef :: TxOutRef
nonceRef =
  TxOutRef
    (TxId (toBuiltin (BS.replicate 32 0xaa)))
    0

nonce :: TokenName
nonce = UserEventFixture.nonceFor nonceRef

witnessScriptHash :: ScriptHash
witnessScriptHash = UserEventFixture.witnessScriptHash nonce

validTo, eventWaitDuration :: Integer
validTo = 2_000
eventWaitDuration = 60_000

orderNonce :: TokenName
orderNonce = TokenName "ord"

phasValidatorHash :: BS.ByteString
phasValidatorHash =
  either (error "bad hex") id
    (Base16.decode (BS8.pack "1fc59ff54da02f2535d64b40b647a8826c8b3d914d7ba5257f5b2721"))

phasRoot :: BS.ByteString
phasRoot = BS.replicate 32 0xaa

orderCount :: Integer
orderCount = 2

-- | @commit_counted_root@ for the forced-transactions domain (tag 1).
forcedRoot :: BS.ByteString
forcedRoot =
  fromBuiltin $
    blake2b_256
      ( toBuiltin ("MidgardRootCountV1" :: BS.ByteString)
          <> serialiseData (dataToBuiltinData (PD.Constr 1 []))
          <> toBuiltin phasRoot
          <> serialiseData (dataToBuiltinData (PD.I orderCount))
      )

serialisedOf :: PD.Data -> BS.ByteString
serialisedOf = fromBuiltin . serialiseData . dataToBuiltinData

addrData :: CurrencySymbol -> PD.Data
addrData cs =
  PD.Constr 0 [PD.Constr 1 [PD.B (fromBuiltin (unCurrencySymbol cs))], PD.Constr 1 []]

txValid, txFailedScript :: PD.Data
txValid = PD.Constr 0 []
txFailedScript = PD.Constr 3 []

orderId :: PD.Data
orderId = builtinDataToData (toBuiltinData (outRefN 4))

nativeSource :: PD.Data
nativeSource = PD.Constr 0 [PD.B "cbor", PD.B "wits", PD.B "lens"]

-- | @TxOrderPayloadV1 { tx_id, transaction_commitment, source, terminal_ref }@.
orderPayload :: BS.ByteString -> PD.Data
orderPayload txId = PD.Constr 0 [PD.B txId, PD.B "commit", nativeSource, PD.Constr 1 []]

--------------------------------------------------------------------------------
-- Mint transaction assembly
--------------------------------------------------------------------------------

data Mint = Mint
  { mEmptyMaterial :: Bool
  , mTerminalReceipt :: Bool
  , mReceiptScriptMatches :: Bool
  , mExtraAsset :: Bool
  }

mintDefaults :: Mint
mintDefaults =
  Mint
    { mEmptyMaterial = True
    , mTerminalReceipt = False
    , mReceiptScriptMatches = True
    , mExtraAsset = False
    }

runMint :: forall s. Mint -> Term s PUnit
runMint mintCase =
  txOrderMintValidator
    # pdata (pconstant (ScriptHash (unCurrencySymbol hubOraclePolicy)))
    # pdata (pconstant receiptScriptHash)
    # pdata (pconstant fieldReceiptPolicy)
    # pconstant ctx
  where
    bodyCbor = emptyBodyCbor (mEmptyMaterial mintCase)
    witnessCbor = emptyWitnessSetCbor
    lengthsCbor =
      if mEmptyMaterial mintCase
        then "\x89\x01\x01\x01\x01\x01\x01\x01\x01\x01"
        else "\x89\x03\x01\x01\x01\x01\x01\x01\x01\x01"
    compactCbor =
      "\x84\x01"
        <> bodyCbor
        <> definiteBytes32 (hash256 witnessCbor)
        <> "\x00"
    transactionId = hash256 ("MidgardNativeTxBodyV1\x01" <> bodyCbor)
    transactionCommitment =
      hash256 $
        "MidgardNativeTxProofSourceV1\x01\x83"
          <> definiteBytes compactCbor
          <> definiteBytes witnessCbor
          <> definiteBytes lengthsCbor
    source = PD.Constr 0 [PD.B compactCbor, PD.B witnessCbor, PD.B lengthsCbor]
    payload =
      PD.Constr
        0
        [ PD.B transactionId
        , PD.B transactionCommitment
        , source
        , if mTerminalReceipt mintCase
            then PD.Constr 0 [builtinDataToData (toBuiltinData receiptRef)]
            else PD.Constr 1 []
        ]
    datum =
      dataToBuiltinData $
        PD.Constr
          0
          [ PD.Constr 0 [builtinDataToData (toBuiltinData nonceRef), payload]
          , PD.I (validTo + eventWaitDuration)
          , PD.B witnessHashBytes
          , addrData auxiliaryPolicy
          , PD.Constr 0 []
          ]
    witnessHashBytes = case witnessScriptHash of
      ScriptHash bytes -> fromBuiltin bytes
    eventValue =
      mkAdaValue 2_000_000
        <> singleton txOrderPolicy nonce 1
        <> if mExtraAsset mintCase then singleton auxiliaryPolicy (TokenName "extra") 1 else mempty
    nonceInput =
      TxInInfo
        nonceRef
        ( TxOut
            (scriptHashAddress (ScriptHash (unCurrencySymbol auxiliaryPolicy)))
            (mkAdaValue 2_000_000)
            NoOutputDatum
            Nothing
        )
    witnessRedeemer =
      dataToBuiltinData (PD.Constr 0 [PD.B (fromBuiltin (unCurrencySymbol txOrderPolicy))])
    mintRedeemer = dataToBuiltinData (PD.Constr 0 [PD.I 0, PD.I 0, PD.I 0, PD.I 0])
    base = buildScriptContext mempty
    txInfo =
      (scriptContextTxInfo base)
        { txInfoInputs = [nonceInput]
        , txInfoReferenceInputs =
            [hubRefIn]
              <> if mTerminalReceipt mintCase then [terminalReceiptInput] else []
        , txInfoOutputs =
            [ TxOut
                (scriptHashAddress (ScriptHash (unCurrencySymbol txOrderPolicy)))
                eventValue
                (OutputDatum (Datum datum))
                Nothing
            ]
        , txInfoMint = UnsafeMintValue (getValue (singleton txOrderPolicy nonce 1))
        , txInfoValidRange =
            Interval
              (LowerBound (Finite (POSIXTime 1_000)) True)
              (UpperBound (Finite (POSIXTime validTo)) True)
        , txInfoRedeemers =
            Map.unsafeFromList
              [ ( Certifying 0 (TxCertRegStaking (ScriptCredential witnessScriptHash) Nothing)
                , Redeemer witnessRedeemer
                )
              ]
        }
    ctx = ScriptContext txInfo (Redeemer mintRedeemer) (MintingScript txOrderPolicy)
    terminalReceiptInput =
      TxInInfo
        receiptRef
        ( TxOut
            ( scriptHashAddress $
                if mReceiptScriptMatches mintCase
                  then receiptScriptHash
                  else ScriptHash (unCurrencySymbol auxiliaryPolicy)
            )
            (mkAdaValue 2_000_000 <> singleton fieldReceiptPolicy receiptAssetName 1)
            (OutputDatum (Datum (dataToBuiltinData receiptDatum)))
            Nothing
        )
    receiptDatum =
      PD.Constr
        0
        [ PD.B (fromBuiltin (unCurrencySymbol fieldReceiptPolicy))
        , PD.B (fromBuiltin (unCurrencySymbol txOrderPolicy))
        , builtinDataToData (toBuiltinData nonceRef)
        , PD.B transactionCommitment
        , collectionProof
        , PD.I 0
        , builtinDataToData (toBuiltinData fieldRef)
        , PD.Constr 1 []
        , PD.I 3
        ]
    receiptAssetName =
      TokenName . toBuiltin . hash256 $
        "MidgardTxFieldReceiptV1"
          <> fromBuiltin (unCurrencySymbol txOrderPolicy)
          <> txIdBytes nonceRef
          <> bigEndian 8 (txOutRefIdx nonceRef)
          <> transactionCommitment
          <> "\x00"
          <> bigEndian 8 0
          <> bigEndian 8 0

emptyBodyCbor :: Bool -> BS.ByteString
emptyBodyCbor isEmpty =
  BS.concat
    [ "\x8c"
    , definiteBytes32 (if isEmpty then emptyFieldCommitment else countedCollectionCommitment)
    , definiteBytes32 emptyFieldCommitment
    , definiteBytes32 emptyFieldCommitment
    , "\x00\x00\x00"
    , definiteBytes32 emptyFieldCommitment
    , definiteBytes32 emptyFieldCommitment
    , definiteBytes32 emptyFieldCommitment
    , definiteBytes32 (BS.replicate 32 0x11)
    , definiteBytes32 (BS.replicate 32 0x22)
    , "\x00"
    ]

emptyWitnessSetCbor :: BS.ByteString
emptyWitnessSetCbor =
  "\x83"
    <> definiteBytes32 emptyFieldCommitment
    <> definiteBytes32 emptyFieldCommitment
    <> definiteBytes32 emptyFieldCommitment

emptyFieldCommitment :: BS.ByteString
emptyFieldCommitment =
  either (error "bad empty field commitment hex") id $
    Base16.decode (BS8.pack "45b0cfc220ceec5b7c1c62c4d4193d38e4eba48e8815729ce75f9c0ab0e4c1c0")

hash256 :: BS.ByteString -> BS.ByteString
hash256 = fromBuiltin . blake2b_256 . toBuiltin

definiteBytes32 :: BS.ByteString -> BS.ByteString
definiteBytes32 = ("\x58\x20" <>)

definiteBytes :: BS.ByteString -> BS.ByteString
definiteBytes bytes
  | BS.length bytes <= 23 = BS.singleton (fromIntegral (64 + BS.length bytes)) <> bytes
  | BS.length bytes <= 255 = BS.pack [0x58, fromIntegral (BS.length bytes)] <> bytes
  | BS.length bytes <= 65_535 =
      BS.pack
        [ 0x59
        , fromIntegral (BS.length bytes `div` 256)
        , fromIntegral (BS.length bytes `mod` 256)
        ]
        <> bytes
  | otherwise = error "test fixture byte string is too large"

receiptScriptHash :: ScriptHash
receiptScriptHash = ScriptHash (toBuiltin (BS.replicate 28 0x66))

fieldReceiptPolicy :: CurrencySymbol
fieldReceiptPolicy = repeatedByte 0x77

receiptRef, fieldRef :: TxOutRef
receiptRef = outRefN 12
fieldRef = outRefN 13

countedItemCommitment, countedLeaf, countedFrontierCommitment, countedCollectionCommitment :: BS.ByteString
countedItemCommitment = BS.replicate 32 0xab
countedLeaf =
  hash256 $
    "MidgardBoundedCollectionItemV1\x85\x01\x00\x00\x01"
      <> definiteBytes32 countedItemCommitment
countedFrontierCommitment =
  hash256 $
    "MidgardValidationMerkleFrontierV1\x01\x81\x82\x00"
      <> definiteBytes32 countedLeaf
countedCollectionCommitment =
  hash256 $
    "MidgardBoundedCollectionCommitmentV1\x84\x01\x00\x01"
      <> definiteBytes32 countedFrontierCommitment

collectionProof :: PD.Data
collectionProof =
  PD.Constr
    0
    [ PD.I 1
    , PD.I 0
    , PD.I 1
    , PD.I 0
    , PD.I 1
    , PD.B countedItemCommitment
    , PD.List [PD.Constr 0 [PD.I 0, PD.B countedLeaf]]
    , PD.List []
    ]

txIdBytes :: TxOutRef -> BS.ByteString
txIdBytes (TxOutRef (TxId bytes) _) = fromBuiltin bytes

bigEndian :: Int -> Integer -> BS.ByteString
bigEndian width n =
  BS.pack
    [ fromIntegral ((n `shiftR` (8 * i)) .&. 0xff)
    | i <- [width - 1, width - 2 .. 0]
    ]

-- | @ForcedInclusionTxV1 { tx_id, source, operator_validity }@.
forcedInclusionTx :: BS.ByteString -> PD.Data -> PD.Data
forcedInclusionTx txId validity = PD.Constr 0 [PD.B txId, nativeSource, validity]

--------------------------------------------------------------------------------
-- Transaction assembly
--------------------------------------------------------------------------------

data Spend = Spend
  { sClaimed :: PD.Data
  , sCommitted :: PD.Data
  , sCommittedTxId :: Maybe BS.ByteString
  , sRefundAddressMatches :: Bool
  , sRefundDatumMatches :: Bool
  , sBurnNft :: Bool
  , sRefScript :: Bool
  , sBurnRedeemer :: Bool
  }

defaults :: Spend
defaults =
  Spend
    { sClaimed = txValid
    , sCommitted = txValid
    , sCommittedTxId = Nothing
    , sRefundAddressMatches = True
    , sRefundDatumMatches = True
    , sBurnNft = True
    , sRefScript = False
    , sBurnRedeemer = True
    }

runSpend :: forall s. Spend -> Term s PUnit
runSpend sp =
  txOrderSpendValidator
    # pdata (pconstant (ScriptHash (unCurrencySymbol hubOraclePolicy)))
    # pconstant ctx
  where
    ownTxId = "txid"
    ownValue = mkAdaValue 2_000_000 <> singleton txOrderPolicy orderNonce 1
    outValue =
      if sBurnNft sp
        then mkAdaValue 2_000_000
        else mkAdaValue 2_000_000 <> singleton txOrderPolicy orderNonce 1

    -- An OptimisticDatum: event, inclusion_time, witness, refund_address,
    -- refund_datum.
    orderDatum =
      dataToBuiltinData $
        PD.Constr
          0
          [ PD.Constr 0 [orderId, orderPayload ownTxId]
          , PD.I 0
          , PD.B "witness"
          , addrData auxiliaryPolicy
          , PD.Constr 0 [] -- NoDatum
          ]

    orderIn =
      TxInInfo
        (outRefN 0)
        ( TxOut
            (scriptHashAddress (ScriptHash (unCurrencySymbol txOrderPolicy)))
            ownValue
            (OutputDatum (Datum orderDatum))
            Nothing
        )

    settlementDatum =
      dataToBuiltinData $
        PD.Constr
          0
          [ PD.B (BS.replicate 32 0x01)
          , PD.B (BS.replicate 32 0x02)
          , PD.B forcedRoot -- forced_transactions_root
          , PD.B (BS.replicate 32 0x04)
          , PD.Constr 1 []
          ]

    settlementRefIn =
      TxInInfo
        (outRefN 8)
        ( TxOut
            (scriptHashAddress (ScriptHash (unCurrencySymbol settlementPolicy)))
            (mkAdaValue 2_000_000 <> singleton settlementPolicy (TokenName "s") 1)
            (OutputDatum (Datum settlementDatum))
            Nothing
        )

    -- What the tree records; the negative cases make it disagree with the claim.
    provenValue =
      forcedInclusionTx (maybe ownTxId id (sCommittedTxId sp)) (sCommitted sp)

    membershipProof =
      PD.Constr
        0
        [ PD.Constr 1 [] -- ForcedTransactionsV1RootDomain
        , PD.B forcedRoot
        , PD.B phasRoot
        , PD.I orderCount
        , PD.B (serialisedOf orderId)
        , PD.B (serialisedOf provenValue)
        , PD.List [PD.B "step"]
        ]

    phasRedeemer =
      dataToBuiltinData $
        PD.List
          [ PD.B phasRoot
          , PD.B (serialisedOf orderId)
          , PD.B (serialisedOf provenValue)
          , PD.List [PD.B "step"]
          ]

    burnRedeemer
      | sBurnRedeemer sp =
          dataToBuiltinData (PD.Constr 1 [PD.B (unTokenName orderNonce), PD.I 0])
      | otherwise = dataToBuiltinData (PD.Constr 0 [PD.I 0, PD.I 0, PD.I 0, PD.I 0])
      where
        unTokenName (TokenName b) = fromBuiltin b

    spendRedeemer =
      dataToBuiltinData $
        PD.Constr
          0
          [ PD.I 0 -- input_index
          , PD.I 0 -- output_index
          , PD.I 0 -- hub_ref_input_index
          , PD.I 1 -- settlement_ref_input_index
          , PD.I 0 -- burn_redeemer_index
          , membershipProof
          , PD.I 1
          , sClaimed sp
          ]

    outAddressPolicy = if sRefundAddressMatches sp then auxiliaryPolicy else txOrderPolicy

    base = buildScriptContext mempty
    txInfo =
      (scriptContextTxInfo base)
        { txInfoInputs = [orderIn]
        , txInfoReferenceInputs = [hubRefIn, settlementRefIn]
        , txInfoOutputs =
            [ TxOut
                (scriptHashAddress (ScriptHash (unCurrencySymbol outAddressPolicy)))
                outValue
                ( if sRefundDatumMatches sp
                    then NoOutputDatum
                    else OutputDatum (Datum (dataToBuiltinData (PD.I 1)))
                )
                (if sRefScript sp then Just (ScriptHash "ab") else Nothing)
            ]
        , txInfoMint = UnsafeMintValue (getValue (singleton txOrderPolicy orderNonce (-1)))
        , txInfoRedeemers =
            Map.unsafeFromList
              [ (Minting txOrderPolicy, Redeemer burnRedeemer)
              ,
                ( Rewarding (ScriptCredential (ScriptHash (toBuiltin phasValidatorHash)))
                , Redeemer phasRedeemer
                )
              ]
        }
    ctx =
      ScriptContext
        txInfo
        (Redeemer spendRedeemer)
        (SpendingScript (outRefN 0) (Just (Datum orderDatum)))

-- | The hub oracle; @tx_order@ (9) and @settlement@ (10) are what is read.
hubRefIn :: TxInInfo
hubRefIn =
  TxInInfo
    (outRefN 9)
    ( TxOut
        (scriptHashAddress (ScriptHash (unCurrencySymbol hubOraclePolicy)))
        (mkAdaValue 2_000_000 <> singleton hubOraclePolicy hubAssetName 1)
        (OutputDatum (Datum hubDatum))
        Nothing
    )

hubDatum :: BuiltinData
hubDatum =
  dataToBuiltinData $
    PD.Constr
      0
      ( replicate 9 auxPolicyData
          <> [PD.B (fromBuiltin (unCurrencySymbol txOrderPolicy))] -- 9: tx_order
          <> [PD.B (fromBuiltin (unCurrencySymbol settlementPolicy))] -- 10: settlement
          <> [auxPolicyData]
          <> replicate 9 (addrData auxiliaryPolicy)
          <> [addrData txOrderPolicy] -- address 9: tx_order_addr
          <> replicate 3 (addrData auxiliaryPolicy)
          <> [auxPolicyData]
      )
  where
    auxPolicyData = PD.B (fromBuiltin (unCurrencySymbol auxiliaryPolicy))
