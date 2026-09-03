{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Testing.WithdrawalValidator
Description : Behavioural tests for the Plutarch port of the @mint@ side of
              @validators/user-events/withdrawal.ak@.

The shared half of this validator is already covered by
"Testing.DepositValidator", so these concentrate on what withdrawal adds: the
value must hold nothing but Ada and the authentication NFT, and the event must
carry the @WithdrawalIsValid@ verdict.

The Aiken file has no @test@ blocks, so none of these mirror one.
-}
module Testing.WithdrawalValidator (tests) where

import Numeric (showHex)

import Data.ByteString qualified as BS
import Data.ByteString.Base16 qualified as Base16
import Data.ByteString.Char8 qualified as BS8
import PlutusCore.Data qualified as PD
import PlutusLedgerApi.V1.Address (scriptHashAddress)
import PlutusLedgerApi.V1.Interval (Extended (..), Interval (..), LowerBound (..), UpperBound (..))
import PlutusLedgerApi.V1.Value (CurrencySymbol (..), TokenName (..), Value, getValue, singleton)
import PlutusLedgerApi.V3 (
  Credential (..),
  Datum (..),
  OutputDatum (..),
  POSIXTime (..),
  Redeemer (..),
  ScriptContext (..),
  ScriptHash (..),
  ScriptInfo (MintingScript, SpendingScript),
  ScriptPurpose (Certifying, Minting, Rewarding),
  Credential (ScriptCredential),
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
  BuiltinByteString,
  BuiltinData,
  blake2b_224,
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

import Midgard.Validators.Withdrawal (withdrawalMintValidator, withdrawalSpendValidator)
import Testing.Eval (pfails, psucceeds)
import Testing.ScriptContextBuilder (buildScriptContext, currencySymbolFromHex, mkAdaValue)

-- | Collects the tests defined in this module.
tests :: TestTree
tests =
  testGroup
    "Withdrawal Validator Tests"
    [ testGroup
        "mint"
        [ testCase "accepts a valid withdrawal holding only Ada and its NFT" $
            psucceeds $ runMint validityValid mempty
        , -- Only valid withdrawals may initialise a payout accumulator, so the
          -- other seven verdicts must not be mintable at all.
          testCase "rejects a withdrawal marked as a non-existent UTxO" $
            pfails $ runMint validityNonExistent mempty
        , testCase "rejects a withdrawal marked with an incorrect owner" $
            pfails $ runMint validityIncorrectOwner mempty
        , testCase "rejects a withdrawal marked as spent, which carries a field" $
            pfails $ runMint validitySpent mempty
        , -- Stricter than the deposit: no basket of assets is allowed here.
          testCase "rejects a withdrawal carrying any asset beyond its NFT" $
            pfails $ runMint validityValid (singleton otherPolicy (TokenName "x") 1)
        ]
    , testGroup
        "spend / InitializePayout"
        [ testCase "opens an accumulator below the L2 target" $
            psucceeds $ runSpend initializeDefaults
        , testCase "accepts an accumulator opened at exactly the target" $
            psucceeds $ runSpend initializeDefaults {sTargetAda = 2_000_000}
        , -- Opening above the target makes the exact-value conclusion path
          -- unreachable, stranding the payout.
          testCase "rejects an accumulator opened above the L2 target" $
            pfails $ runSpend initializeDefaults {sTargetAda = 1_000_000}
        , testCase "rejects a payout datum that alters the L2 target" $
            pfails $ runSpend initializeDefaults {sPayoutDatumValue = Just alteredL2Value}
        , testCase "rejects an output going anywhere but the payout address" $
            pfails $ runSpend initializeDefaults {sOutputToPayout = False}
        , testCase "rejects a payout mint redeemer naming another UTxO" $
            pfails $ runSpend initializeDefaults {sMintOutRefMatches = False}
        , -- Only a withdrawal the operator judged valid may open an accumulator.
          testCase "rejects initialising from an invalid withdrawal" $
            pfails $ runSpend initializeDefaults {sTreeValidity = PD.Constr 3 []}
        ]
    , testGroup
        "spend / Refund"
        [ testCase "refunds a withdrawal the tree records as invalid" $
            psucceeds $ runSpend refundDefaults
        , -- The claimed verdict is substituted into the info before the
          -- membership check, so a verdict the operator never gave cannot pass.
          testCase "rejects a verdict the settlement tree does not record" $
            pfails $ runSpend refundDefaults {sPurpose = PD.Constr 1 [PD.Constr 4 []]}
        , testCase "rejects refunding a valid withdrawal" $
            pfails $
              runSpend refundDefaults {sPurpose = PD.Constr 1 [PD.Constr 0 []], sTreeValidity = PD.Constr 0 []}
        , testCase "rejects a refund sent to the wrong address" $
            pfails $ runSpend refundDefaults {sRefundAddressMatches = False}
        ]
    ]

-- | The L2 target with its quantity changed — the payout datum must not do this.
alteredL2Value :: PD.Data
alteredL2Value = PD.Map [(PD.B "", PD.Map [(PD.B "", PD.I 99)])]

--------------------------------------------------------------------------------
-- Fixtures
--------------------------------------------------------------------------------

hubOraclePolicy, withdrawalPolicy, otherPolicy, auxiliaryPolicy :: CurrencySymbol
hubOraclePolicy = repeatedByte 0x11
withdrawalPolicy = repeatedByte 0x22
otherPolicy = repeatedByte 0x33
auxiliaryPolicy = repeatedByte 0x55

repeatedByte :: Int -> CurrencySymbol
repeatedByte b = currencySymbolFromHex (concat (replicate 28 h))
  where
    h = let x = showHex b "" in if length x == 1 then '0' : x else x

nonceRef :: TxOutRef
nonceRef =
  TxOutRef
    (TxId "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa")
    0

outRefN :: Integer -> TxOutRef
outRefN = TxOutRef (TxId "0101010101010101010101010101010101010101010101010101010101010101")

validTo, eventWaitDuration, plutusVersion :: Integer
validTo = 2_000
eventWaitDuration = 60_000
plutusVersion = 3

hubAssetName :: TokenName
hubAssetName = TokenName "MIDGARD_HUB_ORACLE"

nonce :: TokenName
nonce = TokenName (blake2b_256 (serialiseData (toBuiltinData nonceRef)))

-- | @parameter_validation.apply_prehashed_param@, recomputed independently.
witnessScriptHash :: ScriptHash
witnessScriptHash =
  ScriptHash
    ( blake2b_224
        ( toBuiltin (BS.singleton (fromIntegral plutusVersion))
            <> witnessPrefix
            <> unTokenName nonce
            <> toBuiltin (BS.pack [0x00, 0x01])
        )
    )
  where
    unTokenName (TokenName b) = b

{- | @env.user_events_witness_script_prefix@.

Shared with "Testing.DepositValidator" only by value, not by import — both
modules keep their own copy so a one-sided edit to the constant shows up as a
failing test.
-}
witnessPrefix :: BuiltinByteString
witnessPrefix =
  toBuiltin . either (error "bad witness prefix hex") id . Base16.decode . BS8.pack $
    concat
      [ "5902ce0101003229800aba2aba1aab9faab9eaab9dab9a9bae0024888888966002646530"
      , "01300800198041804800cc0200092225980099b8748018c020dd500146600260126ea800"
      , "a6e1d20029b874800260106ea800d222232332259800980280244c8c966002602a005004"
      , "8b2026375c602600260206ea802a2b30013006004899192cc004c05400a00916404c6eb4"
      , "c04c004c040dd5005456600266e1d2004004899192cc004c05400a00916404c6eb4c04c0"
      , "04c040dd500545900e201c40382653001300100198071baa009918091809980998099809"
      , "9809800a444b3001300700289919912cc004c028006260160051598009805800c4cdc380"
      , "12400314a0809901319199119801001000912cc004006007132325980099b910150018ac"
      , "c004cdc780a800c4dd6980c001401501644cc010010c06c00d0161bae301600130180014"
      , "05c6464660020026eacc060c064c064c064c064c058dd5007112cc004006007132325980"
      , "099b910070018acc004cdc7803800c4dd5980c801401501744cc010010c07000d0171bae"
      , "301700130190014060297adef6c60148000c048dd50031bae30143012375401915980098"
      , "0400144c8c96600266ebc00401e2b3001300930133754003132598009805980a1baa0018"
      , "acc004cdd7980b980a9baa00230173015375400314a316404d16404c602c602e00516404"
      , "9164048602a002660066eb0c004c048dd50051bad301430123754019159800980418089b"
      , "aa0058992cc004c020c048dd5000c56600266ebcc054c04cdd5000980a98099baa0068a5"
      , "18b20228b20223014330033758600260246ea8028dd6980a18091baa00c8b20204040808"
      , "0444b30013371200290004400a2b30010028a5eb8233001003980a0014cdc0000a400280"
      , "19012201e375a602000a601e60200088b200e180400098021baa0088a4d1365640084c01"
      , "225820"
      ]

--------------------------------------------------------------------------------
-- Withdrawal validity verdicts
--------------------------------------------------------------------------------

validityValid, validityNonExistent, validityIncorrectOwner, validitySpent :: PD.Data
validityValid = PD.Constr 0 []
validityNonExistent = PD.Constr 1 []
validitySpent = PD.Constr 2 [PD.B "aa"]
validityIncorrectOwner = PD.Constr 3 []

--------------------------------------------------------------------------------
-- Transaction assembly
--------------------------------------------------------------------------------

{- | A withdrawal mint. @validity@ is the event's verdict; @extraAssets@ is
anything beyond Ada and the NFT that the produced UTxO carries.
-}
runMint :: forall s. PD.Data -> Value -> Term s PUnit
runMint validity extraAssets =
  withdrawalMintValidator
    # pdata (pconstant (ScriptHash (unCurrencySymbol hubOraclePolicy)))
    # pconstant ctx
  where
    withdrawalValue =
      mkAdaValue 2_000_000 <> singleton withdrawalPolicy nonce 1 <> extraAssets

    -- @OptimisticDatum { event, inclusion_time, witness, refund_address,
    -- refund_datum }@; only the first three are read by the mint path.
    withdrawalDatum =
      dataToBuiltinData $
        PD.Constr
          0
          [ PD.Constr -- WithdrawalEvent { id, info }
              0
              [ builtinDataToData (toBuiltinData nonceRef)
              , PD.Constr 0 [PD.B "body", PD.B "sig", validity]
              ]
          , PD.I (validTo + eventWaitDuration)
          , PD.B (fromBuiltin (unScriptHash witnessScriptHash))
          , addrData auxiliaryPolicy
          , PD.Constr 0 []
          ]
      where
        unScriptHash (ScriptHash b) = b

    witnessRedeemer =
      dataToBuiltinData
        (PD.Constr 0 [PD.B (fromBuiltin (unCurrencySymbol withdrawalPolicy))])

    mintRedeemer = dataToBuiltinData (PD.Constr 0 [PD.I 0, PD.I 0, PD.I 0, PD.I 0])

    base = buildScriptContext mempty
    txInfo =
      (scriptContextTxInfo base)
        { txInfoInputs =
            [ TxInInfo
                nonceRef
                ( TxOut
                    (scriptHashAddress (ScriptHash (unCurrencySymbol auxiliaryPolicy)))
                    (mkAdaValue 2_000_000)
                    NoOutputDatum
                    Nothing
                )
            ]
        , txInfoReferenceInputs = [hubRefIn]
        , txInfoOutputs =
            [ TxOut
                (scriptHashAddress (ScriptHash (unCurrencySymbol withdrawalPolicy)))
                withdrawalValue
                (OutputDatum (Datum withdrawalDatum))
                Nothing
            ]
        , txInfoMint = UnsafeMintValue (getValue (singleton withdrawalPolicy nonce 1))
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
    ctx = ScriptContext txInfo (Redeemer mintRedeemer) (MintingScript withdrawalPolicy)

addrData :: CurrencySymbol -> PD.Data
addrData cs =
  PD.Constr
    0
    [PD.Constr 1 [PD.B (fromBuiltin (unCurrencySymbol cs))], PD.Constr 1 []]

-- | The hub oracle reference input; only @withdrawal_addr@ (address 8) is read.
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
      ( replicate 8 auxPolicyData
          <> [PD.B (fromBuiltin (unCurrencySymbol withdrawalPolicy))] -- 8: withdrawal
          <> replicate 3 auxPolicyData
          <> replicate 8 (addrData auxiliaryPolicy)
          <> [addrData withdrawalPolicy] -- address 8: withdrawal_addr
          <> replicate 4 (addrData auxiliaryPolicy)
          <> [auxPolicyData]
      )
  where
    auxPolicyData = PD.B (fromBuiltin (unCurrencySymbol auxiliaryPolicy))

--------------------------------------------------------------------------------
-- Spend fixtures
--------------------------------------------------------------------------------

settlementPolicy, payoutPolicy :: CurrencySymbol
settlementPolicy = repeatedByte 0x66
payoutPolicy = repeatedByte 0x77

countedRootTag :: BS.ByteString
countedRootTag = "MidgardRootCountV1"

emptyMerkleRoot :: BS.ByteString
emptyMerkleRoot =
  either (error "bad hex") id (Base16.decode (BS8.pack "0e5751c026e543b2e8ab2eb06099daa1d1e5df47778f7787faab45cdf12fe3a8"))

phasValidatorHash :: BS.ByteString
phasValidatorHash =
  either (error "bad hex") id (Base16.decode (BS8.pack "1fc59ff54da02f2535d64b40b647a8826c8b3d914d7ba5257f5b2721"))

phasRoot :: BS.ByteString
phasRoot = BS.replicate 32 0xaa

withdrawalCount :: Integer
withdrawalCount = 3

-- | @commit_counted_root@ for the withdrawals domain (tag 0).
withdrawalsCountedRoot :: BS.ByteString
withdrawalsCountedRoot =
  fromBuiltin $
    blake2b_256
      ( toBuiltin countedRootTag
          <> serialiseData (dataToBuiltinData (PD.Constr 0 []))
          <> toBuiltin phasRoot
          <> serialiseData (dataToBuiltinData (PD.I withdrawalCount))
      )

serialisedOf :: PD.Data -> BS.ByteString
serialisedOf = fromBuiltin . serialiseData . dataToBuiltinData

{- | The L2 value owed, as an Ada-only target.

The accumulator opens holding exactly the withdrawal UTxO's Ada — the output is
the input minus the withdrawal NFT plus the payout NFT, so it carries no other
assets. The target must therefore cover that Ada, which is why these tests vary
the target rather than the output.
-}
l2ValueWith :: Integer -> PD.Data
l2ValueWith ada = PD.Map [(PD.B "", PD.Map [(PD.B "", PD.I ada)])]

l1AddressData :: PD.Data
l1AddressData = addrData auxiliaryPolicy

l1DatumData :: PD.Data
l1DatumData = PD.Constr 0 [] -- NoDatum

withdrawalBody :: Integer -> PD.Data
withdrawalBody targetAda =
  PD.Constr 0 [PD.B "l2ref", PD.B "l2own", l2ValueWith targetAda, l1AddressData, l1DatumData]

-- | @WithdrawalInfo { body, signature, validity }@ with a chosen verdict.
withdrawalInfoWith :: Integer -> PD.Data -> PD.Data
withdrawalInfoWith targetAda validity =
  PD.Constr 0 [withdrawalBody targetAda, PD.B "sig", validity]

spentEventId :: PD.Data
spentEventId = builtinDataToData (toBuiltinData nonceRef)

-- | The knobs the spend negatives turn.
data Spend = Spend
  { sPurpose :: PD.Data
  , sTreeValidity :: PD.Data
  , sTargetAda :: Integer
  , sPayoutDatumValue :: Maybe PD.Data
  , sOutputToPayout :: Bool
  , sMintOutRefMatches :: Bool
  , sRefundAddressMatches :: Bool
  }

initializeDefaults :: Spend
initializeDefaults =
  Spend
    { sPurpose = PD.Constr 0 [] -- InitializePayout
    , sTreeValidity = PD.Constr 0 [] -- WithdrawalIsValid
    , sTargetAda = 3_000_000
    , sPayoutDatumValue = Nothing
    , sOutputToPayout = True
    , sMintOutRefMatches = True
    , sRefundAddressMatches = True
    }

refundDefaults :: Spend
refundDefaults =
  initializeDefaults
    { sPurpose = PD.Constr 1 [PD.Constr 3 []] -- Refund IncorrectWithdrawalOwner
    , sTreeValidity = PD.Constr 3 []
    }

{- | A withdrawal being spent.

Inputs: the withdrawal UTxO at 0. Reference inputs: hub at 0, settlement at 1.
Redeemers: the withdrawal burn at 0, the payout mint at 1, the @phas@ withdrawal
at 2.
-}
runSpend :: forall s. Spend -> Term s PUnit
runSpend sp =
  withdrawalSpendValidator
    # pdata (pconstant (ScriptHash (unCurrencySymbol hubOraclePolicy)))
    # pconstant ctx
  where
    isInitialize = case sPurpose sp of
      PD.Constr 0 _ -> True
      _ -> False

    ownValue = mkAdaValue 2_000_000 <> singleton withdrawalPolicy nonce 1

    outputValue
      | isInitialize = mkAdaValue 2_000_000 <> singleton payoutPolicy nonce 1
      | otherwise = mkAdaValue 2_000_000

    withdrawalDatum =
      dataToBuiltinData $
        PD.Constr
          0
          [ PD.Constr 0 [spentEventId, withdrawalInfoWith (sTargetAda sp) (treeVerdict sp)]
          , PD.I 0
          , PD.B "witness"
          , addrData auxiliaryPolicy -- refund_address
          , PD.Constr 0 [] -- refund_datum: NoDatum
          ]

    withdrawalIn =
      TxInInfo
        (outRefN 0)
        ( TxOut
            (scriptHashAddress (ScriptHash (unCurrencySymbol withdrawalPolicy)))
            ownValue
            (OutputDatum (Datum withdrawalDatum))
            Nothing
        )

    settlementDatum =
      dataToBuiltinData $
        PD.Constr
          0
          [ PD.B emptyMerkleRoot
          , PD.B withdrawalsCountedRoot -- withdrawals_root
          , PD.B emptyMerkleRoot
          , PD.B emptyMerkleRoot
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

    -- What the settlement tree records: the info with the tree's verdict.
    provenInfo = withdrawalInfoWith (sTargetAda sp) (sTreeValidity sp)

    membershipProof =
      PD.Constr
        0
        [ PD.Constr 0 [] -- WithdrawalsRootDomain
        , PD.B withdrawalsCountedRoot
        , PD.B phasRoot
        , PD.I withdrawalCount
        , PD.B (serialisedOf spentEventId)
        , PD.B (serialisedOf provenInfo)
        , PD.List [PD.B "step"]
        ]

    phasRedeemer =
      dataToBuiltinData $
        PD.List
          [ PD.B phasRoot
          , PD.B (serialisedOf spentEventId)
          , PD.B (serialisedOf provenInfo)
          , PD.List [PD.B "step"]
          ]

    burnRedeemer =
      dataToBuiltinData (PD.Constr 1 [PD.B (unTokenName nonce), PD.I 0])
      where
        unTokenName (TokenName b) = fromBuiltin b

    -- @payout.MintPayout { withdrawal_utxo_out_ref, withdrawal_input_index,
    -- withdrawal_spend_redeemer_index, hub_ref_input_index }@.
    payoutMintRedeemer =
      dataToBuiltinData $
        PD.Constr
          0
          [ builtinDataToData (toBuiltinData (outRefN (if sMintOutRefMatches sp then 0 else 5)))
          , PD.I 0
          , PD.I 0
          , PD.I 0
          ]

    payoutDatum =
      dataToBuiltinData $
        PD.Constr
          0
          [ maybe (l2ValueWith (sTargetAda sp)) id (sPayoutDatumValue sp)
          , l1AddressData
          , l1DatumData
          ]

    spendRedeemer =
      dataToBuiltinData $
        PD.Constr
          0
          [ PD.I 0 -- input_index
          , PD.I 0 -- output_index
          , PD.I 0 -- hub_ref_input_index
          , PD.I 1 -- settlement_ref_input_index
          , PD.I 0 -- burn_redeemer_index
          , PD.I 1 -- payout_mint_redeemer_index
          , membershipProof
          , PD.I 2
          , sPurpose sp
          ]

    outAddressPolicy
      | isInitialize = if sOutputToPayout sp then payoutPolicy else auxiliaryPolicy
      | otherwise = if sRefundAddressMatches sp then auxiliaryPolicy else payoutPolicy

    base = buildScriptContext mempty
    txInfo =
      (scriptContextTxInfo base)
        { txInfoInputs = [withdrawalIn]
        , txInfoReferenceInputs = [hubSpendRefIn, settlementRefIn]
        , txInfoOutputs =
            [ TxOut
                (scriptHashAddress (ScriptHash (unCurrencySymbol outAddressPolicy)))
                outputValue
                (if isInitialize then OutputDatum (Datum payoutDatum) else NoOutputDatum)
                Nothing
            ]
        , txInfoMint =
            UnsafeMintValue
              ( getValue
                  ( singleton withdrawalPolicy nonce (-1)
                      <> (if isInitialize then singleton payoutPolicy nonce 1 else mempty)
                  )
              )
        , txInfoRedeemers =
            Map.unsafeFromList
              [ (Minting withdrawalPolicy, Redeemer burnRedeemer)
              , (Minting payoutPolicy, Redeemer payoutMintRedeemer)
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
        (SpendingScript (outRefN 0) (Just (Datum withdrawalDatum)))

{- | The verdict recorded in the /datum/.

For @InitializePayout@ it must be the valid one; the refund branch overrides it
from the redeemer, so the datum carries the tree's verdict there too.
-}
treeVerdict :: Spend -> PD.Data
treeVerdict = sTreeValidity

{- | The hub oracle as the spend path reads it: @withdrawal@ (8),
@settlement@ (10), @payout@ (11) and @payout_addr@ (address 12).
-}
hubSpendRefIn :: TxInInfo
hubSpendRefIn =
  TxInInfo
    (outRefN 9)
    ( TxOut
        (scriptHashAddress (ScriptHash (unCurrencySymbol hubOraclePolicy)))
        (mkAdaValue 2_000_000 <> singleton hubOraclePolicy hubAssetName 1)
        (OutputDatum (Datum spendHubDatum))
        Nothing
    )

spendHubDatum :: BuiltinData
spendHubDatum =
  dataToBuiltinData $
    PD.Constr
      0
      ( replicate 8 auxPolicyData
          <> [PD.B (fromBuiltin (unCurrencySymbol withdrawalPolicy))] -- 8: withdrawal
          <> [auxPolicyData]
          <> [PD.B (fromBuiltin (unCurrencySymbol settlementPolicy))] -- 10: settlement
          <> [PD.B (fromBuiltin (unCurrencySymbol payoutPolicy))] -- 11: payout
          <> replicate 8 (addrData auxiliaryPolicy)
          <> [addrData withdrawalPolicy] -- address 8: withdrawal_addr
          <> replicate 3 (addrData auxiliaryPolicy)
          <> [addrData payoutPolicy] -- address 12: payout_addr
          <> [auxPolicyData]
      )
  where
    auxPolicyData = PD.B (fromBuiltin (unCurrencySymbol auxiliaryPolicy))
