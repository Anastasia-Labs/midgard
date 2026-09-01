{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Testing.DepositValidator
Description : Behavioural tests for the Plutarch port of the @mint@ side of
              @validators/user-events/deposit.ak@.

The first four cases carry the names and the fixtures of the Aiken @test@ blocks
in that file, so a divergence between the two implementations fails the same
case on each side. The rest cover the checks Aiken has no test block for — the
nonce derivation, the witness registration, and the event address.
-}
module Testing.DepositValidator (tests, nonceFor, witnessScriptHash) where

import Numeric (showHex)

import Data.ByteString qualified as BS
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
  TxCert (TxCertRegStaking, TxCertUnRegStaking),
  Credential (ScriptCredential),
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
import Data.ByteString.Base16 qualified as Base16
import Data.ByteString.Char8 qualified as BS8
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

import Midgard.Validators.Deposit (depositMintValidator, depositSpendValidator)
import Testing.Eval (pfails, psucceeds)
import Testing.ScriptContextBuilder (buildScriptContext, currencySymbolFromHex, mkAdaValue)

-- | Collects the tests defined in this module.
tests :: TestTree
tests =
  testGroup
    "Deposit Validator Tests"
    [ testGroup
        "mint (mirrors the Aiken test blocks)"
        [ testCase "deposit_mint_accepts_exactly_ten_non_nft_assets_across_ada_and_multiple_policies" $
            psucceeds $ runMint (validTo + eventWaitDuration) False
        , testCase "deposit_mint_rejects_eleven_non_nft_assets_across_ada_and_multiple_policies" $
            pfails $ runMint (validTo + eventWaitDuration) True
        , testCase "deposit_mint_rejects_inclusion_time_minus_one" $
            pfails $ runMint (validTo + eventWaitDuration - 1) False
        , testCase "deposit_mint_rejects_inclusion_time_plus_one" $
            pfails $ runMint (validTo + eventWaitDuration + 1) False
        ]
    , testGroup
        "mint (additional)"
        [ testCase "rejects a nonce that is not the spent output reference" $
            pfails $ runMintWith defaults {dNonce = Just (TokenName "deadbeef")}
        , testCase "rejects an event id naming a different output reference" $
            pfails $ runMintWith defaults {dEventId = Just (outRefN 7)}
        , testCase "rejects a witness the datum records incorrectly" $
            pfails $ runMintWith defaults {dDatumWitness = Just (ScriptHash "00")}
        , testCase "rejects an unregistered witness certificate" $
            pfails $ runMintWith defaults {dRegister = False}
        , testCase "rejects a witness redeemer naming another policy" $
            pfails $ runMintWith defaults {dWitnessTargetPolicy = Just auxiliaryPolicy}
        , testCase "rejects an output at an address other than the deposit's" $
            pfails $ runMintWith defaults {dOutputAddressPolicy = Just auxiliaryPolicy}
        , testCase "rejects a deposit UTxO carrying a reference script" $
            pfails $ runMintWith defaults {dReferenceScript = True}
        , testCase "rejects a mint quantity other than one" $
            pfails $ runMintWith defaults {dMintQty = 2}
        ]
    , testGroup
        "spend"
        [ testCase "moves the funds to the reserve once settled" $
            psucceeds $ runSpendWith spendDefaults
        , -- The whole point of the branch: funds may only leave after the L2
          -- ledger has accounted for them.
          testCase "rejects a deposit the settlement root does not contain" $
            pfails $ runSpendWith spendDefaults {sWitnessKey = Just "not-this-deposit"}
        , testCase "rejects a settlement root the proof does not reconstruct" $
            pfails $ runSpendWith spendDefaults {sSettlementRoot = Just (BS.replicate 32 0xbb)}
        , testCase "rejects an output going anywhere but the reserve" $
            pfails $ runSpendWith spendDefaults {sOutputToReserve = False}
        , testCase "rejects an output that keeps the deposit NFT" $
            pfails $ runSpendWith spendDefaults {sBurnNft = False}
        , testCase "rejects an output carrying a datum" $
            pfails $ runSpendWith spendDefaults {sOutputDatum = True}
        , testCase "rejects an output carrying a reference script" $
            pfails $ runSpendWith spendDefaults {sOutputRefScript = True}
        , testCase "rejects a mint redeemer that is not a burn" $
            pfails $ runSpendWith spendDefaults {sBurnRedeemer = False}
        ]
    ]

--------------------------------------------------------------------------------
-- Fixtures, mirroring the Aiken test constants
--------------------------------------------------------------------------------

hubOraclePolicy, depositPolicy, assetPolicyA, assetPolicyB, auxiliaryPolicy :: CurrencySymbol
hubOraclePolicy = repeatedByte 0x11
depositPolicy = repeatedByte 0x22
assetPolicyA = repeatedByte 0x33
assetPolicyB = repeatedByte 0x44
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

{- | @user_events.out_ref_to_nonce@, computed in Haskell.

Deriving it here rather than reusing the Plutarch term is deliberate: the test
must disagree with the validator if either side's serialisation changes.
-}
nonceFor :: TxOutRef -> TokenName
nonceFor = TokenName . blake2b_256 . serialiseData . toBuiltinData

nonce :: TokenName
nonce = nonceFor nonceRef

{- | @parameter_validation.apply_prehashed_param@, computed in Haskell.

The prefix is the 687-byte constant from @env/default.ak@; only its hash with
the nonce appended matters here, and 'witnessScriptHash' recomputes it exactly
as the validator does.
-}
witnessScriptHash :: TokenName -> ScriptHash
witnessScriptHash (TokenName tn) =
  ScriptHash
    ( blake2b_224
        (toBuiltin (BS.singleton (fromIntegral plutusVersion)) <> witnessPrefix <> tn <> postfix)
    )
  where
    postfix = toBuiltin (BS.pack [0x00, 0x01])

{- | @env.user_events_witness_script_prefix@, 687 bytes.

Copied from @env/default.ak@ independently of "Midgard.Env", so a typo on either
side shows up as a failing test rather than as two matching mistakes.
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
-- Transaction assembly
--------------------------------------------------------------------------------

-- | The knobs the negative cases turn; 'defaults' is the accepted transaction.
data Deposit = Deposit
  { dInclusionTime :: Integer
  , dEleventhAsset :: Bool
  , dNonce :: Maybe TokenName
  , dEventId :: Maybe TxOutRef
  , dDatumWitness :: Maybe ScriptHash
  , dRegister :: Bool
  , dWitnessTargetPolicy :: Maybe CurrencySymbol
  , dOutputAddressPolicy :: Maybe CurrencySymbol
  , dReferenceScript :: Bool
  , dMintQty :: Integer
  }

defaults :: Deposit
defaults =
  Deposit
    { dInclusionTime = validTo + eventWaitDuration
    , dEleventhAsset = False
    , dNonce = Nothing
    , dEventId = Nothing
    , dDatumWitness = Nothing
    , dRegister = True
    , dWitnessTargetPolicy = Nothing
    , dOutputAddressPolicy = Nothing
    , dReferenceScript = False
    , dMintQty = 1
    }

runMint :: forall s. Integer -> Bool -> Term s PUnit
runMint inclusionTime eleventh =
  runMintWith defaults {dInclusionTime = inclusionTime, dEleventhAsset = eleventh}

runMintWith :: forall s. Deposit -> Term s PUnit
runMintWith d =
  depositMintValidator
    # pdata (pconstant (ScriptHash (unCurrencySymbol hubOraclePolicy)))
    # pconstant ctx
  where
    usedNonce = maybe nonce id (dNonce d)
    witnessHash = witnessScriptHash usedNonce
    eventId = maybe nonceRef id (dEventId d)
    datumWitness = maybe witnessHash id (dDatumWitness d)
    outAddrPolicy = maybe depositPolicy id (dOutputAddressPolicy d)
    witnessTarget = maybe depositPolicy id (dWitnessTargetPolicy d)

    depositValue =
      foldr
        (\(p, n) v -> v <> singleton p (TokenName n) 1)
        (mkAdaValue 3_000_000)
        ( [(assetPolicyA, n) | n <- ["\x01", "\x02", "\x03", "\x04", "\x05"]]
            <> [(assetPolicyB, n) | n <- ["\x11", "\x12", "\x13", "\x14"]]
            <> [(assetPolicyB, "\x15") | dEleventhAsset d]
        )
        <> singleton depositPolicy usedNonce 1

    depositDatum =
      dataToBuiltinData $
        PD.Constr
          0
          [ PD.Constr -- DepositEvent { id, info }
              0
              [ builtinDataToData (toBuiltinData eventId)
              , PD.Constr 0 [addrData auxiliaryPolicy, PD.I 0, PD.Constr 1 []]
              ]
          , PD.I (dInclusionTime d)
          , PD.B (fromBuiltin (unScriptHash datumWitness))
          ]
      where
        unScriptHash (ScriptHash b) = b

    witnessCert =
      (if dRegister d then TxCertRegStaking else TxCertUnRegStaking)
        (ScriptCredential witnessHash)
        Nothing

    -- @witness.MintOrBurn { target_policy }@ — constructor 0.
    witnessRedeemer =
      dataToBuiltinData
        (PD.Constr 0 [PD.B (fromBuiltin (unCurrencySymbol witnessTarget))])

    -- @user_events.AuthenticateEvent@ — constructor 0.
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
                (scriptHashAddress (ScriptHash (unCurrencySymbol outAddrPolicy)))
                depositValue
                (OutputDatum (Datum depositDatum))
                (if dReferenceScript d then Just (ScriptHash "ab") else Nothing)
            ]
        , txInfoMint = toMint (singleton depositPolicy usedNonce (dMintQty d))
        , txInfoValidRange =
            Interval
              (LowerBound (Finite (POSIXTime 1_000)) True)
              (UpperBound (Finite (POSIXTime validTo)) True)
        , txInfoRedeemers =
            Map.unsafeFromList [(Certifying 0 witnessCert, Redeemer witnessRedeemer)]
        }
    ctx = ScriptContext txInfo (Redeemer mintRedeemer) (MintingScript depositPolicy)

toMint :: Value -> MintValue
toMint = UnsafeMintValue . getValue

addrData :: CurrencySymbol -> PD.Data
addrData cs =
  PD.Constr
    0
    [PD.Constr 1 [PD.B (fromBuiltin (unCurrencySymbol cs))], PD.Constr 1 []]

{- | The hub oracle reference input; only @deposit_addr@ (address 7) is read.

Every other field is the auxiliary policy, matching @test_hub_datum@.
-}
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
      ( replicate 7 auxPolicyData
          <> [PD.B (fromBuiltin (unCurrencySymbol depositPolicy))] -- 7: deposit
          <> replicate 4 auxPolicyData
          <> replicate 7 (addrData auxiliaryPolicy)
          <> [addrData depositPolicy] -- address 7: deposit_addr
          <> replicate 5 (addrData auxiliaryPolicy)
          <> [auxPolicyData]
      )
  where
    auxPolicyData = PD.B (fromBuiltin (unCurrencySymbol auxiliaryPolicy))

--------------------------------------------------------------------------------
-- Spend fixtures
--------------------------------------------------------------------------------

settlementPolicy, reservePolicy :: CurrencySymbol
settlementPolicy = repeatedByte 0x66
reservePolicy = repeatedByte 0x77

-- | @transition_trace.counted_root_tag@.
countedRootTag :: BS.ByteString
countedRootTag = "MidgardRootCountV1"

emptyMerkleRoot :: BS.ByteString
emptyMerkleRoot =
  either (error "bad hex") id (Base16.decode (BS8.pack "0e5751c026e543b2e8ab2eb06099daa1d1e5df47778f7787faab45cdf12fe3a8"))

phasValidatorHash :: BS.ByteString
phasValidatorHash =
  either (error "bad hex") id (Base16.decode (BS8.pack "1fc59ff54da02f2535d64b40b647a8826c8b3d914d7ba5257f5b2721"))

{- | @commit_counted_root@ for the deposits domain (tag 3), recomputed here.

The settlement's datum records this value; the prover then supplies a witness
that must reconstruct it.
-}
depositsCountedRoot :: BS.ByteString
depositsCountedRoot =
  fromBuiltin $
    blake2b_256
      ( toBuiltin countedRootTag
          <> serialiseData (dataToBuiltinData (PD.Constr 3 []))
          <> toBuiltin phasRoot
          <> serialiseData (dataToBuiltinData (PD.I depositCount))
      )

phasRoot :: BS.ByteString
phasRoot = BS.replicate 32 0xaa

depositCount :: Integer
depositCount = 5

-- | The deposit event's id and info, as the counted tree stores them.
spentEventId, spentEventInfo :: PD.Data
spentEventId = builtinDataToData (toBuiltinData nonceRef)
spentEventInfo = PD.Constr 0 [addrData auxiliaryPolicy, PD.I 0, PD.Constr 1 []]

serialisedOf :: PD.Data -> BS.ByteString
serialisedOf = fromBuiltin . serialiseData . dataToBuiltinData

-- | The knobs the spend negatives turn.
data Spend = Spend
  { sWitnessKey :: Maybe BS.ByteString
  , sSettlementRoot :: Maybe BS.ByteString
  , sOutputToReserve :: Bool
  , sBurnNft :: Bool
  , sOutputDatum :: Bool
  , sOutputRefScript :: Bool
  , sBurnRedeemer :: Bool
  }

spendDefaults :: Spend
spendDefaults =
  Spend
    { sWitnessKey = Nothing
    , sSettlementRoot = Nothing
    , sOutputToReserve = True
    , sBurnNft = True
    , sOutputDatum = False
    , sOutputRefScript = False
    , sBurnRedeemer = True
    }

{- | A deposit being spent into the reserve.

Inputs: the deposit UTxO at index 0. Reference inputs: the hub oracle at 0 and
the settlement at 1. Redeemers: the deposit policy's burn at 0 and the @phas@
withdrawal at 1.
-}
runSpendWith :: forall s. Spend -> Term s PUnit
runSpendWith sp =
  depositSpendValidator
    # pdata (pconstant (ScriptHash (unCurrencySymbol hubOraclePolicy)))
    # pconstant ctx
  where
    depositUtxoValue = mkAdaValue 3_000_000 <> singleton depositPolicy nonce 1
    outputValue =
      if sBurnNft sp
        then mkAdaValue 3_000_000
        else mkAdaValue 3_000_000 <> singleton depositPolicy nonce 1

    depositDatum =
      dataToBuiltinData $
        PD.Constr
          0
          [PD.Constr 0 [spentEventId, spentEventInfo], PD.I 0, PD.B "witness"]

    depositIn =
      TxInInfo
        (outRefN 0)
        ( TxOut
            (scriptHashAddress (ScriptHash (unCurrencySymbol depositPolicy)))
            depositUtxoValue
            (OutputDatum (Datum depositDatum))
            Nothing
        )

    settlementRoot = maybe depositsCountedRoot id (sSettlementRoot sp)

    settlementDatum =
      dataToBuiltinData $
        PD.Constr
          0
          [ PD.B settlementRoot -- deposits_root
          , PD.B emptyMerkleRoot
          , PD.B emptyMerkleRoot
          , PD.B emptyMerkleRoot
          , PD.Constr 1 [] -- no resolution claim
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

    witnessKey = maybe (serialisedOf spentEventId) id (sWitnessKey sp)
    witnessValue = serialisedOf spentEventInfo

    -- @RootMembershipProof@; the phas withdrawal must echo its arguments.
    membershipProof =
      PD.Constr
        0
        [ PD.Constr 3 [] -- DepositsRootDomain
        , PD.B settlementRoot
        , PD.B phasRoot
        , PD.I depositCount
        , PD.B witnessKey
        , PD.B witnessValue
        , PD.List [PD.B "step"]
        ]

    phasRedeemer =
      dataToBuiltinData $
        PD.List
          [ PD.B phasRoot
          , PD.B (serialisedOf spentEventId)
          , PD.B witnessValue
          , PD.List [PD.B "step"]
          ]

    burnRedeemer =
      if sBurnRedeemer sp
        then -- @BurnEventNFT { nonce_asset_name, .. }@ — constructor 1.
          dataToBuiltinData (PD.Constr 1 [PD.B (unTokenName nonce), PD.I 0])
        else dataToBuiltinData (PD.Constr 0 [PD.I 0, PD.I 0, PD.I 0, PD.I 0])
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
          , PD.I 0 -- mint_redeemer_index
          , membershipProof
          , PD.I 1
          ]

    base = buildScriptContext mempty
    txInfo =
      (scriptContextTxInfo base)
        { txInfoInputs = [depositIn]
        , txInfoReferenceInputs = [hubSpendRefIn, settlementRefIn]
        , txInfoOutputs =
            [ TxOut
                ( scriptHashAddress
                    ( ScriptHash
                        ( unCurrencySymbol
                            (if sOutputToReserve sp then reservePolicy else auxiliaryPolicy)
                        )
                    )
                )
                outputValue
                (if sOutputDatum sp then OutputDatum (Datum (dataToBuiltinData (PD.I 1))) else NoOutputDatum)
                (if sOutputRefScript sp then Just (ScriptHash "ab") else Nothing)
            ]
        , txInfoMint = toMint (singleton depositPolicy nonce (-1))
        , txInfoRedeemers =
            Map.unsafeFromList
              [ (Minting depositPolicy, Redeemer burnRedeemer)
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
        (SpendingScript (outRefN 0) (Just (Datum depositDatum)))

{- | The hub oracle as the spend path reads it: @deposit@ (0), @settlement@ (10)
and @reserve_addr@ (address 11) all matter here.
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
      ( replicate 7 auxPolicyData
          <> [PD.B (fromBuiltin (unCurrencySymbol depositPolicy))] -- 7: deposit
          <> replicate 2 auxPolicyData
          <> [PD.B (fromBuiltin (unCurrencySymbol settlementPolicy))] -- 10: settlement
          <> [auxPolicyData]
          <> replicate 7 (addrData auxiliaryPolicy)
          <> [addrData depositPolicy] -- address 7: deposit_addr
          <> replicate 3 (addrData auxiliaryPolicy)
          <> [addrData reservePolicy] -- address 11: reserve_addr
          <> [addrData auxiliaryPolicy]
          <> [auxPolicyData]
      )
  where
    auxPolicyData = PD.B (fromBuiltin (unCurrencySymbol auxiliaryPolicy))
