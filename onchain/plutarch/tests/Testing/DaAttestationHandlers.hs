{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Testing.DaAttestationHandlers
Description : Tests for the dispatch wiring of @validators/da-attestation.ak@.

The three layers beneath these handlers are covered separately. What is tested
here is the wiring: that a burn-side spend binds to the /right/ mint redeemer
and to /this/ input.

@validate_burn_binding@ is the piece worth testing first. Two spend redeemers
carry the same single field and differ only in which mint constructor may
satisfy them, so nothing about their shape stops one being used for the other —
only the @expect_rescue@ flag does. If that selection were dropped, an
attestation could be destroyed under conditions neither branch actually checked:
a @BurnForStateQueue@ satisfied by a rescue authorisation would burn an
attestation without attaching anything to the state queue, and a
@BurnForRescue@ satisfied by an apply would refund one without proving it was
stranded.
-}
module Testing.DaAttestationHandlers (tests) where

import Data.ByteString qualified as BS
import PlutusCore.Data qualified as PD
import PlutusLedgerApi.V1.Address (pubKeyHashAddress, scriptHashAddress)
import PlutusLedgerApi.V1.Interval (interval)
import PlutusLedgerApi.V1.Value (CurrencySymbol (..), TokenName (..), Value, getValue, singleton)
import PlutusLedgerApi.V3 (
  Address,
  Datum (..),
  OutputDatum (NoOutputDatum, OutputDatum),
  POSIXTime (..),
  PubKeyHash (..),
  Redeemer (..),
  ScriptContext (..),
  ScriptHash (..),
  ScriptInfo (MintingScript, SpendingScript),
  ScriptPurpose (Minting),
  TxId (..),
  TxInInfo (..),
  TxInfo,
  TxOut (..),
  TxOutRef (..),
  ToData,
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
import PlutusTx.Builtins (BuiltinData, builtinDataToData, dataToBuiltinData, fromBuiltin, serialiseData, toBuiltin)
import PlutusTx.Builtins qualified as Builtins
import Test.Tasty
import Test.Tasty.HUnit

import Plutarch.LedgerApi.V3 (PRedeemer, PScriptPurpose, PTxInInfo)
import Plutarch.Prelude
import Plutarch.Unsafe (punsafeCoerce)

import Midgard.AvailabilityChallenge (PParametersV1)
import Midgard.Validators.DaAttestation (
  daAttestationMintValidator,
  daAttestationSpendValidator,
  pvalidateBurnBinding,
 )
import Testing.Eval (passertEval, pfails, psucceeds)
import Testing.ScriptContextBuilder (buildScriptContext, currencySymbolFromHex, mkAdaValue)

-- | Collects the tests defined in this module.
tests :: TestTree
tests =
  testGroup
    "DA Attestation Handler Tests"
    [ applyTests
    , rescueTests
    , testGroup
        "validateBurnBinding"
        [ testCase "a state-queue burn binds to an apply redeemer" $
            holds $ bind Apply False 0
        , testCase "a rescue burn binds to a rescue redeemer" $
            holds $ bind Rescue True 0
        , -- The cross pair. Nothing about the two spend redeemers' shape keeps
          -- them apart; only the expect_rescue flag does.
          testCase "a state-queue burn rejects a rescue authorisation" $
            pfails $ bind Rescue False 0
        , testCase "a rescue burn rejects an apply authorisation" $
            pfails $ bind Apply True 0
        , -- A burn deferring to Init would authorise destruction with a
          -- creation.
          testCase "rejects a burn deferring to Init" $
            pfails $ bind InitR False 0
        , testCase "rejects a rescue burn deferring to Init" $
            pfails $ bind InitR True 0
        , -- The mint redeemer must name this very input, not merely be of the
          -- right kind: otherwise one authorisation could cover a different
          -- attestation spent in the same transaction.
          testCase "rejects a mint redeemer naming another input" $
            pfails $ bind Apply False 1
        , testCase "rejects a redeemer index that is not the policy's" $
            pfails $ bindAt 1 Apply False 0
        ]
    ]

applyTests :: TestTree
applyTests =
  testGroup
    "apply"
    [ testCase "da_attestation_apply_control_unchanged_committee_applies" $
        psucceeds $ runApplyMint defaultApply
    , testCase "da_attestation_apply_accepts_window_ending_at_timeout_boundary" $
        psucceeds $ runApplyMint defaultApply {aValidityUpper = 3_602_000}
    , testCase "da_attestation_apply_rejects_window_ending_after_timeout" $
        pfails $ runApplyMint defaultApply {aValidityUpper = 3_602_001}
    , testCase "da_attestation_apply_rejects_rotated_committee" $
        pfails $ runApplyMint defaultApply {aParamsCommittee = committeeB}
    , testCase "da_attestation_apply_rejects_governed_threshold_change" $
        pfails $ runApplyMint defaultApply {aParamsThreshold = 3}
    , testCase "da_attestation_apply_rejects_forged_committee_hash" $
        pfails $
          runApplyMint
            defaultApply
              { aParamsCommittee = committeeB
              , aParamsHashOverride = Just (blake2b256 committeeA)
              }
    , testCase "da_attestation_apply_rejects_stale_params_reference_input" $
        pfails $ runApplyMint defaultApply {aParamsHasNft = False}
    , testCase "da_attestation_apply_rejects_old_committee_quorum" $
        pfails $
          runApplyMint defaultApply {aParamsCommittee = committeeB, aParamsThreshold = 1}
    ]

data ApplyCase = ApplyCase
  { aParamsCommittee :: BS.ByteString
  , aParamsThreshold :: Integer
  , aParamsHashOverride :: Maybe BS.ByteString
  , aParamsHasNft :: Bool
  , aValidityUpper :: Integer
  }

defaultApply :: ApplyCase
defaultApply =
  ApplyCase
    { aParamsCommittee = committeeA
    , aParamsThreshold = 2
    , aParamsHashOverride = Nothing
    , aParamsHasNft = True
    , aValidityUpper = 3_602_000
    }

runApplyMint :: forall s. ApplyCase -> Term s PUnit
runApplyMint applyCase =
  daAttestationMintValidator
    # pdata (pconstant paramsPolicy)
    # pdata (pconstant authPolicy)
    # pdata (pconstant availabilityPolicy)
    # pdata availabilityParametersTerm
    # pconstant ctx
  where
    ctx =
      ScriptContext
        (applyTxInfo applyCase)
        (Redeemer applyMintRedeemer)
        (MintingScript attestationPolicy)

applyTxInfo :: ApplyCase -> TxInfo
applyTxInfo applyCase =
  (scriptContextTxInfo (buildScriptContext mempty))
    { txInfoInputs =
        [ attestationInputFor applyAttestationAsset attestationRef applyAttestationDatum
        , TxInInfo stateQueueRef (stateQueueOutput (PD.Constr 0 []))
        ]
    , txInfoReferenceInputs = [applyParamsInput applyCase, stateQueueRefScriptInput]
    , txInfoOutputs = [stateQueueOutput (PD.Constr 1 [tokenNameData daBondAsset])]
    , txInfoMint =
        toMint $
          singleton attestationPolicy applyAttestationAsset (-1)
            <> singleton availabilityPolicy daBondAsset 1
    , txInfoRedeemers =
        Map.unsafeFromList
          [ (Minting attestationPolicy, Redeemer applyMintRedeemer)
          , (Minting availabilityPolicy, Redeemer availabilityMintRedeemer)
          ]
    , txInfoValidRange = interval (POSIXTime 3_542_000) (POSIXTime (aValidityUpper applyCase))
    }

applyParamsInput :: ApplyCase -> TxInInfo
applyParamsInput applyCase =
  TxInInfo
    paramsRef
    ( TxOut
        (addressOf paramsPolicy)
        ( mkAdaValue 2_000_000
            <> if aParamsHasNft applyCase
              then singleton paramsPolicy daParamsAsset 1
              else mempty
        )
        (OutputDatum (Datum (dataToBuiltinData datum)))
        Nothing
    )
  where
    committee = aParamsCommittee applyCase
    datum =
      PD.Constr
        0
        [ PD.B committee
        , PD.B (maybe (blake2b256 committee) id (aParamsHashOverride applyCase))
        , PD.I (aParamsThreshold applyCase)
        , PD.List [PD.B (BS.replicate 28 n) | n <- [1, 2]]
        , PD.I 2
        ]

stateQueueRefScriptInput :: TxInInfo
stateQueueRefScriptInput =
  TxInInfo
    refScriptRef
    ( TxOut
        (addressOf authPolicy)
        (mkAdaValue 2_000_000 <> singleton authPolicy (TokenName "StateQueueMint") 1)
        NoOutputDatum
        (Just (ScriptHash (unCurrencySymbol stateQueuePolicy)))
    )

stateQueueOutput :: PD.Data -> TxOut
stateQueueOutput status =
  TxOut
    (addressOf stateQueuePolicy)
    (mkAdaValue 2_000_000 <> singleton stateQueuePolicy stateQueueAsset 1)
    (OutputDatum (Datum (dataToBuiltinData datum)))
    Nothing
  where
    datum =
      PD.Constr
        0
        [PD.Constr 1 [PD.Constr 0 [applyHeaderData, status]], PD.Constr 1 []]

attestationInputFor :: TokenName -> TxOutRef -> PD.Data -> TxInInfo
attestationInputFor asset ref datum =
  TxInInfo
    ref
    ( TxOut
        (addressOf attestationPolicy)
        (mkAdaValue attestationLovelace <> singleton attestationPolicy asset 1)
        (OutputDatum (Datum (dataToBuiltinData datum)))
        Nothing
    )

applyAttestationDatum :: PD.Data
applyAttestationDatum =
  PD.Constr
    0
    [ PD.B applyHeaderHash
    , availabilityCommitmentDataFor applyHeaderHash
    , PD.I 2
    , PD.B (blake2b256 committeeA)
    , builtinDataToData (toBuiltinData refundAddress)
    , PD.B (bitmapWith 2)
    , PD.I 2
    ]

applyHeaderData :: PD.Data
applyHeaderData =
  PD.Constr
    0
    ( replicate 9 (PD.B (BS.replicate 32 0x10))
        <> map PD.I [1, 1, 1, 1, 4, 4, 2, 1_000, 2_000, 0, 0, 0, 0]
        <> [PD.B (BS.replicate 28 0xaa), PD.B (BS.replicate 28 0xaa), PD.I 1]
    )

applyHeaderHash :: BS.ByteString
applyHeaderHash = hashData224 applyHeaderData

applyAttestationAsset, daBondAsset, stateQueueAsset :: TokenName
applyAttestationAsset = TokenName (toBuiltin ("DAAT" <> applyHeaderHash))
daBondAsset =
  TokenName (toBuiltin ("DABN" <> hashData224 (builtinDataToData (toBuiltinData attestationRef))))
stateQueueAsset = TokenName (toBuiltin ("MBLC" <> applyHeaderHash))

applyMintRedeemer, availabilityMintRedeemer :: BuiltinData
applyMintRedeemer =
  dataToBuiltinData (PD.Constr 1 [PD.I 0, PD.I 0, PD.I 1, PD.I 0, PD.I 1, PD.I 1])
availabilityMintRedeemer =
  dataToBuiltinData (PD.Constr 0 [PD.I 0, PD.I 2, PD.I 0, PD.I 0, PD.I 1, PD.I 1, PD.I 0])

tokenNameData :: TokenName -> PD.Data
tokenNameData (TokenName bytes) = PD.B (fromBuiltin bytes)

hashData224 :: PD.Data -> BS.ByteString
hashData224 = fromBuiltin . Builtins.blake2b_224 . serialiseData . dataToBuiltinData

rescueTests :: TestTree
rescueTests =
  testGroup
    "rescue"
    [ testCase "da_attestation_rescue_control_rotated_committee_refunds_partial_attestation" $
        psucceeds $
          plet (runRescueMint defaultRescue) $ \_ ->
            plet (runRescueSpend defaultRescue attestationRef) $ \_ ->
              runRescueSpend defaultRescue {rWithSecondAttestation = True} attestationRef
    , testCase "da_attestation_rescue_rejects_unrotated_committee_attestation" $
        pfails $ runRescueMint defaultRescue {rParamsCommittee = committeeA}
    , testCase "da_attestation_rescue_rejects_duplicate_burn_quantity" $
        pfails $ runRescueMint defaultRescue {rBurnQuantity = -2}
    , testCase "da_attestation_rescue_rejects_replayed_mint_binding" $
        pfails $
          runRescueSpend defaultRescue {rWithSecondAttestation = True} secondAttestationRef
    , testCase "da_attestation_rescue_control_threshold_change_refunds_quorum_attestation" $
        psucceeds $
          plet (runRescueMint thresholdChangedRescue) $ \_ ->
            runRescueSpend thresholdChangedRescue attestationRef
    , testCase "da_attestation_rescue_rejects_refund_short_of_attestation_value" $
        pfails $ runRescueMint defaultRescue {rRefundLovelace = attestationLovelace - 1}
    , testCase "da_attestation_rescue_rejects_redirected_beneficiary" $
        pfails $ runRescueMint defaultRescue {rRefundAddress = otherRefundAddress}
    ]

data RescueCase = RescueCase
  { rAttestationCommittee :: BS.ByteString
  , rAttestationThreshold :: Integer
  , rAttestationCount :: Int
  , rParamsCommittee :: BS.ByteString
  , rParamsThreshold :: Integer
  , rBurnQuantity :: Integer
  , rRefundLovelace :: Int
  , rWithSecondAttestation :: Bool
  , rRefundAddress :: Address
  }

defaultRescue :: RescueCase
defaultRescue =
  RescueCase
    { rAttestationCommittee = committeeA
    , rAttestationThreshold = 2
    , rAttestationCount = 1
    , rParamsCommittee = committeeB
    , rParamsThreshold = 2
    , rBurnQuantity = -1
    , rRefundLovelace = attestationLovelace
    , rWithSecondAttestation = False
    , rRefundAddress = refundAddress
    }

thresholdChangedRescue :: RescueCase
thresholdChangedRescue =
  defaultRescue
    { rAttestationCount = 2
    , rParamsCommittee = committeeA
    , rParamsThreshold = 3
    }

runRescueMint :: forall s. RescueCase -> Term s PUnit
runRescueMint rescue =
  daAttestationMintValidator
    # pdata (pconstant paramsPolicy)
    # pdata (pconstant authPolicy)
    # pdata (pconstant availabilityPolicy)
    # pdata availabilityParametersTerm
    # pconstant ctx
  where
    ctx =
      ScriptContext
        (rescueTxInfo rescue)
        (Redeemer rescueMintRedeemer)
        (MintingScript attestationPolicy)

runRescueSpend :: forall s. RescueCase -> TxOutRef -> Term s PUnit
runRescueSpend rescue ownRef =
  daAttestationSpendValidator
    # pdata (pconstant paramsPolicy)
    # pconstant ctx
  where
    ctx =
      ScriptContext
        (rescueTxInfo rescue)
        (Redeemer rescueSpendRedeemer)
        (SpendingScript ownRef Nothing)

rescueTxInfo :: RescueCase -> TxInfo
rescueTxInfo rescue =
  (scriptContextTxInfo (buildScriptContext mempty))
    { txInfoInputs =
        attestationInput attestationRef attestationDatum
          : [attestationInput secondAttestationRef attestationDatum | rWithSecondAttestation rescue]
    , txInfoReferenceInputs = [paramsInput (rParamsCommittee rescue) (rParamsThreshold rescue)]
    , txInfoOutputs =
        [TxOut (rRefundAddress rescue) (mkAdaValue (rRefundLovelace rescue)) NoOutputDatum Nothing]
    , txInfoMint = toMint (singleton attestationPolicy attestationAsset (rBurnQuantity rescue))
    , txInfoRedeemers =
        Map.unsafeFromList [(Minting attestationPolicy, Redeemer rescueMintRedeemer)]
    }
  where
    attestationDatum =
      attestationDatumData
        (rAttestationCommittee rescue)
        (rAttestationThreshold rescue)
        (rAttestationCount rescue)

attestationInput :: TxOutRef -> PD.Data -> TxInInfo
attestationInput ref datum =
  TxInInfo
    ref
    ( TxOut
        (addressOf attestationPolicy)
        attestationValue
        (OutputDatum (Datum (dataToBuiltinData datum)))
        Nothing
    )

paramsInput :: BS.ByteString -> Integer -> TxInInfo
paramsInput committee threshold =
  TxInInfo
    paramsRef
    ( TxOut
        (addressOf paramsPolicy)
        (mkAdaValue 2_000_000 <> singleton paramsPolicy daParamsAsset 1)
        (OutputDatum (Datum (dataToBuiltinData datum)))
        Nothing
    )
  where
    datum =
      PD.Constr
        0
        [ PD.B committee
        , PD.B (blake2b256 committee)
        , PD.I threshold
        , PD.List [PD.B (BS.replicate 28 n) | n <- [1, 2]]
        , PD.I 2
        ]

attestationDatumData :: BS.ByteString -> Integer -> Int -> PD.Data
attestationDatumData committee threshold count =
  PD.Constr
    0
    [ PD.B headerHash
    , availabilityCommitmentData
    , PD.I threshold
    , PD.B (blake2b256 committee)
    , builtinDataToData (toBuiltinData refundAddress)
    , PD.B (bitmapWith count)
    , PD.I (fromIntegral count)
    ]

availabilityParametersTerm :: forall s. Term s PParametersV1
availabilityParametersTerm =
  pfromData (punsafeCoerce (pconstant @PData availabilityParametersData))

availabilityParametersData :: PD.Data
availabilityParametersData =
  PD.Constr
    0
    [ responseGeometryData
    , PD.I (fromIntegral attestationLovelace)
    , PD.I (fromIntegral attestationLovelace)
    , PD.I 1
    , PD.I 1
    , PD.I 1
    , PD.I 1
    , PD.I 1
    ]

availabilityCommitmentData :: PD.Data
availabilityCommitmentData = availabilityCommitmentDataFor headerHash

availabilityCommitmentDataFor :: BS.ByteString -> PD.Data
availabilityCommitmentDataFor commitmentHeaderHash =
  PD.Constr
    0
    [ PD.I 1
    , PD.B (BS.replicate 28 0x31)
    , PD.B commitmentHeaderHash
    , PD.I 1
    , responseGeometryData
    , PD.List
        [ PD.Constr
            0
            [ PD.I 0
            , PD.I 0
            , PD.I 1
            , PD.I 1
            , PD.B (BS.replicate 32 0xac)
            , PD.B (BS.replicate 32 0xab)
            ]
        ]
    , PD.B (BS.replicate 28 0x41)
    ]

responseGeometryData :: PD.Data
responseGeometryData = PD.Constr 0 [PD.I 4_096, PD.I (4 * 1024 * 1024), PD.I 16]

bitmapWith :: Int -> BS.ByteString
bitmapWith count = BS.pack (firstByte : replicate 31 0)
  where
    firstByte = [0, 128, 192, 224, 240, 248, 252, 254, 255] !! count

blake2b256 :: BS.ByteString -> BS.ByteString
blake2b256 = fromBuiltin . Builtins.blake2b_256 . toBuiltin

toMint :: Value -> MintValue
toMint = UnsafeMintValue . getValue

--------------------------------------------------------------------------------
-- Fixtures
--------------------------------------------------------------------------------

data Kind = Apply | Rescue | InitR

holds :: (forall s. Term s PBool) -> Assertion
holds = passertEval

{- | Two attestation inputs are present, so "names this input" is a real
question rather than one there is only one answer to.
-}
bind :: forall s. Kind -> Bool -> Integer -> Term s PBool
bind = bindAt 0

bindAt :: forall s. Integer -> Kind -> Bool -> Integer -> Term s PBool
bindAt redeemerIndex kind expectRescue boundIndex =
  pvalidateBurnBinding
    (inputsT [attestationIn ownRef, attestationIn otherRef])
    (redeemersT [(Minting attestationPolicy, Redeemer (dataToBuiltinData mintRedeemer))])
    (pconstant ownRef)
    (pconstant redeemerIndex)
    (pconstant expectRescue)
  where
    mintRedeemer = case kind of
      -- Init: four fields, tag 0.
      InitR -> PD.Constr 0 [PD.I 0, PD.I 0, PD.I 0, PD.I 0]
      -- ApplyToStateQueue: six fields, tag 1; the first is the bound input.
      Apply -> PD.Constr 1 [PD.I boundIndex, PD.I 0, PD.I 0, PD.I 0, PD.I 0, PD.I 0]
      -- RescueStrandedAttestation: three fields, tag 2.
      Rescue -> PD.Constr 2 [PD.I boundIndex, PD.I 0, PD.I 0]

attestationIn :: TxOutRef -> TxInInfo
attestationIn ref =
  TxInInfo
    ref
    ( TxOut
        (addressOf attestationPolicy)
        (mkAdaValue 2_000_000 <> singleton attestationPolicy attName 1)
        NoOutputDatum
        Nothing
    )

--------------------------------------------------------------------------------
-- Identities and plumbing
--------------------------------------------------------------------------------

policyFor :: Int -> CurrencySymbol
policyFor n = currencySymbolFromHex (concat (replicate 28 (hexByte n)))

hexByte :: Int -> String
hexByte x = [d (x `div` 16), d (x `mod` 16)]
  where
    d i = "0123456789abcdef" !! i

attestationPolicy, paramsPolicy, authPolicy, availabilityPolicy, stateQueuePolicy :: CurrencySymbol
attestationPolicy = policyFor 0x12
paramsPolicy = policyFor 0x11
authPolicy = policyFor 0x13
availabilityPolicy = policyFor 0x14
stateQueuePolicy = policyFor 0x15

addressOf :: CurrencySymbol -> Address
addressOf cs = scriptHashAddress (ScriptHash (unCurrencySymbol cs))

attName :: TokenName
attName = TokenName (toBuiltin ("DAAT" <> BS.replicate 28 0xaa))

daParamsAsset, attestationAsset :: TokenName
daParamsAsset = TokenName "MIDGARD_DA_PARAMS"
attestationAsset = attName

headerHash :: BS.ByteString
headerHash = BS.replicate 28 0xaa

committeeA, committeeB :: BS.ByteString
committeeA = BS.replicate 32 0xa1 <> BS.replicate 32 0xa2
committeeB = BS.replicate 32 0xb1 <> BS.replicate 32 0xb2

refundAddress, otherRefundAddress :: Address
refundAddress = pubKeyHashAddress (PubKeyHash (toBuiltin (BS.replicate 28 0x12)))
otherRefundAddress = pubKeyHashAddress (PubKeyHash (toBuiltin (BS.replicate 28 0x08)))

attestationLovelace :: Int
attestationLovelace = 2_000_000

attestationValue :: Value
attestationValue =
  mkAdaValue attestationLovelace <> singleton attestationPolicy attestationAsset 1

rescueMintRedeemer, rescueSpendRedeemer :: BuiltinData
rescueMintRedeemer = dataToBuiltinData (PD.Constr 2 [PD.I 0, PD.I 0, PD.I 0])
rescueSpendRedeemer = dataToBuiltinData (PD.Constr 2 [PD.I 0])

ownRef, otherRef :: TxOutRef
ownRef = TxOutRef (TxId "0101010101010101010101010101010101010101010101010101010101010101") 0
otherRef = TxOutRef (TxId "0202020202020202020202020202020202020202020202020202020202020202") 0

attestationRef, secondAttestationRef, paramsRef, stateQueueRef, refScriptRef :: TxOutRef
attestationRef = ownRef
secondAttestationRef = otherRef
paramsRef = TxOutRef (TxId "0303030303030303030303030303030303030303030303030303030303030303") 0
stateQueueRef = TxOutRef (TxId "0404040404040404040404040404040404040404040404040404040404040404") 0
refScriptRef = TxOutRef (TxId "0505050505050505050505050505050505050505050505050505050505050505") 0

inputsT :: forall s. [TxInInfo] -> Term s (PBuiltinList (PAsData PTxInInfo))
inputsT xs = punsafeCoerce (pasList # pconstant @PData (PD.List (map toPD xs)))

redeemersT ::
  forall s.
  [(ScriptPurpose, Redeemer)] ->
  Term s (PBuiltinList (PBuiltinPair (PAsData PScriptPurpose) (PAsData PRedeemer)))
redeemersT entries =
  punsafeCoerce
    ( pasMap
        #$ pconstant @PData
          (PD.Map [(toPD p, builtinDataToData (getRedeemer r)) | (p, r) <- entries])
    )

toPD :: ToData a => a -> PD.Data
toPD = builtinDataToData . toBuiltinData
