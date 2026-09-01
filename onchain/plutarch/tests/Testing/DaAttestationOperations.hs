{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Testing.DaAttestationOperations
Description : Tests for the attestation-lifecycle helpers of
              @validators/da-attestation.ak@.

@validate_add_signatures@ is where the interesting conditions are. The theme is
that accumulation must be **monotonic and attributable**: identity fields and
value cannot move, the new bitmap and count are determined by the signatures
rather than supplied, and the count must strictly increase.

Signatures are real Ed25519 so verification is exercised rather than stubbed,
and the expected bitmap is recomputed in Haskell rather than taken from the
term.
-}
module Testing.DaAttestationOperations (tests) where

import Data.ByteString qualified as BS
import PlutusCore.Data qualified as PD
import PlutusLedgerApi.V1.Address (scriptHashAddress)
import PlutusLedgerApi.V1.Value (CurrencySymbol (..), TokenName (..), Value, getValue, singleton)
import PlutusLedgerApi.V3 (
  Address,
  Datum (..),
  OutputDatum (NoOutputDatum, OutputDatum),
  ScriptHash (..),
  TxOut (..),
  toBuiltinData,
 )
import PlutusLedgerApi.V3.MintValue (MintValue (UnsafeMintValue))
import PlutusTx.Builtins (builtinDataToData, dataToBuiltinData, fromBuiltin, toBuiltin)
import PlutusTx.Builtins qualified as Builtins
import Test.Tasty
import Test.Tasty.HUnit

import Cardano.Crypto.DSIGN.Class qualified as DSIGN
import Cardano.Crypto.DSIGN.Ed25519 (Ed25519DSIGN)
import Cardano.Crypto.Seed (mkSeedFromBytes)

import Plutarch.LedgerApi.V3 (PMintValue, PTokenName (..), PTxOut)
import Plutarch.Prelude
import Plutarch.Unsafe (punsafeCoerce)

import Midgard.DaAttestation (PDaAttestationDatum, PDaParamsDatum)
import Midgard.DaAttestation.Operations (
  pexpectSoleBurn,
  pvalidateAddSignatures,
  pvalidateRescueRefund,
 )
import Testing.Eval (passertEval, pfails)
import Testing.ScriptContextBuilder (currencySymbolFromHex, mkAdaValue)

-- | Collects the tests defined in this module.
tests :: TestTree
tests =
  testGroup
    "DA Attestation Operation Tests"
    [ addSignaturesTests
    , soleBurnTests
    , rescueRefundTests
    ]

--------------------------------------------------------------------------------
-- validate_add_signatures
--------------------------------------------------------------------------------

addSignaturesTests :: TestTree
addSignaturesTests =
  testGroup
    "validateAddSignatures"
    [ testCase "accepts a first signature" $
        holds $ addSigs defaultAdd
    , testCase "accepts two signatures at once" $
        holds $ addSigs (withSigners [0, 1])
    , testCase "da_attestation_add_132_signatures_regression" $
        holds $ addSigs (capacityAdd 132 [0 .. 131])
    , testCase "da_attestation_add_prefix_signatures_regression" $
        holds $ addSigs (capacityAdd 3 [0, 1])
    , testCase "da_attestation_add_sparse_signatures_regression" $
        holds $ addSigs (capacityAdd 3 [0, 2])
    , -- Accumulating onto an attestation that already holds one.
      testCase "accepts a second signature onto an existing bitmap" $
        holds $ addSigs (fromExisting [0] [1])
    , -- The count must be a real advance, or a caller could pay to change
      -- nothing — and, worse, the count could move without new signers.
      testCase "rejects resubmitting a signature already recorded" $
        pfails $ addSigs (fromExisting [0] [0])
    , -- The new state is determined, not supplied.
      testCase "rejects an output bitmap that is not the verified one" $
        pfails $ addSigs defaultAdd {aOutBitmap = Just (bitmapWith [1])}
    , testCase "rejects a count that is not the bitmap's population" $
        pfails $ addSigs defaultAdd {aOutCount = Just 2}
    , -- Identity fields carry over untouched.
      testCase "rejects a changed header hash" $
        pfails $ addSigs defaultAdd {aOutHeaderHash = Just otherHeaderHash}
    , testCase "rejects a changed threshold" $
        pfails $ addSigs defaultAdd {aOutThreshold = Just 3}
    , testCase "rejects a changed committee hash" $
        pfails $ addSigs defaultAdd {aOutCommitteeHash = Just (blake2b256 otherCommittee)}
    , -- A rotation retires an in-progress attestation rather than letting it
      -- continue under keys the protocol no longer trusts.
      testCase "rejects params whose committee has been rotated away" $
        pfails $ addSigs defaultAdd {aParamsCommittee = otherCommittee}
    , -- Value and address cannot move.
      testCase "rejects a changed output value" $
        pfails $ addSigs defaultAdd {aOutExtraAda = True}
    , testCase "rejects an output at another address" $
        pfails $ addSigs defaultAdd {aOutAddress = Just otherAddress}
    , testCase "rejects an output carrying a reference script" $
        pfails $ addSigs defaultAdd {aOutRefScript = True}
    , -- The input must be the attestation the datum describes.
      testCase "rejects an input whose datum differs from the one supplied" $
        pfails $ addSigs defaultAdd {aInDatumMismatch = True}
    , testCase "rejects an input not holding its own attestation token" $
        pfails $ addSigs defaultAdd {aInHasNft = False}
    , -- Witness-blob shape.
      testCase "rejects an empty signature blob" $
        pfails $ addSigs defaultAdd {aSignatureOverride = Just BS.empty}
    , testCase "rejects a truncated final witness" $
        pfails $
          addSigs defaultAdd {aSignatureOverride = Just (BS.take 40 (witness [0]))}
    , -- Real verification, not a stub.
      testCase "rejects a signature made by another key" $
        pfails $ addSigs defaultAdd {aSignatureOverride = Just (witnessWrongKey 0)}
    ]

--------------------------------------------------------------------------------
-- expect_sole_burn
--------------------------------------------------------------------------------

soleBurnTests :: TestTree
soleBurnTests =
  testGroup
    "expectSoleBurn"
    [ testCase "accepts exactly one burn of the named asset" $
        holds $ soleBurn (singleton attestationPolicy attName (-1)) attNameBytes
    , -- Two burns of one name collapse to -2; the quantity check catches it.
      testCase "rejects a double burn of the same asset" $
        pfails $ soleBurn (singleton attestationPolicy attName (-2)) attNameBytes
    , -- Two names produce a second pair; the whole-map check catches that.
      testCase "rejects a second attestation burnt alongside" $
        pfails $
          soleBurn
            (singleton attestationPolicy attName (-1) <> singleton attestationPolicy otherAttName (-1))
            attNameBytes
    , testCase "rejects a mint where a burn is required" $
        pfails $ soleBurn (singleton attestationPolicy attName 1) attNameBytes
    , testCase "rejects a burn of an asset the caller did not name" $
        pfails $ soleBurn (singleton attestationPolicy otherAttName (-1)) attNameBytes
    ]
  where
    soleBurn v asset =
      pexpectSoleBurn
        # mintT v
        # pdata (pconstant attestationPolicy)
        # pdata (pcon (PTokenName (pconstant asset)))

--------------------------------------------------------------------------------
-- validate_rescue_refund
--------------------------------------------------------------------------------

rescueRefundTests :: TestTree
rescueRefundTests =
  testGroup
    "validateRescueRefund"
    [ testCase "accepts the whole value less the burnt token" $
        holds $ refund userAddress (mkAdaValue 2_000_000)
    , -- Returning it to this script would re-strand the Ada: every spend path
      -- needs the token, and the token is being burnt.
      testCase "rejects a refund back to the attestation script" $
        pfails $ refund (addressOf attestationPolicy) (mkAdaValue 2_000_000)
    , testCase "rejects a refund short of the attestation's value" $
        pfails $ refund userAddress (mkAdaValue 1_000_000)
    , testCase "rejects a refund still carrying the burnt token" $
        pfails $
          refund userAddress (mkAdaValue 2_000_000 <> singleton attestationPolicy attName 1)
    ]
  where
    attestationValue = mkAdaValue 2_000_000 <> singleton attestationPolicy attName 1
    refund addr v =
      pvalidateRescueRefund
        (txOutT (TxOut addr v NoOutputDatum Nothing))
        (punsafeCoerce (pconstant @PData (toPD attestationValue)))
        (pdata (pconstant attestationPolicy))
        (pdata (pcon (PTokenName (pconstant attNameBytes))))

--------------------------------------------------------------------------------
-- Assertions
--------------------------------------------------------------------------------

holds :: (forall s. Term s PBool) -> Assertion
holds = passertEval

--------------------------------------------------------------------------------
-- add-signatures fixtures
--------------------------------------------------------------------------------

data Add = Add
  { aCommittee :: BS.ByteString
  , aThreshold :: Integer
  , aInBits :: [Int]
  , aSigners :: [Int]
  , aOutBitmap :: Maybe BS.ByteString
  , aOutCount :: Maybe Integer
  , aOutHeaderHash :: Maybe BS.ByteString
  , aOutThreshold :: Maybe Integer
  , aOutCommitteeHash :: Maybe BS.ByteString
  , aParamsCommittee :: BS.ByteString
  , aOutExtraAda :: Bool
  , aOutAddress :: Maybe Address
  , aOutRefScript :: Bool
  , aInDatumMismatch :: Bool
  , aInHasNft :: Bool
  , aSignatureOverride :: Maybe BS.ByteString
  }

defaultAdd :: Add
defaultAdd =
  Add
    { aCommittee = committee
    , aThreshold = 2
    , aInBits = []
    , aSigners = [0]
    , aOutBitmap = Nothing
    , aOutCount = Nothing
    , aOutHeaderHash = Nothing
    , aOutThreshold = Nothing
    , aOutCommitteeHash = Nothing
    , aParamsCommittee = committee
    , aOutExtraAda = False
    , aOutAddress = Nothing
    , aOutRefScript = False
    , aInDatumMismatch = False
    , aInHasNft = True
    , aSignatureOverride = Nothing
    }

withSigners :: [Int] -> Add
withSigners ss = defaultAdd {aSigners = ss}

fromExisting :: [Int] -> [Int] -> Add
fromExisting existing ss = defaultAdd {aInBits = existing, aSigners = ss}

-- | The Aiken capacity fixtures use an empty input bitmap and a committee whose
-- size and threshold both equal the requested signer count.
capacityAdd :: Int -> [Int] -> Add
capacityAdd committeeSize signers =
  defaultAdd
    { aCommittee = capacityCommittee committeeSize
    , aThreshold = fromIntegral committeeSize
    , aParamsCommittee = capacityCommittee committeeSize
    , aSigners = signers
    }

addSigs :: forall s. Add -> Term s PBool
addSigs a =
  pvalidateAddSignatures
    (attDatumTerm inDatumData)
    (txOutT inOut)
    (txOutT outOut)
    (paramsTerm (aParamsCommittee a) (aThreshold a))
    (pconstant sigs)
  where
    sigs = maybe (witness (aSigners a)) id (aSignatureOverride a)
    inBitmap = bitmapWith (aInBits a)
    outBits = aInBits a <> aSigners a
    committeeHash = blake2b256 (aCommittee a)
    inDatumData = attDatumData headerHash (aThreshold a) committeeHash inBitmap (length (aInBits a))
    outDatumData =
      attDatumData
        (maybe headerHash id (aOutHeaderHash a))
        (maybe (aThreshold a) id (aOutThreshold a))
        (maybe committeeHash id (aOutCommitteeHash a))
        (maybe (bitmapWith outBits) id (aOutBitmap a))
        (maybe (length outBits) fromInteger (aOutCount a))
    attValue =
      mkAdaValue 2_000_000
        <> (if aInHasNft a then singleton attestationPolicy attName 1 else mempty)
    inOut =
      TxOut
        (addressOf attestationPolicy)
        attValue
        ( OutputDatum . Datum . dataToBuiltinData $
            if aInDatumMismatch a
              then attDatumData otherHeaderHash (aThreshold a) committeeHash inBitmap (length (aInBits a))
              else inDatumData
        )
        Nothing
    outOut =
      TxOut
        (maybe (addressOf attestationPolicy) id (aOutAddress a))
        (attValue <> if aOutExtraAda a then mkAdaValue 1_000_000 else mempty)
        (OutputDatum (Datum (dataToBuiltinData outDatumData)))
        (if aOutRefScript a then Just (ScriptHash (unCurrencySymbol otherPolicy)) else Nothing)

attDatumData :: BS.ByteString -> Integer -> BS.ByteString -> BS.ByteString -> Int -> PD.Data
attDatumData hh threshold cHash bitmap count =
  PD.Constr 0 [PD.B hh, PD.I threshold, PD.B cHash, PD.B bitmap, PD.I (fromIntegral count)]

attDatumTerm :: forall s. PD.Data -> Term s PDaAttestationDatum
attDatumTerm d = pfromData (punsafeCoerce (pconstant @PData d))

paramsTerm :: forall s. BS.ByteString -> Integer -> Term s PDaParamsDatum
paramsTerm c threshold = pfromData (punsafeCoerce (pconstant @PData dat))
  where
    dat =
      PD.Constr
        0
        [ PD.B c
        , PD.B (blake2b256 c)
        , PD.I threshold
        , PD.List [PD.B (BS.replicate 28 n) | n <- [1, 2, 3]]
        , PD.I 2
        ]

--------------------------------------------------------------------------------
-- Identities
--------------------------------------------------------------------------------

policyFor :: Int -> CurrencySymbol
policyFor n = currencySymbolFromHex (concat (replicate 28 (hexByte n)))

hexByte :: Int -> String
hexByte x = [d (x `div` 16), d (x `mod` 16)]
  where
    d i = "0123456789abcdef" !! i

attestationPolicy, otherPolicy, userPolicy :: CurrencySymbol
attestationPolicy = policyFor 0x12
otherPolicy = policyFor 0x15
userPolicy = policyFor 0x16

addressOf :: CurrencySymbol -> Address
addressOf cs = scriptHashAddress (ScriptHash (unCurrencySymbol cs))

otherAddress, userAddress :: Address
otherAddress = addressOf otherPolicy
userAddress = addressOf userPolicy

headerHash, otherHeaderHash :: BS.ByteString
headerHash = BS.replicate 28 0xaa
otherHeaderHash = BS.replicate 28 0xbb

attNameBytes, otherAttNameBytes :: BS.ByteString
attNameBytes = "DAAT" <> headerHash
otherAttNameBytes = "DAAT" <> otherHeaderHash

attName, otherAttName :: TokenName
attName = TokenName (toBuiltin attNameBytes)
otherAttName = TokenName (toBuiltin otherAttNameBytes)

committee, otherCommittee :: BS.ByteString
committee = BS.concat (map verKeyFor [0, 1, 2])
otherCommittee = BS.concat (map verKeyFor [7, 8, 9])

capacityCommittee :: Int -> BS.ByteString
capacityCommittee count = BS.concat (map verKeyFor [0 .. count - 1])

blake2b256 :: BS.ByteString -> BS.ByteString
blake2b256 = fromBuiltin . Builtins.blake2b_256 . toBuiltin

bitmapWith :: [Int] -> BS.ByteString
bitmapWith indices = BS.pack [byteAt i | i <- [0 .. 31]]
  where
    byteAt i = sum [2 ^ (7 - (b `mod` 8)) | b <- indices, b `div` 8 == i]

toPD :: Value -> PD.Data
toPD = builtinDataToData . toBuiltinData

txOutT :: forall s. TxOut -> Term s PTxOut
txOutT o = pfromData (punsafeCoerce (pconstant @PData (builtinDataToData (toBuiltinData o))))

mintT :: forall s. Value -> Term s PMintValue
mintT v = pfromData (punsafeCoerce (pconstant @PData (builtinDataToData (toBuiltinData (UnsafeMintValue (getValue v))))))

--------------------------------------------------------------------------------
-- Ed25519
--------------------------------------------------------------------------------

signKeyFor :: Int -> DSIGN.SignKeyDSIGN Ed25519DSIGN
signKeyFor i = DSIGN.genKeyDSIGN (mkSeedFromBytes (BS.replicate 32 (fromIntegral i)))

verKeyFor :: Int -> BS.ByteString
verKeyFor = DSIGN.rawSerialiseVerKeyDSIGN . DSIGN.deriveVerKeyDSIGN . signKeyFor

attestedMessage :: BS.ByteString
attestedMessage = "MidgardDAAttestationV1" <> headerHash

signWith :: Int -> BS.ByteString
signWith i = DSIGN.rawSerialiseSigDSIGN (DSIGN.signDSIGN () attestedMessage (signKeyFor i))

witness :: [Int] -> BS.ByteString
witness indices = BS.concat [BS.cons (fromIntegral i) (signWith i) | i <- indices]

witnessWrongKey :: Int -> BS.ByteString
witnessWrongKey i = BS.cons (fromIntegral i) (signWith (i + 1))
