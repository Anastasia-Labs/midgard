{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Testing.DaAttestationReaders
Description : Tests for the authenticated-read layer of
              @validators/da-attestation.ak@.

The case worth the most here is the last one in @get_da_params@: a params datum
whose @committee_signers_hash@ does not hash its own @committee@ must be
refused. The Aiken source records that this was a real gap — until it was added,
every consumer compared its frozen hash against a field nothing re-derived, so a
rotated committee published under the pre-rotation hash would satisfy each such
comparison while signatures verified against the new keys.
-}
module Testing.DaAttestationReaders (tests) where

import Data.ByteString qualified as BS
import PlutusCore.Data qualified as PD
import PlutusLedgerApi.V1.Address (scriptHashAddress)
import PlutusLedgerApi.V1.Value (CurrencySymbol (..), TokenName (..), Value, singleton)
import PlutusLedgerApi.V3 (
  Address,
  Datum (..),
  OutputDatum (NoOutputDatum, OutputDatum),
  ScriptHash (..),
  TxId (..),
  TxInInfo (..),
  TxOut (..),
  TxOutRef (..),
  toBuiltinData,
 )
import PlutusTx.Builtins (builtinDataToData, dataToBuiltinData, fromBuiltin, toBuiltin)
import PlutusTx.Builtins qualified as Builtins
import Test.Tasty
import Test.Tasty.HUnit

import Plutarch.LedgerApi.V3 (PCurrencySymbol, PTokenName (..), PTxInInfo, PTxOut)
import Plutarch.Prelude
import Plutarch.Unsafe (punsafeCoerce)

import Midgard.DaAttestation (PDaParamsDatum)
import Midgard.DaAttestation.Readers (
  pgetAuthenticatedStateQueuePolicyId,
  pgetDaParams,
  pvalidateInitOutput,
 )
import Testing.Eval (passertEval, pfails)
import Testing.ScriptContextBuilder (currencySymbolFromHex, mkAdaValue)

-- | Collects the tests defined in this module.
tests :: TestTree
tests =
  testGroup
    "DA Attestation Reader Tests"
    [ paramsTests
    , refScriptTests
    , initOutputTests
    ]

--------------------------------------------------------------------------------
-- get_da_params
--------------------------------------------------------------------------------

paramsTests :: TestTree
paramsTests =
  testGroup
    "getDaParams"
    [ testCase "reads an authentic params UTxO" $
        holds $ (getParams defaultParams) #== paramsDatumTerm committee 2
    , -- The property this layer exists for: the hash is re-derived, not
      -- believed. A rotated committee published under the old hash would
      -- otherwise satisfy every downstream frozen-hash comparison.
      testCase "rejects a committee that does not hash to its stated hash" $
        pfails $ getParams defaultParams {dpHashOverride = Just (blake2b256 otherCommittee)}
    , testCase "rejects a rotated committee kept under the previous hash" $
        pfails $
          getParams
            defaultParams
              { dpCommittee = otherCommittee
              , dpHashOverride = Just (blake2b256 committee)
              }
    , testCase "accepts a rotated committee under its own hash" $
        holds $
          (getParams defaultParams {dpCommittee = otherCommittee})
            #== paramsDatumTerm otherCommittee 2
    , -- Authentication of the UTxO itself.
      testCase "rejects a params UTxO at another address" $
        pfails $ getParams defaultParams {dpAddress = otherAddress}
    , testCase "rejects a params UTxO without the params NFT" $
        pfails $ getParams defaultParams {dpHasNft = False}
    , testCase "rejects a params UTxO carrying a reference script" $
        pfails $ getParams defaultParams {dpRefScript = True}
    , testCase "rejects a params UTxO with no inline datum" $
        pfails $ getParams defaultParams {dpInlineDatum = False}
    ]

--------------------------------------------------------------------------------
-- get_authenticated_state_queue_policy_id
--------------------------------------------------------------------------------

refScriptTests :: TestTree
refScriptTests =
  testGroup
    "getAuthenticatedStateQueuePolicyId"
    [ -- The ledger computes a reference script's hash, so reading the policy id
      -- from there rather than from a redeemer is what makes it unforgeable.
      testCase "reads the policy id off the attached reference script" $
        holds $ refScriptPolicy defaultRefScript #== pdata (pconstant stateQueuePolicy)
    , testCase "rejects an input with no reference script attached" $
        pfails $ refScriptPolicy defaultRefScript {rsHasScript = False}
    , -- The NFT is what says this reference script is the deployment's own
      -- state-queue minter rather than any script someone chose to attach.
      testCase "rejects a reference script without the authenticating NFT" $
        pfails $ refScriptPolicy defaultRefScript {rsHasNft = False}
    , testCase "rejects an authenticating NFT under another name" $
        pfails $ refScriptPolicy defaultRefScript {rsAssetName = TokenName "Other"}
    , testCase "rejects a second token under the auth policy" $
        pfails $ refScriptPolicy defaultRefScript {rsExtraToken = True}
    ]

--------------------------------------------------------------------------------
-- validate_init_output
--------------------------------------------------------------------------------

initOutputTests :: TestTree
initOutputTests =
  testGroup
    "validateInitOutput"
    [ testCase "accepts a well-formed fresh attestation" $
        holds $
          initOutput defaultInit
            #== pdata (pcon (PTokenName (pconstant ("DAAT" <> headerHash))))
    , -- The threshold and committee hash are frozen from the current params,
      -- which is what the apply path later reconciles against.
      testCase "rejects a threshold that is not the governed one" $
        pfails $ initOutput defaultInit {diThreshold = 3}
    , testCase "rejects a committee hash that is not the governed one" $
        pfails $ initOutput defaultInit {diCommitteeHash = Just (blake2b256 otherCommittee)}
    , -- An attestation must start from nothing, or its creator could mint one
      -- that already claims a quorum.
      testCase "rejects a non-empty starting bitmap" $
        pfails $ initOutput defaultInit {diBitmap = Just (bitmapWith [0])}
    , testCase "rejects a non-zero starting count" $
        pfails $ initOutput defaultInit {diCount = 1}
    , testCase "rejects a header hash other than the block's" $
        pfails $ initOutput defaultInit {diHeaderHash = otherHeaderHash}
    , -- The name is derived from the datum, so the token cannot name a
      -- different block than the datum does.
      testCase "rejects a token whose name is not derived from the datum" $
        pfails $ initOutput defaultInit {diAssetName = Just (TokenName "DAATwrong")}
    , testCase "rejects an output at another address" $
        pfails $ initOutput defaultInit {diAddress = otherAddress}
    , testCase "rejects an output carrying a reference script" $
        pfails $ initOutput defaultInit {diRefScript = True}
    , testCase "rejects an output holding no attestation token" $
        pfails $ initOutput defaultInit {diHasNft = False}
    ]

--------------------------------------------------------------------------------
-- Assertions
--------------------------------------------------------------------------------

holds :: (forall s. Term s PBool) -> Assertion
holds = passertEval

--------------------------------------------------------------------------------
-- Identities
--------------------------------------------------------------------------------

policyFor :: Int -> CurrencySymbol
policyFor n = currencySymbolFromHex (concat (replicate 28 (hexByte n)))

hexByte :: Int -> String
hexByte x = [d (x `div` 16), d (x `mod` 16)]
  where
    d i = "0123456789abcdef" !! i

paramsPolicy, attestationPolicy, authPolicy, stateQueuePolicy, otherPolicy :: CurrencySymbol
paramsPolicy = policyFor 0x11
attestationPolicy = policyFor 0x12
authPolicy = policyFor 0x13
stateQueuePolicy = policyFor 0x14
otherPolicy = policyFor 0x15

addressOf :: CurrencySymbol -> Address
addressOf cs = scriptHashAddress (ScriptHash (unCurrencySymbol cs))

otherAddress :: Address
otherAddress = addressOf otherPolicy

headerHash, otherHeaderHash :: BS.ByteString
headerHash = BS.replicate 28 0xaa
otherHeaderHash = BS.replicate 28 0xbb

committee, otherCommittee :: BS.ByteString
committee = BS.concat [BS.cons n (BS.replicate 31 0x00) | n <- [1, 2, 3]]
otherCommittee = BS.concat [BS.cons n (BS.replicate 31 0x00) | n <- [4, 5, 6]]

emptyBitmap :: BS.ByteString
emptyBitmap = BS.replicate 32 0x00

bitmapWith :: [Int] -> BS.ByteString
bitmapWith indices = BS.pack [byteAt i | i <- [0 .. 31]]
  where
    byteAt i = sum [2 ^ (7 - (b `mod` 8)) | b <- indices, b `div` 8 == i]

blake2b256 :: BS.ByteString -> BS.ByteString
blake2b256 = fromBuiltin . Builtins.blake2b_256 . toBuiltin

outRefN :: Integer -> TxOutRef
outRefN = TxOutRef (TxId "0101010101010101010101010101010101010101010101010101010101010101")

refInputsT :: forall s. [TxInInfo] -> Term s (PBuiltinList (PAsData PTxInInfo))
refInputsT xs =
  punsafeCoerce (pasList # pconstant @PData (PD.List (map toPD xs)))

toPD :: TxInInfo -> PD.Data
toPD = builtinDataToData . toBuiltinData

txOutT :: forall s. TxOut -> Term s PTxOut
txOutT o = pfromData (punsafeCoerce (pconstant @PData (builtinDataToData (toBuiltinData o))))

--------------------------------------------------------------------------------
-- get_da_params fixtures
--------------------------------------------------------------------------------

data Params = Params
  { dpCommittee :: BS.ByteString
  , dpHashOverride :: Maybe BS.ByteString
  , dpAddress :: Address
  , dpHasNft :: Bool
  , dpRefScript :: Bool
  , dpInlineDatum :: Bool
  }

defaultParams :: Params
defaultParams =
  Params
    { dpCommittee = committee
    , dpHashOverride = Nothing
    , dpAddress = addressOf paramsPolicy
    , dpHasNft = True
    , dpRefScript = False
    , dpInlineDatum = True
    }

paramsDatumData :: BS.ByteString -> Maybe BS.ByteString -> PD.Data
paramsDatumData c hashOverride =
  PD.Constr
    0
    [ PD.B c
    , PD.B (maybe (blake2b256 c) id hashOverride)
    , PD.I 2
    , PD.List [PD.B (BS.replicate 28 n) | n <- [1, 2, 3]]
    , PD.I 2
    ]

paramsDatumTerm :: forall s. BS.ByteString -> Integer -> Term s PDaParamsDatum
paramsDatumTerm c _threshold =
  pfromData (punsafeCoerce (pconstant @PData (paramsDatumData c Nothing)))

getParams :: forall s. Params -> Term s PDaParamsDatum
getParams p =
  pgetDaParams # refInputsT [paramsInput] # pdata (pconstant paramsPolicy) # 0
  where
    paramsInput =
      TxInInfo
        (outRefN 0)
        ( TxOut
            (dpAddress p)
            ( mkAdaValue 2_000_000
                <> if dpHasNft p
                  then singleton paramsPolicy (TokenName (toBuiltin ("MIDGARD_DA_PARAMS" :: BS.ByteString))) 1
                  else mempty
            )
            ( if dpInlineDatum p
                then OutputDatum (Datum (dataToBuiltinData (paramsDatumData (dpCommittee p) (dpHashOverride p))))
                else NoOutputDatum
            )
            (if dpRefScript p then Just (ScriptHash (unCurrencySymbol otherPolicy)) else Nothing)
        )

--------------------------------------------------------------------------------
-- reference-script fixtures
--------------------------------------------------------------------------------

data RefScript = RefScript
  { rsHasScript :: Bool
  , rsHasNft :: Bool
  , rsAssetName :: TokenName
  , rsExtraToken :: Bool
  }

defaultRefScript :: RefScript
defaultRefScript =
  RefScript
    { rsHasScript = True
    , rsHasNft = True
    , rsAssetName = TokenName (toBuiltin ("StateQueueMint" :: BS.ByteString))
    , rsExtraToken = False
    }

refScriptPolicy :: forall s. RefScript -> Term s (PAsData PCurrencySymbol)
refScriptPolicy r =
  pgetAuthenticatedStateQueuePolicyId
    # refInputsT [refInput]
    # pdata (pconstant authPolicy)
    # 0
  where
    refInput =
      TxInInfo
        (outRefN 0)
        ( TxOut
            (addressOf otherPolicy)
            ( mkAdaValue 2_000_000
                <> (if rsHasNft r then singleton authPolicy (rsAssetName r) 1 else mempty)
                <> (if rsExtraToken r then singleton authPolicy (TokenName "X") 1 else mempty)
            )
            NoOutputDatum
            (if rsHasScript r then Just (ScriptHash (unCurrencySymbol stateQueuePolicy)) else Nothing)
        )

--------------------------------------------------------------------------------
-- validate_init_output fixtures
--------------------------------------------------------------------------------

data Init = Init
  { diHeaderHash :: BS.ByteString
  , diThreshold :: Integer
  , diCommitteeHash :: Maybe BS.ByteString
  , diBitmap :: Maybe BS.ByteString
  , diCount :: Integer
  , diAddress :: Address
  , diRefScript :: Bool
  , diHasNft :: Bool
  , diAssetName :: Maybe TokenName
  }

defaultInit :: Init
defaultInit =
  Init
    { diHeaderHash = headerHash
    , diThreshold = 2
    , diCommitteeHash = Nothing
    , diBitmap = Nothing
    , diCount = 0
    , diAddress = addressOf attestationPolicy
    , diRefScript = False
    , diHasNft = True
    , diAssetName = Nothing
    }

initOutput :: forall s. Init -> Term s (PAsData PTokenName)
initOutput i =
  pvalidateInitOutput
    # txOutT out
    # pdata (pconstant attestationPolicy)
    # paramsDatumTerm committee 2
    # pconstant headerHash
  where
    derivedName = TokenName (toBuiltin ("DAAT" <> diHeaderHash i))
    name = maybe derivedName id (diAssetName i)
    datumData =
      PD.Constr
        0
        [ PD.B (diHeaderHash i)
        , PD.I (diThreshold i)
        , PD.B (maybe (blake2b256 committee) id (diCommitteeHash i))
        , PD.B (maybe emptyBitmap id (diBitmap i))
        , PD.I (diCount i)
        ]
    out =
      TxOut
        (diAddress i)
        ( mkAdaValue 2_000_000
            <> if diHasNft i then singleton attestationPolicy name 1 else mempty
        )
        (OutputDatum (Datum (dataToBuiltinData datumData)))
        (if diRefScript i then Just (ScriptHash (unCurrencySymbol otherPolicy)) else Nothing)
