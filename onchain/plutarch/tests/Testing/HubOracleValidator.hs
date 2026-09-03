{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Testing.HubOracleValidator
Description : Behavioural tests for the Plutarch port of @validators/hub-oracle.ak@.

Each case names the clause of the Aiken original it pins down. The negative
cases are the load-bearing ones: the one-shot property is only meaningful if
minting without the init UTxO, minting more than one, and minting a second token
name under the same policy all fail.
-}
module Testing.HubOracleValidator (tests) where

import Numeric (showHex)

import PlutusCore.Data qualified as PD
import PlutusLedgerApi.V1.Address (pubKeyHashAddress)
import PlutusLedgerApi.V1.Value (CurrencySymbol (..), TokenName (..), singleton)
import PlutusLedgerApi.V3 (
  Address (..),
  Credential (..),
  PubKeyHash (..),
  ScriptContext,
  ScriptHash (..),
  TxId (..),
  TxInInfo,
  TxOutRef (..),
  Value,
  scriptContextTxInfo,
  toBuiltinData,
  txInfoReferenceInputs,
 )
import PlutusTx.Builtins (builtinDataToData, dataToBuiltinData, fromBuiltin)
import Test.Tasty
import Test.Tasty.HUnit

import Plutarch.Prelude

import Midgard.HubOracle (PHubOracleDatum (..), pgetDatum)
import Midgard.Validators.HubOracle (hubOracleMintValidator)
import Testing.Eval (passertEval, pfails, psucceeds)
import Testing.ScriptContextBuilder (
  buildScriptContext,
  currencySymbolFromHex,
  mkAdaValue,
  withAddress,
  withInlineDatum,
  withInput,
  withMintingScript,
  withOutRef,
  withReferenceInput,
  withRewardingScript,
  withValue,
 )

-- | Collects the tests defined in this module.
tests :: TestTree
tests =
  testGroup
    "Hub Oracle Validator Tests"
    [ testGroup
        "mint"
        [ testCase "mints one when the init UTxO is spent" $
            psucceeds $
              runMint (mintCtx (hubNft 1) True)
        , testCase "rejects minting one without the init UTxO" $
            pfails $
              runMint (mintCtx (hubNft 1) False)
        , testCase "burns one without requiring the init UTxO" $
            psucceeds $
              runMint (mintCtx (hubNft (-1)) False)
        , testCase "rejects minting two" $
            pfails $
              runMint (mintCtx (hubNft 2) True)
        , testCase "rejects a second token name under the same policy" $
            pfails $
              runMint (mintCtx (hubNft 1 <> singleton hubPolicy (TokenName "IMPOSTOR") 1) True)
        , testCase "rejects a non-minting script purpose" $
            pfails $
              runMint rewardingCtx
        ]
    , testGroup
        "getDatum"
        [ testCase "reads a field from an authentic reference input" $
            passertEval $
              pmatch (getDatumAt (refInputCtx (hubNft 1) hubScriptAddress)) $
                \PHubOracleDatum {phubOracle'stateQueue} ->
                  phubOracle'stateQueue #== pdata (pconstant stateQueuePolicy)
        , testCase "rejects a reference input at a foreign address" $
            pfails $
              getDatumAt (refInputCtx (hubNft 1) foreignScriptAddress)
        , testCase "rejects a reference input without the hub NFT" $
            pfails $
              getDatumAt (refInputCtx mempty hubScriptAddress)
        , testCase "rejects a reference input carrying an extra asset" $
            pfails $
              getDatumAt
                (refInputCtx (hubNft 1 <> singleton foreignPolicy (TokenName "OTHER") 1) hubScriptAddress)
        ]
    ]

--------------------------------------------------------------------------------
-- Fixtures
--------------------------------------------------------------------------------

-- | A distinct 28-byte policy id per index, so a test that reads one datum field
-- cannot pass by accidentally reading a neighbouring one.
policyFor :: Int -> CurrencySymbol
policyFor n = currencySymbolFromHex (replicate (56 - length h) '0' <> h)
  where
    h = showHex n ""

{- | The hub oracle's own policy id. Its bytes double as its script hash, which
is the invariant @get_authentic_input_of@ relies on.
-}
hubPolicy :: CurrencySymbol
hubPolicy = policyFor 1

hubScriptHash :: ScriptHash
hubScriptHash = ScriptHash (unCurrencySymbol hubPolicy)

hubScriptAddress :: Address
hubScriptAddress = Address (ScriptCredential hubScriptHash) Nothing

foreignPolicy :: CurrencySymbol
foreignPolicy = policyFor 2

foreignScriptAddress :: Address
foreignScriptAddress =
  Address (ScriptCredential (ScriptHash (unCurrencySymbol foreignPolicy))) Nothing

-- | The @state_queue@ field of the datum below; the fifth of twenty-six.
stateQueuePolicy :: CurrencySymbol
stateQueuePolicy = policyFor 3

hubAssetName :: TokenName
hubAssetName = TokenName "MIDGARD_HUB_ORACLE"

-- | The UTxO whose consumption authorises the single mint.
initUtxo :: TxOutRef
initUtxo = TxOutRef (TxId "0000000000000000000000000000000000000000000000000000000000000001") 0

userAddress :: Address
userAddress = pubKeyHashAddress (PubKeyHash "0000000000000000000000000000000000000000000000000000000f")

-- | @qty@ of the hub oracle NFT.
hubNft :: Integer -> Value
hubNft = singleton hubPolicy hubAssetName

--------------------------------------------------------------------------------
-- Contexts
--------------------------------------------------------------------------------

-- | A minting context for the given mint field, optionally spending the init UTxO.
mintCtx :: Value -> Bool -> ScriptContext
mintCtx mintValue spendsInitUtxo =
  buildScriptContext $
    withMintingScript mintValue (toBuiltinData ())
      <> if spendsInitUtxo
        then
          withInput
            ( withOutRef initUtxo
                <> withAddress userAddress
                <> withValue (mkAdaValue 10_000_000)
            )
        else mempty

-- | A rewarding context, to exercise the Aiken @else(_) { fail }@ branch.
rewardingCtx :: ScriptContext
rewardingCtx =
  buildScriptContext $
    withRewardingScript (toBuiltinData ()) (ScriptCredential hubScriptHash) 0

-- | A context whose sole reference input carries @value@ at @address@.
refInputCtx :: Value -> Address -> ScriptContext
refInputCtx value address =
  buildScriptContext $
    withMintingScript (hubNft 1) (toBuiltinData ())
      <> withReferenceInput
        ( withOutRef initUtxo
            <> withAddress address
            <> withValue (mkAdaValue 2_000_000 <> value)
            <> withInlineDatum (dataToBuiltinData hubDatumData)
        )

--------------------------------------------------------------------------------
-- Applying the terms
--------------------------------------------------------------------------------

-- | Applies the two Aiken validator parameters, then the context.
runMint :: forall s. ScriptContext -> Term s PUnit
runMint ctx =
  hubOracleMintValidator
    # pdata (pconstant initUtxo)
    # pdata (pconstant hubAssetName)
    # pconstant ctx

-- | Resolves the hub datum from the reference inputs of @ctx@, at index 0.
getDatumAt :: forall s. ScriptContext -> Term s PHubOracleDatum
getDatumAt ctx =
  pgetDatum
    # pconstant (referenceInputsOf ctx)
    # pdata (pconstant hubScriptHash)
    # 0

referenceInputsOf :: ScriptContext -> [TxInInfo]
referenceInputsOf = txInfoReferenceInputs . scriptContextTxInfo

--------------------------------------------------------------------------------
-- The 26-field datum
--------------------------------------------------------------------------------

{- | A hub oracle datum matching the field order of @hub_oracle.Datum@: twelve
policy ids, then thirteen addresses, then the reserve observer script hash.
-}
hubDatumData :: PD.Data
hubDatumData =
  PD.Constr
    0
    ( map policyData datumPolicies
        <> replicate 13 addressData
        <> [policyData (policyFor 0x99)]
    )
  where
    policyData = PD.B . fromBuiltin . unCurrencySymbol
    addressData = builtinDataToData (toBuiltinData hubScriptAddress)

-- | The twelve policy id fields, @state_queue@ fifth.
datumPolicies :: [CurrencySymbol]
datumPolicies =
  [ policyFor 0x101
  , policyFor 0x102
  , policyFor 0x103
  , policyFor 0x104
  , stateQueuePolicy
  , policyFor 0x106
  , policyFor 0x107
  , policyFor 0x108
  , policyFor 0x109
  , policyFor 0x10a
  , policyFor 0x10b
  , policyFor 0x10c
  ]
