{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Testing.FraudProofCatalogueValidator
Description : Behavioural tests for the Plutarch port of
              @validators/fraud-proof-catalogue.ak@.

The first four cases are one-to-one with the @test@ blocks in the Aiken source,
including their names, so a divergence between the two implementations shows up
as the same case failing on each side. The remainder cover @get_datum@, which
the Aiken file does not test directly.
-}
module Testing.FraudProofCatalogueValidator (tests) where

import Numeric (showHex)

import Data.ByteString qualified as BS
import PlutusCore.Data qualified as PD
import PlutusLedgerApi.V1.Value (CurrencySymbol (..), TokenName (..), singleton)
import PlutusLedgerApi.V3 (
  Address (..),
  Credential (..),
  ScriptContext,
  ScriptHash (..),
  ScriptInfo (MintingScript),
  TxId (..),
  TxInInfo,
  TxOutRef (..),
  Value,
  scriptContextTxInfo,
  toBuiltinData,
  txInfoReferenceInputs,
 )
import PlutusTx.Builtins (dataToBuiltinData)
import Test.Tasty
import Test.Tasty.HUnit

import Plutarch.Prelude

import Midgard.FraudProofCatalogue (pgetDatum)
import Midgard.Validators.FraudProofCatalogue (
  fraudProofCatalogueMintValidator,
  fraudProofCatalogueSpendValidator,
 )
import Testing.Eval (passertEval, pfails, psucceeds)
import Testing.ScriptContextBuilder (
  ScriptContextBuilder (..),
  ScriptContextBuilderState (..),
  buildScriptContext,
  currencySymbolFromHex,
  mkAdaValue,
  withAddress,
  withInlineDatum,
  withMint,
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
    "Fraud Proof Catalogue Validator Tests"
    [ testGroup
        "mint (mirrors the Aiken test blocks)"
        [ testCase "accepts genesis coupled with hub oracle" $
            psucceeds $
              runMint (hubNft 1 <> catalogueNft 1)
        , testCase "rejects standalone catalogue mint" $
            pfails $
              runMint (catalogueNft 1)
        , testCase "rejects duplicate catalogue token" $
            pfails $
              runMint (hubNft 1 <> catalogueNft 2)
        , testCase "spend always rejects" $
            pfails $
              fraudProofCatalogueSpendValidator # pconstant (mintCtx (catalogueNft 1))
        ]
    , testGroup
        "mint (additional)"
        [ testCase "rejects a missing catalogue token" $
            pfails $
              runMint (hubNft 1)
        , testCase "rejects a second token name under the catalogue policy" $
            pfails $
              runMint (hubNft 1 <> catalogueNft 1 <> singleton cataloguePolicy (TokenName "OTHER") 1)
        , testCase "rejects two hub oracle tokens" $
            pfails $
              runMint (hubNft 2 <> catalogueNft 1)
        , testCase "rejects a non-minting script purpose" $
            pfails $
              fraudProofCatalogueMintValidator
                # pdata (pconstant hubPolicy)
                # pconstant rewardingCtx
        ]
    , testGroup
        "getDatum"
        [ testCase "reads the Merkle root from an authentic reference input" $
            passertEval $
              getDatumAt (refInputCtx (catalogueNft 1)) #== pconstant catalogueRoot
        , testCase "rejects a reference input without the catalogue NFT" $
            pfails $
              getDatumAt (refInputCtx mempty)
        , testCase "rejects a reference input carrying an extra asset" $
            pfails $
              getDatumAt (refInputCtx (catalogueNft 1 <> singleton hubPolicy (TokenName "X") 1))
        , testCase "rejects the wrong token name under the catalogue policy" $
            pfails $
              getDatumAt (refInputCtx (singleton cataloguePolicy (TokenName "NOT_THE_CATALOGUE") 1))
        ]
    ]

--------------------------------------------------------------------------------
-- Fixtures
--------------------------------------------------------------------------------

-- | A distinct 28-byte policy id per index.
policyFor :: Int -> CurrencySymbol
policyFor n = currencySymbolFromHex (replicate (56 - length h) '0' <> h)
  where
    h = showHex n ""

hubPolicy :: CurrencySymbol
hubPolicy = policyFor 1

cataloguePolicy :: CurrencySymbol
cataloguePolicy = policyFor 2

hubAssetName :: TokenName
hubAssetName = TokenName "MIDGARD_HUB_ORACLE"

catalogueAssetName :: TokenName
catalogueAssetName = TokenName "MIDGARD_FRAUD_PROOF_CATALOGUE"

hubNft :: Integer -> Value
hubNft = singleton hubPolicy hubAssetName

catalogueNft :: Integer -> Value
catalogueNft = singleton cataloguePolicy catalogueAssetName

-- | The catalogue datum: a Merkle root over the admissible fraud proofs.
catalogueRoot :: BS.ByteString
catalogueRoot = "01234567890123456789012345678901"

catalogueAddress :: Address
catalogueAddress =
  Address (ScriptCredential (ScriptHash (unCurrencySymbol cataloguePolicy))) Nothing

someOutRef :: TxOutRef
someOutRef = TxOutRef (TxId "0000000000000000000000000000000000000000000000000000000000000001") 0

--------------------------------------------------------------------------------
-- Contexts
--------------------------------------------------------------------------------

{- | A minting context whose own policy is the catalogue's.

'withMintingScript' derives the script info from the first policy in the mint
field, so the catalogue policy id is chosen to sort after the hub's.
-}
mintCtx :: Value -> ScriptContext
mintCtx mintValue = buildScriptContext $ withMintingScript mintValue (toBuiltinData ())

rewardingCtx :: ScriptContext
rewardingCtx =
  buildScriptContext $
    withRewardingScript
      (toBuiltinData ())
      (ScriptCredential (ScriptHash (unCurrencySymbol cataloguePolicy)))
      0

-- | A context whose sole reference input carries @value@ plus the catalogue datum.
refInputCtx :: Value -> ScriptContext
refInputCtx value =
  buildScriptContext $
    withMintingScript (catalogueNft 1) (toBuiltinData ())
      <> withReferenceInput
        ( withOutRef someOutRef
            <> withAddress catalogueAddress
            <> withValue (mkAdaValue 2_000_000 <> value)
            <> withInlineDatum (dataToBuiltinData (PD.B catalogueRoot))
        )

--------------------------------------------------------------------------------
-- Applying the terms
--------------------------------------------------------------------------------

-- | Applies the Aiken validator parameter, then a mint context for @mintValue@.
runMint :: forall s. Value -> Term s PUnit
runMint mintValue =
  fraudProofCatalogueMintValidator
    # pdata (pconstant hubPolicy)
    # pconstant (mintCtxOwnedByCatalogue mintValue)

{- | A mint context carrying exactly @mintValue@, with the running script fixed
to the catalogue policy.

'withMintingScript' would derive the running policy from whichever symbol sorts
first in the mint field, which is not always the catalogue's — and the negative
cases deliberately include mint fields with no catalogue entry at all. Setting
the script info explicitly keeps every case running as the catalogue policy,
which is what the Aiken tests do by calling @mint.mint@ directly.
-}
mintCtxOwnedByCatalogue :: Value -> ScriptContext
mintCtxOwnedByCatalogue mintValue =
  buildScriptContext $
    withMint mintValue (toBuiltinData ()) <> withOwnPolicy cataloguePolicy

-- | Forces the running script to be the minting policy @cs@.
withOwnPolicy :: CurrencySymbol -> ScriptContextBuilder
withOwnPolicy cs = ScriptContextBuilder $ \scb -> scb {scbScriptInfo = MintingScript cs}

-- | Resolves the catalogue datum from the reference inputs of @ctx@, at index 0.
getDatumAt :: forall s. ScriptContext -> Term s PByteString
getDatumAt ctx =
  pgetDatum
    # pconstant (referenceInputsOf ctx)
    # pdata (pconstant cataloguePolicy)
    # 0

referenceInputsOf :: ScriptContext -> [TxInInfo]
referenceInputsOf = txInfoReferenceInputs . scriptContextTxInfo
