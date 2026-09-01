{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Testing.FraudProofValidator
Description : Behavioural tests for the Plutarch port of @validators/fraud-proof.ak@.

The Aiken source has no @test@ blocks, so these are written from its three
numbered checks. The negative cases target each check in turn, plus the two
ways the strict mint-field equality can be broken.
-}
module Testing.FraudProofValidator (tests) where

import Numeric (showHex)

import PlutusLedgerApi.V1.Value (CurrencySymbol (..), TokenName (..), singleton)
import PlutusLedgerApi.V3 (
  Address (..),
  Credential (..),
  Redeemer (..),
  ScriptContext (..),
  ScriptHash (..),
  ScriptInfo (MintingScript),
  ScriptPurpose (Minting, Rewarding),
  TxId (..),
  TxInInfo,
  TxOutRef (..),
  Value,
  scriptContextTxInfo,
  toBuiltinData,
  txInfoRedeemers,
  txInfoReferenceInputs,
 )
import PlutusCore.Data qualified as PD
import PlutusTx.AssocMap qualified as AMap
import PlutusTx.Builtins (BuiltinData, dataToBuiltinData, fromBuiltin)
import Test.Tasty
import Test.Tasty.HUnit

import Plutarch.Prelude

import Midgard.FraudProof (pgetProvenFraudulentBlocksHeaderHash)
import Midgard.Validators.FraudProof (fraudProofMintValidator, fraudProofSpendValidator)
import Testing.Eval (passertEval, pfails, psucceeds)
import Testing.ScriptContextBuilder (
  ScriptContextBuilder (..),
  ScriptContextBuilderState (..),
  buildScriptContext,
  currencySymbolFromHex,
  mkAdaValue,
  withAddress,
  withMint,
  withOutRef,
  withReferenceInput,
  withValue,
 )

-- | Collects the tests defined in this module.
tests :: TestTree
tests =
  testGroup
    "Fraud Proof Validator Tests"
    [ testGroup
        "mint"
        [ testCase "accepts a Success thread burn paired with the fraud proof mint" $
            psucceeds $
              runMint (successRedeemer proofName) 0 (expectedMint proofName)
        , -- Check 1
          testCase "rejects a BurnForCancellation thread redeemer" $
            pfails $
              runMint (cancellationRedeemer proofName) 0 (expectedMint proofName)
        , testCase "rejects a redeemer index pointing at another purpose" $
            pfails $
              runMint (successRedeemer proofName) 1 (expectedMint proofName)
        , -- Check 2
          testCase "rejects a thread asset name differing from the minted one" $
            pfails $
              runMint (successRedeemer otherName) 0 (expectedMint proofName)
        , -- Check 3
          testCase "rejects minting two fraud proof tokens" $
            pfails $
              runMint (successRedeemer proofName) 0 $
                singleton threadPolicy proofName (-1) <> singleton fraudProofPolicy proofName 2
        , testCase "rejects omitting the computation thread burn" $
            pfails $
              runMint (successRedeemer proofName) 0 (singleton fraudProofPolicy proofName 1)
        , testCase "rejects an unrelated extra token in the mint field" $
            pfails $
              runMint (successRedeemer proofName) 0 $
                expectedMint proofName <> singleton otherPolicy (TokenName "EXTRA") 1
        , testCase "rejects a non-minting script purpose" $
            pfails $
              fraudProofMintValidator
                # pdata (pconstant threadPolicy)
                # pconstant rewardingCtx
        ]
    , testGroup
        "spend"
        [ testCase "always rejects" $
            pfails $
              fraudProofSpendValidator
                # pconstant (mintCtx (successRedeemer proofName) 0 (expectedMint proofName))
        ]
    , testGroup
        "getProvenFraudulentBlocksHeaderHash"
        [ testCase "drops the four-byte catalogue id prefix" $
            passertEval $
              headerHashAt (refInputCtx (singleton fraudProofPolicy proofName 1))
                #== pconstant "the-28-byte-header-hash-here"
        , testCase "rejects a reference input under a foreign policy" $
            pfails $
              headerHashAt (refInputCtx (singleton otherPolicy proofName 1))
        , testCase "rejects a reference input holding two of the token" $
            pfails $
              headerHashAt (refInputCtx (singleton fraudProofPolicy proofName 2))
        ]
    ]

--------------------------------------------------------------------------------
-- Fixtures
--------------------------------------------------------------------------------

policyFor :: Int -> CurrencySymbol
policyFor n = currencySymbolFromHex (replicate (56 - length h) '0' <> h)
  where
    h = showHex n ""

-- | Ordered so that thread < fraudProof, exercising the ascending branch of the
-- expected-mint construction.
threadPolicy :: CurrencySymbol
threadPolicy = policyFor 1

fraudProofPolicy :: CurrencySymbol
fraudProofPolicy = policyFor 2

otherPolicy :: CurrencySymbol
otherPolicy = policyFor 3

{- | A fraud proof asset name: four bytes of catalogue id, then the header hash
of the convicted block.
-}
proofName :: TokenName
proofName = TokenName "0001the-28-byte-header-hash-here"

otherName :: TokenName
otherName = TokenName "0002a-different-header-hash-here"

-- | The exact mint field the validator requires.
expectedMint :: TokenName -> Value
expectedMint n = singleton threadPolicy n (-1) <> singleton fraudProofPolicy n 1

someOutRef :: TxOutRef
someOutRef = TxOutRef (TxId "0000000000000000000000000000000000000000000000000000000000000001") 0

--------------------------------------------------------------------------------
-- Redeemers
--------------------------------------------------------------------------------

{- | Computation thread @Success { burning_token_asset_name }@.

Built as raw @Data@ rather than through a Haskell mirror type, so the
constructor tags are stated explicitly: @Init@ is 0, @Success@ 1,
@BurnForCancellation@ 2. Those tags are the contract with the Aiken type, and
writing them out here means a reordering on either side fails a test rather than
passing silently.
-}
successRedeemer :: TokenName -> BuiltinData
successRedeemer n = dataToBuiltinData (PD.Constr 1 [tokenNameData n])

-- | Computation thread @BurnForCancellation { burning_token_asset_name }@.
cancellationRedeemer :: TokenName -> BuiltinData
cancellationRedeemer n = dataToBuiltinData (PD.Constr 2 [tokenNameData n])

{- | The fraud proof's own redeemer: a single-constructor record, so @Constr 0@
with the asset name and the thread redeemer index.
-}
fraudProofRedeemer :: TokenName -> Integer -> BuiltinData
fraudProofRedeemer n idx = dataToBuiltinData (PD.Constr 0 [tokenNameData n, PD.I idx])

tokenNameData :: TokenName -> PD.Data
tokenNameData = PD.B . fromBuiltin . unTokenName

--------------------------------------------------------------------------------
-- Contexts
--------------------------------------------------------------------------------

{- | A mint context in which the computation thread policy is invoked with
@threadRedeemer@ at redeemer index 0, and the fraud proof policy is the running
script.
-}
mintCtx :: BuiltinData -> Integer -> Value -> ScriptContext
mintCtx threadRedeemer redeemerIndex mintValue =
  let base =
        buildScriptContext $
          withMint mintValue (toBuiltinData ())
            <> withOwnPolicy fraudProofPolicy
      txInfo = scriptContextTxInfo base
      redeemers =
        AMap.unsafeFromList
          [ (Minting threadPolicy, Redeemer threadRedeemer)
          , (Rewarding (ScriptCredential (ScriptHash (unCurrencySymbol otherPolicy))), Redeemer (toBuiltinData ()))
          ]
   in base
        { scriptContextTxInfo = txInfo {txInfoRedeemers = redeemers}
        , scriptContextRedeemer = Redeemer (fraudProofRedeemer proofName redeemerIndex)
        }

rewardingCtx :: ScriptContext
rewardingCtx =
  buildScriptContext $
    withMint (expectedMint proofName) (toBuiltinData ())
      <> ScriptContextBuilder
        (\scb -> scb {scbScriptInfo = MintingScript fraudProofPolicy})

refInputCtx :: Value -> ScriptContext
refInputCtx value =
  buildScriptContext $
    withMint (expectedMint proofName) (toBuiltinData ())
      <> withOwnPolicy fraudProofPolicy
      <> withReferenceInput
        ( withOutRef someOutRef
            <> withAddress (Address (ScriptCredential (ScriptHash (unCurrencySymbol fraudProofPolicy))) Nothing)
            <> withValue (mkAdaValue 2_000_000 <> value)
        )

withOwnPolicy :: CurrencySymbol -> ScriptContextBuilder
withOwnPolicy cs = ScriptContextBuilder $ \scb -> scb {scbScriptInfo = MintingScript cs}

--------------------------------------------------------------------------------
-- Applying the terms
--------------------------------------------------------------------------------

runMint :: forall s. BuiltinData -> Integer -> Value -> Term s PUnit
runMint threadRedeemer redeemerIndex mintValue =
  fraudProofMintValidator
    # pdata (pconstant threadPolicy)
    # pconstant (mintCtx threadRedeemer redeemerIndex mintValue)

headerHashAt :: forall s. ScriptContext -> Term s PByteString
headerHashAt ctx =
  pgetProvenFraudulentBlocksHeaderHash
    # pconstant (referenceInputsOf ctx)
    # pdata (pconstant fraudProofPolicy)
    # 0

referenceInputsOf :: ScriptContext -> [TxInInfo]
referenceInputsOf = txInfoReferenceInputs . scriptContextTxInfo
