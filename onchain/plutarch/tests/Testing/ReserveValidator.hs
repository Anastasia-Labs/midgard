{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Testing.ReserveValidator
Description : Behavioural tests for the Plutarch port of @validators/reserve.ak@.

The seven cases in the first group are one-to-one with the @test@ blocks in the
Aiken source, including their names and fixtures, so a divergence between the
two implementations surfaces as the same case failing on each side.
-}
module Testing.ReserveValidator (tests) where

import Numeric (showHex)

import PlutusCore.Data qualified as PD
import PlutusLedgerApi.V1.Address (scriptHashAddress)
import PlutusLedgerApi.V1.Value (CurrencySymbol (..), TokenName (..), singleton)
import PlutusLedgerApi.V3 (
  Address,
  Datum (..),
  OutputDatum (..),
  Redeemer (..),
  ScriptContext (..),
  ScriptHash (..),
  ScriptInfo (SpendingScript),
  ScriptPurpose (Spending),
  TxId (..),
  TxInInfo (..),
  TxOut (..),
  TxOutRef (..),
  Value,
  scriptContextTxInfo,
  toBuiltinData,
  txInfoInputs,
  txInfoMint,
  txInfoRedeemers,
  txInfoReferenceInputs,
 )
import PlutusTx.AssocMap qualified as AMap
import PlutusTx.Builtins (BuiltinData, dataToBuiltinData, fromBuiltin)
import Test.Tasty
import Test.Tasty.HUnit

import Plutarch.Prelude

import Midgard.Validators.Reserve (reserveSpendValidator, reserveWithdrawValidator)
import Testing.Eval (pfails, psucceeds)
import Testing.ScriptContextBuilder (
  buildScriptContext,
  currencySymbolFromHex,
  mkAdaValue,
  withMint,
 )

-- | Collects the tests defined in this module.
tests :: TestTree
tests =
  testGroup
    "Reserve Validator Tests"
    [ testGroup
        "spend (mirrors the Aiken test blocks)"
        [ testCase "accepts matching payout add funds" $
            psucceeds $
              runSpend (baseTx (addFundsRedeemer 0) [reserveInput, payoutInput] mempty)
        , testCase "rejects wrong payout branch" $
            pfails $
              runSpend (baseTx concludeWithdrawalRedeemer [reserveInput, payoutInput] mempty)
        , testCase "rejects payout redeemer pointing at different reserve" $
            pfails $
              runSpend (baseTx (addFundsRedeemer 1) [reserveInput, payoutInput] mempty)
        , testCase "rejects missing payout input" $
            pfails $
              runSpend (baseTx (addFundsRedeemer 0) [reserveInput] mempty)
        , testCase "rejects wrong payout redeemer index" $
            pfails $
              runSpendWith
                (reserveRedeemerWith 0)
                (baseTx (addFundsRedeemer 0) [reserveInput, payoutInput] mempty)
        , testCase "rejects unrelated mint" $
            pfails $
              runSpend
                ( baseTx
                    (addFundsRedeemer 0)
                    [reserveInput, payoutInput]
                    (singleton (policyFor 0x88) (TokenName "\01") 1)
                )
        , testCase "rejects reserve input with datum" $
            pfails $
              runSpend (baseTx (addFundsRedeemer 0) [reserveInputWithDatum, payoutInput] mempty)
        ]
    , testGroup
        "spend (additional)"
        [ testCase "rejects a payout input at a foreign address" $
            pfails $
              runSpend (baseTx (addFundsRedeemer 0) [reserveInput, payoutInputElsewhere] mempty)
        , testCase "rejects a payout input without the payout token" $
            pfails $
              runSpend (baseTx (addFundsRedeemer 0) [reserveInput, payoutInputNoToken] mempty)
        , testCase "rejects a reserve input at a foreign address" $
            pfails $
              runSpend (baseTx (addFundsRedeemer 0) [reserveInputElsewhere, payoutInput] mempty)
        ]
    , testGroup
        "withdraw"
        [ testCase "is fail-closed pending the observer protocol" $
            pfails $
              reserveWithdrawValidator
                # pconstant (buildScriptContext (withMint (singleton hubPolicy hubAssetName 1) (toBuiltinData ())))
        ]
    ]

--------------------------------------------------------------------------------
-- Fixtures, mirroring the Aiken ones
--------------------------------------------------------------------------------

policyFor :: Int -> CurrencySymbol
policyFor n = currencySymbolFromHex (replicate (56 - length h) '0' <> h)
  where
    h = showHex n ""

repeatedByte :: Int -> CurrencySymbol
repeatedByte b = currencySymbolFromHex (concat (replicate 28 h))
  where
    h = let x = showHex b "" in replicate (2 - length x) '0' <> x

hubPolicy, payoutPolicy, reservePolicy :: CurrencySymbol
hubPolicy = repeatedByte 0x11
payoutPolicy = repeatedByte 0x22
reservePolicy = repeatedByte 0x33

hubAssetName :: TokenName
hubAssetName = TokenName "MIDGARD_HUB_ORACLE"

payoutAsset :: TokenName
payoutAsset = TokenName "PAYOUT"

addressOf :: CurrencySymbol -> Address
addressOf = scriptHashAddress . ScriptHash . unCurrencySymbol

reserveRef, payoutRef, hubRef :: TxOutRef
reserveRef = TxOutRef (TxId "0101010101010101010101010101010101010101010101010101010101010101") 0
payoutRef = TxOutRef (TxId "0202020202020202020202020202020202020202020202020202020202020202") 0
hubRef = TxOutRef (TxId "0303030303030303030303030303030303030303030303030303030303030303") 0

--------------------------------------------------------------------------------
-- Inputs
--------------------------------------------------------------------------------

mkIn :: TxOutRef -> Address -> Value -> OutputDatum -> TxInInfo
mkIn ref addr val dat = TxInInfo ref (TxOut addr val dat Nothing)

reserveInput :: TxInInfo
reserveInput = mkIn reserveRef (addressOf reservePolicy) (mkAdaValue 8) NoOutputDatum

-- | The Aiken @reserve_spend_rejects_reserve_input_with_datum@ fixture.
reserveInputWithDatum :: TxInInfo
reserveInputWithDatum =
  mkIn reserveRef (addressOf reservePolicy) (mkAdaValue 8) $
    OutputDatum (Datum (dataToBuiltinData (PD.B "")))

reserveInputElsewhere :: TxInInfo
reserveInputElsewhere =
  mkIn reserveRef (addressOf (policyFor 0x66)) (mkAdaValue 8) NoOutputDatum

payoutValue :: Value
payoutValue = mkAdaValue 2 <> singleton payoutPolicy payoutAsset 1

payoutInput :: TxInInfo
payoutInput =
  mkIn payoutRef (addressOf payoutPolicy) payoutValue (OutputDatum (Datum payoutDatum))

payoutInputElsewhere :: TxInInfo
payoutInputElsewhere =
  mkIn payoutRef (addressOf (policyFor 0x66)) payoutValue (OutputDatum (Datum payoutDatum))

payoutInputNoToken :: TxInInfo
payoutInputNoToken =
  mkIn payoutRef (addressOf payoutPolicy) (mkAdaValue 2) (OutputDatum (Datum payoutDatum))

hubRefInput :: TxInInfo
hubRefInput =
  mkIn
    hubRef
    (addressOf hubPolicy)
    (singleton hubPolicy hubAssetName 1)
    (OutputDatum (Datum hubDatum))

--------------------------------------------------------------------------------
-- Datums and redeemers
--------------------------------------------------------------------------------

-- | The Aiken @test_hub_datum@ fixture: 12 policy ids, 13 addresses, 1 hash.
hubDatum :: BuiltinData
hubDatum =
  dataToBuiltinData $
    PD.Constr
      0
      ( map (PD.B . fromBuiltin . unCurrencySymbol) policies
          <> replicate 11 reserveAddrData
          <> [reserveAddrData, payoutAddrData]
          <> [PD.B (fromBuiltin (unCurrencySymbol reservePolicy))]
      )
  where
    policies =
      [ repeatedByte 0x44
      , repeatedByte 0x45
      , repeatedByte 0x46
      , repeatedByte 0x47
      , repeatedByte 0x48
      , repeatedByte 0x49
      , repeatedByte 0x50
      , repeatedByte 0x51
      , repeatedByte 0x52
      , repeatedByte 0x53
      , repeatedByte 0x54
      , payoutPolicy
      ]
    reserveAddrData = addrData reservePolicy
    payoutAddrData = addrData payoutPolicy
    addrData cs =
      PD.Constr
        0
        [ PD.Constr 1 [PD.B (fromBuiltin (unCurrencySymbol cs))]
        , PD.Constr 1 []
        ]

-- | The Aiken @payout_datum_data@ fixture.
payoutDatum :: BuiltinData
payoutDatum =
  dataToBuiltinData $
    PD.Constr
      0
      [ PD.Map [(PD.B "", PD.Map [(PD.B "", PD.I 7)])]
      , PD.Constr 0 [PD.Constr 1 [PD.B (fromBuiltin (unCurrencySymbol (policyFor 0x66)))], PD.Constr 1 []]
      , PD.Constr 0 []
      ]

-- | The Aiken @reserve_redeemer@ fixture.
reserveRedeemer :: BuiltinData
reserveRedeemer = reserveRedeemerWith 1

-- | As 'reserveRedeemer', but with an explicit @payout_spend_redeemer_index@.
reserveRedeemerWith :: Integer -> BuiltinData
reserveRedeemerWith payoutSpendRedeemerIndex =
  dataToBuiltinData (PD.Constr 0 [PD.I 0, PD.I 1, PD.I payoutSpendRedeemerIndex, PD.I 0])

-- | The Aiken @add_funds_redeemer@ fixture — @AddFunds@ is constructor 0.
addFundsRedeemer :: Integer -> BuiltinData
addFundsRedeemer reserveInputIndex =
  dataToBuiltinData $
    PD.Constr
      0
      [ PD.I 1 -- payout_input_index
      , PD.I 0 -- payout_output_index
      , PD.I reserveInputIndex
      , PD.Constr 0 [PD.I 1] -- reserve_change_output_index = Some(1)
      , PD.I 0 -- reserve_spend_redeemer_index
      , PD.I 1 -- payout_spend_redeemer_index
      , PD.I 0 -- hub_ref_input_index
      ]

-- | @ConcludeWithdrawal@ is constructor 1.
concludeWithdrawalRedeemer :: BuiltinData
concludeWithdrawalRedeemer =
  dataToBuiltinData (PD.Constr 1 [PD.I 1, PD.I 0, PD.I 0, PD.I 0])

--------------------------------------------------------------------------------
-- Contexts
--------------------------------------------------------------------------------

{- | The Aiken @reserve_test_tx@ fixture, as a spending script context for the
reserve input.
-}
baseTx :: BuiltinData -> [TxInInfo] -> Value -> ScriptContext
baseTx payoutRedeemer inputs mintValue =
  let empty = buildScriptContext mempty
      txInfo0 = scriptContextTxInfo empty
      withMinted = buildScriptContext (if mintValue == mempty then mempty else withMint mintValue (toBuiltinData ()))
      txInfo =
        txInfo0
          { txInfoInputs = inputs
          , txInfoReferenceInputs = [hubRefInput]
          , txInfoMint = txInfoMint (scriptContextTxInfo withMinted)
          , txInfoRedeemers =
              AMap.unsafeFromList
                [ (Spending reserveRef, Redeemer reserveRedeemer)
                , (Spending payoutRef, Redeemer payoutRedeemer)
                ]
          }
   in ScriptContext txInfo (Redeemer reserveRedeemer) (SpendingScript reserveRef Nothing)

--------------------------------------------------------------------------------
-- Applying the terms
--------------------------------------------------------------------------------

runSpend :: forall s. ScriptContext -> Term s PUnit
runSpend ctx =
  reserveSpendValidator
    # pdata (pconstant (ScriptHash (unCurrencySymbol hubPolicy)))
    # pconstant ctx

-- | As 'runSpend', but overriding the reserve's own redeemer.
runSpendWith :: forall s. BuiltinData -> ScriptContext -> Term s PUnit
runSpendWith redeemer ctx =
  runSpend
    ctx
      { scriptContextRedeemer = Redeemer redeemer
      , scriptContextTxInfo =
          (scriptContextTxInfo ctx)
            { txInfoRedeemers =
                AMap.insert (Spending reserveRef) (Redeemer redeemer) $
                  txInfoRedeemers (scriptContextTxInfo ctx)
            }
      }
