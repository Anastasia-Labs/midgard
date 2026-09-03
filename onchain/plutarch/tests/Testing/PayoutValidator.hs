{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Testing.PayoutValidator
Description : Behavioural tests for the Plutarch port of @validators/payout.ak@ —
              the minting policy.

The payout policy is a hinge between two other scripts rather than a decision of
its own, so these tests are mostly about what it refuses to take on trust. Three
carry Aiken's own names; the rest cover conditions Aiken's two tests leave to
the surrounding transaction.

The mint side's central claim is that a payout token is a *conversion* of a
withdrawal token: same asset name, opposite direction, in one transaction, with
nothing else riding along.
-}
module Testing.PayoutValidator (tests) where

import Data.ByteString qualified as BS
import PlutusCore.Data qualified as PD
import PlutusLedgerApi.V1.Address (scriptHashAddress)
import PlutusLedgerApi.V1.Value (CurrencySymbol (..), TokenName (..), Value, getValue, singleton)
import PlutusLedgerApi.V3 (
  Address,
  ScriptPurpose (Minting),
  Datum (..),
  OutputDatum (NoOutputDatum, OutputDatum),
  Redeemer (..),
  ScriptContext (..),
  ScriptHash (..),
  ScriptInfo (MintingScript, SpendingScript),
  ScriptPurpose (Spending),
  TxId (..),
  TxInInfo (..),
  TxOut (..),
  TxOutRef (..),
  toBuiltinData,
  scriptContextTxInfo,
  txInfoInputs,
  txInfoMint,
  txInfoOutputs,
  txInfoRedeemers,
  txInfoReferenceInputs,
 )
import PlutusLedgerApi.V3.MintValue (MintValue (UnsafeMintValue))
import PlutusTx.AssocMap qualified as Map
import PlutusTx.Builtins (BuiltinData, builtinDataToData, dataToBuiltinData, fromBuiltin, toBuiltin)
import Test.Tasty
import Test.Tasty.HUnit

import Plutarch.Prelude

import Midgard.Validators.Payout (payoutMintValidator, payoutSpendValidator)
import Midgard.Common.Value (pmergeValues, pnegateValue, pvalueWithoutNft)
import Plutarch.LedgerApi.Value qualified as PValue
import Plutarch.LedgerApi.V3 (PLedgerValue)
import Plutarch.Unsafe (punsafeCoerce)
import PlutusTx.Builtins (builtinDataToData)
import Testing.Eval (pfails, psucceeds)
import Testing.ScriptContextBuilder (buildScriptContext, currencySymbolFromHex, mkAdaValue)

-- | Collects the tests defined in this module.
tests :: TestTree
tests =
  testGroup
    "Payout Validator Tests"
    [ testGroup
        "mint / MintPayout"
        [ testCase "payout_mint_accepts_matching_withdrawal_burn" $
            psucceeds $ runMint defaultMint
        , -- The shared asset name is what carries the withdrawal's identity
          -- into the payout; without it any withdrawal would authorise any
          -- payout.
          testCase "payout_mint_rejects_asset_name_mismatch" $
            pfails $ runMint defaultMint {mPayoutName = otherAsset}
        , testCase "rejects minting the withdrawal token instead of burning it" $
            pfails $ runMint defaultMint {mWithdrawalQuantity = 1}
        , testCase "rejects burning the payout token" $
            pfails $ runMint defaultMint {mPayoutQuantity = -1}
        , testCase "rejects a payout quantity above one" $
            pfails $ runMint defaultMint {mPayoutQuantity = 2}
        , -- A third policy in the mint field would let something unexamined
          -- ride along on the withdrawal's authority.
          testCase "rejects a third policy in the mint field" $
            pfails $
              runMint defaultMint {mExtraMint = Just (singleton otherPolicy (TokenName "x") 1)}
        , -- The withdrawal must be the one actually being spent, at the named
          -- outref and address.
          testCase "rejects a withdrawal input at another outref" $
            pfails $ runMint defaultMint {mWithdrawalInputRef = outRefN 9}
        , testCase "rejects a withdrawal input at another address" $
            pfails $ runMint defaultMint {mWithdrawalInputAddr = otherAddress}
        , testCase "rejects a withdrawal input not holding the burnt token" $
            pfails $ runMint defaultMint {mWithdrawalInputHasToken = False}
        , -- Spending a withdrawal to refund the user is not spending it to
          -- start a payout.
          testCase "rejects a withdrawal spent for a refund" $
            pfails $ runMint defaultMint {mPurposeIsPayout = False}
        , -- The hub oracle is what ties this script to one protocol instance.
          testCase "rejects a hub oracle naming another payout policy" $
            pfails $ runMint defaultMint {mHubPayoutPolicy = otherPolicy}
        , testCase "rejects a hub oracle naming another withdrawal policy" $
            pfails $ runMint defaultMint {mHubWithdrawalPolicy = otherPolicy}
        ]
    , testGroup
        "mint / BurnPayout"
        [ testCase "accepts a burn against a concluding spend" $
            psucceeds $ runBurn defaultBurn
        , -- Aiken's name: a burn redeemer must not be able to bring anything
          -- into existence.
          testCase "payout_burn_rejects_unrelated_mint" $
            pfails $
              runBurn defaultBurn {bExtraMint = Just (singleton otherPolicy (TokenName "x") 1)}
        , testCase "rejects a mint where a burn is required" $
            pfails $ runBurn defaultBurn {bQuantity = 1}
        , testCase "rejects a burn of an asset name the redeemer does not name" $
            pfails $ runBurn defaultBurn {bMintedName = otherAsset}
        , testCase "rejects an input not holding the token being burnt" $
            pfails $ runBurn defaultBurn {bInputHasToken = False}
        , -- Both scripts must mean the same payout and the same hub.
          testCase "rejects a spend redeemer naming another payout input" $
            pfails $ runBurn defaultBurn {bSpendPayoutInputIndex = 7}
        , testCase "rejects a spend redeemer naming another hub reference" $
            pfails $ runBurn defaultBurn {bSpendHubRefIndex = 7}
        , -- Concluding is the only spend a burn may accompany; adding funds is
          -- not the end of a payout's life.
          testCase "rejects an AddFunds spend alongside a burn" $
            pfails $ runBurn defaultBurn {bSpendIsAddFunds = True}
        , testCase "rejects a hub oracle naming another payout policy" $
            pfails $ runBurn defaultBurn {bHubPayoutPolicy = otherPolicy}
        ]
    , spendTests
    ]

--------------------------------------------------------------------------------
-- spend
--------------------------------------------------------------------------------

{- | The spend handler's two branches.

@AddFunds@ defends one invariant above all: the payout can never hold more than
its target. It is checked from both ends — before the collection and after — so
a filled payout is exactly the target and a partial one is strictly under it.
Overshooting would strand reserve funds in a payout no conclusion could release,
because concluding requires the contents to /equal/ the target.
-}
spendTests :: TestTree
spendTests =
  testGroup
    "spend"
    [ testGroup
        "AddFunds"
        [ testCase "payout_add_funds_accepts_exact_collection" $
            psucceeds $ runAdd defaultAdd
        , testCase "accepts a partial collection under the target" $
            psucceeds $ runAdd defaultAdd {aCollected = 60, aChange = 40}
        , -- Over the target there is no conclusion that can release the funds.
          testCase "payout_add_funds_rejects_overfunding_above_target" $
            pfails $ runAdd defaultAdd {aCollected = 120, aChange = -20}
        , -- A collection must collect something.
          testCase "payout_add_funds_rejects_no_positive_needed_asset_contribution" $
            pfails $ runAdd defaultAdd {aCollected = 0, aChange = 100}
        , testCase "rejects a collection that removes value from the payout" $
            pfails $ runAdd defaultAdd {aCollected = -10, aChange = 110}
        , -- The datum fixes the target, so it must survive untouched.
          testCase "payout_add_funds_rejects_datum_mutation" $
            pfails $ runAdd defaultAdd {aOutputDatumTarget = Just 50}
        , -- Everything the reserve gave up must be accounted for.
          testCase "rejects change that does not balance the reserve input" $
            pfails $ runAdd defaultAdd {aCollected = 60, aChange = 40, aChangeShort = True}
        , testCase "payout_add_funds_rejects_wrong_reserve_change_address" $
            pfails $ runAdd defaultAdd {aCollected = 60, aChange = 40, aChangeAddr = otherAddress}
        , -- Change may not carry back something the payout still needs.
          testCase "rejects change carrying a still-needed asset" $
            pfails $ runAdd defaultAdd {aCollected = 60, aChange = 40, aChangeIsNeeded = True}
        , testCase "payout_add_funds_rejects_unrelated_mint" $
            pfails $ runAdd defaultAdd {aMint = True}
        , testCase "payout_add_funds_rejects_second_reserve_input" $
            pfails $ runAdd defaultAdd {aSecondReserveInput = True}
        , testCase "payout_add_funds_rejects_second_payout_policy_output" $
            pfails $ runAdd defaultAdd {aSecondPayoutOutput = True}
        , testCase "rejects a reserve redeemer disagreeing about the pairing" $
            pfails $ runAdd defaultAdd {aReserveRedeemerPayoutIndex = 7}
        ]
    , testGroup
        "ConcludeWithdrawal"
        [ testCase "payout_conclude_accepts_exact_burn_and_destination" $
            psucceeds $ runConclude defaultConclude
        , -- Concluding short would pay the user less than the payout promised.
          testCase "payout_conclude_rejects_underfunded_target_output" $
            pfails $ runConclude defaultConclude {cHeld = 90}
        , testCase "payout_conclude_rejects_surplus_accumulator_value" $
            pfails $ runConclude defaultConclude {cHeld = 110}
        , testCase "payout_conclude_rejects_wrong_destination_address" $
            pfails $ runConclude defaultConclude {cL1Address = otherAddress}
        , testCase "payout_conclude_rejects_wrong_destination_datum" $
            pfails $ runConclude defaultConclude {cL1HasDatum = True}
        , testCase "rejects paying the user less than the target" $
            pfails $ runConclude defaultConclude {cL1Value = 90}
        , -- A payout continuing without its token would be unredeemable state.
          testCase "payout_conclude_rejects_continuing_payout_output" $
            pfails $ runConclude defaultConclude {cContinuingOutput = True}
        , testCase "rejects a burn redeemer naming another payout input" $
            pfails $ runConclude defaultConclude {cBurnPayoutInputIndex = 7}
        ]
    ]

data Add = Add
  { aCollected :: Integer
  , aChange :: Integer
  , aOutputDatumTarget :: Maybe Integer
  , aChangeAddr :: Address
  , aChangeIsNeeded :: Bool
  , aMint :: Bool
  , aSecondReserveInput :: Bool
  , aSecondPayoutOutput :: Bool
  , aReserveRedeemerPayoutIndex :: Integer
  , -- | Return less change than the reserve input actually gave up.
    aChangeShort :: Bool
  }

defaultAdd :: Add
defaultAdd =
  Add
    { aCollected = 100
    , aChange = 0
    , aOutputDatumTarget = Nothing
    , aChangeAddr = reserveAddress
    , aChangeIsNeeded = False
    , aMint = False
    , aSecondReserveInput = False
    , aSecondPayoutOutput = False
    , aReserveRedeemerPayoutIndex = 0
    , aChangeShort = False
    }

{- | A payout whose target is 100 of one token, collecting from a reserve input
holding exactly 100.
-}
runAdd :: forall s. Add -> Term s PUnit
runAdd a =
  payoutSpendValidator # pdata (pconstant hubPolicy) # pconstant ctx
  where
    datumOf t = payoutDatumWith t
    inDatum = datumOf 100
    outDatum = maybe inDatum datumOf (aOutputDatumTarget a)
    payoutIn =
      TxInInfo
        payoutRef
        ( TxOut
            payoutAddress
            (mkAdaValue 2_000_000 <> payoutNft <> tokenValue 0)
            (OutputDatum (Datum (dataToBuiltinData inDatum)))
            Nothing
        )
    -- The reserve input is exactly what the collection takes plus whatever
    -- comes back as change; a partial collection therefore needs a reserve
    -- holding some asset the payout does *not* need.
    reserveIn =
      TxInInfo
        reserveRef
        ( TxOut
            reserveAddress
            (tokenValue (aCollected a) <> otherTokenValue (aChange a))
            NoOutputDatum
            Nothing
        )
    extraReserveIn =
      TxInInfo
        (outRefN 5)
        (TxOut reserveAddress (tokenValue 5) NoOutputDatum Nothing)
    payoutOut =
      TxOut
        payoutAddress
        (mkAdaValue 2_000_000 <> payoutNft <> tokenValue (aCollected a))
        (OutputDatum (Datum (dataToBuiltinData outDatum)))
        Nothing
    changeOut =
      TxOut
        (aChangeAddr a)
        ( let n = if aChangeShort a then aChange a - 1 else aChange a
           in if aChangeIsNeeded a then tokenValue n else otherTokenValue n
        )
        NoOutputDatum
        Nothing
    extraPayoutOut =
      TxOut payoutAddress (mkAdaValue 2_000_000 <> otherPayoutNft) NoOutputDatum Nothing
    outputs =
      [payoutOut]
        <> [changeOut | aChange a /= 0]
        <> [extraPayoutOut | aSecondPayoutOutput a]
    reserveRedeemer =
      Redeemer . dataToBuiltinData $
        PD.Constr 0 [PD.I 1, PD.I (aReserveRedeemerPayoutIndex a), PD.I 0, PD.I 0]
    base = buildScriptContext mempty
    txInfo =
      (scriptContextTxInfo base)
        { txInfoInputs = [payoutIn, reserveIn] <> [extraReserveIn | aSecondReserveInput a]
        , txInfoOutputs = outputs
        , txInfoReferenceInputs = [hubRefIn payoutPolicy withdrawalPolicy]
        , txInfoMint =
            toMint (if aMint a then singleton otherPolicy (TokenName "x") 1 else mempty)
        , txInfoRedeemers =
            Map.unsafeFromList
              [ (Spending payoutRef, Redeemer (dataToBuiltinData addRedeemer))
              , (Spending reserveRef, reserveRedeemer)
              ]
        }
    addRedeemer =
      PD.Constr
        0
        [ PD.I 0 -- payout_input_index
        , PD.I 0 -- payout_output_index
        , PD.I 1 -- reserve_input_index
        , if aChange a /= 0 then PD.Constr 0 [PD.I 1] else PD.Constr 1 []
        , PD.I 1 -- reserve_spend_redeemer_index
        , PD.I 0 -- payout_spend_redeemer_index
        , PD.I 0 -- hub_ref_input_index
        ]
    ctx =
      ScriptContext
        txInfo
        (Redeemer (dataToBuiltinData addRedeemer))
        (SpendingScript payoutRef (Just (Datum (dataToBuiltinData inDatum))))

data Conclude = Conclude
  { cHeld :: Integer
  , cL1Value :: Integer
  , cL1Address :: Address
  , cL1HasDatum :: Bool
  , cContinuingOutput :: Bool
  , cBurnPayoutInputIndex :: Integer
  }

defaultConclude :: Conclude
defaultConclude =
  Conclude
    { cHeld = 100
    , cL1Value = 100
    , cL1Address = userAddress
    , cL1HasDatum = False
    , cContinuingOutput = False
    , cBurnPayoutInputIndex = 0
    }

runConclude :: forall s. Conclude -> Term s PUnit
runConclude c =
  payoutSpendValidator # pdata (pconstant hubPolicy) # pconstant ctx
  where
    datum = payoutDatumWith 100
    payoutIn =
      TxInInfo
        payoutRef
        ( TxOut
            payoutAddress
            (mkAdaValue 2_000_000 <> payoutNft <> tokenValue (cHeld c))
            (OutputDatum (Datum (dataToBuiltinData datum)))
            Nothing
        )
    l1Out =
      TxOut
        (cL1Address c)
        (mkAdaValue 2_000_000 <> tokenValue (cL1Value c))
        (if cL1HasDatum c then OutputDatum (Datum (dataToBuiltinData (PD.I 1))) else NoOutputDatum)
        Nothing
    continuingOut =
      TxOut payoutAddress (mkAdaValue 2_000_000) (OutputDatum (Datum (dataToBuiltinData datum))) Nothing
    burnRedeemer =
      Redeemer . dataToBuiltinData $
        PD.Constr 1 [PD.I (cBurnPayoutInputIndex c), PD.B payoutAsset, PD.I 0, PD.I 0]
    base = buildScriptContext mempty
    txInfo =
      (scriptContextTxInfo base)
        { txInfoInputs = [payoutIn]
        , txInfoOutputs = [l1Out] <> [continuingOut | cContinuingOutput c]
        , txInfoReferenceInputs = [hubRefIn payoutPolicy withdrawalPolicy]
        , txInfoMint = toMint (singleton payoutPolicy (TokenName (toBuiltin payoutAsset)) (-1))
        , txInfoRedeemers =
            Map.unsafeFromList
              [ (Spending payoutRef, Redeemer (dataToBuiltinData concludeRedeemer))
              , (Minting payoutPolicy, burnRedeemer)
              ]
        }
    -- The redeemer map is [Spending payout, Minting payout], so the burn's
    -- entry is at index 1.
    concludeRedeemer =
      PD.Constr 1 [PD.I 0, PD.I 0, PD.I 1, PD.I 0]
    ctx =
      ScriptContext
        txInfo
        (Redeemer (dataToBuiltinData concludeRedeemer))
        (SpendingScript payoutRef (Just (Datum (dataToBuiltinData datum))))

{- | A payout datum whose L2 target is @n@ of the one test token, plus the
min-Ada the payout UTxO itself carries.

The Ada has to be in the target. @value_without_nft@ removes only the payout
NFT, so whatever Ada the UTxO holds stays in the value compared against the
target — a target naming only the token would make @target - current@ negative
in Ada and every collection would fail. Ada sorts first, hence the leading
entry.
-}
payoutDatumWith :: Integer -> PD.Data
payoutDatumWith n =
  PD.Constr
    0
    [ PD.Map
        [ (PD.B BS.empty, PD.Map [(PD.B BS.empty, PD.I 2_000_000)])
        , (PD.B (unCs tokenPolicy), PD.Map [(PD.B tokenAsset, PD.I n)])
        ]
    , addressDataOf userPolicy
    , PD.Constr 0 [] -- NoOutputDatum
    ]

payoutNft, otherPayoutNft :: Value
payoutNft = singleton payoutPolicy (TokenName (toBuiltin payoutAsset)) 1
otherPayoutNft = singleton payoutPolicy (TokenName (toBuiltin otherAsset)) 1

tokenValue :: Integer -> Value
tokenValue n
  | n == 0 = mempty
  | otherwise = singleton tokenPolicy (TokenName (toBuiltin tokenAsset)) n

otherTokenValue :: Integer -> Value
otherTokenValue n
  | n == 0 = mempty
  | otherwise = singleton otherPolicy (TokenName (toBuiltin tokenAsset)) n

tokenPolicy, userPolicy, reservePolicy :: CurrencySymbol
tokenPolicy = policyFor 0x66
userPolicy = policyFor 0x67
reservePolicy = policyFor 0x68

tokenAsset :: BS.ByteString
tokenAsset = BS.pack [0x01]

reserveAddress, userAddress :: Address
reserveAddress = addressOf reservePolicy
userAddress = addressOf userPolicy

reserveRef :: TxOutRef
reserveRef = outRefN 2

unCs :: CurrencySymbol -> BS.ByteString
unCs = fromBuiltin . unCurrencySymbol

addressDataOf :: CurrencySymbol -> PD.Data
addressDataOf p = PD.Constr 0 [PD.Constr 1 [PD.B (unCs p)], PD.Constr 1 []]

--------------------------------------------------------------------------------
-- MintPayout
--------------------------------------------------------------------------------

data Mint = Mint
  { mPayoutName :: BS.ByteString
  , mPayoutQuantity :: Integer
  , mWithdrawalQuantity :: Integer
  , mExtraMint :: Maybe Value
  , mWithdrawalInputRef :: TxOutRef
  , mWithdrawalInputAddr :: Address
  , mWithdrawalInputHasToken :: Bool
  , mPurposeIsPayout :: Bool
  , mHubPayoutPolicy :: CurrencySymbol
  , mHubWithdrawalPolicy :: CurrencySymbol
  }

defaultMint :: Mint
defaultMint =
  Mint
    { mPayoutName = payoutAsset
    , mPayoutQuantity = 1
    , mWithdrawalQuantity = -1
    , mExtraMint = Nothing
    , mWithdrawalInputRef = withdrawalRef
    , mWithdrawalInputAddr = withdrawalAddress
    , mWithdrawalInputHasToken = True
    , mPurposeIsPayout = True
    , mHubPayoutPolicy = payoutPolicy
    , mHubWithdrawalPolicy = withdrawalPolicy
    }

runMint :: forall s. Mint -> Term s PUnit
runMint m =
  payoutMintValidator # pdata (pconstant hubPolicy) # pconstant ctx
  where
    minted =
      singleton payoutPolicy (TokenName (toBuiltin (mPayoutName m))) (mPayoutQuantity m)
        <> singleton withdrawalPolicy (TokenName (toBuiltin payoutAsset)) (mWithdrawalQuantity m)
        <> maybe mempty id (mExtraMint m)
    withdrawalInput =
      TxInInfo
        (mWithdrawalInputRef m)
        ( TxOut
            (mWithdrawalInputAddr m)
            ( mkAdaValue 2_000_000
                <> if mWithdrawalInputHasToken m
                  then singleton withdrawalPolicy (TokenName (toBuiltin payoutAsset)) 1
                  else mempty
            )
            (OutputDatum (Datum (dataToBuiltinData (PD.Constr 0 []))))
            Nothing
        )
    -- The withdrawal's own spend redeemer. Nine fields; only the last, the
    -- purpose, is read here.
    withdrawalSpendRedeemer =
      Redeemer . dataToBuiltinData $
        PD.Constr
          0
          [ PD.I 0
          , PD.I 0
          , PD.I 0
          , PD.I 0
          , PD.I 0
          , PD.I 0
          , PD.Constr 0 [] -- membership proof placeholder
          , PD.I 0
          , if mPurposeIsPayout m
              then PD.Constr 0 [] -- InitializePayout
              else PD.Constr 1 [PD.Constr 0 []] -- Refund
          ]
    base = buildScriptContext mempty
    txInfo =
      (scriptContextTxInfo base)
        { txInfoInputs = [withdrawalInput]
        , txInfoReferenceInputs = [hubRefIn (mHubPayoutPolicy m) (mHubWithdrawalPolicy m)]
        , txInfoMint = toMint minted
        , txInfoRedeemers =
            Map.unsafeFromList [(Spending withdrawalRef, withdrawalSpendRedeemer)]
        }
    redeemer =
      PD.Constr
        0
        [ outRefData withdrawalRef
        , PD.I 0 -- withdrawal_input_index
        , PD.I 0 -- withdrawal_spend_redeemer_index
        , PD.I 0 -- hub_ref_input_index
        ]
    ctx =
      ScriptContext txInfo (Redeemer (dataToBuiltinData redeemer)) (MintingScript payoutPolicy)

--------------------------------------------------------------------------------
-- BurnPayout
--------------------------------------------------------------------------------

data Burn = Burn
  { bMintedName :: BS.ByteString
  , bQuantity :: Integer
  , bExtraMint :: Maybe Value
  , bInputHasToken :: Bool
  , bSpendPayoutInputIndex :: Integer
  , bSpendHubRefIndex :: Integer
  , bSpendIsAddFunds :: Bool
  , bHubPayoutPolicy :: CurrencySymbol
  }

defaultBurn :: Burn
defaultBurn =
  Burn
    { bMintedName = payoutAsset
    , bQuantity = -1
    , bExtraMint = Nothing
    , bInputHasToken = True
    , bSpendPayoutInputIndex = 0
    , bSpendHubRefIndex = 0
    , bSpendIsAddFunds = False
    , bHubPayoutPolicy = payoutPolicy
    }

runBurn :: forall s. Burn -> Term s PUnit
runBurn b =
  payoutMintValidator # pdata (pconstant hubPolicy) # pconstant ctx
  where
    minted =
      singleton payoutPolicy (TokenName (toBuiltin (bMintedName b))) (bQuantity b)
        <> maybe mempty id (bExtraMint b)
    payoutInput =
      TxInInfo
        payoutRef
        ( TxOut
            payoutAddress
            ( mkAdaValue 2_000_000
                <> if bInputHasToken b
                  then singleton payoutPolicy (TokenName (toBuiltin payoutAsset)) 1
                  else mempty
            )
            (OutputDatum (Datum (dataToBuiltinData (PD.Constr 0 []))))
            Nothing
        )
    -- The payout's own spend redeemer: @ConcludeWithdrawal@ is tag 1,
    -- @AddFunds@ tag 0.
    payoutSpendRedeemer
      | bSpendIsAddFunds b =
          Redeemer . dataToBuiltinData $
            PD.Constr 0 [PD.I 0, PD.I 0, PD.I 0, PD.Constr 1 [], PD.I 0, PD.I 0, PD.I 0]
      | otherwise =
          Redeemer . dataToBuiltinData $
            PD.Constr
              1
              [ PD.I (bSpendPayoutInputIndex b)
              , PD.I 0 -- l1_output_index
              , PD.I 0 -- burn_redeemer_index
              , PD.I (bSpendHubRefIndex b)
              ]
    base = buildScriptContext mempty
    txInfo =
      (scriptContextTxInfo base)
        { txInfoInputs = [payoutInput]
        , txInfoReferenceInputs = [hubRefIn (bHubPayoutPolicy b) withdrawalPolicy]
        , txInfoMint = toMint minted
        , txInfoRedeemers = Map.unsafeFromList [(Spending payoutRef, payoutSpendRedeemer)]
        }
    redeemer =
      PD.Constr
        1
        [ PD.I 0 -- payout_input_index
        , PD.B payoutAsset
        , PD.I 0 -- payout_spend_redeemer_index
        , PD.I 0 -- hub_ref_input_index
        ]
    ctx =
      ScriptContext txInfo (Redeemer (dataToBuiltinData redeemer)) (MintingScript payoutPolicy)

--------------------------------------------------------------------------------
-- Identities
--------------------------------------------------------------------------------

policyFor :: Int -> CurrencySymbol
policyFor n = currencySymbolFromHex (concat (replicate 28 (hexByte n)))

hexByte :: Int -> String
hexByte x = [d (x `div` 16), d (x `mod` 16)]
  where
    d i = "0123456789abcdef" !! i

hubPolicy, payoutPolicy, withdrawalPolicy, otherPolicy :: CurrencySymbol
hubPolicy = policyFor 0x11
payoutPolicy = policyFor 0x22
withdrawalPolicy = policyFor 0x52
otherPolicy = policyFor 0x77

payoutAsset, otherAsset :: BS.ByteString
payoutAsset = BS.pack [0xaa]
otherAsset = BS.pack [0xbb]

addressOf :: CurrencySymbol -> Address
addressOf cs = scriptHashAddress (ScriptHash (unCurrencySymbol cs))

payoutAddress, withdrawalAddress, otherAddress :: Address
payoutAddress = addressOf payoutPolicy
withdrawalAddress = addressOf withdrawalPolicy
otherAddress = addressOf otherPolicy

outRefN :: Integer -> TxOutRef
outRefN = TxOutRef (TxId "0101010101010101010101010101010101010101010101010101010101010101")

withdrawalRef, payoutRef :: TxOutRef
withdrawalRef = outRefN 0
payoutRef = outRefN 1

{- | A @TxOutRef@ as the ledger encodes it.

Hand-rolling this encoding is a trap — V3 does not wrap the transaction id the
way the Aiken type declaration reads — so it goes through the ledger's own
'toData' instead.
-}
outRefData :: TxOutRef -> PD.Data
outRefData = builtinDataToData . toBuiltinData

toMint :: Value -> MintValue
toMint = UnsafeMintValue . getValue

--------------------------------------------------------------------------------
-- Hub oracle
--------------------------------------------------------------------------------

hubRefIn :: CurrencySymbol -> CurrencySymbol -> TxInInfo
hubRefIn payoutId withdrawalId =
  TxInInfo
    (outRefN 8)
    ( TxOut
        (addressOf hubPolicy)
        ( mkAdaValue 2_000_000
            <> singleton hubPolicy (TokenName (toBuiltin ("MIDGARD_HUB_ORACLE" :: BS.ByteString))) 1
        )
        (OutputDatum (Datum (hubDatum payoutId withdrawalId)))
        Nothing
    )

hubDatum :: CurrencySymbol -> CurrencySymbol -> BuiltinData
hubDatum payoutId withdrawalId =
  dataToBuiltinData $
    PD.Constr
      0
      ( [ PD.B (cs (policyFor 0x41)) -- 0: registered_operators
        , PD.B (cs (policyFor 0x42)) -- 1: active_operators
        , PD.B (cs (policyFor 0x43)) -- 2: retired_operators
        , PD.B (cs (policyFor 0x44)) -- 3: scheduler
        , PD.B (cs (policyFor 0x45)) -- 4: state_queue
        , PD.B (cs (policyFor 0x46)) -- 5: fraud_proof_catalogue
        , PD.B (cs (policyFor 0x47)) -- 6: fraud_proof
        , PD.B (cs (policyFor 0x48)) -- 7: deposit
        , PD.B (cs withdrawalId) -- 8: withdrawal
        , PD.B (cs (policyFor 0x49)) -- 9: tx_order
        , PD.B (cs (policyFor 0x4a)) -- 10: settlement
        , PD.B (cs payoutId) -- 11: payout
        ]
          <> [ addressData (policyFor 0x42) -- 12: registered_operators_addr
             , addressData (policyFor 0x42) -- 13: active_operators_addr
             , addressData (policyFor 0x42) -- 14: retired_operators_addr
             , addressData (policyFor 0x42) -- 15: scheduler_addr
             , addressData (policyFor 0x42) -- 16: state_queue_addr
             , addressData (policyFor 0x42) -- 17: fraud_proof_catalogue_addr
             , addressData (policyFor 0x42) -- 18: fraud_proof_addr
             , addressData (policyFor 0x42) -- 19: deposit_addr
             , withdrawalAddressData -- 20: withdrawal_addr
             , addressData (policyFor 0x42) -- 21: tx_order_addr
             , addressData (policyFor 0x42) -- 22: settlement_addr
             , addressData reservePolicy -- 23: reserve_addr
             , addressData payoutPolicy -- 24: payout_addr
             ]
          <> [PD.B (cs (policyFor 0x4f))]
      )
  where
    cs = fromBuiltin . unCurrencySymbol
    withdrawalAddressData = addressData withdrawalPolicy
    addressData p =
      PD.Constr 0 [PD.Constr 1 [PD.B (cs p)], PD.Constr 1 []]
