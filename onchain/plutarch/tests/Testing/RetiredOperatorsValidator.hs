{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Testing.RetiredOperatorsValidator
Description : Behavioural tests for the Plutarch port of
              @validators/operator-directory/retired-operators.ak@.

The bond-recovery branch is where this validator's own logic lives — the
operator's signature, the removed node's key, and the unlock-time wait. The
other branches delegate to code already covered elsewhere, so they are tested
for correct wiring rather than re-tested in depth.
-}
module Testing.RetiredOperatorsValidator (tests) where

import Numeric (showHex)

import Data.ByteString qualified as BS
import PlutusCore.Data qualified as PD
import PlutusLedgerApi.V1.Address (scriptHashAddress)
import PlutusLedgerApi.V1.Interval (Extended (..), Interval (..), LowerBound (..), UpperBound (..))
import PlutusLedgerApi.V1.Value (CurrencySymbol (..), TokenName (..), Value, getValue, singleton)
import PlutusLedgerApi.V3 (
  Datum (..),
  OutputDatum (..),
  POSIXTime (..),
  PubKeyHash (..),
  Redeemer (..),
  ScriptContext (..),
  ScriptHash (..),
  ScriptInfo (MintingScript, SpendingScript),
  TxId (..),
  TxInInfo (..),
  TxOut (..),
  TxOutRef (..),
  scriptContextTxInfo,
  toBuiltinData,
  txInfoInputs,
  txInfoMint,
  txInfoOutputs,
  txInfoSignatories,
  txInfoValidRange,
 )
import PlutusLedgerApi.V3.MintValue (MintValue (UnsafeMintValue))
import PlutusTx.Builtins (BuiltinData, builtinDataToData, dataToBuiltinData, toBuiltin)
import Test.Tasty
import Test.Tasty.HUnit

import Plutarch.Prelude

import Midgard.Validators.RetiredOperators (
  retiredOperatorsMintValidator,
  retiredOperatorsSpendValidator,
 )
import Testing.Eval (psucceeds, pfails)
import Testing.ScriptContextBuilder (buildScriptContext, currencySymbolFromHex, mkAdaValue)

-- | Collects the tests defined in this module.
tests :: TestTree
tests =
  testGroup
    "Retired Operators Validator Tests"
    [ testGroup
        "spend"
        [ testCase "permits a spend when the list policy burns" $
            psucceeds $ runSpend (burnNode operatorKey)
        , testCase "rejects a spend with no list-policy mint or burn" $
            pfails $ runSpend (toMint (singleton otherPolicy (TokenName "X") 1))
        ]
    , testGroup
        "mint / RecoverOperatorBond"
        [ testCase "accepts recovery after the unlock time" $
            psucceeds $ runRecover operatorKey (unlockAt 100) (rangeAfter 200) [signer operatorKey]
        , testCase "accepts recovery when no unlock time was set" $
            psucceeds $ runRecover operatorKey noUnlock (rangeAfter 200) [signer operatorKey]
        , testCase "rejects recovery before the unlock time" $
            pfails $ runRecover operatorKey (unlockAt 300) (rangeAfter 200) [signer operatorKey]
        , testCase "rejects recovery without the operator's signature" $
            pfails $ runRecover operatorKey (unlockAt 100) (rangeAfter 200) []
        , testCase "rejects recovery signed by somebody else" $
            pfails $ runRecover operatorKey (unlockAt 100) (rangeAfter 200) [signer "zz"]
        , testCase "rejects recovering a node keyed to another operator" $
            pfails $ runRecover "zz" (unlockAt 100) (rangeAfter 200) [signer "zz"]
        ]
    ]

--------------------------------------------------------------------------------
-- Fixtures
--------------------------------------------------------------------------------

policyFor :: Int -> CurrencySymbol
policyFor n = currencySymbolFromHex (replicate (56 - length h) '0' <> h)
  where
    h = showHex n ""

hubPolicy, dirPolicy, otherPolicy :: CurrencySymbol
hubPolicy = policyFor 1
dirPolicy = policyFor 2
otherPolicy = policyFor 3

rootName :: TokenName
rootName = TokenName "MIDGARD_RETIRED_OPERATORS"

nodePrefix :: BS.ByteString
nodePrefix = "MRET"

operatorKey :: BS.ByteString
operatorKey = "bb"

nodeName :: BS.ByteString -> TokenName
nodeName key = TokenName (toBuiltin (nodePrefix <> key))

mkElemOut :: TokenName -> BuiltinData -> TxOut
mkElemOut tn dat =
  TxOut
    (scriptHashAddress (ScriptHash (unCurrencySymbol dirPolicy)))
    (mkAdaValue 2_000_000 <> singleton dirPolicy tn 1)
    (OutputDatum (Datum dat))
    Nothing

outRefN :: Integer -> TxOutRef
outRefN = TxOutRef (TxId "0101010101010101010101010101010101010101010101010101010101010101")

signer :: BS.ByteString -> PubKeyHash
signer = PubKeyHash . toBuiltin

--------------------------------------------------------------------------------
-- Datums
--------------------------------------------------------------------------------

mkElement :: PD.Data -> PD.Data -> BuiltinData
mkElement elementData link = dataToBuiltinData (PD.Constr 0 [elementData, link])

rootDatum :: PD.Data -> BuiltinData
rootDatum = mkElement (PD.Constr 0 [PD.B ""])

-- | @Node { data = NodeData { bond_unlock_time } }@.
nodeDatum :: PD.Data -> PD.Data -> BuiltinData
nodeDatum nodeData = mkElement (PD.Constr 1 [nodeData])

-- | @NodeData { bond_unlock_time: Some t }@ — a record, so @Constr 0@.
unlockAt :: Integer -> PD.Data
unlockAt t = PD.Constr 0 [PD.Constr 0 [PD.I t]]

-- | @NodeData { bond_unlock_time: None }@.
noUnlock :: PD.Data
noUnlock = PD.Constr 0 [PD.Constr 1 []]

linkNone :: PD.Data
linkNone = PD.Constr 1 []

linkTo :: BS.ByteString -> PD.Data
linkTo key = PD.Constr 0 [PD.B key]

toMint :: Value -> MintValue
toMint = UnsafeMintValue . getValue

burnNode :: BS.ByteString -> MintValue
burnNode key = toMint (singleton dirPolicy (nodeName key) (-1))

-- | A validity range starting strictly after @t@.
rangeAfter :: Integer -> Interval POSIXTime
rangeAfter t = Interval (LowerBound (Finite (POSIXTime t)) True) (UpperBound PosInf True)

{- | @RecoverOperatorBond@ is constructor 3 of the retired mint redeemer.

The anchor out-ref is encoded from the real 'TxOutRef' rather than hand-built,
so the test cannot disagree with the ledger about its shape.
-}
recoverRedeemer :: BS.ByteString -> BuiltinData
recoverRedeemer key =
  dataToBuiltinData $
    PD.Constr
      3
      [ PD.B key
      , builtinDataToData (toBuiltinData (outRefN 0))
      , PD.I 0
      ]

--------------------------------------------------------------------------------
-- Applying the terms
--------------------------------------------------------------------------------

runSpend :: forall s. MintValue -> Term s PUnit
runSpend mint =
  retiredOperatorsSpendValidator
    # pdata (pconstant dirPolicy)
    # pconstant ctx
  where
    base = buildScriptContext mempty
    ctx =
      ScriptContext
        ((scriptContextTxInfo base) {txInfoMint = mint})
        (Redeemer (toBuiltinData ()))
        (SpendingScript (outRefN 0) Nothing)

{- | A bond-recovery transaction: the root anchors one node keyed to
@nodeOperator@, that node is burnt, and the continued root drops the link.
-}
runRecover ::
  forall s.
  BS.ByteString ->
  PD.Data ->
  Interval POSIXTime ->
  [PubKeyHash] ->
  Term s PUnit
runRecover nodeOperator nodeData validRange signatories =
  retiredOperatorsMintValidator
    # pdata (pconstant hubPolicy)
    # pconstant ctx
  where
    base = buildScriptContext mempty
    txInfo =
      (scriptContextTxInfo base)
        { txInfoInputs =
            [ TxInInfo (outRefN 0) (mkElemOut rootName (rootDatum (linkTo nodeOperator)))
            , TxInInfo (outRefN 1) (mkElemOut (nodeName nodeOperator) (nodeDatum nodeData linkNone))
            ]
        , txInfoOutputs = [mkElemOut rootName (rootDatum linkNone)]
        , txInfoMint = burnNode nodeOperator
        , txInfoValidRange = validRange
        , txInfoSignatories = signatories
        }
    ctx =
      ScriptContext
        txInfo
        (Redeemer (recoverRedeemer operatorKey))
        (MintingScript dirPolicy)
