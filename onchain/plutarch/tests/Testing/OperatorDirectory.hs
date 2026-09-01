{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Testing.OperatorDirectory
Description : Tests for the Plutarch port of @lib/midgard/operator-directory.ak@.

The directory is an ascending linked list keyed by operator hash, so the
interesting properties are the ones that depend on that ordering: a
non-membership proof is a gap that straddles the operator, and initialisation is
tied to the hub oracle's own one-shot NFT.
-}
module Testing.OperatorDirectory (tests) where

import Numeric (showHex)

import Data.ByteString qualified as BS
import PlutusCore.Data qualified as PD
import PlutusLedgerApi.V1.Address (scriptHashAddress)
import PlutusLedgerApi.V1.Value (CurrencySymbol (..), TokenName (..), getValue, singleton)
import PlutusLedgerApi.V3 (
  Address,
  Datum (..),
  OutputDatum (..),
  PubKeyHash (..),
  Redeemer (..),
  ScriptHash (..),
  ScriptPurpose (Minting),
  TxId (..),
  TxInInfo (..),
  TxOut (..),
  TxOutRef (..),
  Value,
 )
import PlutusLedgerApi.V3.MintValue (MintValue (UnsafeMintValue))
import PlutusTx.Builtins (BuiltinData, builtinDataToData, dataToBuiltinData, fromBuiltin, toBuiltin)
import PlutusLedgerApi.V3 (toBuiltinData)
import Test.Tasty
import Test.Tasty.HUnit

import Plutarch.LedgerApi.V3 (PMintValue)
import Plutarch.Prelude
import Plutarch.Unsafe (punsafeCoerce)

import Midgard.OperatorDirectory (
  pcrossValidateSlashingReason,
  PSlashingArguments (..),
  PSlashingReason (..),
  pdeinit,
  pinit,
  poperatorIsNotAMember,
  pslashFraudulentOperatorAndGetInfo,
 )
import Testing.Eval (passertEval, pfails)
import Testing.ScriptContextBuilder (currencySymbolFromHex, mkAdaValue)

-- | Collects the tests defined in this module.
tests :: TestTree
tests =
  testGroup
    "Operator Directory Tests"
    [ testGroup
        "init"
        [ testCase "accepts a root minted alongside the hub oracle NFT" $
            passertEval $ runInit rootOut (mintBoth 1)
        , -- `expect nonce_validated` in the linked list, so this errors rather
          -- than returning False.
          testCase "rejects a root without the hub oracle mint" $
            pfails $ runInit rootOut (mintDirOnly 1)
        , testCase "rejects a root carrying a non-empty payload" $
            passertEval $ pnot #$ runInit rootOutWithData (mintBoth 1)
        ]
    , testGroup
        "deinit"
        [ testCase "accepts teardown alongside the hub oracle burn" $
            passertEval $ runDeinit [rootIn linkNone] (mintBoth (-1))
        , testCase "rejects teardown without the hub oracle burn" $
            pfails $ runDeinit [rootIn linkNone] (mintDirOnly (-1))
        , testCase "rejects tearing down a non-empty directory" $
            pfails $ runDeinit [rootIn (linkTo "cc")] (mintBoth (-1))
        ]
    , testGroup
        "operatorIsNotAMember"
        [ testCase "a gap straddling the operator proves absence" $
            passertEval $ runNotAMember "bb" [nodeRef "aa" (linkTo "cc")]
        , testCase "below the first node is absence" $
            passertEval $ runNotAMember "aa" [rootRef (linkTo "cc")]
        , testCase "beyond the last node is absence" $
            passertEval $ runNotAMember "zz" [nodeRef "aa" linkNone]
        , testCase "an operator equal to the gap's lower key is not absent" $
            passertEval $ pnot #$ runNotAMember "aa" [nodeRef "aa" (linkTo "cc")]
        , testCase "an operator equal to the gap's upper key is not absent" $
            passertEval $ pnot #$ runNotAMember "cc" [nodeRef "aa" (linkTo "cc")]
        , testCase "an operator outside the referenced gap is not proven absent" $
            passertEval $ pnot #$ runNotAMember "dd" [nodeRef "aa" (linkTo "cc")]
        ]
    , testGroup
        "slashFraudulentOperator"
        [ testCase "accepts a slash backed by a matching state-queue removal" $
            passertEval $
              runSlash slashedOperator (removeFraudulentHeader slashedOperator) badStateReason
        , testCase "rejects a state-queue redeemer naming a different operator" $
            pfails $
              runSlash slashedOperator (removeFraudulentHeader "zz") badStateReason
        , testCase "rejects a state-queue redeemer on the wrong branch" $
            pfails $
              runSlash slashedOperator commitBlockHeader badStateReason
        , testCase "rejects slashing an operator that is not the removed node" $
            pfails $
              runSlash "aa" (removeFraudulentHeader "aa") badStateReason
        , testCase "rejects a settlement reason with no settlement input" $
            pfails $
              runSlash slashedOperator (removeFraudulentHeader slashedOperator) badSettlementReason
        ]
    , testGroup
        "crossValidateSlashingReason"
        [ testCase "returns the reason the slashing operator set gave" $
            passertEval $
              runCrossValidate slashedOperator slashedOperator 0 #== badStateReason
        , testCase "returns a settlement reason unchanged" $
            passertEval $
              runCrossValidate slashedOperator slashedOperator 1 #== badSettlementReason
        , -- The only thing this enforces is that the two scripts agree on
          -- *who* is being slashed; the reason itself is read, not decided.
          testCase "rejects an operator set naming a different operator" $
            pfails $ runCrossValidate slashedOperator "zz" 0
        ]
    ]

--------------------------------------------------------------------------------
-- Fixtures
--------------------------------------------------------------------------------

policyFor :: Int -> CurrencySymbol
policyFor n = currencySymbolFromHex (replicate (56 - length h) '0' <> h)
  where
    h = showHex n ""

hubPolicy, dirPolicy :: CurrencySymbol
hubPolicy = policyFor 1
dirPolicy = policyFor 2

hubAssetName :: TokenName
hubAssetName = TokenName "MIDGARD_HUB_ORACLE"

rootName :: TokenName
rootName = TokenName "MIDGARD_ACTIVE_OPERATORS"

nodePrefix :: BS.ByteString
nodePrefix = "MACT"

nodePrefixLen :: Integer
nodePrefixLen = 4

mkElemOut :: TokenName -> BuiltinData -> TxOut
mkElemOut tn dat =
  TxOut
    (scriptHashAddress (ScriptHash (unCurrencySymbol dirPolicy)))
    (mkAdaValue 2_000_000 <> singleton dirPolicy tn 1)
    (OutputDatum (Datum dat))
    Nothing

rootOut :: TxOut
rootOut = mkElemOut rootName (rootDatum linkNone)

-- | A root whose payload is not @env.empty_data@.
rootOutWithData :: TxOut
rootOutWithData = mkElemOut rootName (mkElement (PD.Constr 0 [PD.I 7]) linkNone)

nodeName :: BS.ByteString -> TokenName
nodeName key = TokenName (toBuiltin (nodePrefix <> key))

outRefN :: Integer -> TxOutRef
outRefN = TxOutRef (TxId "0101010101010101010101010101010101010101010101010101010101010101")

rootIn :: PD.Data -> TxInInfo
rootIn link = TxInInfo (outRefN 0) (mkElemOut rootName (rootDatum link))

rootRef :: PD.Data -> TxInInfo
rootRef = rootIn

nodeRef :: BS.ByteString -> PD.Data -> TxInInfo
nodeRef key link = TxInInfo (outRefN 1) (mkElemOut (nodeName key) (nodeDatum link))

--------------------------------------------------------------------------------
-- Datums and mints
--------------------------------------------------------------------------------

mkElement :: PD.Data -> PD.Data -> BuiltinData
mkElement elementData link = dataToBuiltinData (PD.Constr 0 [elementData, link])

-- | @Root { data = env.empty_data }@ — the directory root carries @""@.
rootDatum :: PD.Data -> BuiltinData
rootDatum = mkElement (PD.Constr 0 [PD.B ""])

nodeDatum :: PD.Data -> BuiltinData
nodeDatum = mkElement (PD.Constr 1 [PD.B ""])

linkNone :: PD.Data
linkNone = PD.Constr 1 []

linkTo :: BS.ByteString -> PD.Data
linkTo key = PD.Constr 0 [PD.B key]

toMint :: Value -> MintValue
toMint = UnsafeMintValue . getValue

-- | Hub oracle NFT and directory root NFT together.
mintBoth :: Integer -> MintValue
mintBoth q = toMint (singleton hubPolicy hubAssetName q <> singleton dirPolicy rootName q)

-- | Only the directory root NFT — no hub oracle authorisation.
mintDirOnly :: Integer -> MintValue
mintDirOnly q = toMint (singleton dirPolicy rootName q)

pmint :: forall s. MintValue -> Term s PMintValue
pmint m = pfromData (pconstant @(PAsData PMintValue) m)

--------------------------------------------------------------------------------
-- Applying the terms
--------------------------------------------------------------------------------

runInit :: forall s. TxOut -> MintValue -> Term s PBool
runInit out mint =
  pinit
    (pdata (pconstant hubPolicy))
    (pdata (pconstant dirPolicy))
    (pdata (pconstant rootName))
    0
    (pconstant [out])
    (pmint mint)

runDeinit :: forall s. [TxInInfo] -> MintValue -> Term s PBool
runDeinit ins mint =
  pdeinit
    (pdata (pconstant hubPolicy))
    (pdata (pconstant dirPolicy))
    (pdata (pconstant rootName))
    (pconstant ins)
    (pmint mint)

runNotAMember :: forall s. BS.ByteString -> [TxInInfo] -> Term s PBool
runNotAMember operator refs =
  poperatorIsNotAMember
    (pdata (pconstant (PubKeyHash (toBuiltin operator))))
    (pconstant refs)
    0
    (pdata (pconstant dirPolicy))
    (pdata (pconstant rootName))
    (pconstant nodePrefix)
    (pconstant nodePrefixLen)

--------------------------------------------------------------------------------
-- Slashing fixtures
--------------------------------------------------------------------------------

stateQueuePolicy :: CurrencySymbol
stateQueuePolicy = policyFor 3

settlementPolicy :: CurrencySymbol
settlementPolicy = policyFor 4

settlementAddr :: Address
settlementAddr = scriptHashAddress (ScriptHash (unCurrencySymbol settlementPolicy))

-- | The operator being slashed; also the removed node's key.
slashedOperator :: BS.ByteString
slashedOperator = "bb"

hubOutRef :: TxOutRef
hubOutRef = outRefN 9

{- | A hub oracle datum: 12 policy ids, 13 addresses, then the reserve observer.

Only two fields matter here — @state_queue@ (policy 4) and @settlement_addr@
(address 10) — but the whole shape has to be right or the positional read lands
on the wrong field.
-}
hubDatum :: BuiltinData
hubDatum =
  dataToBuiltinData $
    PD.Constr
      0
      ( [PD.B (csBytes (policyFor (0x20 + i))) | i <- [0 .. 3]]
          <> [PD.B (csBytes stateQueuePolicy)] -- 4: state_queue
          <> [PD.B (csBytes (policyFor (0x30 + i))) | i <- [0 .. 6]]
          <> replicate 10 dirAddrData
          <> [settlementAddrData] -- address 10: settlement_addr
          <> replicate 2 dirAddrData
          <> [PD.B (csBytes (policyFor 0x40))]
      )
  where
    csBytes = fromBuiltin . unCurrencySymbol
    addrOf cs = PD.Constr 0 [PD.Constr 1 [PD.B (csBytes cs)], PD.Constr 1 []]
    dirAddrData = addrOf dirPolicy
    settlementAddrData = addrOf settlementPolicy

hubRefIn :: TxInInfo
hubRefIn =
  TxInInfo
    hubOutRef
    ( TxOut
        (scriptHashAddress (ScriptHash (unCurrencySymbol hubPolicy)))
        (mkAdaValue 2_000_000 <> singleton hubPolicy hubAssetName 1)
        (OutputDatum (Datum hubDatum))
        Nothing
    )

-- | @RemoveFraudulentBlockHeader@ is constructor 3 of the state queue redeemer.
removeFraudulentHeader :: BS.ByteString -> BuiltinData
removeFraudulentHeader operator =
  dataToBuiltinData
    (PD.Constr 3 [PD.B operator, PD.B "", PD.I 0, PD.I 0, PD.I 0])

-- | @CommitBlockHeader@ is constructor 2 — the wrong branch.
commitBlockHeader :: BuiltinData
commitBlockHeader =
  dataToBuiltinData (PD.Constr 2 [PD.I 0, PD.I 0, PD.B "bb", PD.I 0, PD.I 0, PD.I 0])

badStateReason :: forall s. Term s (PAsData PSlashingReason)
badStateReason = pdata (pcon (PSlashOperatorForBadState (pdata 0)))

badSettlementReason :: forall s. Term s (PAsData PSlashingReason)
badSettlementReason =
  pdata (pcon (PSlashOperatorForBadSettlement (pdata 0) (pdata 0)))

{- | Slashing context: the directory root anchors a single node keyed by the
operator, that node's NFT is burnt, and the continued root drops the link.
-}
runSlash ::
  forall s.
  BS.ByteString ->
  BuiltinData ->
  Term s (PAsData PSlashingReason) ->
  Term s PBool
runSlash operator stateQueueRedeemer reason =
  pslashFraudulentOperatorAndGetInfo
    (pdata (pconstant hubPolicy))
    ( pcon $
        PSlashingArguments
          { pslashArgs'slashedOperator = pdata (pconstant (PubKeyHash (toBuiltin operator)))
          , pslashArgs'hubOracleRefInputIndex = pdata 0
          , pslashArgs'anchorElementInputOutref = pdata (pconstant (outRefN 0))
          , pslashArgs'anchorElementOutputIndex = pdata 0
          , pslashArgs'slashingReason = reason
          }
    )
    (pconstant [rootIn (linkTo slashedOperator), nodeRef slashedOperator linkNone])
    (pconstant [mkElemOut rootName (rootDatum linkNone)])
    (pconstant [hubRefIn])
    (pmint (toMint (singleton dirPolicy (nodeName slashedOperator) (-1))))
    0
    ( pconstant
        [(Minting stateQueuePolicy, Redeemer stateQueueRedeemer)]
    )
    (\_op _anchorKey _link _hub -> pconstant True)
    (pdata (pconstant dirPolicy))
    (pdata (pconstant rootName))
    (pconstant nodePrefix)
    (pconstant nodePrefixLen)

{- | @cross_validate_slashing_reason@ against a directory's own mint redeemer.

@reasonTag@ picks which 'PSlashingReason' the referenced set claims: 0 for a bad
state commitment, 1 for a bad settlement.
-}
runCrossValidate ::
  forall s.
  BS.ByteString ->
  BS.ByteString ->
  Integer ->
  Term s (PAsData PSlashingReason)
runCrossValidate fraudulentOperator setOperator reasonTag =
  pcrossValidateSlashingReason
    (pdata (pconstant (PubKeyHash (toBuiltin fraudulentOperator))))
    (pdata (pconstant dirPolicy))
    0
    (\rdmr -> pfromData (punsafeCoerce (pto (pfromData rdmr))))
    (pconstant [(Minting dirPolicy, Redeemer slashingArgumentsRedeemer)])
  where
    reasonData = case reasonTag of
      0 -> PD.Constr 0 [PD.I 0]
      _ -> PD.Constr 1 [PD.I 0, PD.I 0]
    -- The bare @SlashingArguments@ record, which is what the caller-supplied
    -- reader is expected to extract from its own redeemer shape.
    slashingArgumentsRedeemer =
      dataToBuiltinData $
        PD.Constr
          0
          [ PD.B setOperator
          , PD.I 0
          , builtinDataToData (toBuiltinData (outRefN 0))
          , PD.I 0
          , reasonData
          ]
