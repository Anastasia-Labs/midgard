{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Testing.RegisteredOperatorsValidator
Description : Behavioural tests for the Plutarch port of
              @validators/operator-directory/registered-operators.ak@.

Two things are specific to this validator and get the attention: the node key is
a derived value — @registration_duration@ past the validity range's upper bound,
big-endian encoded — and activation is gated on that key having elapsed, with an
empty-active-set escape hatch. The rest of each branch is linked-list machinery
covered in "Testing.LinkedList".
-}
module Testing.RegisteredOperatorsValidator (tests) where

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
  ScriptPurpose (Minting),
  TxId (..),
  TxInInfo (..),
  TxOut (..),
  TxOutRef (..),
  scriptContextTxInfo,
  toBuiltinData,
  txInfoInputs,
  txInfoMint,
  txInfoOutputs,
  txInfoRedeemers,
  txInfoReferenceInputs,
  txInfoSignatories,
  txInfoValidRange,
 )
import PlutusLedgerApi.V3.MintValue (MintValue (UnsafeMintValue))
import PlutusTx.AssocMap qualified as Map
import PlutusTx.Builtins (BuiltinData, builtinDataToData, dataToBuiltinData, fromBuiltin, toBuiltin)
import Test.Tasty
import Test.Tasty.HUnit

import Plutarch.Prelude

import Midgard.Validators.RegisteredOperators (
  registeredOperatorsMintValidator,
  registeredOperatorsSpendValidator,
 )
import Testing.Eval (pfails, psucceeds)
import Testing.ScriptContextBuilder (buildScriptContext, currencySymbolFromHex, mkAdaValue)

-- | Collects the tests defined in this module.
tests :: TestTree
tests =
  testGroup
    "Registered Operators Validator Tests"
    [ testGroup
        "spend"
        [ testCase "permits a spend when the list policy mints" $
            psucceeds $ runSpend (mintNode registeredKey 1)
        , testCase "rejects a spend with no list-policy mint or burn" $
            pfails $ runSpend (toMint (singleton otherPolicy (TokenName "X") 1))
        ]
    , testGroup
        "mint / RegisterOperator"
        [ testCase "accepts a registration keyed to the derived activation time" $
            psucceeds $ runRegister operator activationKey operator [signer operator]
        , -- 70 + registration_duration is 100, so 200 is a key the operator
          -- chose rather than one the validity range implies.
          testCase "rejects a node key that is not the derived activation time" $
            pfails $ runRegister operator (keyFor 200) operator [signer operator]
        , testCase "rejects a registration without the operator's signature" $
            pfails $ runRegister operator activationKey operator []
        , testCase "rejects a registration signed by somebody else" $
            pfails $ runRegister operator activationKey operator [signer "zz"]
        , testCase "rejects a node whose data names a different operator" $
            pfails $ runRegister operator activationKey "zz" [signer operator]
        ]
    , testGroup
        "mint / ActivateOperator"
        [ testCase "accepts activation once the activation time has passed" $
            psucceeds $ runActivate operator activationKey (rangeFrom 200) False operator
        , testCase "rejects activation before the activation time" $
            pfails $ runActivate operator activationKey (rangeFrom 50) False operator
        , -- The liveness escape hatch: the earliest registered operator (no
          -- link) may activate immediately into an empty active set.
          testCase "accepts early activation of the earliest node when the active set is empty" $
            psucceeds $ runActivate operator activationKey (rangeFrom 50) True operator
        , testCase "rejects an active-set redeemer naming a different operator" $
            pfails $ runActivate operator activationKey (rangeFrom 200) False "zz"
        ]
    , testGroup
        "mint / DeregisterOperator"
        [ testCase "accepts deregistration signed by the operator" $
            psucceeds $ runDeregister operator [signer operator]
        , testCase "rejects deregistration without a signature" $
            pfails $ runDeregister operator []
        , testCase "rejects deregistering a node belonging to another operator" $
            pfails $ runDeregister "zz" [signer "zz"]
        ]
    , testGroup
        "mint / SlashDuplicateOperator"
        [ testCase "accepts a slash backed by a duplicate registered node" $
            psucceeds $ runSlashDuplicate operator operator
        , testCase "rejects a duplicate node naming a different operator" $
            pfails $ runSlashDuplicate operator "zz"
        ]
    ]

--------------------------------------------------------------------------------
-- Fixtures
--------------------------------------------------------------------------------

policyFor :: Int -> CurrencySymbol
policyFor n = currencySymbolFromHex (replicate (56 - length h) '0' <> h)
  where
    h = showHex n ""

hubPolicy, regPolicy, activePolicy, retiredPolicy, otherPolicy :: CurrencySymbol
hubPolicy = policyFor 1
regPolicy = policyFor 2
activePolicy = policyFor 3
retiredPolicy = policyFor 4
otherPolicy = policyFor 5

hubAssetName :: TokenName
hubAssetName = TokenName "MIDGARD_HUB_ORACLE"

regRootName, activeRootName, retiredRootName :: TokenName
regRootName = TokenName "MIDGARD_REGISTERED_OPERATORS"
activeRootName = TokenName "MIDGARD_ACTIVE_OPERATORS"
retiredRootName = TokenName "MIDGARD_RETIRED_OPERATORS"

operator :: BS.ByteString
operator = "bb"

{- | The node key the validity range below implies.

@env.registration_duration@ is 30 and 'validUpperBound' is 70, so the activation
time is 100 and its minimal big-endian encoding is the single byte @0x64@.
-}
activationKey :: BS.ByteString
activationKey = keyFor 100

-- | Minimal big-endian encoding, matching @integer_to_bytearray(True, 0, n)@.
keyFor :: Integer -> BS.ByteString
keyFor = BS.pack . reverse . go
  where
    go 0 = []
    go n = fromIntegral (n `mod` 256) : go (n `div` 256)

-- | The inclusive upper bound every registration transaction below claims.
validUpperBound :: Integer
validUpperBound = 70

registeredKey :: BS.ByteString
registeredKey = activationKey

--------------------------------------------------------------------------------
-- List elements
--------------------------------------------------------------------------------

mkElemOut :: CurrencySymbol -> TokenName -> BuiltinData -> TxOut
mkElemOut policy tn dat =
  TxOut
    (scriptHashAddress (ScriptHash (unCurrencySymbol policy)))
    (mkAdaValue 2_000_000 <> singleton policy tn 1)
    (OutputDatum (Datum dat))
    Nothing

nodeName :: BS.ByteString -> BS.ByteString -> TokenName
nodeName prefix key = TokenName (toBuiltin (prefix <> key))

regRootOut :: PD.Data -> TxOut
regRootOut link = mkElemOut regPolicy regRootName (rootDatum link)

-- | A registered-set node: its key is an activation time, its data an operator.
regNodeOut :: BS.ByteString -> BS.ByteString -> PD.Data -> TxOut
regNodeOut key op link =
  mkElemOut regPolicy (nodeName "MREG" key) (nodeDatum (nodeData op) link)

outRefN :: Integer -> TxOutRef
outRefN = TxOutRef (TxId "0101010101010101010101010101010101010101010101010101010101010101")

signer :: BS.ByteString -> PubKeyHash
signer = PubKeyHash . toBuiltin

--------------------------------------------------------------------------------
-- Datums
--------------------------------------------------------------------------------

mkElement :: PD.Data -> PD.Data -> BuiltinData
mkElement elementData link = dataToBuiltinData (PD.Constr 0 [elementData, link])

-- | @Root { data = env.empty_data }@.
rootDatum :: PD.Data -> BuiltinData
rootDatum = mkElement (PD.Constr 0 [PD.B ""])

nodeDatum :: PD.Data -> PD.Data -> BuiltinData
nodeDatum d = mkElement (PD.Constr 1 [d])

-- | @registered_operators.NodeData { operator }@ — a record, so @Constr 0@.
nodeData :: BS.ByteString -> PD.Data
nodeData op = PD.Constr 0 [PD.B op]

linkNone :: PD.Data
linkNone = PD.Constr 1 []

linkTo :: BS.ByteString -> PD.Data
linkTo key = PD.Constr 0 [PD.B key]

toMint :: Value -> MintValue
toMint = UnsafeMintValue . getValue

mintNode :: BS.ByteString -> Integer -> MintValue
mintNode key q = toMint (singleton regPolicy (nodeName "MREG" key) q)

-- | A validity range whose lower bound is @t@ and which is unbounded above.
rangeFrom :: Integer -> Interval POSIXTime
rangeFrom t = Interval (LowerBound (Finite (POSIXTime t)) True) (UpperBound PosInf True)

{- | A range bounded above at 'validUpperBound' — registration derives the node
key from this bound, so it must be finite.
-}
registrationRange :: Interval POSIXTime
registrationRange =
  Interval
    (LowerBound NegInf True)
    (UpperBound (Finite (POSIXTime validUpperBound)) True)

--------------------------------------------------------------------------------
-- Hub oracle
--------------------------------------------------------------------------------

{- | A hub oracle datum: 12 policy ids, 13 addresses, then the reserve observer.

Only @active_operators@ (policy 1) is read here, but the whole shape has to be
right or the positional read lands on the wrong field.
-}
hubDatum :: BuiltinData
hubDatum =
  dataToBuiltinData $
    PD.Constr
      0
      ( [PD.B (csBytes regPolicy)]
          <> [PD.B (csBytes activePolicy)] -- 1: active_operators
          <> [PD.B (csBytes retiredPolicy)]
          <> [PD.B (csBytes (policyFor (0x30 + i))) | i <- [0 .. 8]]
          <> replicate 13 addrData
          <> [PD.B (csBytes (policyFor 0x40))]
      )
  where
    csBytes = fromBuiltin . unCurrencySymbol
    addrData = PD.Constr 0 [PD.Constr 1 [PD.B (csBytes regPolicy)], PD.Constr 1 []]

hubRefIn :: TxInInfo
hubRefIn =
  TxInInfo
    (outRefN 9)
    ( TxOut
        (scriptHashAddress (ScriptHash (unCurrencySymbol hubPolicy)))
        (mkAdaValue 2_000_000 <> singleton hubPolicy hubAssetName 1)
        (OutputDatum (Datum hubDatum))
        Nothing
    )

{- | An empty list's root, referenced as a non-membership proof.

A root with no link proves every operator absent: the gap it spans is unbounded
at both ends.
-}
emptyRootRef :: Integer -> CurrencySymbol -> TokenName -> TxInInfo
emptyRootRef n policy rootName =
  TxInInfo (outRefN n) (mkElemOut policy rootName (rootDatum linkNone))

--------------------------------------------------------------------------------
-- Applying the terms
--------------------------------------------------------------------------------

-- | Builds a minting 'ScriptContext' around the seven fields these tests vary.
mintCtx ::
  forall s.
  BuiltinData ->
  ( [TxInInfo]
  , [TxOut]
  , [TxInInfo]
  , MintValue
  , Interval POSIXTime
  , [PubKeyHash]
  , [(ScriptPurpose, Redeemer)]
  ) ->
  Term s PUnit
mintCtx redeemer (ins, outs, refs, mint, validRange, signatories, redeemers) =
  registeredOperatorsMintValidator
    # pdata (pconstant retiredPolicy)
    # pdata (pconstant hubPolicy)
    # pconstant ctx
  where
    base = buildScriptContext mempty
    txInfo =
      (scriptContextTxInfo base)
        { txInfoInputs = ins
        , txInfoOutputs = outs
        , txInfoReferenceInputs = refs
        , txInfoMint = mint
        , txInfoValidRange = validRange
        , txInfoSignatories = signatories
        , txInfoRedeemers = Map.unsafeFromList redeemers
        }
    ctx = ScriptContext txInfo (Redeemer redeemer) (MintingScript regPolicy)

runSpend :: forall s. MintValue -> Term s PUnit
runSpend mint =
  registeredOperatorsSpendValidator
    # pdata (pconstant regPolicy)
    # pconstant ctx
  where
    base = buildScriptContext mempty
    ctx =
      ScriptContext
        ((scriptContextTxInfo base) {txInfoMint = mint})
        (Redeemer (toBuiltinData ()))
        (SpendingScript (outRefN 0) Nothing)

{- | A registration: the empty registered list's root gains one node.

Reference inputs are the hub oracle, then an empty active list and an empty
retired list — the two non-membership proofs.
-}
runRegister ::
  forall s.
  BS.ByteString ->
  BS.ByteString ->
  BS.ByteString ->
  [PubKeyHash] ->
  Term s PUnit
runRegister registeringOperator nodeKey nodeOperator signatories =
  mintCtx
    registerRedeemer
    ( [TxInInfo (outRefN 0) (regRootOut linkNone)]
    , [regRootOut (linkTo nodeKey), regNodeOut nodeKey nodeOperator linkNone]
    , [hubRefIn, emptyRootRef 1 activePolicy activeRootName, emptyRootRef 2 retiredPolicy retiredRootName]
    , mintNode nodeKey 1
    , registrationRange
    , signatories
    , []
    )
  where
    -- @RegisterOperator@ is constructor 2.
    registerRedeemer =
      dataToBuiltinData $
        PD.Constr
          2
          [ PD.B registeringOperator
          , PD.I 0 -- root_output_index
          , PD.I 1 -- registered_node_output_index
          , PD.I 0 -- hub_oracle_ref_input_index
          , PD.I 1 -- active_operators_element_ref_input_index
          , PD.I 2 -- retired_operators_element_ref_input_index
          ]

{- | An activation: the list's only node is burnt and the root drops its link.

The active set's own @ActivateOperator@ redeemer is present in the transaction,
which is where the operator being activated and the empty-set claim come from.
-}
runActivate ::
  forall s.
  BS.ByteString ->
  BS.ByteString ->
  Interval POSIXTime ->
  Bool ->
  BS.ByteString ->
  Term s PUnit
runActivate activatingOperator nodeKey validRange setWasEmpty activeRedeemerOperator =
  mintCtx
    activateRedeemer
    ( [ TxInInfo (outRefN 0) (regRootOut (linkTo nodeKey))
      , TxInInfo (outRefN 1) (regNodeOut nodeKey activatingOperator linkNone)
      ]
    , [regRootOut linkNone]
    , [hubRefIn, emptyRootRef 2 retiredPolicy retiredRootName]
    , mintNode nodeKey (-1)
    , validRange
    , []
    , [(Minting activePolicy, Redeemer activeMintRedeemer)]
    )
  where
    -- @ActivateOperator@ is constructor 3 of the registered redeemer.
    activateRedeemer =
      dataToBuiltinData $
        PD.Constr
          3
          [ PD.B activatingOperator
          , builtinDataToData (toBuiltinData (outRefN 0))
          , PD.I 0 -- anchor_element_output_index
          , PD.I 0 -- hub_oracle_ref_input_index
          , PD.I 1 -- retired_operators_element_ref_input_index
          , PD.I 0 -- active_operators_redeemer_index
          ]
    -- @ActivateOperator@ is constructor 2 of the *active* set's redeemer.
    activeMintRedeemer =
      dataToBuiltinData $
        PD.Constr
          2
          [ PD.B activeRedeemerOperator
          , PD.I 0
          , PD.I 0
          , PD.I 0
          , PD.Constr (if setWasEmpty then 1 else 0) []
          ]

-- | A deregistration: the same removal, gated only on the operator's signature.
runDeregister :: forall s. BS.ByteString -> [PubKeyHash] -> Term s PUnit
runDeregister deregisteringOperator signatories =
  mintCtx
    deregisterRedeemer
    ( [ TxInInfo (outRefN 0) (regRootOut (linkTo activationKey))
      , TxInInfo (outRefN 1) (regNodeOut activationKey operator linkNone)
      ]
    , [regRootOut linkNone]
    , []
    , mintNode activationKey (-1)
    , rangeFrom 200
    , signatories
    , []
    )
  where
    -- @DeregisterOperator@ is constructor 4.
    deregisterRedeemer =
      dataToBuiltinData $
        PD.Constr
          4
          [ PD.B deregisteringOperator
          , builtinDataToData (toBuiltinData (outRefN 0))
          , PD.I 0
          ]

{- | A duplicate slash: the operator's node is removed, and a second registered
node naming the same operator is referenced as the evidence.
-}
runSlashDuplicate :: forall s. BS.ByteString -> BS.ByteString -> Term s PUnit
runSlashDuplicate slashedOperator duplicateNodeOperator =
  mintCtx
    slashRedeemer
    ( [ TxInInfo (outRefN 0) (regRootOut (linkTo activationKey))
      , TxInInfo (outRefN 1) (regNodeOut activationKey operator linkNone)
      ]
    , [regRootOut linkNone]
    , [TxInInfo (outRefN 3) (regNodeOut (keyFor 200) duplicateNodeOperator linkNone)]
    , mintNode activationKey (-1)
    , rangeFrom 200
    , []
    , []
    )
  where
    -- @SlashDuplicateOperator@ is constructor 5; @DuplicateIsRegistered@ is 0.
    slashRedeemer =
      dataToBuiltinData $
        PD.Constr
          5
          [ PD.B slashedOperator
          , builtinDataToData (toBuiltinData (outRefN 0))
          , PD.I 0 -- anchor_element_output_index
          , PD.I 0 -- duplicate_node_ref_input_index
          , PD.Constr 0 []
          ]
