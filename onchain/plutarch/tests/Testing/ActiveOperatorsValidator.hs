{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Testing.ActiveOperatorsValidator
Description : Behavioural tests for the Plutarch port of
              @validators/operator-directory/active-operators.ak@.

This validator is the one that mutates nodes in place, so the tests concentrate
on the state transitions: the bond hold must move forward monotonically, the
strike count must increment by exactly one and stop at the ceiling, and every
removal must account for the scheduler.
-}
module Testing.ActiveOperatorsValidator (tests) where

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
  ScriptPurpose (Minting, Spending),
  TxId (..),
  TxInInfo (..),
  TxOut (..),
  TxOutRef (..),
  scriptContextTxInfo,
  toBuiltinData,
  txInfoFee,
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

import Midgard.Validators.ActiveOperators (
  activeOperatorsMintValidator,
  activeOperatorsSpendValidator,
 )
import Testing.Eval (pfails, psucceeds)
import Testing.ScriptContextBuilder (buildScriptContext, currencySymbolFromHex, mkAdaValue)

-- | Collects the tests defined in this module.
tests :: TestTree
tests =
  testGroup
    "Active Operators Validator Tests"
    [ testGroup
        "spend / ListStateTransition"
        [ testCase "permits a spend when the list policy mints" $
            psucceeds $ runListStateTransition (mintNode operator 1)
        , testCase "rejects a spend with no list-policy mint or burn" $
            pfails $ runListStateTransition (toMint (singleton otherPolicy (TokenName "X") 1))
        ]
    , testGroup
        "spend / UpdateBondHoldNewState"
        [ testCase "sets the hold to the maturity of the committed block" $
            psucceeds $ runUpdateState noHold (holdAt freshHold) 0 0 (commitHeader operator)
        , -- Monotonic: an existing later hold must survive.
          testCase "keeps an existing hold that is later than the fresh one" $
            psucceeds $ runUpdateState (holdAt laterHold) (holdAt laterHold) 0 0 (commitHeader operator)
        , testCase "rejects shortening an existing later hold" $
            pfails $ runUpdateState (holdAt laterHold) (holdAt freshHold) 0 0 (commitHeader operator)
        , testCase "rejects a hold that is not the block maturity time" $
            pfails $ runUpdateState noHold (holdAt (freshHold - 1)) 0 0 (commitHeader operator)
        , testCase "rejects altering the strike count" $
            pfails $ runUpdateState noHold (holdAt freshHold) 2 3 (commitHeader operator)
        , testCase "rejects a state-queue redeemer naming another operator" $
            pfails $ runUpdateState noHold (holdAt freshHold) 0 0 (commitHeader "zz")
        , testCase "rejects a state-queue redeemer on the wrong branch" $
            pfails $ runUpdateState noHold (holdAt freshHold) 0 0 deinitRedeemer
        , -- Exercises the other arm of `get_inclusive_upper_bound_of_interval`:
          -- a range bounded at both ends normalises to ClosedRange, not
          -- FromNegInf.
          testCase "derives the hold from a closed validity range" $
            psucceeds $ runUpdateStateIn closedHoldRange noHold (holdAt freshHold) 0 0 (commitHeader operator)
        ]
    , testGroup
        "spend / UpdateBondHoldNewSettlement"
        [ testCase "sets the hold to the claimed resolution time" $
            psucceeds $ runUpdateSettlement freshHold (holdAt freshHold) (attachClaim operator)
        , -- The redeemer's resolution time is recomputed, not trusted.
          testCase "rejects a resolution time the validity range does not imply" $
            pfails $ runUpdateSettlement (freshHold + 1) (holdAt (freshHold + 1)) (attachClaim operator)
        , testCase "rejects a settlement redeemer naming another operator" $
            pfails $ runUpdateSettlement freshHold (holdAt freshHold) (attachClaim "zz")
        , testCase "rejects a settlement redeemer on the wrong branch" $
            pfails $ runUpdateSettlement freshHold (holdAt freshHold) resolveRedeemer
        ]
    , testGroup
        "spend / StrikeForInactivity"
        [ testCase "increments the strike count by one" $
            psucceeds $ runStrike 0 1 linkNone rewindSkipped
        , testCase "accepts the final strike at the ceiling" $
            psucceeds $ runStrike 4 5 linkNone rewindSkipped
        , -- The ceiling stops an attacker pinning the node with endless strikes.
          testCase "rejects a strike beyond the maximum" $
            pfails $ runStrike 5 6 linkNone rewindSkipped
        , testCase "rejects an increment other than one" $
            pfails $ runStrike 0 2 linkNone rewindSkipped
        , testCase "rejects a link the redeemer states incorrectly" $
            pfails $ runStrike 0 1 (linkTo "ff") rewindSkipped
        , -- Both skipped-operator variants must be accepted; see the note in
          -- the validator on why their arms are written asymmetrically.
          testCase "accepts the go-to-next skipped-operator approach" $
            psucceeds $ runStrike 0 1 linkNone goToNextSkipped
        , testCase "rejects a scheduler not advancing past a skipped operator" $
            pfails $ runStrike 0 1 linkNone goToNextEndOfShift
        , testCase "rejects a scheduler skipping a different node" $
            pfails $ runStrike 0 1 linkNone rewindSkippedOtherNode
        ]
    , testGroup
        "mint / ActivateOperator"
        [ testCase "accepts a fresh node with no hold and no strikes" $
            psucceeds $ runActivate (nodeData noHold 0) True
        , testCase "rejects a node activated with an outstanding hold" $
            pfails $ runActivate (nodeData (holdAt freshHold) 0) True
        , testCase "rejects a node activated with strikes already recorded" $
            pfails $ runActivate (nodeData noHold 1) True
        , -- The registered set relies on this flag to allow immediate
          -- activation, so a false claim must not pass.
          testCase "rejects an empty-set claim that contradicts the list" $
            pfails $ runActivate (nodeData noHold 0) False
        ]
    , testGroup
        "mint / RetireOperator"
        [ testCase "accepts a voluntary retirement signed by the operator" $
            psucceeds $ runRetire 0 False [signer operator] noHold noHold schedulerIdle
        , testCase "rejects a voluntary retirement without the signature" $
            pfails $ runRetire 0 False [] noHold noHold schedulerIdle
        , testCase "accepts a forced retirement at the strike ceiling" $
            psucceeds $ runRetire 5 True [] noHold noHold schedulerIdle
        , testCase "rejects a forced retirement below the strike ceiling" $
            pfails $ runRetire 0 True [] noHold noHold schedulerIdle
        , testCase "rejects a voluntary retirement at the strike ceiling" $
            pfails $ runRetire 5 False [signer operator] noHold noHold schedulerIdle
        , -- The retired set mints against this hold, so the two must agree.
          testCase "rejects a hold the retired set's redeemer disagrees with" $
            pfails $ runRetire 0 False [signer operator] (holdAt freshHold) noHold schedulerIdle
        , testCase "rejects retiring the operator the scheduler still appoints" $
            pfails $ runRetire 0 False [signer operator] noHold noHold (schedulerAppoints operator)
        , testCase "accepts retiring while the scheduler appoints somebody else" $
            psucceeds $ runRetire 0 False [signer operator] noHold noHold (schedulerAppoints "zz")
        ]
    , testGroup
        "mint / RetireOperator, scheduler advancing"
        [ testCase "accepts a go-to-next removal advance" $
            psucceeds $ runRetireAdvancing goToNextRemoval True
        , testCase "accepts a rewind removal advance" $
            psucceeds $ runRetireAdvancing rewindRemoval True
        , testCase "rejects an advance that is not removal-driven" $
            pfails $ runRetireAdvancing goToNextEndOfShift True
        , -- The scheduler trusts this flag; the removed node's link is what
          -- makes it true.
          testCase "rejects a last-member claim the link contradicts" $
            pfails $ runRetireAdvancing goToNextRemoval False
        ]
    ]

--------------------------------------------------------------------------------
-- Fixtures
--------------------------------------------------------------------------------

policyFor :: Int -> CurrencySymbol
policyFor n = currencySymbolFromHex (replicate (56 - length h) '0' <> h)
  where
    h = showHex n ""

hubPolicy, activePolicy, regPolicy, retiredPolicy :: CurrencySymbol
schedulerPolicy, stateQueuePolicy, settlementPolicy, otherPolicy :: CurrencySymbol
hubPolicy = policyFor 1
activePolicy = policyFor 2
regPolicy = policyFor 3
retiredPolicy = policyFor 4
schedulerPolicy = policyFor 5
stateQueuePolicy = policyFor 6
settlementPolicy = policyFor 7
otherPolicy = policyFor 8

hubAssetName, schedulerAssetName, activeRootName :: TokenName
hubAssetName = TokenName "MIDGARD_HUB_ORACLE"
schedulerAssetName = TokenName "MIDGARD_SCHEDULER"
activeRootName = TokenName "MIDGARD_ACTIVE_OPERATORS"

operator :: BS.ByteString
operator = "bb"

{- | Testnet @block_maturity_duration_v1@ — three minutes in milliseconds.

The fresh hold is the validity range's upper bound plus this.
-}
blockMaturityDuration :: Integer
blockMaturityDuration = 3 * 60 * 1000

validUpperBound :: Integer
validUpperBound = 1_000

freshHold :: Integer
freshHold = validUpperBound + blockMaturityDuration

-- | A hold strictly later than 'freshHold', for the monotonicity tests.
laterHold :: Integer
laterHold = freshHold + 1_000

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

nodeName :: BS.ByteString -> TokenName
nodeName key = TokenName (toBuiltin ("MACT" <> key))

activeRootOut :: PD.Data -> TxOut
activeRootOut link = mkElemOut activePolicy activeRootName (rootDatum link)

activeNodeOut :: BS.ByteString -> PD.Data -> PD.Data -> TxOut
activeNodeOut key d link = mkElemOut activePolicy (nodeName key) (nodeDatum d link)

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

nodeDatum :: PD.Data -> PD.Data -> BuiltinData
nodeDatum d = mkElement (PD.Constr 1 [d])

-- | @active_operators.NodeData { bond_unlock_time, inactivity_strikes }@.
nodeData :: PD.Data -> Integer -> PD.Data
nodeData bondUnlockTime strikes = PD.Constr 0 [bondUnlockTime, PD.I strikes]

noHold :: PD.Data
noHold = PD.Constr 1 []

holdAt :: Integer -> PD.Data
holdAt t = PD.Constr 0 [PD.I t]

linkNone :: PD.Data
linkNone = PD.Constr 1 []

linkTo :: BS.ByteString -> PD.Data
linkTo key = PD.Constr 0 [PD.B key]

toMint :: Value -> MintValue
toMint = UnsafeMintValue . getValue

mintNode :: BS.ByteString -> Integer -> MintValue
mintNode key q = toMint (singleton activePolicy (nodeName key) q)

noMint :: MintValue
noMint = toMint mempty

{- | A range bounded at /both/ ends, still at 'validUpperBound' above.

Normalises to @ClosedRange@ where 'holdRange' normalises to @FromNegInf@, so the
two together cover both arms of @get_inclusive_upper_bound_of_interval@.
-}
closedHoldRange :: Interval POSIXTime
closedHoldRange =
  Interval
    (LowerBound (Finite (POSIXTime 0)) True)
    (UpperBound (Finite (POSIXTime validUpperBound)) True)

-- | A range bounded above at 'validUpperBound' — the bond hold derives from it.
holdRange :: Interval POSIXTime
holdRange =
  Interval
    (LowerBound NegInf True)
    (UpperBound (Finite (POSIXTime validUpperBound)) True)

--------------------------------------------------------------------------------
-- Hub oracle and scheduler
--------------------------------------------------------------------------------

schedulerAddrData, settlementAddrData :: PD.Data
schedulerAddrData = addrOf schedulerPolicy
settlementAddrData = addrOf settlementPolicy

addrOf :: CurrencySymbol -> PD.Data
addrOf cs =
  PD.Constr 0 [PD.Constr 1 [PD.B (fromBuiltin (unCurrencySymbol cs))], PD.Constr 1 []]

{- | A hub oracle datum: 12 policy ids, 13 addresses, then the reserve observer.

Four fields are read by the branches under test — @scheduler@ (3),
@state_queue@ (4), @scheduler_addr@ (address 3) and @settlement_addr@
(address 10) — but the whole shape has to be right or the positional reads land
on the wrong fields.
-}
hubDatum :: BuiltinData
hubDatum =
  dataToBuiltinData $
    PD.Constr
      0
      ( [ PD.B (csBytes regPolicy) -- 0: registered_operators
        , PD.B (csBytes activePolicy) -- 1: active_operators
        , PD.B (csBytes retiredPolicy) -- 2: retired_operators
        , PD.B (csBytes schedulerPolicy) -- 3: scheduler
        , PD.B (csBytes stateQueuePolicy) -- 4: state_queue
        ]
          <> [PD.B (csBytes (policyFor (0x30 + i))) | i <- [0 .. 6]]
          <> replicate 3 (addrOf activePolicy)
          <> [schedulerAddrData] -- address 3: scheduler_addr
          <> replicate 6 (addrOf activePolicy)
          <> [settlementAddrData] -- address 10: settlement_addr
          <> replicate 2 (addrOf activePolicy)
          <> [PD.B (csBytes (policyFor 0x40))]
      )
  where
    csBytes = fromBuiltin . unCurrencySymbol

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

-- | @SchedDatum.NoActiveOperators@ — nobody is appointed.
schedulerIdle :: PD.Data
schedulerIdle = PD.Constr 0 []

-- | @SchedDatum.ActiveOperator { operator, start_time }@.
schedulerAppoints :: BS.ByteString -> PD.Data
schedulerAppoints op = PD.Constr 1 [PD.B op, PD.I 0]

schedulerRefIn :: PD.Data -> TxInInfo
schedulerRefIn datum =
  TxInInfo
    (outRefN 8)
    ( TxOut
        (scriptHashAddress (ScriptHash (unCurrencySymbol schedulerPolicy)))
        (mkAdaValue 2_000_000 <> singleton schedulerPolicy schedulerAssetName 1)
        (OutputDatum (Datum (dataToBuiltinData datum)))
        Nothing
    )

-- | A scheduler UTxO spent in the transaction, at the scheduler's address.
schedulerIn :: TxInInfo
schedulerIn =
  TxInInfo
    (outRefN 7)
    ( TxOut
        (scriptHashAddress (ScriptHash (unCurrencySymbol schedulerPolicy)))
        (mkAdaValue 2_000_000 <> singleton schedulerPolicy schedulerAssetName 1)
        (OutputDatum (Datum (dataToBuiltinData schedulerIdle)))
        Nothing
    )

{- | @scheduler.SpendRedeemer@ — a record, so @Constr 0@.

@scheduler_input_index@ is 1: the scheduler is the second input in the
strike transactions below.
-}
schedulerSpendRedeemer :: PD.Data -> BuiltinData
schedulerSpendRedeemer approach =
  dataToBuiltinData (PD.Constr 0 [PD.I 1, PD.I 0, approach])

{- | @RewindDueToSkippedOperator@ is constructor 3 of @AdvancingApproach@; its
second field is the skipped operator's node input index, which must be 0.
-}
rewindSkipped :: PD.Data
rewindSkipped =
  PD.Constr 3 [PD.I 0, PD.I 0, PD.I 0, PD.I 0, PD.I 0, PD.Constr 1 [], PD.I 0, PD.Constr 0 []]

{- | @GoToNextDueToSkippedOperator@ is constructor 2; its second field is the
skipped operator's node input index.
-}
goToNextSkipped :: PD.Data
goToNextSkipped = PD.Constr 2 [PD.I 0, PD.I 0, PD.I 0, PD.I 0, PD.I 0, PD.Constr 0 []]

-- | A rewind naming a node other than the one being struck.
rewindSkippedOtherNode :: PD.Data
rewindSkippedOtherNode =
  PD.Constr 3 [PD.I 0, PD.I 9, PD.I 0, PD.I 0, PD.I 0, PD.Constr 1 [], PD.I 0, PD.Constr 0 []]

-- | @GoToNextDueToOperatorRemoval@ is constructor 4.
goToNextRemoval :: PD.Data
goToNextRemoval = PD.Constr 4 [PD.I 0, PD.Constr 0 []]

-- | @RewindDueToOperatorRemoval@ is constructor 5.
rewindRemoval :: PD.Data
rewindRemoval = PD.Constr 5 [PD.I 0, PD.Constr 1 [], PD.Constr 0 [], PD.I 0]

-- | @GoToNextDueToEndOfShift@ is constructor 0 — not a skipped-operator branch.
goToNextEndOfShift :: PD.Data
goToNextEndOfShift = PD.Constr 0 [PD.I 0]

--------------------------------------------------------------------------------
-- Other scripts' redeemers
--------------------------------------------------------------------------------

-- | @state_queue.CommitBlockHeader@ is constructor 2; its third field is the
-- operator.
commitHeader :: BS.ByteString -> BuiltinData
commitHeader op =
  dataToBuiltinData (PD.Constr 2 [PD.I 0, PD.I 0, PD.B op, PD.I 0, PD.I 0, PD.I 0])

-- | @state_queue.Deinit@ is constructor 1 — the wrong branch.
deinitRedeemer :: BuiltinData
deinitRedeemer = dataToBuiltinData (PD.Constr 1 [])

-- | @settlement.AttachResolutionClaim@ is constructor 0; the operator is its
-- sixth field.
attachClaim :: BS.ByteString -> BuiltinData
attachClaim op =
  dataToBuiltinData
    (PD.Constr 0 [PD.I 0, PD.I 0, PD.I 0, PD.I 0, PD.I 0, PD.B op, PD.I 0])

-- | @settlement.Resolve@ is constructor 2 — the wrong branch.
resolveRedeemer :: BuiltinData
resolveRedeemer = dataToBuiltinData (PD.Constr 2 [PD.I 0])

{- | @registered_operators.ActivateOperator@ is constructor 3; the activating
operator is its first field.
-}
registeredActivate :: BS.ByteString -> BuiltinData
registeredActivate op =
  dataToBuiltinData
    ( PD.Constr
        3
        [ PD.B op
        , builtinDataToData (toBuiltinData (outRefN 0))
        , PD.I 0
        , PD.I 0
        , PD.I 0
        , PD.I 0
        ]
    )

{- | @retired_operators.RetireOperator@ is constructor 2; the retiring operator
is its first field and the bond unlock time its second.
-}
retiredRetire :: BS.ByteString -> PD.Data -> BuiltinData
retiredRetire op bondUnlockTime =
  dataToBuiltinData
    (PD.Constr 2 [PD.B op, bondUnlockTime, PD.I 0, PD.I 0, PD.I 0, PD.I 0])

--------------------------------------------------------------------------------
-- Applying the terms
--------------------------------------------------------------------------------

-- | Builds a spending 'ScriptContext' around the fields these tests vary.
spendCtx ::
  forall s.
  Interval POSIXTime ->
  BuiltinData ->
  TxOutRef ->
  ( [TxInInfo]
  , [TxOut]
  , [TxInInfo]
  , MintValue
  , [(ScriptPurpose, Redeemer)]
  ) ->
  Term s PUnit
spendCtx validRange redeemer ownOutRef (ins, outs, refs, mint, redeemers) =
  activeOperatorsSpendValidator
    # pdata (pconstant activePolicy)
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
        , txInfoRedeemers = Map.unsafeFromList redeemers
        }
    ctx = ScriptContext txInfo (Redeemer redeemer) (SpendingScript ownOutRef Nothing)

runListStateTransition :: forall s. MintValue -> Term s PUnit
runListStateTransition mint =
  spendCtx holdRange (dataToBuiltinData (PD.Constr 0 [])) (outRefN 0) ([], [], [], mint, [])

{- | A bond-hold update: one active node spent and reproduced in place.

@inHold@/@outHold@ are the node's bond unlock time before and after;
@inStrikes@/@outStrikes@ its strike count.
-}
runUpdateState ::
  forall s.
  PD.Data ->
  PD.Data ->
  Integer ->
  Integer ->
  BuiltinData ->
  Term s PUnit
runUpdateState = runUpdateStateIn holdRange

runUpdateStateIn ::
  forall s.
  Interval POSIXTime ->
  PD.Data ->
  PD.Data ->
  Integer ->
  Integer ->
  BuiltinData ->
  Term s PUnit
runUpdateStateIn validRange inHold outHold inStrikes outStrikes stateQueueRedeemer =
  spendCtx
    validRange
    updateRedeemer
    (outRefN 0)
    ( [TxInInfo (outRefN 0) (activeNodeOut operator (nodeData inHold inStrikes) linkNone)]
    , [activeNodeOut operator (nodeData outHold outStrikes) linkNone]
    , [hubRefIn]
    , noMint
    , [(Minting stateQueuePolicy, Redeemer stateQueueRedeemer)]
    )
  where
    -- @UpdateBondHoldNewState@ is constructor 1.
    updateRedeemer =
      dataToBuiltinData
        (PD.Constr 1 [PD.B operator, PD.I 0, PD.I 0, PD.I 0, PD.I 0])

{- | A resolution-claim bond hold. The settlement UTxO is the second input, so
the spending redeemer can be located against the hub's settlement address.
-}
runUpdateSettlement ::
  forall s.
  Integer ->
  PD.Data ->
  BuiltinData ->
  Term s PUnit
runUpdateSettlement resolutionTime outHold settlementRedeemer =
  spendCtx
    holdRange
    updateRedeemer
    (outRefN 0)
    ( [ TxInInfo (outRefN 0) (activeNodeOut operator (nodeData noHold 0) linkNone)
      , settlementIn
      ]
    , [activeNodeOut operator (nodeData outHold 0) linkNone]
    , [hubRefIn]
    , noMint
    , [(Spending (outRefN 5), Redeemer settlementRedeemer)]
    )
  where
    settlementIn =
      TxInInfo
        (outRefN 5)
        ( TxOut
            (scriptHashAddress (ScriptHash (unCurrencySymbol settlementPolicy)))
            (mkAdaValue 2_000_000)
            NoOutputDatum
            Nothing
        )
    -- @UpdateBondHoldNewSettlement@ is constructor 2.
    updateRedeemer =
      dataToBuiltinData
        ( PD.Constr
            2
            [PD.B operator, PD.I 0, PD.I 0, PD.I 0, PD.I 1, PD.I 0, PD.I resolutionTime]
        )

{- | An inactivity strike: the node is reproduced with one more strike while the
scheduler is spent in the same transaction, advancing past this operator.
-}
runStrike ::
  forall s.
  Integer ->
  Integer ->
  PD.Data ->
  PD.Data ->
  Term s PUnit
runStrike inStrikes outStrikes redeemerLink advancingApproach =
  spendCtx
    holdRange
    strikeRedeemer
    (outRefN 0)
    ( [ TxInInfo (outRefN 0) (activeNodeOut operator (nodeData noHold inStrikes) linkNone)
      , schedulerIn
      ]
    , [activeNodeOut operator (nodeData noHold outStrikes) linkNone]
    , [hubRefIn]
    , noMint
    , [(Spending (outRefN 7), Redeemer (schedulerSpendRedeemer advancingApproach))]
    )
  where
    -- @StrikeForInactivity@ is constructor 3.
    strikeRedeemer =
      dataToBuiltinData
        ( PD.Constr
            3
            [PD.I 0, PD.I 0, PD.B operator, redeemerLink, PD.I 1, PD.I 0, PD.I 0]
        )

-- | Builds a minting 'ScriptContext' around the fields these tests vary.
mintCtx ::
  forall s.
  BuiltinData ->
  ( [TxInInfo]
  , [TxOut]
  , [TxInInfo]
  , MintValue
  , [PubKeyHash]
  , [(ScriptPurpose, Redeemer)]
  ) ->
  Term s PUnit
mintCtx redeemer (ins, outs, refs, mint, signatories, redeemers) =
  activeOperatorsMintValidator
    # pdata (pconstant hubPolicy)
    # pdata (pconstant regPolicy)
    # pdata (pconstant retiredPolicy)
    # pconstant ctx
  where
    base = buildScriptContext mempty
    txInfo =
      (scriptContextTxInfo base)
        { txInfoInputs = ins
        , txInfoOutputs = outs
        , txInfoReferenceInputs = refs
        , txInfoMint = mint
        , txInfoValidRange = holdRange
        , txInfoSignatories = signatories
        , txInfoRedeemers = Map.unsafeFromList redeemers
        , txInfoFee = 0
        }
    ctx = ScriptContext txInfo (Redeemer redeemer) (MintingScript activePolicy)

{- | An activation into an empty active set: the root gains one node, and the
registered set's own @ActivateOperator@ redeemer names the same operator.
-}
runActivate :: forall s. PD.Data -> Bool -> Term s PUnit
runActivate insertedNodeData setWasEmpty =
  mintCtx
    activateRedeemer
    ( [TxInInfo (outRefN 0) (activeRootOut linkNone)]
    , [activeRootOut (linkTo operator), activeNodeOut operator insertedNodeData linkNone]
    , []
    , mintNode operator 1
    , []
    , [(Minting regPolicy, Redeemer (registeredActivate operator))]
    )
  where
    -- @ActivateOperator@ is constructor 2.
    activateRedeemer =
      dataToBuiltinData
        ( PD.Constr
            2
            [ PD.B operator
            , PD.I 0 -- anchor_element_output_index
            , PD.I 1 -- inserted_node_output_index
            , PD.I 0 -- registered_operators_redeemer_index
            , PD.Constr (if setWasEmpty then 1 else 0) []
            ]
        )

{- | A retirement: the node is burnt and the retired set mints its counterpart.

The scheduler is discharged by reference (@ShowOperatorIsInactive@), so
@schedulerDatum@ decides whether it still appoints this operator.
-}
runRetire ::
  forall s.
  Integer ->
  Bool ->
  [PubKeyHash] ->
  PD.Data ->
  PD.Data ->
  PD.Data ->
  Term s PUnit
runRetire strikes penalize signatories nodeHold retiredRedeemerHold schedulerDatum =
  mintCtx
    retireRedeemer
    ( [ TxInInfo (outRefN 0) (activeRootOut (linkTo operator))
      , TxInInfo (outRefN 1) (activeNodeOut operator (nodeData nodeHold strikes) linkNone)
      ]
    , [activeRootOut linkNone]
    , [hubRefIn, schedulerRefIn schedulerDatum]
    , mintNode operator (-1)
    , signatories
    , [(Minting retiredPolicy, Redeemer (retiredRetire operator retiredRedeemerHold))]
    )
  where
    -- @RetireOperator@ is constructor 3; @ShowOperatorIsInactive@ is 0, and the
    -- scheduler reference input sits at index 1.
    retireRedeemer =
      dataToBuiltinData
        ( PD.Constr
            3
            [ PD.B operator
            , PD.I 0 -- hub_oracle_ref_input_index
            , builtinDataToData (toBuiltinData (outRefN 0))
            , PD.I 0 -- anchor_element_output_index
            , PD.I 0 -- retired_operators_redeemer_index
            , PD.Constr (if penalize then 1 else 0) []
            , PD.Constr 0 [PD.I 1]
            ]
        )


{- | A retirement discharged by spending the scheduler rather than referencing
it: the scheduler is the third input, carrying an @ActiveOperator@ datum naming
this operator, and its redeemer advances past the removal.
-}
runRetireAdvancing :: forall s. PD.Data -> Bool -> Term s PUnit
runRetireAdvancing advancingApproach isLastMember =
  mintCtx
    retireRedeemer
    ( [ TxInInfo (outRefN 0) (activeRootOut (linkTo operator))
      , TxInInfo (outRefN 1) (activeNodeOut operator (nodeData noHold 0) linkNone)
      , schedulerInWith (schedulerAppoints operator)
      ]
    , [activeRootOut linkNone]
    , [hubRefIn]
    , mintNode operator (-1)
    , [signer operator]
    , [ (Minting retiredPolicy, Redeemer (retiredRetire operator noHold))
      , (Spending (outRefN 7), Redeemer (schedulerSpendRedeemerAt 2 advancingApproach))
      ]
    )
  where
    -- @ShowSchedulerIsAdvancing@ is constructor 1 of the scheduler sync: the
    -- scheduler is input 2, its redeemer is entry 1, the anchor is the root
    -- (so no key), and the removed node has no link.
    retireRedeemer =
      dataToBuiltinData
        ( PD.Constr
            3
            [ PD.B operator
            , PD.I 0
            , builtinDataToData (toBuiltinData (outRefN 0))
            , PD.I 0
            , PD.I 0
            , PD.Constr 0 []
            , PD.Constr 1 [PD.I 2, PD.I 1, PD.Constr 1 [], PD.Constr (if isLastMember then 1 else 0) []]
            ]
        )

-- | A scheduler UTxO with a chosen datum, spent in the transaction.
schedulerInWith :: PD.Data -> TxInInfo
schedulerInWith datum =
  TxInInfo
    (outRefN 7)
    ( TxOut
        (scriptHashAddress (ScriptHash (unCurrencySymbol schedulerPolicy)))
        (mkAdaValue 2_000_000 <> singleton schedulerPolicy schedulerAssetName 1)
        (OutputDatum (Datum (dataToBuiltinData datum)))
        Nothing
    )

-- | 'schedulerSpendRedeemer' with an explicit scheduler input index.
schedulerSpendRedeemerAt :: Integer -> PD.Data -> BuiltinData
schedulerSpendRedeemerAt ix approach =
  dataToBuiltinData (PD.Constr 0 [PD.I ix, PD.I 0, approach])
