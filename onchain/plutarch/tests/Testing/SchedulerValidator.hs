{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Testing.SchedulerValidator
Description : Behavioural tests for the Plutarch port of
              @validators/scheduler.ak@.

The scheduler's seven advancing branches differ in /what they must exhibit/, not
in what they compute, so the tests are organised the same way: one accepting
transaction per branch, then one rejection per condition that branch is the only
place enforcing.

Two families of condition get the most attention, because they are where an
attacker has room. The time conditions decide when a turn may pass at all, and
the two anchors are deliberately different — end-of-shift advancement takes the
validity range's lower bound, everything else the upper. And the cross-script
agreements decide whether the scheduler is being told the truth about a strike
or a removal; those are checked against redeemers built independently here
rather than reused from the term.
-}
module Testing.SchedulerValidator (tests) where

import Data.ByteString qualified as BS
import Data.Maybe (fromMaybe)
import PlutusCore.Data qualified as PD
import PlutusLedgerApi.V1.Address (scriptHashAddress)
import PlutusLedgerApi.V1.Interval (Extended (..), Interval (..), LowerBound (..), UpperBound (..))
import PlutusLedgerApi.V1.Value (CurrencySymbol (..), TokenName (..), Value, getValue, singleton)
import PlutusLedgerApi.V3 (
  Address,
  Datum (..),
  OutputDatum (..),
  POSIXTime (..),
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
  txInfoInputs,
  txInfoMint,
  txInfoOutputs,
  txInfoRedeemers,
  txInfoReferenceInputs,
  txInfoValidRange,
 )
import PlutusLedgerApi.V3.MintValue (MintValue (UnsafeMintValue))
import PlutusTx.AssocMap qualified as Map
import PlutusTx.Builtins (BuiltinData, dataToBuiltinData, fromBuiltin, toBuiltin)
import Test.Tasty
import Test.Tasty.HUnit

import Plutarch.Core.Utils (pand'List)
import Plutarch.LedgerApi.Utils (PMaybeData (..))
import Plutarch.LedgerApi.V3 (PPubKeyHash)
import Plutarch.Prelude
import Plutarch.Unsafe (punsafeCoerce)

import Midgard.Scheduler (
  PAdvancingApproach (..),
  PMintRedeemer (..),
  PNeglectedUserEvent (..),
  POperatorRemovalReason (..),
  PSchedDatum (..),
  PSpendRedeemer (..),
 )
import Midgard.Validators.Scheduler (schedulerMintValidator, schedulerSpendValidator)
import Testing.Eval (passertEvalNoTrace, pfails, psucceeds)
import Testing.ScriptContextBuilder (buildScriptContext, currencySymbolFromHex, mkAdaValue)

-- | Collects the tests defined in this module.
tests :: TestTree
tests =
  testGroup
    "Scheduler Validator Tests"
    [ testCase "canonical_scheduler_v1_typescript_abi_vector" $
        passertEvalNoTrace canonicalSchedulerV1TypescriptAbiVector
    , mintTests
    , endOfShiftTests
    , rewindEndOfShiftTests
    , skippedOperatorTests
    , rewindSkippedOperatorTests
    , removalTests
    , rewindRemovalTests
    , appointFirstTests
    ]

canonicalSchedulerV1TypescriptAbiVector :: forall s. Term s PBool
canonicalSchedulerV1TypescriptAbiVector =
  pand'List
    [ serialisesTo (pcon PNoActiveOperators) "d87980"
    , serialisesTo
        (pcon $ PActiveOperator operatorAa (pdata 1))
        "d87a9f41aa01ff"
    , serialisesTo (pcon PInit) "d87980"
    , serialisesTo (pcon PDeinit) "d87a80"
    , serialisesTo (pcon PNoNeglectedUserEvent) "d87980"
    , serialisesTo (pcon $ PNeglectedDeposit (pdata 1)) "d87a9f01ff"
    , serialisesTo (pcon $ PNeglectedWithdrawal (pdata 2)) "d87b9f02ff"
    , serialisesTo (pcon $ PNeglectedTxOrder (pdata 3)) "d87c9f03ff"
    , serialisesTo (pcon POperatorRetirement) "d87980"
    , serialisesTo (pcon POperatorSlashing) "d87a80"
    , serialisesTo
        (pcon $ PGoToNextDueToEndOfShift (pdata 1))
        "d8799f01ff"
    , serialisesTo
        (pcon $ PRewindDueToEndOfShift (pdata 1) (pdata 2) (pdata 3))
        "d87a9f010203ff"
    , serialisesTo
        ( pcon $
            PGoToNextDueToSkippedOperator
              (pdata 1)
              (pdata 2)
              (pdata 3)
              (pdata 4)
              (pdata 5)
              (pdata $ pcon $ PNeglectedDeposit (pdata 6))
        )
        "d87b9f0102030405d87a9f06ffff"
    , serialisesTo
        ( pcon $
            PRewindDueToSkippedOperator
              (pdata 1)
              (pdata 2)
              (pdata 3)
              (pdata 4)
              (pdata 5)
              (pcon $ PDJust $ pdata 6)
              (pdata 7)
              (pdata $ pcon $ PNeglectedWithdrawal (pdata 8))
        )
        "d87c9f0102030405d8799f06ff07d87b9f08ffff"
    , serialisesTo
        ( pcon $
            PGoToNextDueToOperatorRemoval
              (pdata 1)
              (pdata $ pcon POperatorRetirement)
        )
        "d87d9f01d87980ff"
    , serialisesTo
        ( pcon $
            PRewindDueToOperatorRemoval
              (pdata 1)
              (pcon PDNothing)
              (pdata $ pcon POperatorSlashing)
              (pdata 2)
        )
        "d87e9f01d87a80d87a8002ff"
    , serialisesTo
        (pcon $ PAppointFirstOperator (pdata 1) (pdata 2))
        "d87f9f0102ff"
    , serialisesTo
        ( pcon $
            PSpendRedeemer
              (pdata 1)
              (pdata 2)
              (pdata $ pcon $ PAppointFirstOperator (pdata 3) (pdata 4))
        )
        "d8799f0102d87f9f0304ffff"
    ]
  where
    operatorAa :: Term s (PAsData PPubKeyHash)
    operatorAa = pdata $ punsafeCoerce @PPubKeyHash $ phexByteStr "aa"

serialisesTo :: forall s a. PIsData a => Term s a -> String -> Term s PBool
serialisesTo value expected =
  (pserialiseData # pforgetData (pdata value)) #== phexByteStr expected

--------------------------------------------------------------------------------
-- Mint
--------------------------------------------------------------------------------

mintTests :: TestTree
mintTests =
  testGroup
    "mint"
    [ testCase "Init mints the scheduler NFT alongside the hub's" $
        psucceeds $ runMint initRedeemer $ toMint (hubMint 1 <> schedMint 1)
    , testCase "Deinit burns both" $
        psucceeds $ runMint deinitRedeemer $ toMint (hubMint (-1) <> schedMint (-1))
    , -- The pairing is the whole point: a scheduler cannot appear beside a hub
      -- that is not itself being created.
      testCase "Init rejects a scheduler NFT minted without the hub's" $
        pfails $ runMint initRedeemer (toMint (schedMint 1))
    , testCase "Init rejects a hub NFT minted without the scheduler's" $
        pfails $ runMint initRedeemer $ toMint (hubMint 1)
    , testCase "Init rejects a quantity other than one" $
        pfails $ runMint initRedeemer $ toMint (hubMint 1 <> schedMint 2)
    , testCase "Init rejects a burn" $
        pfails $ runMint initRedeemer $ toMint (hubMint (-1) <> schedMint (-1))
    , testCase "Deinit rejects a mint" $
        pfails $ runMint deinitRedeemer $ toMint (hubMint 1 <> schedMint 1)
    , -- A partial teardown would strand the scheduler UTxO forever.
      testCase "Deinit rejects burning the scheduler NFT but not the hub's" $
        pfails $ runMint deinitRedeemer (toMint (schedMint (-1)))
    , testCase "Init rejects a second token under the scheduler policy" $
        pfails $
          runMint initRedeemer $
            toMint (hubMint 1 <> schedMint 1 <> singleton schedulerPolicy (TokenName "X") 1)
    ]

initRedeemer :: BuiltinData
initRedeemer = dataToBuiltinData (PD.Constr 0 [])

deinitRedeemer :: BuiltinData
deinitRedeemer = dataToBuiltinData (PD.Constr 1 [])

hubMint :: Integer -> Value
hubMint n = singleton hubPolicy hubAssetName n

schedMint :: Integer -> Value
schedMint n = singleton schedulerPolicy schedulerAssetName n

runMint :: forall s. BuiltinData -> MintValue -> Term s PUnit
runMint redeemer mint =
  schedulerMintValidator
    # pdata (pconstant hubPolicy)
    # pconstant ctx
  where
    base = buildScriptContext mempty
    txInfo = (scriptContextTxInfo base) {txInfoMint = mint}
    ctx = ScriptContext txInfo (Redeemer redeemer) (MintingScript schedulerPolicy)

--------------------------------------------------------------------------------
-- GoToNextDueToEndOfShift
--------------------------------------------------------------------------------

{- | The ordinary case: the shift ended, and the turn steps to the node that
anchors the outgoing operator.
-}
endOfShiftTests :: TestTree
endOfShiftTests =
  testGroup
    "spend / GoToNextDueToEndOfShift"
    [ testCase "advances to the node anchoring the outgoing operator" $
        psucceeds $ runEndOfShift defaultEnd
    , -- The new shift starts at the *lower* bound, so a stale schedule catches
      -- up in one transaction instead of replaying every missed shift.
      testCase "starts the new shift at the validity range's lower bound" $
        psucceeds $ runEndOfShift defaultEnd {eosRange = closed 900 950, eosNewStart = 900}
    , testCase "rejects a new shift start other than the lower bound" $
        pfails $ runEndOfShift defaultEnd {eosNewStart = 901}
    , testCase "rejects a new shift start taken from the upper bound" $
        pfails $ runEndOfShift defaultEnd {eosRange = closed 900 950, eosNewStart = 950}
    , -- shift_duration is 30ms, so a shift starting at 100 ends at 130.
      testCase "accepts a range beginning exactly at the shift's end" $
        psucceeds $ runEndOfShift defaultEnd {eosRange = closed 130 150, eosNewStart = 130}
    , testCase "rejects a range beginning before the shift has ended" $
        pfails $ runEndOfShift defaultEnd {eosRange = closed 129 150, eosNewStart = 129}
    , -- The referenced node proves both halves of "is next" at once.
      testCase "rejects a node keyed by an operator other than the incoming one" $
        pfails $ runEndOfShift defaultEnd {eosNodeKey = operatorC}
    , testCase "rejects a node that does not link to the outgoing operator" $
        pfails $ runEndOfShift defaultEnd {eosNodeLink = linkTo operatorC}
    , testCase "rejects a terminal node" $
        pfails $ runEndOfShift defaultEnd {eosNodeLink = linkNone}
    , testCase "rejects the active set's root in place of a node" $
        pfails $ runEndOfShift defaultEnd {eosNodeIsRoot = True}
    , -- Both datums must name an operator on this path.
      testCase "rejects an idle input datum" $
        pfails $ runEndOfShift defaultEnd {eosInDatum = idleDatum}
    , testCase "rejects an idle output datum" $
        pfails $ runEndOfShift defaultEnd {eosOutDatum = idleDatum}
    , -- The scheduler NFT on both sides is what prevents double satisfaction.
      testCase "rejects an output without the scheduler NFT" $
        pfails $ runEndOfShift defaultEnd {eosOutputHasNft = False}
    , testCase "rejects an unbounded validity range" $
        pfails $ runEndOfShift defaultEnd {eosRange = fromNegInf 950}
    ]

data EndOfShift = EndOfShift
  { eosRange :: Interval POSIXTime
  , eosInDatum :: PD.Data
  , eosOutDatum :: PD.Data
  , eosNewStart :: Integer
  , eosNodeKey :: BS.ByteString
  , eosNodeLink :: PD.Data
  , eosNodeIsRoot :: Bool
  , eosOutputHasNft :: Bool
  }

defaultEnd :: EndOfShift
defaultEnd =
  EndOfShift
    { eosRange = closed 200 250
    , eosInDatum = appointed operatorA 100
    , eosOutDatum = appointed operatorB 200
    , eosNewStart = 200
    , eosNodeKey = operatorB
    , eosNodeLink = linkTo operatorA
    , eosNodeIsRoot = False
    , eosOutputHasNft = True
    }

runEndOfShift :: forall s. EndOfShift -> Term s PUnit
runEndOfShift e =
  spendCtx
    (eosRange e)
    (spendRedeemer (PD.Constr 0 [PD.I 1]))
    (eosInDatum e)
    ( [schedulerIn (eosInDatum e)]
    , [schedulerOut (outDatumOf e) (eosOutputHasNft e)]
    , [hubRefIn, activeElement (eosNodeIsRoot e) (eosNodeKey e) (eosNodeLink e)]
    , []
    )
  where
    outDatumOf x = case eosOutDatum x of
      PD.Constr 1 [op, _] -> PD.Constr 1 [op, PD.I (eosNewStart x)]
      other -> other

--------------------------------------------------------------------------------
-- RewindDueToEndOfShift
--------------------------------------------------------------------------------

{- | The wrap: the outgoing operator is the first node, so the turn returns to
the last one.
-}
rewindEndOfShiftTests :: TestTree
rewindEndOfShiftTests =
  testGroup
    "spend / RewindDueToEndOfShift"
    [ testCase "wraps the turn from the first node back to the last" $
        psucceeds $ runRewindEnd defaultRewindEnd
    , -- Rewinding is only legitimate from the front of the list.
      testCase "rejects a root that points at an operator other than the outgoing one" $
        pfails $ runRewindEnd defaultRewindEnd {reRootLink = linkTo operatorC}
    , testCase "rejects a node supplied where the root is required" $
        pfails $ runRewindEnd defaultRewindEnd {reRootIsNode = True}
    , -- The incoming operator must be at the end of the list.
      testCase "rejects a last node keyed by another operator" $
        pfails $ runRewindEnd defaultRewindEnd {reLastKey = operatorC}
    , testCase "rejects a last node that still links onward" $
        pfails $ runRewindEnd defaultRewindEnd {reLastLink = linkTo operatorC}
    , -- Fairness of ordering: a registered operator whose time has come is
      -- entitled to join before the rota starts over.
      testCase "rejects a wrap when a registered operator can already activate" $
        pfails $ runRewindEnd defaultRewindEnd {reRegisteredActivation = 100}
    , testCase "accepts a registered operator whose activation is still ahead" $
        psucceeds $ runRewindEnd defaultRewindEnd {reRegisteredActivation = 10_000}
    , -- Only the last registered element proves anything about the rest.
      testCase "rejects a registered element that is not the last" $
        pfails $ runRewindEnd defaultRewindEnd {reRegisteredLink = linkTo operatorC}
    , testCase "accepts the registered set's root, which means no registrations" $
        psucceeds $ runRewindEnd defaultRewindEnd {reRegisteredIsRoot = True}
    , testCase "rejects a range beginning before the shift has ended" $
        pfails $ runRewindEnd defaultRewindEnd {reRange = closed 129 150, reNewStart = 129}
    ]

data RewindEnd = RewindEnd
  { reRange :: Interval POSIXTime
  , reNewStart :: Integer
  , reRootLink :: PD.Data
  , reRootIsNode :: Bool
  , reLastKey :: BS.ByteString
  , reLastLink :: PD.Data
  , reRegisteredActivation :: Integer
  , reRegisteredLink :: PD.Data
  , reRegisteredIsRoot :: Bool
  }

defaultRewindEnd :: RewindEnd
defaultRewindEnd =
  RewindEnd
    { reRange = closed 200 250
    , reNewStart = 200
    , reRootLink = linkTo operatorA
    , reRootIsNode = False
    , reLastKey = operatorB
    , reLastLink = linkNone
    , reRegisteredActivation = 10_000
    , reRegisteredLink = linkNone
    , reRegisteredIsRoot = False
    }

runRewindEnd :: forall s. RewindEnd -> Term s PUnit
runRewindEnd r =
  spendCtx
    (reRange r)
    (spendRedeemer (PD.Constr 1 [PD.I 1, PD.I 2, PD.I 3]))
    (appointed operatorA 100)
    ( [schedulerIn (appointed operatorA 100)]
    , [schedulerOut (appointed operatorB (reNewStart r)) True]
    ,
      [ hubRefIn
      , activeElement (not (reRootIsNode r)) operatorC (reRootLink r)
      , activeElement False (reLastKey r) (reLastLink r)
      , registeredElement (reRegisteredIsRoot r) (reRegisteredActivation r) (reRegisteredLink r)
      ]
    , []
    )

--------------------------------------------------------------------------------
-- GoToNextDueToSkippedOperator
--------------------------------------------------------------------------------

{- | The inactivity path — the branch with the most to check, because it is the
one that costs an operator something.
-}
skippedOperatorTests :: TestTree
skippedOperatorTests =
  testGroup
    "spend / GoToNextDueToSkippedOperator"
    [ testCase "rejects a well-formed skip: unreachable under env/default.ak" $
        pfails $ runSkipped defaultSkipped
    ]

{- $unreachable

Both skipped-operator branches are dead code in the default environment, and no
transaction can reach them, so there is only one honest assertion to make about
each.

@validate_operator_inactivity_and_get_its_link@ requires

@
inactivity_threshold < shift_end_time(inactive_operators_shift_start_time)
@

where @inactivity_threshold@ is a @max@ whose first argument is
@shift_start + new_shift_inactivity_grace_period@ and @shift_end_time(t)@ is
@t + shift_duration@. With @env/default.ak@'s values that is

@
start + 300_000 < start + 30
@

which is false for every @start@. The grace period is five minutes and a shift
lasts thirty milliseconds, so the threshold always lands past the end of the
shift it is meant to fall inside.

This is a consequence of the environment, not a defect in the branch.
@env/default.ak@ and @env/testnet.ak@ differ in this one constant and nothing
else: 30 against @60 * 60 * 1000@. An hour clears the five-minute grace period,
so a testnet build reaches both branches normally.

Everything else in the branch was verified by temporarily replacing that one
conjunct with @True@ and running the full fixture set: eighteen cases covering
the strike-redeemer agreement, the state-queue tip, the confirmed-state and
block-header paths, the grace period, all three neglected-event types and the
negligence timeout all passed. Those tests are not kept, because with the
conjunct restored every one of them would assert rejection and so discriminate
nothing — a suite that stays green whatever the branch does. The gap is real:
this branch has no regression coverage while the environment stays as it is.
-}

-- | Which user event, if any, the skip accuses the operator of neglecting.
data Neglect = NeglectNone | NeglectDeposit | NeglectWithdrawal | NeglectTxOrder

data Skipped = Skipped
  { skRange :: Interval POSIXTime
  , skShiftStart :: Integer
  , skNewStart :: Integer
  , skStrikeOperator :: BS.ByteString
  , skStrikeSchedulerIndex :: Integer
  , skStrikeIsListTransition :: Bool
  , skQueueIsRoot :: Bool
  , skQueueEndTime :: Integer
  , skQueueLink :: PD.Data
  , skNodeLink :: PD.Data
  , skNeglected :: Neglect
  , skEventInclusionTime :: Integer
  }

{- | The no-specific-event case: the operator committed no block at all, so the
threshold is the queue tip plus @max_inactivity_between_block_commitments@.
-}
defaultSkipped :: Skipped
defaultSkipped =
  Skipped
    { skRange = closed 300_101 300_150
    , skShiftStart = 100
    , skNewStart = 300_150
    , skStrikeOperator = operatorA
    , skStrikeSchedulerIndex = 0
    , skStrikeIsListTransition = False
    , skQueueIsRoot = False
    , skQueueEndTime = 100
    , skQueueLink = linkNone
    , skNodeLink = linkTo operatorA
    , skNeglected = NeglectNone
    , skEventInclusionTime = 0
    }

runSkipped :: forall s. Skipped -> Term s PUnit
runSkipped k =
  spendCtx
    (skRange k)
    ( spendRedeemer
        ( PD.Constr
            2
            [ PD.I 2 -- new shift's operator node ref input index
            , PD.I 1 -- skipped operator node input index
            , PD.I 0 -- active operators spend redeemer index
            , PD.I 1 -- state queue ref input index
            , PD.I 0 -- hub oracle ref input index
            , neglectedData k
            ]
        )
    )
    (appointed operatorA (skShiftStart k))
    ( [schedulerIn (appointed operatorA (skShiftStart k)), activeNodeIn]
    , [schedulerOut (appointed operatorB (skNewStart k)) True]
    ,
      [ hubRefIn
      , queueElement (skQueueIsRoot k) (skQueueEndTime k) (skQueueLink k)
      , activeElement False operatorB (skNodeLink k)
      , eventRefIn k
      ]
    , [(Spending (outRefN 1), Redeemer (strikeRedeemer k))]
    )

neglectedData :: Skipped -> PD.Data
neglectedData k = case skNeglected k of
  NeglectNone -> PD.Constr 0 []
  NeglectDeposit -> PD.Constr 1 [PD.I 3]
  NeglectWithdrawal -> PD.Constr 2 [PD.I 3]
  NeglectTxOrder -> PD.Constr 3 [PD.I 3]

-- | The event reference input, under whichever policy the branch will look for.
eventRefIn :: Skipped -> TxInInfo
eventRefIn k = eventElement policy (skEventInclusionTime k)
  where
    policy = case skNeglected k of
      NeglectNone -> depositPolicy
      NeglectDeposit -> depositPolicy
      NeglectWithdrawal -> withdrawalPolicy
      NeglectTxOrder -> txOrderPolicy

{- | @active_operators.StrikeForInactivity@ — constructor 3, seven fields.

Built here rather than taken from the port, so that a change to either side's
field order shows up as a failure instead of two copies agreeing.
-}
strikeRedeemer :: Skipped -> BuiltinData
strikeRedeemer k
  | skStrikeIsListTransition k = dataToBuiltinData (PD.Constr 0 [])
  | otherwise =
      dataToBuiltinData $
        PD.Constr
          3
          [ PD.I 1 -- active_node_input_index
          , PD.I 0 -- active_node_output_index
          , PD.B (skStrikeOperator k)
          , linkNone -- active_node_link
          , PD.I (skStrikeSchedulerIndex k)
          , PD.I 0 -- scheduler_redeemer_index
          , PD.I 0 -- hub_oracle_ref_input_index
          ]

--------------------------------------------------------------------------------
-- RewindDueToSkippedOperator
--------------------------------------------------------------------------------

rewindSkippedOperatorTests :: TestTree
rewindSkippedOperatorTests =
  testGroup
    "spend / RewindDueToSkippedOperator"
    [ -- Same gate, same reason; see the note above 'skippedOperatorTests'.
      testCase "rejects a well-formed wrap: unreachable under env/default.ak" $
        pfails $ runRewindSkipped defaultRewindSkipped
    ]

data RewindSkipped = RewindSkipped
  { rsRootLink :: PD.Data
  , rsLastNodeIndex :: Maybe Integer
  , rsNewOperator :: BS.ByteString
  , rsStrikeLink :: PD.Data
  , rsRegisteredActivation :: Integer
  }

defaultRewindSkipped :: RewindSkipped
defaultRewindSkipped =
  RewindSkipped
    { rsRootLink = linkTo operatorA
    , rsLastNodeIndex = Just 4
    , rsNewOperator = operatorB
    , rsStrikeLink = linkTo operatorB
    , rsRegisteredActivation = 400_000
    }

runRewindSkipped :: forall s. RewindSkipped -> Term s PUnit
runRewindSkipped r =
  spendCtx
    (closed 300_101 300_150)
    ( spendRedeemer
        ( PD.Constr
            3
            [ PD.I 2 -- active operators root ref input index
            , PD.I 1 -- skipped operator node input index
            , PD.I 0 -- active operators spend redeemer index
            , PD.I 1 -- state queue ref input index
            , PD.I 0 -- hub oracle ref input index
            , maybe (PD.Constr 1 []) (\i -> PD.Constr 0 [PD.I i]) (rsLastNodeIndex r)
            , PD.I 3 -- registered element ref input index
            , PD.Constr 0 [] -- NoNeglectedUserEvent
            ]
        )
    )
    (appointed operatorA 100)
    ( [schedulerIn (appointed operatorA 100), activeNodeIn]
    , [schedulerOut (appointed (rsNewOperator r) 300_150) True]
    ,
      [ hubRefIn
      , queueElement False 100 linkNone
      , activeElement True operatorC (rsRootLink r)
      , registeredElement False (rsRegisteredActivation r) linkNone
      , activeElement False (rsNewOperator r) linkNone
      ]
    , [(Spending (outRefN 1), Redeemer (rewindStrikeRedeemer r))]
    )

rewindStrikeRedeemer :: RewindSkipped -> BuiltinData
rewindStrikeRedeemer r =
  dataToBuiltinData $
    PD.Constr
      3
      [ PD.I 1
      , PD.I 0
      , PD.B operatorA
      , rsStrikeLink r
      , PD.I 0
      , PD.I 0
      , PD.I 0
      ]

--------------------------------------------------------------------------------
-- GoToNextDueToOperatorRemoval
--------------------------------------------------------------------------------

removalTests :: TestTree
removalTests =
  testGroup
    "spend / GoToNextDueToOperatorRemoval"
    [ testCase "advances past a retiring operator" $
        psucceeds $ runRemoval defaultRemoval
    , testCase "advances past a slashed operator" $
        psucceeds $ runRemoval defaultRemoval {rmReason = Slashing}
    , -- The reason names where in the redeemer the operator sits, so a
      -- mismatched pair must not be readable.
      testCase "rejects a retirement reason against a slashing redeemer" $
        pfails $ runRemoval defaultRemoval {rmRedeemerShape = Just Slashing}
    , testCase "rejects a slashing reason against a retirement redeemer" $
        pfails $ runRemoval defaultRemoval {rmReason = Slashing, rmRedeemerShape = Just Retirement}
    , -- Both scripts must be talking about the same operator, and the same
      -- scheduler UTxO.
      testCase "rejects a removal naming a different operator" $
        pfails $ runRemoval defaultRemoval {rmRemovedOperator = operatorC}
    , testCase "rejects a removal pointing at another scheduler input" $
        pfails $ runRemoval defaultRemoval {rmSchedulerIndex = 7}
    , -- Stepping past a removal lands on the node that anchored it.
      testCase "rejects an anchor key other than the incoming operator" $
        pfails $ runRemoval defaultRemoval {rmAnchorKey = Just operatorC}
    , testCase "rejects a root anchor, which would be a rewind" $
        pfails $ runRemoval defaultRemoval {rmAnchorKey = Nothing}
    , -- ShowOperatorIsInactive is the other sync variant and must not be
      -- accepted where advancement is being proved.
      testCase "rejects the inactive-operator sync variant" $
        pfails $ runRemoval defaultRemoval {rmSyncIsInactive = True}
    , testCase "rejects a new shift start other than the upper bound" $
        pfails $ runRemoval defaultRemoval {rmNewStart = 249}
    ]

data RemovalReason = Retirement | Slashing

data Removal = Removal
  { rmReason :: RemovalReason
  , -- Which shape the active set's redeemer actually takes. 'Nothing' means it
    -- agrees with 'rmReason'; setting it exhibits a mismatched pair.
    rmRedeemerShape :: Maybe RemovalReason
  , rmRemovedOperator :: BS.ByteString
  , rmSchedulerIndex :: Integer
  , rmAnchorKey :: Maybe BS.ByteString
  , rmSyncIsInactive :: Bool
  , rmNewStart :: Integer
  }

defaultRemoval :: Removal
defaultRemoval =
  Removal
    { rmReason = Retirement
    , rmRedeemerShape = Nothing
    , rmRemovedOperator = operatorA
    , rmSchedulerIndex = 0
    , rmAnchorKey = Just operatorB
    , rmSyncIsInactive = False
    , rmNewStart = 250
    }

runRemoval :: forall s. Removal -> Term s PUnit
runRemoval r =
  spendCtx
    (closed 200 250)
    (spendRedeemer (PD.Constr 4 [PD.I 0, reasonData (rmReason r)]))
    (appointed operatorA 100)
    ( [schedulerIn (appointed operatorA 100)]
    , [schedulerOut (appointed operatorB (rmNewStart r)) True]
    , [hubRefIn]
    , [(Minting activePolicy, Redeemer (removalRedeemer r False))]
    )

reasonData :: RemovalReason -> PD.Data
reasonData Retirement = PD.Constr 0 []
reasonData Slashing = PD.Constr 1 []

{- | @active_operators.MintRedeemer@ — @RetireOperator@ is 3, @SlashOperator@ 4.

@ShowSchedulerIsAdvancing@ is constructor 1 of @OperatorRemovalSchedulerSync@,
with four fields.
-}
removalRedeemer :: Removal -> Bool -> BuiltinData
removalRedeemer r lastMember
  | slash =
      dataToBuiltinData $
        PD.Constr
          4
          [ PD.Constr
              0
              [ PD.B (rmRemovedOperator r)
              , PD.I 0
              , outRefData
              , PD.I 0
              , PD.Constr 0 [] -- slashing reason
              ]
          , sync
          ]
  | otherwise =
      dataToBuiltinData $
        PD.Constr
          3
          [ PD.B (rmRemovedOperator r)
          , PD.I 0
          , outRefData
          , PD.I 0
          , PD.I 0
          , PD.Constr 1 [] -- penalize_for_inactivity: False
          , sync
          ]
  where
    slash = case fromMaybe (rmReason r) (rmRedeemerShape r) of
      Slashing -> True
      Retirement -> False
    sync
      | rmSyncIsInactive r = PD.Constr 0 [PD.I 0]
      | otherwise =
          PD.Constr
            1
            [ PD.I (rmSchedulerIndex r)
            , PD.I 0
            , maybe (PD.Constr 1 []) (\k -> PD.Constr 0 [PD.B k]) (rmAnchorKey r)
            , if lastMember then PD.Constr 1 [] else PD.Constr 0 []
            ]

outRefData :: PD.Data
outRefData = PD.Constr 0 [PD.Constr 0 [PD.B (BS.replicate 32 0x01)], PD.I 0]

--------------------------------------------------------------------------------
-- RewindDueToOperatorRemoval
--------------------------------------------------------------------------------

rewindRemovalTests :: TestTree
rewindRemovalTests =
  testGroup
    "spend / RewindDueToOperatorRemoval"
    [ -- Removing the only member leaves nothing to schedule.
      testCase "empties the schedule when the last member is removed" $
        psucceeds $ runRewindRemoval defaultRewindRemoval
    , testCase "rejects an emptied set whose output datum still names an operator" $
        pfails $ runRewindRemoval defaultRewindRemoval {rrOutIsIdle = False}
    , testCase "rejects an empty claim when the removed operator was not the last" $
        pfails $ runRewindRemoval defaultRewindRemoval {rrLastMember = False}
    , testCase "wraps to the last node when members remain" $
        psucceeds $ runRewindRemoval remainingMembers
    , testCase "rejects a wrap claiming the removed operator was the last member" $
        pfails $ runRewindRemoval remainingMembers {rrLastMember = True}
    , testCase "rejects a wrap whose output datum is idle" $
        pfails $ runRewindRemoval remainingMembers {rrOutIsIdle = True}
    , -- Rewinding requires the removal to have been anchored at the root.
      testCase "rejects a node anchor where the root is required" $
        pfails $ runRewindRemoval defaultRewindRemoval {rrAnchorKey = Just operatorB}
    , testCase "rejects a wrap when a registered operator can already activate" $
        pfails $ runRewindRemoval remainingMembers {rrRegisteredActivation = 100}
    ]

data RewindRemoval = RewindRemoval
  { rrLastNodeIndex :: Maybe Integer
  , rrLastMember :: Bool
  , rrOutIsIdle :: Bool
  , rrAnchorKey :: Maybe BS.ByteString
  , rrRegisteredActivation :: Integer
  }

defaultRewindRemoval :: RewindRemoval
defaultRewindRemoval =
  RewindRemoval
    { rrLastNodeIndex = Nothing
    , rrLastMember = True
    , rrOutIsIdle = True
    , rrAnchorKey = Nothing
    , rrRegisteredActivation = 10_000
    }

remainingMembers :: RewindRemoval
remainingMembers =
  defaultRewindRemoval
    { rrLastNodeIndex = Just 2
    , rrLastMember = False
    , rrOutIsIdle = False
    }

runRewindRemoval :: forall s. RewindRemoval -> Term s PUnit
runRewindRemoval r =
  spendCtx
    (closed 200 250)
    ( spendRedeemer
        ( PD.Constr
            5
            [ PD.I 0
            , maybe (PD.Constr 1 []) (\i -> PD.Constr 0 [PD.I i]) (rrLastNodeIndex r)
            , PD.Constr 0 [] -- OperatorRetirement
            , PD.I 1 -- registered element ref input index
            ]
        )
    )
    (appointed operatorA 100)
    ( [schedulerIn (appointed operatorA 100)]
    , [schedulerOut (if rrOutIsIdle r then idleDatum else appointed operatorB 250) True]
    ,
      [ hubRefIn
      , registeredElement False (rrRegisteredActivation r) linkNone
      , activeElement False operatorB linkNone
      ]
    , [(Minting activePolicy, Redeemer (removalRedeemer removalSpec (rrLastMember r)))]
    )
  where
    removalSpec = defaultRemoval {rmAnchorKey = rrAnchorKey r}

--------------------------------------------------------------------------------
-- AppointFirstOperator
--------------------------------------------------------------------------------

appointFirstTests :: TestTree
appointFirstTests =
  testGroup
    "spend / AppointFirstOperator"
    [ testCase "appoints an operator to an empty schedule" $
        psucceeds $ runAppoint defaultAppoint
    , -- Only reachable from an empty schedule; otherwise it would be a way to
      -- replace a sitting operator without any of the other branches' proofs.
      testCase "rejects an input datum that already names an operator" $
        pfails $ runAppoint defaultAppoint {apInDatum = appointed operatorA 100}
    , testCase "rejects an output datum that names no operator" $
        pfails $ runAppoint defaultAppoint {apOutIsIdle = True}
    , testCase "rejects an appointee that is not the last active node" $
        pfails $ runAppoint defaultAppoint {apNodeLink = linkTo operatorC}
    , testCase "rejects a node keyed by another operator" $
        pfails $ runAppoint defaultAppoint {apNodeKey = operatorC}
    , testCase "rejects an appointment when a registered operator can activate" $
        pfails $ runAppoint defaultAppoint {apRegisteredActivation = 100}
    , testCase "rejects a new shift start other than the upper bound" $
        pfails $ runAppoint defaultAppoint {apNewStart = 200}
    ]

data Appoint = Appoint
  { apInDatum :: PD.Data
  , apOutIsIdle :: Bool
  , apNewStart :: Integer
  , apNodeKey :: BS.ByteString
  , apNodeLink :: PD.Data
  , apRegisteredActivation :: Integer
  }

defaultAppoint :: Appoint
defaultAppoint =
  Appoint
    { apInDatum = idleDatum
    , apOutIsIdle = False
    , apNewStart = 250
    , apNodeKey = operatorB
    , apNodeLink = linkNone
    , apRegisteredActivation = 10_000
    }

runAppoint :: forall s. Appoint -> Term s PUnit
runAppoint a =
  spendCtx
    (closed 200 250)
    (spendRedeemer (PD.Constr 6 [PD.I 1, PD.I 2]))
    (apInDatum a)
    ( [schedulerIn (apInDatum a)]
    , [schedulerOut (if apOutIsIdle a then idleDatum else appointed operatorB (apNewStart a)) True]
    ,
      [ hubRefIn
      , activeElement False (apNodeKey a) (apNodeLink a)
      , registeredElement False (apRegisteredActivation a) linkNone
      ]
    , []
    )

--------------------------------------------------------------------------------
-- Applying the term
--------------------------------------------------------------------------------

{- | Builds a spending 'ScriptContext' around the fields these tests vary.

The scheduler's own input is always index 0 and its output index 0, matching
'spendRedeemer'; every other index in a branch redeemer is a position in this
list of reference inputs.
-}
spendCtx ::
  forall s.
  Interval POSIXTime ->
  BuiltinData ->
  PD.Data ->
  ([TxInInfo], [TxOut], [TxInInfo], [(ScriptPurpose, Redeemer)]) ->
  Term s PUnit
spendCtx validRange redeemer ownDatum (ins, outs, refs, redeemers) =
  schedulerSpendValidator
    # pdata (pconstant registeredPolicy)
    # pdata (pconstant activeAddress)
    # pdata (pconstant activePolicy)
    # pdata (pconstant schedulerPolicy)
    # pdata (pconstant hubPolicy)
    # pconstant ctx
  where
    base = buildScriptContext mempty
    txInfo =
      (scriptContextTxInfo base)
        { txInfoInputs = ins
        , txInfoOutputs = outs
        , txInfoReferenceInputs = refs
        , txInfoValidRange = validRange
        , txInfoRedeemers = Map.unsafeFromList redeemers
        }
    -- The scheduler's datum reaches the validator through `ScriptInfo`, not
    -- through the resolved input; `expect Some(datum)` reads it from here.
    ctx =
      ScriptContext
        txInfo
        (Redeemer redeemer)
        (SpendingScript (outRefN 0) (Just (Datum (dataToBuiltinData ownDatum))))

-- | @SpendRedeemer { scheduler_input_index: 0, scheduler_output_index: 0, .. }@.
spendRedeemer :: PD.Data -> BuiltinData
spendRedeemer approach = dataToBuiltinData (PD.Constr 0 [PD.I 0, PD.I 0, approach])

--------------------------------------------------------------------------------
-- Identities
--------------------------------------------------------------------------------

policyFor :: Int -> CurrencySymbol
policyFor n = currencySymbolFromHex (concat (replicate 28 (pad n)))
  where
    pad x = let h = showHexByte x in h

showHexByte :: Int -> String
showHexByte x = [hexDigit (x `div` 16), hexDigit (x `mod` 16)]
  where
    hexDigit d = "0123456789abcdef" !! d

schedulerPolicy, hubPolicy, activePolicy, registeredPolicy :: CurrencySymbol
schedulerPolicy = policyFor 0x11
hubPolicy = policyFor 0x12
activePolicy = policyFor 0x13
registeredPolicy = policyFor 0x14

stateQueuePolicy, depositPolicy, withdrawalPolicy, txOrderPolicy :: CurrencySymbol
stateQueuePolicy = policyFor 0x15
depositPolicy = policyFor 0x16
withdrawalPolicy = policyFor 0x17
txOrderPolicy = policyFor 0x18

otherPolicy :: CurrencySymbol
otherPolicy = policyFor 0x19

schedulerAssetName :: TokenName
schedulerAssetName = TokenName (toBuiltin ("MIDGARD_SCHEDULER" :: BS.ByteString))

hubAssetName :: TokenName
hubAssetName = TokenName (toBuiltin ("MIDGARD_HUB_ORACLE" :: BS.ByteString))

operatorA, operatorB, operatorC :: BS.ByteString
operatorA = BS.replicate 28 0xaa
operatorB = BS.replicate 28 0xbb
operatorC = BS.replicate 28 0xcc

addressOf :: CurrencySymbol -> Address
addressOf cs = scriptHashAddress (ScriptHash (unCurrencySymbol cs))

activeAddress :: Address
activeAddress = addressOf activePolicy

outRefN :: Integer -> TxOutRef
outRefN = TxOutRef (TxId "0101010101010101010101010101010101010101010101010101010101010101")

toMint :: Value -> MintValue
toMint = UnsafeMintValue . getValue

--------------------------------------------------------------------------------
-- Datums and elements
--------------------------------------------------------------------------------

idleDatum :: PD.Data
idleDatum = PD.Constr 0 []

appointed :: BS.ByteString -> Integer -> PD.Data
appointed op startTime = PD.Constr 1 [PD.B op, PD.I startTime]

linkNone :: PD.Data
linkNone = PD.Constr 1 []

linkTo :: BS.ByteString -> PD.Data
linkTo key = PD.Constr 0 [PD.B key]

closed :: Integer -> Integer -> Interval POSIXTime
closed lo hi =
  Interval
    (LowerBound (Finite (POSIXTime lo)) True)
    (UpperBound (Finite (POSIXTime hi)) True)

fromNegInf :: Integer -> Interval POSIXTime
fromNegInf hi = Interval (LowerBound NegInf True) (UpperBound (Finite (POSIXTime hi)) True)

schedulerIn :: PD.Data -> TxInInfo
schedulerIn datum =
  TxInInfo
    (outRefN 0)
    ( TxOut
        (addressOf schedulerPolicy)
        (mkAdaValue 2_000_000 <> singleton schedulerPolicy schedulerAssetName 1)
        (OutputDatum (Datum (dataToBuiltinData datum)))
        Nothing
    )

schedulerOut :: PD.Data -> Bool -> TxOut
schedulerOut datum hasNft =
  TxOut
    (addressOf schedulerPolicy)
    ( mkAdaValue 2_000_000
        <> if hasNft
          then singleton schedulerPolicy schedulerAssetName 1
          else singleton otherPolicy (TokenName "Z") 1
    )
    (OutputDatum (Datum (dataToBuiltinData datum)))
    Nothing

-- | The active-operators node being struck; its own validator is not run here.
activeNodeIn :: TxInInfo
activeNodeIn =
  TxInInfo
    (outRefN 1)
    ( TxOut
        activeAddress
        (mkAdaValue 2_000_000 <> singleton activePolicy (TokenName (toBuiltin ("MACT" <> operatorA))) 1)
        (OutputDatum (Datum (dataToBuiltinData idleDatum)))
        Nothing
    )

{- | A linked-list element under a chosen policy.

@isRoot@ selects between the root datum (constructor 0, named by @rootName@) and
a node datum (constructor 1, named @prefix ++ key@) — the distinction the
linked-list reader authenticates.
-}
listElement ::
  CurrencySymbol ->
  BS.ByteString ->
  BS.ByteString ->
  Bool ->
  BS.ByteString ->
  PD.Data ->
  PD.Data ->
  TxInInfo
listElement policy rootName prefix isRoot key payload link =
  TxInInfo
    (outRefN 5)
    ( TxOut
        (addressOf policy)
        (mkAdaValue 2_000_000 <> singleton policy name 1)
        (OutputDatum (Datum (dataToBuiltinData element)))
        Nothing
    )
  where
    name
      | isRoot = TokenName (toBuiltin rootName)
      | otherwise = TokenName (toBuiltin (prefix <> key))
    element
      | isRoot = PD.Constr 0 [PD.Constr 0 [payload], link]
      | otherwise = PD.Constr 0 [PD.Constr 1 [payload], link]

activeElement :: Bool -> BS.ByteString -> PD.Data -> TxInInfo
activeElement isRoot key link =
  listElement activePolicy "MIDGARD_ACTIVE_OPERATORS" "MACT" isRoot key (PD.Constr 0 []) link

{- | A registered-operators element.

The set is keyed by activation time — the node key is the big-endian encoding of
that time — so the key is derived here rather than supplied, mirroring
@activation_time_to_node_key@.
-}
registeredElement :: Bool -> Integer -> PD.Data -> TxInInfo
registeredElement isRoot activationTime link =
  listElement
    registeredPolicy
    "MIDGARD_REGISTERED_OPERATORS"
    "MREG"
    isRoot
    (integerToBytes activationTime)
    (PD.Constr 0 [PD.B operatorC])
    link

-- | Aiken @registered_operators.activation_time_to_node_key@, recomputed here.
integerToBytes :: Integer -> BS.ByteString
integerToBytes 0 = BS.pack [0]
integerToBytes n = BS.pack (go n [])
  where
    go 0 acc = acc
    go x acc = go (x `div` 256) (fromIntegral (x `mod` 256) : acc)

{- | A state-queue element: the root carries a @ConfirmedState@, a node a
@StateQueueNode@ wrapping a @HeaderV1@. Both expose an end time, which is what
the inactivity threshold reads.
-}
queueElement :: Bool -> Integer -> PD.Data -> TxInInfo
queueElement isRoot endTime link =
  listElement
    stateQueuePolicy
    "MIDGARD_CONFIRMED_STATE"
    "MBLC"
    isRoot
    operatorA
    payload
    link
  where
    payload
      | isRoot =
          PD.Constr
            0
            [ PD.B (BS.replicate 28 0x10)
            , PD.B (BS.replicate 28 0x11)
            , PD.B (BS.replicate 32 0x12)
            , PD.I 0
            , PD.I endTime
            , PD.I 1
            ]
      | otherwise = PD.Constr 0 [headerData endTime, PD.B ""]

-- | A @HeaderV1@ whose event interval ends at @endTime@; field 17 of 25.
headerData :: Integer -> PD.Data
headerData endTime =
  PD.Constr
    0
    ( replicate 9 (PD.B (BS.replicate 32 0x01))
        <> replicate 7 (PD.I 0)
        <> [PD.I 0, PD.I endTime, PD.I 0, PD.I 0, PD.I 0, PD.I 0]
        <> [PD.B (BS.replicate 28 0x02)]
        <> [PD.B operatorA]
        <> [PD.I 1]
    )

{- | A user-event UTxO carrying an inclusion time.

Only the first three fields matter to the scheduler, and only the second of
those; the event payload is left as an opaque placeholder.
-}
eventElement :: CurrencySymbol -> Integer -> TxInInfo
eventElement policy inclusionTime =
  TxInInfo
    (outRefN 6)
    ( TxOut
        (addressOf policy)
        (mkAdaValue 2_000_000 <> singleton policy (TokenName "E") 1)
        ( OutputDatum
            ( Datum
                ( dataToBuiltinData
                    ( PD.Constr
                        0
                        [ PD.Constr 0 []
                        , PD.I inclusionTime
                        , PD.B (BS.replicate 28 0x20)
                        , PD.Constr 0 []
                        , PD.Constr 0 []
                        ]
                    )
                )
            )
        )
        Nothing
    )

--------------------------------------------------------------------------------
-- Hub oracle
--------------------------------------------------------------------------------

hubRefIn :: TxInInfo
hubRefIn =
  TxInInfo
    (outRefN 9)
    ( TxOut
        (addressOf hubPolicy)
        (mkAdaValue 2_000_000 <> singleton hubPolicy hubAssetName 1)
        (OutputDatum (Datum hubDatum))
        Nothing
    )

hubDatum :: BuiltinData
hubDatum =
  dataToBuiltinData $
    PD.Constr
      0
      ( [ PD.B (csBytes registeredPolicy) -- 0: registered_operators
        , PD.B (csBytes activePolicy) -- 1: active_operators
        , PD.B (csBytes (policyFor 0x21)) -- 2: retired_operators
        , PD.B (csBytes schedulerPolicy) -- 3: scheduler
        , PD.B (csBytes stateQueuePolicy) -- 4: state_queue
        , PD.B (csBytes (policyFor 0x22)) -- 5: fraud_proof_catalogue
        , PD.B (csBytes (policyFor 0x23)) -- 6: fraud_proof
        , PD.B (csBytes depositPolicy) -- 7: deposit
        , PD.B (csBytes withdrawalPolicy) -- 8: withdrawal
        , PD.B (csBytes txOrderPolicy) -- 9: tx_order
        , PD.B (csBytes (policyFor 0x24)) -- 10: settlement
        , PD.B (csBytes (policyFor 0x25)) -- 11: payout
        ]
          <> replicate 13 addressData
          <> [PD.B (csBytes (policyFor 0x26))]
      )
  where
    csBytes = fromBuiltin . unCurrencySymbol
    addressData =
      PD.Constr
        0
        [ PD.Constr 1 [PD.B (csBytes activePolicy)]
        , PD.Constr 1 []
        ]
