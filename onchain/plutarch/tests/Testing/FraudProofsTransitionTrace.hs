{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Testing.FraudProofsTransitionTrace
Description : Behavioural tests for the Plutarch port of
              @validators/fraud-proofs/transition-trace/@.

One router and eight final validators. The rules they invoke are tested
exhaustively in "Testing.TransitionTraceProof"; what is left for this module is
the dispatch itself, which is where a ten-armed fault type meets nine scripts and
has three separate chances to send a proof somewhere it does not belong.

=== The convictions are not built here

Each final validator convicts only if its entry point answers, so a proof
assembled in this module would either be an accident or a second copy of the
fixtures that already exist. The eight cases come from
'Testing.TransitionTraceProof' instead, each already asserted there to hold — so
a failure below is a dispatch failure and never a fault-rule one.

The routing table is the exception and is deliberately rewritten from
@route-v1.ak@ below, because that is the thing under test.

=== The routing table is the hazard's worst shape

@route_index@ is a ten-arm @when@ in which four arms answer @0@, two answer @2@,
two answer @4@ and two answer @6@ — five pairs of identical bodies, the exact
shape that makes a Plutarch @pmatch@ take its wildcard instead. Every one of the
fourteen distinct shapes is therefore pinned individually below, in both halves
of the read: the fault's own constructor and the nested one-step witness's.
-}
module Testing.FraudProofsTransitionTrace (tests) where

import Data.ByteString qualified as BS
import PlutusCore.Data qualified as PD
import PlutusLedgerApi.V1.Address (scriptHashAddress)
import PlutusLedgerApi.V1.Value (TokenName (..), singleton)
import PlutusLedgerApi.V3 (
  Address,
  Datum (..),
  OutputDatum (..),
  Redeemer,
  ScriptContext,
  ScriptHash (..),
  ScriptPurpose,
  TxInInfo (..),
  TxOut (..),
 )
import PlutusTx.Builtins (dataToBuiltinData, toBuiltin)
import PlutusTx.IsData qualified as PlutusTx
import Test.Tasty
import Test.Tasty.HUnit

import Plutarch.LedgerApi.V3 (PAddress, PCurrencySymbol, PScriptContext, PScriptHash)
import Plutarch.Prelude
import Plutarch.Unsafe (punsafeCoerce)

import Midgard.FraudProofs.TransitionTrace.Proof (PTransitionFault)
import Midgard.Validators.FraudProofs.TransitionTrace (
  ptransitionFaultRouteIndex,
  transitionTraceAcceptedTransactionV1Validator,
  transitionTraceControlV1Validator,
  transitionTraceDepositV1Validator,
  transitionTraceDuplicateV1Validator,
  transitionTraceForcedV1Validator,
  transitionTraceL1EventV1Validator,
  transitionTraceRouteV1Validator,
  transitionTraceSourceV1Validator,
  transitionTraceWithdrawalV1Validator,
 )
import Testing.Eval (passertEval, pfails, psucceeds)
import Testing.FraudProofsFixture
import Testing.TransitionTraceProof (
  ConvictingProof (..),
  acceptedTransactionConviction,
  controlConviction,
  depositConviction,
  depositHubOracleDatum,
  depositReferenceOutput,
  duplicateConviction,
  forcedConviction,
  l1EventConviction,
  sourceConviction,
  withdrawalConviction,
 )
import Testing.ValidationClaim (acceptedMismatchProofFixture)

--------------------------------------------------------------------------------
-- The suite
--------------------------------------------------------------------------------

tests :: TestTree
tests =
  testGroup
    "Transition Trace Dispatch Tests"
    [ testGroup "the routing table" routingTableTests
    , testGroup "route-v1" routeTests
    , testGroup "the eight final validators" finalTests
    ]

--------------------------------------------------------------------------------
-- The routing table, rewritten from route-v1.ak
--------------------------------------------------------------------------------

{- | @route_index@'s outer half, by fault constructor.

Written out as a table rather than derived, because the four arms that share an
answer are the point: a port that collapsed them would agree with a derivation
and disagree with Aiken.
-}
routeIndexFor :: Integer -> Integer
routeIndexFor tag = case tag of
  0 -> 0 -- TraceBoundaryFault
  1 -> 0 -- TraceLinkFault
  2 -> 0 -- EventToStepMismatch
  8 -> 0 -- CountFault
  3 -> 1 -- SourceMembershipMismatch
  5 -> 6 -- OmittedDueL1Event
  7 -> 6 -- OutOfWindowSourceEvent
  6 -> 7 -- DuplicateTraceEvent
  9 -> 4 -- AcceptedTransactionTransitionMismatch
  _ -> error "not a leaf fault"

-- | @route_index@'s inner half, by one-step witness constructor.
oneStepRouteIndexFor :: Integer -> Integer
oneStepRouteIndexFor tag = case tag of
  0 -> 2 -- ValidWithdrawalTransition
  1 -> 2 -- InvalidWithdrawalNoOpTransition
  2 -> 3 -- InvalidForcedTransactionNoOpTransition
  3 -> 5 -- ValidDepositTransition
  4 -> 4 -- L2TransactionTransition
  _ -> error "not a one-step witness"

{- | A fault of the given constructor and nothing else.

Routing reads constructors, so the payload is irrelevant — and giving it none
is what keeps this group a test of the table rather than of the rules.
-}
leafFault :: Integer -> PD.Data
leafFault tag = PD.Constr tag []

-- | @InvalidOneStepTransition@ wrapping a witness of the given constructor.
oneStepFaultOf :: Integer -> PD.Data
oneStepFaultOf witnessTag = PD.Constr 4 [PD.Constr witnessTag []]

routingTableTests :: [TestTree]
routingTableTests =
  [ testCase ("fault " <> show tag <> " routes to " <> show (routeIndexFor tag)) $
    passertEval $
      ptransitionFaultRouteIndex (asFault (leafFault tag)) #== pconstant (routeIndexFor tag)
  | tag <- [0, 1, 2, 3, 5, 6, 7, 8, 9]
  ]
    <> [ testCase
        ("one-step arm " <> show tag <> " routes to " <> show (oneStepRouteIndexFor tag))
        $ passertEval
        $ ptransitionFaultRouteIndex (asFault (oneStepFaultOf tag))
          #== pconstant (oneStepRouteIndexFor tag)
       | tag <- [0 .. 4]
       ]
    <> [ -- Aiken's @when@ is total over the ten constructors and cannot meet an
         -- eleventh, because its redeemer is structurally decoded first. The port
         -- reads positionally, so the same impossibility has to be written down.
         testCase "a constructor no fault has aborts" $
          pfails $ ptransitionFaultRouteIndex (asFault (leafFault 10))
       , testCase "…as does a one-step witness constructor no arm has" $
          pfails $ ptransitionFaultRouteIndex (asFault (oneStepFaultOf 5))
       ]

asFault :: forall (s :: S). PD.Data -> Term s (PAsData PTransitionFault)
asFault d = punsafeCoerce (pconstant @PData d)

--------------------------------------------------------------------------------
-- route-v1
--------------------------------------------------------------------------------

routeTests :: [TestTree]
routeTests =
  [ testCase "sends a control fault to the first final validator" $
      psucceeds $ route (routing (leafFault 0) (finalScript 0))
  , testCase "…a source-membership mismatch to the second" $
      psucceeds $ route (routing (leafFault 3) (finalScript 1))
  , testCase "…a deposit one-step arm to the sixth" $
      psucceeds $ route (routing (oneStepFaultOf 3) (finalScript 5))
  , testCase "…and a duplicate-event fault to the eighth" $
      psucceeds $ route (routing (leafFault 6) (finalScript 7))
  , {- The check the whole layer exists for. A deposit arm sent to the
       accepted-transaction validator would be adjudicated by a rule that does
       not cover it, so the router has to refuse the output rather than leave it
       to the destination. -}
    testCase "refuses a fault sent to a final validator that is not its own" $
      pfails $ route (routing (oneStepFaultOf 3) (finalScript 4))
  , testCase "refuses a thread whose output state is not the routed proof" $
      pfails $
        route
          (routing (leafFault 0) (finalScript 0)) {rOutputState = Just (leafFault 1)}
  , -- Routing is a thread's first step, so it must not accept one already carrying
    -- state — that would be a second routing of an adjudication in progress.
    testCase "refuses a thread that already carries state" $
      pfails $
        route (routing (leafFault 0) (finalScript 0)) {rInputState = Just (leafFault 0)}
  , testCase "refuses a script-hash list that does not name all eight validators" $
      pfails $
        route
          (routing (leafFault 0) (finalScript 0))
            {rScriptHashes = [finalScript i | i <- [0 .. 6]]}
  , testCase "cancels a routing thread that burns its own token" $
      psucceeds $ routeWith ttThreadName cancelRedeemer [] [cancelMintEntry ttThreadName]
  , testCase "…and not one that burns another's" $
      pfails $ routeWith ttThreadName cancelRedeemer [] [cancelMintEntry otherThreadName]
  ]

-- | The eight final validators, at hashes distinct from every other script here.
finalScript :: Int -> BS.ByteString
finalScript i = BS.replicate 28 (0x40 + fromIntegral i)

data Routing = Routing
  { rProof :: PD.Data
  , rOutputScript :: BS.ByteString
  , rOutputState :: Maybe PD.Data
  , rInputState :: Maybe PD.Data
  , rScriptHashes :: [BS.ByteString]
  }

-- | A well-formed routing of the given fault to the given destination.
routing :: PD.Data -> BS.ByteString -> Routing
routing fault destination =
  Routing
    { rProof = proof
    , rOutputScript = destination
    , rOutputState = Just proof
    , rInputState = Nothing
    , rScriptHashes = [finalScript i | i <- [0 .. 7]]
    }
  where
    -- The router never opens the proof, so a hash and a header it would refuse
    -- are enough; only the fault it wraps is read.
    proof = PD.Constr 0 [PD.B (BS.replicate 28 0x00), PD.Constr 0 [], fault]

route :: forall (s :: S). Routing -> Term s PUnit
route r =
  transitionTraceRouteV1Validator
    # hashList (rScriptHashes r)
    # pdata (pconstant ctPolicy)
    # pconstant
      ( spendContext
          (stepDatum (rInputState r))
          (PD.Constr 1 [PD.Constr 0 [PD.I 0, PD.I 0, rProof r]])
          [ttThreadInput ttThreadName]
          [ttStepOutput (rOutputScript r) ttThreadName (rOutputState r)]
          []
          []
          mempty
      )

-- | The router driven with a redeemer of the caller's choosing, for the cancel arm.
routeWith ::
  forall (s :: S).
  BS.ByteString ->
  PD.Data ->
  [TxOut] ->
  [(ScriptPurpose, Redeemer)] ->
  Term s PUnit
routeWith name redeemer outputs redeemers =
  transitionTraceRouteV1Validator
    # hashList [finalScript i | i <- [0 .. 7]]
    # pdata (pconstant ctPolicy)
    # pconstant
      ( spendContext
          (stepDatum Nothing)
          redeemer
          [ttThreadInput name]
          outputs
          []
          redeemers
          mempty
      )

hashList ::
  forall (s :: S). [BS.ByteString] -> Term s (PAsData (PBuiltinList (PAsData PScriptHash)))
hashList hs = punsafeCoerce (pconstant @PData (PD.List [PD.B h | h <- hs]))

--------------------------------------------------------------------------------
-- The eight final validators
--------------------------------------------------------------------------------

finalTests :: [TestTree]
finalTests =
  [ testCase "control-v1 convicts on a trace-boundary fault" $
      psucceeds $ final control controlConviction
  , testCase "…and not on a duplicate-event fault, which is another's" $
      pfails $ final control duplicateConviction
  , testCase "source-v1 convicts on a source-membership mismatch" $
      psucceeds $ final source sourceConviction
  , testCase "…and not on a trace-boundary fault" $
      pfails $ final source controlConviction
  , testCase "withdrawal-v1 convicts on the valid-withdrawal arm" $
      psucceeds $ final withdrawal withdrawalConviction
  , testCase "…and not on the deposit arm" $
      pfails $ final withdrawal depositConviction
  , testCase "forced-v1 convicts on the forced no-op arm" $
      psucceeds $ final forced forcedConviction
  , testCase "…and not on the withdrawal arm" $
      pfails $ final forced withdrawalConviction
  , testCase "accepted-transaction-v1 convicts on the L2 arm" $
      psucceeds $ final acceptedTransaction acceptedTransactionConviction
  , testCase "…and convicts an accepted transaction with the wrong post root" $
      psucceeds $ final acceptedTransaction acceptedMismatchConviction
  , testCase "…and not on the deposit arm" $
      pfails $ final acceptedTransaction depositConviction
  , testCase "deposit-v1 convicts on the deposit arm" $
      psucceeds $ final deposit depositConviction
  , testCase "…and not on the L2 arm" $
      pfails $ final deposit acceptedTransactionConviction
  , testCase "l1-event-v1 convicts on an out-of-window source event" $
      psucceeds $ final l1Event l1EventConviction
  , testCase "…and not on a trace-boundary fault" $
      pfails $ final l1Event controlConviction
  , testCase "duplicate-v1 convicts on a duplicate-event fault" $
      psucceeds $ final duplicate duplicateConviction
  , testCase "…and not on a trace-boundary fault" $
      pfails $ final duplicate controlConviction
  , {- The conviction is a permanent record, so it has to be parked at the
       always-fails address and named for the thread it ends. Both are
       'finalize' checks rather than this family's, and one validator standing
       for the eight is enough to show the port routes through it. -}
    testCase "a conviction parked anywhere but the fraud-proof address is refused" $
      pfails $
        control
          (conviction controlConviction) {cvFraudProofAddress = otherAddress}
  , testCase "…as is one minted under a name that is not the thread's" $
      pfails $
        control (conviction controlConviction) {cvFraudProofName = otherThreadName}
  , testCase "cancels a final thread that burns its own token" $
      psucceeds $ control (cancelling (cpThreadName controlConviction))
  , testCase "…and not one that burns another's" $
      pfails $ control (cancelling otherThreadName)
  ]

acceptedMismatchConviction :: ConvictingProof
acceptedMismatchConviction =
  ConvictingProof
    { cpThreadName = fst acceptedMismatchProofFixture
    , cpProof = snd acceptedMismatchProofFixture
    }

-- | Abandoning a thread parked at a final validator, burning the named token.
cancelling :: BS.ByteString -> Conviction
cancelling burnt =
  (conviction controlConviction)
    { cvState = Nothing
    , cvRedeemer = cancelRedeemer
    , cvOutputs = Just []
    , cvRedeemers = [cancelMintEntry burnt]
    }

--------------------------------------------------------------------------------
-- Driving the final validators
--------------------------------------------------------------------------------

data Conviction = Conviction
  { cvThreadName :: BS.ByteString
  , cvState :: Maybe PD.Data
  , cvRedeemer :: PD.Data
  , cvFraudProofAddress :: Address
  , cvFraudProofName :: BS.ByteString
  , cvOutputs :: Maybe [TxOut]
  , cvRedeemers :: [(ScriptPurpose, Redeemer)]
  }

{- | A transaction ending a thread in the given conviction.

@final_v1.Args@ names the thread input, the conviction output, the hub oracle
reference input — index 1, behind the deposit UTxO the two hub-reading
validators authenticate — and the fraud-proof policy's mint redeemer.
-}
conviction :: ConvictingProof -> Conviction
conviction c =
  Conviction
    { cvThreadName = cpThreadName c
    , cvState = Just (cpProof c)
    , cvRedeemer = PD.Constr 1 [PD.Constr 0 [PD.I 0, PD.I 0, PD.I 1, PD.I 0]]
    , cvFraudProofAddress = fraudProofAddress
    , cvFraudProofName = cpThreadName c
    , cvOutputs = Nothing
    , cvRedeemers = [fraudProofMintEntry (cpThreadName c)]
    }

convictionContext :: Conviction -> ScriptContext
convictionContext cv =
  spendContext
    (stepDatum (cvState cv))
    (cvRedeemer cv)
    [ttThreadInput (cvThreadName cv)]
    (maybe [convictionOutput (cvFraudProofAddress cv) (cvFraudProofName cv)] id (cvOutputs cv))
    [transitionTraceReferenceInput, transitionTraceHubInput]
    (cvRedeemers cv)
    (singleton fpPolicy (TokenName (toBuiltin (cvFraudProofName cv))) 1)

final ::
  forall (s :: S). (Conviction -> Term s PUnit) -> ConvictingProof -> Term s PUnit
final validator c = validator (conviction c)

control, source, withdrawal, forced :: forall (s :: S). Conviction -> Term s PUnit
control = plainFinal transitionTraceControlV1Validator
source = plainFinal transitionTraceSourceV1Validator
withdrawal = plainFinal transitionTraceWithdrawalV1Validator
forced = plainFinal transitionTraceForcedV1Validator

acceptedTransaction, duplicate :: forall (s :: S). Conviction -> Term s PUnit
acceptedTransaction = plainFinal transitionTraceAcceptedTransactionV1Validator
duplicate = plainFinal transitionTraceDuplicateV1Validator

deposit, l1Event :: forall (s :: S). Conviction -> Term s PUnit
deposit = hubFinal transitionTraceDepositV1Validator
l1Event = hubFinal transitionTraceL1EventV1Validator

plainFinal ::
  forall (s :: S).
  Term
    s
    ( PAsData PCurrencySymbol
        :--> PAsData PCurrencySymbol
        :--> PAsData PAddress
        :--> PScriptContext
        :--> PUnit
    ) ->
  Conviction ->
  Term s PUnit
plainFinal validator cv =
  validator
    # pdata (pconstant ctPolicy)
    # pdata (pconstant fpPolicy)
    # pdata (pconstant fraudProofAddress)
    # pconstant (convictionContext cv)

hubFinal ::
  forall (s :: S).
  Term
    s
    ( PAsData PCurrencySymbol
        :--> PAsData PCurrencySymbol
        :--> PAsData PAddress
        :--> PAsData PScriptHash
        :--> PScriptContext
        :--> PUnit
    ) ->
  Conviction ->
  Term s PUnit
hubFinal validator cv =
  validator
    # pdata (pconstant ctPolicy)
    # pdata (pconstant fpPolicy)
    # pdata (pconstant fraudProofAddress)
    # pdata (pconstant hubOracleHash)
    # pconstant (convictionContext cv)

--------------------------------------------------------------------------------
-- Thread plumbing, at this family's asset name
--------------------------------------------------------------------------------

{- | The transition-trace category id and a block hash.

The fixture's own 'threadName' is another family's, and the envelope reads the
first four bytes — so the router's thread, which never meets an envelope, gets
its own name here rather than borrowing one.
-}
ttThreadName :: BS.ByteString
ttThreadName = "\x00\x00\x00\x04" <> BS.replicate 28 0xa4

ttThreadInput :: BS.ByteString -> TxInInfo
ttThreadInput name =
  TxInInfo
    ownRef
    ( TxOut
        (scriptHashAddress (ScriptHash (toBuiltin stepScript)))
        (adaValue 2_000_000 <> singleton ctPolicy (TokenName (toBuiltin name)) 1)
        NoOutputDatum
        Nothing
    )

ttStepOutput :: BS.ByteString -> BS.ByteString -> Maybe PD.Data -> TxOut
ttStepOutput script name mState =
  TxOut
    (scriptHashAddress (ScriptHash (toBuiltin script)))
    (adaValue 2_000_000 <> singleton ctPolicy (TokenName (toBuiltin name)) 1)
    (OutputDatum (Datum (dataToBuiltinData (stepDatum mState))))
    Nothing

--------------------------------------------------------------------------------
-- Reference inputs
--------------------------------------------------------------------------------

{- | The deposit UTxO both hub-reading validators authenticate, at index 0.

Its resolved output is read back from the same @Data@ the rule tests use rather
than rebuilt, so the two cannot drift into agreeing about a UTxO neither
validator would see. Only the output travels: the rule tests never read an
output reference and encode theirs as V1 does, which a V3 @TxOutRef@ will not
decode.
-}
transitionTraceReferenceInput :: TxInInfo
transitionTraceReferenceInput =
  TxInInfo (outRefN 8) (PlutusTx.unsafeFromData depositReferenceOutput)

-- | The hub oracle, at index 1, carrying the datum that names the deposit policy.
transitionTraceHubInput :: TxInInfo
transitionTraceHubInput =
  TxInInfo
    (outRefN 9)
    ( TxOut
        (scriptHashAddress hubOracleHash)
        ( adaValue 2_000_000
            <> singleton hubPolicy (TokenName (toBuiltin ("MIDGARD_HUB_ORACLE" :: BS.ByteString))) 1
        )
        (OutputDatum (Datum (dataToBuiltinData depositHubOracleDatum)))
        Nothing
    )
