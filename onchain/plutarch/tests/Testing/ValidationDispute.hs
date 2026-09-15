{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Testing.ValidationDispute
Description : Behavioural tests for the Plutarch port of
              @lib/midgard/validation-dispute-v1.ak@.

The interactive bisection game. Almost every function here aborts rather than
returning a verdict, so most of the suite is @pfails@ — a malformed move is not a
losing move, it is a transaction that does not exist.

__The bisection direction is the thing to get wrong.__ When the challenger's
midpoint state /agrees/ with the operator's, the disagreement must be in the
upper half and the low bound moves up; when it /disagrees/, it is in the lower
half and the high bound moves down. A port that swapped those would still
terminate, still respect the round cap, and still converge on a one-step
interval — the wrong one. Both directions are driven below, and each asserts
where the interval landed and which of the three carried hashes moved with it.

__The clock is the game.__ Every move resets the deadline, and 'ptimeoutWinner'
hands the game to whoever was /not/ on the clock. The three turn states give
three different winners, and the @ReadyForOneStep@ one is not a draw: it means
the game ran out of time before the step that would have decided it.
-}
module Testing.ValidationDispute (tests) where

import Data.ByteString qualified as BS
import PlutusTx.Builtins qualified as Builtins
import PlutusTx.Builtins (fromBuiltin, toBuiltin)
import Test.Tasty
import Test.Tasty.HUnit

import Plutarch.Prelude

import Midgard.ValidationDispute (
  PDisputeTurn (..),
  PDisputeWinner (..),
  PValidationDisputeV1 (..),
  pcanOpenBeforeMaturity,
  pdescriptorsCanDispute,
  pmaxBisectionRounds,
  pmaxDisputeDurationMilliseconds,
  pmidpoint,
  pnextTurn,
  popen,
  popenAfterSourceVerification,
  previalChallengerMidpoint,
  previalOperatorMidpoint,
  presponseWindowMilliseconds,
  ptimeoutWinner,
 )
import Midgard.ValidationTrace (
  PValidationTraceDescriptorV1 (..),
  PValidationTraceProof (..),
  PValidationVerdict (..),
 )
import Testing.Eval (passertEval, pfails)

tests :: TestTree
tests =
  testGroup
    "Validation Dispute Tests"
    [ testGroup "Aiken conformance" aikenConformanceTests
    , testGroup "the schedule" scheduleTests
    , testGroup "what may be disputed" disputableTests
    , testGroup "opening" openTests
    , testGroup "bisecting" bisectTests
    , testGroup "timeouts" timeoutTests
    ]

aikenConformanceTests :: [TestTree]
aikenConformanceTests =
  [ testCase "dispute_turn_and_winner_tags_match_typescript" $
      holds disputeTurnAndWinnerTagsMatchTypescript
  , testCase "bisection_keeps_the_equal_left_boundary_and_first_different_right_boundary" $
      holds aikenBisectionKeepsExactBoundaries
  , testCase "timeout_assigns_loss_to_the_party_that_owes_the_move" $
      holds aikenTimeoutAssignsLossToMover
  , testCase "dispute_open_maturity_boundary_is_exact_and_fails_closed" $
      holds disputeOpenMaturityBoundaryIsExact
  , testCase "delayed_source_verification_starts_a_fresh_bounded_operator_turn" $
      holds delayedSourceVerificationStartsFreshTurn
  , testCase "final_maturity_source_verification_boundary_remains_resolvable" $
      holds finalMaturitySourceVerificationRemainsResolvable
  , testCase "stale_source_verification_after_maturity_boundary_fails_closed" $
      pfails staleSourceVerificationAfterMaturity
  , testCase "source_verification_cannot_predate_authenticated_open_time" $
      pfails sourceVerificationPredatesOpen
  ]

disputeTurnAndWinnerTagsMatchTypescript :: forall s. Term s PBool
disputeTurnAndWinnerTagsMatchTypescript =
  pall'
    [ serialise (pcon $ PAwaitingOperator (pdata 1))
        #== phexByteStr "d8799f01ff"
    , serialise (pcon $ PAwaitingChallenger (pdata 1) (pdata $ pconstant $ hash32 0xaa))
        #== phexByteStr "d87a9f015820aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaff"
    , serialise (pcon PReadyForOneStep)
        #== phexByteStr "d87b80"
    , serialise (pcon POperatorWins)
        #== phexByteStr "d87980"
    , serialise (pcon PChallengerWins)
        #== phexByteStr "d87a80"
    , serialise (pcon PNeitherClaimValid)
        #== phexByteStr "d87b80"
    ]
  where
    serialise value = pserialiseData # pforgetData (pdata value)

disputeOpenMaturityBoundaryIsExact :: forall s. Term s PBool
disputeOpenMaturityBoundaryIsExact =
  plet (1_000_000 + 604_800_000 - pmaxDisputeDurationMilliseconds) $ \finalOpenTime ->
    pall'
      [ pcanOpenBeforeMaturity # finalOpenTime # 1_000_000 # 604_800_000
      , pnot #$ pcanOpenBeforeMaturity # (finalOpenTime + 1) # 1_000_000 # 604_800_000
      , pnot #$ pcanOpenBeforeMaturity
          # finalOpenTime # 1_000_000 # (pmaxDisputeDurationMilliseconds - 1)
      , pnot #$ pcanOpenBeforeMaturity # (-1) # 1_000_000 # 604_800_000
      ]

delayedSourceVerificationStartsFreshTurn :: forall s. Term s PBool
delayedSourceVerificationStartsFreshTurn =
  plet
    ( popenAfterSourceVerification
        # operatorD # challengerD # 100 # 200 # 1_000_000 # 604_800_000
    )
    $ \dispute ->
      deadlineOf dispute #== 200 + presponseWindowMilliseconds
        #&& pnot # (deadlineOf dispute #== 100 + presponseWindowMilliseconds)

finalMaturitySourceVerificationRemainsResolvable :: forall s. Term s PBool
finalMaturitySourceVerificationRemainsResolvable =
  plet (1_000_000 + 604_800_000 - pmaxDisputeDurationMilliseconds) $ \finalSourceTime ->
  plet
    ( popenAfterSourceVerification
        # operatorD # challengerD # 100 # finalSourceTime # 1_000_000 # 604_800_000
    )
    $ \dispute ->
      deadlineOf dispute #== finalSourceTime + presponseWindowMilliseconds

staleSourceVerificationAfterMaturity :: forall s. Term s PValidationDisputeV1
staleSourceVerificationAfterMaturity =
  plet (1_000_000 + 604_800_000 - pmaxDisputeDurationMilliseconds) $ \finalSourceTime ->
    popenAfterSourceVerification
      # operatorD # challengerD # 100 # (finalSourceTime + 1) # 1_000_000 # 604_800_000

sourceVerificationPredatesOpen :: forall s. Term s PValidationDisputeV1
sourceVerificationPredatesOpen =
  popenAfterSourceVerification
    # operatorD # challengerD # 100 # 99 # 1_000_000 # 604_800_000

aikenBisectionKeepsExactBoundaries :: forall s. Term s PBool
aikenBisectionKeepsExactBoundaries =
  plet (popen # aikenOperatorD # aikenChallengerD # 100) $ \opened ->
  plet
    ( previalOperatorMidpoint
        # opened # proofIn aikenOperatorLeaves 1 (hash32 0xbb) # 101
    )
    $ \withOperatorOne ->
  plet
    ( previalChallengerMidpoint
        # withOperatorOne # proofIn aikenChallengerLeaves 1 (hash32 0xbb) # 102
    )
    $ \afterEqualOne ->
  plet
    ( previalOperatorMidpoint
        # afterEqualOne # proofIn aikenOperatorLeaves 2 (hash32 0xcc) # 103
    )
    $ \withOperatorTwo ->
  plet
    ( previalChallengerMidpoint
        # withOperatorTwo # proofIn aikenChallengerLeaves 2 (hash32 0xee) # 104
    )
    $ \ready ->
      pall'
        [ lowOf ready #== 1
        , highOf ready #== 2
        , agreedLowOf ready #== pconstant (hash32 0xbb)
        , operatorHighOf ready #== pconstant (hash32 0xcc)
        , challengerHighOf ready #== pconstant (hash32 0xee)
        , turnIsReady (turnOf ready)
        , roundOf ready #== 2
        ]

aikenTimeoutAssignsLossToMover :: forall s. Term s PBool
aikenTimeoutAssignsLossToMover =
  plet (popen # aikenOperatorD # aikenChallengerD # 100) $ \opened ->
  plet
    ( previalOperatorMidpoint
        # opened # proofIn aikenOperatorLeaves 1 (hash32 0xbb) # 101
    )
    $ \awaitingChallenger ->
      winnerIs (ptimeoutWinner # opened # 300_101) 1
        #&& winnerIs (ptimeoutWinner # awaitingChallenger # 300_102) 0

--------------------------------------------------------------------------------
-- The schedule
--------------------------------------------------------------------------------

scheduleTests :: [TestTree]
scheduleTests =
  [ testCase "the worst case is both parties using every window, plus two" $
      holds $ pmaxDisputeDurationMilliseconds #== pconstant ((2 * 32 + 2) * 300_000)
  , testCase "…which is 66 windows" $
      holds $ pmaxDisputeDurationMilliseconds #== 66 * presponseWindowMilliseconds
  , testCase "…over 32 rounds" $ holds $ pmaxBisectionRounds #== 32
  , {- Inclusive at the boundary, by design: a dispute using the final permitted
       millisecond stays resolvable, and the next one fails closed. -}
    testCase "a dispute may open on the last permitted millisecond" $
      holds $ pcanOpenBeforeMaturity # pconstant lastPermitted # 1_000_000 # horizon
  , testCase "…and not one millisecond later" $
      refuses $ pcanOpenBeforeMaturity # pconstant (lastPermitted + 1) # 1_000_000 # horizon
  , testCase "a maturity horizon shorter than the schedule admits nothing" $
      refuses $ pcanOpenBeforeMaturity # 0 # 1_000_000 # (pmaxDisputeDurationMilliseconds - 1)
  , testCase "…and one exactly as long as it does" $
      holds $ pcanOpenBeforeMaturity # 0 # 1_000_000 # pmaxDisputeDurationMilliseconds
  , testCase "a negative current time is refused" $
      refuses $ pcanOpenBeforeMaturity # (-1) # 1_000_000 # horizon
  , testCase "a negative block end time is refused" $
      refuses $ pcanOpenBeforeMaturity # 0 # (-1) # horizon
  ]
  where
    horizon = 100_000_000
    -- current + max_duration <= end + maturity
    lastPermitted = 1_000_000 + 100_000_000 - 19_800_000

--------------------------------------------------------------------------------
-- What may be disputed
--------------------------------------------------------------------------------

{- | Two descriptors are disputable when they agree about /what/ was computed and
disagree about the result.

Agreeing on the initial state and the step count is what makes bisection
meaningful — without it the two indices would not name the same steps — and each
of the three disagreement grounds is enough on its own.
-}
disputableTests :: [TestTree]
disputableTests =
  [ testCase "differing terminal states are disputable" $
      holds $ pdescriptorsCanDispute # operatorD # challengerD
  , {- Verdict and rejection code cannot differ independently: a well-formed
       descriptor ties them together, so `Accepted` forces the null rejection
       hash. These two cases are therefore the only shapes the third and fourth
       disagreement grounds can take. -}
    testCase "…as is a differing verdict at the same terminal state" $
      holds $ pdescriptorsCanDispute # operatorD # rejectedD1
  , testCase "…and a differing rejection code between two rejections" $
      holds $ pdescriptorsCanDispute # rejectedD1 # rejectedD2
  , testCase "two identical descriptors are not disputable" $
      refuses $ pdescriptorsCanDispute # operatorD # operatorD
  , testCase "descriptors over different step counts are not disputable" $
      refuses $ pdescriptorsCanDispute # operatorD # shortD
  , testCase "…nor over different initial states" $
      refuses $ pdescriptorsCanDispute # operatorD # otherInitialD
  , testCase "a malformed descriptor is not disputable whatever it claims" $
      refuses $ pdescriptorsCanDispute # operatorD # malformedD
  ]

--------------------------------------------------------------------------------
-- Opening
--------------------------------------------------------------------------------

openTests :: [TestTree]
openTests =
  [ testCase "the opening interval is the whole trace" $
      holds $ (lowOf openedT #== 0) #&& (highOf openedT #== 8)
  , testCase "…the agreed low is the shared initial state" $
      holds $ agreedLowOf openedT #== pconstant initialHash
  , testCase "…the two high hashes are the two claims" $
      holds $
        (operatorHighOf openedT #== pconstant terminalHash)
          #&& (challengerHighOf openedT #== pconstant otherTerminalHash)
  , testCase "…the round is zero and the clock has started" $
      holds $
        (roundOf openedT #== 0)
          #&& (deadlineOf openedT #== pconstant (openSlot + 300_000))
  , testCase "…and the operator is on the clock at the midpoint" $
      holds $ turnIsAwaitingOperator openedT 4
  , testCase "opening two indisputable descriptors aborts" $
      pfails $ popen # operatorD # operatorD # pconstant openSlot
  , testCase "opening at a negative slot aborts" $
      pfails $ popen # operatorD # challengerD # (-1)
  , testCase "opening a zero-step trace aborts" $
      pfails $
        popen
          # descriptorWith operatorRoot 0 initialHash terminalHash False noRejection
          # descriptorWith agreeRoot 0 initialHash otherTerminalHash False noRejection
          # pconstant openSlot
  , -- The source hop cannot travel backwards relative to the opener.
    testCase "source verification after the opening time is accepted" $
      holds $
        lowOf (popenAfterSourceVerification # operatorD # challengerD # 100 # 200 # 1_000_000 # 100_000_000)
          #== 0
  , testCase "…and before it aborts" $
      pfails $
        popenAfterSourceVerification # operatorD # challengerD # 200 # 100 # 1_000_000 # 100_000_000
  , testCase "…and a source time past the maturity horizon aborts" $
      pfails $
        popenAfterSourceVerification # operatorD # challengerD # 0 # 90_000_000 # 1_000_000 # 20_000_000
  ]

--------------------------------------------------------------------------------
-- Bisecting
--------------------------------------------------------------------------------

{- | The midpoint arithmetic, then both directions of the bisection.

@low + (high - low) / 2@ rather than @(low + high) / 2@ — the original's
spelling. On the intervals the game can reach the two agree, but the port keeps
the original's because it is the one that cannot overflow.
-}
bisectTests :: [TestTree]
bisectTests =
  [ testCase "the midpoint splits an interval" $
      holds $
        pall'
          [ (pmidpoint # pconstant lo # pconstant hi) #== pconstant expected
          | (lo, hi, expected) <-
              [(0, 8, 4), (0, 2, 1), (4, 8, 6), (0, 1, 0), (3, 4, 3), (1, 8, 4)]
          ]
  , testCase "an interval one step wide is ready for the one-step" $
      holds $ turnIsReady (pnextTurn # 3 # 4)
  , testCase "…and a wider one asks the operator for a midpoint" $
      holds $ turnIsAwaitingOperatorT (pnextTurn # 0 # 8) 4
  , testCase "the operator's reveal puts the challenger on the clock" $
      holds $ turnIsAwaitingChallenger afterOperatorT 4 (stateHash 4)
  , testCase "…and resets the deadline" $
      holds $ deadlineOf afterOperatorT #== pconstant (revealSlot + 300_000)
  , {- Agreement at the midpoint: the disagreement is in the upper half, so the
       low bound moves up to the midpoint and the agreed low becomes that state.
       The two high hashes are untouched. -}
    testCase "an agreeing midpoint moves the low bound up" $
      holds $
        (lowOf agreeT #== 4)
          #&& (highOf agreeT #== 8)
          #&& (agreedLowOf agreeT #== pconstant (stateHash 4))
          #&& (operatorHighOf agreeT #== pconstant terminalHash)
          #&& (challengerHighOf agreeT #== pconstant otherTerminalHash)
  , {- Disagreement: the lower half, so the high bound comes down and both high
       hashes become the two midpoint claims. -}
    testCase "a disagreeing midpoint moves the high bound down" $
      holds $
        (lowOf disagreeT #== 0)
          #&& (highOf disagreeT #== 4)
          #&& (agreedLowOf disagreeT #== pconstant initialHash)
          #&& (operatorHighOf disagreeT #== pconstant (stateHash 4))
          #&& (challengerHighOf disagreeT #== pconstant altMidpointHash)
  , testCase "either way the round advances and the clock resets" $
      holds $
        (roundOf agreeT #== 1)
          #&& (roundOf disagreeT #== 1)
          #&& (deadlineOf agreeT #== pconstant (challengerSlot + 300_000))
  , testCase "a reveal after the deadline aborts" $
      pfails $ previalOperatorMidpoint # openedT # operatorMidpointProof # pconstant (openSlot + 300_001)
  , testCase "…on the last permitted millisecond it does not" $
      holds $ lowOf (previalOperatorMidpoint # openedT # operatorMidpointProof # pconstant (openSlot + 300_000)) #== 0
  , testCase "a reveal at an index that is not the midpoint aborts" $
      pfails $ previalOperatorMidpoint # openedT # proofIn operatorLeaves 5 (stateHash 5) # pconstant revealSlot
  , testCase "a reveal whose proof does not verify aborts" $
      pfails $ previalOperatorMidpoint # openedT # badProofIn operatorLeaves 4 (stateHash 4) # pconstant revealSlot
  , -- Out of turn: the operator cannot answer its own reveal, nor the challenger open one.
    testCase "the challenger cannot move while the operator is on the clock" $
      pfails $ previalChallengerMidpoint # openedT # agreeProof # pconstant revealSlot
  , testCase "…and the operator cannot move twice" $
      pfails $ previalOperatorMidpoint # afterOperatorT # operatorMidpointProof # pconstant challengerSlot
  , testCase "a dispute at another version aborts" $
      pfails $ previalOperatorMidpoint # wrongVersionT # operatorMidpointProof # pconstant revealSlot
  ]

--------------------------------------------------------------------------------
-- Timeouts
--------------------------------------------------------------------------------

timeoutTests :: [TestTree]
timeoutTests =
  [ testCase "the operator failing to answer hands it to the challenger" $
      holds $ winnerIs (ptimeoutWinner # openedT # pconstant (openSlot + 300_001)) 1
  , testCase "the challenger failing to answer hands it to the operator" $
      holds $ winnerIs (ptimeoutWinner # afterOperatorT # pconstant (revealSlot + 300_001)) 0
  , {- Not a draw: at ReadyForOneStep nobody was on the clock, so the game ran out
       of time before the step that would have decided it. -}
    testCase "a timeout with nobody on the clock establishes neither claim" $
      holds $ winnerIs (ptimeoutWinner # readyT # 999_999_999) 2
  , testCase "a timeout before the deadline aborts" $
      pfails $ ptimeoutWinner # openedT # pconstant (openSlot + 300_000)
  , testCase "…and at another version" $
      pfails $ ptimeoutWinner # wrongVersionT # 999_999_999
  ]

--------------------------------------------------------------------------------
-- The fixture
--------------------------------------------------------------------------------

{- | Three eight-step traces with real Merkle roots.

The proofs below are genuine paths through those roots, so 'pverifyTraceProof'
has something to verify rather than something to wave through — a fixture with a
fabricated root would let every reveal pass and the turn logic would be tested
against nothing.

The two challenger traces are what make both bisection directions real. One
agrees with the operator at the midpoint and differs only at the terminal state;
the other differs at the midpoint too. A single challenger trace could only ever
exercise one direction.
-}
stepCount :: Int
stepCount = 8

stateHash :: Int -> BS.ByteString
stateHash i = hash32 (fromIntegral (0x10 + i))

initialHash, terminalHash, otherTerminalHash, altMidpointHash :: BS.ByteString
initialHash = stateHash 0
terminalHash = stateHash stepCount
otherTerminalHash = hash32 0x77
altMidpointHash = hash32 0x44

-- | @no_rejection_code_hash@ — what an accepted descriptor must carry.
noRejection :: BS.ByteString
noRejection = BS.replicate 32 0x00

rejectionA, rejectionB :: BS.ByteString
rejectionA = hash32 0x66
rejectionB = hash32 0x67

openSlot, revealSlot, challengerSlot :: Integer
openSlot = 1_000
revealSlot = openSlot + 1_000
challengerSlot = revealSlot + 1_000

hash32 :: Integer -> BS.ByteString
hash32 n = BS.replicate 32 (fromIntegral n)

--------------------------------------------------------------------------------
-- The trace trees, reimplemented from validation-trace-v1.ak
--------------------------------------------------------------------------------

blake2b256 :: BS.ByteString -> BS.ByteString
blake2b256 = fromBuiltin . Builtins.blake2b_256 . toBuiltin

leafHash :: BS.ByteString -> BS.ByteString
leafHash h = blake2b256 ("MidgardValidationTraceLeafV1" <> h)

branchHash :: BS.ByteString -> BS.ByteString -> BS.ByteString
branchHash l r = blake2b256 ("MidgardValidationTraceBranchV1" <> l <> r)

-- | @trace_depth@ — the smallest depth whose capacity covers @n + 1@ states.
traceDepth :: Int -> Int
traceDepth n = go 1 0
  where
    go capacity d
      | capacity >= n + 1 = d
      | otherwise = go (capacity * 2) (d + 1)

-- | The operator's leaves, and the two challenger variants.
operatorLeaves, agreeLeaves, disagreeLeaves :: [BS.ByteString]
operatorLeaves = [stateHash i | i <- [0 .. 2 ^ traceDepth stepCount - 1]]
agreeLeaves = replaceAt stepCount otherTerminalHash operatorLeaves
disagreeLeaves = replaceAt 4 altMidpointHash agreeLeaves

replaceAt :: Int -> a -> [a] -> [a]
replaceAt i x xs = take i xs <> [x] <> drop (i + 1) xs

levelsOf :: [BS.ByteString] -> [[BS.ByteString]]
levelsOf level
  | length level <= 1 = [level]
  | otherwise = level : levelsOf (pairUp level)
  where
    pairUp (a : b : rest) = branchHash a b : pairUp rest
    pairUp _ = []

rootOf :: [BS.ByteString] -> BS.ByteString
rootOf = head . last . levelsOf . map leafHash

{- | The bottom-up sibling path for a leaf.

The fold takes the sibling on the right when the index is even and on the left
when it is odd, halving the index each level — so the path is read in that same
order.
-}
siblingsIn :: [BS.ByteString] -> Int -> [BS.ByteString]
siblingsIn leaves i = go i (init (levelsOf (map leafHash leaves)))
  where
    go _ [] = []
    go idx (level : rest) =
      (level !! (if even idx then idx + 1 else idx - 1)) : go (idx `div` 2) rest

operatorRoot, agreeRoot, disagreeRoot :: BS.ByteString
operatorRoot = rootOf operatorLeaves
agreeRoot = rootOf agreeLeaves
disagreeRoot = rootOf disagreeLeaves

aikenOperatorLeaves, aikenChallengerLeaves :: [BS.ByteString]
aikenOperatorLeaves = map hash32 [0xaa, 0xbb, 0xcc, 0xdd]
aikenChallengerLeaves = map hash32 [0xaa, 0xbb, 0xee, 0xff]

--------------------------------------------------------------------------------
-- Building terms
--------------------------------------------------------------------------------

descriptorWith ::
  forall (s :: S).
  BS.ByteString ->
  Integer ->
  BS.ByteString ->
  BS.ByteString ->
  Bool ->
  BS.ByteString ->
  Term s PValidationTraceDescriptorV1
descriptorWith root steps initial terminal rejected rejection =
  pcon $
    PValidationTraceDescriptorV1
      { pdescriptor'schemaVersion = pdata 1
      , pdescriptor'machineVersion = pdata 1
      , pdescriptor'traceRoot = pdata (pconstant root)
      , pdescriptor'stepCount = pdata (pconstant steps)
      , pdescriptor'initialStateHash = pdata (pconstant initial)
      , pdescriptor'terminalStateHash = pdata (pconstant terminal)
      , pdescriptor'verdict = pdata (pcon (if rejected then PRejected else PAccepted))
      , pdescriptor'rejectionCodeHash = pdata (pconstant rejection)
      }

operatorD, challengerD, challengerAltD :: forall (s :: S). Term s PValidationTraceDescriptorV1
operatorD = descriptorWith operatorRoot 8 initialHash terminalHash False noRejection
challengerD = descriptorWith agreeRoot 8 initialHash otherTerminalHash False noRejection
challengerAltD = descriptorWith disagreeRoot 8 initialHash otherTerminalHash False noRejection

aikenOperatorD, aikenChallengerD :: forall (s :: S). Term s PValidationTraceDescriptorV1
aikenOperatorD =
  descriptorWith
    (rootOf aikenOperatorLeaves) 3 (hash32 0xaa) (hash32 0xdd) False noRejection
aikenChallengerD =
  descriptorWith
    (rootOf aikenChallengerLeaves) 3 (hash32 0xaa) (hash32 0xff) False noRejection

rejectedD1, rejectedD2, shortD, otherInitialD, malformedD ::
  forall (s :: S). Term s PValidationTraceDescriptorV1
rejectedD1 = descriptorWith operatorRoot 8 initialHash terminalHash True rejectionA
rejectedD2 = descriptorWith operatorRoot 8 initialHash terminalHash True rejectionB
shortD = descriptorWith agreeRoot 7 initialHash otherTerminalHash False noRejection
otherInitialD = descriptorWith agreeRoot 8 (hash32 0x55) otherTerminalHash False noRejection

-- | Accepted, but carrying a rejection code — which the binding rule forbids.
malformedD = descriptorWith agreeRoot 8 initialHash otherTerminalHash False rejectionA

proofIn ::
  forall (s :: S). [BS.ByteString] -> Integer -> BS.ByteString -> Term s PValidationTraceProof
proofIn leaves index hash = proofWith index hash (siblingsIn leaves (fromIntegral index))

badProofIn ::
  forall (s :: S). [BS.ByteString] -> Integer -> BS.ByteString -> Term s PValidationTraceProof
badProofIn leaves index hash =
  proofWith index hash [hash32 0xee | _ <- siblingsIn leaves (fromIntegral index)]

proofWith ::
  forall (s :: S).
  Integer ->
  BS.ByteString ->
  [BS.ByteString] ->
  Term s PValidationTraceProof
proofWith index hash siblings =
  pcon $
    PValidationTraceProof
      { ptraceProof'stateIndex = pdata (pconstant index)
      , ptraceProof'stateHash = pdata (pconstant hash)
      , ptraceProof'siblings =
          pdata (foldr (\b acc -> pcons # pdata (pconstant b) # acc) pnil siblings)
      }

-- | The operator's own midpoint proof, and the two challenger answers to it.
operatorMidpointProof, agreeProof, disagreeProof ::
  forall (s :: S). Term s PValidationTraceProof
operatorMidpointProof = proofIn operatorLeaves 4 (stateHash 4)
agreeProof = proofIn agreeLeaves 4 (stateHash 4)
disagreeProof = proofIn disagreeLeaves 4 altMidpointHash

openedT, openedAltT, afterOperatorT, afterOperatorAltT, agreeT, disagreeT ::
  forall (s :: S). Term s PValidationDisputeV1
openedT = popen # operatorD # challengerD # pconstant openSlot
openedAltT = popen # operatorD # challengerAltD # pconstant openSlot
afterOperatorT =
  previalOperatorMidpoint # openedT # operatorMidpointProof # pconstant revealSlot
afterOperatorAltT =
  previalOperatorMidpoint # openedAltT # operatorMidpointProof # pconstant revealSlot
agreeT = previalChallengerMidpoint # afterOperatorT # agreeProof # pconstant challengerSlot
disagreeT =
  previalChallengerMidpoint # afterOperatorAltT # disagreeProof # pconstant challengerSlot

-- | A dispute already narrowed to one step, so nobody is on the clock.
readyT :: forall (s :: S). Term s PValidationDisputeV1
readyT = withTurn openedT (pcon PReadyForOneStep)

wrongVersionT :: forall (s :: S). Term s PValidationDisputeV1
wrongVersionT = pmatch openedT $ \d -> pcon d {pdispute'version = pdata 2}

withTurn ::
  forall (s :: S). Term s PValidationDisputeV1 -> Term s PDisputeTurn -> Term s PValidationDisputeV1
withTurn dispute turn = pmatch dispute $ \d -> pcon d {pdispute'turn = pdata turn}

--------------------------------------------------------------------------------
-- Reading terms
--------------------------------------------------------------------------------

lowOf, highOf, roundOf, deadlineOf ::
  forall (s :: S). Term s PValidationDisputeV1 -> Term s PInteger
lowOf d = pmatch d $ \PValidationDisputeV1 {pdispute'lowIndex} -> pfromData pdispute'lowIndex
highOf d = pmatch d $ \PValidationDisputeV1 {pdispute'highIndex} -> pfromData pdispute'highIndex
roundOf d = pmatch d $ \PValidationDisputeV1 {pdispute'round} -> pfromData pdispute'round
deadlineOf d =
  pmatch d $ \PValidationDisputeV1 {pdispute'responseDeadline} -> pfromData pdispute'responseDeadline

agreedLowOf, operatorHighOf, challengerHighOf ::
  forall (s :: S). Term s PValidationDisputeV1 -> Term s PByteString
agreedLowOf d =
  pmatch d $ \PValidationDisputeV1 {pdispute'agreedLowHash} -> pfromData pdispute'agreedLowHash
operatorHighOf d =
  pmatch d $ \PValidationDisputeV1 {pdispute'operatorHighHash} -> pfromData pdispute'operatorHighHash
challengerHighOf d =
  pmatch d $ \PValidationDisputeV1 {pdispute'challengerHighHash} ->
    pfromData pdispute'challengerHighHash

turnOf :: forall (s :: S). Term s PValidationDisputeV1 -> Term s PDisputeTurn
turnOf d = pmatch d $ \PValidationDisputeV1 {pdispute'turn} -> pfromData pdispute'turn

turnIsAwaitingOperator ::
  forall (s :: S). Term s PValidationDisputeV1 -> Integer -> Term s PBool
turnIsAwaitingOperator d = turnIsAwaitingOperatorT (turnOf d)

turnIsAwaitingOperatorT :: forall (s :: S). Term s PDisputeTurn -> Integer -> Term s PBool
turnIsAwaitingOperatorT t expected =
  pmatch t $ \case
    PAwaitingOperator m -> pfromData m #== pconstant expected
    _ -> pconstant False

turnIsAwaitingChallenger ::
  forall (s :: S). Term s PValidationDisputeV1 -> Integer -> BS.ByteString -> Term s PBool
turnIsAwaitingChallenger d expected expectedHash =
  pmatch (turnOf d) $ \case
    PAwaitingChallenger m h ->
      pfromData m #== pconstant expected #&& pfromData h #== pconstant expectedHash
    _ -> pconstant False

turnIsReady :: forall (s :: S). Term s PDisputeTurn -> Term s PBool
turnIsReady t = pmatch t $ \case
  PReadyForOneStep -> pconstant True
  _ -> pconstant False

-- | @0@ operator, @1@ challenger, @2@ neither.
winnerIs :: forall (s :: S). Term s PDisputeWinner -> Integer -> Term s PBool
winnerIs w expected =
  pmatch w $ \case
    POperatorWins -> pconstant expected #== (0 :: Term s PInteger)
    PChallengerWins -> pconstant expected #== (1 :: Term s PInteger)
    PNeitherClaimValid -> pconstant expected #== (2 :: Term s PInteger)

holds :: (forall (s :: S). Term s PBool) -> Assertion
holds = passertEval

refuses :: (forall (s :: S). Term s PBool) -> Assertion
refuses p = passertEval (pnot # p)

pall' :: forall (s :: S). [Term s PBool] -> Term s PBool
pall' = foldr (#&&) (pconstant True)
