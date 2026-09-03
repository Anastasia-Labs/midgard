{- |
Module      : Midgard.ValidationDispute
Description : Plutarch port of @lib/midgard/validation-dispute-v1.ak@.

The interactive bisection game that narrows two disagreeing validation traces
down to a single machine step.

Two parties publish descriptors for the same computation and disagree about where
it ends. Rather than replaying the whole trace on chain, they bisect: the operator
reveals the state at the midpoint, the challenger reveals theirs, and whichever
half they disagree in becomes the new interval. After at most
'pmaxBisectionRounds' rounds the interval is one step wide, and that single step
can be adjudicated directly.

=== Every state transition is an @expect@ chain

Almost nothing here returns a @Bool@. 'popen', 'previalOperatorMidpoint' and
'previalChallengerMidpoint' each abort on any violation and otherwise return the
next dispute state, exactly as the original does — a malformed move is not a
losing move, it is a transaction that does not exist. Only
'pcanOpenBeforeMaturity' and 'pdescriptorsCanDispute' answer @Bool@, because
their callers need to /ask/.

=== The clock is the game

Each move resets 'pdispute'responseDeadline' to @now + response_window@, and
'ptimeoutWinner' hands the game to whoever was not on the clock. That is what
makes the game terminate against a silent opponent, and it is why
'pmaxDisputeDurationMilliseconds' is @(2·rounds + 2)·window@ — the worst case is
both parties using their full window in every round, plus the opening and the
final step.

A dispute may only open while that entire worst case still fits inside the
challenged block's maturity horizon. The boundary is inclusive: a dispute using
the final permitted millisecond stays resolvable, and the next millisecond fails
closed.
-}
module Midgard.ValidationDispute (
  -- * Constants
  pdisputeVersion,
  presponseWindowMilliseconds,
  pmaxBisectionRounds,
  pmaxDisputeDurationMilliseconds,

  -- * Types
  PDisputeTurn (..),
  PValidationDisputeV1 (..),
  PDisputeWinner (..),

  -- * Opening
  pcanOpenBeforeMaturity,
  pdescriptorsCanDispute,
  popen,
  popenAfterSourceVerification,

  -- * Bisecting
  pmidpoint,
  pnextTurn,
  previalOperatorMidpoint,
  previalChallengerMidpoint,

  -- * Ending
  ptimeoutWinner,
) where

import GHC.Generics (Generic)
import Generics.SOP qualified as SOP

import Plutarch.Core.Utils (pand'List)
import Plutarch.Monadic qualified as P
import Plutarch.Prelude

import Midgard.ValidationTrace (
  PValidationTraceDescriptorV1 (..),
  PValidationTraceProof (..),
  pdescriptorIsWellFormed,
  pverifyTraceProof,
 )

--------------------------------------------------------------------------------
-- Constants
--------------------------------------------------------------------------------

-- | Aiken @validation_dispute_v1.dispute_version@ — @1@.
pdisputeVersion :: forall (s :: S). Term s PInteger
pdisputeVersion = 1

-- | Aiken @validation_dispute_v1.response_window_milliseconds@ — five minutes.
presponseWindowMilliseconds :: forall (s :: S). Term s PInteger
presponseWindowMilliseconds = 300_000

{- | Aiken @validation_dispute_v1.max_bisection_rounds@ — @32@.

Enough to bisect any trace the machine can produce: @2^32@ steps is far past
'Midgard.ValidationTrace.pmaxMachineStepCount'.
-}
pmaxBisectionRounds :: forall (s :: S). Term s PInteger
pmaxBisectionRounds = 32

{- | Aiken @validation_dispute_v1.max_dispute_duration_milliseconds@.

@(2 · rounds + 2) · window@: both parties using their full window in every round,
plus the opening move and the final one-step adjudication.
-}
pmaxDisputeDurationMilliseconds :: forall (s :: S). Term s PInteger
pmaxDisputeDurationMilliseconds =
  (2 * pmaxBisectionRounds + 2) * presponseWindowMilliseconds

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

{- | Aiken @validation_dispute_v1.DisputeTurn@ — whose move it is.

@ReadyForOneStep@ carries nothing: the interval is one step wide and there is
nothing left to bisect.
-}
data PDisputeTurn (s :: S)
  = PAwaitingOperator {pawaitingOperator'midpoint :: Term s (PAsData PInteger)}
  | PAwaitingChallenger
      { pawaitingChallenger'midpoint :: Term s (PAsData PInteger)
      , pawaitingChallenger'operatorMidpointHash :: Term s (PAsData PByteString)
      }
  | PReadyForOneStep
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PDisputeTurn)

{- | Aiken @validation_dispute_v1.ValidationDisputeV1@.

The whole game state. @agreed_low_hash@ is the state both parties agree on at
@low_index@; the two @*_high_hash@ fields are what each claims at @high_index@,
and they always differ — that difference is the dispute.
-}
data PValidationDisputeV1 (s :: S) = PValidationDisputeV1
  { pdispute'version :: Term s (PAsData PInteger)
  , pdispute'operatorDescriptor :: Term s (PAsData PValidationTraceDescriptorV1)
  , pdispute'challengerDescriptor :: Term s (PAsData PValidationTraceDescriptorV1)
  , pdispute'lowIndex :: Term s (PAsData PInteger)
  , pdispute'highIndex :: Term s (PAsData PInteger)
  , pdispute'agreedLowHash :: Term s (PAsData PByteString)
  , pdispute'operatorHighHash :: Term s (PAsData PByteString)
  , pdispute'challengerHighHash :: Term s (PAsData PByteString)
  , pdispute'round :: Term s (PAsData PInteger)
  , pdispute'responseDeadline :: Term s (PAsData PInteger)
  , pdispute'turn :: Term s (PAsData PDisputeTurn)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PValidationDisputeV1)

{- | Aiken @validation_dispute_v1.DisputeWinner@.

@NeitherClaimValid@ is not a draw: it is what a timeout at @ReadyForOneStep@
means, where neither party was on the clock and the one-step adjudication simply
never happened.
-}
data PDisputeWinner (s :: S)
  = POperatorWins
  | PChallengerWins
  | PNeitherClaimValid
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PDisputeWinner)

--------------------------------------------------------------------------------
-- Opening
--------------------------------------------------------------------------------

{- | Aiken @validation_dispute_v1.can_open_before_maturity@.

The whole worst-case schedule must still fit inside the challenged block's
maturity horizon. Inclusive at the boundary, by design.
-}
pcanOpenBeforeMaturity ::
  forall (s :: S).
  Term s (PInteger :--> PInteger :--> PInteger :--> PBool)
pcanOpenBeforeMaturity = phoistAcyclic $
  plam $ \currentTimeUpper challengedBlockEndTime maturityDuration ->
    pand'List
      [ 0 #<= currentTimeUpper
      , 0 #<= challengedBlockEndTime
      , pmaxDisputeDurationMilliseconds #<= maturityDuration
      , currentTimeUpper + pmaxDisputeDurationMilliseconds
          #<= challengedBlockEndTime + maturityDuration
      ]

{- | Aiken @validation_dispute_v1.descriptors_can_dispute@.

Two descriptors are disputable when they agree about /what/ was computed and
disagree about the result. Agreeing on the initial state and the step count is
what makes bisection meaningful: without it the two indices would not name the
same steps.
-}
pdescriptorsCanDispute ::
  forall (s :: S).
  Term s (PValidationTraceDescriptorV1 :--> PValidationTraceDescriptorV1 :--> PBool)
pdescriptorsCanDispute = phoistAcyclic $
  plam $ \operator challenger -> P.do
    PValidationTraceDescriptorV1
      { pdescriptor'machineVersion = opMachineVersion
      , pdescriptor'stepCount = opStepCount
      , pdescriptor'initialStateHash = opInitialStateHash
      , pdescriptor'terminalStateHash = opTerminalStateHash
      , pdescriptor'verdict = opVerdict
      , pdescriptor'rejectionCodeHash = opRejectionCodeHash
      } <-
      pmatch operator
    PValidationTraceDescriptorV1
      { pdescriptor'machineVersion = chMachineVersion
      , pdescriptor'stepCount = chStepCount
      , pdescriptor'initialStateHash = chInitialStateHash
      , pdescriptor'terminalStateHash = chTerminalStateHash
      , pdescriptor'verdict = chVerdict
      , pdescriptor'rejectionCodeHash = chRejectionCodeHash
      } <-
      pmatch challenger
    pand'List
      [ pdescriptorIsWellFormed # operator
      , pdescriptorIsWellFormed # challenger
      , opMachineVersion #== chMachineVersion
      , opStepCount #== chStepCount
      , opInitialStateHash #== chInitialStateHash
      ]
      #&& ( pnot
              #$ (opTerminalStateHash #== chTerminalStateHash)
              #&& (opVerdict #== chVerdict)
              #&& (opRejectionCodeHash #== chRejectionCodeHash)
          )

{- | Aiken @validation_dispute_v1.open@.

The opening state: the whole trace is the disputed interval, the agreed low is
the shared initial state, and the clock starts.
-}
popen ::
  forall (s :: S).
  Term
    s
    ( PValidationTraceDescriptorV1
        :--> PValidationTraceDescriptorV1
        :--> PInteger
        :--> PValidationDisputeV1
    )
popen = phoistAcyclic $
  plam $ \operator challenger currentSlot -> P.do
    PValidationTraceDescriptorV1
      { pdescriptor'stepCount
      , pdescriptor'initialStateHash
      , pdescriptor'terminalStateHash = opTerminalStateHash
      } <-
      pmatch operator
    PValidationTraceDescriptorV1 {pdescriptor'terminalStateHash = chTerminalStateHash} <-
      pmatch challenger
    stepCount <- plet $ pfromData pdescriptor'stepCount
    pif
      ( pand'List
          [ 0 #<= currentSlot
          , 0 #< stepCount
          ]
          #&& (pdescriptorsCanDispute # operator # challenger)
      )
      `flip` perror
      $ pcon
        PValidationDisputeV1
          { pdispute'version = pdata pdisputeVersion
          , pdispute'operatorDescriptor = pdata operator
          , pdispute'challengerDescriptor = pdata challenger
          , pdispute'lowIndex = pdata 0
          , pdispute'highIndex = pdata stepCount
          , pdispute'agreedLowHash = pdescriptor'initialStateHash
          , pdispute'operatorHighHash = opTerminalStateHash
          , pdispute'challengerHighHash = chTerminalStateHash
          , pdispute'round = pdata 0
          , pdispute'responseDeadline = pdata (currentSlot + presponseWindowMilliseconds)
          , pdispute'turn = pdata (pnextTurn # 0 # stepCount)
          }

{- | Aiken @validation_dispute_v1.open_after_source_verification@.

Starts the clock only once the source-verification transaction has authenticated
the committed claim. The source hop cannot travel backwards relative to the
opener, and the whole worst-case schedule must still fit the maturity horizon —
so a dispute cannot be opened late enough that it could outlive the block it is
about.
-}
popenAfterSourceVerification ::
  forall (s :: S).
  Term
    s
    ( PValidationTraceDescriptorV1
        :--> PValidationTraceDescriptorV1
        :--> PInteger
        :--> PInteger
        :--> PInteger
        :--> PInteger
        :--> PValidationDisputeV1
    )
popenAfterSourceVerification = phoistAcyclic $
  plam $
    \operator challenger openTimeUpper sourceTimeUpper challengedBlockEndTime maturityDuration ->
      pif
        ( pand'List
            [ 0 #<= openTimeUpper
            , openTimeUpper #<= sourceTimeUpper
            ]
            #&& ( pcanOpenBeforeMaturity
                    # sourceTimeUpper
                    # challengedBlockEndTime
                    # maturityDuration
                )
        )
        `flip` perror
        $ popen
        # operator
        # challenger
        # sourceTimeUpper

--------------------------------------------------------------------------------
-- Bisecting
--------------------------------------------------------------------------------

{- | Aiken @validation_dispute_v1.midpoint@.

@low + (high - low) / 2@ rather than @(low + high) / 2@ — the original's
spelling, and the one that cannot overflow.
-}
pmidpoint :: forall (s :: S). Term s (PInteger :--> PInteger :--> PInteger)
pmidpoint = phoistAcyclic $
  plam $ \low high -> low + pdiv # (high - low) # 2

{- | Aiken @validation_dispute_v1.next_turn@.

An interval one step wide has nothing left to bisect, so the game moves to the
one-step adjudication instead of asking for another midpoint.
-}
pnextTurn :: forall (s :: S). Term s (PInteger :--> PInteger :--> PDisputeTurn)
pnextTurn = phoistAcyclic $
  plam $ \low high ->
    pif
      (high #== low + 1)
      (pcon PReadyForOneStep)
      (pcon (PAwaitingOperator (pdata (pmidpoint # low # high))))

{- | Aiken @validation_dispute_v1.reveal_operator_midpoint@.

The operator states what its trace says at the midpoint, proved against its own
descriptor. Nothing is decided yet — the state is recorded and the clock passes
to the challenger.
-}
previalOperatorMidpoint ::
  forall (s :: S).
  Term
    s
    ( PValidationDisputeV1
        :--> PValidationTraceProof
        :--> PInteger
        :--> PValidationDisputeV1
    )
previalOperatorMidpoint = phoistAcyclic $
  plam $ \dispute proof currentSlot -> P.do
    PValidationDisputeV1
      { pdispute'version
      , pdispute'operatorDescriptor
      , pdispute'responseDeadline
      , pdispute'turn
      } <-
      pmatch dispute
    PValidationTraceProof {ptraceProof'stateIndex, ptraceProof'stateHash} <- pmatch proof
    midpoint' <-
      plet $ pmatch (pfromData pdispute'turn) $ \case
        PAwaitingOperator {pawaitingOperator'midpoint} -> pawaitingOperator'midpoint
        _ -> perror
    pif
      ( pand'List
          [ pfromData pdispute'version #== pdisputeVersion
          , 0 #<= currentSlot
          , currentSlot #<= pfromData pdispute'responseDeadline
          , ptraceProof'stateIndex #== midpoint'
          ]
          #&& (pverifyTraceProof # pfromData pdispute'operatorDescriptor # proof)
      )
      `flip` perror
      $ pwithDeadlineAndTurn
        dispute
        (currentSlot + presponseWindowMilliseconds)
        ( pcon
            ( PAwaitingChallenger
                { pawaitingChallenger'midpoint = midpoint'
                , pawaitingChallenger'operatorMidpointHash = ptraceProof'stateHash
                }
            )
        )

{- | Aiken @validation_dispute_v1.reveal_challenger_midpoint@.

The move that actually bisects. If the challenger's midpoint state agrees with
the operator's, the disagreement is in the /upper/ half and the low bound moves
up; if it disagrees, it is in the lower half and the high bound moves down. Both
@*_high_hash@ fields move with it, so the invariant that they differ is
maintained.

The round cap is checked on the /next/ round, not the current one, so a dispute
cannot take one more round than the schedule budgeted for.
-}
previalChallengerMidpoint ::
  forall (s :: S).
  Term
    s
    ( PValidationDisputeV1
        :--> PValidationTraceProof
        :--> PInteger
        :--> PValidationDisputeV1
    )
previalChallengerMidpoint = phoistAcyclic $
  plam $ \dispute proof currentSlot -> P.do
    PValidationDisputeV1
      { pdispute'version
      , pdispute'operatorDescriptor
      , pdispute'challengerDescriptor
      , pdispute'lowIndex
      , pdispute'highIndex
      , pdispute'agreedLowHash
      , pdispute'operatorHighHash
      , pdispute'challengerHighHash
      , pdispute'round
      , pdispute'responseDeadline
      , pdispute'turn
      } <-
      pmatch dispute
    PValidationTraceProof {ptraceProof'stateIndex, ptraceProof'stateHash} <- pmatch proof
    turnFields <-
      plet $ pmatch (pfromData pdispute'turn) $ \case
        PAwaitingChallenger {pawaitingChallenger'midpoint, pawaitingChallenger'operatorMidpointHash} ->
          pcon (PPair pawaitingChallenger'midpoint pawaitingChallenger'operatorMidpointHash)
        _ -> perror
    PPair midpoint' operatorMidpointHash <- pmatch turnFields
    midpointAgrees <- plet $ operatorMidpointHash #== ptraceProof'stateHash
    nextLow <- plet $ pif midpointAgrees (pfromData midpoint') (pfromData pdispute'lowIndex)
    nextHigh <- plet $ pif midpointAgrees (pfromData pdispute'highIndex) (pfromData midpoint')
    nextRound <- plet $ pfromData pdispute'round + 1
    pif
      ( pand'List
          [ pfromData pdispute'version #== pdisputeVersion
          , 0 #<= currentSlot
          , currentSlot #<= pfromData pdispute'responseDeadline
          , ptraceProof'stateIndex #== midpoint'
          ]
          #&& (pverifyTraceProof # pfromData pdispute'challengerDescriptor # proof)
          #&& (nextLow #< nextHigh)
          #&& (nextRound #<= pmaxBisectionRounds)
      )
      `flip` perror
      $ pcon
        PValidationDisputeV1
          { pdispute'version
          , pdispute'operatorDescriptor
          , pdispute'challengerDescriptor
          , pdispute'lowIndex = pdata nextLow
          , pdispute'highIndex = pdata nextHigh
          , pdispute'agreedLowHash =
              pif midpointAgrees ptraceProof'stateHash pdispute'agreedLowHash
          , pdispute'operatorHighHash =
              pif midpointAgrees pdispute'operatorHighHash operatorMidpointHash
          , pdispute'challengerHighHash =
              pif midpointAgrees pdispute'challengerHighHash ptraceProof'stateHash
          , pdispute'round = pdata nextRound
          , pdispute'responseDeadline = pdata (currentSlot + presponseWindowMilliseconds)
          , pdispute'turn = pdata (pnextTurn # nextLow # nextHigh)
          }

-- | @ValidationDisputeV1 { ..dispute, response_deadline, turn }@.
pwithDeadlineAndTurn ::
  forall (s :: S).
  Term s PValidationDisputeV1 ->
  Term s PInteger ->
  Term s PDisputeTurn ->
  Term s PValidationDisputeV1
pwithDeadlineAndTurn dispute deadline turn =
  pmatch dispute $
    \PValidationDisputeV1
      { pdispute'version
      , pdispute'operatorDescriptor
      , pdispute'challengerDescriptor
      , pdispute'lowIndex
      , pdispute'highIndex
      , pdispute'agreedLowHash
      , pdispute'operatorHighHash
      , pdispute'challengerHighHash
      , pdispute'round
      } ->
        pcon
          PValidationDisputeV1
            { pdispute'version
            , pdispute'operatorDescriptor
            , pdispute'challengerDescriptor
            , pdispute'lowIndex
            , pdispute'highIndex
            , pdispute'agreedLowHash
            , pdispute'operatorHighHash
            , pdispute'challengerHighHash
            , pdispute'round
            , pdispute'responseDeadline = pdata deadline
            , pdispute'turn = pdata turn
            }

--------------------------------------------------------------------------------
-- Ending
--------------------------------------------------------------------------------

{- | Aiken @validation_dispute_v1.timeout_winner@.

Whoever was on the clock loses. At @ReadyForOneStep@ nobody was, so neither claim
is established — the game ran out of time before the step that would have decided
it.

Aborts unless the deadline has actually passed, so this cannot be used to end a
live dispute.
-}
ptimeoutWinner ::
  forall (s :: S).
  Term s (PValidationDisputeV1 :--> PInteger :--> PDisputeWinner)
ptimeoutWinner = phoistAcyclic $
  plam $ \dispute currentSlot -> P.do
    PValidationDisputeV1 {pdispute'version, pdispute'responseDeadline, pdispute'turn} <-
      pmatch dispute
    pif
      ( pfromData pdispute'version
          #== pdisputeVersion
          #&& (pfromData pdispute'responseDeadline #< currentSlot)
      )
      `flip` perror
      $ pmatch (pfromData pdispute'turn)
      $ \case
        PAwaitingOperator _ -> pcon PChallengerWins
        PAwaitingChallenger _ _ -> pcon POperatorWins
        PReadyForOneStep -> pcon PNeitherClaimValid
