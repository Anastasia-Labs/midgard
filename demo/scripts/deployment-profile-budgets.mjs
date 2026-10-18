// Minimum DA response budget. Before either response window closes, the
// committee must see the open at the profile's confirmation depth, then land
// every chained publication of the largest small-class payload, and it gets one
// more block for its poll and submission. Publications are chained: each one
// spends the previous carrier, so they land one per L1 block. The payload and
// chunk sizes copy demo/midgard-sdk/src/availability-challenge.ts
// DA_AVAILABILITY_SMALL_PAYLOAD_MAX_BYTES and
// DA_AVAILABILITY_RESPONSE_GEOMETRY_MEASUREMENT_CANDIDATE.chunkByteLength; this
// script runs before any package builds, so it cannot import them, and
// midgard-sdk/tests/availability-challenge.test.ts pins the copies.
//
// Every "d blocks of time" budget in this file uses Cardano's 20 s mean block
// interval; production is Poisson, so each budget doubles it.
export const L1_MEAN_BLOCK_MS = 20_000;

export const L1_BLOCK_SAFETY_FACTOR = 2;

export const l1BlocksBudgetMs = (blocks) =>
  blocks * L1_MEAN_BLOCK_MS * L1_BLOCK_SAFETY_FACTOR;

export const DA_SMALL_PAYLOAD_MAX_BYTES = 65_536;

export const DA_RESPONSE_CHUNK_BYTES = 14_020;

export const DA_SMALL_PAYLOAD_CHAINED_PUBLICATIONS = Math.ceil(
  DA_SMALL_PAYLOAD_MAX_BYTES / DA_RESPONSE_CHUNK_BYTES,
);

export const DA_RESPONSE_POLL_AND_SUBMIT_BLOCKS = 1;

export const minimumDaResponseBudgetMs = (profile) =>
  l1BlocksBudgetMs(
    profile.l1_finality.confirmation_depth +
      DA_SMALL_PAYLOAD_CHAINED_PUBLICATIONS +
      DA_RESPONSE_POLL_AND_SUBMIT_BLOCKS,
  );

// Minimum event wait of a public profile. An event's inclusion time is its
// transaction's validity upper bound plus the event wait (user-events.ak,
// order-facts.ak), and a block header's end time is its commit's validity
// upper bound, at most the maximum validity range after the commit's lower
// bound. So every event a header must include was on L1 at least (event wait -
// maximum validity range) before that commit became valid. The floor sizes
// that span as confirmation-depth blocks at twice the mean interval, so an
// honest operator can omit a required event only if L1 rolls back past its
// finality depth or produces under half its mean block rate for the whole
// span. That margin is probabilistic, and weaker than the 3N/f worst case
// config/deployments/README.md uses to size the event wait itself.
export const minimumPublicEventWaitMs = (profile) =>
  profile.timing.max_validity_range_ms +
  l1BlocksBudgetMs(profile.l1_finality.confirmation_depth);

// A canonical decimal string (no sign, exponent, leading or trailing zero, so
// one value has one spelling and one digest) as an exact rational. Undefined
// for anything else.
export const exactDecimal = (value) => {
  const match =
    typeof value === "string"
      ? /^(0|[1-9]\d*)(?:\.(\d*[1-9]))?$/u.exec(value)
      : null;
  if (match === null) return undefined;
  const fraction = match[2] ?? "";
  return {
    numerator: BigInt(match[1] + fraction),
    denominator: 10n ** BigInt(fraction.length),
  };
};

// The most L1 blocks the chain-growth bound guarantees within spanMs:
// floor(spanMs * f / (3 * slot_length_ms)), rounded toward negative infinity.
export const guaranteedBlocksWithinMs = (l1Finality, spanMs) => {
  const f = exactDecimal(l1Finality.active_slot_coeff);
  const numerator = BigInt(spanMs) * f.numerator;
  const denominator = 3n * BigInt(l1Finality.slot_length_ms) * f.denominator;
  const quotient = numerator / denominator;
  return quotient * denominator > numerator ? quotient - 1n : quotient;
};

// Largest d with no inactivity strike: W + N >= 3(d + 1) slot/f + L.
export const largestNoStrikeCommitEventDepth = (profile) =>
  guaranteedBlocksWithinMs(
    profile.l1_finality,
    BigInt(profile.timing.event_wait_ms) +
      BigInt(profile.timing.user_events_negligence_timeout_ms) -
      BigInt(profile.timing.max_validity_range_ms),
  ) - 1n;

// The least span from planning a commit to its TTL, the header end time E:
// the node's history-commit planner refuses an E sooner than this after now.
// The node reads it from the generated profiles
// (HISTORY_COMMIT_MINIMUM_FUTURE_BUFFER_MS), so the planner and the bound
// below share one value.
export const COMMIT_TTL_FUTURE_BUFFER_MS = 30_000;

// Largest d at which a commit stays plannable: W - B - slot >= 3(d + 1) slot/f.
// The anchor caps E at time(A) + W - 1, and the planner needs E at least B
// after now, rounded up to the next slot boundary. When a commit is planned,
// the view's tip is d blocks above A and the next block has not arrived, so
// now is at most 3(d + 1) slot/f after A under the chain-growth bound.
export const largestFeasibleCommitEventDepth = (profile) =>
  guaranteedBlocksWithinMs(
    profile.l1_finality,
    BigInt(profile.timing.event_wait_ms) -
      BigInt(COMMIT_TTL_FUTURE_BUFFER_MS) -
      BigInt(profile.l1_finality.slot_length_ms),
  ) - 1n;

// Largest d a production profile admits: W - L >= 3d slot/f.
export const largestProductionCommitEventDepth = (profile) =>
  guaranteedBlocksWithinMs(
    profile.l1_finality,
    BigInt(profile.timing.event_wait_ms) -
      BigInt(profile.timing.max_validity_range_ms),
  );

// Minimum span a public profile keeps between the latest Apply (end time plus
// the attestation timeout) and the Open deadline (end time plus the challenge
// window). A challenger must first see a timeout-edge Apply at the profile's
// confirmation depth, then land an Open whose validity range may be a full
// maximum range wide (the record's opened_at is its upper bound). The second
// range covers clock skew and the poll-and-submit block.
export const minimumPublicOpenAfterApplyMarginMs = (profile) =>
  2 * profile.timing.max_validity_range_ms +
  l1BlocksBudgetMs(profile.l1_finality.confirmation_depth);

// Earliest a DA committee may complete a pool withdrawal after BeginWithdraw
// and still leave every block it applied slashable. The spec (#685 section 6)
// form: the last applied block ends at most one validity range after
// BeginWithdraw, the open lands inside the challenge window, the response
// deadline is one full window later and the timeout gets the slash grace.
export const specDaBondWithdrawDelayFloorMs = (timing) =>
  timing.max_validity_range_ms +
  timing.da_challenge_window_ms +
  timing.da_full_response_window_ms +
  timing.da_slash_grace_ms;

// The enforced form. An unavailable block is timed out only once it is the
// queue head, and its predecessors merge no earlier than end time plus block
// maturity, so the timeout can wait for maturity rather than the response
// deadline. The grace then starts at whichever is later.
export const daBondWithdrawDelayFloorMs = (timing) =>
  timing.max_validity_range_ms +
  Math.max(
    timing.da_challenge_window_ms + timing.da_full_response_window_ms,
    timing.block_maturity_ms,
  ) +
  timing.da_slash_grace_ms;

// The longest fault-proof thread: a transition-trace proof against the
// accepted L2 transaction (kind 0) whose replay takes the most steps. Each step
// spends the thread output the previous one made, so the steps land one per L1
// block. The thread is a route, a binding, an open, one delete per spend
// input, one certification per 32 outputs, each output's own steps and a
// terminal comparison.
//
// Both replayed fields fill to the per-field bound
// (MIDGARD_MAX_TRANSACTION_AGGREGATE_FIELD_BYTES) after a 3-byte list header;
// a spend input's item is 40 bytes. The output that leaves the most steps per
// field byte is a 40-byte output: an enterprise address, lovelace alone with
// its coin in a one-byte head, and a one-byte inline datum. Its 42-byte item
// takes 7 steps: the phase-11 step, two scan steps, a value step, a datum
// step, the assembly and the insert. A whole output takes one step for an
// item of at least 39 bytes, a lovelace-only output to a protected address is
// staged in 6 steps for an item of at least 39 bytes, and every other staged
// output (assets, a reference script, a longer datum) takes fewer steps per
// byte, so filling the outputs field with that output gives the longest
// thread. The coin's one-byte head is the conservative case; the step count
// assumes no minimum lovelace.
//
// The copies are pinned by midgard-fault-proofs
// tests/submit-init-emulator-transition-trace-final-maximum-lifecycle.test.ts,
// which counts the staged output's steps with the off-chain stepper and runs
// this thread on the emulator.
const MAXIMUM_TRANSACTION_FIELD_BYTES = 32_768;
export const MAXIMUM_TRANSITION_TRACE_SHAPE = Object.freeze({
  spendInputs: Math.floor((MAXIMUM_TRANSACTION_FIELD_BYTES - 3) / 40),
  outputs: Math.floor((MAXIMUM_TRANSACTION_FIELD_BYTES - 3) / 42),
  outputsCertifiedPerStep: 32,
  stepsPerOutput: 7,
});

export const maximumTransitionTraceThreadSteps = () => {
  const { spendInputs, outputs, outputsCertifiedPerStep, stepsPerOutput } =
    MAXIMUM_TRANSITION_TRACE_SHAPE;
  return (
    3 +
    spendInputs +
    Math.ceil(outputs / outputsCertifiedPerStep) +
    outputs * stepsPerOutput +
    1
  );
};

export const maximumTransitionTraceThreadBudgetMs = () =>
  l1BlocksBudgetMs(maximumTransitionTraceThreadSteps());
