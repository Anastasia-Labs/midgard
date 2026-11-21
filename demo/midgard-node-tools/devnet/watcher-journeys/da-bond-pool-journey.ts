/**
 * The pooled DA bond journey (spec #685, ticket #692): one driver that walks
 * the six journey steps through an injected port, so the emulator adapter (a
 * fast dry run) and the live devnet adapter run exactly the same chronology,
 * assertions and report.
 *
 * The driver has no chain or Lucid access of its own. Everything it knows
 * comes from the port, and it asserts only what the port can prove; anything
 * the adapter cannot read is recorded as `not-observable`, never as passed.
 *
 * The chronology respects the state queue's append rule
 * (`state_queue_head_allows_append_v1` in `validators/state-queue.ak`): no
 * block can be appended while the queue head is `Challenged`, or `Unattested`
 * past its attestation timeout. The spec's "Append allowed on Challenged" is
 * wrong. So every committed block is attested (or removed) before the next
 * commit, and no merge is needed:
 *
 * 1. commit and attest B1 against a full pool (step 1);
 * 2. withhold B1, Open, wait out the response deadline, settle, Timeout-slash
 *    the pool, remove B1 (step 3);
 * 3. commit B2; Apply is refused while the pool is short, and both alert views
 *    say so (step 4);
 * 4. top up, then attest B2 within its attestation timeout (step 5);
 * 5. commit and attest B3, Open, answer every chunk, settle, Close (step 2);
 * 6. BeginWithdraw, CancelWithdraw, BeginWithdraw, wait for unlock_at,
 *    CompleteWithdraw (step 6).
 *
 * The ledger is ordered by run order; the report is ordered by the spec's six
 * steps.
 *
 * Process-level evidence (program rulings P16, P18 and P27): the committee's
 * pool readiness reasons and transition events must come from a real
 * `da-committee-node` process (the pool reasons of its `/readyz` body, and
 * the pool transition lines of its stderr written by the pid that was read)
 * at every committee observation, and every top-up and withdraw step must be
 * submitted by the real `midgard-node da-bond` CLI chain. An adapter that
 * composes these in process leaves those assertions `not-observable`; with
 * `requireProcessEvidence` they fail. The first observation after each node
 * start (step 1, and step 6 after the restart) is a negative control: a
 * backed, Bonded pool, no pool reason and no pool event. The same node,
 * holding no payload for B1, must report B1's challenge `unavailable` on
 * stderr and never act on it (step 3).
 */

import "./da-bond-pool-process-evidence.js";
import "./error-chain.js";
import "./da-bond-pool-journey.da-bond-pool-journey-port.js";
import "./da-bond-pool-journey.stage-context.js";
import "./da-bond-pool-journey.try-run-da-bond-pool-journey.js";
import "./da-bond-pool-journey.render-da-bond-pool-journey-report.js";
export {
  DA_BOND_POOL_JOURNEY_CHRONOLOGY,
  DA_BOND_POOL_JOURNEY_COMMITTEE_SIGNALS,
  DA_BOND_POOL_JOURNEY_REFUSALS,
  DA_BOND_POOL_JOURNEY_STEP_NAMES,
  type DaBondPoolJourneyAlerts,
  type DaBondPoolJourneyAssertion,
  type DaBondPoolJourneyAttestResult,
  type DaBondPoolJourneyBlockStatus,
  type DaBondPoolJourneyCliTx,
  type DaBondPoolJourneyCommitIntent,
  type DaBondPoolJourneyCommitResult,
  type DaBondPoolJourneyCommittedBlock,
  type DaBondPoolJourneyOptions,
  type DaBondPoolJourneyParams,
  type DaBondPoolJourneyPort,
  type DaBondPoolJourneyResume,
  type DaBondPoolJourneySnapshot,
  type DaBondPoolJourneyStageTimer,
  type DaBondPoolJourneyStep,
  type DaBondPoolJourneyTimeoutResult,
  type DaBondPoolJourneyTx,
  type DaBondPoolJourneyTxs,
} from "./da-bond-pool-journey.da-bond-pool-journey-port.js";
export {
  type DaBondPoolJourneyReportMeta,
  renderDaBondPoolJourneyReport,
  runDaBondPoolJourney,
} from "./da-bond-pool-journey.render-da-bond-pool-journey-report.js";
export {
  DaBondPoolJourneyFailure,
  type DaBondPoolJourneyObservation,
  type DaBondPoolJourneyRecord,
  type DaBondPoolJourneyStepOutcome,
  planDaBondPoolSlash,
} from "./da-bond-pool-journey.stage-context.js";
export { tryRunDaBondPoolJourney } from "./da-bond-pool-journey.try-run-da-bond-pool-journey.js";
