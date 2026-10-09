/**
 * The live devnet adapter for the pooled DA bond journey (ticket #692): a
 * `DaBondPoolJourneyPort` over a journey run directory's process devnet, so
 * the driver in `da-bond-pool-journey.ts` walks its six steps against real
 * blocks, real Kupo and Ogmios, and the deployed validators.
 *
 * Each port method runs the production code path for its action:
 *
 * - commits, attestations and their confirmation: the published-block actor
 *   (`midgard-watcher/tests/support/published-block-actor`), with the
 *   journey's operator and cosigner as the local DA signers. An Apply the SDK
 *   builder refuses with `pool-under-backed` or `pool-withdrawing` is reported
 *   as a refusal carrying that reason; every other failure throws;
 * - Open, responses, settlements, Close, Timeout and the follow-up removal:
 *   the availability command flow of `midgard-node availability-challenge`
 *   (#690), composed in process from its exported steps: the canonical
 *   source on the node's L1 access (its follower store and local node), the
 *   durable operation journal, reconciliation before
 *   every action, a snapshot bracketed by two equal canonical points, the
 *   command's action planner and transaction builder. The composed form is
 *   needed because the CLI entry point resolves only Mainnet, Preprod and
 *   Preview; the devnet is `Custom`;
 * - top-up and the withdrawal quorum (build, one witness per owner and the
 *   fee payer, assemble): the real `midgard-node da-bond` CLI, run as
 *   processes bracketed by `da-bond status` processes (P18,
 *   `da-bond-pool-cli-process.ts`); each chain is the step's `cli` evidence,
 *   and its transaction is confirmed by the adapter's own pool read. The
 *   `da-bond` CLI admits the devnet's `Custom` network (P25, #691) with the
 *   slot mapping from the local node's ledger, and reads through the node's
 *   L1 access (the local node and the node database);
 * - the pool snapshot: `da-bond status` over a `DaBondContext` built as
 *   `loadDaBondContext` builds it, with the devnet's slot configuration and
 *   the ledger tip as its clock. That context never submits;
 * - alerts: the watcher's `deriveWatcherDaBondPoolObservation` over its
 *   authenticated pool read, and the committee view of one real
 *   `da-committee-node` process (P16, P27, `da-bond-pool-committee-process.ts`):
 *   the reasons and the verbatim body of its `GET /readyz`, and the pool
 *   events on its stderr, tied to its pid. Each observation first waits,
 *   within a bound, until the node has read the pool after the call and
 *   agrees with the adapter's snapshot.
 *
 * The committee node (ruling P27) runs from its built `dist/index.js` with L1
 * submission on and preflight on, no DA signer key, no auto-fund key, and two
 * fresh submitter keys distinct from each other and from every operational
 * key. It never holds a journey payload, so it cannot attest, answer the
 * withheld block or Apply: its availability responder must report B1's
 * challenge `unavailable` on stderr and never act on it. It starts before
 * step 1, stops (exit 0 required) before step 2's commit so its payload-free
 * settle and Close cannot race B3, restarts before step 6 and stops again,
 * with the same checks, at the end of step 6. Both submitter addresses must
 * hold the same UTxOs before each start and after each stop. Each
 * observation's wait for the node to read the pool is bounded by ten of its
 * polls plus twice the ideal time for the release confirmation depth. Its DA runtime manifest and
 * libp2p keys (ruling P31, `da-bond-pool-committee-runtime.ts`) come from the
 * real `midgard-node da-libp2p-generate-manifest --target committee` process
 * over fresh keys, before the first transaction; it loads one member's libp2p
 * identity, never that member's DA signing key. If the runtime cannot be
 * produced, or the node's own configuration loader or peer check refuses it,
 * the adapter refuses to start (`DaBondPoolCommitteeUnavailableError`).
 *
 * Preconditions, all checked before the first transaction:
 *
 * - no watcher or DA committee daemon runs against the run directory, by
 *   argument vector or environment, other than the adapter's own committee
 *   node (checked before it starts, after each start, before each step and
 *   before each Open): a watcher would contest the journey's challenges, and
 *   a committee node holding the payload would answer the withheld block;
 * - Kupo indexes every address (`*`), which the adapter's own reads (the
 *   challenge snapshots and the expired-commit spend read) use;
 * - the state queue holds only its root. The journey appends B1 as the head
 *   (`root.next`), because only the head can be removed after its availability
 *   Timeout, so it needs a freshly deployed run directory. A resumed run
 *   (`resume`, a smoke of steps 2 and 6 on a kept devnet) needs instead the
 *   root and the B2 its earlier run recorded, and reuses that run's committee
 *   runtime, keys and database;
 * - the DA params owners the quorum needs are keys this run holds (the
 *   journey operator and cosigner); anything missing is named in a
 *   `DaBondJourneySigningMaterialError`.
 *
 * The challenger is a fresh key kept in `secrets/da-bond-pool-challenger.seed`
 * (mode 0600), distinct from every operational key, funded from the journey's
 * availability account with one exact Open coin per challenge, one collateral
 * coin that covers the worst Timeout fee at the ledger's collateral percentage
 * (G9), and one operating coin for the removal fee and the capital checks.
 *
 * Nothing here enforces operator liveness: inactivity strikes and the
 * scheduler shift act only through strike and advance transactions, a strike
 * needs an undelivered user event, and no process in a journey devnet submits
 * either, so the long waits (the response deadline and the withdraw delay)
 * need no keep-alive commits.
 */
import "node:child_process";
import "node:crypto";
import "node:fs";
import "node:path";
import "node:timers/promises";
import "node:url";
import "@al-ft/midgard-core/availability-operation-journal";
import "@al-ft/midgard-fault-proofs/test-support/transition-trace-retained";
import "@al-ft/midgard-sdk";
import "@lucid-evolution/lucid";
import "@lucid-evolution/scalus-uplc";
import "da-committee-node/config";
import "effect";
import "midgard-node/commands/availability-challenge";
import "midgard-node/commands/availability-challenge-deployment";
import "midgard-node/commands/availability-challenge-source";
import "midgard-node/commands/da-bond";
import "midgard-node/da/local-signers";
import "midgard-watcher";
import "midgard-watcher/tests/support/published-block-actor";
import "./artifacts.js";
import "./da-bond-pool-cli-process.js";
import "./da-bond-pool-committee-process.js";
import "./da-bond-pool-committee-runtime.js";
import "./error-chain.js";
import "./journey-timing.js";
import "./ledger-tip.js";
import "./live-context.js";
import "./da-bond-pool-live-port.select-da-bond-pool-challenger-coins.js";
import "./da-bond-pool-live-port.da-bond-pool-apply-refusal.js";
import "./da-bond-pool-live-port.summarize-da-bond-pool-timeout.js";
import "./da-bond-pool-live-port.await-availability-inclusion.js";
import "./da-bond-pool-live-port.commit-within-ledger-validity.js";
import "./da-bond-pool-live-port.create-live-da-bond-pool-journey-port.js";

import { errorChainTexts } from "./error-chain.js";
export {
  availabilityAttemptRecovery,
  availabilitySubmissionToAwait,
  awaitAvailabilityInclusion,
  awaitQuietJournal,
  landAvailabilitySubmission,
  MAX_LAPSED_REPLANS,
  prepareAvailabilityAttempt,
  settleExpiredCommitReads,
} from "./da-bond-pool-live-port.await-availability-inclusion.js";
export {
  attestWithinLedgerValidity,
  commitWithinLedgerValidity,
  DA_BOND_POOL_COMMITTEE_SUBMITTER_SECRETS,
  DaBondPoolCommitteeUnavailableError,
  DaBondPoolJourneyQueueNotEmptyError,
  DaBondPoolJourneyResumeMismatchError,
  JOURNEY_DEPLOYMENT_MANIFEST,
  type LiveDaBondPoolJourneyPort,
  type LiveDaBondPoolJourneyPortOptions,
  requireResumableQueue,
} from "./da-bond-pool-live-port.commit-within-ledger-validity.js";
export { createLiveDaBondPoolJourneyPort } from "./da-bond-pool-live-port.create-live-da-bond-pool-journey-port.js";
export {
  absentBlockStatus,
  attestRefusalResult,
  awaitTimeBudgetMs,
  DA_BOND_POOL_APPLY_REFUSAL_REASONS,
  type DaBondPoolApplyRefusal,
  daBondPoolApplyRefusal,
  type DaBondPoolJourneyOutput,
  findJourneyDaemons,
  type JourneyProcess,
  nextJourneyBlockInterval,
  planDaBondOwnerQuorum,
  readLinuxProcesses,
  requireJourneySeed,
} from "./da-bond-pool-live-port.da-bond-pool-apply-refusal.js";
export {
  assertDistinctChallengerKey,
  DA_BOND_POOL_AWAIT_TIME_SLACK_MS,
  DA_BOND_POOL_CHALLENGER_OPERATING_LOVELACE,
  DA_BOND_POOL_CHALLENGER_SECRET,
  DA_BOND_POOL_COLLATERAL_MARGIN_LOVELACE,
  type DaBondJourneyHeldKey,
  DaBondJourneySigningMaterialError,
  type DaBondPoolChallengerCoins,
  type DaBondPoolChallengerFundingPlan,
  daBondPoolChallengerFundingShortfall,
  daBondPoolJourneyDirectory,
  daBondPoolJourneyParamsOf,
  JOURNEY_ACCOUNTS_SECRET,
  journeyEndpointsFromRunEnv,
  kupoMatchesEverything,
  type LiveJourneyContext,
  planDaBondPoolChallengerFunding,
  selectDaBondPoolChallengerCoins,
} from "./da-bond-pool-live-port.select-da-bond-pool-challenger-coins.js";
export {
  availabilityEndingError,
  AvailabilityIntentLapsedError,
  type AvailabilityJournalView,
  decodeJourneyTransaction,
  isSpentInputsRebroadcastRefusal,
  isTransientCanonicalError,
  type LedgerValidityRefusal,
  ledgerValidityRefusal,
  summarizeDaBondPoolTimeout,
  unsettledReconciliationError,
  validityIntervalRefusal,
} from "./da-bond-pool-live-port.summarize-da-bond-pool-timeout.js";

export { errorChainTexts };
