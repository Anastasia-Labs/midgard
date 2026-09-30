/**
 * The real committee node behind the live pooled DA bond journey's P16
 * evidence (ruling P27): one unmodified `da-committee-node`, run from its
 * built `dist/index.js`, whose `GET /readyz` body and stderr pool events the
 * journey driver judges.
 *
 * The node submits to L1 (`DA_L1_SUBMISSION_ENABLED=true`), so its
 * coordinator hooks, its tick runner's pool read and its readiness pool
 * reasons all run in production form. It holds no DA signer key and never
 * receives a journey payload, so it cannot attest, answer an availability
 * challenge or Apply. Its two submitter keys are fresh, distinct from each
 * other and from every operational key; the unchanged-UTxO check proves it
 * spent nothing.
 *
 * Everything here that decides something is a pure function or takes its
 * clock, reads and process as dependencies, so both polarities are testable
 * without a node.
 */

import "node:child_process";
import "node:crypto";
import "node:fs";
import "node:path";
import "node:timers/promises";
import "./da-bond-pool-process-evidence.js";
import "./ledger-tip.js";
import "./da-bond-pool-committee-process.build-da-bond-pool-committee-env.js";
import "./da-bond-pool-committee-process.spawn-da-bond-pool-committee-node.js";
import "./da-bond-pool-committee-process.create-da-bond-pool-committee-observer.js";
export {
  buildDaBondPoolCommitteeEnv,
  DA_BOND_POOL_COMMITTEE_OWNED_ENV,
  DA_BOND_POOL_COMMITTEE_REFUSED_ENV,
  DA_BOND_POOL_INHERITED_ENV,
  type DaBondPoolCommitteeEnv,
  DaBondPoolCommitteeEnvError,
  type DaBondPoolCommitteeExpectedView,
  daBondPoolCommitteeExpectedView,
  type DaBondPoolCommitteeKey,
  type DaBondPoolCommitteeSync,
  daBondPoolCommitteeViewAgrees,
  type DaBondPoolReadyzRead,
  redactDaBondPoolEnv,
  worktreeDerivedPort,
} from "./da-bond-pool-committee-process.build-da-bond-pool-committee-env.js";
export {
  createDaBondPoolCommitteeObserver,
  type DaBondPoolCommitteeObservation,
  type DaBondPoolCommitteeObserver,
  DaBondPoolCommitteeProcessError,
  type DaBondPoolCommitteeRecord,
} from "./da-bond-pool-committee-process.create-da-bond-pool-committee-observer.js";
export {
  awaitDaBondPoolCommitteeSync,
  DA_BOND_POOL_RESPONDER_EVENT,
  type DaBondPoolCommitteeExit,
  daBondPoolCommitteeLifecycle,
  type DaBondPoolCommitteeProcess,
  daBondPoolCommitteeStopSettleMs,
  daBondPoolCommitteeSyncBoundMs,
  daBondPoolSubmitterUtxoChange,
  spawnDaBondPoolCommitteeNode,
} from "./da-bond-pool-committee-process.spawn-da-bond-pool-committee-node.js";
