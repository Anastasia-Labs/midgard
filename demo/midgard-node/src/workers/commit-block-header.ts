/**
 * Production block-commit worker entrypoint.
 * This module orchestrates MPF root processing, commit transaction
 * assembly, submission, and recovery by composing the smaller worker helpers.
 */

import "node:crypto";
import "@al-ft/midgard-sdk";
import "@effect/sql";
import "@lucid-evolution/lucid";
import "effect";
import "worker_threads";
import "../database/index.js";
import "../database/utils/common.js";
import "../database/utils/ledger.js";
import "../database/utils/tx.js";
import "../e2e/commit-crash-checkpoint.js";
import "../fibers/fetch-and-insert-tx-order-utxos.js";
import "../lucid-time.js";
import "../mpf/index.js";
import "../services/event-history-producer.js";
import "../services/history-commit-window.js";
import "../services/index.js";
import "../transactions/state-queue/confirmed-ledger-snapshot.js";
import "../transactions/utils.js";
import "../tx-context.js";
import "./commit-block-header/build-unsigned-tx.js";
import "./commit-block-header/event-roots.js";
import "./commit-block-header/state-queue.js";
import "./commit-block-header/submission.js";
import "./commit-block-header/transition-commitments.js";
import "./utils/commit-block-header.js";
import "./utils/commit-block-planner.js";
import "./utils/commit-end-time.js";
import "./utils/scheduler-refresh.js";
import "./commit-block-header.pending-user-event-counts-up-to.js";
import "./commit-block-header.select-authenticated-foreign-base-candidate.js";
import "./commit-block-header.resolve-commit-base-ledger-entries.js";
import "./commit-block-header.commit-explicit-block-header-program.js";
import "./commit-block-header.database-operations-program.js";
import "./commit-block-header.run-commit-block-header-worker-program.js";
export {
  commitExplicitBlockHeaderProgram,
  type ExplicitBlockHeaderCommitOutput,
  type ExplicitBlockHeaderCommitParams,
  shouldPreserveCommitMpfRoots,
  shouldShortCircuitIdleCommitAttempt,
  workerPreIngestionDueWorkOutputFromPlan,
} from "./commit-block-header.commit-explicit-block-header-program.js";
export {
  type CommitLucidFactory,
  defaultCommitLucidFactory,
  provideCommitBlockWorkerServices,
  shouldHydrateCommitBaseEntries,
} from "./commit-block-header.pending-user-event-counts-up-to.js";
export {
  captureCommitWorkerFailure,
  runCommitBlockHeaderWorkerProgram,
} from "./commit-block-header.run-commit-block-header-worker-program.js";
export { selectAuthenticatedForeignBaseCandidate } from "./commit-block-header.select-authenticated-foreign-base-candidate.js";
