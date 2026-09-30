/**
 * Startup-only invariant checks and bootstrap seeding for the node process.
 * This module isolates safety checks that must run before serving traffic from
 * the steady-state wiring in the main listen entrypoint.
 */

import "@al-ft/midgard-core/error-format";
import "@al-ft/midgard-sdk";
import "effect";
import "../database/index.js";
import "../mpf/index.js";
import "../services/canonical-journal-recovery.js";
import "../services/history-expired-intent-release.js";
import "../services/index.js";
import "../services/state-queue-topology.js";
import "../transactions/availability-challenge-registration.js";
import "../transactions/initialization.js";
import "../transactions/reference-scripts.js";
import "../transactions/state-queue/confirmed-ledger-snapshot.js";
import "../workers/utils/commit-block-header.js";
import "./contract-deployment-info.js";
import "./startup-policy.js";
import "./listen-startup.ensure-protocol-initialized-on-startup.js";
import "./listen-startup.seed-latest-local-block-boundary-on-startup.js";
import "./listen-startup.assert-startup-mutation-jobs-recoverable.js";
export { assertStartupMutationJobsRecoverable } from "./listen-startup.assert-startup-mutation-jobs-recoverable.js";
export {
  ensureProtocolInitializedOnStartup,
  fetchProtocolDeploymentStatusWithStartupRetry,
} from "./listen-startup.ensure-protocol-initialized-on-startup.js";
export {
  classifyUnfinishedMutationJobOnStartup,
  hydratePendingBlockFinalizationOnStartup,
  seedLatestLocalBlockBoundaryOnStartup,
} from "./listen-startup.seed-latest-local-block-boundary-on-startup.js";
