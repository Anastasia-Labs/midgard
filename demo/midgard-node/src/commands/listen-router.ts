/**
 * Explicit HTTP route graph for the node's command server.
 * This module groups endpoint handlers and access control in one place while
 * delegating startup checks and response shaping to narrower modules.
 */

import "@al-ft/midgard-core/cek-proof";
import "@al-ft/midgard-core/codec";
import "@al-ft/midgard-core/consensus-profile";
import "@al-ft/midgard-core/consensus-validation";
import "@al-ft/midgard-core/error-format";
import "@al-ft/midgard-core/hex";
import "@al-ft/midgard-core/script-proof";
import "@al-ft/midgard-sdk";
import "@effect/platform";
import "@effect/platform/HttpServerRequest";
import "@effect/sql/SqlClient";
import "@lucid-evolution/lucid";
import "effect";
import "../database/index.js";
import "../fibers/index.js";
import "../genesis.js";
import "@al-ft/midgard-core/ogmios-slot";
import "../services/index.js";
import "../services/state-queue-topology.js";
import "../transactions/initialization.js";
import "../transactions/operators/commands.js";
import "../transactions/reference-scripts.js";
import "../transactions/state-queue/merge-readiness.js";
import "../transactions/submit-deposit.js";
import "./command-utils.js";
import "./deposit-status.js";
import "./l1-provider-preflight.js";
import "./listen-response.js";
import "./listen-utils.js";
import "./protocol-info.js";
import "./readiness.js";
import "./tx-status.js";
import "./utxos.js";
import "./listen-router.l1-provider-readiness-evidence-is-fresh.js";
import "./listen-router.run-exact-gated-direct-l1-provider-probe.js";
import "./listen-router.get-tx-handler.js";
import "./listen-router.get-tx-status-handler.js";
import "./listen-router.post-tx-status-batch-handler.js";
import "./listen-router.get-readiness-handler.js";
import "./listen-router.get-pipeline-status-handler.js";
import "./listen-router.get-state-queue-handler.js";
import "./listen-router.post-deposit-build-handler.js";
import "./listen-router.post-submit-handler.js";
import "./listen-router.build-listen-router.js";
export {
  buildListenRouter,
  buildSubmitRouter,
} from "./listen-router.build-listen-router.js";
export {
  encodePipelineStatusOldestActive,
  PIPELINE_STATUS_ACTIVE_PENDING_FINALIZATION_STATUSES,
  type PipelineStatusOldestActiveRow,
} from "./listen-router.get-pipeline-status-handler.js";
export { getMergeHandler } from "./listen-router.get-state-queue-handler.js";
export {
  readSubmitBodyWithProtocolLimit,
  resolveSubmitIngressReservation,
  SUBMIT_HTTP_BODY_MAX_BYTES,
  submitBodyReadDurationTimer,
  submitDurableAdmissionDurationTimer,
  submitHandlerLatencyTimer,
  type SubmitIngressReservation,
  submitNormalizeDurationTimer,
  submitResponseDurationTimer,
  withSubmitIngressPermit,
} from "./listen-router.get-tx-handler.js";
export {
  l1ProviderEvidenceIsFresh,
  l1ProviderReadinessEvidenceIsFresh,
  type L1ProviderReadinessProbe,
  localOgmiosSlotFromPreflight,
  runBoundedDirectL1ProviderPreflight,
} from "./listen-router.l1-provider-readiness-evidence-is-fresh.js";
export {
  resolveL1ProviderReadinessSnapshot,
  runExactGatedDirectL1ProviderProbe,
} from "./listen-router.run-exact-gated-direct-l1-provider-probe.js";
