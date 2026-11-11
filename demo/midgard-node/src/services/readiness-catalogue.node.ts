/**
 * Readiness catalogue rows of the serving node's own checks: workers,
 * admission, the database, the validation pool, the native MPF owner,
 * settlement, the L1 provider and control plane, and the operator duties
 * (`readiness-catalogue.ts`).
 */
import type { WorkerHeartbeats } from "../commands/readiness.js";
import {
  NATIVE_MPF_OWNER_RECOVERY_PENDING,
  NATIVE_MPF_OWNER_STUCK,
  NATIVE_MPF_PROMOTION_INDEX_CAP_EXCEEDED,
} from "../fibers/native-mpf-owner-supervisor.js";
import {
  OPERATOR_WATCHDOG_MANIFEST_MISMATCH,
  OPERATOR_WATCHDOG_MANIFEST_UNVERIFIED,
} from "../fibers/operator-watchdog.manifest-gate.js";
import { OPERATOR_WATCHDOG_TAKEOVER_FAILING } from "../fibers/operator-watchdog.takeover-retry.js";
import { RETENTION_L1_VIEW_STALE } from "../fibers/retention-sweeper.js";
import {
  MPF_CLOSURE_MISSING,
  NATIVE_MPF_RESTORE_INDEX_CAP_EXCEEDED,
  NATIVE_MPF_RESTORE_READ_TRANSIENT,
} from "./liveness-halt.js";
import { NATIVE_MPF_FULL_INDEX_NEAR_CAP } from "./mpf-native-owner/service.full-index-accounting.js";
import {
  COUNT,
  detail,
  field,
  hex,
  members,
  oneOf,
  type ReadinessRow,
  reason,
  TEXT,
} from "./readiness-catalogue.row.js";

const HEARTBEAT_WORKERS = members<keyof WorkerHeartbeats>({
  blockCommitment: null,
  blockConfirmation: null,
  merge: null,
  txQueueProcessor: null,
});

export const NODE_READINESS_ROWS: readonly ReadinessRow[] = [
  // `evaluateReadiness` (`commands/readiness.ts`).
  reason("operator_not_yet_active"),
  reason("db_unhealthy"),
  reason("stale_heartbeat", oneOf(...HEARTBEAT_WORKERS), COUNT),
  reason("queue_depth_exceeded", COUNT, COUNT),
  reason("local_finalization_pending"),
  reason("validation_worker_pool_degraded", COUNT, COUNT, COUNT),
  reason("validation_worker_job_timeout", COUNT, COUNT),
  reason("unresolved_block_submission", COUNT, COUNT),
  reason("attestation_timeout_correction_failing", hex(56), COUNT, COUNT),
  reason("attestation_timeout_correction_stalled", hex(56), COUNT, COUNT),
  reason("attestation_timeout_queue_unknown", COUNT, COUNT),
  // Why a validation worker last left the pool (`validation-pool`).
  reason("validation_worker_crashed"),
  reason("validation_worker_load_failed"),
  reason("validation_worker_start_failed"),
  // No producer names it; it leaves with the reset flag it stands for
  // (`Globals.RESET_IN_PROGRESS`).
  field("reset_in_progress"),
  // The `/readyz` handler (`commands/http/readiness*.ts`).
  reason("durable_admission_oldest_age_exceeded", COUNT, COUNT),
  reason("admission_writer_stalled", COUNT, COUNT),
  reason("unfinished_local_mutation_jobs", COUNT),
  reason("da_publication_conflict", COUNT),
  reason("provider_query_unhealthy", oneOf("l1-provider")),
  detail("provider_query_degraded", oneOf("l1-provider"), COUNT),
  detail("pending_finalization_age", COUNT, COUNT),
  detail("operator_membership_unavailable"),
  reason("native_mpf_owner_unavailable"),
  reason("native_mpf_owner_unhealthy"),
  reason(NATIVE_MPF_OWNER_RECOVERY_PENDING),
  reason(NATIVE_MPF_OWNER_STUCK),
  reason(NATIVE_MPF_PROMOTION_INDEX_CAP_EXCEEDED),
  reason(NATIVE_MPF_RESTORE_INDEX_CAP_EXCEEDED),
  reason(NATIVE_MPF_RESTORE_READ_TRANSIENT),
  reason(MPF_CLOSURE_MISSING),
  detail(
    NATIVE_MPF_FULL_INDEX_NEAR_CAP,
    oneOf("FULL_INDEX_MAX_RECORDS", "FULL_INDEX_MAX_BYTES"),
    COUNT,
    COUNT,
  ),
  // Settlement (`settlement-readiness.ts`): the last error is free text.
  reason("settlement_worker_failing", COUNT, TEXT),
  detail("settlement_jobs_stuck", COUNT, TEXT),
  detail("settlement_reconcile_stuck", TEXT),
  // The L1 control plane's wedge and hold timeouts
  // (`globals.l1-control-plane.activity.ts`) name holders and scopes.
  reason("l1_control_plane_wedged", TEXT),
  reason("l1_control_plane_hold_timeouts", TEXT),
  reason(OPERATOR_WATCHDOG_MANIFEST_MISMATCH),
  reason(OPERATOR_WATCHDOG_MANIFEST_UNVERIFIED),
  reason(OPERATOR_WATCHDOG_TAKEOVER_FAILING),
  reason(RETENTION_L1_VIEW_STALE),
];
