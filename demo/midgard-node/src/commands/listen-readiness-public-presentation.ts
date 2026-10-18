import { SELECTED_DEPLOYMENT_PROFILE } from "@al-ft/midgard-core/deployment-profile";

const REDACTED = "[redacted]";
const DIAGNOSTIC_REDACTED = "E_READINESS_DIAGNOSTIC_REDACTED";
const secretName =
  /seed|password|secret|credential|token|private.*key|signed.*cbor|raw.*cbor/iu;
const diagnosticName =
  /^(?:detail|last|lastError|last_error|error|dbError|providerQueryError|message)$/u;
const controlledDiagnostic =
  /^E_READINESS_(?:DATABASE_UNAVAILABLE(?::(?:connection_unavailable|capacity_unavailable|lock_unavailable|query_cancelled|schema_unavailable))?|L1_PROVIDER_UNAVAILABLE|NATIVE_OWNER_UNAVAILABLE|DIAGNOSTIC_REDACTED)$/u;

const observedEnums: ReadonlySet<string> = new Set([
  "ok",
  "starting",
  "running",
  "waiting",
  "error",
  "following",
  "intervention",
  "exhausted",
  "stopped",
  "healthy",
  "unhealthy",
  "idle",
  "scheduler_alignment",
  "mutation_worker",
  "awaiting_activation",
  "removed",
  "landed",
  "safe",
  "final",
  "busy",
  "ready",
  "active",
  "released",
  "pending_submission",
  "submitted_local_finalization_pending",
  "submitted_unconfirmed",
  "observed_waiting_stability",
  "locally_applied",
  "abandoned",
  "retired",
  "unconfigured",
  "failed",
  "seen",
  "pending",
  "unknown",
  "available",
  "unavailable",
  "current",
  "stale",
  "selected",
  "normal",
  "catch_up",
  "cached_fresh",
  "cached_stale",
  "exact_gated_direct_preflight",
  "direct_preflight_in_flight",
  "success",
  "failure",
  "exact",
  "raw",
  "extension",
  "withdrawal",
  "deposit",
  "initialize",
  "fund",
  "absorb",
  "conclude",
  "complete",
  "commit",
  "commit_worker",
  "commit_base",
  "commit_window",
  "commit_anchor",
  "retention_sweeper",
  "own_block_event_orphaned",
  "operator_watchdog_manifest",
  "native_mpf_owner",
  "native_mpf_promotion_index_cap",
  "landed_block_rebase",
  "commit_da_frame",
  "merge",
  "settlement",
  "operator_membership",
  "node_instance_lock",
  "l1_control_plane",
  "state_correction",
  "whole_instant",
  "ordinary",
  "CommitPlanningNotice",
  "skipped_local_finalization_pending",
  "skipped_unresolved_commitment",
  "skipped_reset_in_progress",
  "no_queued_block",
  "tail_eligible_final_merge",
  "skipped_pending_local_work",
  "skipped_below_min_queue_length",
]);
const observedReasons: ReadonlySet<string> = new Set([
  "commit_anchor_unavailable",
  "commit_base_pending",
  "commit_da_frame_events_overflow",
  "commit_window_pending",
  "commit_worker_failed",
  "l1_node_config_failed",
  "l1_node_config_unreadable",
  "l1_own_block_event_orphaned",
  "mpf_closure_missing",
  "native_mpf_owner_recovery_pending",
  "native_mpf_owner_stuck",
  "native_mpf_promotion_index_cap_exceeded",
  "native_mpf_restore_index_cap_exceeded",
  "native_mpf_restore_read_transient",
  "operator_watchdog_manifest_mismatch",
  "operator_watchdog_manifest_unverified",
  "retention_l1_view_stale",
  "broken_link",
  "client_protocol_violation",
  "clock_invalid",
  "clock_unavailable",
  "confirmed_ledger_base_mismatch",
  "confirmed_ledger_behind",
  "confirmed_ledger_own_block_pending",
  "cycle",
  "database_connection_failed",
  "database_unreachable",
  "db_unhealthy",
  "deployment_authority_changed",
  "deployment_identity_unavailable",
  "duplicate_key",
  "follower_authority_changed",
  "follower_recomputing",
  "forced_order_admission_stopped",
  "forced_order_carriage_pending",
  "forced_order_ingestion_failed",
  "intent_bytes_mismatch",
  "intent_commit_anchor_not_deep",
  "intent_content_ref_missing",
  "intent_gate_unjournaled",
  "intent_input_untracked",
  "intent_journal_no_view",
  "intent_journal_unavailable",
  "intent_reconcile_failed",
  "intent_reconcile_transient",
  "intent_resubmit_rejected",
  "intent_undecodable",
  "intersection_outside_history",
  "invalid_output",
  "l1_control_plane_hold_timeouts",
  "l1_control_plane_wedged",
  "l1_driver_recompute_failed",
  "l1_driver_recompute_pending",
  "l1_event_identity_conflict",
  "l1_event_undecodable",
  "l1_events_hook_failed",
  "l1_events_ingestion_failed",
  "l1_events_ingestion_waiting",
  "l1_events_orphan_recovery",
  "l1_follower_apply_stuck",
  "l1_follower_catching_up",
  "l1_follower_migration_failed",
  "l1_follower_prune_failing",
  "l1_follower_transient_exhausted",
  "l1_follower_unconfigured",
  "l1_follower_view_stale",
  "l1_follower_view_unapplied",
  "l1_follower_waiting",
  "l1_node_behind",
  "l1_node_unavailable",
  "l1_transport_unready",
  "landed_block_awaiting_da",
  "landed_block_da_refetch_pending",
  "landed_block_event_unknown",
  "landed_block_follower_schema_missing",
  "landed_block_forced_order_pending",
  "landed_block_invalid",
  "landed_block_local_fault",
  "landed_block_origin_after_frontier",
  "landed_block_own_journal_mismatch",
  "landed_block_own_revival_pending",
  "landed_block_rebase_failed",
  "landed_block_rebase_pending",
  "landed_block_replay_failed",
  "landed_block_replay_incomplete",
  "landed_blocks_waiting",
  "local_finalization_pending",
  "malformed_frame",
  "missing_admitted_parent",
  "missing_follower_permit",
  "multiple_roots",
  "native_mpf_owner_unavailable",
  "native_mpf_owner_unhealthy",
  "no_root",
  "no_selected_plan",
  "node_connection_lost",
  "node_handshake_failed",
  "node_instance_lock_failed",
  "node_instance_lock_suspended",
  "node_unreachable",
  "operator_membership_unavailable",
  "operator_not_yet_active",
  "operator_removed",
  "origin_after_protocol_init",
  "origin_mismatch",
  "origin_not_on_chain",
  "orphan_node",
  "over_cap",
  "plan_expired",
  "planning_started",
  "queue_unavailable",
  "queue_unhealthy",
  "reset_in_progress",
  "retained_da_payload_missing",
  "rollback_beyond_k",
  "services_unavailable",
  "sidecar_restarting",
  "sidecar_starting",
  "sidecar_unavailable",
  "source_timeout",
  "source_unavailable",
  "startup_preparation_failed",
  "state_queue_lease_stale",
  "state_queue_unhealthy",
  "store_integrity",
  "tracked_set_changed",
  "validation_capacity_unavailable",
  "version_unsupported",
  "wallet_seed_pending",
]);
const enumField =
  /^(?:status|state|mode|kind|phase|source|rescue|holder|type|level|commitPipelinePhase|protocolInit|providerQueryMode|providerQueryEvidenceKind|providerQueryLastObservationKind|providerQueryLastExactObservationKind)$/u;
const identityField =
  /^(?:deploymentFingerprint|manifestId|headerHash|baseHeaderHash|baseUtxosRoot|hash|root|durableRoot|eventId|event_id|submittedTxHash|txHash|ownerEpoch)$/u;
// These are complete server-authored structured reason formats, never message excerpts.
const boundedReason =
  /^(?:(?:provider_query_unhealthy|provider_query_degraded):l1-provider(?::[0-9]+)?|(?:pending_finalization_age|durable_admission_oldest_age_exceeded):[0-9]+:[0-9]+|(?:unfinished_local_mutation_jobs|da_publication_conflict|l1_own_block_forced_order_orphaned|l1_event_undecodable):[0-9]+|stale_heartbeat:(?:blockCommitment|blockConfirmation|merge|txQueueProcessor):[0-9]+|(?:queue_depth_exceeded|validation_worker_job_timeout|unresolved_block_submission|attestation_timeout_queue_unknown):[0-9]+:[0-9]+|validation_worker_pool_degraded:[0-9]+:[0-9]+:[0-9]+|state_queue_lease_stale:(?:commit|merge|initialize|state_correction|reconcile|unknown):-?[0-9]+|attestation_timeout_correction_(?:failing|stalled):[0-9a-f]{56}:[0-9]+:[0-9]+|native_mpf_full_index_near_cap:FULL_INDEX_MAX_(?:RECORDS|BYTES):[0-9]+:[0-9]+|(?:landed_frontier|intent_journal_replay|operator_activity_record)_prune_floor:[0-9]+|l1_transport_unready:(?:sidecar_starting|sidecar_restarting|sidecar_unavailable|node_unreachable|node_connection_lost|stopped|node_handshake_failed|version_unsupported|malformed_frame|client_protocol_violation)|commit_da_frame_pressure:(?:candidate|initial_candidate|required_work):[0-9]+(?:\.[0-9]+)?)$/u;

// Fixed stage vocabulary from the actual commit-build alarm producers.
const commitAlarmReason =
  /^(?:commit_build_held|commit_stage_breaker):(?:phase_a|phase_b|transaction_root_derivation|program_material_sidecar|program_material_coverage|withdrawal_classification|unclassified)$/u;
const pipelineCountPath =
  /^(?:durableAdmission\.backlog|localResidue\.(?:mempoolTxCount|processedMempoolTxCount)|localMutationJobs\.unfinished|settlement\.unfinishedJobs|pendingBlockFinalizations\.countsByStatus\.(?:pending_submission|submitted_local_finalization_pending|submitted_unconfirmed|observed_waiting_stability|locally_applied|abandoned))$/u;

/** Public presentation only: readiness decisions and typed observation fields are unchanged.
 * Free-form diagnostic fields and mutation tokens are not public authority facts.
 * In particular, planning authority epoch/view/hash identifiers remain intact.
 */
export const publicReadinessPresentation = (
  input: Readonly<Record<string, unknown>>,
  profileName: string = SELECTED_DEPLOYMENT_PROFILE.name,
  env: NodeJS.ProcessEnv = process.env,
): Readonly<Record<string, unknown>> => {
  if (profileName !== "mainnet" && profileName !== "preprod-public")
    return input;
  const secrets = Object.entries(env)
    .filter(
      ([name, value]) =>
        secretName.test(name) && value !== undefined && value.length > 0,
    )
    .flatMap(([, value]) => [value!, value!.trim()])
    .filter((value) => value.length > 0);
  const scrubSecret = (text: string): string => {
    let safe = text;
    for (const secret of secrets) safe = safe.split(secret).join(REDACTED);
    return safe;
  };
  const render = (
    value: unknown,
    key: string,
    path: string,
    depth: number,
  ): unknown => {
    if (secretName.test(key)) return REDACTED;
    if (typeof value === "string") {
      const safe = scrubSecret(value);
      if (diagnosticName.test(key)) {
        if (controlledDiagnostic.test(safe)) return safe;
        if (key === "providerQueryError" || path.startsWith("localLedgerSlot."))
          return "E_READINESS_L1_PROVIDER_UNAVAILABLE";
        if (path.startsWith("nativeMpfOwner."))
          return "E_READINESS_NATIVE_OWNER_UNAVAILABLE";
        return DIAGNOSTIC_REDACTED;
      }
      if (key === "reasons" || key === "details" || key === "reason") {
        // Keep the authoritative reason; counts remain in the separately typed fields.
        // No number, hash or state is recovered from an upstream message.
        for (const code of [
          "l1_control_plane_wedged",
          "l1_control_plane_hold_timeouts",
        ])
          if (safe.startsWith(`${code}:`)) return code;
        if (
          safe.startsWith("state_queue_lease_stale:") &&
          !boundedReason.test(safe)
        )
          return "state_queue_lease_stale";
        for (const alarm of ["commit_build_held", "commit_stage_breaker"])
          if (safe.startsWith(`${alarm}:`))
            return commitAlarmReason.test(safe)
              ? safe
              : `${alarm}:unclassified`;
        if (safe.startsWith("settlement_worker_failing:"))
          return "settlement_worker_failing";
        if (safe.startsWith("settlement_jobs_stuck:"))
          return "settlement_jobs_stuck";
        return observedReasons.has(safe) || boundedReason.test(safe)
          ? safe
          : DIAGNOSTIC_REDACTED;
      }
      if (
        (path === "finalityVocabulary.txStatusCommittedMeaning" &&
          safe === "immutable_db_inclusion_not_confirmed_ledger_merge") ||
        (path === "finalityVocabulary.confirmedDrainMeaning" &&
          safe ===
            "containing state-queue header merged to confirmed ledger and locally finalized after L1 confirmation")
      )
        return safe;
      if (pipelineCountPath.test(path) && /^[0-9]{1,40}$/u.test(safe))
        return safe;
      if (
        path === "stateQueue.unconfirmedSubmittedBlockTxHash" &&
        /^[0-9a-f]{64}$/u.test(safe)
      )
        return safe;
      if (enumField.test(key))
        return observedEnums.has(safe) ? safe : DIAGNOSTIC_REDACTED;
      if (
        identityField.test(key) &&
        /^(?:[0-9a-f]{32}|[0-9a-f]{56}|[0-9a-f]{64}|[0-9a-f]{68}|[0-9a-f]{76}|[0-9a-f]{78}|[0-9a-f]{80}|[0-9a-f]{82})$/u.test(
          safe,
        )
      )
        return safe;
      if (
        key === "epoch" &&
        path === "commitPlanningProgress.planned.authority.epoch" &&
        /^(?:0|[1-9][0-9]{0,18})$/u.test(safe) &&
        BigInt(safe) <= 9_223_372_036_854_775_807n
      )
        return safe;
      if (
        /^(?:durableAdmissionBacklog|mempoolTxCount|unfinishedLocalMutationJobs)$/u.test(
          key,
        ) &&
        /^[0-9]{1,40}$/u.test(safe)
      )
        return safe;
      if (
        /^(?:dbNow|acquiredAt|expiresAt|releasedAt|blockedUntil|createdAt|updatedAt|now|dueAt|observedConfirmedAt)$/u.test(
          key,
        ) &&
        /^[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}:[0-9]{2}\.[0-9]{3}Z$/u.test(
          safe,
        )
      )
        return safe;
      return REDACTED;
    }
    if (value === null || typeof value !== "object") return value;
    if (diagnosticName.test(key)) return DIAGNOSTIC_REDACTED;
    if (value instanceof Uint8Array)
      return identityField.test(key) &&
        [16, 28, 32, 34, 38, 39, 40, 41].includes(value.byteLength)
        ? value
        : REDACTED;
    if (depth >= 12) return REDACTED;
    if (Array.isArray(value))
      return value
        .slice(0, 256)
        .map((entry) => render(entry, key, path, depth + 1));
    return record(value, path, depth + 1);
  };
  const record = (
    value: object,
    path: string,
    depth: number,
  ): Readonly<Record<string, unknown>> =>
    Object.fromEntries(
      Object.entries(Object.getOwnPropertyDescriptors(value))
        .slice(0, 256)
        .map(([key, descriptor]) => [
          key,
          "value" in descriptor
            ? render(
                descriptor.value,
                key,
                path.length === 0 ? key : `${path}.${key}`,
                depth,
              )
            : REDACTED,
        ]),
    );
  return record(input, "", 0);
};
