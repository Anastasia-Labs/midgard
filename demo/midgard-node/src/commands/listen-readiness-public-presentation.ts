import {
  isProductionDeploymentProfileName,
  SELECTED_DEPLOYMENT_PROFILE,
} from "@al-ft/midgard-core/deployment-profile";

import { publicReadinessLine } from "../services/readiness-catalogue.js";
import { TX_STATUS_COMMITTED_MEANINGS } from "./tx-status.js";

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
  "commit_scheduler",
  "commit_anchor",
  "retention_sweeper",
  "own_block_event_orphaned",
  "operator_watchdog_manifest",
  "operator_watchdog_takeover",
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
const enumField =
  /^(?:status|state|mode|kind|phase|source|rescue|holder|type|level|commitPipelinePhase|protocolInit|providerQueryMode|providerQueryEvidenceKind|providerQueryLastObservationKind|providerQueryLastExactObservationKind)$/u;
const identityField =
  /^(?:deploymentFingerprint|manifestId|headerHash|baseHeaderHash|baseUtxosRoot|hash|root|durableRoot|eventId|event_id|submittedTxHash|txHash|ownerEpoch)$/u;
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
  if (!isProductionDeploymentProfileName(profileName)) return input;
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
      // Keep the authoritative code and the bounded parameters the catalogue
      // declares; no number, hash or state is recovered from a message.
      if (key === "reasons" || key === "details" || key === "reason")
        return publicReadinessLine(safe) ?? DIAGNOSTIC_REDACTED;
      if (
        (path === "finalityVocabulary.txStatusCommittedMeaning" &&
          (TX_STATUS_COMMITTED_MEANINGS as readonly string[]).includes(safe)) ||
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
