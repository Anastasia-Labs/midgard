/**
 * Explicit HTTP route graph for the node's command server.
 * This module groups endpoint handlers and access control in one place while
 * delegating startup checks and response shaping to narrower modules.
 */
import {
  decodeMidgardCekProgramMaterialSidecarV1,
  decodeMidgardProofSubmissionV1,
  encodeMidgardCekProgramMaterialSidecarV1,
  verifyMidgardCekProgramMaterialBundleV1,
} from "@al-ft/midgard-core/cek-proof";
import {
  decodeMidgardNativeByteListPreimage,
  decodeMidgardNativeTxFullV1FromCanonicalCbor,
} from "@al-ft/midgard-core/codec";
import {
  MIDGARD_CONSENSUS_LIMITS_V1,
  MIDGARD_CONSENSUS_PROFILE_V1,
  type MidgardConsensusProfileV1,
} from "@al-ft/midgard-core/consensus-profile-v1";
import { validateMidgardConsensusV1TxCbor } from "@al-ft/midgard-core/consensus-validation-v1";
import { hexToBytes } from "@al-ft/midgard-core/hex";
import { collectMidgardV1AttachedProgramEnvelopes } from "@al-ft/midgard-core/script-proof";
import * as SDK from "@al-ft/midgard-sdk";
import {
  HttpIncomingMessage,
  HttpRouter,
  HttpServerRequest,
  HttpServerResponse,
} from "@effect/platform";
import type { HttpBodyError } from "@effect/platform/HttpBody";
import { ParsedSearchParams } from "@effect/platform/HttpServerRequest";
import { SqlClient } from "@effect/sql/SqlClient";
import { toHex } from "@lucid-evolution/lucid";
import { Cause, Duration, Effect, Exit, Metric, Option, Ref } from "effect";

import {
  parseAddressArgument,
  parseTxOutRefCborHex,
} from "@/commands/command-utils.js";
import * as DepositStatusCommand from "@/commands/deposit-status.js";
import {
  type L1ProviderPreflightReport,
  runL1ProviderPreflight,
} from "@/commands/l1-provider-preflight.js";
import {
  failWith500,
  handleStateQueueGetFailure,
} from "@/commands/listen-response.js";
import {
  authorizeAdminRoute,
  isAdminRoutePath,
  normalizeSubmitTxCanonicalCborToNative,
  validateSubmitTxCanonicalCbor,
} from "@/commands/listen-utils.js";
import * as ProtocolInfoCommand from "@/commands/protocol-info.js";
import { evaluateReadiness } from "@/commands/readiness.js";
import { resolveTxStatus, resolveTxStatusBatch } from "@/commands/tx-status.js";
import * as UtxosCommand from "@/commands/utxos.js";
import {
  AddressHistoryDB,
  BlocksDB,
  DaPayloadPublicationsDB,
  ForeignTipReconciliationsDB,
  ImmutableDB,
  MempoolDB,
  MempoolLedgerDB,
  MutationJobsDB,
  PendingBlockFinalizationsDB,
  ProcessedMempoolDB,
  StateQueueMutationLeasesDB,
  TxAdmissionsDB,
  TxRejectionsDB,
} from "@/database/index.js";
import { DatabaseError } from "@/database/utils/common.js";
import {
  admissionFailureDefinitelyDidNotInsert,
  blockCommitmentAction,
  commitAdmissionBacklogSlot,
  mergeAction,
  releaseAdmissionBacklogSlot,
  requestTxQueueProcessorWakeup,
  reserveAdmissionBacklogSlot,
} from "@/fibers/index.js";
import * as Genesis from "@/genesis.js";
import {
  localOgmiosSubmitSlotEvidence,
  readLocalOgmiosSubmitSlot,
  type SubmitSlotSnapshot,
} from "@/local-ogmios-slot.js";
import {
  AdmissionWriter,
  type AdmissionWriterShutdownError,
  BatchSql,
  Database,
  DEFAULT_L1_CONTROL_PLANE_MAX_HOLD_MS,
  type L1ProviderHealthEvidence,
  MempoolLedgerCache,
  ValidationPool,
  WriteBehind,
} from "@/services/index.js";
import {
  ContractDeploymentIdentity,
  Globals,
  Lucid,
  MidgardContracts,
  nextL1ProviderHealthEvidence,
  NodeConfig,
  withL1ControlPlaneIfAvailable,
} from "@/services/index.js";
import {
  fetchStateQueueTopologyProgram,
  formatStateQueueTopology,
} from "@/services/state-queue-topology.js";
import * as Initialization from "@/transactions/initialization.js";
import {
  fetchReferenceScriptUtxosProgram,
  referenceScriptByName,
  referenceScriptTargetsByCommand,
} from "@/transactions/reference-scripts.js";
import {
  classifyOldestQueuedBlockReadiness,
  DEFAULT_MIN_QUEUE_LENGTH_FOR_MERGING,
  mergeMaturityWindow,
  planMergePreflight,
} from "@/transactions/state-queue/merge-readiness.js";
import * as SubmitDeposit from "@/transactions/submit-deposit.js";
import { SerializedStateQueueUTxO } from "@/workers/utils/commit-block-header.js";

const TX_ENDPOINT: string = "tx";
const ADDRESS_HISTORY_ENDPOINT: string = "txs";
const MERGE_ENDPOINT: string = "merge";
const UTXO_ENDPOINT: string = "utxo";
const UTXOS_ENDPOINT: string = "utxos";
const BLOCK_ENDPOINT: string = "block";
const INIT_ENDPOINT: string = "init";
const COMMIT_ENDPOINT: string = "commit";
const SUBMIT_ENDPOINT: string = "submit";
const DEPOSIT_BUILD_ENDPOINT: string = "deposit/build";
const READINESS_L1_PROVIDER_LIVE_TIMEOUT_MS = 2_000;

export type L1ProviderReadinessProbe =
  | { readonly mode: "cached_fresh"; readonly baseRevision: number }
  | { readonly mode: "busy"; readonly baseRevision: number }
  | {
      readonly mode: "live";
      readonly healthy: true;
      readonly ogmiosSlot: SubmitSlotSnapshot;
      readonly publishedRevision: number;
    }
  | {
      readonly mode: "live";
      readonly healthy: false;
      readonly error: string;
      readonly publishedRevision: number;
    }
  | {
      readonly mode: "live_preflight_control_plane_busy";
      readonly healthy: true;
      readonly ogmiosSlot: SubmitSlotSnapshot;
      readonly publishedRevision: number;
    }
  | {
      readonly mode: "live_preflight_control_plane_busy";
      readonly healthy: false;
      readonly error: string;
      readonly publishedRevision: number;
    };

export const l1ProviderEvidenceIsFresh = ({
  lastSuccessAtMs,
  nowMs,
  maxAgeMs,
}: {
  readonly lastSuccessAtMs: number;
  readonly nowMs: number;
  readonly maxAgeMs: number;
}): boolean =>
  lastSuccessAtMs > 0 && Math.max(0, nowMs - lastSuccessAtMs) <= maxAgeMs;

export const l1ProviderReadinessEvidenceIsFresh = ({
  evidence,
  nowMs,
  maxAgeMs,
  maxExactAgeMs,
}: {
  readonly evidence: Pick<
    L1ProviderHealthEvidence,
    | "lastSuccessAtMs"
    | "lastExactSuccessAtMs"
    | "evidenceRevision"
    | "lastExactEvidenceRevision"
    | "lastObservationKind"
    | "lastExactObservationKind"
    | "lastSuccessKind"
  >;
  readonly nowMs: number;
  readonly maxAgeMs: number;
  readonly maxExactAgeMs: number;
}): boolean =>
  l1ProviderEvidenceIsFresh({
    lastSuccessAtMs: evidence.lastSuccessAtMs,
    nowMs,
    maxAgeMs,
  }) &&
  (evidence.lastObservationKind === "exact_success" ||
    evidence.lastObservationKind === "direct_success") &&
  evidence.lastExactSuccessAtMs > 0 &&
  evidence.lastExactEvidenceRevision > 0 &&
  evidence.lastExactEvidenceRevision <= evidence.evidenceRevision &&
  evidence.lastExactObservationKind === "exact_success" &&
  (evidence.lastSuccessKind !== "direct" ||
    l1ProviderEvidenceIsFresh({
      lastSuccessAtMs: evidence.lastExactSuccessAtMs,
      nowMs,
      maxAgeMs: maxExactAgeMs,
    }));

export const localOgmiosSlotFromPreflight = (
  report: L1ProviderPreflightReport,
): SubmitSlotSnapshot => {
  const localOgmiosSlot = report.sources.find(
    (source) => source.healthy && source.localLedgerSlot !== undefined,
  )?.localLedgerSlot;
  if (!report.ok || localOgmiosSlot === undefined) {
    const failures = report.sources
      .filter((source) => !source.healthy)
      .map((source) =>
        [
          `${source.source}:${source.failureKind ?? "unhealthy"}`,
          source.latencyMs === undefined
            ? undefined
            : `latency_ms=${source.latencyMs.toString()}`,
          source.bodySummary,
        ]
          .filter(
            (part): part is string => part !== undefined && part.length > 0,
          )
          .join(":"),
      )
      .join(",");
    throw new Error(
      failures.length > 0
        ? `Direct L1 provider preflight failed (${failures})`
        : "Direct L1 provider preflight returned no local Ogmios slot evidence",
    );
  }
  return localOgmiosSlot;
};

export const runBoundedDirectL1ProviderPreflight = ({
  runPreflight,
  timeoutMs,
}: {
  readonly runPreflight: (
    signal: AbortSignal,
  ) => Promise<L1ProviderPreflightReport>;
  readonly timeoutMs: number;
}): Effect.Effect<SubmitSlotSnapshot, unknown> =>
  Effect.tryPromise({
    try: runPreflight,
    catch: (cause) => cause,
  }).pipe(
    Effect.flatMap((report) =>
      Effect.try({
        try: () => localOgmiosSlotFromPreflight(report),
        catch: (cause) => cause,
      }),
    ),
    Effect.timeoutFail({
      duration: Duration.millis(timeoutMs),
      onTimeout: () =>
        new Error(
          `Direct L1 provider preflight exceeded ${timeoutMs.toString()}ms`,
        ),
    }),
  );

const readinessProbeFromLatestExactObservation = (
  evidence: L1ProviderHealthEvidence,
): L1ProviderReadinessProbe => {
  if (evidence.lastObservationKind === "exact_success") {
    return {
      mode: "cached_fresh",
      baseRevision: evidence.evidenceRevision,
    };
  }
  return {
    mode: "live_preflight_control_plane_busy",
    healthy: false,
    error:
      evidence.lastObservationKind === "exact_failure"
        ? (evidence.lastExactFailure ??
          "Exact HubOracle readiness probe failed")
        : "L1 provider evidence changed while a direct probe was in flight",
    publishedRevision: evidence.evidenceRevision,
  };
};

export const reconcileReadinessProbeWithExactEvidence = ({
  probe,
  evidence,
}: {
  readonly probe: L1ProviderReadinessProbe;
  readonly evidence: L1ProviderHealthEvidence;
}): L1ProviderReadinessProbe => {
  if (probe.mode !== "live_preflight_control_plane_busy") {
    return probe;
  }
  if (
    evidence.evidenceRevision > probe.publishedRevision &&
    (evidence.lastObservationKind === "exact_success" ||
      evidence.lastObservationKind === "exact_failure")
  ) {
    return readinessProbeFromLatestExactObservation(evidence);
  }
  return probe;
};

const recordDirectProbeFailure = ({
  globals,
  error,
  observedAtMs,
  expectedRevision,
}: {
  readonly globals: Globals;
  readonly error: string;
  readonly observedAtMs: number;
  readonly expectedRevision: number;
}): Effect.Effect<L1ProviderReadinessProbe> =>
  Ref.modify(
    globals.L1_PROVIDER_HEALTH,
    (latest): readonly [L1ProviderReadinessProbe, L1ProviderHealthEvidence] => {
      if (latest.evidenceRevision !== expectedRevision) {
        return [readinessProbeFromLatestExactObservation(latest), latest];
      }
      const updated = nextL1ProviderHealthEvidence({
        current: latest,
        healthy: false,
        error,
        observedAtMs,
        successKind: "direct",
      });
      return [
        {
          mode: "live_preflight_control_plane_busy",
          healthy: false,
          error,
          publishedRevision: updated.evidenceRevision,
        },
        updated,
      ];
    },
  );

const recordDirectProbeSuccess = ({
  globals,
  ogmiosSlot,
  observedAtMs,
  expectedRevision,
}: {
  readonly globals: Globals;
  readonly ogmiosSlot: SubmitSlotSnapshot;
  readonly observedAtMs: number;
  readonly expectedRevision: number;
}): Effect.Effect<L1ProviderReadinessProbe> =>
  Ref.modify(
    globals.L1_PROVIDER_HEALTH,
    (latest): readonly [L1ProviderReadinessProbe, L1ProviderHealthEvidence] => {
      if (latest.evidenceRevision !== expectedRevision) {
        return [readinessProbeFromLatestExactObservation(latest), latest];
      }
      const updated = nextL1ProviderHealthEvidence({
        current: latest,
        healthy: true,
        observedAtMs,
        ogmiosSlot,
        successKind: "direct",
      });
      return [
        {
          mode: "live_preflight_control_plane_busy",
          healthy: true,
          ogmiosSlot,
          publishedRevision: updated.evidenceRevision,
        },
        updated,
      ];
    },
  );

/**
 * Deduplicated raw-provider fallback used only when the shared Lucid control
 * plane is busy. It cannot bootstrap readiness: a recent exact HubOracle probe
 * is required, and direct successes never refresh that exact timestamp.
 */
export const runBusyL1ProviderReadinessProbe = <E, R>({
  globals,
  directProbe,
  now,
  maxAgeMs,
  maxExactAgeMs,
}: {
  readonly globals: Globals;
  readonly directProbe: Effect.Effect<SubmitSlotSnapshot, E, R>;
  readonly now: () => number;
  readonly maxAgeMs: number;
  readonly maxExactAgeMs: number;
}): Effect.Effect<L1ProviderReadinessProbe, never, R> =>
  Effect.gen(function* () {
    const requestedRevision = (yield* Ref.get(globals.L1_PROVIDER_HEALTH))
      .evidenceRevision;
    const attempt =
      yield* globals.L1_PROVIDER_DIRECT_PROBE.withPermitsIfAvailable(1)(
        Effect.gen(function* () {
          const observedAtMs = now();
          const current = yield* Ref.get(globals.L1_PROVIDER_HEALTH);
          if (
            l1ProviderReadinessEvidenceIsFresh({
              evidence: current,
              nowMs: observedAtMs,
              maxAgeMs,
              maxExactAgeMs,
            })
          ) {
            return {
              mode: "cached_fresh",
              baseRevision: current.evidenceRevision,
            } as const;
          }
          if (
            current.evidenceRevision > requestedRevision &&
            current.lastObservationKind === "direct_failure" &&
            current.lastFailure !== null
          ) {
            return {
              mode: "live_preflight_control_plane_busy",
              healthy: false,
              error: current.lastFailure,
              publishedRevision: current.evidenceRevision,
            } as const;
          }
          if (
            current.evidenceRevision > requestedRevision &&
            (current.lastObservationKind === "exact_success" ||
              current.lastObservationKind === "exact_failure")
          ) {
            return readinessProbeFromLatestExactObservation(current);
          }

          const exactEvidenceIsFresh =
            current.lastExactSuccessAtMs > 0 &&
            current.lastExactEvidenceRevision > 0 &&
            current.lastExactEvidenceRevision <= current.evidenceRevision &&
            current.lastExactObservationKind === "exact_success" &&
            l1ProviderEvidenceIsFresh({
              lastSuccessAtMs: current.lastExactSuccessAtMs,
              nowMs: observedAtMs,
              maxAgeMs: maxExactAgeMs,
            });
          if (!exactEvidenceIsFresh) {
            const error =
              current.lastExactSuccessAtMs <= 0
                ? "Direct L1 provider fallback requires prior exact HubOracle evidence"
                : current.lastExactObservationKind === "exact_failure"
                  ? `Direct L1 provider fallback blocked by exact HubOracle failure: ${current.lastExactFailure ?? "unknown exact failure"}`
                  : `Exact HubOracle evidence is ${Math.max(0, observedAtMs - current.lastExactSuccessAtMs).toString()}ms old (max ${maxExactAgeMs.toString()}ms)`;
            if (current.lastExactObservationKind === "exact_failure") {
              return {
                mode: "live_preflight_control_plane_busy",
                healthy: false,
                error:
                  current.lastExactFailure ??
                  "Exact HubOracle readiness probe failed",
                publishedRevision: current.evidenceRevision,
              } as const;
            }
            if (current.lastExactObservationKind === "exact_success") {
              return {
                mode: "live_preflight_control_plane_busy",
                healthy: false,
                error,
                publishedRevision: current.evidenceRevision,
              } as const;
            }
            return yield* recordDirectProbeFailure({
              globals,
              error,
              observedAtMs,
              expectedRevision: current.evidenceRevision,
            });
          }

          const startRevision = current.evidenceRevision;
          const attempt = yield* Effect.either(directProbe);
          const completedAtMs = now();
          if (attempt._tag === "Left") {
            const error = String(attempt.left);
            return yield* recordDirectProbeFailure({
              globals,
              error,
              observedAtMs: completedAtMs,
              expectedRevision: startRevision,
            });
          }

          if (
            !l1ProviderEvidenceIsFresh({
              lastSuccessAtMs: current.lastExactSuccessAtMs,
              nowMs: completedAtMs,
              maxAgeMs: maxExactAgeMs,
            })
          ) {
            return yield* recordDirectProbeFailure({
              globals,
              error:
                "Exact HubOracle evidence expired during direct L1 provider preflight",
              observedAtMs: completedAtMs,
              expectedRevision: startRevision,
            });
          }
          return yield* recordDirectProbeSuccess({
            globals,
            ogmiosSlot: attempt.right,
            observedAtMs: completedAtMs,
            expectedRevision: startRevision,
          });
        }),
      );
    if (Option.isSome(attempt)) return attempt.value;

    const current = yield* Ref.get(globals.L1_PROVIDER_HEALTH);
    return {
      mode: "busy",
      baseRevision: current.evidenceRevision,
    } as const;
  });

export const runCombinedL1ReadinessProbe = <A, E1, R1, E2, R2>(
  hubOracleProbe: Effect.Effect<A, E1, R1>,
  localOgmiosSlotProbe: Effect.Effect<SubmitSlotSnapshot, E2, R2>,
): Effect.Effect<SubmitSlotSnapshot, E1 | E2, R1 | R2> =>
  hubOracleProbe.pipe(Effect.zipRight(localOgmiosSlotProbe));

export const resolveL1ProviderReadinessEvidence = ({
  probe,
  lastSuccessAtMs,
  lastFailure,
  cachedOgmiosSlot,
  nowMs,
  maxAgeMs,
}: {
  readonly probe: L1ProviderReadinessProbe;
  readonly lastSuccessAtMs: number;
  readonly lastFailure: string | null;
  readonly cachedOgmiosSlot: SubmitSlotSnapshot | null;
  readonly nowMs: number;
  readonly maxAgeMs: number;
}) => {
  const evidenceAgeMs =
    lastSuccessAtMs <= 0 ? null : Math.max(0, nowMs - lastSuccessAtMs);
  if (probe.mode === "busy" || probe.mode === "cached_fresh") {
    const healthy = evidenceAgeMs !== null && evidenceAgeMs <= maxAgeMs;
    return {
      healthy,
      mode:
        probe.mode === "cached_fresh"
          ? ("cached_fresh" as const)
          : ("cached_control_plane_busy" as const),
      evidenceAgeMs,
      ogmiosSlot: healthy ? cachedOgmiosSlot : null,
      error: healthy
        ? null
        : (lastFailure ??
          (evidenceAgeMs === null
            ? "No successful cached L1 provider evidence"
            : `Cached L1 provider evidence is ${evidenceAgeMs.toString()}ms old (max ${maxAgeMs.toString()}ms)`)),
    };
  }
  return probe.healthy
    ? {
        healthy: true,
        mode: probe.mode,
        evidenceAgeMs: 0,
        error: null,
        ogmiosSlot: probe.ogmiosSlot,
      }
    : {
        healthy: false,
        mode: probe.mode,
        evidenceAgeMs,
        error: probe.error,
        ogmiosSlot: null,
      };
};

export const resolveL1ProviderReadinessSnapshot = ({
  probe,
  evidence,
  nowMs,
  maxAgeMs,
  maxExactAgeMs,
}: {
  readonly probe: L1ProviderReadinessProbe;
  readonly evidence: L1ProviderHealthEvidence;
  readonly nowMs: number;
  readonly maxAgeMs: number;
  readonly maxExactAgeMs: number;
}) => {
  const evidenceAgeMs =
    evidence.lastSuccessAtMs <= 0
      ? null
      : Math.max(0, nowMs - evidence.lastSuccessAtMs);
  const exactEvidenceAgeMs =
    evidence.lastExactSuccessAtMs <= 0
      ? null
      : Math.max(0, nowMs - evidence.lastExactSuccessAtMs);
  const latestSuccessIsFresh =
    evidenceAgeMs !== null && evidenceAgeMs <= maxAgeMs;
  const exactPrerequisiteIsFresh =
    evidence.lastExactObservationKind === "exact_success" &&
    evidence.lastExactEvidenceRevision > 0 &&
    evidence.lastExactEvidenceRevision <= evidence.evidenceRevision &&
    exactEvidenceAgeMs !== null &&
    exactEvidenceAgeMs <= maxExactAgeMs;

  let healthy = false;
  let error: string | null;
  switch (evidence.lastObservationKind) {
    case "exact_success":
      healthy = latestSuccessIsFresh;
      error = healthy
        ? null
        : `Exact HubOracle evidence is ${evidenceAgeMs?.toString() ?? "missing"}ms old (max ${maxAgeMs.toString()}ms)`;
      break;
    case "direct_success":
      healthy = latestSuccessIsFresh && exactPrerequisiteIsFresh;
      error = healthy
        ? null
        : !latestSuccessIsFresh
          ? `Direct L1 provider evidence is ${evidenceAgeMs?.toString() ?? "missing"}ms old (max ${maxAgeMs.toString()}ms)`
          : evidence.lastExactObservationKind !== "exact_success"
            ? (evidence.lastExactFailure ??
              "Direct L1 provider evidence has no active exact prerequisite")
            : `Exact HubOracle prerequisite is ${exactEvidenceAgeMs?.toString() ?? "missing"}ms old (max ${maxExactAgeMs.toString()}ms)`;
      break;
    case "exact_failure":
      error =
        evidence.lastExactFailure ?? "Exact HubOracle readiness probe failed";
      break;
    case "direct_failure":
      error = evidence.lastFailure ?? "Direct L1 provider preflight failed";
      break;
    case null:
      error = "No L1 provider readiness evidence has been published";
      break;
  }

  const probeRevision =
    probe.mode === "cached_fresh" || probe.mode === "busy"
      ? probe.baseRevision
      : probe.publishedRevision;
  const localMode =
    probe.mode === "busy" ? "cached_control_plane_busy" : probe.mode;
  const snapshotMode =
    evidence.lastObservationKind === null
      ? "snapshot_uninitialized"
      : evidence.lastObservationKind.startsWith("exact_")
        ? "snapshot_exact"
        : "snapshot_direct";

  return {
    healthy,
    mode:
      probeRevision === evidence.evidenceRevision ? localMode : snapshotMode,
    evidenceAgeMs,
    error,
    ogmiosSlot: healthy ? evidence.lastOgmiosSlot : null,
  };
};
const STATE_QUEUE_ENDPOINT: string = "stateQueue";
const TX_STATUS_ENDPOINT: string = "tx-status";
const PIPELINE_STATUS_ENDPOINT: string = "pipeline-status";
const DEPOSIT_STATUS_ENDPOINT: string = "deposit-status";
const PROTOCOL_INFO_ENDPOINT: string = "protocol-info";
const HEALTH_ENDPOINT: string = "healthz";
const READINESS_ENDPOINT: string = "readyz";

const errorMessage = (error: unknown): string =>
  error instanceof Error ? error.message : String(error);
const STATE_QUEUE_MUTATION_LEASE_ENDPOINT: string = "stateQueueMutationLease";

const txCounter = Metric.counter("tx_count", {
  description: "A counter for tracking submit transactions",
  bigint: true,
  incremental: true,
});

export const submitHandlerLatencyTimer = Metric.timer(
  "submit_handler_latency",
  "Latency of POST /submit handler responses in milliseconds",
);

export const submitBodyReadDurationTimer = Metric.timer(
  "submit_body_read_duration",
  "Duration of POST /submit request body reads in milliseconds",
);

export const submitNormalizeDurationTimer = Metric.timer(
  "submit_normalize_duration",
  "Duration of POST /submit canonical CBOR validation and normalization in milliseconds",
);

export const submitDurableAdmissionDurationTimer = Metric.timer(
  "submit_durable_admission_duration",
  "Duration of POST /submit durable admission writes in milliseconds",
);

export const submitResponseDurationTimer = Metric.timer(
  "submit_response_duration",
  "Duration of POST /submit response construction in milliseconds",
);

const submitQueueOfferFailureCounter = Metric.counter(
  "submit_queue_offer_failure_count",
  {
    description:
      "Number of POST /submit requests rejected because the queue was full",
    bigint: true,
    incremental: true,
  },
);

const V1_SUBMISSION_MEDIA_TYPE = "application/vnd.midgard.v1+cbor";
export const SUBMIT_HTTP_BODY_MAX_BYTES =
  MIDGARD_CONSENSUS_LIMITS_V1.maxDaPayloadBytes;

type SubmitIngressPool = {
  readonly concurrency: Effect.Semaphore;
  readonly bytes: Effect.Semaphore;
};

const submitIngressPools = new Map<string, SubmitIngressPool>();

const submitIngressPool = ({
  maxConcurrency,
  maxInFlightBytes,
}: {
  readonly maxConcurrency: number;
  readonly maxInFlightBytes: number;
}): SubmitIngressPool => {
  const key = `${maxConcurrency.toString()}:${maxInFlightBytes.toString()}`;
  const existing = submitIngressPools.get(key);
  if (existing !== undefined) return existing;
  const created = {
    concurrency: Effect.unsafeMakeSemaphore(maxConcurrency),
    bytes: Effect.unsafeMakeSemaphore(maxInFlightBytes),
  };
  submitIngressPools.set(key, created);
  return created;
};

export type SubmitIngressReservation =
  | { readonly kind: "invalid_content_length" }
  | { readonly kind: "too_large"; readonly declaredBytes: number }
  | {
      readonly kind: "ready";
      readonly declaredBytes: number | null;
      readonly permitBytes: number;
    };

/**
 * Resolves the request's resource weight without consuming its body. Requests
 * without a trustworthy length reserve the full protocol envelope, while a
 * declared bounded length reserves its exact byte weight.
 */
export const resolveSubmitIngressReservation = (
  contentLength: string | undefined,
  maxBodyBytes = SUBMIT_HTTP_BODY_MAX_BYTES,
): SubmitIngressReservation => {
  if (contentLength === undefined) {
    return {
      kind: "ready",
      declaredBytes: null,
      permitBytes: maxBodyBytes,
    };
  }
  if (!/^(?:0|[1-9][0-9]*)$/u.test(contentLength)) {
    return { kind: "invalid_content_length" };
  }
  const declaredBytes = Number(contentLength);
  if (!Number.isSafeInteger(declaredBytes)) {
    return { kind: "invalid_content_length" };
  }
  if (declaredBytes > maxBodyBytes) {
    return { kind: "too_large", declaredBytes };
  }
  return {
    kind: "ready",
    declaredBytes,
    permitBytes: Math.max(1, declaredBytes),
  };
};

export const readSubmitBodyWithProtocolLimit = (
  request: HttpServerRequest.HttpServerRequest,
) =>
  HttpIncomingMessage.withMaxBodySize(
    request.arrayBuffer,
    Option.some(SUBMIT_HTTP_BODY_MAX_BYTES),
  );

/**
 * Runs the body-read/decode/verification path only while both global
 * request-count and byte-weighted permits are held. Ingress is fail-fast when
 * either bound is exhausted so waiting sockets cannot become an unbounded
 * queue. Both permits are released by Effect on every exit.
 */
export const withSubmitIngressPermit = <A, E, R>({
  maxConcurrency,
  maxInFlightBytes,
  permitBytes,
  effect,
}: {
  readonly maxConcurrency: number;
  readonly maxInFlightBytes: number;
  readonly permitBytes: number;
  readonly effect: Effect.Effect<A, E, R>;
}): Effect.Effect<Option.Option<A>, E, R> => {
  if (
    !Number.isSafeInteger(maxConcurrency) ||
    maxConcurrency <= 0 ||
    !Number.isSafeInteger(maxInFlightBytes) ||
    maxInFlightBytes <= 0 ||
    !Number.isSafeInteger(permitBytes) ||
    permitBytes <= 0 ||
    permitBytes > maxInFlightBytes
  ) {
    return Effect.dieMessage("Invalid submit ingress permit configuration");
  }
  const pool = submitIngressPool({ maxConcurrency, maxInFlightBytes });
  return pool.concurrency
    .withPermitsIfAvailable(
      1,
    )(pool.bytes.withPermitsIfAvailable(permitBytes)(effect))
    .pipe(Effect.map(Option.flatten));
};

const requestMediaType = (contentType: string | undefined): string =>
  contentType?.split(";")[0]?.trim().toLowerCase() ?? "";

const parseFixedHexParam = (
  value: unknown,
  byteLength: number,
): Buffer | null => {
  if (typeof value !== "string") {
    return null;
  }
  try {
    return hexToBytes(value, { byteLength, trim: false });
  } catch {
    return null;
  }
};

const parsePositiveInteger = (
  value: unknown,
  label: string,
): number | undefined => {
  if (value === undefined) {
    return undefined;
  }
  if (typeof value !== "number" || !Number.isSafeInteger(value) || value <= 0) {
    throw new Error(`${label} must be a positive safe integer.`);
  }
  return value;
};

const encodeStateQueueMutationLease = (
  lease: StateQueueMutationLeasesDB.Entry | undefined,
  now: Date = new Date(),
) =>
  lease === undefined
    ? null
    : (() => {
        const expiresAt = lease[StateQueueMutationLeasesDB.Columns.EXPIRES_AT];
        const remainingMs = expiresAt.getTime() - now.getTime();
        return {
          token: lease[StateQueueMutationLeasesDB.Columns.TOKEN],
          holder: lease[StateQueueMutationLeasesDB.Columns.HOLDER],
          status: lease[StateQueueMutationLeasesDB.Columns.STATUS],
          acquiredAt:
            lease[StateQueueMutationLeasesDB.Columns.ACQUIRED_AT].toISOString(),
          expiresAt: expiresAt.toISOString(),
          releasedAt:
            lease[
              StateQueueMutationLeasesDB.Columns.RELEASED_AT
            ]?.toISOString() ?? null,
          lastError:
            lease[StateQueueMutationLeasesDB.Columns.LAST_ERROR] ?? null,
          remainingMs,
          expired: remainingMs < 0,
          blockedUntil: expiresAt.toISOString(),
        };
      })();

const encodeStateQueueMutationLeaseInspection = (
  inspection: StateQueueMutationLeasesDB.LeaseInspection,
) => ({
  status: inspection.activeLease === undefined ? "idle" : "busy",
  dbNow: inspection.dbNow.toISOString(),
  activeLease: encodeStateQueueMutationLease(
    inspection.activeLease,
    inspection.dbNow,
  ),
  pendingFinalizations: inspection.pendingFinalizations.map((entry) => ({
    headerHash: entry.headerHash,
    submittedTxHash: entry.submittedTxHash,
    status: entry.status,
    createdAt: entry.createdAt.toISOString(),
    updatedAt: entry.updatedAt.toISOString(),
  })),
  recentLeases: inspection.recentLeases.map((lease) =>
    encodeStateQueueMutationLease(lease, inspection.dbNow),
  ),
});

export type StateQueueMutationLeaseEndpointResult = {
  readonly statusCode: number;
  readonly body: unknown;
};

export type StateQueueMutationLeaseEndpointStore<R = Database> = {
  readonly inspect: (args?: {
    readonly recentLimit?: number;
  }) => Effect.Effect<StateQueueMutationLeasesDB.LeaseInspection, unknown, R>;
  readonly tryAcquire: (args: {
    readonly holder: string;
    readonly ttlMs?: number;
  }) => Effect.Effect<
    StateQueueMutationLeasesDB.LeaseAcquireResult,
    unknown,
    R
  >;
  readonly renew: (args: {
    readonly token: string;
    readonly ttlMs?: number;
  }) => Effect.Effect<void, unknown, R>;
  readonly release: (token: string) => Effect.Effect<void, unknown, R>;
  readonly markFailed: (
    token: string,
    error: string,
  ) => Effect.Effect<void, unknown, R>;
};

const defaultStateQueueMutationLeaseEndpointStore: StateQueueMutationLeaseEndpointStore =
  {
    inspect: StateQueueMutationLeasesDB.inspect,
    tryAcquire: StateQueueMutationLeasesDB.tryAcquire,
    renew: StateQueueMutationLeasesDB.renew,
    release: StateQueueMutationLeasesDB.release,
    markFailed: StateQueueMutationLeasesDB.markFailed,
  };

export const resolveStateQueueMutationLeaseRequest = <R = Database>(
  body: unknown,
  store: StateQueueMutationLeaseEndpointStore<R> = defaultStateQueueMutationLeaseEndpointStore as StateQueueMutationLeaseEndpointStore<R>,
): Effect.Effect<StateQueueMutationLeaseEndpointResult, never, R> =>
  Effect.gen(function* () {
    if (typeof body !== "object" || body === null || !("action" in body)) {
      return {
        statusCode: 400,
        body: { error: 'Request body must include an "action" field.' },
      };
    }
    const action = (body as { readonly action?: unknown }).action;
    if (action === "inspect") {
      let recentLimit: number | undefined;
      try {
        recentLimit = parsePositiveInteger(
          (body as { readonly recentLimit?: unknown }).recentLimit,
          "recentLimit",
        );
      } catch (error) {
        return {
          statusCode: 400,
          body: {
            error: errorMessage(error),
          },
        };
      }
      const inspection = yield* store.inspect(
        recentLimit === undefined ? undefined : { recentLimit },
      );
      return {
        statusCode: 200,
        body: encodeStateQueueMutationLeaseInspection(inspection),
      };
    }
    if (action === "acquire") {
      const holderValue = (body as { readonly holder?: unknown }).holder;
      const holder =
        typeof holderValue === "string" && holderValue.trim().length > 0
          ? holderValue.trim()
          : "fault_proof_removal";
      let ttlMs: number | undefined;
      try {
        ttlMs = parsePositiveInteger(
          (body as { readonly ttlMs?: unknown }).ttlMs,
          "ttlMs",
        );
      } catch (error) {
        return {
          statusCode: 400,
          body: {
            error: errorMessage(error),
          },
        };
      }
      const result = yield* store.tryAcquire({
        holder,
        ...(ttlMs === undefined ? {} : { ttlMs }),
      });
      if (result._tag === "Busy") {
        return {
          statusCode: 409,
          body: {
            status: "busy",
            activeLease: encodeStateQueueMutationLease(result.activeLease),
          },
        };
      }
      return {
        statusCode: 200,
        body: {
          status: "acquired",
          token: result.token,
        },
      };
    }

    const token = (body as { readonly token?: unknown }).token;
    if (typeof token !== "string" || token.trim().length === 0) {
      return {
        statusCode: 400,
        body: { error: '"token" must be a non-empty string.' },
      };
    }
    if (action === "renew") {
      let ttlMs: number | undefined;
      try {
        ttlMs = parsePositiveInteger(
          (body as { readonly ttlMs?: unknown }).ttlMs,
          "ttlMs",
        );
      } catch (error) {
        return {
          statusCode: 400,
          body: {
            error: errorMessage(error),
          },
        };
      }
      yield* store.renew({
        token: token.trim(),
        ...(ttlMs === undefined ? {} : { ttlMs }),
      });
      return { statusCode: 200, body: { status: "renewed" } };
    }
    if (action === "release") {
      yield* store.release(token.trim());
      return { statusCode: 200, body: { status: "released" } };
    }
    if (action === "fail") {
      const errorValue = (body as { readonly error?: unknown }).error;
      const error =
        typeof errorValue === "string"
          ? errorValue
          : "external state-queue mutation failed";
      yield* store.markFailed(token.trim(), error);
      return { statusCode: 200, body: { status: "failed" } };
    }

    return {
      statusCode: 400,
      body: { error: `Unsupported action: ${String(action)}` },
    };
  }).pipe(
    Effect.catchAll((error) =>
      Effect.succeed({
        statusCode: 500,
        body: {
          error: "State-queue mutation lease request failed.",
          detail: String(error),
        },
      }),
    ),
  );

/**
 * Wraps a route handler with admin-key authorization when the path belongs to
 * the admin-only route set.
 */
const withAdminAccess = <E, R>(
  endpoint: string,
  handler: Effect.Effect<HttpServerResponse.HttpServerResponse, E, R>,
): Effect.Effect<
  HttpServerResponse.HttpServerResponse,
  E | HttpBodyError,
  R | NodeConfig | HttpServerRequest.HttpServerRequest
> =>
  Effect.gen(function* () {
    const routePath = `/${endpoint}`;
    if (!isAdminRoutePath(routePath)) {
      return yield* handler;
    }
    const request = yield* HttpServerRequest.HttpServerRequest;
    const nodeConfig = yield* NodeConfig;
    const auth = authorizeAdminRoute(
      nodeConfig.ADMIN_API_KEY,
      request.headers["x-midgard-admin-key"],
    );
    if (!auth.authorized) {
      yield* Effect.logWarning(
        `Denied admin route ${routePath}: ${auth.error} (${auth.status})`,
      );
      return yield* HttpServerResponse.json(
        { error: auth.error },
        { status: auth.status },
      );
    }
    return yield* handler;
  });

/**
 * `GET /tx`: returns the CBOR for a known tx hash from mempool or immutable
 * storage.
 */
const getTxHandler = Effect.gen(function* () {
  const params = yield* ParsedSearchParams;
  const txHashParam = params["tx_hash"];
  const txHashBytes = parseFixedHexParam(txHashParam, 32);
  if (txHashBytes === null) {
    yield* Effect.logInfo(
      `GET /${TX_ENDPOINT} - Invalid transaction hash: ${String(txHashParam)}`,
    );
    return yield* HttpServerResponse.json(
      { error: `Invalid transaction hash: ${String(txHashParam)}` },
      { status: 404 },
    );
  }
  yield* Effect.logInfo("txHashBytes", txHashBytes);
  const foundCbor: Buffer = yield* MempoolDB.retrieveTxCborByHash(
    txHashBytes,
  ).pipe(
    Effect.catchAll((_e) =>
      Effect.gen(function* () {
        const fromImmutable =
          yield* ImmutableDB.retrieveTxCborByHash(txHashBytes);
        yield* Effect.logInfo(
          `GET /${TX_ENDPOINT} - Transaction found in ImmutableDB: ${String(txHashParam)}`,
        );
        return fromImmutable;
      }),
    ),
  );
  yield* Effect.logInfo(
    `GET /${TX_ENDPOINT} - Transaction found in mempool: ${String(txHashParam)}`,
  );
  yield* Effect.logInfo("foundCbor", SDK.bufferToHex(foundCbor));
  return yield* HttpServerResponse.json({
    tx: SDK.bufferToHex(foundCbor),
  });
}).pipe(
  Effect.catchTag("HttpBodyError", (e) => failWith500("GET", TX_ENDPOINT, e)),
  Effect.catchTag("DatabaseError", (e) =>
    failWith500(
      "GET",
      TX_ENDPOINT,
      e.cause,
      `db failure with table ${e.table}`,
    ),
  ),
);

/**
 * `GET /utxos`: returns spendable mempool-ledger UTxOs for an address.
 */
const getUtxosHandler = Effect.gen(function* () {
  const params = yield* ParsedSearchParams;
  const addr = params["address"];

  if (typeof addr !== "string") {
    yield* Effect.logInfo(
      `GET /${UTXOS_ENDPOINT} - Invalid address type: ${String(addr)}`,
    );
    return yield* HttpServerResponse.json(
      { error: `Invalid address type: ${String(addr)}` },
      { status: 400 },
    );
  }
  try {
    const address = parseAddressArgument(addr);

    const utxosWithAddress =
      yield* MempoolLedgerDB.retrieveSpendableByAddress(address);
    const response = UtxosCommand.encodeStoredUtxos(utxosWithAddress);

    yield* Effect.logInfo(`Found ${response.length} UTxOs for ${addr}`);
    return yield* HttpServerResponse.json({
      utxos: response,
    });
  } catch (_error) {
    yield* Effect.logInfo(`Invalid address: ${addr}`);
    return yield* HttpServerResponse.json(
      { error: `Invalid address: ${addr}` },
      { status: 400 },
    );
  }
}).pipe(
  Effect.catchTag("HttpBodyError", (e) =>
    failWith500("GET", UTXOS_ENDPOINT, e),
  ),
  Effect.catchTag("DatabaseError", (e) =>
    failWith500(
      "GET",
      UTXOS_ENDPOINT,
      e.cause,
      `db failure with table ${e.table}`,
    ),
  ),
);

/**
 * `GET /utxo`: returns one spendable mempool-ledger UTxO by raw TxOutRef CBOR
 * hex.
 */
const getUtxoHandler = Effect.gen(function* () {
  const params = yield* ParsedSearchParams;
  const rawTxOutRef = params["txOutRef"];

  let txOutRef: Buffer;
  try {
    txOutRef = parseTxOutRefCborHex(rawTxOutRef, "txOutRef");
  } catch (error) {
    const message = errorMessage(error);
    yield* Effect.logInfo(
      `GET /${UTXO_ENDPOINT} - invalid txOutRef: ${message}`,
    );
    return yield* HttpServerResponse.json({ error: message }, { status: 400 });
  }

  const matched = yield* UtxosCommand.utxosByTxOutRefsProgram([txOutRef]);
  if (matched.length === 0) {
    return yield* HttpServerResponse.json(
      { error: `UTxO not found for txOutRef ${txOutRef.toString("hex")}` },
      { status: 404 },
    );
  }

  return yield* HttpServerResponse.json({
    utxo: UtxosCommand.encodeStoredUtxo(matched[0]),
  });
}).pipe(
  Effect.catchTag("HttpBodyError", (e) => failWith500("GET", UTXO_ENDPOINT, e)),
  Effect.catchTag("DatabaseError", (e) =>
    failWith500(
      "GET",
      UTXO_ENDPOINT,
      e.cause,
      `db failure with table ${e.table}`,
    ),
  ),
);

/**
 * `POST /utxos?by-outrefs`: returns spendable mempool-ledger UTxOs for a
 * requested list of `txHash#outputIndex` identifiers.
 */
const postUtxosByTxOutRefsHandler = Effect.gen(function* () {
  const request = yield* HttpServerRequest.HttpServerRequest;
  const params = yield* ParsedSearchParams;

  try {
    UtxosCommand.requireByOutRefsSelector(params);
  } catch (error) {
    const message = errorMessage(error);
    yield* Effect.logInfo(
      `POST /${UTXOS_ENDPOINT} - missing selector: ${message}`,
    );
    return yield* HttpServerResponse.json({ error: message }, { status: 400 });
  }

  const parsedBody = yield* Effect.either(request.json);
  if (parsedBody._tag === "Left") {
    yield* Effect.logInfo(
      `POST /${UTXOS_ENDPOINT} - invalid JSON request body`,
    );
    return yield* HttpServerResponse.json(
      { error: "Request body must be valid JSON." },
      { status: 400 },
    );
  }

  let txOutRefs: readonly Buffer[];
  try {
    txOutRefs = UtxosCommand.parseTxOutRefsRequest(parsedBody.right);
  } catch (error) {
    const message = errorMessage(error);
    yield* Effect.logInfo(
      `POST /${UTXOS_ENDPOINT} - invalid request: ${message}`,
    );
    return yield* HttpServerResponse.json({ error: message }, { status: 400 });
  }

  const matched = yield* UtxosCommand.utxosByTxOutRefsProgram(txOutRefs);
  return yield* HttpServerResponse.json({
    utxos: UtxosCommand.encodeStoredUtxos(matched),
  });
}).pipe(
  Effect.catchTag("HttpBodyError", (e) =>
    failWith500("POST", UTXOS_ENDPOINT, e),
  ),
  Effect.catchTag("DatabaseError", (e) =>
    failWith500(
      "GET",
      UTXOS_ENDPOINT,
      e.cause,
      `db failure with table ${e.table}`,
    ),
  ),
);

/**
 * `GET /tx-status`: resolves the node's canonical status for a tx hash.
 */
const getTxStatusHandler = Effect.gen(function* () {
  const params = yield* ParsedSearchParams;
  const txHashParam = params["tx_hash"];
  const txHashBytes = parseFixedHexParam(txHashParam, 32);
  if (txHashBytes === null) {
    return yield* HttpServerResponse.json(
      { error: `Invalid transaction hash: ${String(txHashParam)}` },
      { status: 400 },
    );
  }

  const globals = yield* Globals;
  const rejected = yield* TxRejectionsDB.retrieveByTxId(txHashBytes);
  const admission = yield* TxAdmissionsDB.getByTxId(txHashBytes);
  const inImmutable = yield* ImmutableDB.retrieveTxCborsByHashes([txHashBytes]);
  const inMempool = yield* MempoolDB.retrieveTxCborsByHashes([txHashBytes]);
  const inProcessedMempool = yield* ProcessedMempoolDB.retrieveTxCborsByHashes([
    txHashBytes,
  ]);

  const resolved = resolveTxStatus({
    txIdHex: txHashParam as string,
    rejection:
      rejected.length > 0
        ? {
            rejectCode: rejected[0].reject_code,
            rejectDetail: rejected[0].reject_detail,
            createdAtIso: rejected[0].created_at.toISOString(),
          }
        : null,
    admissionStatus: admission?.status ?? null,
    inImmutable: inImmutable.length > 0,
    inMempool: inMempool.length > 0,
    inProcessedMempool: inProcessedMempool.length > 0,
    localFinalizationPending: yield* Ref.get(
      globals.LOCAL_FINALIZATION_PENDING,
    ),
  });

  if (resolved.status === "not_found") {
    return yield* HttpServerResponse.json(resolved, { status: 404 });
  }

  return yield* HttpServerResponse.json(resolved);
}).pipe(
  Effect.catchTag("HttpBodyError", (e) =>
    failWith500("GET", TX_STATUS_ENDPOINT, e),
  ),
  Effect.catchTag("DatabaseError", (e) =>
    failWith500(
      "GET",
      TX_STATUS_ENDPOINT,
      e.cause,
      `db failure with table ${e.table}`,
    ),
  ),
);

type TxStatusBatchRejectionRow = {
  readonly tx_hash: string;
  readonly reject_code: string;
  readonly reject_detail: string | null;
  readonly created_at: Date;
};

type TxStatusBatchAdmissionRow = {
  readonly tx_hash: string;
  readonly status: TxAdmissionsDB.Status;
};

type TxStatusBatchMembershipRow = {
  readonly tx_hash: string;
};

type TxStatusBatchHeaderRow = {
  readonly tx_hash: string;
  readonly header_hash: string;
  readonly status: string;
};

const postTxStatusBatchHandler = Effect.gen(function* () {
  const request = yield* HttpServerRequest.HttpServerRequest;
  const parsedBody = yield* Effect.either(request.json);
  if (parsedBody._tag === "Left") {
    return yield* HttpServerResponse.json(
      { error: "Request body must be valid JSON." },
      { status: 400 },
    );
  }
  const txHashes = (parsedBody.right as { readonly txHashes?: unknown })
    .txHashes;
  if (
    !Array.isArray(txHashes) ||
    txHashes.some((entry) => typeof entry !== "string")
  ) {
    return yield* HttpServerResponse.json(
      { error: "Request body must include txHashes: string[]." },
      { status: 400 },
    );
  }
  if (txHashes.length === 0 || txHashes.length > 1000) {
    return yield* HttpServerResponse.json(
      { error: "txHashes must contain 1 to 1000 transaction hashes." },
      { status: 400 },
    );
  }
  const normalized = txHashes.map((txHash) => txHash.toLowerCase());
  const txIdBytes = normalized.map((txHash) => parseFixedHexParam(txHash, 32));
  const invalidIndex = txIdBytes.findIndex((txId) => txId === null);
  if (invalidIndex >= 0) {
    return yield* HttpServerResponse.json(
      { error: `Invalid transaction hash: ${txHashes[invalidIndex]}` },
      { status: 400 },
    );
  }
  const txIds = txIdBytes as Buffer[];
  const sql = yield* SqlClient;
  const globals = yield* Globals;
  const [
    rejectionRows,
    admissionRows,
    immutableRows,
    mempoolRows,
    processedMempoolRows,
    headerRows,
  ] = yield* Effect.all(
    [
      sql<TxStatusBatchRejectionRow>`SELECT DISTINCT ON (tx_id)
          encode(tx_id, 'hex') AS tx_hash,
          reject_code,
          reject_detail,
          created_at
        FROM tx_rejections
        WHERE ${sql.in("tx_id", txIds)}
        ORDER BY tx_id, created_at DESC`,
      sql<TxStatusBatchAdmissionRow>`SELECT
          encode(tx_id, 'hex') AS tx_hash,
          status
        FROM tx_admissions
        WHERE ${sql.in("tx_id", txIds)}`,
      sql<TxStatusBatchMembershipRow>`SELECT
          encode(tx_id, 'hex') AS tx_hash
        FROM immutable
        WHERE ${sql.in("tx_id", txIds)}`,
      sql<TxStatusBatchMembershipRow>`SELECT
          encode(tx_id, 'hex') AS tx_hash
        FROM mempool
        WHERE ${sql.in("tx_id", txIds)}`,
      sql<TxStatusBatchMembershipRow>`SELECT
          encode(tx_id, 'hex') AS tx_hash
        FROM processed_mempool
        WHERE ${sql.in("tx_id", txIds)}`,
      sql<TxStatusBatchHeaderRow>`SELECT
          encode(member.member_id, 'hex') AS tx_hash,
          encode(member.header_hash, 'hex') AS header_hash,
          pending.status
        FROM pending_block_finalization_txs AS member
        JOIN pending_block_finalizations AS pending
          ON pending.header_hash = member.header_hash
        WHERE ${sql.in("member.member_id", txIds)}`,
    ],
    { concurrency: "unbounded" },
  );
  const rejectionsByTxId = new Map(
    rejectionRows.map((row) => [
      row.tx_hash,
      {
        rejectCode: row.reject_code,
        rejectDetail: row.reject_detail,
        createdAtIso: row.created_at.toISOString(),
      },
    ]),
  );
  const admissionStatusByTxId = new Map(
    admissionRows.map((row) => [row.tx_hash, row.status]),
  );
  const immutableTxIds = new Set(immutableRows.map((row) => row.tx_hash));
  const mempoolTxIds = new Set(mempoolRows.map((row) => row.tx_hash));
  const processedMempoolTxIds = new Set(
    processedMempoolRows.map((row) => row.tx_hash),
  );
  const headerEvidenceByTxId = new Map(
    headerRows.map((row) => [
      row.tx_hash,
      {
        headerHash: row.header_hash,
        headerStatus: row.status,
        mergeStatus: row.status === "finalized" ? "finalized" : "not_finalized",
        confirmedLedgerFinalized: row.status === "finalized",
      },
    ]),
  );
  const results = resolveTxStatusBatch({
    txIdsHex: normalized,
    rejectionsByTxId,
    admissionStatusByTxId,
    immutableTxIds,
    mempoolTxIds,
    processedMempoolTxIds,
    localFinalizationPending: yield* Ref.get(
      globals.LOCAL_FINALIZATION_PENDING,
    ),
    headerEvidenceByTxId,
  });
  return yield* HttpServerResponse.json({ results });
}).pipe(
  Effect.catchTag("HttpBodyError", (e) =>
    failWith500("POST", TX_STATUS_ENDPOINT, e),
  ),
  Effect.catchTag("SqlError", (e) =>
    failWith500(
      "POST",
      TX_STATUS_ENDPOINT,
      e.cause,
      "batched transaction status query failed",
    ),
  ),
);

/**
 * `POST /stateQueueMutationLease`: admin-only coordination endpoint for
 * external state-queue mutators such as manual fault-proof removal.
 */
const postStateQueueMutationLeaseHandler = Effect.gen(function* () {
  const request = yield* HttpServerRequest.HttpServerRequest;
  const parsedBody = yield* Effect.either(request.json);
  if (parsedBody._tag === "Left") {
    return yield* HttpServerResponse.json(
      { error: "Request body must be valid JSON." },
      { status: 400 },
    );
  }
  const result = yield* resolveStateQueueMutationLeaseRequest(parsedBody.right);
  return yield* HttpServerResponse.json(result.body, {
    status: result.statusCode,
  });
}).pipe(
  Effect.catchTag("HttpBodyError", (e) =>
    failWith500("POST", STATE_QUEUE_MUTATION_LEASE_ENDPOINT, e),
  ),
);

const getStateQueueMutationLeaseHandler = Effect.gen(function* () {
  const params = yield* ParsedSearchParams;
  const recentLimitParam = params["recent_limit"];
  let recentLimit: number | undefined;
  try {
    recentLimit =
      recentLimitParam === undefined
        ? undefined
        : parsePositiveInteger(Number(recentLimitParam), "recent_limit");
  } catch (error) {
    return yield* HttpServerResponse.json(
      { error: errorMessage(error) },
      { status: 400 },
    );
  }
  const inspection = yield* StateQueueMutationLeasesDB.inspect(
    recentLimit === undefined ? undefined : { recentLimit },
  );
  return yield* HttpServerResponse.json(
    encodeStateQueueMutationLeaseInspection(inspection),
  );
}).pipe(
  Effect.catchTag("HttpBodyError", (e) =>
    failWith500("GET", STATE_QUEUE_MUTATION_LEASE_ENDPOINT, e),
  ),
  Effect.catchTag("DatabaseError", (e) =>
    failWith500(
      "GET",
      STATE_QUEUE_MUTATION_LEASE_ENDPOINT,
      e.cause,
      `db failure with table ${e.table}`,
    ),
  ),
);

/**
 * `GET /deposit-status`: returns one serialized deposit row by event id or L1
 * tx hash.
 */
const getDepositStatusHandler = Effect.gen(function* () {
  const params = yield* ParsedSearchParams;

  let lookup: DepositStatusCommand.DepositStatusLookup;
  try {
    lookup = DepositStatusCommand.parseDepositStatusLookup(params);
  } catch (error) {
    const message = errorMessage(error);
    yield* Effect.logInfo(
      `GET /${DEPOSIT_STATUS_ENDPOINT} - invalid request: ${message}`,
    );
    return yield* HttpServerResponse.json({ error: message }, { status: 400 });
  }

  const deposit =
    yield* DepositStatusCommand.resolveDepositStatusProgram(lookup);
  return yield* HttpServerResponse.json(
    DepositStatusCommand.encodeDepositStatus(deposit),
  );
}).pipe(
  Effect.catchTag("HttpBodyError", (e) =>
    failWith500("GET", DEPOSIT_STATUS_ENDPOINT, e),
  ),
  Effect.catchTag("DepositStatusCommandError", (e) =>
    HttpServerResponse.json({ error: e.message }, { status: e.status }),
  ),
  Effect.catchTag("DatabaseError", (e) =>
    failWith500(
      "GET",
      DEPOSIT_STATUS_ENDPOINT,
      e.cause,
      `db failure with table ${e.table}`,
    ),
  ),
);

/**
 * `GET /healthz`: liveness endpoint that only confirms the server is running.
 */
const getHealthHandler = Effect.gen(function* () {
  return yield* HttpServerResponse.json({
    status: "ok",
    now: new Date().toISOString(),
  });
}).pipe(
  Effect.catchTag("HttpBodyError", (e) =>
    failWith500("GET", HEALTH_ENDPOINT, e),
  ),
);

/**
 * `GET /readyz`: readiness endpoint that checks worker heartbeats, queue depth,
 * local recovery state, and database connectivity.
 */
const getReadinessHandler = Effect.gen(function* () {
  const globals = yield* Globals;
  const nodeConfig = yield* NodeConfig;
  const sql = yield* SqlClient;
  const validationPool = yield* ValidationPool;
  const validationPoolStats = yield* validationPool.stats;

  const durableAdmissionBacklog = yield* TxAdmissionsDB.countBacklog;
  const durableAdmissionOldestAgeMs = yield* TxAdmissionsDB.oldestQueuedAgeMs;
  const unfinishedMutationJobs = yield* MutationJobsDB.countUnfinished;
  const daPublicationConflicts =
    yield* DaPayloadPublicationsDB.conflictCount(15);
  const awaitingForeignTipReconciliations =
    yield* ForeignTipReconciliationsDB.countAwaiting;
  const mempoolTxCount = yield* MempoolDB.retrieveTxCount;
  const nowMillis = Date.now();
  const stateQueueBlocksInQueue = yield* Ref.get(globals.BLOCKS_IN_QUEUE);
  const resetInProgress = yield* Ref.get(globals.RESET_IN_PROGRESS);
  const commitWorkerActive = yield* Ref.get(globals.COMMIT_WORKER_ACTIVE);
  const commitPipelinePhase = yield* Ref.get(globals.COMMIT_PIPELINE_PHASE);
  const blockCommitmentHeartbeat = yield* Ref.get(
    globals.HEARTBEAT_BLOCK_COMMITMENT,
  );
  const blockConfirmationHeartbeat = yield* Ref.get(
    globals.HEARTBEAT_BLOCK_CONFIRMATION,
  );
  const mergeHeartbeat = yield* Ref.get(globals.HEARTBEAT_MERGE);
  const depositFetchHeartbeat = yield* Ref.get(globals.HEARTBEAT_DEPOSIT_FETCH);
  const withdrawalFetchHeartbeat = yield* Ref.get(
    globals.HEARTBEAT_WITHDRAWAL_FETCH,
  );
  const txQueueProcessorHeartbeat = yield* Ref.get(
    globals.HEARTBEAT_TX_QUEUE_PROCESSOR,
  );
  const localFinalizationPending = yield* Ref.get(
    globals.LOCAL_FINALIZATION_PENDING,
  );
  const unconfirmedSubmittedBlockTxHash = yield* Ref.get(
    globals.UNCONFIRMED_SUBMITTED_BLOCK_TX_HASH,
  );
  const unconfirmedSubmittedBlockSinceMs = yield* Ref.get(
    globals.UNCONFIRMED_SUBMITTED_BLOCK_SINCE_MS,
  );
  const unresolvedBlockSubmissionAgeMs =
    unconfirmedSubmittedBlockTxHash === "" ||
    unconfirmedSubmittedBlockSinceMs <= 0
      ? 0
      : nowMillis - unconfirmedSubmittedBlockSinceMs;

  const dbProbe = yield* Effect.either(sql`SELECT 1 AS ok`);
  const dbHealthy = dbProbe._tag === "Right";
  const lucid = yield* Lucid;
  const contracts = yield* MidgardContracts;
  const providerHealthBefore = yield* Ref.get(globals.L1_PROVIDER_HEALTH);
  const cachedProviderEvidenceIsFresh = l1ProviderReadinessEvidenceIsFresh({
    evidence: providerHealthBefore,
    nowMs: nowMillis,
    maxAgeMs: nodeConfig.READINESS_L1_PROVIDER_EVIDENCE_MAX_AGE_MS,
    maxExactAgeMs: DEFAULT_L1_CONTROL_PLANE_MAX_HOLD_MS,
  });
  const liveProviderProbeTimeoutMs = Math.min(
    nodeConfig.L1_PROVIDER_PREFLIGHT_TIMEOUT_MS,
    READINESS_L1_PROVIDER_LIVE_TIMEOUT_MS,
  );
  let readinessProbe: L1ProviderReadinessProbe = {
    mode: "cached_fresh",
    baseRevision: providerHealthBefore.evidenceRevision,
  };
  if (!cachedProviderEvidenceIsFresh) {
    const primaryProbeAttempt = yield* Effect.either(
      withL1ControlPlaneIfAvailable(
        globals,
        {
          scope: "readiness_l1_provider",
          maxHoldMs: liveProviderProbeTimeoutMs,
        },
        runCombinedL1ReadinessProbe(
          Initialization.fetchHubOracleWitness(lucid.api, contracts),
          readLocalOgmiosSubmitSlot({
            ogmiosUrl: nodeConfig.L1_OGMIOS_KEY,
            timeoutMs: liveProviderProbeTimeoutMs,
          }),
        ),
      ),
    );
    if (primaryProbeAttempt._tag === "Left") {
      const observedAtMs = Date.now();
      const error = String(primaryProbeAttempt.left);
      const published = yield* Ref.modify(
        globals.L1_PROVIDER_HEALTH,
        (current) => {
          const updated = nextL1ProviderHealthEvidence({
            current,
            healthy: false,
            error,
            observedAtMs,
            successKind: "exact",
          });
          return [updated, updated] as const;
        },
      );
      readinessProbe = {
        mode: "live",
        healthy: false,
        error,
        publishedRevision: published.evidenceRevision,
      };
    } else if (Option.isSome(primaryProbeAttempt.right)) {
      const observedAtMs = Date.now();
      const ogmiosSlot = primaryProbeAttempt.right.value;
      const published = yield* Ref.modify(
        globals.L1_PROVIDER_HEALTH,
        (current) => {
          const updated = nextL1ProviderHealthEvidence({
            current,
            healthy: true,
            observedAtMs,
            ogmiosSlot,
            successKind: "exact",
          });
          return [updated, updated] as const;
        },
      );
      readinessProbe = {
        mode: "live",
        healthy: true,
        ogmiosSlot,
        publishedRevision: published.evidenceRevision,
      };
    } else {
      const directProbe = runBoundedDirectL1ProviderPreflight({
        runPreflight: (signal) =>
          runL1ProviderPreflight({
            config: {
              L1_PROVIDER: nodeConfig.L1_PROVIDER,
              L1_PROVIDER_PREFLIGHT_TIMEOUT_MS: liveProviderProbeTimeoutMs,
              L1_PROVIDER_RATE_LIMIT_COOLDOWN_MS:
                nodeConfig.L1_PROVIDER_RATE_LIMIT_COOLDOWN_MS,
              L1_OGMIOS_KEY: nodeConfig.L1_OGMIOS_KEY,
              L1_KUPO_KEY: nodeConfig.L1_KUPO_KEY,
              NETWORK: nodeConfig.NETWORK,
            },
            signal,
          }),
        timeoutMs: liveProviderProbeTimeoutMs,
      });
      readinessProbe = yield* runBusyL1ProviderReadinessProbe({
        globals,
        directProbe,
        now: Date.now,
        maxAgeMs: nodeConfig.READINESS_L1_PROVIDER_EVIDENCE_MAX_AGE_MS,
        maxExactAgeMs: DEFAULT_L1_CONTROL_PLANE_MAX_HOLD_MS,
      });
    }
  }
  const providerHealthAfter = yield* Ref.get(globals.L1_PROVIDER_HEALTH);
  const providerEvidenceObservedAtMs = Date.now();
  const providerProbe = resolveL1ProviderReadinessSnapshot({
    probe: readinessProbe,
    evidence: providerHealthAfter,
    nowMs: providerEvidenceObservedAtMs,
    maxAgeMs: nodeConfig.READINESS_L1_PROVIDER_EVIDENCE_MAX_AGE_MS,
    maxExactAgeMs: DEFAULT_L1_CONTROL_PLANE_MAX_HOLD_MS,
  });
  const leaseInspection = yield* StateQueueMutationLeasesDB.inspect({
    recentLimit: 3,
  });
  const encodedLeaseInspection =
    encodeStateQueueMutationLeaseInspection(leaseInspection);
  const activeLease = leaseInspection.activeLease;
  const activeLeaseRemainingMs =
    activeLease === undefined
      ? null
      : activeLease[StateQueueMutationLeasesDB.Columns.EXPIRES_AT].getTime() -
        leaseInspection.dbNow.getTime();

  const baseReadiness = evaluateReadiness({
    nowMillis,
    maxHeartbeatAgeMs: nodeConfig.READINESS_MAX_HEARTBEAT_AGE_MS,
    maxQueueDepth: nodeConfig.READINESS_MAX_DURABLE_ADMISSION_BACKLOG,
    queueDepth: Number(durableAdmissionBacklog),
    workerHeartbeats: {
      blockCommitment: blockCommitmentHeartbeat,
      blockConfirmation: blockConfirmationHeartbeat,
      merge: mergeHeartbeat,
      depositFetch: depositFetchHeartbeat,
      withdrawalFetch: withdrawalFetchHeartbeat,
      txQueueProcessor: txQueueProcessorHeartbeat,
    },
    localFinalizationPending,
    unresolvedBlockSubmissionAgeMs,
    maxUnresolvedBlockSubmissionAgeMs: nodeConfig.UNCONFIRMED_BLOCK_MAX_AGE_MS,
    dbHealthy,
    awaitingForeignTipReconciliations,
    validationPool: {
      configuredWorkers: validationPool.poolSize,
      liveWorkers: validationPoolStats.liveWorkers,
      restartingWorkers: validationPoolStats.restartingWorkers,
      oldestInFlightAgeMs: validationPoolStats.oldestInFlightAgeMs,
      jobTimeoutMs: nodeConfig.VALIDATION_WORKER_JOB_TIMEOUT_MS,
    },
    stateQueueMutationLease: {
      active: activeLease !== undefined,
      stale:
        activeLeaseRemainingMs !== null &&
        activeLeaseRemainingMs <
          -nodeConfig.STATE_QUEUE_MUTATION_LEASE_STALE_GRACE_MS,
      remainingMs: activeLeaseRemainingMs,
      holder: activeLease?.[StateQueueMutationLeasesDB.Columns.HOLDER] ?? null,
    },
  });
  const reasons = [...baseReadiness.reasons];
  const nativeMpfOwner = yield* Ref.get(globals.NATIVE_MPF_OWNER);
  const nativeMpfDiagnostics =
    nodeConfig.MPF_ENGINE !== "architecture_g"
      ? undefined
      : nativeMpfOwner === undefined
        ? undefined
        : yield* Effect.either(
            Effect.promise(() => nativeMpfOwner.diagnostics()),
          );
  if (nodeConfig.MPF_ENGINE === "architecture_g") {
    if (nativeMpfOwner === undefined) {
      reasons.push("native_mpf_owner_unavailable");
    } else if (nativeMpfDiagnostics?._tag === "Left") {
      reasons.push("native_mpf_owner_unhealthy");
    }
  }
  if (!providerProbe.healthy) {
    reasons.push("provider_query_unhealthy:l1-provider");
  }
  if (
    durableAdmissionOldestAgeMs >
    nodeConfig.READINESS_MAX_DURABLE_ADMISSION_AGE_MS
  ) {
    reasons.push(
      `durable_admission_oldest_age_exceeded:${durableAdmissionOldestAgeMs}:${nodeConfig.READINESS_MAX_DURABLE_ADMISSION_AGE_MS}`,
    );
  }
  if (unfinishedMutationJobs > 0n) {
    reasons.push(
      `unfinished_local_mutation_jobs:${unfinishedMutationJobs.toString()}`,
    );
  }
  if (daPublicationConflicts > 0) {
    reasons.push(
      `da_publication_conflict:${daPublicationConflicts.toString()}`,
    );
  }
  const mergeReadiness = planMergePreflight({
    force: false,
    queueLength: stateQueueBlocksInQueue,
    minQueueLength:
      nodeConfig.MIN_QUEUE_LENGTH_FOR_MERGING ??
      DEFAULT_MIN_QUEUE_LENGTH_FOR_MERGING,
    unresolvedSubmittedBlockTxHash: unconfirmedSubmittedBlockTxHash,
    localFinalizationPending,
    resetInProgress,
    durableAdmissionBacklog,
    mempoolTxCount,
    unfinishedMutationJobs,
  });
  const readiness = {
    ready: reasons.length === 0,
    reasons,
    durableAdmissionBacklog: durableAdmissionBacklog.toString(),
    durableAdmissionOldestAgeMs,
    mempoolTxCount: mempoolTxCount.toString(),
    unfinishedLocalMutationJobs: unfinishedMutationJobs.toString(),
    daPublicationConflicts,
    awaitingForeignTipReconciliations,
    unresolvedBlockSubmissionAgeMs,
    providerQueryHealthy: providerProbe.healthy,
    providerQueryMode: providerProbe.mode,
    providerQueryEvidenceAgeMs: providerProbe.evidenceAgeMs,
    providerQueryEvidenceRevision: providerHealthAfter.evidenceRevision,
    providerQueryLastObservationKind: providerHealthAfter.lastObservationKind,
    providerQueryLastExactEvidenceRevision:
      providerHealthAfter.lastExactEvidenceRevision,
    providerQueryLastExactObservationKind:
      providerHealthAfter.lastExactObservationKind,
    providerQueryEvidenceKind: providerHealthAfter.lastSuccessKind,
    providerQueryExactEvidenceAgeMs:
      providerHealthAfter.lastExactSuccessAtMs <= 0
        ? null
        : Math.max(0, Date.now() - providerHealthAfter.lastExactSuccessAtMs),
    providerQueryError: providerProbe.error,
    localOgmiosSlot:
      providerProbe.ogmiosSlot !== null
        ? {
            ...providerProbe.ogmiosSlot,
            evidence: localOgmiosSubmitSlotEvidence(providerProbe.ogmiosSlot),
            mode: providerProbe.mode,
            evidenceAgeMs: providerProbe.evidenceAgeMs,
          }
        : {
            mode: providerProbe.mode,
            evidenceAgeMs: providerProbe.evidenceAgeMs,
            error: providerProbe.error,
          },
    stateQueueMutationLease: encodedLeaseInspection,
    blockCommitmentCoordination: {
      commitWorkerActive,
      commitPipelinePhase,
    },
    nativeMpfOwner:
      nativeMpfDiagnostics === undefined
        ? null
        : nativeMpfDiagnostics._tag === "Left"
          ? { healthy: false, error: String(nativeMpfDiagnostics.left) }
          : {
              healthy: true,
              ...nativeMpfDiagnostics.right,
              ownerEpoch: Buffer.from(
                nativeMpfDiagnostics.right.ownerEpoch,
              ).toString("hex"),
            },
    mergeReadiness,
  };

  return yield* HttpServerResponse.json(readiness, {
    status: readiness.ready ? 200 : 503,
  });
});

type PipelineStatusCountRow = {
  readonly status: string;
  readonly count: bigint | number | string;
};

export const PIPELINE_STATUS_ACTIVE_PENDING_FINALIZATION_STATUSES = [
  PendingBlockFinalizationsDB.Status.PendingSubmission,
  PendingBlockFinalizationsDB.Status.SubmittedLocalFinalizationPending,
  PendingBlockFinalizationsDB.Status.SubmittedUnconfirmed,
  PendingBlockFinalizationsDB.Status.ObservedWaitingStability,
] as const satisfies readonly PendingBlockFinalizationsDB.Status[];

export type PipelineStatusOldestActiveRow = {
  readonly header_hash: string;
  readonly submitted_tx_hash: string | null;
  readonly status: (typeof PIPELINE_STATUS_ACTIVE_PENDING_FINALIZATION_STATUSES)[number];
  readonly created_at: Date;
  readonly updated_at: Date;
  readonly observed_confirmed_at_ms: bigint | number | string | null;
};

type PipelineStatusCountOnlyRow = {
  readonly count: bigint | number | string;
};

const bigintString = (value: bigint | number | string): string =>
  BigInt(value).toString();

export const encodePipelineStatusOldestActive = (
  oldestActive: PipelineStatusOldestActiveRow | undefined,
  now: Date,
) =>
  oldestActive === undefined
    ? null
    : {
        headerHash: oldestActive.header_hash,
        submittedTxHash: oldestActive.submitted_tx_hash,
        status: oldestActive.status,
        ageMs: Math.max(0, now.getTime() - oldestActive.created_at.getTime()),
        createdAt: oldestActive.created_at.toISOString(),
        updatedAt: oldestActive.updated_at.toISOString(),
        observedConfirmedAt:
          oldestActive.observed_confirmed_at_ms === null
            ? null
            : new Date(
                Number(oldestActive.observed_confirmed_at_ms),
              ).toISOString(),
      };

const getPipelineStatusHandler = Effect.gen(function* () {
  const globals = yield* Globals;
  const sql = yield* SqlClient;
  const now = new Date();
  const [
    pendingCounts,
    oldestActiveRows,
    durableAdmissionBacklog,
    mempoolTxCountRows,
    processedMempoolTxCountRows,
    unfinishedMutationJobs,
    leaseInspection,
  ] = yield* Effect.all(
    [
      sql<PipelineStatusCountRow>`SELECT
          status,
          COUNT(*)::bigint AS count
        FROM pending_block_finalizations
        GROUP BY status
        ORDER BY status`,
      sql<PipelineStatusOldestActiveRow>`SELECT
          encode(header_hash, 'hex') AS header_hash,
          encode(submitted_tx_hash, 'hex') AS submitted_tx_hash,
          status,
          created_at,
          updated_at,
          observed_confirmed_at_ms
        FROM pending_block_finalizations
        WHERE ${sql(PendingBlockFinalizationsDB.Columns.STATUS)} IN ${sql.in(
          PIPELINE_STATUS_ACTIVE_PENDING_FINALIZATION_STATUSES,
        )}
        ORDER BY created_at ASC
        LIMIT 1`,
      TxAdmissionsDB.countBacklog,
      sql<PipelineStatusCountOnlyRow>`SELECT COUNT(*)::bigint AS count FROM mempool`,
      sql<PipelineStatusCountOnlyRow>`SELECT COUNT(*)::bigint AS count FROM processed_mempool`,
      MutationJobsDB.countUnfinished,
      StateQueueMutationLeasesDB.inspect({ recentLimit: 5 }),
    ],
    { concurrency: "unbounded" },
  );
  const queueLength = yield* Ref.get(globals.BLOCKS_IN_QUEUE);
  const localFinalizationPending = yield* Ref.get(
    globals.LOCAL_FINALIZATION_PENDING,
  );
  const unconfirmedSubmittedBlockTxHash = yield* Ref.get(
    globals.UNCONFIRMED_SUBMITTED_BLOCK_TX_HASH,
  );
  const unconfirmedSubmittedBlockSinceMs = yield* Ref.get(
    globals.UNCONFIRMED_SUBMITTED_BLOCK_SINCE_MS,
  );
  const oldestActive = oldestActiveRows[0];
  return yield* HttpServerResponse.json({
    status: "ok",
    now: now.toISOString(),
    finalityVocabulary: {
      txStatusCommittedMeaning:
        "immutable_db_inclusion_not_confirmed_ledger_merge",
      confirmedDrainMeaning:
        "containing state-queue header merged to confirmed ledger and locally finalized after L1 confirmation",
    },
    durableAdmission: {
      backlog: durableAdmissionBacklog.toString(),
    },
    localResidue: {
      mempoolTxCount: bigintString(mempoolTxCountRows[0]?.count ?? 0),
      processedMempoolTxCount: bigintString(
        processedMempoolTxCountRows[0]?.count ?? 0,
      ),
    },
    pendingBlockFinalizations: {
      countsByStatus: Object.fromEntries(
        pendingCounts.map((row) => [row.status, bigintString(row.count)]),
      ),
      oldestActive: encodePipelineStatusOldestActive(oldestActive, now),
    },
    stateQueue: {
      queueLength,
      unconfirmedSubmittedBlockTxHash:
        unconfirmedSubmittedBlockTxHash === ""
          ? null
          : unconfirmedSubmittedBlockTxHash,
      unconfirmedSubmittedBlockAgeMs:
        unconfirmedSubmittedBlockTxHash === "" ||
        unconfirmedSubmittedBlockSinceMs <= 0
          ? 0
          : Math.max(0, now.getTime() - unconfirmedSubmittedBlockSinceMs),
      localFinalizationPending,
    },
    stateQueueMutationLease:
      encodeStateQueueMutationLeaseInspection(leaseInspection),
    localMutationJobs: {
      unfinished: unfinishedMutationJobs.toString(),
    },
  });
}).pipe(
  Effect.catchTag("HttpBodyError", (e) =>
    failWith500("GET", PIPELINE_STATUS_ENDPOINT, e),
  ),
  Effect.catchTag("SqlError", (e) =>
    failWith500(
      "GET",
      PIPELINE_STATUS_ENDPOINT,
      e.cause,
      "pipeline status query failed",
    ),
  ),
);

/**
 * `GET /protocol-info`: returns stable public facts needed by external
 * Midgard transaction builders.
 */
const getProtocolInfoHandler = Effect.gen(function* () {
  const nodeConfig = yield* NodeConfig;
  const lucid = yield* Lucid;
  const deploymentIdentity = yield* ContractDeploymentIdentity;
  const response = yield* Effect.try({
    try: () =>
      ProtocolInfoCommand.encodeProtocolInfo({
        nodeConfig,
        currentSlot: lucid.api.currentSlot(),
        deploymentMarker: deploymentIdentity.deploymentMarker,
        consensusProfile: deploymentIdentity.consensusProfile,
      }),
    catch: (error) => error,
  });
  return yield* HttpServerResponse.json(response);
}).pipe(Effect.catchAll((e) => failWith500("GET", PROTOCOL_INFO_ENDPOINT, e)));

/**
 * `GET /block`: returns tx hashes referenced by a committed block header.
 */
const getBlockHandler = Effect.gen(function* () {
  const params = yield* ParsedSearchParams;
  const hdrHash = params["header_hash"];
  yield* Effect.logInfo(
    `GET /block - Request received for header_hash: ${String(hdrHash)}`,
  );

  const headerHash = parseFixedHexParam(hdrHash, 28);
  if (headerHash === null) {
    yield* Effect.logInfo(
      `GET /${BLOCK_ENDPOINT} - Invalid block hash: ${String(hdrHash)}`,
    );
    return yield* HttpServerResponse.json(
      { error: `Invalid block hash: ${String(hdrHash)}` },
      { status: 400 },
    );
  }
  const hashes = yield* BlocksDB.retrieveTxHashesByHeaderHash(headerHash);
  yield* Effect.logInfo(
    `GET /${BLOCK_ENDPOINT} - Found ${hashes.length} txs for block: ${String(hdrHash)}`,
  );
  return yield* HttpServerResponse.json({
    hashes: hashes.map(SDK.bufferToHex),
  });
}).pipe(
  Effect.catchTag("HttpBodyError", (e) =>
    failWith500("GET", BLOCK_ENDPOINT, e),
  ),
  Effect.catchTag("DatabaseError", (e) =>
    failWith500(
      "GET",
      BLOCK_ENDPOINT,
      e.cause,
      `db failure with table ${e.table}`,
    ),
  ),
);

/**
 * `GET /init`: initializes protocol state when startup policy and on-chain
 * topology allow it.
 */
const getInitHandler = Effect.gen(function* () {
  yield* Effect.logInfo(`✨ Initialization request received`);
  const lucid = yield* Lucid;
  const contracts = yield* MidgardContracts;
  const topology = yield* fetchStateQueueTopologyProgram(
    lucid.api,
    contracts.stateQueue,
  );
  if (topology.initialized) {
    const details = formatStateQueueTopology(topology);
    if (!topology.healthy) {
      yield* Effect.logWarning(
        `GET /${INIT_ENDPOINT} - Refusing to initialize over invalid state_queue topology (${details}): ${topology.reason ?? "unknown"}`,
      );
      return yield* HttpServerResponse.json(
        {
          error:
            "Cannot initialize: configured state_queue policy already has invalid on-chain topology",
          details,
          reason: topology.reason ?? "unknown",
        },
        { status: 409 },
      );
    }
    yield* Effect.logInfo(
      `GET /${INIT_ENDPOINT} - Skipping initialization (already initialized): ${details}`,
    );
    return yield* HttpServerResponse.json({
      message: "State queue already initialized",
      details,
    });
  }

  const txHash = yield* Initialization.program;
  yield* Genesis.program;
  yield* Effect.logInfo(
    `GET /${INIT_ENDPOINT} - Initialization successful: ${txHash}`,
  );
  return yield* HttpServerResponse.json({
    message: `Initiation successful: ${txHash}`,
  });
}).pipe(Effect.catchAll((e) => failWith500("GET", INIT_ENDPOINT, e)));

/**
 * `GET /commit`: triggers manual block commitment.
 */
const getCommitEndpoint = Effect.gen(function* () {
  yield* Effect.logInfo(
    `GET /${COMMIT_ENDPOINT} - Manual block commitment order received`,
  );
  yield* blockCommitmentAction;
  yield* Effect.logInfo(
    `GET /${COMMIT_ENDPOINT} - Block commitment successful`,
  );
  return yield* HttpServerResponse.json({
    message: "Block commitment successful",
  });
}).pipe(
  Effect.catchTag("HttpBodyError", (e) =>
    failWith500("GET", COMMIT_ENDPOINT, e),
  ),
  Effect.catchTag("WorkerError", (e) =>
    failWith500("GET", COMMIT_ENDPOINT, e.cause, "failed worker"),
  ),
);

/**
 * `GET /merge`: triggers manual merge of the oldest queued block into
 * confirmed state.
 */
const getMergeHandler = Effect.gen(function* () {
  yield* Effect.logInfo(`GET /${MERGE_ENDPOINT} - Manual merge order received`);
  const result = yield* mergeAction(true);
  yield* Effect.logInfo(
    `GET /${MERGE_ENDPOINT} - Merge result: ${JSON.stringify(result)}`,
  );
  return yield* HttpServerResponse.json({
    message: "Merge request processed",
    result,
  });
}).pipe(
  Effect.catchTag("HttpBodyError", (e) =>
    failWith500("GET", MERGE_ENDPOINT, e),
  ),
  Effect.catchTag("DatabaseError", (e) =>
    failWith500(
      "GET",
      MERGE_ENDPOINT,
      e.cause,
      `db failure with table ${e.table}`,
    ),
  ),
  Effect.catchTag("TxSubmitError", (e) =>
    failWith500("GET", MERGE_ENDPOINT, e.cause, `${e._tag}: ${e.message}`),
  ),
  Effect.catchTag("TxConfirmError", (e) =>
    failWith500("GET", MERGE_ENDPOINT, e.cause, `${e._tag}: ${e.message}`),
  ),
  Effect.catchTag("TxSignError", (e) =>
    failWith500("GET", MERGE_ENDPOINT, e.cause, `${e._tag}: ${e.message}`),
  ),
  Effect.catchTag("CmlDeserializationError", (e) =>
    failWith500("GET", MERGE_ENDPOINT, e.cause, e.message),
  ),
  Effect.catchTag("DataCoercionError", (e) =>
    failWith500("GET", MERGE_ENDPOINT, e.cause, e.message),
  ),
  Effect.catchTag("LinkedListError", (e) =>
    failWith500("GET", MERGE_ENDPOINT, e.cause, e.message),
  ),
  Effect.catchTag("HashingError", (e) =>
    failWith500("GET", MERGE_ENDPOINT, e.cause, e.message),
  ),
  Effect.catchTag("LucidError", (e) =>
    failWith500("GET", MERGE_ENDPOINT, e.cause, e.message),
  ),
  Effect.catchTag("StateQueueError", (e) =>
    handleStateQueueGetFailure(MERGE_ENDPOINT, e),
  ),
);

/**
 * `GET /txs`: returns the address-history tx payloads for an address.
 */
const getTxsOfAddressHandler = Effect.gen(function* () {
  const params = yield* ParsedSearchParams;
  const addr = params["address"];

  if (typeof addr !== "string") {
    yield* Effect.logInfo(
      `GET /${ADDRESS_HISTORY_ENDPOINT} - Invalid address type: ${String(addr)}`,
    );
    return yield* HttpServerResponse.json(
      { error: `Invalid address type: ${String(addr)}` },
      { status: 400 },
    );
  }
  try {
    const address = parseAddressArgument(addr);
    const cbors = yield* AddressHistoryDB.retrieve(address);
    yield* Effect.logInfo(`Found ${cbors.length} CBORs with ${addr}`);
    return yield* HttpServerResponse.json({
      txs: cbors.map(SDK.bufferToHex),
    });
  } catch (_error) {
    yield* Effect.logInfo(`Invalid address: ${addr}`);
    return yield* HttpServerResponse.json(
      { error: `Invalid address: ${addr}` },
      { status: 400 },
    );
  }
}).pipe(
  Effect.catchTag("HttpBodyError", (e) => failWith500("GET", "txs", e)),
  Effect.catchTag("DatabaseError", (e) =>
    failWith500(
      "GET",
      ADDRESS_HISTORY_ENDPOINT,
      e.cause,
      `db failure with table ${e.table}`,
    ),
  ),
);

/**
 * `GET /stateQueue`: logs and returns the current ordered state-queue headers.
 */
const getStateQueueHandler = Effect.gen(function* () {
  yield* Effect.logInfo(`✍  Drawing state queue UTxOs...`);
  const globals = yield* Globals;
  const lucid = yield* Lucid;
  const contracts = yield* MidgardContracts;
  const nodeConfig = yield* NodeConfig;
  const fetchConfig: SDK.StateQueueFetchConfig = {
    stateQueuePolicyId: contracts.stateQueue.policyId,
    stateQueueAddress: contracts.stateQueue.spendingScriptAddress,
  };
  const sortedUTxOs = yield* SDK.fetchSortedStateQueueUTxOsProgram(
    lucid.api,
    fetchConfig,
  );
  const headers = sortedUTxOs.flatMap((u) =>
    u.datum.key === "Empty" ? [] : [u.datum.key.Key.key],
  );
  const [
    unconfirmedSubmittedBlockTxHash,
    localFinalizationPending,
    resetInProgress,
    durableAdmissionBacklog,
    mempoolTxCount,
    unfinishedMutationJobs,
  ] = yield* Effect.all(
    [
      Ref.get(globals.UNCONFIRMED_SUBMITTED_BLOCK_TX_HASH),
      Ref.get(globals.LOCAL_FINALIZATION_PENDING),
      Ref.get(globals.RESET_IN_PROGRESS),
      TxAdmissionsDB.countBacklog,
      MempoolDB.retrieveTxCount,
      MutationJobsDB.countUnfinished,
    ],
    { concurrency: "unbounded" },
  );
  const mergeReadiness = planMergePreflight({
    force: false,
    queueLength: headers.length,
    minQueueLength:
      nodeConfig.MIN_QUEUE_LENGTH_FOR_MERGING ??
      DEFAULT_MIN_QUEUE_LENGTH_FOR_MERGING,
    unresolvedSubmittedBlockTxHash: unconfirmedSubmittedBlockTxHash,
    localFinalizationPending,
    resetInProgress,
    durableAdmissionBacklog,
    mempoolTxCount,
    unfinishedMutationJobs,
  });
  const oldestQueuedBlock = sortedUTxOs.find((u) => u.datum.key !== "Empty");
  const oldestBlockReadiness =
    oldestQueuedBlock === undefined
      ? null
      : yield* Effect.gen(function* () {
          const blockHeader = yield* SDK.getHeaderV1FromStateQueueDatum(
            oldestQueuedBlock.datum,
          );
          const stateQueueNode =
            yield* SDK.getStateQueueNodeV1FromStateQueueDatum(
              oldestQueuedBlock.datum,
            );
          const headerHash = yield* SDK.hashBlockHeaderV1(blockHeader);
          const maturity = mergeMaturityWindow(
            lucid.api,
            Number(blockHeader.endTime),
          );
          return classifyOldestQueuedBlockReadiness({
            headerHash,
            currentDaAvailability: stateQueueNode.da_attestation,
            readyAfterUnixTime: maturity.readyAfterUnixTime,
            nowUnixTime: Date.now(),
          });
        });
  let drawn = `
---------------------------- STATE QUEUE ----------------------------`;
  yield* Effect.allSuccesses(
    sortedUTxOs.map((u) =>
      Effect.gen(function* () {
        let info = "";
        const isHead = u.datum.key === "Empty";
        const isEnd = u.datum.next === "Empty";
        const emoji = isHead ? "🚢" : isEnd ? "⚓" : "⛓ ";
        if (u.datum.key !== "Empty") {
          const icon = isEnd ? "  " : emoji;
          info = `
${icon} ╰─ header: ${u.datum.key.Key.key}`;
        }
        drawn = `${drawn}
${emoji} ${u.utxo.txHash}#${u.utxo.outputIndex}${info}`;
      }),
    ),
  );
  drawn += `
---------------------------------------------------------------------
`;
  yield* Effect.logInfo(drawn);
  return yield* HttpServerResponse.json({
    headers,
    mergeReadiness: {
      ...mergeReadiness,
      durableAdmissionBacklog: durableAdmissionBacklog.toString(),
      mempoolTxCount: mempoolTxCount.toString(),
      unfinishedMutationJobs: unfinishedMutationJobs.toString(),
      oldestBlock: oldestBlockReadiness,
    },
  });
}).pipe(
  Effect.catchTag("HttpBodyError", (e) =>
    failWith500("GET", "logStateQueue", e),
  ),
  Effect.catchTag("LinkedListError", (e) =>
    failWith500("GET", "logStateQueue", e.cause, e.message),
  ),
  Effect.catchTag("DataCoercionError", (e) =>
    failWith500("GET", "logStateQueue", e.cause, e.message),
  ),
  Effect.catchTag("HashingError", (e) =>
    failWith500("GET", "logStateQueue", e.cause, e.message),
  ),
  Effect.catchTag("DatabaseError", (e) =>
    failWith500(
      "GET",
      "logStateQueue",
      e.cause,
      `db failure with table ${e.table}`,
    ),
  ),
  Effect.catchTag("LucidError", (e) =>
    failWith500("GET", "logStateQueue", e.cause, e.message),
  ),
);

/**
 * `GET /logBlocksDB`: logs a compact summary of block-link rows.
 */
const getLogBlocksDBHandler = Effect.gen(function* () {
  yield* Effect.logInfo(`✍  Querying BlocksDB...`);
  const allBlocksData = yield* BlocksDB.retrieve;
  const keyValues: Record<string, number> = allBlocksData.reduce(
    (acc: Record<string, number>, entry) => {
      const bHex = toHex(entry.header_hash);
      if (!acc[bHex]) {
        acc[bHex] = 1;
      } else {
        acc[bHex] += 1;
      }
      return acc;
    },
    {} as Record<string, number>,
  );
  let drawn = `
------------------------------ BLOCKS DB ----------------------------`;
  for (const bHex in keyValues) {
    drawn = `${drawn}
${bHex} -──▶ ${keyValues[bHex]} tx(s)`;
  }
  drawn += `
---------------------------------------------------------------------
`;
  yield* Effect.logInfo(drawn);
  return yield* HttpServerResponse.json({
    message: `BlocksDB drawn in server logs!`,
  });
}).pipe(
  Effect.catchTag("HttpBodyError", (e) => failWith500("GET", "logBlocksDB", e)),
  Effect.catchTag("DatabaseError", (e) =>
    failWith500(
      "GET",
      "logBlocksDB",
      e.cause,
      `db failure with table ${e.table}`,
    ),
  ),
);

/**
 * `GET /logGlobals`: logs the current process-global coordination state.
 */
const getLogGlobalsHandler = Effect.gen(function* () {
  yield* Effect.logInfo(`✍  Logging global variables...`);
  const globals = yield* Globals;
  const BLOCKS_IN_QUEUE: number = yield* Ref.get(globals.BLOCKS_IN_QUEUE);
  const LATEST_SYNC_TIME_OF_STATE_QUEUE_LENGTH: number = yield* Ref.get(
    globals.LATEST_SYNC_TIME_OF_STATE_QUEUE_LENGTH,
  );
  const RESET_IN_PROGRESS: boolean = yield* Ref.get(globals.RESET_IN_PROGRESS);
  const COMMIT_WORKER_ACTIVE: boolean = yield* Ref.get(
    globals.COMMIT_WORKER_ACTIVE,
  );
  const COMMIT_PIPELINE_PHASE: string = yield* Ref.get(
    globals.COMMIT_PIPELINE_PHASE,
  );
  const AVAILABLE_CONFIRMED_BLOCK: "" | SerializedStateQueueUTxO =
    yield* Ref.get(globals.AVAILABLE_CONFIRMED_BLOCK);
  const PROCESSED_UNSUBMITTED_TXS_COUNT: number = yield* Ref.get(
    globals.PROCESSED_UNSUBMITTED_TXS_COUNT,
  );
  const PROCESSED_UNSUBMITTED_TXS_SIZE: number = yield* Ref.get(
    globals.PROCESSED_UNSUBMITTED_TXS_SIZE,
  );
  const UNCONFIRMED_SUBMITTED_BLOCK_TX_HASH: string = yield* Ref.get(
    globals.UNCONFIRMED_SUBMITTED_BLOCK_TX_HASH,
  );
  const UNCONFIRMED_SUBMITTED_BLOCK_SINCE_MS: number = yield* Ref.get(
    globals.UNCONFIRMED_SUBMITTED_BLOCK_SINCE_MS,
  );
  const unconfirmedSubmittedBlockAgeMs =
    UNCONFIRMED_SUBMITTED_BLOCK_TX_HASH === "" ||
    UNCONFIRMED_SUBMITTED_BLOCK_SINCE_MS <= 0
      ? 0
      : Date.now() - UNCONFIRMED_SUBMITTED_BLOCK_SINCE_MS;
  const LOCAL_FINALIZATION_PENDING: boolean = yield* Ref.get(
    globals.LOCAL_FINALIZATION_PENDING,
  );
  const HEARTBEAT_BLOCK_COMMITMENT: number = yield* Ref.get(
    globals.HEARTBEAT_BLOCK_COMMITMENT,
  );
  const HEARTBEAT_BLOCK_CONFIRMATION: number = yield* Ref.get(
    globals.HEARTBEAT_BLOCK_CONFIRMATION,
  );
  const HEARTBEAT_MERGE: number = yield* Ref.get(globals.HEARTBEAT_MERGE);
  const HEARTBEAT_DEPOSIT_FETCH: number = yield* Ref.get(
    globals.HEARTBEAT_DEPOSIT_FETCH,
  );
  const HEARTBEAT_WITHDRAWAL_FETCH: number = yield* Ref.get(
    globals.HEARTBEAT_WITHDRAWAL_FETCH,
  );
  const HEARTBEAT_TX_QUEUE_PROCESSOR: number = yield* Ref.get(
    globals.HEARTBEAT_TX_QUEUE_PROCESSOR,
  );

  yield* Effect.logInfo(`
  BLOCKS_IN_QUEUE ⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅ ${BLOCKS_IN_QUEUE}
  LATEST_SYNC ⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅ ${new Date(Number(LATEST_SYNC_TIME_OF_STATE_QUEUE_LENGTH)).toLocaleString()}
  RESET_IN_PROGRESS ⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅ ${RESET_IN_PROGRESS}
  COMMIT_WORKER_ACTIVE ⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅ ${COMMIT_WORKER_ACTIVE}
  COMMIT_PIPELINE_PHASE ⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅ ${COMMIT_PIPELINE_PHASE}
  AVAILABLE_CONFIRMED_BLOCK ⋅⋅⋅⋅⋅⋅⋅⋅⋅ ${JSON.stringify(AVAILABLE_CONFIRMED_BLOCK)}
  PROCESSED_UNSUBMITTED_TXS_COUNT ⋅⋅⋅ ${PROCESSED_UNSUBMITTED_TXS_COUNT}
  PROCESSED_UNSUBMITTED_TXS_SIZE ⋅⋅⋅⋅ ${PROCESSED_UNSUBMITTED_TXS_SIZE}
  UNCONFIRMED_SUBMITTED_BLOCK_TX_HASH ⋅⋅⋅⋅⋅⋅⋅ ${UNCONFIRMED_SUBMITTED_BLOCK_TX_HASH}
  UNCONFIRMED_SUBMITTED_BLOCK_SINCE ⋅⋅⋅⋅⋅⋅⋅ ${UNCONFIRMED_SUBMITTED_BLOCK_SINCE_MS > 0 ? new Date(Number(UNCONFIRMED_SUBMITTED_BLOCK_SINCE_MS)).toLocaleString() : "N/A"} (${unconfirmedSubmittedBlockAgeMs}ms)
  LOCAL_FINALIZATION_PENDING ⋅⋅⋅⋅⋅⋅⋅⋅ ${LOCAL_FINALIZATION_PENDING}
  HEARTBEAT_BLOCK_COMMITMENT ⋅⋅ ${new Date(Number(HEARTBEAT_BLOCK_COMMITMENT)).toLocaleString()}
  HEARTBEAT_BLOCK_CONFIRMATION ⋅ ${new Date(Number(HEARTBEAT_BLOCK_CONFIRMATION)).toLocaleString()}
  HEARTBEAT_MERGE ⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅ ${new Date(Number(HEARTBEAT_MERGE)).toLocaleString()}
  HEARTBEAT_DEPOSIT_FETCH ⋅⋅⋅⋅ ${new Date(Number(HEARTBEAT_DEPOSIT_FETCH)).toLocaleString()}
  HEARTBEAT_WITHDRAWAL_FETCH ⋅ ${new Date(Number(HEARTBEAT_WITHDRAWAL_FETCH)).toLocaleString()}
  HEARTBEAT_TX_QUEUE_PROCESSOR ⋅ ${new Date(Number(HEARTBEAT_TX_QUEUE_PROCESSOR)).toLocaleString()}
`);
  return yield* HttpServerResponse.json({
    message: `Global variables logged!`,
  });
}).pipe(
  Effect.catchTag("HttpBodyError", (e) => failWith500("GET", "logGlobals", e)),
);

/**
 * `POST /deposit/build`: builds an unsigned L1 deposit transaction from a
 * caller-supplied wallet view and returns the CBOR for external signing.
 */
const postDepositBuildHandler = Effect.gen(function* () {
  const request = yield* HttpServerRequest.HttpServerRequest;
  const parsedBody = yield* Effect.either(request.json);
  if (parsedBody._tag === "Left") {
    yield* Effect.logInfo(
      `POST /${DEPOSIT_BUILD_ENDPOINT} - invalid JSON request body`,
    );
    return yield* HttpServerResponse.json(
      { error: "Request body must be valid JSON." },
      { status: 400 },
    );
  }

  const lucid = yield* Lucid;
  let buildRequest: SubmitDeposit.BuildDepositRequest;
  try {
    buildRequest = SubmitDeposit.parseBuildDepositRequest(parsedBody.right, {
      expectedNetwork: lucid.api.config().network,
    });
  } catch (error) {
    const message = errorMessage(error);
    yield* Effect.logInfo(
      `POST /${DEPOSIT_BUILD_ENDPOINT} - invalid request: ${message}`,
    );
    return yield* HttpServerResponse.json({ error: message }, { status: 400 });
  }

  const contracts = yield* MidgardContracts;
  const depositReferenceScripts = yield* fetchReferenceScriptUtxosProgram(
    lucid.api,
    lucid.referenceScriptsAddress,
    referenceScriptTargetsByCommand(contracts).deposit,
    contracts.referenceScriptAuth,
  ).pipe(
    Effect.map((resolved) => ({
      depositMinting: referenceScriptByName(resolved, "deposit minting"),
    })),
  );
  const built =
    yield* SubmitDeposit.buildUnsignedDepositTxFromFundingContextProgram(
      lucid.api,
      contracts,
      { ...buildRequest, referenceScripts: depositReferenceScripts },
    );
  return yield* HttpServerResponse.json(built);
}).pipe(
  Effect.catchTag("HttpBodyError", (e) =>
    failWith500("POST", DEPOSIT_BUILD_ENDPOINT, e),
  ),
  Effect.catchTag("SubmitDepositError", (e) =>
    failWith500("POST", DEPOSIT_BUILD_ENDPOINT, e.cause, e.message),
  ),
  Effect.catchTag("StateQueueError", (e) =>
    failWith500("POST", DEPOSIT_BUILD_ENDPOINT, e.cause, e.message),
  ),
  Effect.catchTag("HubOracleError", (e) =>
    failWith500("POST", DEPOSIT_BUILD_ENDPOINT, e.cause, e.message),
  ),
  Effect.catchTag("LucidError", (e) =>
    failWith500("POST", DEPOSIT_BUILD_ENDPOINT, e.cause, e.message),
  ),
  Effect.catchTag("Bech32DeserializationError", (e) =>
    failWith500("POST", DEPOSIT_BUILD_ENDPOINT, e.cause, e.message),
  ),
  Effect.catchTag("HashingError", (e) =>
    failWith500("POST", DEPOSIT_BUILD_ENDPOINT, e.cause, e.message),
  ),
);

/**
 * `POST /submit`: validates, normalizes, and enqueues a submitted L2
 * transaction.
 */
const postSubmitHandler = <R>(
  withMonitoring: boolean | undefined,
  wakeTxQueueProcessor: Effect.Effect<void, never, R>,
) =>
  Effect.gen(function* () {
    const startedAt = withMonitoring === true ? Date.now() : 0;
    const recordLatency = () =>
      withMonitoring === true
        ? submitHandlerLatencyTimer(
            Effect.succeed(Duration.millis(Date.now() - startedAt)),
          )
        : Effect.void;
    return yield* Effect.gen(function* () {
      const nodeConfig = yield* NodeConfig;
      const request = yield* HttpServerRequest.HttpServerRequest;

      if (
        requestMediaType(request.headers["content-type"]) !==
        V1_SUBMISSION_MEDIA_TYPE
      ) {
        yield* Effect.logInfo(
          `▫️ Invalid submit payload: expected ${V1_SUBMISSION_MEDIA_TYPE}`,
        );
        yield* recordLatency();
        return yield* HttpServerResponse.json(
          {
            error: `V1 requests must be a canonical proof submission envelope with Content-Type ${V1_SUBMISSION_MEDIA_TYPE}`,
          },
          { status: 415 },
        );
      }

      const ingressReservation = resolveSubmitIngressReservation(
        request.headers["content-length"],
      );
      if (ingressReservation.kind === "invalid_content_length") {
        yield* recordLatency();
        return yield* HttpServerResponse.json(
          { error: "Invalid Content-Length for V1 submission" },
          { status: 400 },
        );
      }
      if (ingressReservation.kind === "too_large") {
        yield* recordLatency();
        return yield* HttpServerResponse.json(
          {
            error: `V1 submission exceeds the DA proof envelope (${ingressReservation.declaredBytes.toString()} > ${SUBMIT_HTTP_BODY_MAX_BYTES.toString()})`,
          },
          { status: 413 },
        );
      }

      const permittedResponse = yield* withSubmitIngressPermit({
        maxConcurrency: nodeConfig.SUBMIT_INGRESS_MAX_CONCURRENCY,
        maxInFlightBytes: nodeConfig.SUBMIT_INGRESS_MAX_IN_FLIGHT_BYTES,
        permitBytes: ingressReservation.permitBytes,
        effect: Effect.gen(function* () {
          const bodyReadStartedAt = Date.now();
          const bodyBytes = yield* Effect.either(
            readSubmitBodyWithProtocolLimit(request),
          );
          yield* submitBodyReadDurationTimer(
            Effect.succeed(Duration.millis(Date.now() - bodyReadStartedAt)),
          );
          if (bodyBytes._tag === "Left") {
            yield* Effect.logInfo(
              `▫️ Submit rejected: request body exceeded or failed the bounded HTTP read`,
            );
            yield* recordLatency();
            return HttpServerResponse.json(
              {
                error: `V1 submission exceeds or could not be read within the DA proof envelope (${SUBMIT_HTTP_BODY_MAX_BYTES.toString()} bytes)`,
              },
              { status: 413 },
            );
          }

          const requestBody = Buffer.from(bodyBytes.right);
          if (
            ingressReservation.declaredBytes !== null &&
            requestBody.length !== ingressReservation.declaredBytes
          ) {
            yield* recordLatency();
            return HttpServerResponse.json(
              { error: "V1 submission Content-Length mismatch" },
              { status: 400 },
            );
          }
          if (requestBody.length > SUBMIT_HTTP_BODY_MAX_BYTES) {
            yield* recordLatency();
            return HttpServerResponse.json(
              {
                error: `V1 submission exceeds the DA proof envelope (${requestBody.length.toString()} > ${SUBMIT_HTTP_BODY_MAX_BYTES.toString()})`,
              },
              { status: 413 },
            );
          }
          const decodedSubmission = yield* Effect.either(
            Effect.try({
              try: () => decodeMidgardProofSubmissionV1(requestBody),
              catch: (cause) => cause,
            }),
          );
          if (decodedSubmission._tag === "Left") {
            yield* Effect.logInfo(
              `▫️ Submit rejected: malformed V1 submission envelope`,
            );
            yield* recordLatency();
            return HttpServerResponse.json(
              { error: "Invalid canonical V1 submission envelope" },
              { status: 400 },
            );
          }
          const transactionBytes = decodedSubmission.right.transactionCbor;
          const programMaterial = decodedSubmission.right.programMaterial;
          const programMaterialSidecarCbor =
            encodeMidgardCekProgramMaterialSidecarV1(programMaterial);
          // Re-decode the independently stored representation at the admission
          // boundary; database replay must never depend on the HTTP wrapper.
          decodeMidgardCekProgramMaterialSidecarV1(programMaterialSidecarCbor);

          const normalizeStartedAt = Date.now();
          const validation = validateSubmitTxCanonicalCbor(
            transactionBytes,
            Math.min(
              nodeConfig.MAX_SUBMIT_TX_CBOR_BYTES,
              MIDGARD_CONSENSUS_LIMITS_V1.maxTxCanonicalCborBytes,
            ),
          );
          if (!validation.ok) {
            yield* submitNormalizeDurationTimer(
              Effect.succeed(Duration.millis(Date.now() - normalizeStartedAt)),
            );
            yield* Effect.logInfo(`▫️ Submit rejected: ${validation.error}`);
            yield* recordLatency();
            return HttpServerResponse.json(
              { error: validation.error },
              { status: validation.status },
            );
          }

          const normalized = normalizeSubmitTxCanonicalCborToNative(
            validation.txCanonicalCbor,
          );
          if (!normalized.ok) {
            yield* submitNormalizeDurationTimer(
              Effect.succeed(Duration.millis(Date.now() - normalizeStartedAt)),
            );
            yield* Effect.logInfo(`▫️ ${normalized.error}`);
            yield* Effect.logInfo(`▫️ ${normalized.detail}`);
            yield* recordLatency();
            return HttpServerResponse.json(
              { error: normalized.error },
              { status: 400 },
            );
          }
          const proofViolation = validateMidgardConsensusV1TxCbor(
            normalized.txCanonicalCbor,
          );
          if (proofViolation !== null) {
            yield* submitNormalizeDurationTimer(
              Effect.succeed(Duration.millis(Date.now() - normalizeStartedAt)),
            );
            yield* recordLatency();
            return HttpServerResponse.json(
              {
                error: proofViolation.code,
                feature: proofViolation.featureId,
                detail: proofViolation.detail,
              },
              { status: 400 },
            );
          }
          const materialValidation = yield* Effect.either(
            Effect.try({
              try: () => {
                const tx = decodeMidgardNativeTxFullV1FromCanonicalCbor(
                  normalized.txCanonicalCbor,
                );
                const envelopes = collectMidgardV1AttachedProgramEnvelopes(tx);
                const hasUnresolvedReferenceInputs =
                  decodeMidgardNativeByteListPreimage(
                    tx.body.referenceInputsPreimageCbor,
                    "reference_inputs_preimage",
                  ).length > 0;
                // Reference-input program envelopes are ledger-state dependent and
                // become authoritative in Phase B. The one bundle traversal still
                // proves every attached envelope and preserves envelope position.
                verifyMidgardCekProgramMaterialBundleV1(
                  envelopes,
                  programMaterial,
                  hasUnresolvedReferenceInputs
                    ? { allowUnreachable: true }
                    : undefined,
                );
              },
              catch: (cause) => cause,
            }),
          );
          if (materialValidation._tag === "Left") {
            yield* submitNormalizeDurationTimer(
              Effect.succeed(Duration.millis(Date.now() - normalizeStartedAt)),
            );
            yield* recordLatency();
            return HttpServerResponse.json(
              {
                error: "E_CEK_PROGRAM_MATERIAL",
                detail:
                  "V1 program material does not cover every attached program envelope",
              },
              { status: 400 },
            );
          }
          yield* submitNormalizeDurationTimer(
            Effect.succeed(Duration.millis(Date.now() - normalizeStartedAt)),
          );

          // Return the durable work as a suspended effect so both ingress
          // permits are released immediately after verification. PostgreSQL's
          // direct/microbatch admission quotas remain the durable queue bound.
          return Effect.gen(function* () {
            const durableAdmissionStartedAt = Date.now();
            const reservation = yield* reserveAdmissionBacklogSlot(
              nodeConfig.MAX_DURABLE_ADMISSION_BACKLOG,
            );
            const admissionWriter = yield* AdmissionWriter;
            const admissionEffect: Effect.Effect<
              TxAdmissionsDB.AdmitResult,
              | DatabaseError
              | TxAdmissionsDB.TxAdmissionConflictError
              | TxAdmissionsDB.TxAdmissionBacklogFullError
              | AdmissionWriterShutdownError,
              SqlClient
            > = reservation.reserved
              ? admissionWriter.admitReserved({
                  txId: normalized.txId,
                  txCanonicalCbor: normalized.txCanonicalCbor,
                  programMaterialSidecarCbor,
                  submitSource: normalized.source,
                  maxBacklogBytes:
                    nodeConfig.MAX_DURABLE_ADMISSION_BACKLOG_BYTES,
                })
              : TxAdmissionsDB.admit({
                  txId: normalized.txId,
                  txCanonicalCbor: normalized.txCanonicalCbor,
                  programMaterialSidecarCbor,
                  submitSource: normalized.source,
                  currentBacklog: reservation.currentBacklog,
                  maxBacklog: nodeConfig.MAX_DURABLE_ADMISSION_BACKLOG,
                  maxBacklogBytes:
                    nodeConfig.MAX_DURABLE_ADMISSION_BACKLOG_BYTES,
                });
            const admitted = yield* admissionEffect.pipe(
              Effect.onExit((exit) => {
                if (!reservation.reserved) return Effect.void;
                if (Exit.isSuccess(exit)) {
                  return exit.value.kind === "new"
                    ? commitAdmissionBacklogSlot
                    : releaseAdmissionBacklogSlot;
                }
                const failure = Option.getOrUndefined(
                  Cause.failureOption(exit.cause),
                );
                // Conflict/backlog errors prove no row was inserted. A SqlError,
                // interruption, or defect can be observed after PostgreSQL committed,
                // so retain the slot conservatively until the next live-count refresh.
                return admissionFailureDefinitelyDidNotInsert(failure)
                  ? releaseAdmissionBacklogSlot
                  : commitAdmissionBacklogSlot;
              }),
            );
            yield* submitDurableAdmissionDurationTimer(
              Effect.succeed(
                Duration.millis(Date.now() - durableAdmissionStartedAt),
              ),
            );
            if (admitted.kind === "new") {
              if (!reservation.reserved) {
                return yield* Effect.dieMessage(
                  "Durable admission inserted without a reserved backlog slot",
                );
              }
              yield* wakeTxQueueProcessor;
            }

            Effect.runSync(Metric.increment(txCounter));
            yield* recordLatency();
            const responseStartedAt = Date.now();
            const response = yield* HttpServerResponse.json(
              {
                txId: normalized.txIdHex,
                status: admitted.entry.status,
                firstSeenAt: admitted.entry.first_seen_at.toISOString(),
                lastSeenAt: admitted.entry.last_seen_at.toISOString(),
                duplicate: admitted.kind === "duplicate",
              },
              { status: admitted.kind === "new" ? 202 : 200 },
            );
            yield* submitResponseDurationTimer(
              Effect.succeed(Duration.millis(Date.now() - responseStartedAt)),
            );
            return response;
          });
        }),
      });
      if (Option.isNone(permittedResponse)) {
        yield* Metric.increment(submitQueueOfferFailureCounter);
        yield* recordLatency();
        return yield* HttpServerResponse.json(
          {
            error: "Submit ingress capacity is full; retry later",
          },
          { status: 503 },
        );
      }
      return yield* permittedResponse.value;
    }).pipe(
      Effect.catchTag("TxAdmissionConflictError", (e) =>
        Effect.gen(function* () {
          yield* recordLatency();
          return yield* HttpServerResponse.json(
            {
              error: "E_TX_ID_BYTES_CONFLICT",
              message: e.message,
              txId: e.txIdHex,
            },
            { status: 409 },
          );
        }),
      ),
      Effect.catchTag("TxAdmissionBacklogFullError", (e) =>
        Effect.gen(function* () {
          yield* Metric.increment(submitQueueOfferFailureCounter);
          yield* recordLatency();
          if (e.unit === "bytes") {
            return yield* HttpServerResponse.json(
              {
                error: "Durable submission admission byte backlog is full",
                backlogBytes: e.backlog.toString(),
                maxBacklogBytes: e.maxBacklog.toString(),
              },
              { status: 503 },
            );
          }
          return yield* HttpServerResponse.json(
            {
              error: "Durable submission admission backlog is full",
              backlog: e.backlog.toString(),
              maxBacklog: e.maxBacklog.toString(),
            },
            { status: 503 },
          );
        }),
      ),
      Effect.catchTag("AdmissionWriterShutdownError", (e) =>
        Effect.gen(function* () {
          yield* Metric.increment(submitQueueOfferFailureCounter);
          yield* recordLatency();
          return yield* HttpServerResponse.json(
            {
              error: "Durable submission admission writer is unavailable",
              message: e.message,
            },
            { status: 503 },
          );
        }),
      ),
      Effect.catchTag("DatabaseError", (e) =>
        Effect.gen(function* () {
          yield* recordLatency();
          return yield* failWith500(
            "POST",
            "submit",
            e.cause,
            "durable transaction admission failed",
          );
        }),
      ),
      Effect.catchTag("HttpBodyError", (e) =>
        failWith500("POST", "submit", e, "▫️ L2 transaction failed"),
      ),
    );
  });

/**
 * Focused router used by the admission integration harness. Keeping this as a
 * real HttpRouter (rather than calling the handler as a function) makes route,
 * request-body, status-code, and response-body contract executable while
 * allowing the harness to hold validation wakeups at a deterministic boundary.
 */
export const buildSubmitRouter = <R>(
  wakeTxQueueProcessor: Effect.Effect<void, never, R>,
  withMonitoring?: boolean,
  consensusProfile: MidgardConsensusProfileV1 = MIDGARD_CONSENSUS_PROFILE_V1,
) =>
  HttpRouter.empty.pipe(
    HttpRouter.post(
      `/${SUBMIT_ENDPOINT}`,
      postSubmitHandler(withMonitoring, wakeTxQueueProcessor).pipe(
        Effect.provideService(
          ContractDeploymentIdentity,
          ContractDeploymentIdentity.make({
            kind: "derived",
            consensusProfile,
          }),
        ),
      ),
    ),
  );

/**
 * Builds the full HTTP router for the node command server.
 */
export const buildListenRouter = (
  withMonitoring?: boolean,
): Effect.Effect<
  HttpServerResponse.HttpServerResponse,
  HttpBodyError,
  | Database
  | AdmissionWriter
  | BatchSql
  | WriteBehind
  | ValidationPool
  | MempoolLedgerCache
  | Lucid
  | NodeConfig
  | MidgardContracts
  | ContractDeploymentIdentity
  | SqlClient
  | HttpServerRequest.HttpServerRequest
  | Globals
> =>
  HttpRouter.empty
    .pipe(
      HttpRouter.get(`/${HEALTH_ENDPOINT}`, getHealthHandler),
      HttpRouter.get(`/${READINESS_ENDPOINT}`, getReadinessHandler),
      HttpRouter.get(`/${PIPELINE_STATUS_ENDPOINT}`, getPipelineStatusHandler),
      HttpRouter.get(`/${PROTOCOL_INFO_ENDPOINT}`, getProtocolInfoHandler),
      HttpRouter.get(`/${TX_ENDPOINT}`, getTxHandler),
      HttpRouter.get(`/${TX_STATUS_ENDPOINT}`, getTxStatusHandler),
      HttpRouter.post(`/${TX_STATUS_ENDPOINT}`, postTxStatusBatchHandler),
      HttpRouter.get(`/${DEPOSIT_STATUS_ENDPOINT}`, getDepositStatusHandler),
      HttpRouter.get(`/${ADDRESS_HISTORY_ENDPOINT}`, getTxsOfAddressHandler),
      HttpRouter.get(`/${UTXO_ENDPOINT}`, getUtxoHandler),
      HttpRouter.get(`/${UTXOS_ENDPOINT}`, getUtxosHandler),
      HttpRouter.get(`/${BLOCK_ENDPOINT}`, getBlockHandler),
    )
    .pipe(
      HttpRouter.get(
        `/${INIT_ENDPOINT}`,
        withAdminAccess(INIT_ENDPOINT, getInitHandler),
      ),
      HttpRouter.get(
        `/${COMMIT_ENDPOINT}`,
        withAdminAccess(COMMIT_ENDPOINT, getCommitEndpoint),
      ),
      HttpRouter.get(
        `/${MERGE_ENDPOINT}`,
        withAdminAccess(MERGE_ENDPOINT, getMergeHandler),
      ),
      HttpRouter.get(
        `/${STATE_QUEUE_ENDPOINT}`,
        withAdminAccess(STATE_QUEUE_ENDPOINT, getStateQueueHandler),
      ),
      HttpRouter.get(
        `/${STATE_QUEUE_MUTATION_LEASE_ENDPOINT}`,
        withAdminAccess(
          STATE_QUEUE_MUTATION_LEASE_ENDPOINT,
          getStateQueueMutationLeaseHandler,
        ),
      ),
      HttpRouter.post(
        `/${STATE_QUEUE_MUTATION_LEASE_ENDPOINT}`,
        withAdminAccess(
          STATE_QUEUE_MUTATION_LEASE_ENDPOINT,
          postStateQueueMutationLeaseHandler,
        ),
      ),
      HttpRouter.get(
        `/logBlocksDB`,
        withAdminAccess("logBlocksDB", getLogBlocksDBHandler),
      ),
      HttpRouter.get(
        `/logGlobals`,
        withAdminAccess("logGlobals", getLogGlobalsHandler),
      ),
      HttpRouter.post(`/${UTXOS_ENDPOINT}`, postUtxosByTxOutRefsHandler),
      HttpRouter.post(`/${DEPOSIT_BUILD_ENDPOINT}`, postDepositBuildHandler),
      HttpRouter.post(
        `/${SUBMIT_ENDPOINT}`,
        postSubmitHandler(withMonitoring, requestTxQueueProcessorWakeup),
      ),
    )
    .pipe(
      Effect.catchAllCause((cause) =>
        failWith500("GET", "router", Cause.pretty(cause), "unknown endpoint"),
      ),
    );
