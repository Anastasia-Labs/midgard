import { Effect, Option, Ref } from "effect";

import { type SubmitSlotSnapshot } from "../local-ogmios-slot.js";
import { type L1ProviderHealthEvidence } from "../services/index.js";
import { Globals } from "../services/index.js";
import {
  l1ProviderEvidenceIsFresh,
  l1ProviderReadinessEvidenceIsFresh,
  type L1ProviderReadinessProbe,
  readinessProbeFromLatestExactObservation,
  recordDirectProbeFailure,
  recordDirectProbeSuccess,
} from "./listen-router.l1-provider-readiness-evidence-is-fresh.js";

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

export const STATE_QUEUE_ENDPOINT: string = "stateQueue";

export const TX_STATUS_ENDPOINT: string = "tx-status";

export const PIPELINE_STATUS_ENDPOINT: string = "pipeline-status";

export const DEPOSIT_STATUS_ENDPOINT: string = "deposit-status";

export const PROTOCOL_INFO_ENDPOINT: string = "protocol-info";

export const HEALTH_ENDPOINT: string = "healthz";

export const errorMessage = (error: unknown): string =>
  error instanceof Error ? error.message : String(error);
