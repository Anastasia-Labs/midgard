import { Duration, Effect, Ref } from "effect";

import { type SubmitSlotSnapshot } from "../local-ogmios-slot.js";
import { type L1ProviderHealthEvidence } from "../services/index.js";
import { Globals, nextL1ProviderHealthEvidence } from "../services/index.js";
import { type L1ProviderPreflightReport } from "./l1-provider-preflight.js";

export const TX_ENDPOINT: string = "tx";

export const ADDRESS_HISTORY_ENDPOINT: string = "txs";

export const MERGE_ENDPOINT: string = "merge";

export const UTXO_ENDPOINT: string = "utxo";

export const UTXOS_ENDPOINT: string = "utxos";

export const BLOCK_ENDPOINT: string = "block";

export const INIT_ENDPOINT: string = "init";

export const COMMIT_ENDPOINT: string = "commit";

export const SUBMIT_ENDPOINT: string = "submit";

export const DEPOSIT_BUILD_ENDPOINT: string = "deposit/build";

export const READINESS_L1_PROVIDER_LIVE_TIMEOUT_MS = 2_000;

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

export const readinessProbeFromLatestExactObservation = (
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

export const recordDirectProbeFailure = ({
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

export const recordDirectProbeSuccess = ({
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
