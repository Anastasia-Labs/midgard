import { type SubmitSlotSnapshot } from "@al-ft/midgard-core/ogmios-slot";
import { Duration, Effect, Ref } from "effect";

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

/**
 * How `/readyz` answered the L1 provider question: from fresh cached
 * evidence, from the cache because another request's direct preflight is in
 * flight, or through the direct raw-provider preflight that only runs on
 * fresh exact HubOracle evidence.
 */
export type L1ProviderReadinessProbe =
  | { readonly mode: "cached_fresh"; readonly baseRevision: number }
  | {
      readonly mode: "direct_preflight_in_flight";
      readonly baseRevision: number;
    }
  | {
      readonly mode: "exact_gated_direct_preflight";
      readonly healthy: true;
      readonly ledgerSlot: SubmitSlotSnapshot;
      readonly publishedRevision: number;
    }
  | {
      readonly mode: "exact_gated_direct_preflight";
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

export const localLedgerSlotFromPreflight = (
  report: L1ProviderPreflightReport,
): SubmitSlotSnapshot => {
  const localLedgerSlot = report.sources.find(
    (source) => source.healthy && source.localLedgerSlot !== undefined,
  )?.localLedgerSlot;
  if (!report.ok || localLedgerSlot === undefined) {
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
        : "Direct L1 provider preflight returned no local ledger slot evidence",
    );
  }
  return localLedgerSlot;
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
        try: () => localLedgerSlotFromPreflight(report),
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
    mode: "exact_gated_direct_preflight",
    healthy: false,
    error:
      evidence.lastObservationKind === "exact_failure"
        ? (evidence.lastExactFailure ??
          "Exact HubOracle readiness probe failed")
        : "L1 provider evidence changed while a direct probe was in flight",
    publishedRevision: evidence.evidenceRevision,
  };
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
          mode: "exact_gated_direct_preflight",
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
  ledgerSlot,
  observedAtMs,
  expectedRevision,
}: {
  readonly globals: Globals;
  readonly ledgerSlot: SubmitSlotSnapshot;
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
        ledgerSlot,
        successKind: "direct",
      });
      return [
        {
          mode: "exact_gated_direct_preflight",
          healthy: true,
          ledgerSlot,
          publishedRevision: updated.evidenceRevision,
        },
        updated,
      ];
    },
  );
