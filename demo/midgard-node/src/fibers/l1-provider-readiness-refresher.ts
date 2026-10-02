import { Cause, Effect, Either, Option, Ref, Schedule } from "effect";

import {
  READINESS_L1_PROVIDER_PROBE_TIMEOUT_MS,
  runCombinedL1ReadinessProbe,
} from "../l1-provider-readiness-probe.js";
import {
  readLocalOgmiosSubmitSlot,
  type SubmitSlotSnapshot,
} from "../local-ogmios-slot.js";
import {
  DEFAULT_L1_CONTROL_PLANE_MAX_HOLD_MS,
  Globals,
  type L1ProviderHealthEvidence,
  Lucid,
  MidgardContracts,
  nextL1ProviderHealthEvidence,
  NodeConfig,
  withL1ControlPlaneWaitTimeout,
} from "../services/index.js";
import { fetchHubOracleWitness } from "../transactions/initialization.js";

/**
 * How often the exact HubOracle + local-Ogmios readiness probe runs. Exact
 * evidence expires after DEFAULT_L1_CONTROL_PLANE_MAX_HOLD_MS; this cadence
 * leaves room for a full queue wait behind other control-plane holders.
 */
export const L1_PROVIDER_EXACT_REFRESH_INTERVAL_MS = 15_000;

/**
 * Longest the refresher queues for the control plane. Every holder is bounded
 * by its own max hold, so a permit not granted within one default hold means
 * the exact evidence is stale anyway and readiness must say so.
 */
export const L1_PROVIDER_EXACT_REFRESH_WAIT_MS =
  DEFAULT_L1_CONTROL_PLANE_MAX_HOLD_MS;

/** Longest failure text the refresher publishes to `/readyz`. */
export const READINESS_FAILURE_MAX_CHARS = 300;

/**
 * The failure text `/readyz` may carry: the first non-empty line, bounded.
 * `/readyz` is public, and a defect's `Cause.pretty` is a stack trace with
 * absolute install paths, so the full text goes to the node log only.
 */
export const readinessFailureSummary = (detail: string): string =>
  (
    detail.split(/\r?\n/u).find((line) => line.trim() !== "") ??
    "L1 provider readiness probe failed"
  )
    .trim()
    .slice(0, READINESS_FAILURE_MAX_CHARS);

export type ExactL1ProviderRefreshOutcome =
  | {
      readonly kind: "published";
      readonly evidence: L1ProviderHealthEvidence;
      /** The probe's whole failure text, for the log; null on success. */
      readonly failureDetail: string | null;
    }
  | { readonly kind: "control_plane_unavailable" };

/**
 * Queues (bounded) for the shared Lucid control plane and runs the exact
 * readiness probe under a bounded hold, publishing its success or failure (a
 * defect included) as exact evidence. A wait that times out observed nothing,
 * so it publishes nothing and the evidence ages toward unready.
 */
export const refreshExactL1ProviderEvidence = <E, R>({
  globals,
  probe,
  maxHoldMs,
  waitTimeoutMs,
}: {
  readonly globals: Globals;
  readonly probe: Effect.Effect<SubmitSlotSnapshot, E, R>;
  readonly maxHoldMs: number;
  readonly waitTimeoutMs: number;
}): Effect.Effect<ExactL1ProviderRefreshOutcome, never, R> =>
  Effect.gen(function* () {
    const attempt = yield* Effect.either(
      withL1ControlPlaneWaitTimeout(
        globals,
        { scope: "l1_provider_readiness_refresh", waitTimeoutMs, maxHoldMs },
        probe,
      ),
    ).pipe(
      // A defect in the probe (a synchronous throw in the Lucid read, say) is
      // an exact failure too. Leaving the last exact success in place would
      // keep `/readyz` healthy on direct probes until that success aged out.
      Effect.catchAllCause((cause) =>
        Effect.succeed(Either.left(Cause.pretty(cause))),
      ),
    );
    if (attempt._tag === "Right" && Option.isNone(attempt.right)) {
      return { kind: "control_plane_unavailable" } as const;
    }
    const observedAtMs = Date.now();
    const failureDetail = attempt._tag === "Left" ? String(attempt.left) : null;
    const evidence = yield* Ref.updateAndGet(
      globals.L1_PROVIDER_HEALTH,
      (current) =>
        attempt._tag === "Left"
          ? nextL1ProviderHealthEvidence({
              current,
              healthy: false,
              error: readinessFailureSummary(String(attempt.left)),
              observedAtMs,
              successKind: "exact",
            })
          : nextL1ProviderHealthEvidence({
              current,
              healthy: true,
              observedAtMs,
              ogmiosSlot: Option.getOrThrow(attempt.right),
              successKind: "exact",
            }),
    );
    return { kind: "published", evidence, failureDetail } as const;
  });

/**
 * Repeats the exact refresh on a schedule, logging refreshes that did not
 * produce fresh exact success.
 */
export const runL1ProviderReadinessRefresher = <E, R>({
  globals,
  probe,
  maxHoldMs,
  waitTimeoutMs,
  schedule,
}: {
  readonly globals: Globals;
  readonly probe: Effect.Effect<SubmitSlotSnapshot, E, R>;
  readonly maxHoldMs: number;
  readonly waitTimeoutMs: number;
  readonly schedule: Schedule.Schedule<number>;
}): Effect.Effect<void, never, R> =>
  Effect.repeat(
    refreshExactL1ProviderEvidence({
      globals,
      probe,
      maxHoldMs,
      waitTimeoutMs,
    }).pipe(
      Effect.tap((outcome) =>
        outcome.kind === "control_plane_unavailable"
          ? Effect.logWarning(
              `L1 provider readiness refresh could not acquire the L1 control plane within ${waitTimeoutMs.toString()}ms.`,
            )
          : outcome.evidence.lastObservationKind === "exact_failure"
            ? Effect.logWarning(
                `L1 provider readiness refresh failed: ${outcome.failureDetail ?? "unknown failure"}`,
              )
            : Effect.void,
      ),
      // A defect after the probe (in logging, say) must not take the node's
      // fiber set down; the next round refreshes the evidence again.
      Effect.catchAllCause(Effect.logWarning),
    ),
    schedule,
  ).pipe(Effect.asVoid);

/**
 * The only producer of exact L1 provider evidence. `/readyz` reads it and,
 * between refreshes, extends it with raw-provider probes that never touch the
 * shared Lucid instance. The probe holds the control plane no longer than the
 * readiness probe timeout, so a commit waits at most that long per refresh.
 */
export const l1ProviderReadinessRefresherFiber = (
  schedule: Schedule.Schedule<number>,
): Effect.Effect<
  void,
  never,
  Globals | NodeConfig | Lucid | MidgardContracts
> =>
  Effect.gen(function* () {
    const globals = yield* Globals;
    const nodeConfig = yield* NodeConfig;
    const lucid = yield* Lucid;
    const contracts = yield* MidgardContracts;
    const probeTimeoutMs = Math.min(
      nodeConfig.L1_PROVIDER_PREFLIGHT_TIMEOUT_MS,
      READINESS_L1_PROVIDER_PROBE_TIMEOUT_MS,
    );
    yield* Effect.logInfo("L1 provider readiness refresher fiber started.");
    yield* runL1ProviderReadinessRefresher({
      globals,
      probe: runCombinedL1ReadinessProbe(
        fetchHubOracleWitness(lucid.api, contracts),
        readLocalOgmiosSubmitSlot({
          ogmiosUrl: nodeConfig.L1_OGMIOS_KEY,
          timeoutMs: probeTimeoutMs,
        }),
      ),
      maxHoldMs: probeTimeoutMs,
      waitTimeoutMs: L1_PROVIDER_EXACT_REFRESH_WAIT_MS,
      schedule,
    });
  });
