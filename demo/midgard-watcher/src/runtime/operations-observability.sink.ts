import {
  createWatcherAlertBook,
  WATCHER_DA_FETCH_ALERT_MAXIMUM_AGE_MS,
} from "./operations-observability.alert-book.js";
import { hash32, percentile } from "./operations-observability.percentile.js";
import {
  MAXIMUM_PAGE_SIZE,
  MAXIMUM_RETAINED_DIAGNOSTICS,
  NATURAL,
  natural,
  unverifiedHeaderCounter,
  verificationLatency,
  WATCHER_OPERATIONS_OBSERVABILITY,
  WATCHER_PROOF_STAGE_KINDS,
  type WatcherAlertDiagnostic,
  type WatcherDaFetchDiagnostic,
  type WatcherEventDiagnostic,
  type WatcherL1SourceDiagnostic,
  type WatcherOperationsApi,
  type WatcherOperationsDaBondPool,
  type WatcherOperationsDaBondPoolReadFailure,
  type WatcherOperationsDiagnostic,
  type WatcherOperationsMetrics,
  type WatcherOperationsSink,
  type WatcherOperationsStatus,
  type WatcherProofStepDiagnostic,
  type WatcherVerificationDiagnostic,
} from "./operations-observability.watcher-operations-metrics.js";

export type WatcherOperationsSinkInput = Readonly<{
  deploymentFingerprint: string;
  nowMs?: () => bigint;
  monotonicNowMs?: () => number;
  l1FreshnessMaximumAgeMs?: number;
  maximumRetainedDiagnostics?: number;
  /** How long a failed DA fetch holds readiness unless it is repeated. */
  daFetchAlertMaximumAgeMs?: number;
}>;

/** A wall timestamp's age, carried forward on the monotonic clock. */
export type WatcherAgeAnchor = Readonly<{
  origin: string;
  receivedAt: bigint;
  initialAge: bigint | null;
}>;

/** What the sink recorded, read-only, as the operations view serves it. */
export type WatcherOperationsRecord = Readonly<{
  deploymentFingerprint: string;
  /** The wall clock in milliseconds; it throws rather than go negative. */
  nowMs: () => bigint;
  anchorAge: (origin: string) => WatcherAgeAnchor;
  /** The anchored age now, or null when the origin was in the future. */
  ageMs: (anchor: WatcherAgeAnchor) => bigint | null;
  diagnostics: WatcherOperationsApi["diagnostics"];
  /** No L1 source recorded yet, or one stale or disagreeing; else null. */
  l1SourceReason: () => "l1_source_unavailable" | "l1_source_stale" | null;
  alertsHoldReadiness: (nowMs: bigint) => boolean;
  status: () => Pick<
    WatcherOperationsStatus,
    "activeAlerts" | "daBondPool" | "daBondPoolReadFailure"
  >;
  metrics: () => Pick<
    WatcherOperationsMetrics,
    | "verificationLatencyMs"
    | "daLatencyMs"
    | "proofSteps"
    | "unprocessedEventCount"
    | "oldestUnprocessedEventAgeMs"
    | "l1Sources"
    | "activeAlertCount"
    | "unverifiedHeaders"
    | "deferredClassifications"
  >;
}>;

export type WatcherOperationsRecorder = Readonly<{
  sink: WatcherOperationsSink;
  record: WatcherOperationsRecord;
}>;

const DIAGNOSTIC_KINDS: readonly string[] = Object.freeze([
  "verification",
  "da_fetch",
  "proof_step",
  "event",
  "l1_source",
  "alert",
]);

/**
 * The sink every module reports to: the diagnostics ring, the alert book,
 * latency samples, L1 sources and the DA bond pool. It needs only the
 * deployment fingerprint, clocks and bounds, so startup creates it before
 * any module that reports to it; the operations view reads its record.
 */
export const createWatcherOperationsSink = (
  input: WatcherOperationsSinkInput,
): WatcherOperationsRecorder => {
  hash32(input.deploymentFingerprint, "observability deployment fingerprint");
  const wallMs = input.nowMs ?? (() => BigInt(Date.now()));
  const nowMs = (): bigint => {
    const value = wallMs();
    if (value < 0n) throw new Error("observability clock is invalid");
    return value;
  };
  const monotonicNowMs = input.monotonicNowMs ?? (() => performance.now());
  const monotonicTime = (): bigint => {
    const value = Math.floor(monotonicNowMs());
    if (!Number.isSafeInteger(value) || value < 0) {
      throw new Error("observability monotonic clock is invalid");
    }
    return BigInt(value);
  };
  const l1FreshnessMaximumAgeMs = input.l1FreshnessMaximumAgeMs ?? 120_000;
  const maximumRetainedDiagnostics =
    input.maximumRetainedDiagnostics ?? MAXIMUM_RETAINED_DIAGNOSTICS;
  if (
    !Number.isSafeInteger(l1FreshnessMaximumAgeMs) ||
    l1FreshnessMaximumAgeMs < 1 ||
    l1FreshnessMaximumAgeMs > 3_600_000 ||
    !Number.isSafeInteger(maximumRetainedDiagnostics) ||
    maximumRetainedDiagnostics < MAXIMUM_PAGE_SIZE ||
    maximumRetainedDiagnostics > MAXIMUM_RETAINED_DIAGNOSTICS
  ) {
    throw new Error("observability bounds are invalid");
  }

  let nextSequence = 1n;
  const diagnostics: WatcherOperationsDiagnostic[] = [];
  const verificationLatencies: bigint[] = [];
  const unverifiedHeaders = unverifiedHeaderCounter();
  const daLatencies: bigint[] = [];
  const latestProofSteps = new Map<string, WatcherProofStepDiagnostic>();
  const latestEvents = new Map<string, WatcherEventDiagnostic>();
  const latestL1Sources = new Map<string, WatcherL1SourceDiagnostic>();
  let latestDaBondPool: WatcherOperationsDaBondPool | null = null;
  let latestDaBondPoolReadFailure: WatcherOperationsDaBondPoolReadFailure | null =
    null;
  const sourceAges = new Map<string, WatcherAgeAnchor>();
  const eventAges = new Map<string, WatcherAgeAnchor>();
  // Wall timestamps may cross a host clock adjustment. Unknown initial ages
  // stay unknown; known ages advance only with this process's monotonic clock.
  const anchorAge = (origin: string): WatcherAgeAnchor => {
    const timestamp = natural(origin, "age origin");
    const wall = nowMs();
    return {
      origin,
      receivedAt: monotonicTime(),
      initialAge: timestamp > wall ? null : wall - timestamp,
    };
  };
  const ageAt = (
    anchor: WatcherAgeAnchor,
    monotonic: bigint,
  ): bigint | null => {
    if (monotonic < anchor.receivedAt)
      throw new Error("observability monotonic clock regressed");
    return anchor.initialAge === null
      ? null
      : anchor.initialAge + monotonic - anchor.receivedAt;
  };

  const append = <T extends WatcherOperationsDiagnostic>(
    record: Omit<T, "sequence">,
  ): T => {
    const sequenced = Object.freeze({
      ...record,
      sequence: nextSequence.toString(),
    }) as T;
    nextSequence += 1n;
    diagnostics.push(sequenced);
    if (diagnostics.length > maximumRetainedDiagnostics) diagnostics.shift();
    return sequenced;
  };

  const boundedSample = (values: bigint[], value: bigint): void => {
    values.push(value);
    if (values.length > maximumRetainedDiagnostics) values.shift();
  };
  const alerts = createWatcherAlertBook({
    append: (record) => append<WatcherAlertDiagnostic>(record),
    daFetchMaximumAgeMs:
      input.daFetchAlertMaximumAgeMs ?? WATCHER_DA_FETCH_ALERT_MAXIMUM_AGE_MS,
  });
  const setAlert = alerts.set;

  const sink: WatcherOperationsSink = Object.freeze({
    recordVerification: (value) => {
      const latency = verificationLatency(value);
      append<WatcherVerificationDiagnostic>({
        kind: "verification",
        ...value,
      });
      if (latency !== null) boundedSample(verificationLatencies, latency);
      unverifiedHeaders.record(value.outcome);
      alerts.settleHeader(value.headerHash, value.outcome, value.completedAtMs);
    },
    recordDaFetch: (value) => {
      hash32(value.subjectDigest, "DA subject digest");
      natural(value.startedAtMs, "DA fetch start time");
      natural(value.completedAtMs, "DA fetch completion time");
      const latency = natural(value.elapsedMs, "DA fetch elapsed time");
      append<WatcherDaFetchDiagnostic>({
        kind: "da_fetch",
        ...value,
      });
      boundedSample(daLatencies, latency);
    },
    recordProofStep: (value) => {
      hash32(value.decisionDigest, "proof-step decision digest");
      hash32(value.actionIdentityDigest, "proof-step action identity digest");
      if (!WATCHER_PROOF_STAGE_KINDS.includes(value.stage)) {
        throw new Error("proof-step stage is invalid");
      }
      natural(value.updatedAtMs, "proof-step update time");
      const record = append<WatcherProofStepDiagnostic>({
        kind: "proof_step",
        ...value,
      });
      latestProofSteps.set(
        `${value.decisionDigest}:${value.actionIdentityDigest}`,
        record,
      );
    },
    recordEvent: (value) => {
      hash32(value.eventDigest, "event digest");
      natural(value.inclusionAtMs, "event inclusion time");
      natural(value.updatedAtMs, "event update time");
      if (eventAges.get(value.eventDigest)?.origin !== value.inclusionAtMs) {
        eventAges.set(value.eventDigest, anchorAge(value.inclusionAtMs));
      }
      const record = append<WatcherEventDiagnostic>({
        kind: "event",
        ...value,
      });
      latestEvents.set(value.eventDigest, record);
    },
    recordL1Source: (value) => {
      hash32(value.sourceIdentityDigest, "L1 source identity digest");
      hash32(value.blockHash, "L1 source block hash");
      natural(value.blockNo, "L1 source block number");
      natural(value.slot, "L1 source slot");
      natural(value.observedAtMs, "L1 source observation time");
      const record = append<WatcherL1SourceDiagnostic>({
        kind: "l1_source",
        ...value,
      });
      latestL1Sources.set(value.sourceIdentityDigest, record);
      sourceAges.set(value.sourceIdentityDigest, anchorAge(value.observedAtMs));
    },
    setAlert,
    recordDaBondPool: (readout, subjectDigest, observedAtMs) => {
      hash32(subjectDigest, "DA bond pool subject digest");
      natural(observedAtMs, "DA bond pool observation time");
      latestDaBondPool = Object.freeze({
        ...readout,
        alerts: Object.freeze({ ...readout.alerts }),
        observedAtMs,
      });
      latestDaBondPoolReadFailure = null;
      for (const [code, active] of [
        ["da_bond_pool_under_backed", readout.alerts.underBacked],
        ["da_bond_pool_withdrawing", readout.alerts.withdrawing],
      ] as const) {
        if (alerts.state(code, subjectDigest) === active) continue;
        setAlert({ code, subjectDigest, active, observedAtMs });
      }
    },
    recordDaBondPoolReadFailure: (error, failedAtMs) => {
      natural(failedAtMs, "DA bond pool read failure time");
      latestDaBondPoolReadFailure = Object.freeze({
        // A concise cause, never an embedded transaction payload.
        error: error
          .replace(/[a-fA-F0-9]{128,}/g, "[hex omitted]")
          .slice(0, 2048),
        failedAtMs,
      });
    },
  });

  const sourceHealth = () => {
    const monotonic = monotonicTime();
    let fresh = 0;
    let stale = 0;
    let disagreement = 0;
    let maximumAge: bigint | null = null;
    let unknownAge = false;
    for (const source of latestL1Sources.values()) {
      const age = ageAt(
        sourceAges.get(source.sourceIdentityDigest)!,
        monotonic,
      );
      if (age === null) unknownAge = true;
      else if (maximumAge === null || age > maximumAge) maximumAge = age;
      if (source.status === "disagreement") disagreement += 1;
      else if (
        source.status === "stale" ||
        age === null ||
        age > BigInt(l1FreshnessMaximumAgeMs)
      )
        stale += 1;
      else fresh += 1;
    }
    return {
      fresh,
      stale,
      disagreement,
      maximumAge: unknownAge ? null : maximumAge,
    };
  };

  const metrics: WatcherOperationsRecord["metrics"] = () => {
    const proofSteps = {
      queued: 0,
      preflight: 0,
      submitted: 0,
      confirmed: 0,
      reconciling: 0,
      completed: 0,
      cancelled: 0,
      failed: 0,
    };
    for (const step of latestProofSteps.values()) {
      proofSteps[step.status] += 1;
    }
    const unprocessed = [...latestEvents.values()].filter(
      ({ status: eventStatus }) => eventStatus === "unprocessed",
    );
    const source = sourceHealth();
    const monotonic = monotonicTime();
    const unprocessedAges = unprocessed.map(({ eventDigest }) =>
      ageAt(eventAges.get(eventDigest)!, monotonic),
    );
    const oldestEventAge =
      unprocessedAges.length === 0 ||
      unprocessedAges.some((age) => age === null)
        ? null
        : unprocessedAges.reduce<bigint>(
            (maximum, age) => (age! > maximum ? age! : maximum),
            0n,
          );
    const summarize = (values: readonly bigint[]) =>
      Object.freeze({
        sampleCount: values.length.toString(),
        p50: percentile(values, 50, 100),
        p95: percentile(values, 95, 100),
        maximum: percentile(values, 100, 100),
      });
    return {
      verificationLatencyMs: summarize(verificationLatencies),
      daLatencyMs: summarize(daLatencies),
      proofSteps: Object.freeze(
        Object.fromEntries(
          Object.entries(proofSteps).map(([key, value]) => [
            key,
            value.toString(),
          ]),
        ),
      ) as WatcherOperationsMetrics["proofSteps"],
      unprocessedEventCount: unprocessed.length.toString(),
      oldestUnprocessedEventAgeMs: oldestEventAge?.toString() ?? null,
      l1Sources: Object.freeze({
        configured: latestL1Sources.size.toString(),
        fresh: source.fresh.toString(),
        stale: source.stale.toString(),
        disagreement: source.disagreement.toString(),
        maximumFreshnessAgeMs: source.maximumAge?.toString() ?? null,
      }),
      activeAlertCount: alerts.active().length.toString(),
      unverifiedHeaders: unverifiedHeaders.summary(),
      deferredClassifications: unverifiedHeaders.deferred(),
    };
  };

  const record: WatcherOperationsRecord = Object.freeze({
    deploymentFingerprint: input.deploymentFingerprint,
    nowMs,
    anchorAge,
    ageMs: (anchor: WatcherAgeAnchor) => ageAt(anchor, monotonicTime()),
    diagnostics: ({ kind, cursor = "0", limit = 50 }) => {
      if (
        !DIAGNOSTIC_KINDS.includes(kind) ||
        !NATURAL.test(cursor) ||
        !Number.isSafeInteger(limit) ||
        limit < 1 ||
        limit > MAXIMUM_PAGE_SIZE
      ) {
        throw new Error("observability diagnostic page request is invalid");
      }
      const after = BigInt(cursor);
      const matching = diagnostics.filter(
        (entry) => entry.kind === kind && BigInt(entry.sequence) > after,
      );
      const records = Object.freeze(matching.slice(0, limit));
      return Object.freeze({
        schemaVersion: WATCHER_OPERATIONS_OBSERVABILITY,
        kind,
        records,
        nextCursor:
          matching.length > records.length
            ? (records.at(-1)?.sequence ?? cursor)
            : null,
      });
    },
    l1SourceReason: () => {
      const sources = sourceHealth();
      if (latestL1Sources.size === 0) return "l1_source_unavailable";
      return sources.stale > 0 || sources.disagreement > 0
        ? "l1_source_stale"
        : null;
    },
    alertsHoldReadiness: alerts.holdsReadiness,
    status: () => ({
      activeAlerts: alerts.active(),
      daBondPool: latestDaBondPool,
      daBondPoolReadFailure: latestDaBondPoolReadFailure,
    }),
    metrics,
  });

  return Object.freeze({ sink, record });
};
