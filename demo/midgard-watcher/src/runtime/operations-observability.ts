import type { WatcherFaultProofSupervisor } from "../fault-proofs/fault-proof-supervisor.js";

export const WATCHER_OPERATIONS_OBSERVABILITY =
  "midgard-watcher-production-operations-observability-v1" as const;

const HEX_32 = /^[0-9a-f]{64}$/u;
const NATURAL = /^(?:0|[1-9][0-9]*)$/u;
const MAXIMUM_PAGE_SIZE = 100;
const MAXIMUM_RETAINED_DIAGNOSTICS = 10_000;

export const WATCHER_ALERT_CODES = Object.freeze([
  "da_fetch_failure",
  "root_mismatch",
  "proof_submission_failure",
  "maturity_deadline_risk",
  "provider_disagreement",
  "chain_rollback",
  "deployment_fingerprint_mismatch",
  "proof_family_coverage_gap",
  "l1_source_stale",
] as const);

export type WatcherAlertCode = (typeof WATCHER_ALERT_CODES)[number];

export type WatcherOperationsDiagnosticKind =
  | "verification"
  | "da_fetch"
  | "proof_step"
  | "event"
  | "l1_source"
  | "alert";

export const WATCHER_PROOF_STAGE_KINDS = Object.freeze([
  "prepare",
  "init",
  "proof_step",
  "publication",
  "certification",
  "correction",
  "removal",
  "terminal",
] as const);

export type WatcherProofStageKind = (typeof WATCHER_PROOF_STAGE_KINDS)[number];

type Sequenced = Readonly<{ sequence: string }>;

export type WatcherVerificationDiagnostic = Sequenced &
  Readonly<{
    kind: "verification";
    subjectDigest: string;
    queuedAtMs: string;
    startedAtMs: string;
    completedAtMs: string;
    outcome:
      | "verified"
      | "pending_da"
      | "unprovable_gap"
      | "fault_detected"
      | "fault_proven"
      | "removed_or_resolved"
      | "failed";
  }>;

export type WatcherDaFetchDiagnostic = Sequenced &
  Readonly<{
    kind: "da_fetch";
    subjectDigest: string;
    startedAtMs: string;
    completedAtMs: string;
    outcome: "succeeded" | "failed" | "timed_out";
  }>;

export type WatcherProofStepDiagnostic = Sequenced &
  Readonly<{
    kind: "proof_step";
    decisionDigest: string;
    stage: WatcherProofStageKind;
    actionIdentityDigest: string;
    status:
      | "queued"
      | "preflight"
      | "submitted"
      | "confirmed"
      | "reconciling"
      | "completed"
      | "cancelled"
      | "failed";
    updatedAtMs: string;
  }>;

export type WatcherEventDiagnostic = Sequenced &
  Readonly<{
    kind: "event";
    eventDigest: string;
    eventKind: "deposit" | "withdrawal" | "forced_order" | "settlement";
    status: "unprocessed" | "processed" | "invalid";
    inclusionAtMs: string;
    updatedAtMs: string;
  }>;

export type WatcherL1SourceDiagnostic = Sequenced &
  Readonly<{
    kind: "l1_source";
    sourceIdentityDigest: string;
    sourceMode: "local_node" | "external_provider";
    status: "consistent" | "stale" | "disagreement";
    blockHash: string;
    blockNo: string;
    slot: string;
    observedAtMs: string;
  }>;

export type WatcherAlertDiagnostic = Sequenced &
  Readonly<{
    kind: "alert";
    code: WatcherAlertCode;
    subjectDigest: string;
    active: boolean;
    observedAtMs: string;
  }>;

export type WatcherOperationsDiagnostic =
  | WatcherVerificationDiagnostic
  | WatcherDaFetchDiagnostic
  | WatcherProofStepDiagnostic
  | WatcherEventDiagnostic
  | WatcherL1SourceDiagnostic
  | WatcherAlertDiagnostic;

export type WatcherOperationsStatus = Readonly<{
  schemaVersion: typeof WATCHER_OPERATIONS_OBSERVABILITY;
  deploymentFingerprint: string;
  observedAtMs: string;
  liveness: "live" | "stopping" | "stopped" | "blocked";
  readiness: "ready" | "not_ready";
  readinessReasons: readonly (
    | "supervisor_not_accepting"
    | "recovery_incomplete"
    | "launch_scope_incomplete"
    | "deadline_at_risk"
    | "deadline_unsafe"
    | "l1_source_unavailable"
    | "l1_source_stale"
    | "active_alert"
  )[];
  launchScope: Readonly<{
    installedCategoryCount: string;
    requiredCategoryCount: string;
    complete: boolean;
  }>;
  supervisor: ReturnType<WatcherFaultProofSupervisor["status"]>;
  activeAlerts: readonly Readonly<{
    code: WatcherAlertCode;
    subjectDigest: string;
    observedAtMs: string;
  }>[];
}>;

export type WatcherOperationsMetrics = Readonly<{
  schemaVersion: typeof WATCHER_OPERATIONS_OBSERVABILITY;
  observedAtMs: string;
  queuedProofCount: string;
  oldestQueuedProofAgeMs: string | null;
  verificationLatencyMs: Readonly<{
    sampleCount: string;
    p50: string | null;
    p95: string | null;
    maximum: string | null;
  }>;
  daLatencyMs: Readonly<{
    sampleCount: string;
    p50: string | null;
    p95: string | null;
    maximum: string | null;
  }>;
  deadlineHealth: "safe" | "at_risk" | "unsafe";
  remainingSafeStartMs: string | null;
  proofSteps: Readonly<{
    queued: string;
    preflight: string;
    submitted: string;
    confirmed: string;
    reconciling: string;
    completed: string;
    cancelled: string;
    failed: string;
  }>;
  unprocessedEventCount: string;
  oldestUnprocessedEventAgeMs: string | null;
  l1Sources: Readonly<{
    configured: string;
    fresh: string;
    stale: string;
    disagreement: string;
    maximumFreshnessAgeMs: string | null;
  }>;
  activeAlertCount: string;
}>;

export type WatcherOperationsPage = Readonly<{
  schemaVersion: typeof WATCHER_OPERATIONS_OBSERVABILITY;
  kind: WatcherOperationsDiagnosticKind;
  records: readonly WatcherOperationsDiagnostic[];
  nextCursor: string | null;
}>;

export type WatcherOperationsApi = Readonly<{
  status(): WatcherOperationsStatus;
  metrics(): WatcherOperationsMetrics;
  diagnostics(
    input: Readonly<{
      kind: WatcherOperationsDiagnosticKind;
      cursor?: string;
      limit?: number;
    }>,
  ): WatcherOperationsPage;
}>;

export type WatcherOperationsSink = Readonly<{
  recordVerification(
    value: Omit<WatcherVerificationDiagnostic, "kind" | "sequence">,
  ): void;
  recordDaFetch(
    value: Omit<WatcherDaFetchDiagnostic, "kind" | "sequence">,
  ): void;
  recordProofStep(
    value: Omit<WatcherProofStepDiagnostic, "kind" | "sequence">,
  ): void;
  recordEvent(value: Omit<WatcherEventDiagnostic, "kind" | "sequence">): void;
  recordL1Source(
    value: Omit<WatcherL1SourceDiagnostic, "kind" | "sequence">,
  ): void;
  setAlert(value: Omit<WatcherAlertDiagnostic, "kind" | "sequence">): void;
}>;

export type WatcherOperationsObservability = Readonly<{
  schemaVersion: typeof WATCHER_OPERATIONS_OBSERVABILITY;
  api: WatcherOperationsApi;
  sink: WatcherOperationsSink;
  handleHttpRequest(request: Request): Promise<Response>;
}>;

const natural = (value: string, label: string): bigint => {
  if (!NATURAL.test(value)) throw new Error(`${label} is invalid`);
  return BigInt(value);
};

const hash32 = (value: string, label: string): string => {
  if (!HEX_32.test(value)) throw new Error(`${label} is invalid`);
  return value;
};

const orderedInterval = (start: string, end: string, label: string): bigint => {
  const startValue = natural(start, `${label} start`);
  const endValue = natural(end, `${label} end`);
  if (endValue < startValue) throw new Error(`${label} is reversed`);
  return endValue - startValue;
};

const percentile = (
  values: readonly bigint[],
  numerator: number,
  denominator: number,
): string | null => {
  if (values.length === 0) return null;
  const ordered = [...values].sort((left, right) =>
    left < right ? -1 : left > right ? 1 : 0,
  );
  const rank = Math.max(
    0,
    Math.ceil((ordered.length * numerator) / denominator) - 1,
  );
  return ordered[rank]!.toString();
};

export const createWatcherOperationsObservability = (input: {
  readonly deploymentFingerprint: string;
  readonly supervisor: WatcherFaultProofSupervisor;
  readonly launchScopeStatus: () => Readonly<{
    installedCategoryCount: number;
    requiredCategoryCount: number;
  }>;
  /** Must be read from the same durable EDF queue as supervisor.status(). */
  readonly durableProofQueueStatus: () => Readonly<{
    queuedJobCount: number;
    oldestQueuedAtMs: string | null;
  }>;
  readonly nowMs?: () => bigint;
  readonly l1FreshnessMaximumAgeMs?: number;
  readonly maximumRetainedDiagnostics?: number;
}): WatcherOperationsObservability => {
  hash32(input.deploymentFingerprint, "observability deployment fingerprint");
  const nowMs = input.nowMs ?? (() => BigInt(Date.now()));
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
  const daLatencies: bigint[] = [];
  const latestProofSteps = new Map<string, WatcherProofStepDiagnostic>();
  const latestEvents = new Map<string, WatcherEventDiagnostic>();
  const latestL1Sources = new Map<string, WatcherL1SourceDiagnostic>();
  const latestAlerts = new Map<string, WatcherAlertDiagnostic>();

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

  const observedTime = (value: string, label: string): bigint => {
    const time = natural(value, label);
    const now = nowMs();
    if (now < 0n || time > now) {
      throw new Error(`${label} is in the future`);
    }
    return time;
  };

  const sink: WatcherOperationsSink = Object.freeze({
    recordVerification: (value) => {
      hash32(value.subjectDigest, "verification subject digest");
      orderedInterval(
        value.queuedAtMs,
        value.startedAtMs,
        "verification queue interval",
      );
      const verificationLatency = orderedInterval(
        value.startedAtMs,
        value.completedAtMs,
        "verification interval",
      );
      observedTime(value.completedAtMs, "verification completion time");
      append<WatcherVerificationDiagnostic>({
        kind: "verification",
        ...value,
      });
      boundedSample(verificationLatencies, verificationLatency);
    },
    recordDaFetch: (value) => {
      hash32(value.subjectDigest, "DA subject digest");
      const latency = orderedInterval(
        value.startedAtMs,
        value.completedAtMs,
        "DA fetch interval",
      );
      observedTime(value.completedAtMs, "DA fetch completion time");
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
      observedTime(value.updatedAtMs, "proof-step update time");
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
      orderedInterval(
        value.inclusionAtMs,
        value.updatedAtMs,
        "event observation interval",
      );
      observedTime(value.updatedAtMs, "event update time");
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
      observedTime(value.observedAtMs, "L1 source observation time");
      const record = append<WatcherL1SourceDiagnostic>({
        kind: "l1_source",
        ...value,
      });
      latestL1Sources.set(value.sourceIdentityDigest, record);
    },
    setAlert: (value) => {
      if (!WATCHER_ALERT_CODES.includes(value.code)) {
        throw new Error("operational alert code is invalid");
      }
      hash32(value.subjectDigest, "operational alert subject digest");
      observedTime(value.observedAtMs, "operational alert observation time");
      const record = append<WatcherAlertDiagnostic>({
        kind: "alert",
        ...value,
      });
      latestAlerts.set(`${value.code}:${value.subjectDigest}`, record);
    },
  });

  const launchScope = () => {
    const value = input.launchScopeStatus();
    if (
      !Number.isSafeInteger(value.installedCategoryCount) ||
      value.installedCategoryCount < 0 ||
      !Number.isSafeInteger(value.requiredCategoryCount) ||
      value.requiredCategoryCount < 1 ||
      value.installedCategoryCount > value.requiredCategoryCount
    ) {
      throw new Error("observability launch-scope status is invalid");
    }
    return Object.freeze({
      installedCategoryCount: value.installedCategoryCount.toString(),
      requiredCategoryCount: value.requiredCategoryCount.toString(),
      complete: value.installedCategoryCount === value.requiredCategoryCount,
    });
  };

  const sourceHealth = (observedAt: bigint) => {
    let fresh = 0;
    let stale = 0;
    let disagreement = 0;
    let maximumAge: bigint | null = null;
    for (const source of latestL1Sources.values()) {
      const sourceTime = natural(source.observedAtMs, "L1 source time");
      if (sourceTime > observedAt) {
        throw new Error("L1 source time is in the future");
      }
      const age = observedAt - sourceTime;
      if (maximumAge === null || age > maximumAge) maximumAge = age;
      if (source.status === "disagreement") disagreement += 1;
      else if (
        source.status === "stale" ||
        age > BigInt(l1FreshnessMaximumAgeMs)
      )
        stale += 1;
      else fresh += 1;
    }
    return Object.freeze({ fresh, stale, disagreement, maximumAge });
  };

  const activeAlerts = () =>
    Object.freeze(
      [...latestAlerts.values()]
        .filter(({ active }) => active)
        .sort(
          (left, right) =>
            left.code.localeCompare(right.code) ||
            left.subjectDigest.localeCompare(right.subjectDigest),
        )
        .map(({ code, subjectDigest, observedAtMs }) =>
          Object.freeze({ code, subjectDigest, observedAtMs }),
        ),
    );

  const status = (): WatcherOperationsStatus => {
    const observedAt = nowMs();
    if (observedAt < 0n) throw new Error("observability clock is invalid");
    const supervisor = input.supervisor.status();
    const scope = launchScope();
    const sources = sourceHealth(observedAt);
    const alerts = activeAlerts();
    const reasons: WatcherOperationsStatus["readinessReasons"][number][] = [];
    if (supervisor.phase !== "accepting")
      reasons.push("supervisor_not_accepting");
    if (!supervisor.recovered) reasons.push("recovery_incomplete");
    if (!scope.complete) reasons.push("launch_scope_incomplete");
    if (supervisor.deadlineHealth === "at_risk")
      reasons.push("deadline_at_risk");
    if (supervisor.deadlineHealth === "unsafe") reasons.push("deadline_unsafe");
    if (latestL1Sources.size === 0) reasons.push("l1_source_unavailable");
    else if (sources.stale > 0 || sources.disagreement > 0)
      reasons.push("l1_source_stale");
    if (alerts.length > 0) reasons.push("active_alert");
    const liveness =
      supervisor.phase === "closed"
        ? "stopped"
        : supervisor.phase === "closing"
          ? "stopping"
          : supervisor.phase === "blocked"
            ? "blocked"
            : "live";
    return Object.freeze({
      schemaVersion: WATCHER_OPERATIONS_OBSERVABILITY,
      deploymentFingerprint: input.deploymentFingerprint,
      observedAtMs: observedAt.toString(),
      liveness,
      readiness: reasons.length === 0 ? "ready" : "not_ready",
      readinessReasons: Object.freeze(reasons),
      launchScope: scope,
      supervisor,
      activeAlerts: alerts,
    });
  };

  const metrics = (): WatcherOperationsMetrics => {
    const observedAt = nowMs();
    if (observedAt < 0n) throw new Error("observability clock is invalid");
    const supervisor = input.supervisor.status();
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
    const source = sourceHealth(observedAt);
    const durableQueue = input.durableProofQueueStatus();
    if (
      !Number.isSafeInteger(durableQueue.queuedJobCount) ||
      durableQueue.queuedJobCount < 0 ||
      durableQueue.queuedJobCount !== supervisor.queuedJobCount ||
      (durableQueue.queuedJobCount === 0) !==
        (durableQueue.oldestQueuedAtMs === null)
    ) {
      throw new Error("durable proof queue status differs from supervisor");
    }
    const oldestQueuedAt =
      durableQueue.oldestQueuedAtMs === null
        ? null
        : natural(durableQueue.oldestQueuedAtMs, "oldest durable proof time");
    if (oldestQueuedAt !== null && oldestQueuedAt > observedAt) {
      throw new Error("oldest durable proof time is in the future");
    }
    const summarize = (values: readonly bigint[]) =>
      Object.freeze({
        sampleCount: values.length.toString(),
        p50: percentile(values, 50, 100),
        p95: percentile(values, 95, 100),
        maximum: percentile(values, 100, 100),
      });
    return Object.freeze({
      schemaVersion: WATCHER_OPERATIONS_OBSERVABILITY,
      observedAtMs: observedAt.toString(),
      queuedProofCount: durableQueue.queuedJobCount.toString(),
      oldestQueuedProofAgeMs:
        oldestQueuedAt === null
          ? null
          : (observedAt - oldestQueuedAt).toString(),
      verificationLatencyMs: summarize(verificationLatencies),
      daLatencyMs: summarize(daLatencies),
      deadlineHealth: supervisor.deadlineHealth,
      remainingSafeStartMs: supervisor.remainingSafeStartMs,
      proofSteps: Object.freeze(
        Object.fromEntries(
          Object.entries(proofSteps).map(([key, value]) => [
            key,
            value.toString(),
          ]),
        ),
      ) as WatcherOperationsMetrics["proofSteps"],
      unprocessedEventCount: unprocessed.length.toString(),
      oldestUnprocessedEventAgeMs:
        unprocessed.length === 0
          ? null
          : (() => {
              const oldest = unprocessed
                .map(({ inclusionAtMs }) =>
                  natural(inclusionAtMs, "event inclusion time"),
                )
                .reduce((left, right) => (right < left ? right : left));
              if (oldest > observedAt) {
                throw new Error("event inclusion time is in the future");
              }
              return (observedAt - oldest).toString();
            })(),
      l1Sources: Object.freeze({
        configured: latestL1Sources.size.toString(),
        fresh: source.fresh.toString(),
        stale: source.stale.toString(),
        disagreement: source.disagreement.toString(),
        maximumFreshnessAgeMs: source.maximumAge?.toString() ?? null,
      }),
      activeAlertCount: activeAlerts().length.toString(),
    });
  };

  const api: WatcherOperationsApi = Object.freeze({
    status,
    metrics,
    diagnostics: ({ kind, cursor = "0", limit = 50 }) => {
      if (
        ![
          "verification",
          "da_fetch",
          "proof_step",
          "event",
          "l1_source",
          "alert",
        ].includes(kind) ||
        !NATURAL.test(cursor) ||
        !Number.isSafeInteger(limit) ||
        limit < 1 ||
        limit > MAXIMUM_PAGE_SIZE
      ) {
        throw new Error("observability diagnostic page request is invalid");
      }
      const after = BigInt(cursor);
      const matching = diagnostics.filter(
        (record) => record.kind === kind && BigInt(record.sequence) > after,
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
  });

  const jsonResponse = (statusCode: number, value: unknown): Response =>
    new Response(JSON.stringify(value), {
      status: statusCode,
      headers: Object.freeze({
        "cache-control": "no-store",
        "content-type": "application/json; charset=utf-8",
        "x-content-type-options": "nosniff",
      }),
    });

  const handleHttpRequest = async (request: Request): Promise<Response> => {
    if (request.method !== "GET") {
      return new Response(null, {
        status: 405,
        headers: Object.freeze({ allow: "GET", "cache-control": "no-store" }),
      });
    }
    let url: URL;
    try {
      url = new URL(request.url);
    } catch {
      return jsonResponse(400, { error: "invalid_request" });
    }
    try {
      if (url.pathname === "/v1/status" && url.search === "") {
        return jsonResponse(200, api.status());
      }
      if (url.pathname === "/v1/metrics" && url.search === "") {
        return jsonResponse(200, api.metrics());
      }
      if (url.pathname === "/v1/diagnostics") {
        const keys = [...url.searchParams.keys()];
        if (
          keys.some(
            (key) => key !== "kind" && key !== "cursor" && key !== "limit",
          ) ||
          new Set(keys).size !== keys.length
        ) {
          throw new Error("invalid diagnostics query");
        }
        const kind = url.searchParams.get("kind");
        const cursor = url.searchParams.get("cursor") ?? undefined;
        const rawLimit = url.searchParams.get("limit");
        const limit = rawLimit === null ? undefined : Number(rawLimit);
        if (kind === null) throw new Error("diagnostic kind is required");
        return jsonResponse(
          200,
          api.diagnostics({
            kind: kind as WatcherOperationsDiagnosticKind,
            ...(cursor === undefined ? {} : { cursor }),
            ...(limit === undefined ? {} : { limit }),
          }),
        );
      }
      return jsonResponse(404, { error: "not_found" });
    } catch {
      return jsonResponse(400, { error: "invalid_request" });
    }
  };

  return Object.freeze({
    schemaVersion: WATCHER_OPERATIONS_OBSERVABILITY,
    api,
    sink,
    handleHttpRequest,
  });
};
