import { createHash } from "node:crypto";

import { hash32 } from "./operations-observability.percentile.js";
import {
  MAXIMUM_PAGE_SIZE,
  natural,
  WATCHER_ALERT_CODES,
  WATCHER_INFORMATIONAL_ALERT_CODES,
  type WatcherAlertCode,
  type WatcherAlertDiagnostic,
  type WatcherOperationsSink,
  type WatcherVerificationDiagnostic,
} from "./operations-observability.watcher-operations-metrics.js";

/**
 * One `da_fetch_failure` subject per state-queue header, whatever request
 * failed for it, so a later outcome for the header can clear it.
 */
export const watcherDaFetchAlertSubject = (headerHash: string): string =>
  createHash("sha256")
    .update("midgard-watcher-da-fetch-alert-v1\u0000")
    .update(headerHash.toLowerCase())
    .digest("hex");

/**
 * One `validation_trace_replay_stuck` subject per state-queue header, whatever
 * event of it is stuck, so the header's outcome can clear it.
 */
export const watcherValidationTraceReplayStuckAlertSubject = (
  headerHash: string,
): string =>
  createHash("sha256")
    .update("midgard-watcher-validation-trace-replay-stuck-alert-v1\u0000")
    .update(headerHash.toLowerCase())
    .digest("hex");

/** Codes kept one subject per header, cleared by the header's outcome. */
const PER_HEADER_CODES = Object.freeze([
  ["da_fetch_failure", watcherDaFetchAlertSubject],
  [
    "validation_trace_replay_stuck",
    watcherValidationTraceReplayStuckAlertSubject,
  ],
] as const);

/** A failed DA fetch stops holding readiness once this old, by default. */
export const WATCHER_DA_FETCH_ALERT_MAXIMUM_AGE_MS = 180_000;

/** Distinct subjects kept of each per-header code; the oldest go first. */
const MAXIMUM_PER_HEADER_ALERTS = MAXIMUM_PAGE_SIZE * 10;

/**
 * Codes whose inactive record is read back to suppress repeated diagnostics.
 * Stuck replays come and go per header, and their reporter deduplicates.
 */
const DEDUPLICATED_CODES: ReadonlySet<WatcherAlertCode> = new Set(
  [...WATCHER_INFORMATIONAL_ALERT_CODES].filter(
    (code) => code !== "validation_trace_replay_stuck",
  ),
);

/** Outcomes after which a header's DA fetch is no longer outstanding. */
const SETTLED_OUTCOMES: ReadonlySet<WatcherVerificationDiagnostic["outcome"]> =
  new Set<WatcherVerificationDiagnostic["outcome"]>([
    "verified",
    "unprovable_gap",
    "fault_detected",
    "fault_proven",
    "removed_or_resolved",
    "unverified_merged",
    "unverified_removed",
    "unverified_past_horizon",
  ]);

/**
 * The latest record per alert subject. An inactive alert is dropped unless
 * its code is deduplicated against it, so subjects that come and go (one per
 * header) do not accumulate for the life of the process.
 */
export const createWatcherAlertBook = (input: {
  readonly append: (
    record: Omit<WatcherAlertDiagnostic, "sequence">,
  ) => WatcherAlertDiagnostic;
  readonly daFetchMaximumAgeMs: number;
}) => {
  if (
    !Number.isSafeInteger(input.daFetchMaximumAgeMs) ||
    input.daFetchMaximumAgeMs < 1
  )
    throw new Error("observability DA fetch alert age bound is invalid");
  const latest = new Map<string, WatcherAlertDiagnostic>();
  const set: WatcherOperationsSink["setAlert"] = (value) => {
    if (!WATCHER_ALERT_CODES.includes(value.code)) {
      throw new Error("operational alert code is invalid");
    }
    hash32(value.subjectDigest, "operational alert subject digest");
    natural(value.observedAtMs, "operational alert observation time");
    const record = input.append({ kind: "alert", ...value });
    const key = `${value.code}:${value.subjectDigest}`;
    latest.delete(key);
    if (value.active || DEDUPLICATED_CODES.has(value.code))
      latest.set(key, record);
    if (!PER_HEADER_CODES.some(([code]) => code === value.code)) return;
    const subjects = [...latest.keys()].filter((entry) =>
      entry.startsWith(`${value.code}:`),
    );
    // Insertion order is update order: the first entries are the oldest.
    for (const stale of subjects.slice(
      0,
      Math.max(0, subjects.length - MAXIMUM_PER_HEADER_ALERTS),
    ))
      latest.delete(stale);
  };
  return Object.freeze({
    set,
    /** The subject's latest state, or `undefined` when none is kept. */
    state: (
      code: WatcherAlertCode,
      subjectDigest: string,
    ): boolean | undefined => latest.get(`${code}:${subjectDigest}`)?.active,
    /**
     * Clears a header's DA fetch failure and stuck replay once the header has
     * an outcome.
     */
    settleHeader: (
      headerHash: string | undefined,
      outcome: WatcherVerificationDiagnostic["outcome"],
      observedAtMs: string,
    ): void => {
      if (headerHash === undefined || !SETTLED_OUTCOMES.has(outcome)) return;
      for (const [code, subjectOf] of PER_HEADER_CODES) {
        const subjectDigest = subjectOf(headerHash);
        if (latest.get(`${code}:${subjectDigest}`)?.active === true)
          set({ code, subjectDigest, active: false, observedAtMs });
      }
    },
    active: () =>
      Object.freeze(
        [...latest.values()]
          .filter(({ active }) => active)
          .sort(
            (left, right) =>
              left.code.localeCompare(right.code) ||
              left.subjectDigest.localeCompare(right.subjectDigest),
          )
          .map(({ code, subjectDigest, observedAtMs }) =>
            Object.freeze({ code, subjectDigest, observedAtMs }),
          ),
      ),
    /**
     * Whether an active alert holds readiness. Informational codes never do;
     * a DA fetch failure does only while it is recent, so one that is never
     * repeated (its header merged, was removed or rolled back) stops holding
     * readiness on its own. A fetch that keeps failing keeps renewing it.
     */
    holdsReadiness: (nowMs: bigint): boolean =>
      [...latest.values()].some(
        ({ active, code, observedAtMs }) =>
          active &&
          !WATCHER_INFORMATIONAL_ALERT_CODES.has(code) &&
          (code !== "da_fetch_failure" ||
            nowMs - BigInt(observedAtMs) <= BigInt(input.daFetchMaximumAgeMs)),
      ),
  });
};
