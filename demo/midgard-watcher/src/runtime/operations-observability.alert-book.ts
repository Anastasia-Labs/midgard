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

/** A failed DA fetch stops holding readiness once this old, by default. */
export const WATCHER_DA_FETCH_ALERT_MAXIMUM_AGE_MS = 180_000;

/** Distinct `da_fetch_failure` subjects kept; the oldest go first. */
const MAXIMUM_DA_FETCH_ALERTS = MAXIMUM_PAGE_SIZE * 10;

/** Codes whose inactive record is read back to suppress repeated diagnostics. */
const DEDUPLICATED_CODES: ReadonlySet<WatcherAlertCode> =
  WATCHER_INFORMATIONAL_ALERT_CODES;

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
    if (value.code !== "da_fetch_failure") return;
    const daFetch = [...latest.keys()].filter((entry) =>
      entry.startsWith("da_fetch_failure:"),
    );
    // Insertion order is update order: the first entries are the oldest.
    for (const stale of daFetch.slice(
      0,
      Math.max(0, daFetch.length - MAXIMUM_DA_FETCH_ALERTS),
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
    /** Clears a header's DA fetch failure once the header has an outcome. */
    settleHeader: (
      headerHash: string | undefined,
      outcome: WatcherVerificationDiagnostic["outcome"],
      observedAtMs: string,
    ): void => {
      if (headerHash === undefined || !SETTLED_OUTCOMES.has(outcome)) return;
      const subjectDigest = watcherDaFetchAlertSubject(headerHash);
      if (latest.get(`da_fetch_failure:${subjectDigest}`)?.active !== true)
        return;
      set({
        code: "da_fetch_failure",
        subjectDigest,
        active: false,
        observedAtMs,
      });
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
