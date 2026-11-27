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

/**
 * One `proof_family_coverage_gap` subject per state-queue header. Unlike the
 * per-header codes below, an unprovable outcome does not settle it: the
 * fault decision bridge raises it on that outcome and clears it itself.
 */
export const watcherProofFamilyCoverageGapAlertSubject = (
  headerHash: string,
): string =>
  createHash("sha256")
    .update("midgard-watcher-proof-family-coverage-gap-alert-v1\u0000")
    .update(headerHash.toLowerCase())
    .digest("hex");

/** One `unproven_fault_merged` subject per state-queue header; it is never
 * cleared (fault-decision-bridge.unproven-merges.ts). */
export const watcherUnprovenFaultMergedAlertSubject = (
  headerHash: string,
): string =>
  createHash("sha256")
    .update("midgard-watcher-unproven-fault-merged-alert-v1\u0000")
    .update(headerHash.toLowerCase())
    .digest("hex");

/**
 * One `fault_detector_failed` subject per state-queue header, whatever
 * detector failed on it, so it stands until the header is decided or leaves
 * the queue (fault-proofs/fault-detector-isolation.ts).
 */
export const watcherFaultDetectorFailedAlertSubject = (
  headerHash: string,
): string =>
  createHash("sha256")
    .update("midgard-watcher-fault-detector-failed-alert-v2\u0000")
    .update(headerHash.toLowerCase())
    .digest("hex");

/**
 * Codes kept one subject per header, cleared by the fault decision bridge
 * once the header leaves the observed queue (fault-proofs/per-header-alert.ts),
 * and by the header's outcome: here, except `da_fetch_failure`, which the
 * outcome settles through the DA fetchers' binding (`settle`), as it stands
 * while some fetcher still names the header.
 */
const PER_HEADER_CODES = Object.freeze([
  ["da_fetch_failure", watcherDaFetchAlertSubject],
  [
    "validation_trace_replay_stuck",
    watcherValidationTraceReplayStuckAlertSubject,
  ],
  ["fault_detector_failed", watcherFaultDetectorFailedAlertSubject],
] as const);

const isPerHeaderCode = (code: WatcherAlertCode): boolean =>
  PER_HEADER_CODES.some(([perHeader]) => perHeader === code);

/**
 * Codes whose repeated raise of a subject already active is not recorded
 * again: a detector failing on a header on every pass until the header is
 * decided adds one record, not one per pass.
 */
const RAISED_ONCE_CODES: ReadonlySet<WatcherAlertCode> = new Set([
  "fault_detector_failed",
]);

/** A failed DA fetch stops holding readiness once this old, by default. */
export const WATCHER_DA_FETCH_ALERT_MAXIMUM_AGE_MS = 180_000;

/** Distinct subjects kept of each per-header code; the oldest go first. */
const MAXIMUM_PER_HEADER_ALERTS = MAXIMUM_PAGE_SIZE * 10;

/**
 * Codes whose inactive record is read back to suppress repeated diagnostics.
 * Stuck replays and failed detectors come and go per header; the book or
 * their reporters deduplicate.
 */
const DEDUPLICATED_CODES: ReadonlySet<WatcherAlertCode> = new Set(
  [...WATCHER_INFORMATIONAL_ALERT_CODES].filter(
    (code) =>
      code !== "validation_trace_replay_stuck" &&
      code !== "fault_detector_failed",
  ),
);

/** Outcomes that settle a header: its per-header alerts are no longer
 * outstanding for it. */
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

export const watcherOutcomeSettlesHeader = (
  outcome: WatcherVerificationDiagnostic["outcome"],
): boolean => SETTLED_OUTCOMES.has(outcome);

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
    const key = `${value.code}:${value.subjectDigest}`;
    const standing = latest.get(key)?.active === true;
    // A per-header clear of a subject not active records nothing: a header
    // that leaves the queue after its outcome cleared its alert, or a fetch
    // that succeeds with no failure standing, adds no clear.
    if (
      (RAISED_ONCE_CODES.has(value.code) && standing && value.active) ||
      (!value.active && !standing && isPerHeaderCode(value.code))
    )
      return;
    const record = input.append({ kind: "alert", ...value });
    latest.delete(key);
    if (value.active || DEDUPLICATED_CODES.has(value.code))
      latest.set(key, record);
    if (!isPerHeaderCode(value.code)) return;
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
     * Clears a header's stuck replay and failed detector once the header has
     * an outcome; its DA fetch failure is settled by its fetchers' binding.
     */
    settleHeader: (
      headerHash: string | undefined,
      outcome: WatcherVerificationDiagnostic["outcome"],
      observedAtMs: string,
    ): void => {
      if (headerHash === undefined || !watcherOutcomeSettlesHeader(outcome))
        return;
      for (const [code, subjectOf] of PER_HEADER_CODES) {
        if (code === "da_fetch_failure") continue;
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
