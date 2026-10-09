// `ci-status.mjs --wait`: look again until the head's runs settle, and stop
// at a deadline. It replaces hand-written `until gh ...; do sleep; done`
// loops, which wait forever on a run that never starts and read "the loop
// ended" as "CI passed".
//
// It keeps looking while a run on the head is in progress, and, for the
// first MISSING_GRACE_MS, while an expected workflow has no run yet (GitHub
// creates runs some seconds after a push). It stops at once when a run fails,
// when a missing run cannot appear (a conflicting pull request), and at the
// deadline. The exit code is the usual verdict of the last look; a deadline
// reached with runs in progress is exit 4, as without --wait.
//
// A failed look before any succeeded is exit 3 straight away: gh is missing
// or not authenticated, and looking again cannot help. After one succeeded,
// up to MAX_QUERY_FAILURES failed looks in a row are taken as a network blip
// and retried at the next interval; one more is exit 3.

import { collectStatus, verdict } from "./ci-status.collect-status.mjs";
import {
  DEFAULT_REPO,
  ghRunner,
  QueryError,
} from "./ci-status.parse-workflow.mjs";

export const POLL_INTERVAL_MS = 60_000;
export const MISSING_GRACE_MS = 5 * 60_000;
export const MAX_QUERY_FAILURES = 3;
export const DEFAULT_TIMEOUT_MINUTES = 60;
// GitHub stops a job after six hours.
export const MAX_TIMEOUT_MINUTES = 360;

const sleepSync = (ms) =>
  Atomics.wait(new Int32Array(new SharedArrayBuffer(4)), 0, 0, ms);

const minutes = (ms) =>
  `${String(Math.floor(ms / 60_000))}m${String(Math.floor((ms % 60_000) / 1000)).padStart(2, "0")}s`;

const tally = (report) => {
  const counts = new Map();
  for (const row of report.workflows)
    counts.set(row.state, (counts.get(row.state) ?? 0) + 1);
  return [...counts]
    .map(([state, count]) => `${String(count)} ${state}`)
    .join(", ");
};

// Whether another look can change the verdict.
const unsettled = (report, elapsed) => {
  const states = new Set(report.workflows.map((row) => row.state));
  if (states.has("failed")) return false;
  if (states.has("pending")) return true;
  return (
    states.has("missing") &&
    elapsed < MISSING_GRACE_MS &&
    report.pr?.mergeable !== "CONFLICTING"
  );
};

/**
 * Collect the status until it settles or `timeoutMinutes` pass. Returns the
 * last report with a `wait` record, and its exit code. Throws QueryError
 * when GitHub cannot be read (see above).
 */
export const waitForStatus = (
  target,
  {
    run = ghRunner,
    repo = DEFAULT_REPO,
    timeoutMinutes = DEFAULT_TIMEOUT_MINUTES,
    intervalMs = POLL_INTERVAL_MS,
    sleep = sleepSync,
    now = Date.now,
    progress = () => {},
  } = {},
) => {
  const started = now();
  const deadline = started + timeoutMinutes * 60_000;
  let report;
  let looks = 0;
  let failures = 0;
  const heads = [];
  for (;;) {
    try {
      report = collectStatus(target, { run, repo });
      looks += 1;
      failures = 0;
    } catch (error) {
      if (!(error instanceof QueryError) || report === undefined) throw error;
      failures += 1;
      if (failures > MAX_QUERY_FAILURES) throw error;
      progress(
        `could not query GitHub (${String(failures)} of ${String(MAX_QUERY_FAILURES)} retries): ${error.message}`,
      );
    }
    if (heads.at(-1) !== report.headSha) heads.push(report.headSha);
    const elapsed = now() - started;
    const settled = failures === 0 && !unsettled(report, elapsed);
    if (settled || now() + intervalMs > deadline) {
      const timedOut = !settled;
      const notes = [...report.notes];
      if (heads.length > 1)
        notes.push(
          `the head moved during the wait (${heads.map((sha) => sha.slice(0, 10)).join(" -> ")}); this reports the last`,
        );
      if (failures > 0)
        notes.push(
          `the last ${String(failures)} look(s) could not query GitHub; this is the look before them`,
        );
      if (timedOut)
        notes.push(
          `stopped waiting after ${minutes(elapsed)} (--timeout ${String(timeoutMinutes)}); the runs below had not settled`,
        );
      const final = {
        ...report,
        notes,
        wait: { looks, elapsedSeconds: Math.round(elapsed / 1000), timedOut },
      };
      return { report: final, code: verdict(final) };
    }
    if (failures === 0)
      progress(
        `${minutes(elapsed)}: ${tally(report)} on ${report.headSha.slice(0, 10)}; looking again in ${String(Math.round(intervalMs / 1000))}s`,
      );
    sleep(intervalMs);
  }
};
