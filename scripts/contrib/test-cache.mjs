import { existsSync } from "node:fs";
import { relative } from "node:path";

import { inputIdentity } from "./files.mjs";
import {
  cachedGraph,
  reachThroughGraph,
  runIdentity,
  uncacheableReason,
  unrecordableReason,
} from "./test-cache-key.mjs";
import {
  cacheDirectory,
  changedSince,
  filePassed,
  findReusable,
  forgetFiles,
  pruneCache,
  recordPasses,
  repositorySnapshot,
  TEST_CACHE_SCHEMA,
  touchedSince,
} from "./test-cache-store.mjs";

export {
  cachedGraph,
  incrementalChanges,
  keyedEnvironment,
  reachThroughGraph,
  runIdentity,
  runnerIdentity,
  runnerSources,
  uncacheableReason,
  unrecordableReason,
} from "./test-cache-key.mjs";
export {
  CACHE_MAX_AGE_MS,
  CACHE_MAX_BYTES,
  cacheDirectory,
  cachePath,
  cacheStats,
  changedSince,
  clearCache,
  filePassed,
  findReusable,
  forgetFiles,
  pruneCache,
  recordPasses,
  repositorySnapshot,
  TEST_CACHE_SCHEMA,
} from "./test-cache-store.mjs";

// The opt-in test result cache of `contrib test --cached`.
//
// A test file's recorded pass is reused when nothing it depends on changed
// since. "Depends on" is exactly what `--related` uses: the recorded pass
// keeps a snapshot of every repository file's digest, and the files that
// differ between that snapshot and the checkout now go to `reachedTests`
// (reached.mjs) with the current import graph. A file the difference does
// not reach, in a run whose other inputs are identical (the run key below),
// is reused; anything the reach cannot decide widens to the whole package,
// so nothing is reused. Asking the reach about the difference is the same
// test as hashing each file's forward closure under the reach's rules, but
// with one implementation of those rules, so the cache and `--related`
// cannot drift apart.
//
// The run key binds everything outside the repository files: the package,
// its Vitest flags and environment, the tier and seed, every environment
// variable but a short list of per-session ones, Node, the installed
// packages, the compiled artifacts and blueprint by content, the test
// Postgres version for packages that use it, the runner's own scripts by
// content, and this schema.
//
// A pass is recorded only from a run Vitest exited 0 on, whose report says
// it succeeded, and during which no repository file was written; a run with
// any failure records nothing. Any run, cached or not, that fails forgets
// the recorded passes of the files it failed (all it ran when no file is to
// blame).
//
// The run key, uncacheable files and the graph cache live in
// test-cache-key.mjs; snapshots, recorded passes and the store's upkeep in
// test-cache-store.mjs.

// --- one cached run -------------------------------------------------------------

/**
 * The collected files Vitest's file filters select: a filter selects every
 * file whose path relative to the package contains it, ignoring case. Run
 * the files to run as filters, and this is exactly what runs.
 */
export const selectedByFilters = (cwd, collected, filters) => {
  const lowered = filters.map((filter) => filter.toLocaleLowerCase());
  return collected.filter((file) => {
    const path = relative(cwd, file).toLocaleLowerCase();
    return lowered.some((filter) => path.includes(filter));
  });
};

/**
 * Decide which of a run's collected test files reuse a recorded pass.
 * `selected` holds the collected files (absolute), `before` the package's
 * input identity (its ignored source files join the snapshot). Returns the
 * context `recordCachedRun` needs, `run` (the absolute files to run, widened
 * to what their filters select) and `reused`.
 */
export const planCachedRun = async ({
  root,
  pkg,
  command,
  cwd,
  env,
  tier,
  seed,
  sourceOnly,
  artifacts,
  postgres,
  before,
  selected,
  evidence,
  runDirectory,
  signal,
  probe,
  directory = cacheDirectory(),
}) => {
  const started = performance.now();
  pruneCache(directory);
  // Each file's metadata, read before the snapshot reads it: a file touched
  // after this was not necessarily what the snapshot saw, nor what the run
  // ran.
  const stamps = new Map();
  const snapshot = repositorySnapshot(root, before.files, stamps);
  const identity = runIdentity({
    root,
    pkg,
    command,
    tier,
    seed,
    sourceOnly,
    env,
    artifacts,
    postgres,
  });
  const files = selected.map((file) => relative(cwd, file));
  const unrecordable = unrecordableReason(root, pkg);
  const uncacheable = new Map(
    files.flatMap((file) => {
      const reason =
        unrecordable ?? uncacheableReason(root, pkg, file, { evidence });
      return reason ? [[file, reason]] : [];
    }),
  );
  const graphs = cachedGraph({
    directory,
    root,
    pkg,
    command,
    env,
    snapshot,
    extra: () => inputIdentity(root, pkg.name).files,
    runDirectory,
    signal,
    probe,
  });
  const { reused, run } = await findReusable({
    directory,
    runKey: identity.sha256,
    files,
    snapshot,
    uncacheable,
    reach: reachThroughGraph({
      root,
      pkg,
      command,
      env,
      signal,
      runDirectory,
      graphs,
    }),
  });
  const misses = [...run.keys()];
  const runFiles = misses.length
    ? selectedByFilters(cwd, selected, misses)
    : [];
  // A reused file a miss's filter also selects runs again, and counts as run.
  for (const file of runFiles) reused.delete(relative(cwd, file));
  return {
    context: {
      directory,
      runKey: identity.sha256,
      identity,
      snapshot,
      stamps,
      uncacheable,
      unrecordable,
      graph: graphs.info,
      keyMs: Math.round(performance.now() - started - graphs.info.ms),
    },
    filters: misses,
    run: runFiles,
    why: run,
    reused,
  };
};

/**
 * Forget, under every run key, the recorded passes of the files a run (with
 * or without --cached) failed: those its report shows failing, or when
 * Vitest failed with none to blame (an error outside every test, a crash, a
 * stop), every file it ran. Returns how many files were forgotten.
 */
export const forgetFailedRun = ({ directory, cwd, report, exitCode, ran }) => {
  if (!existsSync(directory)) return 0;
  const failing = (report?.testResults ?? [])
    .filter(
      (suite) =>
        suite.status === "failed" ||
        (suite.assertionResults ?? []).some(
          (assertion) => assertion.status === "failed",
        ),
    )
    .map((suite) => relative(cwd, suite.name));
  const files =
    failing.length || exitCode === 0
      ? failing
      : ran.map((file) => relative(cwd, file));
  forgetFiles({ directory, files });
  return files.length;
};

/**
 * Record the full passes of a run when the run as a whole can vouch for
 * them: Vitest exited 0 and its report says it succeeded (an unhandled
 * error after a test passed fails the exit code alone), nothing it binds
 * changed or was touched while it ran, and it was not stopped. A run with
 * any failure records nothing: an error outside the tests cannot be pinned
 * on a file. Returns the receipt's `cache` field.
 */
export const recordCachedRun = ({
  plan,
  pkg,
  cwd,
  root,
  report,
  exitCode,
  reportEvidence,
  unsound,
  after,
  forgotten = 0,
}) => {
  const { context } = plan;
  const ran = new Set(plan.run.map((file) => relative(cwd, file)));
  const suites = (report?.testResults ?? []).filter((suite) =>
    ran.has(relative(cwd, suite.name)),
  );
  let recorded = 0;
  let why;
  if (ran.size === 0) why = undefined;
  else if (context.unrecordable) why = context.unrecordable;
  else if (unsound) why = unsound;
  else if (exitCode !== 0)
    why = `Vitest exited ${exitCode ?? "without a code"}: a test failed, or an error came from outside every test`;
  else if (!report) why = "the run has no report";
  else if (report.success !== true) why = "its report says the run failed";
  else {
    const now = repositorySnapshot(root, after.files);
    const touched = touchedSince(root, context.snapshot, context.stamps);
    if (now.id !== context.snapshot.id)
      why = `${changedSince(context.snapshot, now).length} repository file(s) changed while it ran`;
    else if (touched.length)
      why = `${touched.length} repository file(s) were written while it ran (${touched[0]}), even if their content is back`;
  }
  if (!why)
    recorded = recordPasses({
      directory: context.directory,
      runKey: context.runKey,
      packageName: pkg.name,
      snapshot: context.snapshot,
      passes: suites
        .filter(
          (suite) =>
            filePassed(suite) &&
            !context.uncacheable.has(relative(cwd, suite.name)),
        )
        .map((suite) => ({
          file: relative(cwd, suite.name),
          assertions: suite.assertionResults.length,
          durationMs: Math.round(
            Number(suite.endTime ?? 0) - Number(suite.startTime ?? 0),
          ),
        })),
      evidence: { ...reportEvidence, root, cwd },
    });
  return {
    schema: TEST_CACHE_SCHEMA,
    runKey: context.runKey,
    snapshot: context.snapshot.id,
    ran: plan.run.length,
    reused: [...plan.reused.values()]
      .map((entry) => ({
        file: entry.file,
        assertions: entry.assertions,
        changedSince: entry.changedSince,
        from: {
          receipt: entry.evidence.receipt,
          report: {
            path: entry.evidence.report.path,
            sha256: entry.evidence.report.sha256,
          },
          root: entry.evidence.root,
          snapshot: entry.snapshot,
          createdAt: entry.createdAt,
        },
      }))
      .sort((left, right) => (left.file < right.file ? -1 : 1)),
    runReasons: Object.fromEntries(plan.why),
    uncacheable: Object.fromEntries(context.uncacheable),
    recorded,
    ...(why ? { notRecorded: why } : {}),
    forgotten,
    graph: context.graph,
    keyMs: context.keyMs,
  };
};
