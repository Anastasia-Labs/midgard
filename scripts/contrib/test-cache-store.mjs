import { randomUUID } from "node:crypto";
import {
  lstatSync,
  mkdirSync,
  readdirSync,
  readFileSync,
  readlinkSync,
  renameSync,
  rmSync,
  statSync,
  utimesSync,
  writeFileSync,
} from "node:fs";
import { resolve, sep } from "node:path";
import { execFileSync } from "node:child_process";

import { atomicJson, sha256 } from "./files.mjs";
import { resetReachIndexes } from "./reached.mjs";
import { resourceDirectory } from "./resources.mjs";

// The store of `contrib test --cached` (test-cache.mjs): repository
// snapshots, recorded passes and their lookup, and the size, age and
// clearing of the whole. Every write is atomic, and a folder is removed by
// renaming it away first, so concurrent runs never see a torn file.

// v2: recording needs a clean Vitest exit, the key binds every variable and
// the runner's own code; every v1 record is dead.
export const TEST_CACHE_SCHEMA = "midgard-contrib-test-cache/v2";
// The run registry keeps a run's report (the evidence a reused pass points
// at) this long; time-dependent tests get no older passes either.
export const CACHE_MAX_AGE_MS = 7 * 24 * 3_600_000;
export const CACHE_MAX_BYTES = 512 * 1024 * 1024;
// Older passes cost a reach each; past these the file runs.
const SNAPSHOTS_CONSULTED = 4;
const PRUNE_INTERVAL_MS = 3_600_000;

/** Where the cache lives: owner-only, beside the run registry. */
export const cachePath = () => resolve(resourceDirectory(), "test-cache");

/** The cache directory, created when missing. */
export const cacheDirectory = () => {
  const directory = cachePath();
  mkdirSync(directory, { recursive: true, mode: 0o700 });
  return directory;
};

export const readJson = (path) => {
  try {
    return JSON.parse(readFileSync(path, "utf8"));
  } catch {
    // Missing, pruned by another run, or torn: the same as absent.
    return undefined;
  }
};

/**
 * Write a JSON file atomically (a reader sees the old file or the new one),
 * retrying when another run removed its folder meanwhile (a forgotten file,
 * a clear). Returns whether it was written: a cache that cannot write only
 * reuses less.
 */
export const store = (path, value) => {
  for (let attempt = 0; attempt < 3; attempt += 1)
    try {
      atomicJson(path, value);
      return true;
    } catch (error) {
      if (error.code !== "ENOENT") throw error;
    }
  return false;
};

export const byPath = ([left], [right]) =>
  left < right ? -1 : left > right ? 1 : 0;

// --- repository snapshots ---------------------------------------------------

/** A file's metadata as `touchedSince` compares it, or undefined if absent. */
const fileStamp = (absolute) => {
  let stats;
  try {
    stats = lstatSync(absolute, { bigint: true });
  } catch {
    return undefined;
  }
  return stampOf(stats);
};
const stampOf = ({ ino, size, mtimeNs, ctimeNs }) =>
  `${ino}:${size}:${mtimeNs}:${ctimeNs}`;

/**
 * The digest of every repository file (tracked, or untracked and not
 * ignored), plus `extra` digests by repository path (ignored files inside
 * source directories, which builds and suites can read). `id` names the
 * whole. Given `stamps` (a Map), it also stores there each file's metadata
 * by repository path (see `touchedSince`), read before its content (an
 * `extra` file's is read before this call: a change since then and left in
 * place changes the digest at the end, one undone changes the times).
 */
export const repositorySnapshot = (root, extra = {}, stamps = undefined) => {
  const files = { ...extra };
  if (stamps)
    for (const path of Object.keys(extra)) {
      const stamp = fileStamp(resolve(root, path));
      if (stamp) stamps.set(path, stamp);
    }
  const listed = execFileSync(
    "git",
    ["ls-files", "-z", "--cached", "--others", "--exclude-standard"],
    { cwd: root, encoding: "utf8", maxBuffer: 256 * 1024 * 1024 },
  ).split("\0");
  for (const path of new Set(listed)) {
    if (!path || path in files) continue;
    const absolute = resolve(root, path);
    let stats;
    try {
      stats = lstatSync(absolute, { bigint: true });
    } catch {
      continue; // deleted from the work tree, still in the index
    }
    if (stats.isSymbolicLink())
      files[path] = `symlink:${readlinkSync(absolute)}`;
    else if (stats.isFile()) files[path] = sha256(readFileSync(absolute));
    else continue;
    stamps?.set(path, stampOf(stats));
  }
  const sorted = Object.fromEntries(Object.entries(files).sort(byPath));
  return { id: sha256(JSON.stringify(sorted)), files: sorted };
};

/** Repository paths added, removed or changed between two snapshots. */
export const changedSince = (before, after) =>
  [...new Set([...Object.keys(before.files), ...Object.keys(after.files)])]
    .filter((path) => before.files[path] !== after.files[path])
    .sort();

/**
 * The snapshot's files whose metadata is not what `stamps` holds (filled by
 * `repositorySnapshot` when it read them): removed, never stamped, or with
 * another inode, size, or modification or change time at nanosecond
 * precision. An edit undone before the end of a run (a mutation check)
 * leaves the same digest, but not the same times.
 *
 * Each file is compared with its own earlier metadata, never ordered against
 * a clock reading taken at the start: the wall clock can be stepped back
 * while a run runs (WSL2 steps it by about half a second every half minute),
 * so a file written before such a reading can carry a later time, and one
 * written after it an earlier one. This assumes any change to a file moves
 * its change or modification time, at nanosecond precision, away from a
 * value that was read (true of ext4 with multigrain timestamps, and APFS).
 */
export const touchedSince = (root, snapshot, stamps) =>
  Object.keys(snapshot.files).filter((path) => {
    const stamp = stamps.get(path);
    return !stamp || fileStamp(resolve(root, path)) !== stamp;
  });

const snapshotPath = (directory, id) =>
  resolve(directory, "snapshots", `${id}.json`);
export const storeSnapshot = (directory, snapshot) => {
  const path = snapshotPath(directory, snapshot.id);
  try {
    const now = new Date();
    utimesSync(path, now, now); // still in use: prune it last
  } catch {
    store(path, snapshot);
  }
};
export const loadSnapshot = (directory, id) => {
  const snapshot = readJson(snapshotPath(directory, id));
  return snapshot?.id === id ? snapshot : undefined;
};

// --- recorded passes ---------------------------------------------------------

const fileFolder = (directory, runKey, file) =>
  resolve(directory, "entries", runKey, sha256(file).slice(0, 40));

const entriesOf = (directory, runKey, file, now) => {
  const folder = fileFolder(directory, runKey, file);
  let names;
  try {
    names = readdirSync(folder);
  } catch {
    return [];
  }
  return names
    .filter((name) => name.endsWith(".json"))
    .map((name) => readJson(resolve(folder, name)))
    .filter(
      (entry) =>
        entry?.schema === TEST_CACHE_SCHEMA &&
        entry.runKey === runKey &&
        entry.file === file &&
        now - Date.parse(entry.createdAt) < CACHE_MAX_AGE_MS,
    );
};

/** Whether a pass's report is still there, unchanged, and says it passed. */
const evidenceChecker = () => {
  const reports = new Map();
  return (entry) => {
    const { path, sha256: digest } = entry.evidence?.report ?? {};
    if (!path) return false;
    if (!reports.has(path)) {
      let report;
      try {
        const data = readFileSync(path);
        report = sha256(data) === digest ? JSON.parse(data) : undefined;
      } catch {
        report = undefined;
      }
      reports.set(path, report);
    }
    const report = reports.get(path);
    const suite = report?.testResults?.find(
      (result) => result.name === resolve(entry.evidence.cwd, entry.file),
    );
    return Boolean(suite && filePassed(suite));
  };
};

/** A Vitest JSON suite that ran every one of its tests, and all passed. */
export const filePassed = (suite) =>
  suite.status === "passed" &&
  Array.isArray(suite.assertionResults) &&
  suite.assertionResults.length > 0 &&
  suite.assertionResults.every((assertion) => assertion.status === "passed");

/**
 * Split package-relative test files into reused passes and files to run.
 * `reach(changed)` answers which files a change reaches (reachThroughGraph).
 * Returns `{ reused: Map(file -> entry), run: Map(file -> why) }`.
 */
export const findReusable = async ({
  directory,
  runKey,
  files,
  snapshot,
  reach,
  uncacheable = new Map(),
  now = Date.now(),
}) => {
  resetReachIndexes();
  const reused = new Map();
  const why = new Map();
  const candidates = new Map();
  for (const file of files) {
    if (uncacheable.has(file)) {
      why.set(file, `always runs: ${uncacheable.get(file)}`);
      continue;
    }
    const entries = entriesOf(directory, runKey, file, now);
    if (!entries.length) why.set(file, "no recorded pass under this run key");
    for (const entry of entries) {
      if (!candidates.has(entry.snapshot)) candidates.set(entry.snapshot, []);
      candidates.get(entry.snapshot).push(entry);
    }
  }
  const newest = (entries) =>
    Math.max(...entries.map((entry) => Date.parse(entry.createdAt)));
  const order = [...candidates].sort(
    ([, left], [, right]) => newest(right) - newest(left),
  );
  const evidence = evidenceChecker();
  let consulted = 0;
  for (const [id, entries] of order) {
    const pending = entries.filter((entry) => !reused.has(entry.file));
    if (!pending.length) continue;
    if (consulted >= SNAPSHOTS_CONSULTED) {
      for (const entry of pending)
        if (!why.has(entry.file))
          why.set(entry.file, "its recorded passes are older than those tried");
      continue;
    }
    consulted += 1;
    const before = loadSnapshot(directory, id);
    if (!before) {
      for (const entry of pending)
        why.set(entry.file, "the snapshot of its recorded pass was pruned");
      continue;
    }
    const changed = changedSince(before, snapshot);
    const reached = changed.length ? await reach(changed) : undefined;
    for (const entry of pending) {
      if (!evidence(entry)) {
        why.set(entry.file, "the report of its recorded pass is gone");
        continue;
      }
      if (reached?.whole) {
        why.set(
          entry.file,
          `${changed.length} file(s) changed since its pass, and the change reaches the whole package (${reached.why[0]})`,
        );
        continue;
      }
      if (reached && reached.known && !reached.known.has(entry.file)) {
        why.set(entry.file, "the import graph does not hold it");
        continue;
      }
      if (reached?.files.has(entry.file)) {
        why.set(
          entry.file,
          `${changed.length} file(s) changed since its pass, and the change reaches it`,
        );
        continue;
      }
      reused.set(entry.file, { ...entry, changedSince: changed.length });
      why.delete(entry.file);
    }
  }
  const run = new Map(
    files
      .filter((file) => !reused.has(file))
      .map((file) => [file, why.get(file) ?? "no recorded pass"]),
  );
  return { reused, run };
};

/** Record full passes of test files against the snapshot they ran with. */
export const recordPasses = ({
  directory,
  runKey,
  packageName,
  snapshot,
  passes,
  evidence,
}) => {
  if (!passes.length) return 0;
  storeSnapshot(directory, snapshot);
  const createdAt = new Date().toISOString();
  let recorded = 0;
  for (const { file, assertions, durationMs } of passes)
    if (
      store(
        resolve(fileFolder(directory, runKey, file), `${snapshot.id}.json`),
        {
          schema: TEST_CACHE_SCHEMA,
          package: packageName,
          file,
          runKey,
          snapshot: snapshot.id,
          createdAt,
          assertions,
          durationMs,
          evidence,
        },
      )
    )
      recorded += 1;
  return recorded;
};

/**
 * Remove a directory other runs may be writing into: renamed away first (at
 * once, so no reader or writer sees it half removed), then deleted. A
 * writer racing the deletion leaves a file the age limit prunes.
 */
const discard = (path) => {
  const gone = `${path}.${randomUUID()}.gone`;
  try {
    renameSync(path, gone);
  } catch (error) {
    if (error.code === "ENOENT") return;
    throw error;
  }
  try {
    rmSync(gone, { recursive: true, force: true, maxRetries: 3 });
  } catch {
    // Left for the age limit.
  }
};

/**
 * Forget every recorded pass of `files` (package-relative), under every run
 * key: a failure under any environment, flags or tier casts doubt on the
 * passes recorded under the others. A file of the same relative path in
 * another package is forgotten too, which only costs a rerun.
 */
export const forgetFiles = ({ directory, files }) => {
  let keys;
  try {
    keys = readdirSync(resolve(directory, "entries"));
  } catch {
    return; // nothing recorded
  }
  for (const runKey of keys)
    for (const file of files) discard(fileFolder(directory, runKey, file));
};

// --- size, pruning, clearing --------------------------------------------------

const walk = (directory) => {
  const found = [];
  const visit = (path) => {
    let entries;
    try {
      entries = readdirSync(path, { withFileTypes: true });
    } catch {
      return;
    }
    for (const entry of entries) {
      const child = resolve(path, entry.name);
      if (entry.isDirectory()) visit(child);
      else
        try {
          const stats = statSync(child);
          found.push({
            path: child,
            bytes: stats.size,
            mtimeMs: stats.mtimeMs,
          });
        } catch {
          // Removed by another run meanwhile.
        }
    }
  };
  visit(directory);
  return found;
};

/** Counts and size of the cache. */
export const cacheStats = (directory = cacheDirectory()) => {
  const files = walk(directory);
  const under = (name) =>
    files.filter(({ path }) =>
      path.startsWith(`${resolve(directory, name)}${sep}`),
    );
  return {
    schema: TEST_CACHE_SCHEMA,
    directory,
    passes: under("entries").length,
    snapshots: under("snapshots").length,
    graphs: under("graphs").filter(({ path }) => !path.endsWith("latest.json"))
      .length,
    bytes: files.reduce((sum, { bytes }) => sum + bytes, 0),
  };
};

/** Drop the whole cache. */
export const clearCache = (directory = cacheDirectory()) => {
  const before = cacheStats(directory);
  discard(directory);
  return { ...before, cleared: true };
};

/**
 * Remove what has expired, then the least recently used snapshots and
 * graphs until the cache fits `maxBytes`. At most once per interval.
 */
export const pruneCache = (
  directory,
  {
    now = Date.now(),
    maxBytes = CACHE_MAX_BYTES,
    maxAgeMs = CACHE_MAX_AGE_MS,
    force = false,
  } = {},
) => {
  const marker = resolve(directory, "pruned");
  try {
    if (!force && now - statSync(marker).mtimeMs < PRUNE_INTERVAL_MS)
      return { pruned: 0 };
  } catch {
    // Never pruned.
  }
  mkdirSync(directory, { recursive: true, mode: 0o700 });
  writeFileSync(marker, "");
  let files = walk(directory).filter(({ path }) => path !== marker);
  let pruned = 0;
  const remove = ({ path }) => {
    rmSync(path, { force: true });
    pruned += 1;
  };
  for (const file of files.filter(({ mtimeMs }) => now - mtimeMs > maxAgeMs))
    remove(file);
  files = files.filter(({ mtimeMs }) => now - mtimeMs <= maxAgeMs);
  let bytes = files.reduce((sum, { bytes: size }) => sum + size, 0);
  for (const file of files.sort(
    (left, right) => left.mtimeMs - right.mtimeMs,
  )) {
    if (bytes <= maxBytes) break;
    remove(file);
    bytes -= file.bytes;
  }
  return { pruned, bytes };
};
