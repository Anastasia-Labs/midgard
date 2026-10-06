#!/usr/bin/env node

/**
 * Refreshes the per-file CI seconds table that `duration-shards.js` packs
 * shards by, and warns when the committed table has drifted from what a CI
 * run measured.
 *
 * One command, from the repository root, refreshes a package's committed
 * table from a finished Node CI run:
 *
 *   node demo/midgard-test-support/scripts/ci-file-durations.mjs \
 *     --package demo/midgard-fault-proofs \
 *     --table demo/midgard-fault-proofs/tests/support/ci-file-durations.json \
 *     --run <run id>
 *
 * `--run` downloads the run's `file-durations-<package>-<shard>` artifacts
 * (written by the `FileDurationsReporter` that `durationShards` adds when
 * `MIDGARD_FILE_DURATIONS_OUT` is set) with `gh`; for a run older than those
 * artifacts it reads the logs of the jobs named `<package>` or
 * `<package> (i/n)` instead. Records or logs can also be passed as files:
 *
 *   ci-file-durations.mjs --package <dir> --table <table.json> [--out <path>]
 *     [--run <id>] [--repo owner/name] [--overhead-seconds 5]
 *     [--project-shards <n>] [--warn-share 0.1] [--test-file <regex>]
 *     [<record.json | job log>...]
 *
 * The refreshed table is written to `--out` (default: `--table`, in place).
 * A measured file takes its measured seconds; a file of the committed table
 * the inputs did not measure (a shard that did not finish, say) keeps its
 * committed seconds; entries for files no longer under `--package` are
 * dropped. A test file is a file under `<package>/tests` that `--test-file`
 * matches (default: the extensions of Vitest's default include, so
 * midgard-node's `.test.mjs` files count). `defaultSeconds` (the weight of a file the table does not know) is
 * the median of the result; `forksPerShard`, `reservedSeconds` and `$comment`
 * are carried over from `--table`.
 *
 * Drift check: files the committed table does not know, and files it weighs
 * wrongly (off by more than half and by more than 10 s), are what make the
 * shard plan unbalanced. When their measured seconds exceed `--warn-share`
 * (default a tenth) of the run's measured seconds, the script prints a
 * warning (a GitHub annotation under Actions) naming the largest of them.
 * It never fails on drift: a stale table costs balance, never coverage.
 *
 * `--project-shards <n>` prints the projected seconds of each of `n` shards
 * under the refreshed table (list scheduling over `forksPerShard` forks,
 * longest first, plus the shard's reserved seconds).
 *
 * Log inputs: both Vitest console reporters are read. The default reporter
 * prints one line per file with the file's test time (`✓ project
 * tests/x.test.ts (9 tests) 1234ms`). The verbose reporter prints one line per
 * test and gives a duration only for tests above its slow threshold, so a
 * file's seconds are the larger of the summed test durations and the span
 * between its first and last reported test. Either way `--overhead-seconds`
 * is added per file for the fork start, import and collection a log does not
 * show. Only lines before a log's first `Test Files` summary count, so a later
 * step that reruns some files does not stretch their spans. Records already
 * include import and collection, so no overhead is added to them. A file
 * named in several inputs keeps its largest value.
 */

import { execFileSync } from "node:child_process";
import {
  existsSync,
  mkdtempSync,
  readdirSync,
  readFileSync,
  rmSync,
  writeFileSync,
} from "node:fs";
import { tmpdir } from "node:os";
import { basename, join, resolve } from "node:path";
import { parseArgs } from "node:util";

import {
  projectShardSeconds,
  readShardDurationTable,
} from "../duration-plan.js";

const { values, positionals } = parseArgs({
  allowPositionals: true,
  options: {
    package: { type: "string" },
    table: { type: "string" },
    out: { type: "string" },
    run: { type: "string" },
    repo: { type: "string", default: "Anastasia-Labs/midgard" },
    "overhead-seconds": { type: "string", default: "5" },
    "project-shards": { type: "string" },
    "warn-share": { type: "string", default: "0.1" },
    "test-file": {
      type: "string",
      default: String.raw`\.(?:test|spec)\.[cm]?[jt]sx?$`,
    },
  },
});
const tablePath = values.table ?? values.out;
const outPath = values.out ?? values.table;
if (
  !values.package ||
  !tablePath ||
  (positionals.length === 0 && values.run === undefined)
) {
  process.stderr.write(
    "usage: ci-file-durations.mjs --package <dir> --table <table.json> [--out <path>] (--run <id> | <record.json | log>...)\n",
  );
  process.exit(2);
}
const overhead = Number(values["overhead-seconds"]);
if (!Number.isFinite(overhead) || overhead < 0)
  throw new Error("--overhead-seconds must be a non-negative number");
const warnShare = Number(values["warn-share"]);
if (!Number.isFinite(warnShare) || warnShare <= 0)
  throw new Error("--warn-share must be a positive number");
const packageName = basename(resolve(values.package));
const testFile = new RegExp(values["test-file"], "u");

const inActions = process.env.GITHUB_ACTIONS === "true";
const warn = (title, message) =>
  process.stdout.write(
    inActions
      ? `::warning title=${title}::${message.replace(/\n/gu, "%0A")}\n`
      : `warning: ${title}: ${message}\n`,
  );

// --- inputs -----------------------------------------------------------------

const gh = (...args) =>
  execFileSync("gh", args, {
    encoding: "utf8",
    maxBuffer: 256 * 1024 * 1024,
    stdio: ["ignore", "pipe", "pipe"],
  });

/** Downloads a run's records, or failing those, its shard job logs. */
const fetchRun = (run) => {
  const dir = mkdtempSync(join(tmpdir(), "ci-file-durations-"));
  process.on("exit", () => rmSync(dir, { recursive: true, force: true }));
  try {
    gh(
      "run",
      "download",
      run,
      "--repo",
      values.repo,
      "--pattern",
      `file-durations-${packageName}-*`,
      "--dir",
      dir,
    );
  } catch {
    // No such artifacts: a run from before the reporter existed.
  }
  const records = readdirSync(dir, { recursive: true, encoding: "utf8" })
    .filter((name) => name.endsWith(".json"))
    .map((name) => join(dir, name));
  if (records.length > 0) return records;
  const jobs = JSON.parse(
    gh("run", "view", run, "--repo", values.repo, "--json", "jobs"),
  ).jobs.filter(
    ({ name }) => name === packageName || name.startsWith(`${packageName} (`),
  );
  if (jobs.length === 0)
    throw new Error(`run ${run} has no ${packageName} records or jobs`);
  return jobs.map(({ databaseId, name }) => {
    const path = join(dir, `${databaseId}.log`);
    writeFileSync(
      path,
      gh("api", `repos/${values.repo}/actions/jobs/${databaseId}/logs`),
    );
    process.stdout.write(`read the log of ${name} (job ${databaseId})\n`);
    return path;
  });
};

// eslint-disable-next-line no-control-regex -- strips ANSI colour escapes
const ansi = /\x1b\[[0-9;]*m/gu;
const stamp = /(\d{4}-\d\d-\d\dT\d\d:\d\d:\d\d(?:\.\d+)?Z)\s/u;
const file = String.raw`((?:[\w.-]+/)*[\w.-]+\.(?:test|spec)\.m?[tj]sx?)`;
const fileLine = new RegExp(
  String.raw`\s[✓×↓]\s+(?:\S+\s+)?${file}\s+\((\d+) tests?[^)]*\)\s*(?:(\d+)ms)?\s*$`,
  "u",
);
const testLine = new RegExp(
  String.raw`\s[✓×]\s+(?:\S+\s+)?${file} > .*?(?:\s(\d+)ms)?\s*$`,
  "u",
);

/** Seconds per file from one Vitest console log, overhead included. */
const readLog = (text) => {
  const fileTotals = new Map();
  const tests = new Map();
  for (const raw of text.split("\n")) {
    const line = raw.replace(ansi, "");
    const at = stamp.exec(line);
    const time = at ? Date.parse(at[1]) : undefined;
    const body = at ? line.slice(at.index + at[0].length - 1) : line;
    // Only the first Vitest run of the job: later steps (shard 1's traced
    // refusal reruns, say) print the same files again.
    if (/^\s*Test Files\s/u.test(body)) break;
    const whole = fileLine.exec(body);
    if (whole) {
      fileTotals.set(whole[1], Number(whole[3] ?? 0) / 1000);
      continue;
    }
    const test = testLine.exec(body);
    if (test) {
      const ms = Number(test[2] ?? 0);
      const entry = tests.get(test[1]) ?? {
        sum: 0,
        first: time,
        firstMs: ms,
        last: time,
      };
      entry.sum += ms;
      entry.last = time ?? entry.last;
      tests.set(test[1], entry);
    }
  }
  for (const [name, entry] of tests) {
    if (fileTotals.has(name)) continue;
    const span =
      entry.first !== undefined && entry.last !== undefined
        ? entry.last - entry.first + entry.firstMs
        : 0;
    fileTotals.set(name, Math.max(entry.sum, span) / 1000);
  }
  return new Map(
    [...fileTotals].map(([name, total]) => [name, total + overhead]),
  );
};

/** Seconds per file from one `FileDurationsReporter` record. */
const readRecord = (raw, path) => {
  if (raw?.schema !== "midgard-file-durations/v1")
    throw new Error(`${path}: not a midgard-file-durations/v1 record`);
  return new Map(Object.entries(raw.files));
};

const inputs = [
  ...positionals,
  ...(values.run === undefined ? [] : fetchRun(values.run)),
];
/** @type {Map<string, number>} */
const measured = new Map();
for (const path of inputs) {
  const text = readFileSync(path, "utf8");
  let record;
  try {
    record = JSON.parse(text);
  } catch {
    record = undefined;
  }
  const seconds =
    record === undefined ? readLog(text) : readRecord(record, path);
  for (const [name, value] of seconds)
    measured.set(name, Math.max(measured.get(name) ?? 0, value));
}
if (measured.size === 0) throw new Error("no test files found in the inputs");

// --- refreshed table ----------------------------------------------------------

const committedRaw = existsSync(tablePath)
  ? JSON.parse(readFileSync(tablePath, "utf8"))
  : {};
const committed = existsSync(tablePath)
  ? readShardDurationTable(tablePath)
  : {
      files: new Map(),
      defaultSeconds: 0,
      forksPerShard: 2,
      reservedSeconds: new Map(),
    };
const onDisk = new Set(
  readdirSync(join(values.package, "tests"), {
    recursive: true,
    encoding: "utf8",
  })
    .filter((name) => testFile.test(name))
    .map((name) => `tests/${name.split("\\").join("/")}`),
);
const round = (value) => Math.round(value * 10) / 10;
const files = Object.fromEntries(
  [...new Set([...committed.files.keys(), ...measured.keys()])]
    .filter((name) => onDisk.has(name))
    .sort()
    .map((name) => [
      name,
      round(measured.get(name) ?? committed.files.get(name)),
    ]),
);
const sorted = Object.values(files).sort((a, b) => a - b);
const median = sorted[Math.floor(sorted.length / 2)];
const table = {
  $comment: committedRaw.$comment,
  forksPerShard: committedRaw.forksPerShard ?? 2,
  reservedSeconds: committedRaw.reservedSeconds ?? {},
  defaultSeconds: median,
  files,
};
writeFileSync(outPath, JSON.stringify(table, null, 2) + "\n");
const total = (seconds) => Math.round(seconds.reduce((a, b) => a + b, 0));
process.stdout.write(
  `${outPath}: ${sorted.length} files, ${total(sorted)} s total, default ${median} s ` +
    `(${measured.size} measured in ${inputs.length} inputs)\n`,
);

// --- drift of the committed table -------------------------------------------

const measuredOnDisk = [...measured].filter(([name]) => onDisk.has(name));
const runSeconds = measuredOnDisk.reduce((sum, [, value]) => sum + value, 0);
const unknown = measuredOnDisk.filter(([name]) => !committed.files.has(name));
const misweighted = measuredOnDisk.filter(([name, value]) => {
  const weight = committed.files.get(name);
  if (weight === undefined) return false;
  const error = Math.abs(value - weight);
  return error > 10 && error > weight / 2;
});
const vanished = [...committed.files.keys()].filter(
  (name) => !onDisk.has(name),
);
const drift = [...unknown, ...misweighted];
const driftSeconds = drift.reduce((sum, [, value]) => sum + value, 0);
const share = runSeconds === 0 ? 0 : driftSeconds / runSeconds;
process.stdout.write(
  `drift of ${tablePath}: ${unknown.length} unknown and ${misweighted.length} misweighted files carry ` +
    `${Math.round(driftSeconds)} of ${Math.round(runSeconds)} measured s (${(share * 100).toFixed(1)}%); ` +
    `${vanished.length} entries name files that no longer exist\n`,
);
if (share > warnShare) {
  const largest = drift
    .sort(([, left], [, right]) => right - left)
    .slice(0, 10)
    .map(
      ([name, value]) =>
        `${name} ${round(value)} s (table: ${committed.files.get(name) ?? `unknown, weighed ${committed.defaultSeconds}`})`,
    )
    .join("; ");
  warn(
    `${packageName} shard durations are stale`,
    `${(share * 100).toFixed(1)}% of this run's seconds are in files the committed table ` +
      `does not know or weighs wrongly, so its shards are unbalanced. Refresh it: ` +
      `node demo/midgard-test-support/scripts/ci-file-durations.mjs --package ${values.package} ` +
      `--table ${tablePath} --run <this run id>. Largest: ${largest}`,
  );
}

// --- projection ---------------------------------------------------------------

if (values["project-shards"] !== undefined) {
  const count = Number(values["project-shards"]);
  const projection = projectShardSeconds({
    entries: [...onDisk].sort().map((name) => ({ id: name, file: name })),
    count,
    table: readShardDurationTable(outPath),
  });
  process.stdout.write(
    `projected seconds of ${count} shards: ${projection.map(Math.round).join(" / ")} ` +
      `(critical ${Math.round(Math.max(...projection))} s)\n`,
  );
}
