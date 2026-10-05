#!/usr/bin/env node

/**
 * Writes the per-file CI seconds table that `duration-shards.js` packs shards
 * by, from GitHub Actions logs of the package's test step.
 *
 *   node scripts/ci-file-durations.mjs --package <dir> --out <table.json> \
 *     [--overhead-seconds 5] <job log>...
 *
 * Fetch each shard's log with
 *   gh api repos/Anastasia-Labs/midgard/actions/jobs/<job id>/logs > job.log
 *
 * Both Vitest console reporters are read. The default reporter prints one
 * line per file with the file's total (`✓ project tests/x.test.ts (9 tests)
 * 1234ms`), which is used as is. The verbose reporter prints one line per
 * test and gives a duration only for tests above its slow threshold, so a
 * file's seconds are the larger of the summed test durations and the span
 * between its first and last reported test (which also covers hooks between
 * tests). Either way `--overhead-seconds` is added per file for the fork
 * start, import and collection that no test duration includes.
 *
 * Only lines before a log's first `Test Files` summary count, so a later step
 * that reruns some files does not stretch their spans. A file named in several
 * logs keeps its largest value. Files that no longer
 * exist under `--package` are dropped. `defaultSeconds` (the weight of a file
 * the table does not know) is the median of the table, and `forksPerShard`
 * and `reservedSeconds` are carried over from an existing `--out` file.
 */

import { existsSync, readFileSync, writeFileSync } from "node:fs";
import { join } from "node:path";
import { parseArgs } from "node:util";

const { values, positionals } = parseArgs({
  allowPositionals: true,
  options: {
    package: { type: "string" },
    out: { type: "string" },
    "overhead-seconds": { type: "string", default: "5" },
  },
});
if (!values.package || !values.out || positionals.length === 0) {
  process.stderr.write(
    "usage: ci-file-durations.mjs --package <dir> --out <table.json> [--overhead-seconds N] <log>...\n",
  );
  process.exit(2);
}
const overhead = Number(values["overhead-seconds"]);
if (!Number.isFinite(overhead) || overhead < 0)
  throw new Error("--overhead-seconds must be a non-negative number");

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

/** @type {Map<string, number>} */
const seconds = new Map();
for (const log of positionals) {
  const fileTotals = new Map();
  const tests = new Map();
  for (const raw of readFileSync(log, "utf8").split("\n")) {
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
  for (const [name, total] of fileTotals) {
    const value = total + overhead;
    seconds.set(name, Math.max(seconds.get(name) ?? 0, value));
  }
}

const files = Object.fromEntries(
  [...seconds]
    .filter(([name]) => existsSync(join(values.package, name)))
    .sort(([left], [right]) => (left < right ? -1 : left > right ? 1 : 0))
    .map(([name, value]) => [name, Math.round(value * 10) / 10]),
);
const sorted = Object.values(files).sort((a, b) => a - b);
if (sorted.length === 0) throw new Error("no test files found in the logs");
const median = sorted[Math.floor(sorted.length / 2)];
const previous = existsSync(values.out)
  ? JSON.parse(readFileSync(values.out, "utf8"))
  : {};
writeFileSync(
  values.out,
  JSON.stringify(
    {
      $comment: previous.$comment,
      forksPerShard: previous.forksPerShard ?? 2,
      reservedSeconds: previous.reservedSeconds ?? {},
      defaultSeconds: median,
      files,
    },
    null,
    2,
  ) + "\n",
);
process.stdout.write(
  `${values.out}: ${sorted.length} files, ${Math.round(sorted.reduce((a, b) => a + b, 0))} s total, default ${median} s\n`,
);
