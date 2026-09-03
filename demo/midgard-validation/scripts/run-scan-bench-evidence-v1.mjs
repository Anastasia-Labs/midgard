#!/usr/bin/env node

/**
 * #633/#642 — native-script scan ExUnits evidence orchestrator.
 *
 * The single-process benchmark run cannot complete: `@lucid-evolution/uplc`
 * leaks wasm linear memory across evaluations, wasm32 memory never shrinks,
 * and the accumulated evaluations trap with an opaque `unreachable` at
 * `deep 65 native:finalize` — reproduced identically before and after the
 * parameter-application memoization, so the leak is in the per-measurement
 * evaluations themselves. The only isolation the wasm module respects is a
 * process boundary, and vitest isolates per FILE, not per test — hence this
 * orchestrator: it runs the benchmark file
 * (`tests/native-script-scan-fault-proof-exunits-emulator.test.ts`) once
 * per MEASUREMENT in a fresh process, then once more to judge the merged
 * readings against the committed ledger. The file's own env knobs
 * (`MIDGARD_SCAN_BENCH_{SHAPES,NODES,STEPS,MAXFIT,MODE,OUT,MERGED}`) are the
 * whole control surface; nothing here bypasses the ledger discipline.
 *
 * Phases:
 *   1. list    — one child per shape resolves its curve points (including
 *                `maxfit`) and the step labels each point selects.
 *   2. measure — one child per (shape, nodes, label): fresh process, fresh
 *                wasm heap, exactly one measured evaluation. Children run
 *                `MIDGARD_SCAN_BENCH_CHILD_CONCURRENCY` at a time (default 2).
 *   3. check   — one child reads the merged readings and runs the full ledger
 *                judgement. The user's own narrowing env (if any) is restored
 *                for this child ONLY, so `checkScanBenchLedger`'s
 *                truncated-bootstrap refusal still sees it; the orchestrator's
 *                internal narrowing of measure children is invisible to it.
 *
 * `MIDGARD_SCAN_BENCH_UPDATE=1` reaches only the check child — a measure
 * child must never write the ledger.
 */

import { spawn } from "node:child_process";
import { mkdtempSync, readFileSync, rmSync } from "node:fs";
import { tmpdir } from "node:os";
import { join, resolve } from "node:path";
import { fileURLToPath } from "node:url";

const packageRoot = resolve(fileURLToPath(new URL("..", import.meta.url)));
const BENCH_TEST_FILE =
  "tests/native-script-scan-fault-proof-exunits-emulator.test.ts";
const FILTER_ENV = [
  "MIDGARD_SCAN_BENCH_SHAPES",
  "MIDGARD_SCAN_BENCH_NODES",
  "MIDGARD_SCAN_BENCH_STEPS",
  "MIDGARD_SCAN_BENCH_MAXFIT",
];

/** The narrowing the CALLER asked for — restored verbatim for the check child. */
const userFilters = Object.fromEntries(
  FILTER_ENV.filter(
    (name) => process.env[name] !== undefined && process.env[name] !== "",
  ).map((name) => [name, process.env[name]]),
);
const updateRequested = process.env.MIDGARD_SCAN_BENCH_UPDATE === "1";

const childConcurrency = (() => {
  const fromEnv = Number(
    process.env.MIDGARD_SCAN_BENCH_CHILD_CONCURRENCY ?? "",
  );
  return Number.isInteger(fromEnv) && fromEnv >= 1 ? fromEnv : 2;
})();

/**
 * Base env for every child: the caller's env minus every benchmark knob this
 * orchestrator owns, so a child sees exactly what is set for it and nothing
 * ambient. Evidence mode is always on for children — that is the lane the
 * benchmark file lives behind.
 */
const childBaseEnv = () => {
  const env = { ...process.env, MIDGARD_VALIDATION_EVIDENCE: "1" };
  for (const name of [
    ...FILTER_ENV,
    "MIDGARD_SCAN_BENCH_MODE",
    "MIDGARD_SCAN_BENCH_OUT",
    "MIDGARD_SCAN_BENCH_MERGED",
    "MIDGARD_SCAN_BENCH_UPDATE",
  ]) {
    delete env[name];
  }
  return env;
};

const runChild = (label, extraEnv) =>
  new Promise((resolveChild) => {
    const child = spawn(
      "pnpm",
      ["exec", "vitest", "run", BENCH_TEST_FILE, "--reporter=basic"],
      {
        cwd: packageRoot,
        env: { ...childBaseEnv(), ...extraEnv },
        stdio: ["ignore", "pipe", "pipe"],
      },
    );
    const forward = (stream, sink) => {
      stream.on("data", (chunk) => {
        for (const line of chunk.toString().split("\n")) {
          if (line.length > 0) sink.write(`[${label}] ${line}\n`);
        }
      });
    };
    forward(child.stdout, process.stdout);
    forward(child.stderr, process.stderr);
    child.on("close", (code) => resolveChild(code ?? 1));
  });

const fail = (message) => {
  process.stderr.write(`[scan-bench] ${message}\n`);
  process.exit(1);
};

const tmpDir = mkdtempSync(join(tmpdir(), "scan-bench-evidence-"));
try {
  // Phase 1 — list. One child per shape (the user's shape narrowing, if any,
  // applies here and thereby to everything downstream).
  const shapes = (userFilters.MIDGARD_SCAN_BENCH_SHAPES ?? "deep,wide")
    .split(",")
    .filter((part) => part !== "");
  const listing = [];
  for (const shape of shapes) {
    const outPath = join(tmpDir, `list-${shape}.json`);
    const code = await runChild(`list:${shape}`, {
      MIDGARD_SCAN_BENCH_MODE: "list",
      MIDGARD_SCAN_BENCH_SHAPES: shape,
      ...(userFilters.MIDGARD_SCAN_BENCH_NODES === undefined
        ? {}
        : { MIDGARD_SCAN_BENCH_NODES: userFilters.MIDGARD_SCAN_BENCH_NODES }),
      ...(userFilters.MIDGARD_SCAN_BENCH_MAXFIT === undefined
        ? {}
        : { MIDGARD_SCAN_BENCH_MAXFIT: userFilters.MIDGARD_SCAN_BENCH_MAXFIT }),
      MIDGARD_SCAN_BENCH_OUT: outPath,
    });
    if (code !== 0) fail(`list child for shape '${shape}' exited ${code}`);
    listing.push(...JSON.parse(readFileSync(outPath, "utf8")));
  }

  // Phase 2 — measure: one fresh process per (shape, nodes, label).
  const stepFilter = (userFilters.MIDGARD_SCAN_BENCH_STEPS ?? "")
    .split(",")
    .filter((part) => part !== "");
  const jobs = listing.flatMap((point) =>
    point.labels
      .filter((label) => stepFilter.length === 0 || stepFilter.includes(label))
      .map((label) => ({ ...point, label })),
  );
  if (jobs.length === 0) fail("the listing produced no measurement jobs");
  process.stdout.write(
    `[scan-bench] ${jobs.length} measurement(s) across ${listing.length} curve point(s), concurrency ${childConcurrency}\n`,
  );
  const jobOut = (job) =>
    join(
      tmpDir,
      `measure-${job.shape}-${job.nodes}-${job.label.replace(/[^A-Za-z0-9]/gu, "_")}.json`,
    );
  const queue = [...jobs];
  const failures = [];
  await Promise.all(
    Array.from(
      { length: Math.min(childConcurrency, queue.length) },
      async () => {
        for (;;) {
          const job = queue.shift();
          if (job === undefined) return;
          const label = `${job.shape}:${job.nodes}:${job.label}`;
          const code = await runChild(label, {
            MIDGARD_SCAN_BENCH_MODE: "measure",
            MIDGARD_SCAN_BENCH_SHAPES: job.shape,
            MIDGARD_SCAN_BENCH_NODES: String(job.nodes),
            MIDGARD_SCAN_BENCH_MAXFIT: "0",
            MIDGARD_SCAN_BENCH_STEPS: job.label,
            MIDGARD_SCAN_BENCH_OUT: jobOut(job),
          });
          if (code !== 0) failures.push({ label, code });
        }
      },
    ),
  );
  if (failures.length > 0) {
    for (const failure of failures) {
      process.stderr.write(
        `[scan-bench] FAILED ${failure.label} (exit ${failure.code})\n`,
      );
    }
    fail(
      `${failures.length} measurement child(ren) failed — not judging a partial reading set`,
    );
  }

  // Merge, preserving listing order. Point-level fields must agree across a
  // point's children (same deterministic build) — a mismatch is a build
  // nondeterminism finding, refused loudly.
  const merged = listing.map((point) => {
    const labels = point.labels.filter(
      (label) => stepFilter.length === 0 || stepFilter.includes(label),
    );
    const parts = labels.map((label) => {
      const reports = JSON.parse(
        readFileSync(jobOut({ ...point, label }), "utf8"),
      );
      if (reports.length !== 1 || reports[0].measurements.length !== 1) {
        fail(
          `child ${point.shape}:${point.nodes}:${label} wrote ${reports.length} report(s) — expected exactly one with one measurement`,
        );
      }
      return reports[0];
    });
    const head = parts[0];
    for (const part of parts) {
      for (const field of ["payloadBytes", "spentOutputBytes"]) {
        if (part[field] !== head[field]) {
          fail(
            `curve point ${point.shape}:${point.nodes} rebuilt with a different ${field} across children — the case build is not deterministic`,
          );
        }
      }
      if (
        JSON.stringify(part.proofStepCounts) !==
        JSON.stringify(head.proofStepCounts)
      ) {
        fail(
          `curve point ${point.shape}:${point.nodes} rebuilt with different proofStepCounts across children`,
        );
      }
    }
    return {
      shape: head.shape,
      nodes: head.nodes,
      payloadBytes: head.payloadBytes,
      spentOutputBytes: head.spentOutputBytes,
      proofStepCounts: head.proofStepCounts,
      measurements: parts.map((part) => part.measurements[0]),
    };
  });
  const mergedPath = join(tmpDir, "merged-readings.json");
  const { writeFileSync } = await import("node:fs");
  writeFileSync(mergedPath, `${JSON.stringify(merged, null, 2)}\n`);

  // Phase 3 — judge the merged readings. The user's own narrowing (if any) is
  // restored so the ledger check's truncated-bootstrap refusal can see it.
  const checkCode = await runChild("check", {
    ...userFilters,
    MIDGARD_SCAN_BENCH_MODE: "check",
    MIDGARD_SCAN_BENCH_MERGED: mergedPath,
    ...(updateRequested ? { MIDGARD_SCAN_BENCH_UPDATE: "1" } : {}),
  });
  process.exit(checkCode);
} finally {
  rmSync(tmpDir, { recursive: true, force: true });
}
