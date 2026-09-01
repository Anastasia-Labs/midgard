#!/usr/bin/env node

/**
 * Workspace test orchestrator — three lanes on a bounded two-worker queue
 * instead of a serial `pnpm -r` walk.
 *
 * The measured serial pass is ~126 minutes on a 32-core machine because the
 * two dominant suites used to run one after the other. Lane A is fault proofs;
 * watcher runs as a serial prelude because several of its measured setup paths
 * already sit close to 60/120-second Vitest budgets in isolation and time out
 * under either heavy lane. After node's emulator-snapshot optimization made
 * lane B much shorter, the stable post-watcher critical path is A+C first and
 * B as soon as A frees a worker. Node then overlaps validation without being
 * serialized behind all of C.
 *
 * Memory discipline is BY CONSTRUCTION, not by containment. Each worker's heap
 * is bounded where it belongs — `poolOptions.forks.execArgv` in each suite's
 * own vitest config — and this runner then admits only as many concurrent lanes
 * as the machine's free RAM can hold at that per-worker bound. There is no
 * cgroup ceiling around a lane.
 *
 * This deliberately replaced a `systemd-run --user --scope` wrapper at
 * `MemoryMax=12G`. That wrapper required a per-user systemd manager, which is a
 * per-login service that can die (OOM kill is the usual cause) long after boot
 * and leave every lane unable to start; it also bought nothing CI had, since CI
 * does not use this script at all — `midgard-node-ci.yml` runs each package's
 * `test` serially, bounded by the runner VM. Sizing concurrency to RAM gives the
 * same protection against the realistic failure (three heavy lanes at once)
 * without depending on a service that must be alive.
 *
 * A lane dying on memory is still a FINDING: lower that suite's fork bound
 * (MIDGARD_FAULT_PROOF_FORKS / MIDGARD_NODE_TEST_FORKS) or its `execArgv` heap,
 * never just re-run hoping for a different scheduler.
 *
 * Degradation: lane concurrency is `MIDGARD_TEST_LANE_CONCURRENCY` when set;
 * otherwise the smaller of what the CPUs allow (2, dropping to 1 below 8 cores
 * so constrained runners never oversubscribe — the July 2026 lesson
 * (5b9982a8) that every past timeout here was a scheduling bug wearing a
 * timeout's clothes) and what free RAM allows.
 *
 * Typecheck runs first, workspace-wide, exactly as the previous `test`
 * script did — a red typecheck fails the pass before any lane starts.
 */

import { spawn } from "node:child_process";
import { availableParallelism, freemem } from "node:os";
import { readFileSync } from "node:fs";
import { fileURLToPath } from "node:url";
import { resolve } from "node:path";

const demoRoot = resolve(fileURLToPath(new URL("..", import.meta.url)));

const LANES = [
  { name: "A:fault-proofs", filters: ["@al-ft/midgard-fault-proofs"] },
  {
    name: "C:rest",
    filters: [
      "@al-ft/midgard-validation",
      "@al-ft/midgard-core",
      "@al-ft/midgard-sdk",
      "@al-ft/lucid-midgard",
    ],
  },
  { name: "B:node", filters: ["midgard-node"] },
];

const SERIAL_PRELUDE_FILTERS = ["midgard-watcher"];

// Worst-case resident memory a single lane can reach: its heaviest suite's fork
// bound times that suite's per-worker heap, plus room for the fork's non-heap
// footprint (native allocations and the uplc/wasm evaluators, which sit OUTSIDE
// V8's --max-old-space-size and are the reason a heap flag alone never bounded
// these suites). Deliberately generous: over-estimating costs one lane of
// parallelism, under-estimating costs the machine.
const LANE_MEMORY_BUDGET_GIB = 12;

// Free RAM, not total: what is actually available to these lanes right now.
// `freemem()` alone under-reports on Linux because it excludes reclaimable page
// cache, so prefer MemAvailable, which the kernel computes for exactly this
// question, and fall back to freemem() where /proc is unreadable.
const availableMemoryGiB = () => {
  try {
    const meminfo = readFileSync("/proc/meminfo", "utf8");
    const match = /^MemAvailable:\s+(\d+)\s+kB$/m.exec(meminfo);
    if (match) {
      return Number(match[1]) / 1024 / 1024;
    }
  } catch {
    // Not Linux, or /proc unavailable — fall through.
  }
  return freemem() / 1024 ** 3;
};

const laneConcurrency = (() => {
  const fromEnv = Number(process.env.MIDGARD_TEST_LANE_CONCURRENCY ?? "");
  if (Number.isInteger(fromEnv) && fromEnv >= 1) {
    // An explicit pin is honoured as-is. Overriding it by memory would make the
    // one knob a developer reaches for when reproducing a scheduling bug
    // silently not mean what it says.
    return fromEnv;
  }
  // Three simultaneous heavy suites made Postgres leases and child-process
  // timing flaky without shortening the measured critical path. Two workers
  // keep the machine responsive; the lane order above still overlaps node
  // with C after fault proofs completes.
  const byCpu = availableParallelism() < 8 ? 1 : 2;
  // At least one lane always runs: refusing to test on a busy machine is worse
  // than running the lanes serially.
  const byMemory = Math.max(
    1,
    Math.floor(availableMemoryGiB() / LANE_MEMORY_BUDGET_GIB),
  );
  return Math.min(byCpu, byMemory);
})();

const run = (command, args, label) =>
  new Promise((resolveRun) => {
    const child = spawn(command, args, {
      cwd: demoRoot,
      env: process.env,
      stdio: ["ignore", "pipe", "pipe"],
    });
    const forward = (stream, sink) => {
      let buffered = "";
      stream.on("data", (chunk) => {
        buffered += chunk.toString();
        const lines = buffered.split("\n");
        buffered = lines.pop() ?? "";
        for (const line of lines) {
          sink.write(`[${label}] ${line}\n`);
        }
      });
      stream.on("end", () => {
        if (buffered.length > 0) {
          sink.write(`[${label}] ${buffered}\n`);
        }
      });
    };
    forward(child.stdout, process.stdout);
    forward(child.stderr, process.stderr);
    child.on("close", (code) => {
      resolveRun(code ?? 1);
    });
  });

// Per-worker heap is set in each suite's own vitest config
// (`poolOptions.forks.execArgv`), not exported here: a blanket NODE_OPTIONS
// would also apply to pnpm, vitest's own main process and every unrelated tool
// in the lane, and would be silently overridden by any suite that sets its own.
const laneCommand = (filter) => ({
  command: "pnpm",
  args: ["--filter", filter, "--if-present", "run", "test"],
});

const runLane = async (lane) => {
  // Run EVERY package in the lane even after one fails: with long-lived
  // known-red suites, stopping at the first failure would permanently shadow
  // everything queued behind it (lane C's validation red would hide watcher,
  // core, sdk and lucid). All failures are collected and the pass still exits
  // red — coverage and the verdict are both preserved.
  const laneFailures = [];
  for (const filter of lane.filters) {
    const { command, args } = laneCommand(filter);
    const started = Date.now();
    const code = await run(command, args, lane.name);
    const seconds = Math.round((Date.now() - started) / 1000);
    process.stdout.write(
      `[lanes] ${lane.name} ${filter}: exit ${String(code)} in ${String(seconds)}s\n`,
    );
    if (code !== 0) {
      laneFailures.push({ lane: lane.name, filter, code });
    }
  }
  return laneFailures;
};

process.stdout.write(
  `[lanes] concurrency ${String(laneConcurrency)} ` +
    `(cpus ${String(availableParallelism())}, ` +
    `available ${availableMemoryGiB().toFixed(1)}GiB, ` +
    `budget ${String(LANE_MEMORY_BUDGET_GIB)}GiB/lane)\n`,
);

const typecheckCode = await run(
  "pnpm",
  ["-r", "--if-present", "--workspace-concurrency=4", "run", "typecheck"],
  "typecheck",
);
if (typecheckCode !== 0) {
  process.stderr.write("[lanes] typecheck failed — no lane was started\n");
  process.exit(typecheckCode);
}

for (const filter of SERIAL_PRELUDE_FILTERS) {
  const { command, args } = laneCommand(filter);
  const started = Date.now();
  const code = await run(command, args, "prelude");
  const seconds = Math.round((Date.now() - started) / 1000);
  process.stdout.write(
    `[lanes] prelude ${filter}: exit ${String(code)} in ${String(seconds)}s\n`,
  );
  if (code !== 0) {
    process.stderr.write(
      `[lanes] prelude failed — no parallel lane was started\n`,
    );
    process.exit(code);
  }
}

const failures = [];
if (laneConcurrency >= LANES.length) {
  const results = await Promise.all(LANES.map((lane) => runLane(lane)));
  failures.push(...results.flat());
} else {
  // Bounded lane concurrency: a simple work queue, preserving lane order.
  const queue = [...LANES];
  const workers = Array.from(
    { length: Math.min(laneConcurrency, queue.length) },
    async () => {
      for (;;) {
        const lane = queue.shift();
        if (lane === undefined) {
          return;
        }
        failures.push(...(await runLane(lane)));
      }
    },
  );
  await Promise.all(workers);
}

if (failures.length > 0) {
  for (const failure of failures) {
    process.stderr.write(
      `[lanes] FAILED ${failure.lane} ${failure.filter} (exit ${String(failure.code)})\n`,
    );
  }
  process.exit(1);
}
process.stdout.write("[lanes] all lanes green\n");
