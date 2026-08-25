#!/usr/bin/env node

/**
 * Workspace test orchestrator — three concurrent lanes instead of a serial
 * `pnpm -r` walk.
 *
 * The measured serial pass is ~126 minutes on a 32-core machine because the
 * two dominant suites (midgard-fault-proofs, midgard-node) run one after the
 * other, each internally bounded. Lane A and lane B are those two suites;
 * lane C strings the five cheap suites (their combined wall clock is minutes)
 * behind midgard-validation. The pass's wall clock is the max of the three
 * lanes, not their sum.
 *
 * Memory discipline: when `systemd-run` is available, each lane runs in its
 * OWN transient scope at `MemoryMax=12G, MemorySwapMax=0` with
 * `NODE_OPTIONS=--max-old-space-size=4096` — the same envelope every suite
 * already runs under individually, applied per lane. A lane dying at its cap
 * is a FINDING (lower that suite's fork bound — MIDGARD_FAULT_PROOF_FORKS /
 * MIDGARD_NODE_TEST_FORKS — never raise the cap). Without systemd-run the
 * lanes run unscoped, exactly as `pnpm -r run test` always has.
 *
 * Degradation: lane concurrency is `MIDGARD_TEST_LANE_CONCURRENCY` when set;
 * otherwise 3, dropping to 1 (strictly serial lanes, the historical
 * behaviour) on machines with fewer than 8 CPUs so constrained CI runners
 * never oversubscribe — the July 2026 lesson (5b9982a8) that every past
 * timeout here was a scheduling bug wearing a timeout's clothes.
 *
 * Typecheck runs first, workspace-wide, exactly as the previous `test`
 * script did — a red typecheck fails the pass before any lane starts.
 */

import { spawn, spawnSync } from "node:child_process";
import { availableParallelism } from "node:os";
import { fileURLToPath } from "node:url";
import { resolve } from "node:path";

const demoRoot = resolve(fileURLToPath(new URL("..", import.meta.url)));

const LANES = [
  { name: "A:fault-proofs", filters: ["@al-ft/midgard-fault-proofs"] },
  { name: "B:node", filters: ["midgard-node"] },
  {
    name: "C:rest",
    filters: [
      "@al-ft/midgard-validation",
      "midgard-watcher",
      "@al-ft/midgard-core",
      "@al-ft/midgard-sdk",
      "@al-ft/lucid-midgard",
    ],
  },
];

const hasSystemdRun = () =>
  spawnSync("systemd-run", ["--version"], { stdio: "ignore" }).status === 0;

const scoped = hasSystemdRun();

const laneConcurrency = (() => {
  const fromEnv = Number(process.env.MIDGARD_TEST_LANE_CONCURRENCY ?? "");
  if (Number.isInteger(fromEnv) && fromEnv >= 1) {
    return fromEnv;
  }
  return availableParallelism() < 8 ? 1 : LANES.length;
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

const laneCommand = (filter) => {
  const pnpmArgs = ["--filter", filter, "--if-present", "run", "test"];
  if (scoped) {
    return {
      command: "systemd-run",
      args: [
        "--user",
        "--scope",
        "-q",
        "-p",
        "MemoryMax=12G",
        "-p",
        "MemorySwapMax=0",
        "env",
        "NODE_OPTIONS=--max-old-space-size=4096",
        "pnpm",
        ...pnpmArgs,
      ],
    };
  }
  return { command: "pnpm", args: pnpmArgs };
};

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
  `[lanes] concurrency ${String(laneConcurrency)}${scoped ? ", per-lane 12G scopes" : ", unscoped (no systemd-run)"}\n`,
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
