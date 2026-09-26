#!/usr/bin/env node
// Runs one command N times in sequence and reports how many runs passed.
//
// Usage:
//   node rerun.mjs --times N [--k K] [--timeout-s S] [--tail L] [--no-nice] -- <command...>
//
//   --times N      how many runs (required, positive integer)
//   --k K          the flake's measured rate is about 1 failure in K runs; the
//                  report then says whether N is large enough to claim a fix
//   --timeout-s S  kill a run after S seconds and count it as failed
//   --tail L       lines of the first failure's output to print (default 40)
//   --no-nice      run the command at normal priority (default: nice -n 19)
//
// The command runs without a shell; wrap pipelines in `bash -c '...'`.
// Each run sees RERUN_ITERATION (1-based) and RERUN_TIMES in its environment.
//
// Exit codes:
//   0  every run passed
//   1  at least one run failed (non-zero exit, signal, or timeout)
//   2  usage error, or the command could not be run (not found, not
//      executable, `nice` missing, interrupted): nothing was measured

import { spawn } from "node:child_process";
import { accessSync, constants, statSync } from "node:fs";
import { delimiter, join } from "node:path";
import { fileURLToPath } from "node:url";

const USAGE =
  "usage: rerun.mjs --times N [--k K] [--timeout-s S] [--tail L] [--no-nice] -- <command...>";
const OUTPUT_CAP_BYTES = 1024 * 1024;

export class UsageError extends Error {}

const positiveInteger = (flag, raw) => {
  if (raw === undefined || !/^[1-9][0-9]*$/u.test(raw)) {
    throw new UsageError(
      `${flag} needs a positive integer, got ${raw ?? "nothing"}`,
    );
  }
  return Number(raw);
};

export const parseArgs = (argv) => {
  const separator = argv.indexOf("--");
  if (separator === -1) throw new UsageError("missing `--` before the command");
  const command = argv.slice(separator + 1);
  if (command.length === 0) throw new UsageError("no command after `--`");
  const options = {
    times: undefined,
    k: undefined,
    timeoutS: undefined,
    tail: 40,
    nice: true,
  };
  const flags = argv.slice(0, separator);
  for (let i = 0; i < flags.length; i += 1) {
    const flag = flags[i];
    switch (flag) {
      case "--times":
        options.times = positiveInteger(flag, flags[++i]);
        break;
      case "--k":
        options.k = positiveInteger(flag, flags[++i]);
        break;
      case "--timeout-s":
        options.timeoutS = positiveInteger(flag, flags[++i]);
        break;
      case "--tail":
        options.tail = positiveInteger(flag, flags[++i]);
        break;
      case "--no-nice":
        options.nice = false;
        break;
      default:
        throw new UsageError(`unknown option ${flag}`);
    }
  }
  if (options.times === undefined) throw new UsageError("--times is required");
  return { ...options, command };
};

/** Runs needed to claim a fix for a flake that fails about 1 in k runs. */
export const requiredRuns = (k) => Math.max(3 * k, 20);

/** Chance that a flake failing 1 in k runs passes all n runs anyway. */
export const missProbability = (k, n) => (1 - 1 / k) ** n;

/**
 * The failure rate that n clean runs rule out at 95% confidence: any rate p
 * with (1-p)^n <= 0.05. Returned as the "1 in X" denominator.
 */
export const ruledOutOneIn = (n) => 1 / (1 - 0.05 ** (1 / n));

/** Resolve a command the way execvp would, or return undefined. */
export const resolveExecutable = (name, pathEnv = process.env.PATH ?? "") => {
  const candidates = name.includes("/")
    ? [name]
    : pathEnv
        .split(delimiter)
        .filter((dir) => dir !== "")
        .map((dir) => join(dir, name));
  for (const candidate of candidates) {
    try {
      if (!statSync(candidate).isFile()) continue;
      accessSync(candidate, constants.X_OK);
      return candidate;
    } catch {
      // not here; keep looking
    }
  }
  return undefined;
};

const runOnce = ({ argv, env, timeoutS }) =>
  new Promise((resolve) => {
    const started = Date.now();
    const chunks = [];
    let size = 0;
    const keep = (chunk) => {
      chunks.push(chunk);
      size += chunk.length;
      while (size > OUTPUT_CAP_BYTES && chunks.length > 1)
        size -= chunks.shift().length;
    };
    // Own process group, so a timeout or Ctrl-C reaches the command's children too.
    const child = spawn(argv[0], argv.slice(1), {
      env,
      stdio: ["ignore", "pipe", "pipe"],
      detached: true,
    });
    child.stdout.on("data", keep);
    child.stderr.on("data", keep);
    let timedOut = false;
    const killGroup = (signal) => {
      try {
        process.kill(-child.pid, signal);
      } catch {
        // already gone
      }
    };
    const timer =
      timeoutS === undefined
        ? undefined
        : setTimeout(() => {
            timedOut = true;
            killGroup("SIGTERM");
            setTimeout(() => killGroup("SIGKILL"), 5000).unref();
          }, timeoutS * 1000);
    const onInterrupt = () => {
      killGroup("SIGTERM");
      resolve({ interrupted: true });
    };
    process.once("SIGINT", onInterrupt);
    process.once("SIGTERM", onInterrupt);
    const settle = () => {
      if (timer) clearTimeout(timer);
      process.removeListener("SIGINT", onInterrupt);
      process.removeListener("SIGTERM", onInterrupt);
    };
    child.on("error", (error) => {
      settle();
      resolve({ spawnError: error });
    });
    child.on("close", (code, signal) => {
      settle();
      resolve({
        code,
        signal,
        timedOut,
        seconds: (Date.now() - started) / 1000,
        output: Buffer.concat(chunks).toString("utf8"),
      });
    });
  });

const describeFailure = (result) => {
  if (result.timedOut) return "timed out";
  if (result.signal) return `killed by ${result.signal}`;
  return `exit ${result.code}`;
};

const tailLines = (text, lines) =>
  text.replace(/\s+$/u, "").split("\n").slice(-lines).join("\n");

const fmt = (probability) => `${(probability * 100).toFixed(1)}%`;

export const sizingReport = ({ times, k, failed = 0 }) => {
  const lines = [];
  if (failed === 0) {
    const oneIn = ruledOutOneIn(times);
    lines.push(
      `${times} clean runs rule out, at 95% confidence, a failure rate worse than about 1 in ${oneIn < 10 ? oneIn.toFixed(1) : Math.floor(oneIn)} (rule of three: 3/N).`,
    );
  } else {
    lines.push(
      `Observed ${failed} failure(s) in ${times} runs, about 1 in ${(times / failed).toFixed(1)}. Not fixed.`,
    );
  }
  if (k !== undefined) {
    const needed = requiredRuns(k);
    lines.push(
      `For a 1-in-${k} flake, ${times} runs all pass by luck ${fmt(missProbability(k, times))} of the time; max(3k, 20) = ${needed} runs gives ${fmt(missProbability(k, needed))}.`,
    );
    if (times < needed) {
      lines.push(
        `WARNING: N=${times} is below max(3k, 20) = ${needed}. A clean run here does not support a fix claim for a 1-in-${k} flake.`,
      );
    }
  } else {
    lines.push(
      "No --k given: pass the measured rate's denominator to check N against max(3k, 20).",
    );
  }
  return lines.join("\n");
};

export const main = async (
  argv,
  { stdout = process.stdout, stderr = process.stderr } = {},
) => {
  let options;
  try {
    options = parseArgs(argv);
  } catch (error) {
    if (!(error instanceof UsageError)) throw error;
    stderr.write(`rerun: ${error.message}\n${USAGE}\n`);
    return 2;
  }
  if (resolveExecutable(options.command[0]) === undefined) {
    stderr.write(
      `rerun: could not run ${options.command[0]}: not found or not executable. Nothing was measured.\n`,
    );
    return 2;
  }
  if (options.nice && resolveExecutable("nice") === undefined) {
    stderr.write(
      "rerun: `nice` not found; pass --no-nice to run at normal priority. Nothing was measured.\n",
    );
    return 2;
  }
  const argvToRun = options.nice
    ? ["nice", "-n", "19", ...options.command]
    : options.command;
  stdout.write(
    `rerun: ${options.times} runs of \`${options.command.join(" ")}\`${options.nice ? " under nice -n 19" : ""}\n`,
  );

  const failures = [];
  let firstFailure;
  for (let iteration = 1; iteration <= options.times; iteration += 1) {
    const result = await runOnce({
      argv: argvToRun,
      env: {
        ...process.env,
        RERUN_ITERATION: String(iteration),
        RERUN_TIMES: String(options.times),
      },
      timeoutS: options.timeoutS,
    });
    if (result.interrupted) {
      stderr.write(
        `rerun: interrupted during run ${iteration}; ${iteration - 1} completed runs are not a result.\n`,
      );
      return 2;
    }
    if (result.spawnError) {
      stderr.write(
        `rerun: could not start run ${iteration}: ${result.spawnError.message}. Nothing more was measured.\n`,
      );
      return 2;
    }
    const passed =
      !result.timedOut && result.signal === null && result.code === 0;
    stdout.write(
      `run ${iteration}/${options.times}: ${passed ? "pass" : `FAIL (${describeFailure(result)})`} in ${result.seconds.toFixed(1)} s\n`,
    );
    if (!passed) {
      failures.push(iteration);
      firstFailure ??= { iteration, result };
    }
  }

  const passes = options.times - failures.length;
  stdout.write(
    `\nresult: ${passes}/${options.times} passed, ${failures.length} failed`,
  );
  stdout.write(failures.length > 0 ? ` (runs ${failures.join(", ")})\n` : "\n");
  if (firstFailure !== undefined) {
    stdout.write(
      `\nfirst failure, run ${firstFailure.iteration} (${describeFailure(firstFailure.result)}), last ${options.tail} lines:\n`,
    );
    stdout.write(`${tailLines(firstFailure.result.output, options.tail)}\n`);
  }
  stdout.write(`\n${sizingReport({ ...options, failed: failures.length })}\n`);
  return failures.length === 0 ? 0 : 1;
};

if (process.argv[1] === fileURLToPath(import.meta.url)) {
  process.exitCode = await main(process.argv.slice(2));
}
