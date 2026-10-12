import { spawn } from "node:child_process";
import { createWriteStream, type WriteStream } from "node:fs";
import { join } from "node:path";
import { setTimeout as pause } from "node:timers/promises";

import {
  type DaBondPoolCommitteeExpectedView,
  type DaBondPoolCommitteeSync,
  daBondPoolCommitteeViewAgrees,
  type DaBondPoolReadyzRead,
  reasonCheckedAt,
} from "./da-bond-pool-committee-process.build-da-bond-pool-committee-env.js";
import type { DaBondPoolJourneyStep } from "./da-bond-pool-journey.js";
import { parseDaBondPoolReadyz } from "./da-bond-pool-process-evidence.js";
import { isTransientTransportError } from "./ledger-tip.js";

/**
 * Polls `/readyz` until the node has read the pool after `since` and its pool
 * reasons agree with `expected`, or the bound passes; returns the last answer
 * either way, so the driver judges what the node actually said.
 *
 * Fresh: a pool reason checked at or after `since`, or two answers whose
 * `scanner.lastStartedAt` values S1 >= `since` and S2 > S1. Ticks never
 * overlap and the pool is read after the scanner in each tick, so S2 proves
 * the tick that started at S1 finished its pool read.
 *
 * A read that got no answer (`isTransientTransportError`) while the node is
 * `alive` is polled past until the bound, and the last answer stands; with
 * no answer at all by the bound, the last such error is rethrown. A status
 * or body the node did answer is judged as it is. An answer whose L1 follower
 * holds the node on an intervention returns at once: no wait clears it.
 */
export const awaitDaBondPoolCommitteeSync = async (deps: {
  readonly read: () => Promise<DaBondPoolReadyzRead>;
  readonly expected: DaBondPoolCommitteeExpectedView | undefined;
  readonly since: number;
  readonly timeoutMs: number;
  readonly pollMs: number;
  /** Default: always alive. */
  readonly alive?: () => boolean;
  readonly now?: () => number;
  readonly sleep?: (ms: number) => Promise<unknown>;
}): Promise<DaBondPoolCommitteeSync> => {
  const now = deps.now ?? Date.now;
  const sleep = deps.sleep ?? pause;
  const alive = deps.alive ?? (() => true);
  const deadline = now() + deps.timeoutMs;
  let firstFreshStart: number | undefined;
  let fresh = false;
  let last: DaBondPoolCommitteeSync | undefined;
  for (let reads = 1; ; reads += 1) {
    let read: DaBondPoolReadyzRead;
    try {
      read = await deps.read();
    } catch (error) {
      if (!isTransientTransportError(error) || !alive()) throw error;
      if (now() >= deadline) {
        if (last === undefined) throw error;
        return { ...last, reads };
      }
      await sleep(deps.pollMs);
      continue;
    }
    const readyz = parseDaBondPoolReadyz(read.httpStatus, read.body);
    const started =
      readyz.scannerLastStartedAt === undefined
        ? Number.NaN
        : Date.parse(readyz.scannerLastStartedAt);
    if (!Number.isNaN(started) && started >= deps.since) {
      if (firstFreshStart === undefined) firstFreshStart = started;
      else if (started > firstFreshStart) fresh = true;
    }
    if (reasonCheckedAt(readyz.poolReasons).some((at) => at >= deps.since))
      fresh = true;
    const synced =
      fresh && daBondPoolCommitteeViewAgrees(readyz.poolReasons, deps.expected);
    last = { read, readyz, fresh, synced, reads };
    if (
      synced ||
      readyz.l1Source?.status === "intervention" ||
      now() >= deadline
    )
      return last;
    await sleep(deps.pollMs);
  }
};

/**
 * How long one observation may wait for the node to catch up (P27(4)): a few
 * of the node's poll intervals plus the finality lag of the manifest's
 * `confirmationDepth`, taken as twice the chain's ideal block cadence
 * (`slotLength / activeSlotsCoeff` per block), as the journey timing budgets
 * a confirmation. Past it the observation returns what it read, so the
 * driver goes red instead of waiting forever.
 */
export const daBondPoolCommitteeSyncBoundMs = (input: {
  readonly pollIntervalMs: number;
  readonly confirmationDepth: number;
  readonly slotLengthMs: number;
  readonly activeSlotsCoeff: number;
  /** Default 10. */
  readonly polls?: number;
}): number => {
  const polls = input.polls ?? 10;
  if (
    !Number.isSafeInteger(input.pollIntervalMs) ||
    input.pollIntervalMs <= 0 ||
    !Number.isSafeInteger(polls) ||
    polls <= 0 ||
    !Number.isSafeInteger(input.confirmationDepth) ||
    input.confirmationDepth < 1 ||
    !Number.isFinite(input.slotLengthMs) ||
    input.slotLengthMs <= 0 ||
    !Number.isFinite(input.activeSlotsCoeff) ||
    input.activeSlotsCoeff <= 0 ||
    input.activeSlotsCoeff > 1
  )
    throw new Error("Invalid committee sync bound inputs");
  return (
    polls * input.pollIntervalMs +
    Math.ceil(
      (2 * input.confirmationDepth * input.slotLengthMs) /
        input.activeSlotsCoeff,
    )
  );
};

/**
 * How long the submitter addresses are watched after each stop: one node
 * poll plus the same finality lag as the sync bound, so a transaction the
 * node submitted in its last tick lands or is indexed before the watch ends.
 */
export const daBondPoolCommitteeStopSettleMs = (input: {
  readonly pollIntervalMs: number;
  readonly confirmationDepth: number;
  readonly slotLengthMs: number;
  readonly activeSlotsCoeff: number;
}): number => daBondPoolCommitteeSyncBoundMs({ ...input, polls: 1 });

/** The committee node's availability responder report event. */
export const DA_BOND_POOL_RESPONDER_EVENT = "availability_responder";

/**
 * P27(3): what the journey does with its committee node around each step.
 * It runs through steps 1, 3, 4 and 5, is stopped before step 2's commit so
 * its payload-free settle and Close cannot race B3, is restarted before step
 * 6, and is stopped at the end of step 6, each stop checked.
 */
export const daBondPoolCommitteeLifecycle = (
  phase: "before" | "after",
  step: DaBondPoolJourneyStep,
): "start" | "stop" | undefined => {
  if (phase === "before")
    return step === 1 || step === 6 ? "start" : step === 2 ? "stop" : undefined;
  return step === 6 ? "stop" : undefined;
};

// ---------------------------------------------------------------------------
// Submitter UTxOs
// ---------------------------------------------------------------------------

/** Why two UTxO sets differ, or undefined when they are equal. */
export const daBondPoolSubmitterUtxoChange = (
  baseline: readonly string[],
  current: readonly string[],
): string | undefined => {
  const before = new Set(baseline);
  const after = new Set(current);
  const spent = [...before].filter((ref) => !after.has(ref)).sort();
  const created = [...after].filter((ref) => !before.has(ref)).sort();
  return spent.length === 0 && created.length === 0
    ? undefined
    : `spent [${spent.join(", ")}], created [${created.join(", ")}]`;
};

// ---------------------------------------------------------------------------
// The process
// ---------------------------------------------------------------------------

/** One running committee node, as the observer drives it. */
export type DaBondPoolCommitteeProcess = Readonly<{
  pid: number;
  alive: () => boolean;
  /** The whole stderr capture so far. */
  stderr: () => Uint8Array;
  /**
   * The whole stdout capture so far. The node writes its availability
   * responder's pending, included and confirmed reports, the only ones that
   * carry an `action`, to stdout (P27(2)).
   */
  stdout: () => Uint8Array;
  readyz: () => Promise<DaBondPoolReadyzRead>;
  /** Sends SIGTERM, then SIGKILL after `boundMs`. */
  stop: (boundMs: number) => Promise<DaBondPoolCommitteeExit>;
}>;

export type DaBondPoolCommitteeExit = Readonly<{
  exitCode: number | null;
  signal: string | null;
  /** SIGKILL was needed. */
  killed: boolean;
  stderrTail: string;
}>;

export const STDERR_TAIL_BYTES = 8_192;

export const tail = (captured: Uint8Array): string =>
  Buffer.from(
    captured.subarray(Math.max(0, captured.length - STDERR_TAIL_BYTES)),
  ).toString("utf8");

/**
 * Spawns `argv` with exactly `env`, capturing stdout and stderr in memory and
 * in `<logDirectory>/committee-<pid>.{stdout,stderr}.log`.
 */
export const spawnDaBondPoolCommitteeNode = async (input: {
  readonly argv: readonly [string, ...string[]];
  readonly env: Readonly<Record<string, string>>;
  readonly cwd: string;
  readonly logDirectory: string;
  readonly apiUrl: string;
  readonly readTimeoutMs?: number;
}): Promise<DaBondPoolCommitteeProcess> => {
  const [program, ...args] = input.argv;
  const child = spawn(program, args, {
    cwd: input.cwd,
    env: { ...input.env },
    stdio: ["ignore", "pipe", "pipe"],
  });
  const pid = await new Promise<number>((resolvePid, reject) => {
    child.once("spawn", () => resolvePid(child.pid!));
    child.once("error", reject);
  });
  const chunks: Buffer[] = [];
  let captured = new Uint8Array(0);
  const stdoutChunks: Buffer[] = [];
  let stdoutCaptured = new Uint8Array(0);
  const logs: WriteStream[] = [];
  const log = (stream: "stdout" | "stderr") => {
    const file = createWriteStream(
      join(input.logDirectory, `committee-${pid.toString()}.${stream}.log`),
      { flags: "a", mode: 0o600 },
    );
    logs.push(file);
    return file;
  };
  const stdoutLog = log("stdout");
  const stderrLog = log("stderr");
  child.stdout.on("data", (chunk: Buffer) => {
    stdoutLog.write(chunk);
    stdoutChunks.push(chunk);
    stdoutCaptured = new Uint8Array(0);
  });
  child.stderr.on("data", (chunk: Buffer) => {
    stderrLog.write(chunk);
    chunks.push(chunk);
    captured = new Uint8Array(0);
  });
  let exited: { code: number | null; signal: string | null } | undefined;
  const exit = new Promise<{ code: number | null; signal: string | null }>(
    (resolveExit) =>
      child.once("exit", (code, signal) => {
        exited = { code, signal };
        resolveExit(exited);
      }),
  );
  const stderr = (): Uint8Array => {
    if (captured.length === 0 && chunks.length > 0)
      captured = new Uint8Array(Buffer.concat(chunks));
    return captured;
  };
  const stdout = (): Uint8Array => {
    if (stdoutCaptured.length === 0 && stdoutChunks.length > 0)
      stdoutCaptured = new Uint8Array(Buffer.concat(stdoutChunks));
    return stdoutCaptured;
  };
  const closeLogs = async () => {
    await Promise.all(
      logs.map(
        (file) =>
          new Promise<void>((resolveClose) => file.end(() => resolveClose())),
      ),
    );
  };
  return {
    pid,
    alive: () => exited === undefined,
    stderr,
    stdout,
    readyz: async () => {
      const response = await fetch(`${input.apiUrl}/readyz`, {
        signal: AbortSignal.timeout(input.readTimeoutMs ?? 10_000),
      });
      return { httpStatus: response.status, body: await response.text() };
    },
    stop: async (boundMs) => {
      let killed = false;
      if (exited === undefined) {
        child.kill("SIGTERM");
        const done = await Promise.race([
          exit.then(() => true),
          pause(boundMs).then(() => false),
        ]);
        if (!done) {
          killed = true;
          child.kill("SIGKILL");
        }
      }
      const { code, signal } = await exit;
      await closeLogs();
      return { exitCode: code, signal, killed, stderrTail: tail(stderr()) };
    },
  };
};
