import { readdirSync, readFileSync } from "node:fs";

import { type Assets } from "@lucid-evolution/lucid";
import { Cause, Runtime } from "effect";

import type {
  DaBondPoolJourneyAttestResult,
  DaBondPoolJourneyBlockStatus,
} from "./da-bond-pool-journey.js";
import {
  DA_BOND_POOL_AWAIT_TIME_SLACK_MS,
  type DaBondJourneyHeldKey,
  DaBondJourneySigningMaterialError,
} from "./da-bond-pool-live-port.select-da-bond-pool-challenger-coins.js";

/**
 * The withdrawal quorum: `update_threshold` of the DA params owners, from the
 * keys the run holds, in owner order. Throws a signing-material error naming
 * every owner whose key the run lacks when fewer than the threshold are held.
 */
export const planDaBondOwnerQuorum = (input: {
  readonly owners: readonly string[];
  readonly updateThreshold: bigint;
  readonly held: readonly DaBondJourneyHeldKey[];
  readonly source: string;
}): readonly DaBondJourneyHeldKey[] => {
  const owners = [...new Set(input.owners)];
  if (
    input.updateThreshold < 1n ||
    input.updateThreshold > BigInt(owners.length)
  )
    throw new Error(
      `DA params update_threshold ${input.updateThreshold.toString()} cannot be met by ${owners.length.toString()} owner(s)`,
    );
  const held = new Map(input.held.map((key) => [key.keyHash, key]));
  const signers = owners.flatMap((owner) => {
    const key = held.get(owner);
    return key === undefined ? [] : [key];
  });
  if (BigInt(signers.length) < input.updateThreshold) {
    const missing = owners
      .filter((owner) => !held.has(owner))
      .map((owner) => `the signing key of DA params owner ${owner}`);
    throw new DaBondJourneySigningMaterialError(
      missing,
      `The withdrawal quorum needs ${input.updateThreshold.toString()} of ${owners.length.toString()} owners; ${input.source} holds ${signers.length.toString()} (${input.held.map((key) => `${key.role} ${key.keyHash}`).join(", ") || "none"}).`,
    );
  }
  return signers.slice(0, Number(input.updateThreshold));
};

/** One seed phrase of `secrets/journey-accounts.json`, or a named error. */
export const requireJourneySeed = (
  accounts: Readonly<
    Record<string, Readonly<{ seedPhrase?: string }> | undefined>
  >,
  role: string,
  source: string,
): string => {
  const seed = accounts[role]?.seedPhrase?.trim();
  if (seed === undefined || seed.length === 0)
    throw new DaBondJourneySigningMaterialError(
      [`the ${role} seedPhrase in ${source}`],
      `The journey signs with the ${role} key.`,
    );
  return seed;
};

/** The Apply refusals the SDK's pool precheck raises. */
export const DA_BOND_POOL_APPLY_REFUSAL_REASONS = Object.freeze([
  "pool-under-backed",
  "pool-withdrawing",
  "pool-unavailable",
] as const);

export type DaBondPoolApplyRefusal = Readonly<{
  reason: (typeof DA_BOND_POOL_APPLY_REFUSAL_REASONS)[number];
  message: string;
}>;

/**
 * The SDK's pool refusal of an Apply, wherever it sits: the error itself, a
 * failure of an Effect `FiberFailure` (what `Effect.runPromise` rejects with),
 * an `AggregateError` member or a `cause` link. Any other error is not a
 * refusal and yields undefined.
 */
export const daBondPoolApplyRefusal = (
  error: unknown,
): DaBondPoolApplyRefusal | undefined => {
  const seen = new Set<unknown>();
  const visit = (value: unknown): DaBondPoolApplyRefusal | undefined => {
    if (value === null || typeof value !== "object" || seen.has(value))
      return undefined;
    seen.add(value);
    const fields = value as {
      readonly _tag?: unknown;
      readonly reason?: unknown;
      readonly message?: unknown;
      readonly cause?: unknown;
    };
    if (
      fields._tag === "DaAttestationBuildError" &&
      typeof fields.reason === "string" &&
      (DA_BOND_POOL_APPLY_REFUSAL_REASONS as readonly string[]).includes(
        fields.reason,
      )
    )
      return {
        reason: fields.reason as DaBondPoolApplyRefusal["reason"],
        message: typeof fields.message === "string" ? fields.message : "",
      };
    if (Runtime.isFiberFailure(value)) {
      const cause = value[Runtime.FiberFailureCauseId];
      for (const failure of [
        ...Cause.failures(cause),
        ...Cause.defects(cause),
      ]) {
        const found = visit(failure);
        if (found !== undefined) return found;
      }
    }
    if (value instanceof AggregateError)
      for (const member of value.errors) {
        const found = visit(member);
        if (found !== undefined) return found;
      }
    return "cause" in fields ? visit(fields.cause) : undefined;
  };
  return visit(error);
};

/** A pool refusal as the driver's attest result; undefined for other errors. */
export const attestRefusalResult = (
  error: unknown,
): DaBondPoolJourneyAttestResult | undefined => {
  const refusal = daBondPoolApplyRefusal(error);
  return refusal === undefined
    ? undefined
    : {
        kind: "refused",
        reason:
          refusal.message.length > 0
            ? `${refusal.reason}: ${refusal.message}`
            : refusal.reason,
      };
};

/**
 * One running process: its pid, argument vector and environment
 * (`"unreadable"` when `/proc/<pid>/environ` could not be read).
 */
export type JourneyProcess = Readonly<{
  pid: number;
  argv: readonly string[];
  environ?: readonly string[] | "unreadable";
}>;

const WATCHER_CLI = /midgard-watcher[\\/]dist[\\/]cli\.js$/u;

export const COMMITTEE_NODE = /da-committee-node/u;

/**
 * Journey daemons that must not run: a watcher CLI
 * (`midgard-watcher/dist/cli.js`) or a committee node whose argument vector
 * or environment names a path inside the run directory, other than the pids
 * in `admitted` (ruling P27(5)). A daemon whose environment cannot be read
 * counts, since it may name the run. Each admitted pid must itself be a
 * running committee node; one that is not, or is gone, is reported too.
 */
export const findJourneyDaemons = (
  processes: readonly JourneyProcess[],
  runDirectory: string,
  admitted: ReadonlySet<number> = new Set(),
  selfPid: number = process.pid,
): readonly string[] => {
  const root = runDirectory.replace(/\/+$/u, "");
  const namesRun = (value: string) =>
    value === root || value.includes(`${root}/`);
  const found: string[] = [];
  for (const { pid, argv, environ } of processes) {
    if (pid === selfPid) continue;
    const shown = `pid ${pid.toString()}: ${argv.join(" ")}`;
    const committee = argv.some((arg) => COMMITTEE_NODE.test(arg));
    if (admitted.has(pid)) {
      if (!committee)
        found.push(`${shown} (admitted, but not a da-committee-node)`);
      continue;
    }
    const daemon = committee || argv.some((arg) => WATCHER_CLI.test(arg));
    if (!daemon) continue;
    if (environ === "unreadable") {
      found.push(`${shown} (environment unreadable)`);
      continue;
    }
    const inRun =
      argv.some((arg) => namesRun(arg)) ||
      (environ ?? []).some((entry) =>
        namesRun(entry.slice(entry.indexOf("=") + 1)),
      );
    if (inRun) found.push(shown);
  }
  const seen = new Set(processes.map(({ pid }) => pid));
  for (const pid of admitted)
    if (!seen.has(pid))
      found.push(`pid ${pid.toString()} (admitted, but not running)`);
  return found;
};

/**
 * Every readable process on Linux, with its environment; fails closed when
 * `/proc` is unreadable.
 */
export const readLinuxProcesses = (): readonly JourneyProcess[] => {
  let entries: string[];
  try {
    entries = readdirSync("/proc");
  } catch (cause) {
    throw new Error(
      "Cannot list processes from /proc to check that no watcher or committee daemon runs against the journey devnet",
      { cause },
    );
  }
  const nulSeparated = (path: string) =>
    readFileSync(path, "utf8")
      .split("\0")
      .filter((item) => item.length > 0);
  return entries.flatMap((entry): JourneyProcess[] => {
    if (!/^[0-9]+$/u.test(entry)) return [];
    let argv: string[];
    try {
      argv = nulSeparated(`/proc/${entry}/cmdline`);
    } catch {
      // The process exited between the listing and the read.
      return [];
    }
    if (argv.length === 0) return [];
    let environ: readonly string[] | "unreadable";
    try {
      environ = nulSeparated(`/proc/${entry}/environ`);
    } catch (error) {
      if ((error as NodeJS.ErrnoException).code === "ENOENT") return [];
      environ = "unreadable";
    }
    return [{ pid: Number(entry), argv, environ }];
  });
};

/** A block no longer in the queue merged iff the confirmed state is its header. */
export const absentBlockStatus = (
  headerHash: string,
  confirmedHeaderHash: string,
): Extract<DaBondPoolJourneyBlockStatus, "merged" | "removed"> =>
  headerHash === confirmedHeaderHash ? "merged" : "removed";

/**
 * A journey block's interval: it starts where its predecessor ended and ends
 * one minute from now, never at or before its start. The commit's validity
 * ends at `end_time + 1`, and the ledger bounds validity in whole one-second
 * slots, so `end_time` is always the last millisecond of a slot (the script
 * requires it to equal the commit's inclusive upper bound).
 */
export const nextJourneyBlockInterval = (input: {
  readonly predecessorEndTime: bigint;
  readonly nowMs: number;
}): Readonly<{ startTime: bigint; endTime: bigint }> => {
  const startTime = input.predecessorEndTime;
  const proposed = BigInt(input.nowMs + 59_999);
  return {
    startTime,
    endTime:
      proposed > startTime ? proposed : (startTime / 1000n + 1n) * 1000n + 999n,
  };
};

/** How long `awaitTime` lets the tip take to reach `targetMs`. */
export const awaitTimeBudgetMs = (
  targetMs: number,
  nowMs: number,
  slackMs: number = DA_BOND_POOL_AWAIT_TIME_SLACK_MS,
): number => Math.max(0, targetMs - nowMs) + slackMs;

/** One decoded transaction output. */
export type DaBondPoolJourneyOutput = Readonly<{
  address: string;
  assets: Assets;
  datum?: string | null;
}>;
