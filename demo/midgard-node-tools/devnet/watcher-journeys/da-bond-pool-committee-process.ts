/**
 * The real committee node behind the live pooled DA bond journey's P16
 * evidence (ruling P27): one unmodified `da-committee-node`, run from its
 * built `dist/index.js`, whose `GET /readyz` body and stderr pool events the
 * journey driver judges.
 *
 * The node submits to L1 (`DA_L1_SUBMISSION_ENABLED=true`), so its
 * coordinator hooks, its tick runner's pool read and its readiness pool
 * reasons all run in production form. It holds no DA signer key and never
 * receives a journey payload, so it cannot attest, answer an availability
 * challenge or Apply. Its two submitter keys are fresh, distinct from each
 * other and from every operational key; the unchanged-UTxO check proves it
 * spent nothing.
 *
 * Everything here that decides something is a pure function or takes its
 * clock, reads and process as dependencies, so both polarities are testable
 * without a node.
 */
import { spawn } from "node:child_process";
import { createHash } from "node:crypto";
import { createWriteStream, type WriteStream } from "node:fs";
import { isAbsolute, join } from "node:path";
import { setTimeout as pause } from "node:timers/promises";

import type { DaBondPoolJourneyStep } from "./da-bond-pool-journey.js";
import {
  createDaBondPoolStderrCursor,
  type DaBondPoolReadyz,
  parseDaBondPoolReadyz,
} from "./da-bond-pool-process-evidence.js";
import { isTransientTransportError } from "./ledger-tip.js";

// ---------------------------------------------------------------------------
// The environment
// ---------------------------------------------------------------------------

/**
 * Variables that would give the node a DA signer or let it fund itself; the
 * env builder refuses to spawn while any is set.
 */
export const DA_BOND_POOL_COMMITTEE_REFUSED_ENV =
  /^(DA_SIGNER_INDEX|DA_SIGNER_KEY_SOURCE(_.*)?|DA_L1_AUTO_FUND_KEY_SOURCE)$/u;

/** The variables the env builder sets itself; settings may not carry them. */
export const DA_BOND_POOL_COMMITTEE_OWNED_ENV = Object.freeze([
  "DA_L1_SUBMISSION_ENABLED",
  "DA_L1_PREFLIGHT_ENABLED",
  "L1_SUBMITTER_KEY_SOURCE",
  "DA_AVAILABILITY_SUBMITTER_KEY_SOURCE",
  "DA_AVAILABILITY_JOURNAL_PATH",
  "DA_COMMITTEE_DATABASE_URL",
  "DA_COMMITTEE_DB_PATH",
  "DA_COMMITTEE_API_HOST",
  "DA_COMMITTEE_API_PORT",
  "DA_COMMITTEE_POLL_INTERVAL_MS",
  "DA_LIBP2P_PRIVATE_KEY_SOURCE",
  "MIDGARD_CONFIG_MODE",
  "MIDGARD_DOTENV_MODE",
]);

/** Inherited variables the node may see; everything else is dropped. */
export const DA_BOND_POOL_INHERITED_ENV = Object.freeze([
  "PATH",
  "HOME",
  "LANG",
  "TMPDIR",
]);

/** Variables whose values are secrets, recorded as `<redacted>`. */
const SECRET_ENV = new Set(["DA_COMMITTEE_DATABASE_URL"]);
const INLINE_KEY_SOURCE = /^(seed|mnemonic|private-key|privateKey|hex):/u;

export class DaBondPoolCommitteeEnvError extends Error {
  constructor(problems: readonly string[]) {
    super(
      `Refusing to spawn the journey's committee node: ${problems.join("; ")}`,
    );
    this.name = "DaBondPoolCommitteeEnvError";
  }
}

/** One of the node's two submitter keys. */
export type DaBondPoolCommitteeKey = Readonly<{
  /** A key source the node reads, `file:<absolute path>`. */
  source: string;
  /** The key's payment key hash. */
  keyHash: string;
}>;

export type DaBondPoolCommitteeEnv = Readonly<{
  /** The child's whole environment. */
  env: Readonly<Record<string, string>>;
  /** The variables set beyond the inherited ones, secrets redacted. */
  recorded: Readonly<Record<string, string>>;
}>;

/**
 * The committee node's environment (P27(1)). Refuses a signer or auto-fund
 * variable, a port-owned variable in `settings`, equal submitter keys, a
 * submitter key equal to an operational key, and a relative journal path.
 */
export const buildDaBondPoolCommitteeEnv = (input: {
  /** Deployment, network and L1 source variables. */
  readonly settings: Readonly<Record<string, string>>;
  readonly l1Submitter: DaBondPoolCommitteeKey;
  readonly availabilitySubmitter: DaBondPoolCommitteeKey;
  /** Every operational key hash, by role. */
  readonly operationalKeyHashes: Readonly<Record<string, string>>;
  readonly libp2pKeySource: string;
  readonly journalPath: string;
  readonly databaseUrl: string;
  readonly apiHost: string;
  readonly apiPort: number;
  readonly pollIntervalMs: number;
  readonly inherited?: Readonly<Record<string, string | undefined>>;
}): DaBondPoolCommitteeEnv => {
  const problems: string[] = [];
  const settingNames = Object.keys(input.settings);
  for (const name of settingNames.filter((name) =>
    DA_BOND_POOL_COMMITTEE_REFUSED_ENV.test(name),
  ))
    problems.push(
      `${name} is set; the node must hold no signer or auto-fund key`,
    );
  for (const name of settingNames.filter((name) =>
    DA_BOND_POOL_COMMITTEE_OWNED_ENV.includes(name),
  ))
    problems.push(`${name} is set by the port, not by settings`);
  const { l1Submitter, availabilitySubmitter } = input;
  if (
    l1Submitter.keyHash === availabilitySubmitter.keyHash ||
    l1Submitter.source === availabilitySubmitter.source
  )
    problems.push(
      "the L1 submitter and availability submitter keys are the same key",
    );
  for (const [label, key] of [
    ["L1 submitter", l1Submitter],
    ["availability submitter", availabilitySubmitter],
  ] as const)
    for (const [role, keyHash] of Object.entries(input.operationalKeyHashes))
      if (keyHash === key.keyHash)
        problems.push(`the ${label} key is the ${role} key`);
  if (!isAbsolute(input.journalPath))
    problems.push(`the journal path ${input.journalPath} is not absolute`);
  if (problems.length > 0) throw new DaBondPoolCommitteeEnvError(problems);

  const set: Record<string, string> = {
    ...input.settings,
    DA_L1_SUBMISSION_ENABLED: "true",
    DA_L1_PREFLIGHT_ENABLED: "true",
    L1_SUBMITTER_KEY_SOURCE: l1Submitter.source,
    DA_AVAILABILITY_SUBMITTER_KEY_SOURCE: availabilitySubmitter.source,
    DA_AVAILABILITY_JOURNAL_PATH: input.journalPath,
    DA_COMMITTEE_DATABASE_URL: input.databaseUrl,
    DA_COMMITTEE_API_HOST: input.apiHost,
    DA_COMMITTEE_API_PORT: input.apiPort.toString(),
    DA_COMMITTEE_POLL_INTERVAL_MS: input.pollIntervalMs.toString(),
    DA_LIBP2P_PRIVATE_KEY_SOURCE: input.libp2pKeySource,
    MIDGARD_CONFIG_MODE: "disabled",
    MIDGARD_DOTENV_MODE: "disabled",
  };
  const inherited = Object.fromEntries(
    DA_BOND_POOL_INHERITED_ENV.flatMap((name) => {
      const value = input.inherited?.[name];
      return value === undefined ? [] : [[name, value]];
    }),
  );
  return {
    env: { ...inherited, ...set },
    recorded: redactDaBondPoolEnv(set),
  };
};

/** `env` with every secret value replaced by `<redacted>`. */
export const redactDaBondPoolEnv = (
  env: Readonly<Record<string, string>>,
  secretNames: ReadonlySet<string> = SECRET_ENV,
): Readonly<Record<string, string>> =>
  Object.fromEntries(
    Object.entries(env).map(([name, value]) => [
      name,
      secretNames.has(name) || INLINE_KEY_SOURCE.test(value)
        ? "<redacted>"
        : value,
    ]),
  );

/**
 * A port in 20000..39999 derived from the worktree path and a purpose, so two
 * worktrees running the journey at once do not collide.
 */
export const worktreeDerivedPort = (
  worktree: string,
  purpose: string,
): number =>
  20_000 +
  (createHash("sha256")
    .update(`${worktree}\0${purpose}`)
    .digest()
    .readUInt32BE(0) %
    20_000);

// ---------------------------------------------------------------------------
// Agreement with the port's pool snapshot
// ---------------------------------------------------------------------------

/** What the node must report for the pool the port read. */
export type DaBondPoolCommitteeExpectedView = Readonly<{
  short: boolean;
  backing: bigint;
  withdrawing: boolean;
  unlockAt?: bigint;
}>;

/** The expected view of a pool snapshot; undefined when there is no pool. */
export const daBondPoolCommitteeExpectedView = (
  snapshot: Readonly<{
    state: "bonded" | "withdrawing" | "missing";
    backing: bigint;
    unlockAt?: number;
  }>,
  daBondLovelace: bigint,
): DaBondPoolCommitteeExpectedView | undefined =>
  snapshot.state === "missing"
    ? undefined
    : {
        short: snapshot.backing < daBondLovelace,
        backing: snapshot.backing,
        withdrawing: snapshot.state === "withdrawing",
        ...(snapshot.unlockAt === undefined
          ? {}
          : { unlockAt: BigInt(snapshot.unlockAt) }),
      };

const SHORT_REASON =
  /^da_bond_pool_backing_short: backing=(\d+), required=(\d+), checkedAt=(\S+)$/u;
const WITHDRAWING_REASON =
  /^da_bond_pool_withdrawing: unlockAt=(\d+|unknown), checkedAt=(\S+)$/u;

/** The `checkedAt` of each pool reason, as POSIX ms. */
const reasonCheckedAt = (reasons: readonly string[]): number[] =>
  reasons.flatMap((reason) => {
    const at =
      SHORT_REASON.exec(reason)?.[3] ?? WITHDRAWING_REASON.exec(reason)?.[2];
    const ms = at === undefined ? Number.NaN : Date.parse(at);
    return Number.isNaN(ms) ? [] : [ms];
  });

/**
 * The node's pool reasons are exactly the expected view: the short reason
 * iff the pool is short, carrying its backing; the withdrawing reason iff it
 * is Withdrawing, carrying its unlock time; nothing else.
 */
export const daBondPoolCommitteeViewAgrees = (
  poolReasons: readonly string[],
  expected: DaBondPoolCommitteeExpectedView | undefined,
): boolean => {
  if (expected === undefined) return false;
  const short = poolReasons.map((reason) => SHORT_REASON.exec(reason));
  const withdrawing = poolReasons.map((reason) =>
    WITHDRAWING_REASON.exec(reason),
  );
  if (poolReasons.some((_, index) => !short[index] && !withdrawing[index]))
    return false;
  const shortFound = short.filter((match) => match !== null);
  const withdrawingFound = withdrawing.filter((match) => match !== null);
  const shortAgrees = expected.short
    ? shortFound.length === 1 &&
      shortFound[0]![1] === expected.backing.toString()
    : shortFound.length === 0;
  const withdrawingAgrees = expected.withdrawing
    ? withdrawingFound.length === 1 &&
      expected.unlockAt !== undefined &&
      withdrawingFound[0]![1] === expected.unlockAt.toString()
    : withdrawingFound.length === 0;
  return shortAgrees && withdrawingAgrees;
};

/** One `/readyz` answer, verbatim. */
export type DaBondPoolReadyzRead = Readonly<{
  httpStatus: number;
  body: string;
}>;

export type DaBondPoolCommitteeSync = Readonly<{
  read: DaBondPoolReadyzRead;
  readyz: DaBondPoolReadyz;
  /** A pool read that started after `since` is in the answer. */
  fresh: boolean;
  /** Fresh and agreeing with the expected view. */
  synced: boolean;
  reads: number;
}>;

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
 * or body the node did answer is judged as it is. An answer whose L1 source
 * is quarantined returns at once: the node never leaves that state.
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
      readyz.l1Source?.status === "quarantined" ||
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

const STDERR_TAIL_BYTES = 8_192;

const tail = (captured: Uint8Array): string =>
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

// ---------------------------------------------------------------------------
// The observer
// ---------------------------------------------------------------------------

/** What one observation of the node returns to the journey driver. */
export type DaBondPoolCommitteeObservation = Readonly<{
  readinessReasons: readonly string[];
  events: readonly string[];
  process: Readonly<{
    pid: number;
    readyzHttpStatus: number;
    readyzBody: string;
    eventPids: readonly number[];
    /**
     * The distinct `availability_responder` JSON lines this pid wrote since
     * the previous observation, stderr's (failed, unavailable) before
     * stdout's (pending, included, confirmed, each with its `action`), each
     * in first-seen order (P27(2)).
     */
    availabilityResponder: readonly string[];
  }>;
  synced: boolean;
  fresh: boolean;
}>;

/** A start, stop or observation the port records as an artifact. */
export type DaBondPoolCommitteeRecord = Readonly<
  | { kind: "start"; pid: number; env: Readonly<Record<string, string>> }
  | ({ kind: "stop"; pid: number } & DaBondPoolCommitteeExit)
  | {
      kind: "observe";
      pid: number;
      readyzHttpStatus: number;
      readyzBody: string;
      events: readonly string[];
      availabilityResponder: readonly string[];
      synced: boolean;
      fresh: boolean;
      reads: number;
    }
>;

export class DaBondPoolCommitteeProcessError extends Error {
  constructor(message: string, options?: ErrorOptions) {
    super(message, options);
    this.name = "DaBondPoolCommitteeProcessError";
  }
}

/**
 * The node's lifecycle (P27(3)): start, observe, stop, teardown. Every
 * failure throws, so the step it happens in fails; there is no fallback.
 *
 * - `start`: the submitter UTxOs are the baseline (the first start records
 *   it), no journey daemon runs, the node spawns, then the running daemons
 *   must be exactly its pid and it must answer `/readyz`.
 * - `observe`: the node is alive, `/readyz` is polled until it agrees with
 *   the port's pool snapshot (or the bound passes), and the stderr pool
 *   events and the availability responder reports on stderr and stdout
 *   since the last observation are taken, tied to the pid. An answer whose
 *   L1 source is quarantined is recorded, then fails the observation: the
 *   node reads no L1 again and exits once its L1 view goes stale.
 * - `stop`: SIGTERM, exit 0 required; the submitter UTxOs stay unchanged
 *   for `daBondPoolCommitteeStopSettleMs(nodeCadence)` after the exit.
 */
export const createDaBondPoolCommitteeObserver = (deps: {
  readonly spawn: () => Promise<DaBondPoolCommitteeProcess>;
  /** Throws unless the journey daemons are exactly `admitted`. */
  readonly checkDaemons: (admitted: ReadonlySet<number>) => void;
  /** Both submitter addresses' UTxO outrefs. */
  readonly submitterUtxos: () => Promise<readonly string[]>;
  readonly expectedView: () => Promise<
    DaBondPoolCommitteeExpectedView | undefined
  >;
  readonly env: Readonly<Record<string, string>>;
  readonly record?: (entry: DaBondPoolCommitteeRecord) => Promise<void>;
  readonly syncTimeoutMs: number;
  readonly startTimeoutMs: number;
  readonly pollMs: number;
  readonly stopBoundMs: number;
  /**
   * The node's poll interval and the chain's finality cadence. The observer
   * derives from them how long after an exit the submitter addresses are
   * watched (`daBondPoolCommitteeStopSettleMs`). A transaction the node
   * submitted in its last tick can still be in the mempool or unindexed
   * when it exits, and the stop after step 6 has no later read to catch it.
   */
  readonly nodeCadence: Parameters<typeof daBondPoolCommitteeStopSettleMs>[0];
  readonly now?: () => number;
  readonly sleep?: (ms: number) => Promise<unknown>;
}) => {
  const now = deps.now ?? Date.now;
  const sleep = deps.sleep ?? pause;
  const record = deps.record ?? (async () => {});
  const stopSettleMs = daBondPoolCommitteeStopSettleMs(deps.nodeCadence);
  let running:
    | Readonly<{
        node: DaBondPoolCommitteeProcess;
        cursor: ReturnType<typeof createDaBondPoolStderrCursor>;
        responderCursor: ReturnType<typeof createDaBondPoolStderrCursor>;
        stdoutResponderCursor: ReturnType<typeof createDaBondPoolStderrCursor>;
      }>
    | undefined;
  let baseline: readonly string[] | undefined;

  const requireUnchangedUtxos = async (when: string) => {
    const current = await deps.submitterUtxos();
    if (baseline === undefined) {
      baseline = current;
      return;
    }
    const change = daBondPoolSubmitterUtxoChange(baseline, current);
    if (change !== undefined)
      throw new DaBondPoolCommitteeProcessError(
        `The committee node's submitter addresses changed ${when}: ${change}`,
      );
  };

  const admitted = (): ReadonlySet<number> =>
    new Set(running === undefined ? [] : [running.node.pid]);

  const requireAlive = (node: DaBondPoolCommitteeProcess) => {
    if (!node.alive())
      throw new DaBondPoolCommitteeProcessError(
        `The committee node (pid ${node.pid.toString()}) exited; stderr: ${tail(node.stderr())}`,
      );
  };

  return {
    admitted,
    running: () => running !== undefined,

    start: async (): Promise<number> => {
      if (running !== undefined)
        throw new DaBondPoolCommitteeProcessError(
          `The committee node already runs (pid ${running.node.pid.toString()})`,
        );
      await requireUnchangedUtxos("before its start");
      deps.checkDaemons(new Set());
      const node = await deps.spawn();
      running = {
        node,
        cursor: createDaBondPoolStderrCursor(node.pid),
        responderCursor: createDaBondPoolStderrCursor(
          node.pid,
          (event) => event === DA_BOND_POOL_RESPONDER_EVENT,
        ),
        stdoutResponderCursor: createDaBondPoolStderrCursor(
          node.pid,
          (event) => event === DA_BOND_POOL_RESPONDER_EVENT,
        ),
      };
      await record({ kind: "start", pid: node.pid, env: deps.env });
      deps.checkDaemons(admitted());
      const deadline = now() + deps.startTimeoutMs;
      for (;;) {
        requireAlive(node);
        try {
          await node.readyz();
          return node.pid;
        } catch (cause) {
          if (now() >= deadline)
            throw new DaBondPoolCommitteeProcessError(
              `The committee node (pid ${node.pid.toString()}) never answered /readyz`,
              { cause },
            );
        }
        await sleep(deps.pollMs);
      }
    },

    observe: async (): Promise<DaBondPoolCommitteeObservation> => {
      if (running === undefined)
        throw new DaBondPoolCommitteeProcessError(
          "The committee node is not running",
        );
      const { node, cursor, responderCursor, stdoutResponderCursor } = running;
      requireAlive(node);
      const since = now();
      const expected = await deps.expectedView();
      const sync = await awaitDaBondPoolCommitteeSync({
        read: node.readyz,
        alive: node.alive,
        expected,
        since,
        timeoutMs: deps.syncTimeoutMs,
        pollMs: deps.pollMs,
        now,
        sleep,
      });
      requireAlive(node);
      const captured = node.stderr();
      const events = cursor.take(captured);
      const availabilityResponder = [
        ...new Set(
          [
            ...responderCursor.take(captured),
            ...stdoutResponderCursor.take(node.stdout()),
          ].map(({ line }) => line),
        ),
      ];
      const observation: DaBondPoolCommitteeObservation = {
        readinessReasons: sync.readyz.poolReasons,
        events: events.map((event) => event.event),
        process: {
          pid: node.pid,
          readyzHttpStatus: sync.read.httpStatus,
          readyzBody: sync.read.body,
          eventPids: events.map((event) => event.pid),
          availabilityResponder,
        },
        synced: sync.synced,
        fresh: sync.fresh,
      };
      await record({
        kind: "observe",
        pid: node.pid,
        readyzHttpStatus: sync.read.httpStatus,
        readyzBody: sync.read.body,
        events: observation.events,
        availabilityResponder,
        synced: sync.synced,
        fresh: sync.fresh,
        reads: sync.reads,
      });
      const l1Source = sync.readyz.l1Source;
      if (l1Source?.status === "quarantined")
        throw new DaBondPoolCommitteeProcessError(
          `The committee node (pid ${node.pid.toString()}) quarantined its L1 source: ${l1Source.quarantineReason ?? "no reason given"}; /readyz: ${sync.read.body}`,
        );
      return observation;
    },

    stop: async (): Promise<DaBondPoolCommitteeExit> => {
      if (running === undefined)
        throw new DaBondPoolCommitteeProcessError(
          "The committee node is not running",
        );
      const { node } = running;
      running = undefined;
      const exit = await node.stop(deps.stopBoundMs);
      await record({ kind: "stop", pid: node.pid, ...exit });
      if (exit.exitCode !== 0 || exit.killed)
        throw new DaBondPoolCommitteeProcessError(
          `The committee node (pid ${node.pid.toString()}) did not exit 0 on SIGTERM: code ${String(exit.exitCode)}, signal ${String(exit.signal)}${exit.killed ? ", killed" : ""}; stderr: ${exit.stderrTail}`,
        );
      const settled = now() + stopSettleMs;
      for (;;) {
        await requireUnchangedUtxos("while it ran");
        if (now() >= settled) break;
        await sleep(deps.pollMs);
      }
      deps.checkDaemons(new Set());
      return exit;
    },

    /** SIGTERM, then SIGKILL after the bound; never throws. */
    teardown: async (): Promise<DaBondPoolCommitteeExit | undefined> => {
      if (running === undefined) return undefined;
      const { node } = running;
      running = undefined;
      try {
        const exit = await node.stop(deps.stopBoundMs);
        await record({ kind: "stop", pid: node.pid, ...exit }).catch(
          () => undefined,
        );
        return exit;
      } catch {
        return undefined;
      }
    },
  };
};

export type DaBondPoolCommitteeObserver = ReturnType<
  typeof createDaBondPoolCommitteeObserver
>;
