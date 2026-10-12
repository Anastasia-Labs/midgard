import { createHash } from "node:crypto";
import { isAbsolute } from "node:path";

import { type DaBondPoolReadyz } from "./da-bond-pool-process-evidence.js";

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

export const WITHDRAWING_REASON =
  /^da_bond_pool_withdrawing: unlockAt=(\d+|unknown), checkedAt=(\S+)$/u;

/** The `checkedAt` of each pool reason, as POSIX ms. */
export const reasonCheckedAt = (reasons: readonly string[]): number[] =>
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
