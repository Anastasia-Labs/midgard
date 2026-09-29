/**
 * The live devnet adapter for the pooled DA bond journey (ticket #692): a
 * `DaBondPoolJourneyPort` over a journey run directory's process devnet, so
 * the driver in `da-bond-pool-journey.ts` walks its six steps against real
 * blocks, real Kupo and Ogmios, and the deployed validators.
 *
 * Each port method runs the production code path for its action:
 *
 * - commits, attestations and their confirmation: the published-block actor
 *   (`midgard-watcher/tests/support/published-block-actor`), with the
 *   journey's operator and cosigner as the local DA signers. An Apply the SDK
 *   builder refuses with `pool-under-backed` or `pool-withdrawing` is reported
 *   as a refusal carrying that reason; every other failure throws;
 * - Open, responses, settlements, Close, Timeout and the follow-up removal:
 *   the availability command flow of `midgard-node availability-challenge`
 *   (#690), composed in process from its exported steps: the canonical
 *   Kupo/Ogmios source, the durable operation journal, reconciliation before
 *   every action, a snapshot bracketed by two equal canonical points, the
 *   command's action planner and transaction builder. The composed form is
 *   needed because the CLI entry point resolves only Mainnet, Preprod and
 *   Preview; the devnet is `Custom`;
 * - top-up and the withdrawal quorum (build, one witness per owner and the
 *   fee payer, assemble): the real `midgard-node da-bond` CLI, run as
 *   processes bracketed by `da-bond status` processes (P18,
 *   `da-bond-pool-cli-process.ts`); each chain is the step's `cli` evidence,
 *   and its transaction is confirmed by the adapter's own pool read. The
 *   `da-bond` CLI admits the devnet's `Custom` network (P25, #691) with the
 *   node's own Ogmios-derived slot mapping;
 * - the pool snapshot: `da-bond status` over a `DaBondContext` built as
 *   `loadDaBondContext` builds it, with the devnet's slot configuration and
 *   the ledger tip as its clock. That context never submits;
 * - alerts: the watcher's `deriveWatcherDaBondPoolObservation` over its
 *   authenticated pool read, and the committee view of one real
 *   `da-committee-node` process (P16, P27, `da-bond-pool-committee-process.ts`):
 *   the reasons and the verbatim body of its `GET /readyz`, and the pool
 *   events on its stderr, tied to its pid. Each observation first waits,
 *   within a bound, until the node has read the pool after the call and
 *   agrees with the adapter's snapshot.
 *
 * The committee node (ruling P27) runs from its built `dist/index.js` with L1
 * submission on and preflight on, no DA signer key, no auto-fund key, and two
 * fresh submitter keys distinct from each other and from every operational
 * key. It never holds a journey payload, so it cannot attest, answer the
 * withheld block or Apply: its availability responder must report B1's
 * challenge `unavailable` on stderr and never act on it. It starts before
 * step 1, stops (exit 0 required) before step 2's commit so its payload-free
 * settle and Close cannot race B3, restarts before step 6 and stops again,
 * with the same checks, at the end of step 6. Both submitter addresses must
 * hold the same UTxOs before each start and after each stop. Each
 * observation's wait for the node to read the pool is bounded by ten of its
 * polls plus twice the ideal time for the release confirmation depth. Its DA runtime manifest and
 * libp2p keys (ruling P31, `da-bond-pool-committee-runtime.ts`) come from the
 * real `midgard-node da-libp2p-generate-manifest --target committee` process
 * over fresh keys, before the first transaction; it loads one member's libp2p
 * identity, never that member's DA signing key. If the runtime cannot be
 * produced, or the node's own configuration loader or peer check refuses it,
 * the adapter refuses to start (`DaBondPoolCommitteeUnavailableError`).
 *
 * Preconditions, all checked before the first transaction:
 *
 * - no watcher or DA committee daemon runs against the run directory, by
 *   argument vector or environment, other than the adapter's own committee
 *   node (checked before it starts, after each start, before each step and
 *   before each Open): a watcher would contest the journey's challenges, and
 *   a committee node holding the payload would answer the withheld block;
 * - Kupo indexes every address (`*`), which the Open's commitment recovery
 *   and the challenge snapshots read;
 * - the state queue holds only its root. The journey appends B1 as the head
 *   (`root.next`), because only the head can be removed after its availability
 *   Timeout, so it needs a freshly deployed run directory;
 * - the DA params owners the quorum needs are keys this run holds (the
 *   journey operator and cosigner); anything missing is named in a
 *   `DaBondJourneySigningMaterialError`.
 *
 * The challenger is a fresh key kept in `secrets/da-bond-pool-challenger.seed`
 * (mode 0600), distinct from every operational key, funded from the journey's
 * availability account with one exact Open coin per challenge, one collateral
 * coin that covers the worst Timeout fee at the ledger's collateral percentage
 * (G9), and one operating coin for the removal fee and the capital checks.
 *
 * Nothing here enforces operator liveness: `max_inactivity` and the scheduler
 * shift act only through strike and advance transactions, and no process in a
 * journey devnet submits them, so the long waits (the response deadline and
 * the withdraw delay) need no keep-alive commits.
 */
import { execFileSync } from "node:child_process";
import { randomBytes } from "node:crypto";
import {
  existsSync,
  mkdirSync,
  readdirSync,
  readFileSync,
  statSync,
  writeFileSync,
} from "node:fs";
import { join, resolve } from "node:path";
import { setTimeout as pause } from "node:timers/promises";
import { fileURLToPath } from "node:url";

import { openAvailabilityOperationJournal } from "@al-ft/midgard-core/availability-operation-journal";
import { depositEventsRetainedBlock } from "@al-ft/midgard-fault-proofs/test-support/transition-trace-retained";
import * as SDK from "@al-ft/midgard-sdk";
import {
  type Assets,
  CML,
  coreToTxOutput,
  credentialToAddress,
  Data,
  generateSeedPhrase,
  Lucid,
  paymentCredentialOf,
  toUnit,
  type UTxO,
} from "@lucid-evolution/lucid";
import { createScalusEvaluator } from "@lucid-evolution/scalus-uplc";
import {
  DA_L1_SUBMITTER_MIN_PLAIN_ADA_LOVELACE,
  DEFAULT_L1_SUBMITTER_PREFLIGHT,
} from "da-committee-node/config";
import { Cause, Effect, Runtime } from "effect";
import {
  availabilityTimeoutCollateralLovelace,
  buildAvailabilityCommandTransaction,
  planAvailabilityCommandAction,
} from "midgard-node/commands/availability-challenge";
import {
  authenticatedManifestReference,
  availabilityDeploymentFromManifest,
  availabilityParametersFromManifest,
  manifestReferenceScriptAuthPolicy,
  mintingValidatorOf,
  spendingValidatorOf,
} from "midgard-node/commands/availability-challenge-deployment";
import { availabilityCommandCanonicalSource } from "midgard-node/commands/availability-challenge-source";
import {
  type DaBondContext,
  daBondStatusCommand,
} from "midgard-node/commands/da-bond";
import { daLocalSigners } from "midgard-node/da/local-signers";
import {
  authenticWatcherDaBondPool,
  deriveWatcherDaBondPoolObservation,
} from "midgard-watcher";
import { createPublishedWatcherBlockActor } from "midgard-watcher/tests/support/published-block-actor";

import { writeJourneyArtifact, writeJourneyFile } from "./artifacts.js";
import {
  createDaBondPoolCli,
  DaBondCliProcessError,
  spawnDaBondCliProcess,
} from "./da-bond-pool-cli-process.js";
import {
  buildDaBondPoolCommitteeEnv,
  createDaBondPoolCommitteeObserver,
  DA_BOND_POOL_INHERITED_ENV,
  daBondPoolCommitteeExpectedView,
  daBondPoolCommitteeLifecycle,
  daBondPoolCommitteeSyncBoundMs,
  spawnDaBondPoolCommitteeNode,
  worktreeDerivedPort,
} from "./da-bond-pool-committee-process.js";
import {
  type DaBondPoolCommitteeRuntimeEvidence,
  daBondPoolCommitteeSettings,
  planDaBondPoolCommitteeRuntime,
  produceDaBondPoolCommitteeRuntime,
  readWorktreePortOffset,
  spawnDaBondPoolRuntimeProcess,
  verifyDaBondPoolCommitteeRuntime,
} from "./da-bond-pool-committee-runtime.js";
import type {
  DaBondPoolJourneyAlerts,
  DaBondPoolJourneyAttestResult,
  DaBondPoolJourneyBlockStatus,
  DaBondPoolJourneyParams,
  DaBondPoolJourneyPort,
  DaBondPoolJourneySnapshot,
  DaBondPoolJourneyStep,
  DaBondPoolJourneyTimeoutResult,
} from "./da-bond-pool-journey.js";
import { readJourneyCadence } from "./journey-timing.js";
import { awaitLedgerTipSlot, readOgmiosTipSlot } from "./ledger-tip.js";
import {
  JOURNEY_ACTION_DEPTH,
  type loadJourneyContext,
} from "./live-context.js";

export type LiveJourneyContext = Awaited<ReturnType<typeof loadJourneyContext>>;

/** Where the journey keeps its journal, payloads, withdraw files and record. */
export const daBondPoolJourneyDirectory = (runDirectory: string): string =>
  join(runDirectory, "work/journeys/da-bond-pool");

/** The challenger's mnemonic, relative to the run directory. */
export const DA_BOND_POOL_CHALLENGER_SECRET =
  "secrets/da-bond-pool-challenger.seed";

/** The run's signing material, relative to the run directory. */
export const JOURNEY_ACCOUNTS_SECRET = "secrets/journey-accounts.json";

/** The operating coin the challenger starts with: removal fee and change. */
export const DA_BOND_POOL_CHALLENGER_OPERATING_LOVELACE = 250_000_000n;

/**
 * Collateral held above the G9 minimum, so the collateral return of every
 * availability action stays above the ledger's minimum UTxO value.
 */
export const DA_BOND_POOL_COLLATERAL_MARGIN_LOVELACE = 5_000_000n;

/** Extra time `awaitTime` allows the tip beyond the wait itself. */
export const DA_BOND_POOL_AWAIT_TIME_SLACK_MS = 15 * 60_000;

const INCLUSION_TIMEOUT_MS = 12 * 60_000;
const JOURNAL_QUIET_TIMEOUT_MS = 15 * 60_000;
const ACTION_DEPTH_TIMEOUT_MS = 10 * 60_000;
const POLL_MS = 2_000;
const MAX_TRANSIENT_RETRIES = 30;
const MAX_RESPONSE_TRANSACTIONS = 64;
const MAX_SETTLEMENTS = 32;
const MAX_REMOVAL_STEPS = 4;

// ---------------------------------------------------------------------------
// Pure pieces (unit tested in da-bond-pool-live-port.test.ts)
// ---------------------------------------------------------------------------

/** Kupo and Ogmios of a run, from its `run.env` ports. */
export const journeyEndpointsFromRunEnv = (
  runEnv: Readonly<Record<string, string | undefined>>,
): Readonly<{ kupoUrl: string; ogmiosUrl: string }> => {
  const port = (name: string): number => {
    const raw = runEnv[name]?.trim() ?? "";
    const value = Number(raw);
    if (!/^[1-9][0-9]*$/u.test(raw) || value > 65_535)
      throw new Error(`run.env ${name} must be a TCP port, got "${raw}"`);
    return value;
  };
  return {
    kupoUrl: `http://127.0.0.1:${port("MIDGARD_PHASE4_KUPO_PORT")}`,
    ogmiosUrl: `http://127.0.0.1:${port("MIDGARD_PHASE4_OGMIOS_PORT")}`,
  };
};

/** Kupo's `/patterns` answer covers every address. */
export const kupoMatchesEverything = (patterns: unknown): boolean =>
  Array.isArray(patterns) && patterns.includes("*");

/** The deployment's journey parameters from its manifest values. */
export const daBondPoolJourneyParamsOf = (
  parameters: SDK.DaAvailabilityParameters,
  timing: Readonly<{
    da_bond_withdraw_delay_ms: number | bigint | string;
    da_attestation_timeout_ms: number | bigint | string;
  }>,
): DaBondPoolJourneyParams => {
  const ms = (value: number | bigint | string, name: string): number => {
    const result = Number(value);
    if (!Number.isSafeInteger(result) || result <= 0)
      throw new Error(`Deployment timing ${name} must be a positive integer`);
    return result;
  };
  return {
    daBond: parameters.da_bond_lovelace,
    penalty: parameters.da_slash_penalty_lovelace,
    floor: parameters.da_bond_pool_floor_lovelace,
    minTopUp: parameters.da_bond_min_top_up_lovelace,
    maxTimeoutFee: parameters.max_timeout_fee_lovelace,
    challengeRecordLovelace: parameters.challenge_record_lovelace,
    withdrawDelayMs: ms(
      timing.da_bond_withdraw_delay_ms,
      "da_bond_withdraw_delay_ms",
    ),
    attestationTimeoutMs: ms(
      timing.da_attestation_timeout_ms,
      "da_attestation_timeout_ms",
    ),
  };
};

/** What the challenger wallet must hold before the journey starts. */
export type DaBondPoolChallengerFundingPlan = Readonly<{
  /** `challenger_bond + challenge_record + max_open_fee`: the SDK Open builder takes exactly this. */
  openCoinLovelace: bigint;
  /** One exact Open coin per challenge: B1 (timed out) and B3 (answered). */
  openCoins: number;
  /** G9: `collateral% x (penalty + max_timeout_fee)`, rounded up. */
  timeoutCollateralLovelace: bigint;
  /** The collateral coin: the G9 minimum plus a margin for its return. */
  collateralLovelace: bigint;
  /** Pays the removal fee and backs the removal-capital check. */
  operatingLovelace: bigint;
}>;

export const planDaBondPoolChallengerFunding = (input: {
  readonly parameters: SDK.DaAvailabilityParameters;
  readonly collateralPercentage: number;
  readonly openCoins?: number;
  readonly operatingLovelace?: bigint;
}): DaBondPoolChallengerFundingPlan => {
  const openCoins = input.openCoins ?? 2;
  if (!Number.isSafeInteger(openCoins) || openCoins < 1)
    throw new Error("The challenger needs at least one Open coin");
  const timeoutCollateralLovelace = availabilityTimeoutCollateralLovelace({
    parameters: input.parameters,
    collateralPercentage: input.collateralPercentage,
  });
  const operatingLovelace =
    input.operatingLovelace ?? DA_BOND_POOL_CHALLENGER_OPERATING_LOVELACE;
  const minimumOperating =
    4n * input.parameters.max_timeout_fee_lovelace + 5_000_000n;
  if (operatingLovelace < minimumOperating)
    throw new Error(
      `The challenger operating coin must hold at least ${minimumOperating.toString()} lovelace`,
    );
  return {
    openCoinLovelace:
      input.parameters.challenger_bond_lovelace +
      input.parameters.challenge_record_lovelace +
      input.parameters.max_open_fee_lovelace,
    openCoins,
    timeoutCollateralLovelace,
    collateralLovelace:
      timeoutCollateralLovelace + DA_BOND_POOL_COLLATERAL_MARGIN_LOVELACE,
    operatingLovelace,
  };
};

const outRefOf = (utxo: Pick<UTxO, "txHash" | "outputIndex">): string =>
  `${utxo.txHash}#${utxo.outputIndex.toString()}`;

const isPlainAda = (utxo: UTxO, address: string): boolean =>
  utxo.address === address &&
  utxo.datum == null &&
  utxo.datumHash == null &&
  utxo.scriptRef == null &&
  Object.keys(utxo.assets).every((unit) => unit === "lovelace") &&
  (utxo.assets.lovelace ?? 0n) > 0n;

/** The challenger's coins by role; each is absent when the wallet lacks it. */
export type DaBondPoolChallengerCoins = Readonly<{
  collateral?: UTxO;
  openFunding?: UTxO;
  operating?: UTxO;
}>;

/**
 * Picks the challenger's coins: an exact Open coin, a collateral coin covering
 * G9 (the planned one first), and the largest other plain coin as operating
 * capital. Reserved outrefs (inputs of journaled intents not yet confirmed)
 * are never spent. The planned collateral coin stays eligible while reserved:
 * the adapter only ever posts it as collateral, and the journal admits one
 * actor's collateral in several unconfirmed intents.
 */
export const selectDaBondPoolChallengerCoins = (input: {
  readonly utxos: readonly UTxO[];
  readonly address: string;
  readonly plan: DaBondPoolChallengerFundingPlan;
  readonly reserved?: ReadonlySet<string>;
}): DaBondPoolChallengerCoins => {
  const { plan } = input;
  const lovelace = (utxo: UTxO) => utxo.assets.lovelace ?? 0n;
  const ascending = (a: UTxO, b: UTxO) =>
    lovelace(a) === lovelace(b)
      ? outRefOf(a) < outRefOf(b)
        ? -1
        : 1
      : lovelace(a) < lovelace(b)
        ? -1
        : 1;
  const owned = input.utxos
    .filter((utxo) => isPlainAda(utxo, input.address))
    .sort(ascending);
  const plain = owned.filter(
    (utxo) => !(input.reserved?.has(outRefOf(utxo)) ?? false),
  );
  const notOpen = plain.filter(
    (utxo) => lovelace(utxo) !== plan.openCoinLovelace,
  );
  const collateral =
    owned.find((utxo) => lovelace(utxo) === plan.collateralLovelace) ??
    notOpen.find((utxo) => lovelace(utxo) >= plan.timeoutCollateralLovelace);
  const openFunding = plain.find(
    (utxo) => lovelace(utxo) === plan.openCoinLovelace,
  );
  const operating = notOpen.filter((utxo) => utxo !== collateral).at(-1);
  return {
    ...(collateral === undefined ? {} : { collateral }),
    ...(openFunding === undefined ? {} : { openFunding }),
    ...(operating === undefined ? {} : { operating }),
  };
};

/**
 * The outputs (lovelace each) the funding transaction must pay the challenger
 * so its wallet matches the plan; empty when it already does.
 */
export const daBondPoolChallengerFundingShortfall = (input: {
  readonly utxos: readonly UTxO[];
  readonly address: string;
  readonly plan: DaBondPoolChallengerFundingPlan;
}): readonly bigint[] => {
  const { plan } = input;
  const plain = input.utxos.filter((utxo) => isPlainAda(utxo, input.address));
  const exactOpen = plain.filter(
    (utxo) => utxo.assets.lovelace === plan.openCoinLovelace,
  ).length;
  const outputs: bigint[] = Array.from(
    { length: Math.max(0, plan.openCoins - exactOpen) },
    () => plan.openCoinLovelace,
  );
  const coins = selectDaBondPoolChallengerCoins(input);
  if (coins.collateral === undefined) outputs.push(plan.collateralLovelace);
  if (
    coins.operating === undefined ||
    (coins.operating.assets.lovelace ?? 0n) < plan.operatingLovelace
  )
    outputs.push(plan.operatingLovelace);
  return outputs;
};

/** The challenger key must differ from every key the run already uses. */
export const assertDistinctChallengerKey = (
  challengerKeyHash: string,
  others: Readonly<Record<string, string>>,
): void => {
  const clashes = Object.entries(others)
    .filter(([, keyHash]) => keyHash === challengerKeyHash)
    .map(([role]) => role);
  if (clashes.length > 0)
    throw new Error(
      `The DA bond journey challenger key ${challengerKeyHash} is also the ${clashes.join(", ")} key; delete ${DA_BOND_POOL_CHALLENGER_SECRET} to generate a fresh one`,
    );
};

/** A run lacks a key or seed the journey needs; `missing` names each. */
export class DaBondJourneySigningMaterialError extends Error {
  readonly missing: readonly string[];
  constructor(missing: readonly string[], detail: string) {
    super(
      `DA bond pool journey is missing signing material: ${missing.join("; ")}. ${detail}`,
    );
    this.name = "DaBondJourneySigningMaterialError";
    this.missing = missing;
  }
}

/** A key the run holds, by its role in the run. */
export type DaBondJourneyHeldKey = Readonly<{
  role: string;
  keyHash: string;
}>;

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
const COMMITTEE_NODE = /da-committee-node/u;

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
 * one minute from now (the commit's validity ends at `end_time + 1`), never
 * at or before its start.
 */
export const nextJourneyBlockInterval = (input: {
  readonly predecessorEndTime: bigint;
  readonly nowMs: number;
}): Readonly<{ startTime: bigint; endTime: bigint }> => {
  const startTime = input.predecessorEndTime;
  const proposed = BigInt(input.nowMs + 59_999);
  return {
    startTime,
    endTime: proposed > startTime ? proposed : startTime + 1n,
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

/**
 * The Timeout's slash evidence from its landed body: the fee, the one pool
 * output (its lovelace, and whether it kept the input's datum and carries only
 * the pool NFT), and the outputs paid to the challenger's key address.
 */
export const summarizeDaBondPoolTimeout = (input: {
  readonly txId: string;
  readonly fee: bigint;
  readonly inputs: readonly string[];
  readonly outputs: readonly DaBondPoolJourneyOutput[];
  readonly pool: Readonly<{
    outRef: string;
    address: string;
    unit: string;
    lovelace: bigint;
    datum: string;
  }>;
  readonly challengerAddress: string;
  readonly challengerRemainingLovelace?: bigint;
}): DaBondPoolJourneyTimeoutResult => {
  if (!input.inputs.includes(input.pool.outRef))
    throw new Error(
      `Timeout ${input.txId} does not spend the observed pool ${input.pool.outRef}`,
    );
  const pools = input.outputs.filter(
    (output) =>
      output.address === input.pool.address &&
      (output.assets[input.pool.unit] ?? 0n) !== 0n,
  );
  if (pools.length !== 1)
    throw new Error(
      `Timeout ${input.txId} has ${pools.length.toString()} pool outputs; it must continue the pool exactly once`,
    );
  const pool = pools[0]!;
  const challenger = input.outputs.filter(
    (output) => output.address === input.challengerAddress,
  );
  return {
    txId: input.txId,
    fee: input.fee,
    challengerOutputLovelace: challenger.reduce(
      (sum, output) => sum + (output.assets.lovelace ?? 0n),
      0n,
    ),
    challengerOutputCount: challenger.length,
    poolBefore: input.pool.lovelace,
    poolAfter: pool.assets.lovelace ?? 0n,
    poolDatumAndNftKept:
      pool.datum === input.pool.datum &&
      pool.assets[input.pool.unit] === 1n &&
      Object.keys(pool.assets).every(
        (unit) => unit === "lovelace" || unit === input.pool.unit,
      ),
    ...(input.challengerRemainingLovelace === undefined
      ? {}
      : { challengerRemainingLovelace: input.challengerRemainingLovelace }),
  };
};

/** Fee, spent inputs and outputs of a transaction's CBOR. */
export const decodeJourneyTransaction = (
  txCbor: string,
): Readonly<{
  fee: bigint;
  inputs: readonly string[];
  outputs: readonly DaBondPoolJourneyOutput[];
}> => {
  const tx = CML.Transaction.from_cbor_hex(txCbor);
  const body = tx.body();
  const inputList = body.inputs();
  const outputList = body.outputs();
  const inputs: string[] = [];
  for (let index = 0; index < inputList.len(); index += 1) {
    const input = inputList.get(index);
    inputs.push(
      `${input.transaction_id().to_hex()}#${input.index().toString()}`,
    );
  }
  const outputs: DaBondPoolJourneyOutput[] = [];
  for (let index = 0; index < outputList.len(); index += 1) {
    const output = coreToTxOutput(outputList.get(index));
    outputs.push({
      address: output.address,
      assets: output.assets,
      datum: output.datum ?? null,
    });
  }
  return { fee: body.fee(), inputs, outputs };
};

/** Canonical-source errors that clear once Kupo catches up with Ogmios. */
export const isTransientCanonicalError = (error: unknown): boolean => {
  const message = error instanceof Error ? error.message : String(error);
  return /aligned at the same canonical tip|changed during canonical discovery|next stable point|could not read the canonical Kupo checkpoint/u.test(
    message,
  );
};

// ---------------------------------------------------------------------------
// The live port
// ---------------------------------------------------------------------------

export type LiveDaBondPoolJourneyPortOptions = Readonly<{
  /** Default `daBondPoolJourneyDirectory(context.runDirectory)`. */
  artifactDirectory?: string;
  /** Default `DA_BOND_POOL_CHALLENGER_OPERATING_LOVELACE`. */
  operatingLovelace?: bigint;
  /** Progress lines; default `console.info`. */
  log?: (line: string) => void;
  /** Default `readLinuxProcesses`. */
  listProcesses?: () => readonly JourneyProcess[];
}>;

/** The committee node's two submitter mnemonics, relative to the run directory. */
export const DA_BOND_POOL_COMMITTEE_SUBMITTER_SECRETS = Object.freeze({
  l1: "secrets/da-bond-pool-committee-l1-submitter.seed",
  availability: "secrets/da-bond-pool-committee-availability-submitter.seed",
});
/** The finalized manifest the deployment writes and the CLI reads. */
export const JOURNEY_DEPLOYMENT_MANIFEST = "deploymentInfo/manifest.json";

/** The repository root of this worktree. */
const REPOSITORY_ROOT = fileURLToPath(new URL("../../../../", import.meta.url));

/**
 * The committee node cannot be started against this run (P27(8), P31): a file
 * it needs is missing, its DA libp2p runtime could not be produced, or its
 * runtime manifest's peer set does not admit its libp2p identity. A missing
 * file or a refused runtime stops the journey before any transaction; a
 * refused configuration stops it after the challenger funding transaction may
 * have landed, but before any pool or availability transaction.
 */
export class DaBondPoolCommitteeUnavailableError extends Error {
  constructor(problem: string, options?: ErrorOptions) {
    super(
      `The DA bond pool journey cannot start its committee node (ruling P27): ${problem}`,
      options,
    );
    this.name = "DaBondPoolCommitteeUnavailableError";
  }
}

export type LiveDaBondPoolJourneyPort = DaBondPoolJourneyPort &
  Readonly<{
    artifactDirectory: string;
    manifestId: string;
    networkMagic: number;
    challengerAddress: string;
    /**
     * Whether the committee node still runs. False after a passed journey
     * only when the checked stop at the end of step 6 ran (P27(3)); `dispose`
     * tears a running node down without those checks.
     */
    committeeRunning: () => boolean;
    /** Closes the availability journal; the port is unusable afterwards. */
    dispose: () => Promise<void>;
  }>;

/** The queue is not root-only: the journey needs its head to be B1. */
export class DaBondPoolJourneyQueueNotEmptyError extends Error {
  constructor(headers: number) {
    super(
      `The DA bond pool journey needs an empty state queue (root only), but it holds ${headers.toString()} header(s). The journey appends B1 as the head, and only the head can be removed after its availability Timeout; run it on a freshly deployed journey run directory.`,
    );
    this.name = "DaBondPoolJourneyQueueNotEmptyError";
  }
}

const loadOrCreateSeed = (path: string): string => {
  if (existsSync(path)) {
    const mode = statSync(path).mode & 0o777;
    if ((mode & 0o077) !== 0)
      throw new Error(
        `${path} is readable by others (mode ${mode.toString(8)}); it must be 0600`,
      );
    const seed = readFileSync(path, "utf8").trim();
    if (seed.split(/\s+/u).length < 12)
      throw new Error(`${path} does not hold a mnemonic`);
    return seed;
  }
  const seed = generateSeedPhrase();
  writeFileSync(path, `${seed}\n`, { mode: 0o600, flag: "wx" });
  return seed;
};

type JourneyBlock = Readonly<{
  label: string;
  header: SDK.Header;
  headerHash: string;
  payloadEnvelopeCbor: Buffer;
}>;

type AvailabilityRequest = "open" | "respond" | "settle" | "close" | "timeout";

/**
 * Builds the live port. Checks every precondition (no daemon, Kupo indexes
 * everything, root-only queue, owner quorum held) and funds the challenger
 * before returning.
 */
export const createLiveDaBondPoolJourneyPort = async (
  context: LiveJourneyContext,
  options: LiveDaBondPoolJourneyPortOptions = {},
): Promise<LiveDaBondPoolJourneyPort> => {
  const { deployment, accounts, provider, customNetwork, runDirectory } =
    context;
  const { manifest, contracts, chain } = deployment;
  const log =
    options.log ??
    ((line: string) => console.info(`DA bond pool journey: ${line}`));
  // The availability journal needs an absolute, normalized path.
  const artifactDirectory = resolve(
    options.artifactDirectory ?? daBondPoolJourneyDirectory(runDirectory),
  );
  mkdirSync(artifactDirectory, { recursive: true });

  // Preconditions that need no chain read.
  const endpoints = journeyEndpointsFromRunEnv(context.runEnv);
  if (
    endpoints.kupoUrl !== context.kupoUrl ||
    endpoints.ogmiosUrl !== context.ogmiosUrl
  )
    throw new Error("Journey context endpoints differ from its run.env");
  const listProcesses = options.listProcesses ?? readLinuxProcesses;
  // The journey daemons must be exactly the adapter's own committee node.
  const checkDaemons = (admitted: ReadonlySet<number>): void => {
    const daemons = findJourneyDaemons(listProcesses(), runDirectory, admitted);
    if (daemons.length > 0)
      throw new Error(
        `Refusing to run the DA bond pool journey while a watcher or DA committee daemon other than its own committee node runs against ${runDirectory}: ${daemons.join("; ")}. Stop the session watcher first: it would contest the journey's challenges, and a committee node holding the payload would answer the withheld block.`,
      );
  };
  checkDaemons(new Set());
  const patternsResponse = await fetch(`${context.kupoUrl}/patterns`, {
    signal: AbortSignal.timeout(20_000),
  });
  if (!patternsResponse.ok)
    throw new Error(
      `Kupo refused its pattern list: HTTP ${patternsResponse.status.toString()}`,
    );
  const patterns: unknown = await patternsResponse.json();
  if (!kupoMatchesEverything(patterns))
    throw new Error(
      `The DA bond pool journey needs Kupo to match "*" (every address); it matches ${JSON.stringify(patterns)}`,
    );

  const accountsSource = join(runDirectory, JOURNEY_ACCOUNTS_SECRET);
  const operatorSeed = requireJourneySeed(accounts, "operator", accountsSource);
  const cosignerSeed = requireJourneySeed(accounts, "cosigner", accountsSource);
  const availabilitySeed = requireJourneySeed(
    accounts,
    "availability",
    accountsSource,
  );
  const daSignerConfig = {
    NETWORK: "Custom" as const,
    L1_OPERATOR_SEED_PHRASE: operatorSeed,
    DA_COSIGNER_SEED_PHRASE: cosignerSeed,
  };
  const seedByRole: Readonly<Record<string, string>> = {
    operator: operatorSeed,
    cosigner: cosignerSeed,
  };
  const localSigners = daLocalSigners(daSignerConfig);

  const newLucid = () =>
    Lucid(provider, "Custom", {
      slotConfig: customNetwork.slotConfig,
      evaluator: createScalusEvaluator(),
    });
  const readLucid = await newLucid();
  const network = readLucid.config().network;
  if (network === undefined || network !== manifest.network)
    throw new Error("Journey Lucid network differs from the deployment");

  // Clock and depth.
  const tipTime = async (): Promise<number> =>
    readLucid.slotToUnixTime(await readOgmiosTipSlot(context.ogmiosUrl));
  const blockHeight = chain.blockHeight;
  if (blockHeight === undefined)
    throw new Error("The journey chain does not report its block height");
  const awaitActionDepth = async (): Promise<void> => {
    const target = (await blockHeight()) + JOURNEY_ACTION_DEPTH;
    const deadline = Date.now() + ACTION_DEPTH_TIMEOUT_MS;
    for (;;) {
      const height = await blockHeight();
      if (height >= target) return;
      if (Date.now() > deadline)
        throw new Error(
          `The chain stalled at block ${height.toString()} before depth ${JOURNEY_ACTION_DEPTH.toString()}`,
        );
      await pause(1_000);
    }
  };

  // The state queue must hold only its root.
  const sortedQueue = () =>
    SDK.fetchSortedStateQueueUTxOs(readLucid, {
      stateQueueAddress: contracts.stateQueue.spendingScriptAddress,
      stateQueuePolicyId: contracts.stateQueue.policyId,
    });
  const initialQueue = await sortedQueue();
  if (initialQueue.length !== 1)
    throw new DaBondPoolJourneyQueueNotEmptyError(initialQueue.length - 1);

  // The da-bond command context, as loadDaBondContext builds it.
  const authPolicy = manifestReferenceScriptAuthPolicy(manifest);
  const bondLucid = await newLucid();
  const poolSpending = await authenticatedManifestReference(
    bondLucid,
    manifest,
    authPolicy,
    "daBondPoolSpend",
    "da-bond-pool spending",
  );
  const poolMinting = await authenticatedManifestReference(
    bondLucid,
    manifest,
    authPolicy,
    "daBondPoolMint",
    "da-bond-pool minting",
  );
  const poolValidator: SDK.AuthenticatedValidator = {
    ...spendingValidatorOf(network, poolSpending.scriptRef),
    ...mintingValidatorOf(poolMinting.scriptRef),
  };
  if (
    poolValidator.policyId !== contracts.daBondPool.policyId ||
    poolValidator.spendingScriptAddress !==
      contracts.daBondPool.spendingScriptAddress
  )
    throw new Error(
      "The manifest's DA bond pool references differ from the deployment",
    );
  const governorSpend = manifest.contracts.daParamsGovernorSpend?.scriptHash;
  const governorMint = manifest.contracts.daParamsGovernorMint?.scriptHash;
  if (governorSpend === undefined || governorMint === undefined)
    throw new Error("Deployment omits the DA params governor");
  const parameters = availabilityParametersFromManifest(manifest);
  const journeyParams = daBondPoolJourneyParamsOf(
    parameters,
    manifest.deploymentProfile.timing,
  );
  const daParamsGovernor = {
    address: credentialToAddress(network, {
      type: "Script",
      hash: governorSpend,
    }),
    unit: toUnit(governorMint, SDK.DA_PARAMS_ASSET_NAME),
  };
  // The da-bond commands read a synchronous clock; the adapter sets it to the
  // ledger tip before each command, since the node checks validity bounds
  // against the tip rather than the wall clock.
  let bondNow = await tipTime();
  const refreshBondNow = async () => {
    bondNow = await tipTime();
  };
  const bondContext: DaBondContext = {
    lucid: bondLucid,
    network,
    manifestId: manifest.manifestId,
    poolValidator,
    poolSpendingReference: poolSpending,
    parameters,
    daParamsGovernor,
    withdrawDelayMs: BigInt(journeyParams.withdrawDelayMs),
    now: () => bondNow,
    // Pool transactions go through the da-bond CLI processes only (P18).
    submit: async () => {
      throw new Error(
        "The live DA bond pool journey submits pool transactions only through the da-bond CLI",
      );
    },
  };

  // The owner quorum must be keys this run holds.
  const daParamsUtxos = await readLucid.utxosAtWithUnit(
    daParamsGovernor.address,
    daParamsGovernor.unit,
  );
  if (daParamsUtxos.length !== 1 || typeof daParamsUtxos[0]!.datum !== "string")
    throw new Error("Expected one DA params UTxO with an inline datum");
  const daParams = Data.from(daParamsUtxos[0]!.datum, SDK.DaParamsDatum);
  const quorum = planDaBondOwnerQuorum({
    owners: daParams.owners,
    updateThreshold: daParams.update_threshold,
    held: localSigners.map((signer) => ({
      role: signer.role,
      keyHash: signer.keyHashHex,
    })),
    source: `${accountsSource} (operator and cosigner seed phrases)`,
  });
  const availabilityAddress = accounts.availability.address;

  // The committee node's DA libp2p runtime (P31): fresh libp2p keys and the
  // committee-target runtime manifest from the real
  // `da-libp2p-generate-manifest` process, over the finalized deployment
  // manifest's DA committee. It runs before any transaction of this journey.
  const committeeDirectory = join(artifactDirectory, "committee");
  mkdirSync(committeeDirectory, { recursive: true });
  const manifestPath = join(runDirectory, JOURNEY_DEPLOYMENT_MANIFEST);
  const committeeBin = join(
    REPOSITORY_ROOT,
    "demo/da-committee-node/dist/index.js",
  );
  const cliBin = join(REPOSITORY_ROOT, "demo/midgard-node/dist/index.js");
  const missing = [manifestPath, committeeBin, cliBin].filter(
    (path) => !existsSync(path),
  );
  if (missing.length > 0)
    throw new DaBondPoolCommitteeUnavailableError(
      `missing ${missing.join(", ")}`,
    );
  const inheritedEnv = Object.fromEntries(
    DA_BOND_POOL_INHERITED_ENV.flatMap((name) => {
      const value = process.env[name];
      return value === undefined ? [] : [[name, value]];
    }),
  );
  let committeeRuntime: DaBondPoolCommitteeRuntimeEvidence;
  try {
    const runtimePlan = planDaBondPoolCommitteeRuntime({
      runDirectory,
      deployment: manifest,
      portOffset: readWorktreePortOffset(REPOSITORY_ROOT),
    });
    mkdirSync(join(runDirectory, "secrets"), { recursive: true, mode: 0o700 });
    committeeRuntime = await produceDaBondPoolCommitteeRuntime({
      plan: runtimePlan,
      command: [process.execPath, cliBin],
      env: {
        ...inheritedEnv,
        MIDGARD_CONFIG_MODE: "disabled",
        MIDGARD_DOTENV_MODE: "disabled",
      },
      cwd: committeeDirectory,
      run: spawnDaBondPoolRuntimeProcess(120_000),
    });
  } catch (cause) {
    throw new DaBondPoolCommitteeUnavailableError(
      `its DA libp2p runtime could not be produced: ${cause instanceof Error ? cause.message : String(cause)}`,
      { cause },
    );
  }
  await writeJourneyArtifact(
    join(committeeDirectory, "runtime.json"),
    committeeRuntime,
  );
  log(
    `generated the committee runtime manifest ${committeeRuntime.outPath} (sha256 ${committeeRuntime.outputSha256}); the observer is member ${committeeRuntime.observer.signerIndex.toString()} (${committeeRuntime.observer.peerId})`,
  );
  const runtimeManifestPath = committeeRuntime.outPath;
  const libp2pKeySource = committeeRuntime.observer.libp2pKeySource;

  // The challenger: a fresh key, distinct from every operational key.
  const challengerSeed = loadOrCreateSeed(
    join(runDirectory, DA_BOND_POOL_CHALLENGER_SECRET),
  );
  const challengerLucid = await newLucid();
  challengerLucid.selectWallet.fromSeed(challengerSeed, {
    addressType: "Enterprise",
  });
  const challengerAddress = await challengerLucid.wallet().address();
  const challengerKey = paymentCredentialOf(challengerAddress).hash;
  assertDistinctChallengerKey(challengerKey, {
    operator: paymentCredentialOf(accounts.operator.address).hash,
    publisher: paymentCredentialOf(accounts.publisher.address).hash,
    cosigner: paymentCredentialOf(accounts.cosigner.address).hash,
    availability: paymentCredentialOf(availabilityAddress).hash,
    "reference-script deployer": paymentCredentialOf(
      manifest.referenceScriptDeployAddress,
    ).hash,
  });
  const protocol = challengerLucid.config().protocolParameters;
  if (protocol === undefined)
    throw new Error("The challenger needs live ledger parameters");
  const plan = planDaBondPoolChallengerFunding({
    parameters,
    collateralPercentage: protocol.collateralPercentage,
    ...(options.operatingLovelace === undefined
      ? {}
      : { operatingLovelace: options.operatingLovelace }),
  });
  const shortfall = daBondPoolChallengerFundingShortfall({
    utxos: await challengerLucid.wallet().getUtxos(),
    address: challengerAddress,
    plan,
  });
  if (shortfall.length > 0) {
    log(
      `funding challenger ${challengerAddress} from the availability account: ${shortfall.join(", ")} lovelace`,
    );
    const funder = await newLucid();
    funder.selectWallet.fromSeed(availabilitySeed);
    let fundingTx = funder.newTx();
    for (const lovelace of shortfall)
      fundingTx = fundingTx.pay.ToAddress(challengerAddress, { lovelace });
    const signed = await (await fundingTx.complete()).sign
      .withWallet()
      .complete();
    const txHash = await signed.submit();
    await funder.awaitTx(txHash);
    await awaitActionDepth();
    await writeJourneyArtifact(join(artifactDirectory, "challenger.json"), {
      challengerAddress,
      challengerKeyHash: challengerKey,
      fundingTxId: txHash,
      outputs: shortfall,
      plan,
    });
  }

  // The committee node (P27): its submitter keys, database and environment.
  const submitterKey = async (secret: string) => {
    const path = join(runDirectory, secret);
    const seed = loadOrCreateSeed(path);
    const lucid = await newLucid();
    // The node selects its submitter wallets from the seed with Lucid's
    // default (base) address, so the adapter reads the same address.
    lucid.selectWallet.fromSeed(seed);
    const address = await lucid.wallet().address();
    return {
      source: `file:${path}`,
      keyHash: paymentCredentialOf(address).hash,
      address,
    };
  };
  const l1Submitter = await submitterKey(
    DA_BOND_POOL_COMMITTEE_SUBMITTER_SECRETS.l1,
  );
  const availabilitySubmitter = await submitterKey(
    DA_BOND_POOL_COMMITTEE_SUBMITTER_SECRETS.availability,
  );
  const nativeLedgerPaths = {
    socket: join(runDirectory, "cardano/ipc/node.socket"),
    config: join(runDirectory, "config/config.json"),
    binary: join(runDirectory, "work/midgard-chain-sync"),
  };
  const nativeLedger = Object.values(nativeLedgerPaths).every((path) =>
    existsSync(path),
  );
  const postgres = {
    database: context.runEnv.MIDGARD_PHASE4_POSTGRES_DATABASE,
    user: context.runEnv.MIDGARD_PHASE4_POSTGRES_USER,
    password: context.runEnv.MIDGARD_PHASE4_POSTGRES_PASSWORD,
    port: context.runEnv.MIDGARD_PHASE4_POSTGRES_PORT,
    project: context.runEnv.MIDGARD_PHASE4_COMPOSE_PROJECT,
  };
  const postgresMissing = Object.entries(postgres)
    .filter(([, value]) => value === undefined || value === "")
    .map(([name]) => name);
  if (postgresMissing.length > 0)
    throw new DaBondPoolCommitteeUnavailableError(
      `run.env lacks the devnet Postgres ${postgresMissing.join(", ")}`,
    );
  const committeeDatabase = `da_bond_pool_committee_${randomBytes(4).toString("hex")}`;
  const committeeDatabaseUrl = new URL(
    `postgres://127.0.0.1:${postgres.port!}/${committeeDatabase}`,
  );
  committeeDatabaseUrl.username = postgres.user!;
  committeeDatabaseUrl.password = postgres.password!;
  const apiPort = worktreeDerivedPort(
    REPOSITORY_ROOT,
    "da-bond-pool-committee-api",
  );
  // P27(4): the node's sync wait is bounded by its own poll cadence plus
  // twice the ideal time for the release confirmation depth on this devnet.
  const committeePollIntervalMs = 2_000;
  const cadence = await readJourneyCadence(runDirectory, {
    authenticatedConfirmationDepth: manifest.l1Finality.confirmationDepth,
  });
  // The observer also derives from it how long each stop watches the
  // submitter addresses: one node poll plus the same finality lag, so a
  // last-tick submission cannot land unseen.
  const committeeCadence = {
    pollIntervalMs: committeePollIntervalMs,
    confirmationDepth: cadence.confirmationDepth,
    slotLengthMs: cadence.slotLengthSeconds * 1000,
    activeSlotsCoeff: cadence.activeSlotsCoeff,
  };
  const committeeSyncTimeoutMs =
    daBondPoolCommitteeSyncBoundMs(committeeCadence);
  const committee = buildDaBondPoolCommitteeEnv({
    settings: daBondPoolCommitteeSettings({
      runtimeManifestPath,
      deploymentManifestPath: manifestPath,
      network: manifest.network,
      networkMagic: customNetwork.networkMagic,
      kupoUrl: context.kupoUrl,
      ogmiosUrl: context.ogmiosUrl,
      chainSyncCursorPath: join(committeeDirectory, "chain-sync-cursor.json"),
      finalityDepth: manifest.l1Finality.confirmationDepth,
      ...(nativeLedger ? { nativeLedger: nativeLedgerPaths } : {}),
    }),
    l1Submitter,
    availabilitySubmitter,
    operationalKeyHashes: {
      operator: paymentCredentialOf(accounts.operator.address).hash,
      publisher: paymentCredentialOf(accounts.publisher.address).hash,
      cosigner: paymentCredentialOf(accounts.cosigner.address).hash,
      availability: paymentCredentialOf(availabilityAddress).hash,
      "reference-script deployer": paymentCredentialOf(
        manifest.referenceScriptDeployAddress,
      ).hash,
      challenger: challengerKey,
    },
    libp2pKeySource,
    journalPath: join(committeeDirectory, "availability-journal.sqlite"),
    databaseUrl: committeeDatabaseUrl.toString(),
    apiHost: "127.0.0.1",
    apiPort,
    pollIntervalMs: committeePollIntervalMs,
    inherited: process.env,
  });
  // P27(8), P31(6): the node's own configuration loader accepts this
  // environment and the runtime manifest's peer set admits its libp2p
  // identity, before the node's database is created.
  try {
    await verifyDaBondPoolCommitteeRuntime(committee.env);
  } catch (cause) {
    throw new DaBondPoolCommitteeUnavailableError(
      `its configuration is refused: ${cause instanceof Error ? cause.message : String(cause)}`,
      { cause },
    );
  }
  execFileSync(
    "docker",
    [
      "exec",
      "-i",
      `${postgres.project!}-postgres-1`,
      "psql",
      "-U",
      postgres.user!,
      "-d",
      postgres.database!,
      "-v",
      "ON_ERROR_STOP=1",
    ],
    {
      input: `CREATE DATABASE ${committeeDatabase};`,
      stdio: ["pipe", "pipe", "pipe"],
    },
  );
  // Fund each submitter for the node's preflight: the plain ADA of one round
  // and a collateral coin, from the availability account, once.
  const submitterFunding = [
    DA_L1_SUBMITTER_MIN_PLAIN_ADA_LOVELACE,
    DEFAULT_L1_SUBMITTER_PREFLIGHT.minCollateralLovelace * 2n,
  ];
  const unfunded: string[] = [];
  for (const { address } of [l1Submitter, availabilitySubmitter])
    if ((await readLucid.utxosAt(address)).length === 0) unfunded.push(address);
  if (unfunded.length > 0) {
    const funder = await newLucid();
    funder.selectWallet.fromSeed(availabilitySeed);
    let fundingTx = funder.newTx();
    for (const address of unfunded)
      for (const lovelace of submitterFunding)
        fundingTx = fundingTx.pay.ToAddress(address, { lovelace });
    const signed = await (await fundingTx.complete()).sign
      .withWallet()
      .complete();
    const txHash = await signed.submit();
    await funder.awaitTx(txHash);
    await awaitActionDepth();
    log(`funded committee submitters ${unfunded.join(", ")}: ${txHash}`);
  }
  await writeJourneyArtifact(join(committeeDirectory, "committee.json"), {
    l1SubmitterAddress: l1Submitter.address,
    availabilitySubmitterAddress: availabilitySubmitter.address,
    database: committeeDatabase,
    apiPort,
    observerSignerIndex: committeeRuntime.observer.signerIndex,
    observerPeerId: committeeRuntime.observer.peerId,
    runtimeManifestSha256: committeeRuntime.outputSha256,
    env: committee.recorded,
  });
  let committeeRecords = 0;
  const committeeNode = createDaBondPoolCommitteeObserver({
    spawn: () =>
      spawnDaBondPoolCommitteeNode({
        argv: [process.execPath, committeeBin],
        env: committee.env,
        cwd: committeeDirectory,
        logDirectory: committeeDirectory,
        apiUrl: `http://127.0.0.1:${apiPort.toString()}`,
      }),
    checkDaemons,
    submitterUtxos: async () =>
      (
        await Promise.all(
          [l1Submitter.address, availabilitySubmitter.address].map((address) =>
            readLucid.utxosAt(address),
          ),
        )
      )
        .flat()
        .map(outRefOf),
    expectedView: async () =>
      daBondPoolCommitteeExpectedView(
        await port.poolSnapshot(),
        journeyParams.daBond,
      ),
    env: committee.recorded,
    record: async (entry) => {
      committeeRecords += 1;
      await writeJourneyArtifact(
        join(
          committeeDirectory,
          `${committeeRecords.toString().padStart(3, "0")}-${entry.kind}.json`,
        ),
        entry,
      );
    },
    syncTimeoutMs: committeeSyncTimeoutMs,
    startTimeoutMs: 180_000,
    pollMs: POLL_MS,
    stopBoundMs: 30_000,
    nodeCadence: committeeCadence,
  });

  // The da-bond CLI (P18).
  let cliChains = 0;
  const cli = createDaBondPoolCli({
    run: spawnDaBondCliProcess({
      cwd: artifactDirectory,
      timeoutMs: INCLUSION_TIMEOUT_MS,
      inheritedNames: new Set(DA_BOND_POOL_INHERITED_ENV),
    }),
    command: [process.execPath, cliBin],
    manifestPath,
    kupoUrl: context.kupoUrl,
    ogmiosUrl: context.ogmiosUrl,
    env: {
      ...inheritedEnv,
      MIDGARD_CONFIG_MODE: "disabled",
      MIDGARD_DOTENV_MODE: "disabled",
      ...(nativeLedger
        ? {
            L1_NODE_SOCKET_PATH: nativeLedgerPaths.socket,
            L1_NODE_CONFIG_PATH: nativeLedgerPaths.config,
            L1_NATIVE_CHAIN_SYNC_BINARY_PATH: nativeLedgerPaths.binary,
          }
        : {}),
    },
    workDirectory: (label) => {
      const directory = join(
        artifactDirectory,
        "cli",
        `${new Date().toISOString().replaceAll(":", "-")}-${label.replaceAll(" ", "-")}`,
      );
      mkdirSync(directory, { recursive: true });
      return directory;
    },
    // The adapter's own read: the pool output now sits at the transaction.
    confirm: async (txHash) => {
      const deadline = Date.now() + INCLUSION_TIMEOUT_MS;
      for (;;) {
        const holders = await readLucid.utxosAtWithUnit(
          poolValidator.spendingScriptAddress,
          SDK.daBondPoolUnit(poolValidator.policyId),
        );
        if (holders.length === 1 && holders[0]!.txHash === txHash) {
          await awaitActionDepth();
          return true;
        }
        if (Date.now() > deadline) return false;
        await pause(POLL_MS);
      }
    },
    record: async (label, runs) => {
      cliChains += 1;
      await writeJourneyArtifact(
        join(
          artifactDirectory,
          "cli",
          `${cliChains.toString().padStart(3, "0")}-${label.replaceAll(" ", "-")}.json`,
        ),
        runs,
      );
    },
  });
  mkdirSync(join(artifactDirectory, "cli"), { recursive: true });

  // The availability command flow, composed from the CLI's steps.
  const availabilityDeployment = await availabilityDeploymentFromManifest(
    challengerLucid,
    manifest,
  );
  const buildContext = {
    daChallengeWindowMs: BigInt(
      manifest.deploymentProfile.timing.da_challenge_window_ms,
    ),
    daAttestationPolicyId: manifest.contracts.daAttestationMint?.scriptHash,
    kupoUrl: context.kupoUrl,
  };
  const source = availabilityCommandCanonicalSource({
    lucid: challengerLucid,
    kupoUrl: context.kupoUrl,
    ogmiosUrl: context.ogmiosUrl,
  });
  const retryTransient = async <T>(
    label: string,
    action: () => Promise<T>,
  ): Promise<T> => {
    for (let attempt = 1; ; attempt += 1) {
      try {
        return await action();
      } catch (error) {
        if (
          !isTransientCanonicalError(error) ||
          attempt >= MAX_TRANSIENT_RETRIES
        )
          throw error;
        log(`${label}: Kupo is catching up with Ogmios; retrying`);
        await pause(POLL_MS);
      }
    }
  };
  // Blocks: commit and attest through the published-block actor. It is built
  // before the journal opens, so no later await can leave the journal open.
  const actor = await createPublishedWatcherBlockActor({
    deployment,
    lucid: deployment.operatorLucid,
    daSignerConfig,
    onStage: (stage) => log(stage),
  });
  let canonicalAnchor = await retryTransient("canonical anchor", () =>
    source.readBoundary(),
  );
  const journal = openAvailabilityOperationJournal(
    join(artifactDirectory, "availability-journal.sqlite"),
  );
  const operationContext: SDK.DaAvailabilityOperationContext = {
    deploymentIdentity: manifest.manifestId,
    actor: challengerKey,
    journal,
    stateQueuePolicyId: availabilityDeployment.contracts.stateQueue.policyId,
    minimumConfirmationDepth: manifest.l1Finality.confirmationDepth,
    transactionLimits: SDK.daAvailabilityOperationLimits(
      challengerLucid,
      availabilityDeployment.parameters,
    ),
    assertActuationCurrent: async () => {
      await source.assertCanonicalAncestor(canonicalAnchor);
    },
    observe: source.observe,
    submit: (cbor) => provider.submitTx(cbor),
  };
  const finalStates = new Set(["included", "confirmed", "expired"]);
  const quietJournal = async (): Promise<void> => {
    const deadline = Date.now() + JOURNAL_QUIET_TIMEOUT_MS;
    for (;;) {
      const results =
        await SDK.reconcileDaAvailabilityOperations(operationContext);
      const open = results.filter((result) => !finalStates.has(result.status));
      if (open.length === 0) return;
      if (open.some((result) => result.status === "conflict"))
        throw new Error(
          `Availability journal holds a conflicting intent: ${JSON.stringify(open)}`,
        );
      if (Date.now() > deadline)
        throw new Error(
          `Availability journal did not settle: ${JSON.stringify(open)}`,
        );
      await pause(POLL_MS);
    }
  };
  const awaitIncluded = async (txId: string): Promise<void> => {
    const deadline = Date.now() + INCLUSION_TIMEOUT_MS;
    for (;;) {
      const results =
        await SDK.reconcileDaAvailabilityOperations(operationContext);
      const status =
        results.find((result) => result.txHash === txId)?.status ??
        journal.findTransaction(txId)?.state;
      if (status === "included" || status === "confirmed") return;
      if (status === "expired" || status === "conflict")
        throw new Error(`Availability transaction ${txId} ended ${status}`);
      if (Date.now() > deadline)
        throw new Error(
          `Availability transaction ${txId} was not included in time (${status ?? "unknown"})`,
        );
      await pause(POLL_MS);
    }
  };
  const canonicalSnapshot = (headerHash: string) =>
    retryTransient("availability snapshot", async () => {
      for (let attempt = 1; ; attempt += 1) {
        const before = await source.readBoundary();
        const snapshot = await SDK.fetchDaAvailabilityChallengeSnapshot(
          challengerLucid,
          availabilityDeployment,
          headerHash,
        );
        const after = await source.readBoundary();
        if (before.pointId === after.pointId) return snapshot;
        if (attempt >= MAX_TRANSIENT_RETRIES)
          throw new Error(
            "Availability state kept changing during canonical discovery",
          );
      }
    });

  const payloadFiles = new Map<string, string>();
  /**
   * Lands one availability action through the journal: reconcile, snapshot,
   * plan (it must be one of `expected`), build, sign, submit, wait for
   * inclusion, then wait out the action depth.
   */
  const landAvailability = async (
    headerHash: string,
    requested: AvailabilityRequest,
    expected: readonly SDK.DaAvailabilityTransactionAction[],
    onBuilt?: (
      built: SDK.BuiltDaAvailabilityTransaction,
      snapshot: SDK.DaAvailabilityChallengeSnapshot,
    ) => void,
  ): Promise<
    Readonly<{
      txId: string;
      operation: SDK.DaAvailabilityTransactionAction;
      snapshot: SDK.DaAvailabilityChallengeSnapshot;
    }>
  > => {
    let reconciledOthers = 0;
    for (let attempt = 1; ; attempt += 1) {
      try {
        await quietJournal();
        canonicalAnchor = await source.readBoundary();
        const snapshot = await canonicalSnapshot(headerHash);
        const operation = planAvailabilityCommandAction(
          requested,
          snapshot,
          Date.now(),
        );
        if (!expected.includes(operation))
          throw new Error(
            `Availability ${requested} of ${headerHash} plans ${operation}, expected ${expected.join(" or ")}`,
          );
        const reserved = new Set(journal.reservedOutRefs(challengerKey));
        const coins = selectDaBondPoolChallengerCoins({
          utxos: await challengerLucid.wallet().getUtxos(),
          address: challengerAddress,
          plan,
          reserved,
        });
        const need = (coin: UTxO | undefined, name: string): string => {
          if (coin === undefined)
            throw new Error(
              `The challenger wallet ${challengerAddress} has no ${name} coin for ${operation}`,
            );
          return outRefOf(coin);
        };
        const payloadFile = payloadFiles.get(headerHash);
        let built: SDK.BuiltDaAvailabilityTransaction | undefined;
        const result = await SDK.runDaAvailabilityOperation(operationContext, {
          headerHash,
          action: operation,
          completesWorkflow:
            operation === "timeout"
              ? snapshot.descendant === undefined
              : undefined,
          build: async () => {
            built = await buildAvailabilityCommandTransaction(
              challengerLucid,
              availabilityDeployment,
              buildContext,
              snapshot,
              operation,
              {
                headerHash,
                collateralOutRef: need(coins.collateral, "collateral"),
                ...(operation === "open"
                  ? { fundingOutRef: need(coins.openFunding, "exact Open") }
                  : operation === "remove" || operation === "prune"
                    ? { fundingOutRef: need(coins.operating, "operating") }
                    : {}),
                ...(operation === "publish" && payloadFile !== undefined
                  ? { payloadFile }
                  : {}),
              },
              challengerKey,
              reserved,
            );
            onBuilt?.(built, snapshot);
            return built;
          },
        });
        if (built === undefined) {
          // The executor reconciled an earlier intent (a finalized anchor
          // still short of its confirmation depth) instead of building.
          reconciledOthers += 1;
          if (reconciledOthers > 60)
            throw new Error(
              `Availability ${requested} of ${headerHash} kept waiting on earlier intents: ${JSON.stringify(result)}`,
            );
          await pause(POLL_MS * 5);
          attempt -= 1;
          continue;
        }
        const txId = (built as SDK.BuiltDaAvailabilityTransaction).txId;
        if (result.txHash !== txId)
          throw new Error(
            `Availability executor returned ${result.txHash} for the transaction built as ${txId}`,
          );
        if (result.status === "expired" || result.status === "conflict")
          throw new Error(`Availability ${operation} ${txId} ${result.status}`);
        log(`${operation} ${headerHash}: submitted ${txId}`);
        await awaitIncluded(txId);
        await awaitActionDepth();
        return { txId, operation, snapshot };
      } catch (error) {
        if (
          !isTransientCanonicalError(error) ||
          attempt >= MAX_TRANSIENT_RETRIES
        )
          throw error;
        log(`availability ${requested}: ${String(error)}; retrying`);
        await pause(POLL_MS);
      }
    }
  };

  const blocks = new Map<string, JourneyBlock>();
  let onboarded = false;
  const headerUnit = (headerHash: string) =>
    toUnit(
      contracts.stateQueue.policyId,
      SDK.STATE_QUEUE_NODE_ASSET_NAME_PREFIX + headerHash,
    );
  const confirmedHeaderHash = async (): Promise<string> => {
    const [root] = await sortedQueue();
    if (root === undefined) throw new Error("The state queue has no root");
    return (
      await Effect.runPromise(
        SDK.getConfirmedStateFromStateQueueDatum(root.datum),
      )
    ).data.headerHash;
  };

  const feePayer = paymentCredentialOf(availabilityAddress).hash;
  const withdrawKeys = [
    ...quorum.map((key) => ({ role: key.role, seed: seedByRole[key.role] })),
    ...(quorum.some((key) => key.keyHash === feePayer)
      ? []
      : [{ role: "fee-payer", seed: availabilitySeed }]),
  ].map(({ role, seed }) => {
    if (seed === undefined)
      throw new DaBondJourneySigningMaterialError(
        [`the ${role} seedPhrase in ${accountsSource}`],
        "It witnesses the pool withdrawal steps.",
      );
    return { role, seed };
  });
  const withdraw = async (
    step: "begin" | "cancel" | "complete",
    complete?: Readonly<{ amount: bigint }>,
  ) => {
    const result = await cli.withdraw({
      step,
      feeAddress: availabilityAddress,
      signers: quorum.map((key) => key.keyHash),
      witnesses: withdrawKeys,
      ...(complete === undefined
        ? {}
        : { complete: { amount: complete.amount, to: availabilityAddress } }),
    });
    log(`withdraw ${step}: ${result.txId}`);
    return result;
  };

  const committeeLifecycle = async (
    phase: "before" | "after",
    step: DaBondPoolJourneyStep,
  ): Promise<void> => {
    const action = daBondPoolCommitteeLifecycle(phase, step);
    if (action === "start") {
      const pid = await committeeNode.start();
      log(
        `committee node started ${phase} step ${step.toString()}: pid ${pid.toString()}`,
      );
    } else if (action === "stop") {
      const exit = await committeeNode.stop();
      log(
        `committee node stopped ${phase} step ${step.toString()}: code ${String(exit.exitCode)}`,
      );
    }
  };

  const port: LiveDaBondPoolJourneyPort = {
    artifactDirectory,
    manifestId: manifest.manifestId,
    networkMagic: customNetwork.networkMagic,
    challengerAddress,
    committeeRunning: () => committeeNode.running(),
    dispose: async () => {
      try {
        const exit = await committeeNode.teardown();
        if (exit !== undefined)
          log(
            `committee node stopped: code ${String(exit.exitCode)}, signal ${String(exit.signal)}${exit.killed ? ", killed" : ""}`,
          );
      } finally {
        journal.close();
      }
    },

    // P27(3): started before steps 1 and 6, stopped before step 2 and at
    // the end of step 6 (`daBondPoolCommitteeLifecycle`); every stop checks
    // exit 0 on SIGTERM, no daemon left and unchanged submitter UTxOs, so a
    // node that dies or spends fails the step it ran in.
    beforeStep: async (step) => {
      checkDaemons(committeeNode.admitted());
      await committeeLifecycle("before", step);
    },
    afterStep: (step) => committeeLifecycle("after", step),

    params: async () => journeyParams,

    now: tipTime,

    poolSnapshot: async (): Promise<DaBondPoolJourneySnapshot> => {
      const holders = await readLucid.utxosAtWithUnit(
        poolValidator.spendingScriptAddress,
        SDK.daBondPoolUnit(poolValidator.policyId),
      );
      if (holders.length === 0)
        return { state: "missing", lovelace: 0n, backing: 0n };
      await refreshBondNow();
      const status = await daBondStatusCommand(bondContext);
      return {
        state: status.state,
        lovelace: BigInt(status.lovelace),
        backing: BigInt(status.backing),
        ...("unlockAt" in status && status.unlockAt !== undefined
          ? { unlockAt: Number(status.unlockAt) }
          : {}),
        utxoRef: status.poolOutRef,
      };
    },

    observeAlerts: async (): Promise<DaBondPoolJourneyAlerts> => {
      const nowMs = await tipTime();
      const poolAddress = poolValidator.spendingScriptAddress;
      const policyId = poolValidator.policyId;
      const watcher = deriveWatcherDaBondPoolObservation({
        pool: authenticWatcherDaBondPool({
          utxos: await readLucid.utxosAt(poolAddress),
          policyId,
          address: poolAddress,
        }),
        policyId,
        parameters,
        nowMs: BigInt(nowMs),
      });
      const committee = await committeeNode.observe();
      return {
        watcher: {
          underBacked: watcher.alerts.underBacked,
          withdrawing: watcher.alerts.withdrawing,
        },
        committee: {
          readinessReasons: committee.readinessReasons,
          events: committee.events,
          process: committee.process,
        },
      };
    },

    commitBlock: async (intent) => {
      if (!onboarded || !(await actor.operatorActive())) {
        await actor.onboardOperator();
        onboarded = true;
      }
      const queue = await sortedQueue();
      const root = queue[0];
      const tail = queue.at(-1);
      if (root === undefined || tail === undefined)
        throw new Error("The state queue has no root");
      let predecessor: {
        headerHash: string;
        utxosRoot: string;
        endTime: bigint;
      };
      if (queue.length === 1) {
        const genesis = (
          await Effect.runPromise(
            SDK.getConfirmedStateFromStateQueueDatum(root.datum),
          )
        ).data;
        // Genesis closes a real interval; the first header must end after it.
        await chain.awaitLedgerTime(Number(genesis.endTime) + 1);
        predecessor = {
          headerHash: genesis.headerHash,
          utxosRoot: genesis.utxoRoot,
          endTime: genesis.endTime,
        };
      } else {
        const key = tail.datum.key;
        if (key === "Empty") throw new Error("The queue tail has no key");
        const header = await Effect.runPromise(
          SDK.getHeaderFromStateQueueDatum(tail.datum),
        );
        predecessor = {
          headerHash: key.Key.key,
          utxosRoot: header.utxosRoot,
          endTime: header.endTime,
        };
      }
      const interval = nextJourneyBlockInterval({
        predecessorEndTime: predecessor.endTime,
        nowMs: chain.now(),
      });
      const block = await depositEventsRetainedBlock({
        operatorVkey: actor.operatorVkey,
        startTime: interval.startTime,
        endTime: interval.endTime,
        blockSlot: BigInt(
          deployment.operatorLucid.unixTimeToSlot(Number(interval.endTime)),
        ),
        prevHeaderHash: predecessor.headerHash,
        prevUtxosRoot: predecessor.utxosRoot,
        priorLedger: [],
        events: [],
      });
      const txId = await actor.commit(
        block,
        tail.utxo,
        queue.length > 1 ? queue[1]!.utxo : undefined,
      );
      const committed: JourneyBlock = {
        label: intent.label,
        header: block.header,
        headerHash: block.headerHash,
        payloadEnvelopeCbor: Buffer.from(block.payloadEnvelopeCbor),
      };
      blocks.set(block.headerHash, committed);
      await writeJourneyArtifact(
        join(artifactDirectory, `block-${intent.label}.json`),
        { ...committed, responder: intent.responder, commitTxId: txId },
      );
      log(`commit ${intent.label} ${block.headerHash}: ${txId}`);
      await awaitActionDepth();
      return {
        headerHash: block.headerHash,
        txId,
        headerEndTime: Number(interval.endTime),
      };
    },

    attest: async (headerHash) => {
      const block = blocks.get(headerHash);
      if (block === undefined)
        throw new Error(`Block ${headerHash} was not committed by this port`);
      let outcome: Awaited<ReturnType<typeof actor.attest>>;
      try {
        outcome = await actor.attest(block);
      } catch (error) {
        const refused = attestRefusalResult(error);
        if (refused === undefined) throw error;
        log(`Apply ${block.label} refused: ${JSON.stringify(refused)}`);
        return refused;
      }
      if (outcome.kind !== "attested")
        throw new Error(
          `Block ${block.label} was corrected before its Apply landed`,
        );
      const appliedAt = await tipTime();
      await awaitActionDepth();
      return { kind: "applied", txId: outcome.txHash, appliedAt };
    },

    open: async (headerHash) => {
      checkDaemons(committeeNode.admitted());
      const { txId } = await landAvailability(headerHash, "open", ["open"]);
      const snapshot = await canonicalSnapshot(headerHash);
      const record = snapshot.recordDatum;
      if (record === undefined)
        throw new Error(`Open ${txId} left no challenge record`);
      return { txId, responseDeadline: Number(record.response_deadline) };
    },

    respondAll: async (headerHash) => {
      const block = blocks.get(headerHash);
      if (block === undefined)
        throw new Error(`Block ${headerHash} was not committed by this port`);
      const payloadFile = join(artifactDirectory, `payload-${headerHash}.cbor`);
      await writeJourneyFile(payloadFile, block.payloadEnvelopeCbor);
      payloadFiles.set(headerHash, payloadFile);
      const txIds: string[] = [];
      for (;;) {
        const snapshot = await canonicalSnapshot(headerHash);
        if (!snapshot.tranches.some(({ datum }) => "Active" in datum))
          return { txIds };
        if (txIds.length >= MAX_RESPONSE_TRANSACTIONS)
          throw new Error(
            `Responding to ${headerHash} took more than ${MAX_RESPONSE_TRANSACTIONS.toString()} publications`,
          );
        txIds.push(
          (await landAvailability(headerHash, "respond", ["publish"])).txId,
        );
      }
    },

    settle: async (headerHash) => {
      const txIds: string[] = [];
      for (;;) {
        const snapshot = await canonicalSnapshot(headerHash);
        const record = snapshot.recordDatum;
        const terminal = snapshot.terminalDatum;
        if (
          record === undefined ||
          terminal === undefined ||
          terminal.next_tranche_index >=
            BigInt(record.commitment.tranche_descriptors.length)
        )
          return { txIds };
        if (txIds.length >= MAX_SETTLEMENTS)
          throw new Error(
            `Settling ${headerHash} took more than ${MAX_SETTLEMENTS.toString()} settlements`,
          );
        txIds.push(
          (await landAvailability(headerHash, "settle", ["settle"])).txId,
        );
      }
    },

    close: async (headerHash) => ({
      txId: (await landAvailability(headerHash, "close", ["close"])).txId,
    }),

    awaitTime: async (posixMs) => {
      const nowMs = await tipTime();
      if (nowMs >= posixMs) return;
      const enclosing = readLucid.unixTimeToSlot(posixMs);
      const targetSlot =
        readLucid.slotToUnixTime(enclosing) < posixMs
          ? enclosing + 1
          : enclosing;
      log(
        `waiting ${Math.ceil((posixMs - nowMs) / 1000).toString()} s for the tip to reach ${new Date(posixMs).toISOString()}`,
      );
      await awaitLedgerTipSlot({
        targetSlot,
        readTipSlot: () => readOgmiosTipSlot(context.ogmiosUrl),
        timeoutMs: awaitTimeBudgetMs(posixMs, nowMs),
        pollMs: 1_000,
      });
    },

    timeout: async (headerHash) => {
      const { txId, snapshot } = await landAvailability(
        headerHash,
        "timeout",
        ["timeout"],
        (built, planned) => {
          const pool = planned.pool;
          if (pool === undefined)
            throw new Error("Timeout snapshot holds no DA bond pool");
          const expectedFeePart = SDK.planDaBondPoolSlash({
            poolLovelace: pool.assets.lovelace ?? 0n,
            parameters,
          }).feePart;
          if (built.timeoutFeePartLovelace !== expectedFeePart)
            throw new Error(
              `Timeout fee part ${String(built.timeoutFeePartLovelace)} differs from the pool slash plan ${expectedFeePart.toString()}`,
            );
        },
      );
      const record = journal.findTransaction(txId);
      if (record === null)
        throw new Error(`Timeout ${txId} is missing from the journal`);
      const pool = snapshot.pool;
      if (pool === undefined || typeof pool.datum !== "string")
        throw new Error("Timeout snapshot holds no DA bond pool datum");
      const landed = decodeJourneyTransaction(record.intent.signedCbor);
      const summary = summarizeDaBondPoolTimeout({
        txId,
        fee: landed.fee,
        inputs: landed.inputs,
        outputs: landed.outputs,
        pool: {
          outRef: outRefOf(pool),
          address: poolValidator.spendingScriptAddress,
          unit: SDK.daBondPoolUnit(poolValidator.policyId),
          lovelace: pool.assets.lovelace ?? 0n,
          datum: pool.datum,
        },
        challengerAddress,
        ...(snapshot.terminalDatum === undefined
          ? {}
          : {
              challengerRemainingLovelace:
                snapshot.terminalDatum.remaining_challenger_lovelace,
            }),
      });
      await writeJourneyArtifact(
        join(artifactDirectory, `timeout-${headerHash}.json`),
        summary,
      );
      return summary;
    },

    removeOrPrune: async (headerHash) => {
      const txIds: string[] = [];
      for (;;) {
        const present = await readLucid.utxosAtWithUnit(
          contracts.stateQueue.spendingScriptAddress,
          headerUnit(headerHash),
        );
        if (present.length === 0) return { txIds };
        if (txIds.length >= MAX_REMOVAL_STEPS)
          throw new Error(
            `Removing ${headerHash} took more than ${MAX_REMOVAL_STEPS.toString()} steps`,
          );
        txIds.push(
          (await landAvailability(headerHash, "timeout", ["remove", "prune"]))
            .txId,
        );
      }
    },

    topUp: async (amount) => {
      const result = await cli.topUp({ amount, walletSeed: availabilitySeed });
      log(`top-up ${amount.toString()}: ${result.txId}`);
      return { txId: result.txId, cli: result.cli };
    },

    beginWithdraw: async () => {
      const { txId, cli: evidence, output } = await withdraw("begin");
      const status = output.status as { unlockAt?: unknown } | undefined;
      const unlockAt = Number(status?.unlockAt);
      if (!Number.isSafeInteger(unlockAt))
        throw new DaBondCliProcessError(
          `da-bond assemble for withdraw begin ${txId} printed no status.unlockAt`,
          [evidence.submit],
        );
      return { txId, unlockAt, cli: evidence };
    },

    cancelWithdraw: async () => {
      const { txId, cli: evidence } = await withdraw("cancel");
      return { txId, cli: evidence };
    },

    completeWithdraw: async (amount) => {
      const { txId, cli: evidence } = await withdraw("complete", { amount });
      return { txId, cli: evidence };
    },

    blockStatus: async (headerHash) => {
      const outputs = await readLucid.utxosAtWithUnit(
        contracts.stateQueue.spendingScriptAddress,
        headerUnit(headerHash),
      );
      if (outputs.length > 1)
        throw new Error(
          `Header ${headerHash} is held by several queue outputs`,
        );
      const output = outputs[0];
      if (output === undefined)
        return absentBlockStatus(headerHash, await confirmedHeaderHash());
      const view = await Effect.runPromise(
        SDK.getLinkedListNodeViewFromUTxO(output),
      );
      const node = await Effect.runPromise(
        SDK.getStateQueueNodeFromStateQueueDatum(view),
      );
      return SDK.daAvailabilityStateQueueStatusKind(node.da_attestation);
    },
  };
  return port;
};
