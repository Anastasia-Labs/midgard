import { join } from "node:path";

import * as SDK from "@al-ft/midgard-sdk";
import { type UTxO } from "@lucid-evolution/lucid";
import { availabilityTimeoutCollateralLovelace } from "midgard-node/commands/availability-challenge";

import type { DaBondPoolJourneyParams } from "./da-bond-pool-journey.js";
import { type loadJourneyContext } from "./live-context.js";

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

export const INCLUSION_TIMEOUT_MS = 12 * 60_000;

export const JOURNAL_QUIET_TIMEOUT_MS = 15 * 60_000;

export const ACTION_DEPTH_TIMEOUT_MS = 10 * 60_000;

export const POLL_MS = 2_000;

export const MAX_TRANSIENT_RETRIES = 30;

export const MAX_RESPONSE_TRANSACTIONS = 64;

export const MAX_SETTLEMENTS = 32;

export const MAX_REMOVAL_STEPS = 4;

/**
 * Header commits and attestations a validity-interval refusal (or, for a
 * commit, an unminted expiry) may rebuild.
 */
export const COMMIT_VALIDITY_ATTEMPTS = 3;

/**
 * How recent the ledger tip must be before a header commit, an attestation or
 * an availability action is built. Each opens its interval sixty seconds
 * before the wall clock, and the ledger checks it against its tip, so a tip
 * this fresh leaves room to build and submit.
 */
export const COMMIT_FRESH_TIP_MS = 30_000;

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

export const outRefOf = (utxo: Pick<UTxO, "txHash" | "outputIndex">): string =>
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
