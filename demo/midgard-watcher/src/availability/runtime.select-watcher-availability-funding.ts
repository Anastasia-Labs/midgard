import * as SDK from "@al-ft/midgard-sdk";
import { type UTxO } from "@lucid-evolution/lucid";

import { type WatcherAvailabilityAction } from "./action.js";
import {
  plainAda,
  WATCHER_AVAILABILITY_MAX_COLLATERAL_INPUTS,
  WatcherAvailabilityCapitalShortfall,
  type WatcherAvailabilityOpenRefusal,
  type WatcherAvailabilityTimeoutDeferral,
  WatcherAvailabilityTimeoutPoolUnavailable,
} from "./runtime.release-watcher-availability-workflows.js";

/**
 * The first ordered step the journal admits and the wallet can fund, built.
 * Neither a step the journal refuses nor an Open the wallet cannot fund may
 * starve a later step, such as a live challenge's own settle, close or
 * Timeout: an unfundable Open (or its preparation) is recorded as refused and
 * the next step is tried, and so is a Timeout whose pool cannot be read at the
 * tip, which is recorded as deferred. Any other build failure propagates. An
 * Open that builds as a preparation is admitted again under that action.
 */
export const buildAdmittedWatcherAvailabilityOperation = async <
  T extends Readonly<{
    snapshot: Pick<SDK.DaAvailabilityChallengeSnapshot, "headerHash">;
    action: Pick<WatcherAvailabilityAction, "action">;
  }>,
  O extends Readonly<{ action: string }>,
>(
  ordered: readonly T[],
  admits: (headerHash: string, action: string) => boolean,
  build: (step: T) => Promise<O>,
): Promise<
  Readonly<{
    selected?: Readonly<{ step: T; operation: O }>;
    openRefused: readonly WatcherAvailabilityOpenRefusal[];
    timeoutsDeferred: readonly WatcherAvailabilityTimeoutDeferral[];
  }>
> => {
  const openRefused: WatcherAvailabilityOpenRefusal[] = [];
  const timeoutsDeferred: WatcherAvailabilityTimeoutDeferral[] = [];
  for (const step of ordered) {
    const { headerHash } = step.snapshot;
    if (!admits(headerHash, step.action.action)) continue;
    let operation: O;
    try {
      operation = await build(step);
    } catch (cause) {
      if (
        step.action.action === "timeout" &&
        cause instanceof WatcherAvailabilityTimeoutPoolUnavailable
      ) {
        timeoutsDeferred.push({
          headerHash,
          reason: "tip-pool-unavailable",
          detail: cause.message,
        });
        continue;
      }
      if (
        step.action.action !== "open" ||
        !(cause instanceof WatcherAvailabilityCapitalShortfall)
      )
        throw cause;
      openRefused.push({
        headerHash,
        reason: "insufficient-availability-capital",
        requiredLovelace: cause.requiredLovelace.toString(),
        availableLovelace: cause.availableLovelace.toString(),
        detail: cause.message,
      });
      continue;
    }
    if (
      operation.action !== step.action.action &&
      !admits(headerHash, operation.action)
    )
      continue;
    return { selected: { step, operation }, openRefused, timeoutsDeferred };
  }
  return { openRefused, timeoutsDeferred };
};

/**
 * Collateral a Timeout can need (spec #685 G9/D4). Its fee is
 * `min(penalty, taken) + c` with `c <= max_timeout_fee`, and the ledger holds
 * collateral of `collateralPercentage` of the fee, so a full slash needs
 * `collateralPercentage x (penalty + max_timeout_fee)` in at most three coins,
 * plus room for a valid collateral return.
 */
export const watcherAvailabilityTimeoutCollateralLovelace = (input: {
  parameters: Pick<
    SDK.DaAvailabilityParameters,
    "da_slash_penalty_lovelace" | "max_timeout_fee_lovelace"
  >;
  collateralPercentage: number;
  minimumReturnLovelace: bigint;
}): bigint =>
  ((input.parameters.da_slash_penalty_lovelace +
    input.parameters.max_timeout_fee_lovelace) *
    BigInt(input.collateralPercentage) +
    99n) /
    100n +
  input.minimumReturnLovelace;

export const selectWatcherAvailabilityFunding = (input: {
  utxos: readonly UTxO[];
  collateralLovelace: bigint;
  openingLovelace: bigint;
  requiredWorkingLovelace: bigint;
  reservedOutRefs?: ReadonlySet<string>;
}): Readonly<{
  collateral: readonly UTxO[];
  funding: UTxO;
  exactOpening?: UTxO;
}> => {
  const candidates = input.utxos
    .filter(plainAda)
    .sort((left, right) =>
      left.assets.lovelace === right.assets.lovelace
        ? `${left.txHash}#${left.outputIndex}`.localeCompare(
            `${right.txHash}#${right.outputIndex}`,
          )
        : left.assets.lovelace < right.assets.lovelace
          ? -1
          : 1,
    );
  // Collateral is never spent by a valid transaction, so an actor's existing
  // (journal-reserved) collateral stays eligible. An exact opening coin never is.
  const collateralCandidates = candidates.filter(
    (utxo) => utxo.assets.lovelace !== input.openingLovelace,
  );
  const single = collateralCandidates.find(
    (utxo) => utxo.assets.lovelace >= input.collateralLovelace,
  );
  let collateral: UTxO[] | undefined =
    single === undefined ? undefined : [single];
  if (collateral === undefined) {
    const largest = [...collateralCandidates]
      .reverse()
      .slice(0, WATCHER_AVAILABILITY_MAX_COLLATERAL_INPUTS);
    let total = 0n;
    for (const [index, utxo] of largest.entries()) {
      total += utxo.assets.lovelace;
      if (total >= input.collateralLovelace) {
        collateral = largest.slice(0, index + 1);
        break;
      }
    }
  }
  if (collateral === undefined)
    throw new WatcherAvailabilityCapitalShortfall(
      `Availability wallet needs separate plain-ADA collateral of at least ${input.collateralLovelace.toString()} lovelace in at most ${WATCHER_AVAILABILITY_MAX_COLLATERAL_INPUTS.toString()} coins`,
      input.collateralLovelace,
      collateralCandidates
        .slice(-WATCHER_AVAILABILITY_MAX_COLLATERAL_INPUTS)
        .reduce((sum, utxo) => sum + utxo.assets.lovelace, 0n),
    );
  const collateralSet = new Set(collateral);
  const spending = candidates.filter(
    (utxo) =>
      !collateralSet.has(utxo) &&
      !input.reservedOutRefs?.has(`${utxo.txHash}#${utxo.outputIndex}`),
  );
  const spendable = spending.reduce(
    (sum, utxo) => sum + utxo.assets.lovelace,
    0n,
  );
  if (spendable < input.requiredWorkingLovelace) {
    throw new WatcherAvailabilityCapitalShortfall(
      "Availability wallet cannot fund the challenger bond and reachable timeout removal path",
      input.requiredWorkingLovelace,
      spendable,
    );
  }
  const funding = spending.at(-1);
  if (funding === undefined)
    throw new Error("Availability wallet has no independent fee funding");
  const exactOpening = spending.find(
    (utxo) => utxo.assets.lovelace === input.openingLovelace,
  );
  return {
    collateral,
    funding,
    ...(exactOpening === undefined ? {} : { exactOpening }),
  };
};
