/**
 * Wallet funding preflight for operator lifecycle commands. Every command that
 * locks a bond or pays a fee runs this first, so an underfunded wallet gets a
 * shortfall figure instead of a failed transaction build, and so the wallet
 * view the build and the signature rely on is the provider's current ledger.
 */
import * as SDK from "@al-ft/midgard-sdk";
import type { LucidEvolution } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import { isPlainAdaOnlyUtxo } from "../wallet-hygiene.js";

/** Fee and change headroom kept beyond the value a command locks or pays. */
const OPERATOR_COMMAND_FEE_HEADROOM_LOVELACE = 5_000_000n;

export type OperatorFundingPreflight = {
  readonly label: string;
  readonly walletAddress: string;
  readonly availableLovelace: bigint;
  readonly requiredLovelace: bigint;
  /** Plain lovelace the ledger must see as collateral for an exact-fee tx. */
  readonly collateralLovelace: bigint;
  readonly shortfallLovelace: bigint;
  readonly sufficient: boolean;
};

export class OperatorFundingShortfall extends Error {
  readonly preflight: OperatorFundingPreflight;
  constructor(preflight: OperatorFundingPreflight) {
    super(
      `${preflight.label}: wallet ${preflight.walletAddress} holds ${formatAda(preflight.availableLovelace)} but needs ${formatAda(preflight.requiredLovelace)}; short by ${formatAda(preflight.shortfallLovelace)}`,
    );
    this.name = "OperatorFundingShortfall";
    this.preflight = preflight;
  }
}

export const formatAda = (lovelace: bigint): string => {
  const whole = lovelace / 1_000_000n;
  const frac = (lovelace % 1_000_000n).toString().padStart(6, "0");
  return `${whole.toString()}.${frac} ADA (${lovelace.toString()} lovelace)`;
};

/** Default Conway collateral percentage when the provider reports none. */
const DEFAULT_COLLATERAL_PERCENTAGE = 150;

/**
 * The collateral a transaction paying `feeLovelace` must present: the
 * protocol's collateral percentage of the fee, rounded up. A slashing
 * transaction pays the whole penalty as its fee, so its collateral is large
 * even though the fee itself comes out of the slashed bond.
 */
export const collateralForExactFee = (
  lucid: LucidEvolution,
  feeLovelace: bigint,
): bigint => {
  const percentage = BigInt(
    lucid.config().protocolParameters?.collateralPercentage ??
      DEFAULT_COLLATERAL_PERCENTAGE,
  );
  return (feeLovelace * percentage + 99n) / 100n;
};

/**
 * Drops the wallet's cached UTxO view and measures the plain lovelace the
 * provider holds at its address against the value a command needs.
 *
 * `handleSignSubmit` pins a predicted view of the wallet after every submit
 * and nothing refreshes it, so a coin another process spent since would still
 * be offered to the builder, and Lucid signs only for inputs its view knows.
 * Clearing the pin here makes the build, the exact-fee plan and the signature
 * all read the same ledger.
 *
 * Only plain-ADA coins count, which also keeps reference-script publications
 * out. `collateralLovelace` is not spent, but it must be present in plain-ADA
 * outputs for the transaction to be accepted, so it counts toward the
 * requirement.
 */
const operatorFundingPreflightProgram = (
  lucid: LucidEvolution,
  input: {
    readonly label: string;
    readonly lockedLovelace: bigint;
    readonly collateralLovelace?: bigint;
    readonly feeHeadroomLovelace?: bigint;
  },
): Effect.Effect<OperatorFundingPreflight, SDK.StateQueueError> =>
  Effect.gen(function* () {
    const walletAddress = yield* Effect.tryPromise({
      try: () => lucid.wallet().address(),
      catch: (cause) =>
        new SDK.StateQueueError({
          message: "Failed to resolve operator wallet address",
          cause,
        }),
    });
    lucid.clearUTxOOverride();
    const utxos = yield* Effect.tryPromise({
      try: () => lucid.wallet().getUtxos(),
      catch: (cause) =>
        new SDK.StateQueueError({
          message:
            "Failed to fetch operator wallet UTxOs for funding preflight",
          cause,
        }),
    });
    const availableLovelace = utxos
      .filter(isPlainAdaOnlyUtxo)
      .reduce((sum, utxo) => sum + (utxo.assets["lovelace"] ?? 0n), 0n);
    const collateralLovelace = input.collateralLovelace ?? 0n;
    const requiredLovelace =
      input.lockedLovelace +
      collateralLovelace +
      (input.feeHeadroomLovelace ?? OPERATOR_COMMAND_FEE_HEADROOM_LOVELACE);
    const shortfallLovelace =
      availableLovelace >= requiredLovelace
        ? 0n
        : requiredLovelace - availableLovelace;
    return {
      label: input.label,
      walletAddress,
      availableLovelace,
      requiredLovelace,
      collateralLovelace,
      shortfallLovelace,
      sufficient: shortfallLovelace === 0n,
    };
  });

/**
 * Runs the preflight and fails with a shortfall error when the wallet cannot
 * cover the command.
 */
export const requireOperatorFundingProgram = (
  lucid: LucidEvolution,
  input: Parameters<typeof operatorFundingPreflightProgram>[1],
): Effect.Effect<
  OperatorFundingPreflight,
  SDK.StateQueueError | OperatorFundingShortfall
> =>
  Effect.gen(function* () {
    const preflight = yield* operatorFundingPreflightProgram(lucid, input);
    if (!preflight.sufficient) {
      return yield* Effect.fail(new OperatorFundingShortfall(preflight));
    }
    yield* Effect.logInfo(
      `${preflight.label}: funding preflight ok (available=${formatAda(preflight.availableLovelace)}, required=${formatAda(preflight.requiredLovelace)})`,
    );
    return preflight;
  });
