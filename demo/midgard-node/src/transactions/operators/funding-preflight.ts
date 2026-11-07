/**
 * Wallet funding preflight for operator lifecycle commands. Every command that
 * locks a bond or pays a fee runs this first, so an underfunded wallet gets a
 * shortfall figure instead of a failed transaction build. It measures the
 * wallet view (plan §8.5) the build selects from and the signature covers.
 */
import * as SDK from "@al-ft/midgard-sdk";
import {
  type Assets,
  calculateMinLovelaceFromUTxO,
  type LucidEvolution,
  type UTxO,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";

import type { IntentJournal } from "../../services/intent-journal.js";
import {
  readSelectedWalletViewInputs,
  readWalletView,
} from "../utils.wallet-view.js";

/** Fee and change headroom kept beyond the value a command locks or pays. */
const OPERATOR_COMMAND_FEE_HEADROOM_LOVELACE = 5_000_000n;

export type OperatorFundingPreflight = {
  readonly label: string;
  readonly walletAddress: string;
  /** The view's lovelace less `tokenChangeLovelace`. */
  readonly availableLovelace: bigint;
  /** The minimum ADA of the change output that returns the view's tokens. */
  readonly tokenChangeLovelace: bigint;
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
      `${preflight.label}: wallet ${preflight.walletAddress} holds ${formatAda(preflight.availableLovelace)} spendable${preflight.tokenChangeLovelace > 0n ? ` (after ${formatAda(preflight.tokenChangeLovelace)} kept to return its tokens in change)` : ""} but needs ${formatAda(preflight.requiredLovelace)}; short by ${formatAda(preflight.shortfallLovelace)}`,
    );
    this.name = "OperatorFundingShortfall";
    this.preflight = preflight;
  }
}

/**
 * An operator-lifecycle build was handed no wallet input. It is refused
 * rather than built: lucid's coin selection reads the provider's wallet when
 * `presetWalletInputs` is empty, which can spend an output a live intent
 * already spends (plan §8.5). The wallet is not ready to fund the command; a
 * later attempt may succeed once the view holds an output.
 */
export class OperatorWalletInputsEmpty extends Error {
  readonly label: string;
  constructor(label: string) {
    super(
      `${label}: the operator wallet view gave no input to fund it; refused rather than letting coin selection read the provider wallet`,
    );
    this.name = "OperatorWalletInputsEmpty";
    this.label = label;
  }
}

/**
 * The `presetWalletInputs` an operator-lifecycle builder completes with:
 * `walletInputs` unchanged, or `OperatorWalletInputsEmpty` when there are
 * none.
 */
export const presetOperatorWalletInputs = (
  label: string,
  walletInputs: readonly UTxO[],
): Effect.Effect<UTxO[], OperatorWalletInputsEmpty> =>
  walletInputs.length > 0
    ? Effect.succeed([...walletInputs])
    : Effect.fail(new OperatorWalletInputsEmpty(label));

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
 * The minimum ADA of one change output at `address` carrying every token in
 * `utxos`, with the widest coin encoding; zero when they hold no token.
 */
export const tokenChangeMinLovelace = (
  lucid: LucidEvolution,
  address: string,
  utxos: readonly UTxO[],
): bigint => {
  const tokens: Assets = {};
  for (const utxo of utxos)
    for (const [unit, amount] of Object.entries(utxo.assets))
      if (unit !== "lovelace" && amount > 0n)
        tokens[unit] = (tokens[unit] ?? 0n) + amount;
  if (Object.keys(tokens).length === 0) return 0n;
  const coinsPerUtxoByte = lucid.config().protocolParameters?.coinsPerUtxoByte;
  if (coinsPerUtxoByte === undefined)
    throw new Error("funding preflight requires protocol parameters");
  return calculateMinLovelaceFromUTxO(coinsPerUtxoByte, {
    txHash: "00".repeat(32),
    outputIndex: 0,
    address,
    assets: { lovelace: 0xffffffffffffffffn, ...tokens },
  });
};

/**
 * Measures the lovelace in the wallet view of the selected wallet against the
 * value a command needs. Coin selection draws on the whole view, tokens and
 * script-reference outputs included, and returns the tokens in change, so the
 * spendable lovelace is the view's total less the change output's minimum ADA.
 * `collateralLovelace` is not spent, but it must be present for the
 * transaction to be accepted, so it counts toward the requirement.
 */
const operatorFundingPreflightProgram = (
  lucid: LucidEvolution,
  input: {
    readonly label: string;
    readonly lockedLovelace: bigint;
    readonly collateralLovelace?: bigint;
    readonly feeHeadroomLovelace?: bigint;
  },
): Effect.Effect<
  OperatorFundingPreflight,
  SDK.StateQueueError,
  IntentJournal
> =>
  Effect.gen(function* () {
    const walletAddress = yield* Effect.tryPromise({
      try: () => lucid.wallet().address(),
      catch: (cause) =>
        new SDK.StateQueueError({
          message: "Failed to resolve operator wallet address",
          cause,
        }),
    });
    const { utxos } = yield* readWalletView(lucid, walletAddress).pipe(
      Effect.mapError(
        (cause) =>
          new SDK.StateQueueError({
            message: `Failed to read the operator wallet view for funding preflight: ${cause.message}`,
            cause,
          }),
      ),
    );
    const totalLovelace = utxos.reduce(
      (sum, utxo) => sum + (utxo.assets["lovelace"] ?? 0n),
      0n,
    );
    const tokenChangeLovelace = yield* Effect.try({
      try: () => tokenChangeMinLovelace(lucid, walletAddress, utxos),
      catch: (cause) =>
        new SDK.StateQueueError({
          message: `Failed to measure the operator wallet's token change: ${String(cause)}`,
          cause,
        }),
    });
    const availableLovelace =
      totalLovelace > tokenChangeLovelace
        ? totalLovelace - tokenChangeLovelace
        : 0n;
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
      tokenChangeLovelace,
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
  SDK.StateQueueError | OperatorFundingShortfall,
  IntentJournal
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

/**
 * The selected wallet's view (§8.5) as the explicit `walletInputs` an
 * operator-lifecycle builder selects coins and collateral from. An empty view
 * is refused by name (`wallet_view_empty` in the cause). The builder does not
 * rely on that: it completes through `presetOperatorWalletInputs`, which
 * refuses an empty set itself, so it never falls back to the provider.
 */
export const operatorWalletInputsProgram = (
  lucid: LucidEvolution,
  label: string,
): Effect.Effect<readonly UTxO[], SDK.StateQueueError, IntentJournal> =>
  readSelectedWalletViewInputs(lucid, label).pipe(
    Effect.mapError(
      (cause) =>
        new SDK.StateQueueError({
          message: `Failed to read the operator wallet view to fund ${label}: ${cause.message}`,
          cause,
        }),
    ),
  );
