import * as SDK from "@al-ft/midgard-sdk";
import {
  addAssets,
  assetsEqual,
  subtractAssets,
  valueToAssets,
} from "@al-ft/midgard-sdk";
import {
  type Assets,
  Data as LucidData,
  toUnit,
  type UTxO,
} from "@lucid-evolution/lucid";
import { Effect, Option } from "effect";

import * as WithdrawalsDB from "../database/withdrawals.js";
import {
  Database,
  Lucid,
  MidgardContracts,
  NodeConfig,
} from "../services/index.js";
import { outRefLabel } from "../tx-context.js";
import { parseEventId } from "./command-utils.js";
import { addressDataToBech32 } from "./withdrawal-utils.js";

type ReserveUtxoSummary = {
  readonly outRef: string;
  readonly assets: Readonly<Assets>;
  readonly datum: "NoDatum" | "InlineDatum" | "DatumHash";
  readonly hasReferenceScript: boolean;
  /** False when the reserve and payout validators can never spend it. */
  readonly spendable: boolean;
  readonly unspendableReason: string | null;
};

export type ReserveUtxosResult = {
  readonly reserveAddress: string;
  readonly utxoCount: number;
  readonly totals: Readonly<Assets>;
  /** Totals over the spendable UTxOs only; what reserve funding can use. */
  readonly spendableTotals: Readonly<Assets>;
  readonly utxos: readonly ReserveUtxoSummary[];
};

export type PayoutStatusResult = {
  readonly withdrawalEventId: string;
  readonly payoutUnit: string;
  readonly payoutOutRef: string | null;
  readonly phase:
    | "not_initialized"
    | "initialized"
    | "partially_funded"
    | "funded"
    | "concluded"
    | "not_found_after_initialization";
  readonly targetAssets: Readonly<Assets>;
  readonly currentAssets: Readonly<Assets>;
  readonly remainingAssets: Readonly<Assets>;
  readonly l1Address: string | null;
  readonly l1Datum: string | null;
  readonly diagnostic: string | null;
};

const decodePayoutDatum = (payout: UTxO): SDK.PayoutDatum => {
  if (payout.datum == null) {
    throw new Error(`Payout UTxO ${outRefLabel(payout)} has no inline datum.`);
  }
  return LucidData.from(payout.datum, SDK.PayoutDatum) as SDK.PayoutDatum;
};

export const reserveUtxosProgram: Effect.Effect<
  ReserveUtxosResult,
  Error | SDK.LucidError,
  Lucid | MidgardContracts
> = Effect.gen(function* () {
  const { api: lucid } = yield* Lucid;
  const contracts = yield* MidgardContracts;
  const utxos = yield* Effect.tryPromise({
    try: () => lucid.utxosAt(contracts.reserve.spendingScriptAddress),
    catch: (cause) =>
      new SDK.LucidError({
        message: "Failed to fetch reserve UTxOs",
        cause,
      }),
  });
  const summaries = utxos.map((utxo): ReserveUtxoSummary => {
    const unspendableReason = SDK.reserveInputShapeRejection(utxo) ?? null;
    return {
      outRef: outRefLabel(utxo),
      assets: utxo.assets,
      datum:
        utxo.datum != null
          ? "InlineDatum"
          : utxo.datumHash != null
            ? "DatumHash"
            : "NoDatum",
      hasReferenceScript: utxo.scriptRef != null,
      spendable: unspendableReason === null,
      unspendableReason,
    };
  });
  const total = (selected: readonly ReserveUtxoSummary[]) =>
    selected.reduce<Assets>(
      (totals, utxo) => addAssets(totals, utxo.assets),
      {},
    );
  return {
    reserveAddress: contracts.reserve.spendingScriptAddress,
    utxoCount: utxos.length,
    totals: total(summaries),
    spendableTotals: total(summaries.filter((utxo) => utxo.spendable)),
    utxos: summaries,
  };
});

export const payoutStatusProgram = (
  eventIdHex: string,
): Effect.Effect<
  PayoutStatusResult,
  Error | SDK.LucidError,
  Database | Lucid | MidgardContracts | NodeConfig
> =>
  Effect.gen(function* () {
    const eventId = parseEventId(eventIdHex, "--withdrawal-event-id");
    const contracts = yield* MidgardContracts;
    const nodeConfig = yield* NodeConfig;
    const { api: lucid } = yield* Lucid;
    const maybeEntry = yield* WithdrawalsDB.retrieveByEventId(eventId);
    if (Option.isNone(maybeEntry)) {
      return yield* Effect.fail(
        new Error(`Withdrawal event ${eventId.toString("hex")} not found.`),
      );
    }
    const entry = maybeEntry.value;
    const payoutUnit = toUnit(
      contracts.payout.policyId,
      entry[WithdrawalsDB.Columns.ASSET_NAME].toString("hex"),
    );
    const payoutUtxos = yield* Effect.tryPromise({
      try: () =>
        lucid.utxosAtWithUnit(
          contracts.payout.spendingScriptAddress,
          payoutUnit,
        ),
      catch: (cause) =>
        new SDK.LucidError({
          message: "Failed to fetch payout UTxOs",
          cause,
        }),
    });
    if (payoutUtxos.length === 0) {
      const withdrawalOutRef = [
        {
          txHash:
            entry[WithdrawalsDB.Columns.WITHDRAWAL_L1_TX_HASH].toString("hex"),
          outputIndex: entry[WithdrawalsDB.Columns.WITHDRAWAL_L1_OUTPUT_INDEX],
        },
      ];
      const withdrawalOrderUtxos = yield* Effect.tryPromise({
        try: async () => {
          try {
            return await lucid.utxosByOutRef(withdrawalOutRef);
          } catch (cause) {
            const message =
              cause instanceof Error ? cause.message : String(cause);
            if (message.includes("Missing requested UTxO")) {
              return [];
            }
            throw cause;
          }
        },
        catch: (cause) =>
          new SDK.LucidError({
            message: "Failed to fetch withdrawal order UTxO",
            cause,
          }),
      });
      const withdrawalOrderStillPresent = withdrawalOrderUtxos.length === 1;
      const isValidFinalizedWithdrawal =
        entry[WithdrawalsDB.Columns.STATUS] ===
          WithdrawalsDB.Status.Finalized &&
        entry[WithdrawalsDB.Columns.VALIDITY] ===
          WithdrawalsDB.Validity.WithdrawalIsValid;
      const phase =
        isValidFinalizedWithdrawal && !withdrawalOrderStillPresent
          ? "concluded"
          : isValidFinalizedWithdrawal && withdrawalOrderStillPresent
            ? "not_initialized"
            : "not_found_after_initialization";
      return {
        withdrawalEventId: eventId.toString("hex"),
        payoutUnit,
        payoutOutRef: null,
        phase,
        targetAssets: {},
        currentAssets: {},
        remainingAssets: {},
        l1Address: null,
        l1Datum: null,
        diagnostic:
          phase === "concluded"
            ? "No payout UTxO is present and the valid finalized withdrawal order UTxO has been consumed."
            : phase === "not_initialized"
              ? "No payout UTxO is present and the valid finalized withdrawal order UTxO is still available for initialization."
              : "No payout UTxO is present, but local state does not prove a concluded valid payout.",
      };
    }
    if (payoutUtxos.length !== 1) {
      return yield* Effect.fail(
        new Error(
          `Expected exactly one payout UTxO for ${payoutUnit}, found ${payoutUtxos.length.toString()}.`,
        ),
      );
    }
    const payout = payoutUtxos[0]!;
    const datum = decodePayoutDatum(payout);
    const targetAssets = valueToAssets(datum.l2_value);
    const currentAssets = subtractAssets(payout.assets, { [payoutUnit]: 1n });
    const remainingAssets = subtractAssets(targetAssets, currentAssets);
    const phase = assetsEqual(currentAssets, targetAssets)
      ? "funded"
      : Object.keys(currentAssets).length > 0
        ? "partially_funded"
        : "initialized";
    return {
      withdrawalEventId: eventId.toString("hex"),
      payoutUnit,
      payoutOutRef: outRefLabel(payout),
      phase,
      targetAssets,
      currentAssets,
      remainingAssets,
      l1Address: addressDataToBech32(nodeConfig.NETWORK, datum.l1_address),
      l1Datum: LucidData.to(datum.l1_datum, SDK.CardanoDatum),
      diagnostic: null,
    };
  });
