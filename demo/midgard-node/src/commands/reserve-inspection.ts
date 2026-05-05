import * as SDK from "@al-ft/midgard-sdk";
import {
  Data as LucidData,
  toUnit,
  type Assets,
  type UTxO,
} from "@lucid-evolution/lucid";
import { Effect, Option } from "effect";
import * as WithdrawalsDB from "@/database/withdrawals.js";
import {
  Database,
  Lucid,
  MidgardContracts,
  NodeConfig,
} from "@/services/index.js";
import {
  addressDataToBech32,
  formatJson,
  parseEventId,
} from "@/commands/withdrawal-utils.js";
import { valueToAssets } from "@/transactions/reserve-payout.js";

type ReserveUtxoSummary = {
  readonly outRef: string;
  readonly assets: Readonly<Assets>;
  readonly datum: "NoDatum";
  readonly hasReferenceScript: false;
  readonly spendable: true;
};

export type ReserveUtxosResult = {
  readonly reserveAddress: string;
  readonly utxoCount: number;
  readonly totals: Readonly<Assets>;
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

const outRef = (utxo: UTxO): string =>
  `${utxo.txHash}#${utxo.outputIndex.toString()}`;

const normalizeAssets = (assets: Readonly<Assets>): Assets => {
  const normalized: Record<string, bigint> = {};
  for (const [unit, quantity] of Object.entries(assets)) {
    if (quantity !== 0n) {
      normalized[unit] = quantity;
    }
  }
  return normalized as Assets;
};

const sumAssets = (utxos: readonly UTxO[]): Assets => {
  const totals: Record<string, bigint> = {};
  for (const utxo of utxos) {
    for (const [unit, quantity] of Object.entries(utxo.assets)) {
      totals[unit] = (totals[unit] ?? 0n) + quantity;
    }
  }
  return normalizeAssets(totals as Assets);
};

const subtractAssets = (
  left: Readonly<Assets>,
  right: Readonly<Assets>,
): Assets => {
  const result: Record<string, bigint> = { ...left };
  for (const [unit, quantity] of Object.entries(right)) {
    result[unit] = (result[unit] ?? 0n) - quantity;
  }
  return normalizeAssets(result as Assets);
};

const assetsEqual = (
  left: Readonly<Assets>,
  right: Readonly<Assets>,
): boolean => {
  const units = new Set([...Object.keys(left), ...Object.keys(right)]);
  for (const unit of units) {
    if ((left[unit] ?? 0n) !== (right[unit] ?? 0n)) {
      return false;
    }
  }
  return true;
};

const decodePayoutDatum = (payout: UTxO): SDK.PayoutDatum => {
  if (payout.datum == null) {
    throw new Error(`Payout UTxO ${outRef(payout)} has no inline datum.`);
  }
  return LucidData.from(payout.datum, SDK.PayoutDatum) as SDK.PayoutDatum;
};

const assertReserveUtxoShape = (utxo: UTxO): void => {
  if (utxo.datum != null) {
    throw new Error(
      `Reserve UTxO ${outRef(utxo)} has unexpected inline datum.`,
    );
  }
  if (utxo.scriptRef !== undefined) {
    throw new Error(
      `Reserve UTxO ${outRef(utxo)} has unexpected reference script.`,
    );
  }
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
  for (const utxo of utxos) {
    assertReserveUtxoShape(utxo);
  }
  return {
    reserveAddress: contracts.reserve.spendingScriptAddress,
    utxoCount: utxos.length,
    totals: sumAssets(utxos),
    utxos: utxos.map((utxo) => ({
      outRef: outRef(utxo),
      assets: utxo.assets,
      datum: "NoDatum",
      hasReferenceScript: false,
      spendable: true,
    })),
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
      const withdrawalOutRef = {
        txHash:
          entry[WithdrawalsDB.Columns.WITHDRAWAL_L1_TX_HASH].toString("hex"),
        outputIndex: entry[WithdrawalsDB.Columns.WITHDRAWAL_L1_OUTPUT_INDEX],
      };
      const withdrawalOrderUtxos = yield* Effect.tryPromise({
        try: async () => {
          try {
            return await lucid.utxosByOutRef([withdrawalOutRef]);
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
    const currentAssets = normalizeAssets({
      ...payout.assets,
      [payoutUnit]: (payout.assets[payoutUnit] ?? 0n) - 1n,
    });
    const remainingAssets = subtractAssets(targetAssets, currentAssets);
    const phase = assetsEqual(currentAssets, targetAssets)
      ? "funded"
      : Object.keys(currentAssets).length > 0
        ? "partially_funded"
        : "initialized";
    return {
      withdrawalEventId: eventId.toString("hex"),
      payoutUnit,
      payoutOutRef: outRef(payout),
      phase,
      targetAssets,
      currentAssets,
      remainingAssets,
      l1Address: addressDataToBech32(nodeConfig.NETWORK, datum.l1_address),
      l1Datum: LucidData.to(datum.l1_datum, SDK.CardanoDatum),
      diagnostic: null,
    };
  });

export const formatReserveUtxosResult = (result: ReserveUtxosResult): string =>
  formatJson(result);

export const formatPayoutStatusResult = (result: PayoutStatusResult): string =>
  formatJson(result);
