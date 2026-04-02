import * as MempoolLedgerDB from "@/database/mempoolLedger.js";
import { DatabaseError } from "@/database/utils/common.js";
import { Database } from "@/services/database.js";
import {
  CML,
  coreToUtxo,
  getAddressDetails,
  type Assets,
  type UTxO,
} from "@lucid-evolution/lucid";
import { Data as EffectData, Effect } from "effect";

export class UtxosCommandError extends EffectData.TaggedError(
  "UtxosCommandError",
)<{
  message: string;
  cause: unknown;
}> {}

export type StoredUtxoRecord = {
  readonly outref: Buffer;
  readonly output: Buffer;
};

export type UtxosCommandResult = {
  readonly address: string;
  readonly utxoCount: number;
  readonly totals: Readonly<Assets>;
  readonly utxos: readonly UTxO[];
};

export const canonicalUtxoOrder = (a: UTxO, b: UTxO): number => {
  const hashOrder = a.txHash.localeCompare(b.txHash);
  if (hashOrder !== 0) {
    return hashOrder;
  }
  return a.outputIndex - b.outputIndex;
};

export const parseAddressArgument = (address: string): string => {
  const normalized = address.trim();
  if (normalized.length === 0) {
    throw new Error("Address must not be empty.");
  }

  try {
    const details = getAddressDetails(normalized);
    if (!details.paymentCredential) {
      throw new Error("Address must include a payment credential.");
    }
    return details.address.bech32;
  } catch (cause) {
    throw new Error(`Invalid address "${normalized}": ${String(cause)}`);
  }
};

export const decodeStoredUtxo = (
  entry: StoredUtxoRecord,
): Effect.Effect<UTxO, UtxosCommandError> =>
  Effect.try({
    try: () => {
      const input = CML.TransactionInput.from_cbor_bytes(entry.outref);
      const output = CML.TransactionOutput.from_cbor_bytes(entry.output);
      return coreToUtxo(CML.TransactionUnspentOutput.new(input, output));
    },
    catch: (cause) =>
      new UtxosCommandError({
        message: `Failed to decode Midgard UTxO ${entry.outref.toString("hex")}`,
        cause,
      }),
  });

export const sumAssets = (utxos: readonly UTxO[]): Readonly<Assets> => {
  const totals: Assets = { lovelace: 0n };
  for (const utxo of utxos) {
    for (const [unit, amount] of Object.entries(utxo.assets)) {
      totals[unit] = (totals[unit] ?? 0n) + amount;
    }
  }
  return totals;
};

export const formatUtxosResult = (result: UtxosCommandResult): string =>
  JSON.stringify(
    result,
    (_key, value) => (typeof value === "bigint" ? value.toString(10) : value),
    2,
  );

export const utxosProgram = (
  address: string,
): Effect.Effect<UtxosCommandResult, DatabaseError | UtxosCommandError, Database> =>
  Effect.gen(function* () {
    const entries = yield* MempoolLedgerDB.retrieveByAddress(address);
    const decoded = yield* Effect.forEach(entries, (entry) =>
      decodeStoredUtxo({
        outref: entry.outref,
        output: entry.output,
      }),
    );
    const utxos = [...decoded].sort(canonicalUtxoOrder);

    return {
      address,
      utxoCount: utxos.length,
      totals: sumAssets(utxos),
      utxos,
    };
  });
