import {
  decodeMidgardTxOutput,
  encodeMidgardAddressText,
  midgardValueToCmlValue,
} from "@al-ft/midgard-core/codec";
import {
  type Assets,
  CML,
  type UTxO,
  valueToAssets,
} from "@lucid-evolution/lucid";
import { Data as EffectData, Effect } from "effect";

import { parseTxOutRefLabel } from "@/commands/command-utils.js";
import * as MempoolLedgerDB from "@/database/mempoolLedger.js";
import { DatabaseError } from "@/database/utils/common.js";
import { Database } from "@/services/database.js";
import { compareOutRefs } from "@/tx-context.js";

/**
 * Tagged error for the `utxos` command path.
 */
export class UtxosCommandError extends EffectData.TaggedError(
  "UtxosCommandError",
)<{
  message: string;
  cause: unknown;
}> {}

/**
 * Raw UTxO record shape read from the database.
 */
export type StoredUtxoRecord = {
  readonly outref: Buffer;
  readonly output: Buffer;
};

/**
 * Stable JSON shape exposed by the HTTP UTxO endpoints.
 */
export type EncodedStoredUtxo = {
  readonly outref: string;
  readonly outputCbor: string;
};

/**
 * Structured result returned by the `utxos` command.
 */
export type UtxosCommandResult = {
  readonly address: string;
  readonly utxoCount: number;
  readonly totals: Readonly<Assets>;
  readonly utxos: readonly UTxO[];
};

/**
 * Parses a POST /utxos request body containing a JSON array of
 * `txHash#outputIndex` strings.
 */
export const parseTxOutRefsRequest = (body: unknown): readonly Buffer[] => {
  if (!Array.isArray(body)) {
    throw new Error(
      "Request body must be a JSON array of `txHash#outputIndex` strings.",
    );
  }

  const seen = new Set<string>();
  return body.map((item, index) => {
    const outRef = parseTxOutRefLabel(
      item,
      `txOutRefs[${index.toString()}]`,
    ).cbor;
    const outRefHex = outRef.toString("hex");
    if (seen.has(outRefHex)) {
      throw new Error(
        `Duplicate txOutRef provided at txOutRefs[${index.toString()}].`,
      );
    }
    seen.add(outRefHex);
    return outRef;
  });
};

/**
 * Requires the explicit `?by-outrefs` selector for batch outref lookups.
 */
export const requireByOutRefsSelector = (
  params: Readonly<Record<string, unknown>>,
): void => {
  if (!Object.hasOwn(params, "by-outrefs")) {
    throw new Error("POST /utxos requires the `?by-outrefs` query selector.");
  }
};

/**
 * Decodes one stored outref/output pair into a Lucid `UTxO`.
 */
export const decodeStoredUtxo = (
  entry: StoredUtxoRecord,
): Effect.Effect<UTxO, UtxosCommandError> =>
  Effect.try({
    try: () => {
      const input = CML.TransactionInput.from_cbor_bytes(entry.outref);
      const output = decodeMidgardTxOutput(entry.output);
      const outputIndex = Number(input.index());
      if (!Number.isSafeInteger(outputIndex)) {
        throw new Error("output index exceeds JavaScript safe integer range");
      }
      return {
        txHash: input.transaction_id().to_hex(),
        outputIndex,
        address: encodeMidgardAddressText(output.address),
        assets: valueToAssets(midgardValueToCmlValue(output.value)) as Assets,
        ...(output.datum === undefined
          ? {}
          : { datum: output.datum.cbor.toString("hex") }),
      } satisfies UTxO;
    },
    catch: (cause) =>
      new UtxosCommandError({
        message: `Failed to decode Midgard UTxO ${entry.outref.toString("hex")}`,
        cause,
      }),
  });

/**
 * Sums assets across a collection of UTxOs.
 */
export const sumAssets = (utxos: readonly UTxO[]): Readonly<Assets> => {
  const totals: Assets = { lovelace: 0n };
  for (const utxo of utxos) {
    for (const [unit, amount] of Object.entries(utxo.assets)) {
      totals[unit] = (totals[unit] ?? 0n) + amount;
    }
  }
  return totals;
};

/**
 * Encodes one ledger entry into the stable HTTP response shape.
 */
export const encodeStoredUtxo = (
  entry: StoredUtxoRecord,
): EncodedStoredUtxo => ({
  outref: entry.outref.toString("hex"),
  outputCbor: entry.output.toString("hex"),
});

/**
 * Encodes a list of ledger entries into the stable HTTP response shape.
 */
export const encodeStoredUtxos = (
  entries: readonly StoredUtxoRecord[],
): readonly EncodedStoredUtxo[] => entries.map(encodeStoredUtxo);

/**
 * Orders fetched ledger entries by the requested outref sequence, omitting
 * misses while preserving request order.
 */
export const orderStoredUtxosByOutRef = (
  requestedOutRefs: readonly Buffer[],
  entries: readonly StoredUtxoRecord[],
): readonly StoredUtxoRecord[] => {
  const byOutRef = new Map(
    entries.map((entry) => [entry.outref.toString("hex"), entry] as const),
  );
  return requestedOutRefs.flatMap((outRef) => {
    const entry = byOutRef.get(outRef.toString("hex"));
    return entry === undefined ? [] : [entry];
  });
};

/**
 * Reads, decodes, orders, and summarizes mempool-ledger UTxOs for an address.
 */
export const utxosProgram = (
  address: string,
): Effect.Effect<
  UtxosCommandResult,
  DatabaseError | UtxosCommandError,
  Database
> =>
  Effect.gen(function* () {
    const entries = yield* MempoolLedgerDB.retrieveByAddress(address);
    const decoded = yield* Effect.forEach(entries, decodeStoredUtxo);
    const utxos = [...decoded].sort(compareOutRefs);

    return {
      address,
      utxoCount: utxos.length,
      totals: sumAssets(utxos),
      utxos,
    };
  });

/**
 * Reads mempool-ledger UTxOs by raw TxOutRef CBOR bytes and preserves the
 * caller's requested order for found entries.
 */
export const utxosByTxOutRefsProgram = (
  txOutRefs: readonly Buffer[],
): Effect.Effect<readonly StoredUtxoRecord[], DatabaseError, Database> =>
  Effect.gen(function* () {
    if (txOutRefs.length === 0) {
      return [];
    }
    const entries = yield* MempoolLedgerDB.retrieveByTxOutRefs(txOutRefs);
    return orderStoredUtxosByOutRef(txOutRefs, entries);
  });
