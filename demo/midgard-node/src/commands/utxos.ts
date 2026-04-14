import * as MempoolLedgerDB from "@/database/mempoolLedger.js";
import { DatabaseError } from "@/database/utils/common.js";
import { Database } from "@/services/database.js";
import { isHexString } from "@/utils.js";
import {
  CML,
  coreToUtxo,
  getAddressDetails,
  type Assets,
  type UTxO,
} from "@lucid-evolution/lucid";
import { Data as EffectData, Effect } from "effect";

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
  readonly value: string;
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
 * Canonical lexicographic ordering for Cardano UTxOs.
 */
export const canonicalUtxoOrder = (a: UTxO, b: UTxO): number => {
  const hashOrder = a.txHash.localeCompare(b.txHash);
  if (hashOrder !== 0) {
    return hashOrder;
  }
  return a.outputIndex - b.outputIndex;
};

/**
 * Validates and normalizes a bech32 address passed to the command.
 */
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

/**
 * Parses a raw TxOutRef CBOR hex string and normalizes it to canonical CBOR.
 */
export const parseTxOutRefCborHex = (
  value: unknown,
  fieldName = "txOutRef",
): Buffer => {
  if (typeof value !== "string") {
    throw new Error(`${fieldName} must be a hex string.`);
  }

  const normalized = value.trim().toLowerCase();
  if (normalized.length === 0) {
    throw new Error(`${fieldName} must not be empty.`);
  }
  if (normalized.length % 2 !== 0 || !isHexString(normalized)) {
    throw new Error(`${fieldName} must be an even-length hex string.`);
  }

  try {
    const parsed = CML.TransactionInput.from_cbor_bytes(
      Buffer.from(normalized, "hex"),
    );
    return Buffer.from(parsed.to_cbor_bytes());
  } catch (cause) {
    throw new Error(
      `Invalid ${fieldName}: failed to decode TxOutRef CBOR (${String(cause)}).`,
    );
  }
};

/**
 * Parses a textual TxOutRef in `txHash#outputIndex` form and normalizes it to
 * canonical CBOR bytes.
 */
export const parseTxOutRefLabel = (
  value: unknown,
  fieldName = "txOutRef",
): Buffer => {
  if (typeof value !== "string") {
    throw new Error(`${fieldName} must be a string.`);
  }

  const normalized = value.trim().toLowerCase();
  if (normalized.length === 0) {
    throw new Error(`${fieldName} must not be empty.`);
  }

  const parts = normalized.split("#");
  if (parts.length !== 2) {
    throw new Error(
      `${fieldName} must use the format <txHash>#<outputIndex>.`,
    );
  }

  const [txHash, outputIndexRaw] = parts;
  if (
    txHash === undefined ||
    txHash.length !== 64 ||
    !isHexString(txHash)
  ) {
    throw new Error(`${fieldName}.txHash must be a 32-byte hex string.`);
  }
  if (outputIndexRaw === undefined || !/^\d+$/.test(outputIndexRaw)) {
    throw new Error(`${fieldName}.outputIndex must be a non-negative integer.`);
  }

  try {
    return Buffer.from(
      CML.TransactionInput.new(
        CML.TransactionHash.from_hex(txHash),
        BigInt(outputIndexRaw),
      ).to_cbor_bytes(),
    );
  } catch (cause) {
    throw new Error(
      `Invalid ${fieldName}: failed to encode TxOutRef (${String(cause)}).`,
    );
  }
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
    const outRef = parseTxOutRefLabel(item, `txOutRefs[${index.toString()}]`);
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
      const output = CML.TransactionOutput.from_cbor_bytes(entry.output);
      return coreToUtxo(CML.TransactionUnspentOutput.new(input, output));
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
  value: entry.output.toString("hex"),
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
 * Formats the command result as stable JSON, stringifying bigint values.
 */
export const formatUtxosResult = (result: UtxosCommandResult): string =>
  JSON.stringify(
    result,
    (_key, value) => (typeof value === "bigint" ? value.toString(10) : value),
    2,
  );

/**
 * Reads, decodes, orders, and summarizes mempool-ledger UTxOs for an address.
 */
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
    return orderStoredUtxosByOutRef(
      txOutRefs,
      entries.map((entry) => ({
        outref: entry.outref,
        output: entry.output,
      })),
    );
  });
