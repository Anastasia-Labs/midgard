import {
  CborReadError,
  readArray,
  readHead,
  skipItem,
  slice,
} from "../cbor/reader.js";
import { blake2b256 } from "../codec.js";
import type { OutputSummary } from "../types.js";
import { type BodyFields, readBody } from "./block.js";

export class TxDecodeError extends Error {
  constructor(message: string, options?: { cause?: unknown }) {
    super(message, options);
    this.name = "TxDecodeError";
  }
}

/** A transaction's id (blake2b-256 of its body's exact bytes) and body fields. */
export type DecodedTransaction = Readonly<
  BodyFields & { hash: Buffer; bodyCbor: Buffer }
>;

/**
 * Decodes a transaction from its CBOR: the whole transaction
 * (`[body, witnesses, isValid, auxiliary]`, or the three-element form before
 * Alonzo) or the bare body map. The id is blake2b-256 of the body's bytes
 * exactly as given, so a caller checks it against the id it asked for.
 */
export const decodeTransaction = (bytes: Uint8Array): DecodedTransaction => {
  try {
    const end = skipItem(bytes, 0);
    if (end !== bytes.length)
      throw new CborReadError("trailing bytes after the transaction", end);
    let bodyCbor: Buffer;
    if (readHead(bytes, 0).major === 5) bodyCbor = slice(bytes, 0, end);
    else {
      const tx = readArray(bytes, 0);
      const bodyAt = tx.items[0];
      if (bodyAt === undefined || tx.items.length < 3 || tx.items.length > 4)
        throw new CborReadError(
          "a transaction is [body, witnesses, isValid?, auxiliary]",
          0,
        );
      bodyCbor = slice(bytes, bodyAt, skipItem(bytes, bodyAt));
    }
    return { hash: blake2b256(bodyCbor), bodyCbor, ...readBody(bodyCbor) };
  } catch (error) {
    throw new TxDecodeError(
      `the transaction does not decode: ${error instanceof Error ? error.message : String(error)}`,
      { cause: error },
    );
  }
};

/**
 * The output a transaction created at `index`: a body output, or its
 * collateral return at index `outputs.length` (a phase-2 failure creates only
 * that one). Null when the body names no output there.
 */
export const transactionOutputAt = (
  tx: Pick<BodyFields, "outputs" | "collateralReturn">,
  index: number,
): OutputSummary | null =>
  tx.outputs[index] ??
  (index === tx.outputs.length ? tx.collateralReturn : null);
