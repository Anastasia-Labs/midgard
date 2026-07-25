import { Data } from "@lucid-evolution/lucid";

import { aikenSerialisedPlutusDataCborPreservingMapOrder } from "../plutus-data-cbor.js";
import { MidgardTxCodecError, MidgardTxCodecErrorCodes } from "./errors.js";

export type MidgardDatum = {
  readonly kind: "inline";
  readonly cbor: Buffer;
};

const fail = (message: string, detail?: string): never => {
  throw new MidgardTxCodecError(
    MidgardTxCodecErrorCodes.InvalidFieldType,
    message,
    detail,
  );
};

/**
 * Validates the exact Cardano Plutus-Data encoding used by Aiken's
 * `cbor.serialise`. This intentionally delegates the normalization rule to the
 * same Lucid/CML parser used at the SDK boundary, then applies the ledger
 * `serialiseData` framing: non-empty lists and constructor fields are
 * indefinite, Data maps are definite and retain their explicit pair order,
 * and byte strings above 64 bytes use canonical 64-byte chunks. A
 * structurally decodable alternate encoding is rejected rather than silently
 * normalized.
 */
export const decodeMidgardDatum = (bytes: Uint8Array): MidgardDatum => {
  const source = Buffer.from(bytes);
  if (source.length === 0) {
    return fail("PlutusData datum must not be empty");
  }
  try {
    Data.from(source.toString("hex"));
  } catch (cause) {
    return fail("Invalid PlutusData datum CBOR", String(cause));
  }
  let canonical: Buffer;
  try {
    canonical = Buffer.from(
      aikenSerialisedPlutusDataCborPreservingMapOrder(
        source.toString("hex"),
      ),
      "hex",
    );
  } catch (cause) {
    return fail("Unsupported PlutusData datum value", String(cause));
  }
  if (!canonical.equals(source)) {
    return fail("PlutusData datum is not canonical");
  }
  return Object.freeze({ kind: "inline", cbor: canonical });
};

export const encodeMidgardDatum = (datum: MidgardDatum): Buffer =>
  decodeMidgardDatum(datum.cbor).cbor;
