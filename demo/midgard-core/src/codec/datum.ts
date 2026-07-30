import {
  aikenSerialisedPlutusDataCborPreservingMapOrder,
  assertMidgardPlutusDataWellFormedV1,
} from "../plutus-data-cbor.js";
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
 * `cbor.serialise`. Well-formedness is checked with the recursion-free
 * `assertMidgardPlutusDataWellFormedV1` pass, which is verdict-equivalent to
 * the recursive Lucid/CML `Data.from` probe it replaced once the canonicity
 * gate below is composed in, and — unlike CML's wasm build, which traps near
 * 1,522 nested nodes — admits every depth a maximal 16,384-byte Cardano
 * transaction can carry. The ledger `serialiseData` framing is then enforced
 * byte-exactly: non-empty lists and constructor fields are indefinite, Data
 * maps are definite and retain their explicit pair order, and byte strings
 * above 64 bytes use canonical 64-byte chunks. A structurally decodable
 * alternate encoding is rejected rather than silently normalized.
 */
export const decodeMidgardDatum = (bytes: Uint8Array): MidgardDatum => {
  const source = Buffer.from(bytes);
  if (source.length === 0) {
    return fail("PlutusData datum must not be empty");
  }
  try {
    assertMidgardPlutusDataWellFormedV1(source);
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
