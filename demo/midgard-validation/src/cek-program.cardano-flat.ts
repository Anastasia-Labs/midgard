import { decodeCardanoFlatProgramOrThrow } from "./cek-program.cardano-flat.decode.js";
import {
  encodeCardanoFlatProgram,
  type FlatProgram,
} from "./cek-program.cardano-flat.encode.js";
import type { UPLCProgram } from "./uplc-term.js";

/**
 * The Cardano-exact Flat decoder: `decodeCardanoFlatProgram` returns the
 * program Cardano reads, or the refusal class and the bit offset of the
 * refused item; `CardanoFlatRefusal` is the error the throwing entries raise.
 */
export {
  type CardanoFlatDecodeResult,
  decodeCardanoFlatProgram,
} from "./cek-program.cardano-flat.decode.js";
export type {
  FlatProgram,
  FlatTerm,
} from "./cek-program.cardano-flat.encode.js";
export {
  CardanoFlatRefusal,
  type CardanoFlatRefusalClass,
} from "./cek-program.cardano-flat.reader.js";

/**
 * A program's Flat bytes, with Data constants in Cardano's canonical Data
 * CBOR, including inside list and pair constants.
 */
export const encodeMidgardCekCardanoFlatProgram = (
  program: FlatProgram,
): Buffer => Buffer.from(encodeCardanoFlatProgram(program));

/**
 * Reads a script's Flat bytes as the Cardano ledger does (Plutus V3, protocol
 * version 11), or throws the `CardanoFlatRefusal`.
 */
export const decodeMidgardCekCardanoFlatProgram = (
  flat: Uint8Array,
): UPLCProgram => decodeCardanoFlatProgramOrThrow(flat);
