import { isData } from "@harmoniclabs/plutus-data";
import {
  type ConstType,
  ConstTyTag,
  UPLCEncoder,
  type UPLCProgram,
} from "@harmoniclabs/uplc";

import { encodeMidgardCekPlutusData } from "./cek-constant.js";
import { decodeCardanoFlatProgramOrThrow } from "./cek-program.cardano-flat.decode.js";

/**
 * The Cardano-exact Flat decoder: `decodeCardanoFlatProgram` returns the
 * program Cardano reads, or the refusal class and the bit offset of the
 * refused item; `CardanoFlatRefusal` is the error the throwing entries raise.
 */
export {
  type CardanoFlatDecodeResult,
  decodeCardanoFlatProgram,
} from "./cek-program.cardano-flat.decode.js";
export {
  CardanoFlatRefusal,
  type CardanoFlatRefusalClass,
} from "./cek-program.cardano-flat.reader.js";

/** Flat uses Cardano Data CBOR inside Data constants, including nested types. */
class CardanoFlatEncoder extends UPLCEncoder {
  override encodeConstValue(type: ConstType, value: unknown): void {
    if (type[0] === ConstTyTag.data) {
      if (!isData(value))
        throw new Error("Flat Data constant contains invalid Data");
      this.encodeByteString(encodeMidgardCekPlutusData(value));
      return;
    }
    // The encoder calls this method recursively for list/pair constants.
    super.encodeConstValue(type, value);
  }
}

export const encodeMidgardCekCardanoFlatProgram = (
  program: UPLCProgram,
): Buffer => Buffer.from(new CardanoFlatEncoder().compile(program));

/**
 * Reads a script's Flat bytes as the Cardano ledger does (Plutus V3, protocol
 * version 11), or throws the `CardanoFlatRefusal`.
 */
export const decodeMidgardCekCardanoFlatProgram = (
  flat: Uint8Array,
): UPLCProgram => decodeCardanoFlatProgramOrThrow(flat);
