import { type Data, isData } from "@harmoniclabs/plutus-data";
import {
  type ConstType,
  ConstTyTag,
  UPLCDecoder,
  UPLCEncoder,
  type UPLCProgram,
} from "@harmoniclabs/uplc";

import { encodeMidgardCekPlutusData } from "./cek-constant.js";
import { plutusDataFromCborIterative } from "./plutus-data-iterative.decode.js";

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

/**
 * Reads every Data constant, including those nested in list and pair
 * constants, with the Cardano-exact Plutus Data decoder: any spelling it
 * accepts stands for its value, and a form it refuses aborts the decode.
 */
class CardanoFlatDecoder extends UPLCDecoder {
  override decodeCborData(): Data {
    return plutusDataFromCborIterative(this.decodeByteString());
  }
}

export const encodeMidgardCekCardanoFlatProgram = (
  program: UPLCProgram,
): Buffer => Buffer.from(new CardanoFlatEncoder().compile(program));

export const decodeMidgardCekCardanoFlatProgram = (
  flat: Uint8Array,
): UPLCProgram => new CardanoFlatDecoder(flat).decodeProgram();
