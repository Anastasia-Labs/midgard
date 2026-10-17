import { isData } from "@harmoniclabs/plutus-data";
import {
  type ConstType,
  ConstTyTag,
  UPLCEncoder,
  type UPLCProgram,
} from "@harmoniclabs/uplc";

import { encodeMidgardCekPlutusData } from "./cek-constant.js";

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
