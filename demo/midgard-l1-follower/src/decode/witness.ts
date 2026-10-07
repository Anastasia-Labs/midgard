import {
  readArray,
  readMap,
  readSmallUint,
  skipItem,
  slice,
  untag,
} from "../cbor/reader.js";
import { blake2b256 } from "../codec.js";

/** Witness-set key 4: `plutus_data`, an array or a tag-258 set. */
const PLUTUS_DATA = 4;
const SET_TAG = 258n;

/**
 * The witness datum whose blake2b-256 of its exact CBOR is `hash`, or null.
 * Bytes are sliced from the witness set as the ledger hashed them.
 */
export const witnessDatum = (
  witnessCbor: Uint8Array,
  hash: Buffer,
): Buffer | null => {
  for (const { key, value } of readMap(witnessCbor, 0).entries) {
    if (readSmallUint(witnessCbor, key) !== PLUTUS_DATA) continue;
    const { items } = readArray(
      witnessCbor,
      untag(witnessCbor, value, SET_TAG),
    );
    for (const start of items) {
      const datum = slice(witnessCbor, start, skipItem(witnessCbor, start));
      if (blake2b256(datum).equals(hash)) return datum;
    }
  }
  return null;
};
