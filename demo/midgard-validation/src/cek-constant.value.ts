import { type Data, DataB, DataI, DataMap } from "./uplc-data.js";

/** Cardano's bound on a Value's currency symbols and token names. */
export const MIDGARD_CEK_MAX_VALUE_KEY_BYTES = 32;

/** Cardano's Value quantities are signed 128-bit integers. */
export const MIDGARD_CEK_MIN_VALUE_QUANTITY = -(1n << 127n);
export const MIDGARD_CEK_MAX_VALUE_QUANTITY = (1n << 127n) - 1n;

/** Haskell `ByteString` order: bytewise, and a proper prefix sorts first. */
const bytesLessThan = (left: Uint8Array, right: Uint8Array): boolean =>
  Buffer.compare(Buffer.from(left), Buffer.from(right)) < 0;

const keyFollows = (key: Data, previous: Uint8Array | undefined): boolean =>
  key instanceof DataB &&
  key.bytes.length <= MIDGARD_CEK_MAX_VALUE_KEY_BYTES &&
  (previous === undefined || bytesLessThan(previous, key.bytes));

/**
 * A Value constant's payload is its `valueData` map, which Cardano's decoder
 * admits only in canonical form: currency symbols and token names strictly
 * ascending and at most 32 bytes, no empty token map, and every quantity a
 * non-zero signed 128-bit integer.
 */
export const isMidgardCekCanonicalValueData = (payload: Data): boolean => {
  if (!(payload instanceof DataMap)) return false;
  let previousPolicy: Uint8Array | undefined;
  for (const { fst: policy, snd: tokens } of (payload as DataMap<Data, Data>)
    .map) {
    if (!keyFollows(policy, previousPolicy) || !(tokens instanceof DataMap)) {
      return false;
    }
    if (tokens.map.length === 0) return false;
    let previousToken: Uint8Array | undefined;
    for (const { fst: token, snd: quantity } of (tokens as DataMap<Data, Data>)
      .map) {
      if (
        !keyFollows(token, previousToken) ||
        !(quantity instanceof DataI) ||
        quantity.int === 0n ||
        quantity.int < MIDGARD_CEK_MIN_VALUE_QUANTITY ||
        quantity.int > MIDGARD_CEK_MAX_VALUE_QUANTITY
      ) {
        return false;
      }
      previousToken = (token as DataB).bytes;
    }
    previousPolicy = (policy as DataB).bytes;
  }
  return true;
};

/** Cardano's memory size of a Value: its number of (policy, token) entries. */
export const midgardCekValueEntryCount = (payload: Data): bigint => {
  if (!(payload instanceof DataMap)) {
    throw new Error("V1 value payload is not DataMap");
  }
  let total = 0n;
  for (const { snd: tokens } of payload.map) {
    if (!(tokens instanceof DataMap)) {
      throw new Error("V1 value token map is not DataMap");
    }
    total += BigInt(tokens.map.length);
  }
  return total;
};
