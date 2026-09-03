import { encodeCbor } from "./cbor.js";
import { computeHash32 } from "./hash.js";

export const MIDGARD_NATIVE_TX_VERSION = 1n;
export const MIDGARD_POSIX_TIME_NONE = -1n;
export const MIDGARD_NATIVE_NETWORK_ID_NONE = 255n;
export const EMPTY_CBOR_LIST = encodeCbor([]);
export const EMPTY_CBOR_NULL = encodeCbor(null);
export const EMPTY_NULL_ROOT = computeHash32(EMPTY_CBOR_NULL);
