import { encodeCbor } from "./cbor.js";
import { computeHash32 } from "./hash.js";
import { EMPTY_PREIMAGE_LIST } from "./native-preimage.js";

export const MIDGARD_NATIVE_TX_VERSION = 1n;
export const MIDGARD_POSIX_TIME_NONE = -1n;
export const MIDGARD_NATIVE_NETWORK_ID_NONE = 255n;
/** Binary-encoded empty preimage list — 8 zero bytes (u64 length = 0). */
export { EMPTY_PREIMAGE_LIST };
/**
 * Sentinel hash for absent `scriptIntegrityHash` / `auxiliaryDataHash` slots.
 * These hash slots are fixed-size and live outside the preimage system, so the
 * sentinel remains the blake2b of an empty CBOR `null` for hash-domain
 * continuity.
 */
export const EMPTY_CBOR_NULL = encodeCbor(null);
export const EMPTY_NULL_ROOT = computeHash32(EMPTY_CBOR_NULL);
