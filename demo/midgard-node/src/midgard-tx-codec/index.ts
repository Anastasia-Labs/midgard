// Midgard transaction / block / output binary codec.
//
// The Midgard transaction encoding is the 8-byte-aligned canonical binary
// format implemented in `@al-ft/midgard-ts` (it replaced the previous
// CBOR-based "native" codec). Inner Cardano structures that Midgard does not
// re-encode (Plutus datums, scripts, redeemers, raw address bytes, cost-model
// views) are still handled via the CBOR helpers from `@al-ft/midgard-core`.
export * from "@al-ft/midgard-ts";

export {
  MIDGARD_PROTECTED_ADDRESS_HEADER_MASK,
  MIDGARD_ADDRESS_MAINNET_HRP,
  MIDGARD_ADDRESS_TESTNET_HRP,
  MIDGARD_ADDRESS_MAINNET_NETWORK_ID,
  MIDGARD_ADDRESS_TESTNET_NETWORK_ID,
  decodeMidgardAddressBytes,
  encodeMidgardAddressBytes,
  midgardAddressFromText,
  encodeMidgardAddressText,
  midgardAddressToText,
  decodeMidgardAddressText,
  isProtectedMidgardAddress,
  unprotectMidgardAddress,
  protectMidgardAddress,
  paymentCredentialFromMidgardAddress,
} from "@al-ft/midgard-core/codec/address";
export type {
  MidgardAddress,
  MidgardCredentialKind,
  MidgardCredential,
  DecodedMidgardAddress,
} from "@al-ft/midgard-core/codec/address";

export * from "@al-ft/midgard-core/codec/script-language-views";
export * from "@al-ft/midgard-core/codec/errors";
export * from "@al-ft/midgard-core/codec/cbor";

/** Version of the Midgard transaction binary codec. */
export const MIDGARD_NATIVE_TX_VERSION = 1n;

// Transitional: a few callers still consume CBOR byte-list preimages produced
// by the (not-yet-migrated) lucid-midgard tx builder.
export { decodeMidgardNativeByteListPreimage } from "@al-ft/midgard-core/codec/native";
