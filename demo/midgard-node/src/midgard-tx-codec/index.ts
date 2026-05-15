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

// Version of the Midgard transaction binary format. The midgard-ts encoding
// itself has no on-wire version byte (unlike the old CBOR "native" codec); this
// constant is the protocol-info surface clients use to gate compatibility, and
// must be bumped whenever the midgard-ts wire format changes incompatibly.
export const MIDGARD_NATIVE_TX_VERSION = 1n;

// Transitional: tests and a few builder-side callers still reference the
// old midgard-core "native" CBOR codec (which lucid-midgard's builder uses
// internally before re-encoding to midgard-ts on the wire). These re-exports
// will go away once the tests are rewritten against midgard-ts directly
// (Phase 6) and lucid-midgard's builder operates on midgard-ts structurally
// (Phase 5-main).
export {
  cardanoTxBytesToMidgardNativeTxFullBytes,
  computeMidgardNativeTxIdFromFull,
  decodeMidgardNativeTxBodyCompact,
  decodeMidgardNativeTxCompact,
  decodeMidgardNativeTxFull,
  decodeMidgardNativeTxWitnessSetCompact,
  deriveMidgardNativeTxBodyCompactFromFull,
  deriveMidgardNativeTxCompact,
  deriveMidgardNativeTxWitnessSetCompactFromFull,
  encodeMidgardNativeTxBodyCompact,
  encodeMidgardNativeTxCompact,
  encodeMidgardNativeTxFull,
  encodeMidgardNativeTxWitnessSetCompact,
  midgardNativeTxFullToCardanoTxEncoding,
  verifyMidgardNativeTxFullConsistency,
  MIDGARD_NATIVE_NETWORK_ID_NONE,
  MIDGARD_POSIX_TIME_NONE,
} from "@al-ft/midgard-core/codec/native";
export type {
  MidgardNativeTxBodyFull,
  MidgardNativeTxFull,
  MidgardNativeTxWitnessSetFull,
} from "@al-ft/midgard-core/codec/native";
