import { fromText } from "@lucid-evolution/lucid";

/**
 * Shared token names and genesis constants used across the SDK.
 *
 * These values must remain aligned with the on-chain contracts because many of
 * the transaction builders derive asset units directly from them.
 */
export const NODE_ASSET_NAME = fromText("Node");

export const HUB_ORACLE_ASSET_NAME = fromText("Hub Oracle");

// The real scheduler witness token uses the empty asset name on-chain.
export const SCHEDULER_ASSET_NAME = fromText("");

export const ESCAPE_HATCH_ASSET_NAME = fromText("Escape Hatch");

export const FRAUD_PROOF_CATALOGUE_ASSET_NAME = fromText(
  "Fraud Proof Catalogue",
);

// Operator-list anchor values must stay constant across prepend/append/remove
// transitions, so initialize them with headroom for later datum growth.

export const GENESIS_HASH_28 = "00".repeat(28);
export const GENESIS_HASH_32 = "00".repeat(32);

export const INITIAL_PROTOCOL_VERSION = 0n;
