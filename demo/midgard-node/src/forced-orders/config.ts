/**
 * What the node's forced-order projection (N10, plan §12) reads from the
 * deployment: the tx-order policy and order address (§5.2 rule 3) and the
 * CEK program-material credential (rule 13). Plain hex, so a soak plugin can
 * take it from JSON.
 */
import type { TrackedSet } from "@al-ft/midgard-l1-follower";
import { getAddressDetails } from "@lucid-evolution/lucid";

export type ForcedOrderConfig = Readonly<{
  /** The tx-order minting policy (56 hex). */
  policyId: string;
  /** The tx-order spending address, as raw address bytes (hex). */
  orderAddress: string;
  /** The CEK program-material script's payment credential (56 hex). */
  cekMaterialCredential: string;
}>;

/** The forced-order config of a deployment. */
export const forcedOrderConfigFromContracts = (contracts: {
  readonly txOrder: Readonly<{
    policyId: string;
    spendingScriptAddress: string;
  }>;
  readonly cekProgramMaterial: Readonly<{ spendingScriptHash: string }>;
}): ForcedOrderConfig => ({
  policyId: contracts.txOrder.policyId.toLowerCase(),
  orderAddress: getAddressDetails(
    contracts.txOrder.spendingScriptAddress,
  ).address.hex.toLowerCase(),
  cekMaterialCredential:
    contracts.cekProgramMaterial.spendingScriptHash.toLowerCase(),
});

/** Order outputs, order mints, and published CEK program material. */
export const forcedOrderTrackedSet = (
  config: ForcedOrderConfig,
): TrackedSet => ({
  addresses: new Set([config.orderAddress]),
  paymentCredentials: new Set([config.cekMaterialCredential]),
  policies: new Set([config.policyId]),
});
