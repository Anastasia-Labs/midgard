import type { DeploymentManifest } from "@al-ft/midgard-core/deployment-manifest-identity";
import { expect } from "vitest";

/** Inspect the actual production policy, without substituting its factory. */
export const assertPublishedFundingCustodyRoles = (
  contracts: readonly { readonly scriptHash: string; readonly role: string }[],
  manifest: DeploymentManifest,
): void => {
  const carrierHash =
    manifest.contracts.validationTraceDisputeProofItem.scriptHash;
  // Immutable publication aliases have one address and one custody role.
  expect(carrierHash).toBe(manifest.contracts.fraudProofSpend.scriptHash);
  expect(carrierHash).toBe(
    manifest.contracts.fraudProofCatalogueSpend.scriptHash,
  );
  expect(
    contracts.filter(({ scriptHash }) => scriptHash === carrierHash),
  ).toEqual([
    expect.objectContaining({ scriptHash: carrierHash, role: "field_carrier" }),
  ]);
  // The active computation chain keeps independent proof-thread custody.
  expect(contracts).toContainEqual(
    expect.objectContaining({
      scriptHash: manifest.contracts.fraudProofTransitionTrace.scriptHash,
      role: "proof_thread",
    }),
  );
};
