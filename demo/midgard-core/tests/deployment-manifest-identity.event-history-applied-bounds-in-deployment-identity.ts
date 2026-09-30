import "./deployment-manifest-identity.deployment-manifest-v1-shared-identity.js";

import { credentialToAddress } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  computeDeploymentManifestId,
  parseDeploymentManifestEventHistoryBounds,
  parseDeploymentManifestEventHistoryRecipe,
  verifyFinalizedDeploymentManifest,
} from "../src/deployment-manifest-identity.js";
import { finalizedManifest } from "./deployment-manifest-identity.finalized-manifest.js";

describe("event history applied bounds in deployment identity", () => {
  const bounds = {
    inlineLimitBytes: "512",
    maxPayloadBytes: "5000",
    maxPayloadNodes: "512",
  };
  it("requires explicit canonical integers and a total bound covering inline data", () => {
    expect(parseDeploymentManifestEventHistoryBounds(bounds)).toEqual(bounds);
    for (const invalid of [
      undefined,
      {},
      { ...bounds, inlineLimitBytes: 512 },
      { ...bounds, maxPayloadNodes: "0" },
      { ...bounds, maxPayloadBytes: "04999" },
      { ...bounds, maxPayloadBytes: "511" },
      { ...bounds, inlineLimitBytes: "-1" },
      { ...bounds, maxPayloadNodes: "9007199254740992" },
      { ...bounds, extra: true },
    ])
      expect(() =>
        parseDeploymentManifestEventHistoryBounds(invalid),
      ).toThrow();
  });
  it("transition history requires both addresses and binds each address and bound to identity", () => {
    for (const key of ["deposit", "withdrawal"] as const) {
      const manifest = finalizedManifest();
      const entry = manifest.contracts.fraudProofTransitionTrace!;
      const addresses = entry.eventHistoryRetentionAddresses as Record<
        string,
        string
      >;
      addresses[key] = credentialToAddress("Preview", {
        type: "Script",
        hash: "ab".repeat(28),
      });
      expect(() => verifyFinalizedDeploymentManifest(manifest)).toThrow(
        /id mismatch/,
      );
      const { manifestId: _old, ...changed } = manifest;
      manifest.manifestId = computeDeploymentManifestId(changed);
      expect(verifyFinalizedDeploymentManifest(manifest)).toBe(manifest);
      delete addresses[key];
      const { manifestId: _missing, ...missing } = manifest;
      manifest.manifestId = computeDeploymentManifestId(missing);
      expect(() => verifyFinalizedDeploymentManifest(manifest)).toThrow(
        /eventHistoryRetentionAddresses/,
      );
    }
    const manifest = finalizedManifest();
    manifest.contracts.fraudProofTransitionTrace!.eventHistoryBounds = {
      ...bounds,
      maxPayloadNodes: "513",
    };
    expect(() => verifyFinalizedDeploymentManifest(manifest)).toThrow(
      /id mismatch/,
    );
  });
  for (const name of [
    "fraudProofFabricatedDeposit",
    "fraudProofFabricatedWithdrawal",
  ])
    it(`${name} bounds are required and bind the complete manifest identity`, () => {
      const manifest = finalizedManifest();
      verifyFinalizedDeploymentManifest(manifest);
      const entry = manifest.contracts[name]!;
      const before = manifest.manifestId;
      entry.eventHistoryBounds = { ...bounds, maxPayloadNodes: "513" };
      expect(() => verifyFinalizedDeploymentManifest(manifest)).toThrow(
        /id mismatch/,
      );
      const { manifestId: _id, ...identity } = manifest;
      manifest.manifestId = computeDeploymentManifestId(identity);
      expect(manifest.manifestId).not.toBe(before);
      // Syntax/identity acceptance is separate from reapplying the blueprint.
      expect(verifyFinalizedDeploymentManifest(manifest)).toBe(manifest);
      const addressBefore = manifest.manifestId;
      entry.eventHistoryRetentionAddress = credentialToAddress("Preprod", {
        type: "Script",
        hash: "ef".repeat(28),
      });
      expect(() => verifyFinalizedDeploymentManifest(manifest)).toThrow(
        /id mismatch/,
      );
      const { manifestId: _address, ...withAddress } = manifest;
      manifest.manifestId = computeDeploymentManifestId(withAddress);
      expect(manifest.manifestId).not.toBe(addressBefore);
      expect(verifyFinalizedDeploymentManifest(manifest)).toBe(manifest);
      delete entry.eventHistoryRetentionAddress;
      const { manifestId: _missingAddress, ...withoutAddress } = manifest;
      manifest.manifestId = computeDeploymentManifestId(withoutAddress);
      expect(() => verifyFinalizedDeploymentManifest(manifest)).toThrow(
        /eventHistoryRetentionAddress/,
      );
      entry.eventHistoryRetentionAddress = credentialToAddress("Preprod", {
        type: "Script",
        hash: "ef".repeat(28),
      });
      delete entry.eventHistoryBounds;
      const { manifestId: _changed, ...missing } = manifest;
      manifest.manifestId = computeDeploymentManifestId(missing);
      expect(() => verifyFinalizedDeploymentManifest(manifest)).toThrow(
        /eventHistoryBounds/,
      );
    });
});

describe("event history list deployment recipes", () => {
  it("requires exact nonce, kind, policy and positive canonical protection parameters", () => {
    const recipe =
      finalizedManifest().contracts.depositMint!.eventHistoryRecipe!;
    expect(parseDeploymentManifestEventHistoryRecipe(recipe)).toEqual(recipe);
    for (const invalid of [
      { ...recipe, kind: "Unknown" },
      { ...recipe, hubPolicyId: "ab" },
      { ...recipe, protectionDurationMs: "0" },
      { ...recipe, protectionDurationMs: "02000" },
      { ...recipe, protectionDurationMs: "9007199254740992" },
      {
        ...recipe,
        initializationNonce: { txHash: "11".repeat(32), outputIndex: -1 },
      },
      { ...recipe, extra: true },
    ])
      expect(() =>
        parseDeploymentManifestEventHistoryRecipe(invalid),
      ).toThrow();
  });

  for (const name of ["depositMint", "withdrawalMint"] as const) {
    it(`${name} requires a recipe and binds its parameters to manifest identity`, () => {
      const manifest = finalizedManifest();
      const entry = manifest.contracts[name]!;
      entry.eventHistoryRecipe = {
        ...entry.eventHistoryRecipe!,
        protectionDurationMs: "2001",
      };
      expect(() => verifyFinalizedDeploymentManifest(manifest)).toThrow(
        /id mismatch/u,
      );
      const { manifestId: _before, ...changed } = manifest;
      manifest.manifestId = computeDeploymentManifestId(changed);
      expect(verifyFinalizedDeploymentManifest(manifest)).toBe(manifest);
      delete entry.eventHistoryRecipe;
      const { manifestId: _after, ...missing } = manifest;
      manifest.manifestId = computeDeploymentManifestId(missing);
      expect(() => verifyFinalizedDeploymentManifest(manifest)).toThrow(
        /eventHistoryRecipe/u,
      );
    });

    it(`${name} cannot declare a nonce outside its deployment`, () => {
      const manifest = finalizedManifest();
      const entry = manifest.contracts[name]!;
      entry.eventHistoryRecipe = {
        ...entry.eventHistoryRecipe!,
        initializationNonce: { txHash: "aa".repeat(32), outputIndex: 0 },
      };
      const { manifestId: _before, ...changed } = manifest;
      manifest.manifestId = computeDeploymentManifestId(changed);
      expect(() => verifyFinalizedDeploymentManifest(manifest)).toThrow(
        /history recipe/u,
      );
    });
  }
});
