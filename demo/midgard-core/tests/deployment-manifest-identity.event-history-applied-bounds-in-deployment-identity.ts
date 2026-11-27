import "./deployment-manifest-identity.deployment-manifest-v1-shared-identity.js";

import { credentialToAddress } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  parseDeploymentManifestEventHistoryBounds,
  parseDeploymentManifestEventHistoryRecipe,
  verifyFinalizedDeploymentManifest,
} from "../src/deployment-manifest-identity.js";
import { finalizedManifest } from "./deployment-manifest-identity.finalized-manifest.js";

describe("event history applied bounds in the deployment manifest", () => {
  const bounds = {
    inlineLimitBytes: "512",
    maxPayloadBytes: "5000",
    maxPayloadNodes: "512",
  };
  it("requires explicit canonical integers and a total bound covering inline data", () => {
    expect(parseDeploymentManifestEventHistoryBounds(bounds)).toEqual(bounds);
    const largest = { ...bounds, maxPayloadNodes: "1024" };
    expect(parseDeploymentManifestEventHistoryBounds(largest)).toEqual(largest);
    for (const invalid of [
      { ...bounds, maxPayloadNodes: "1025" },
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
  it("transition history requires both addresses", () => {
    for (const key of ["deposit", "withdrawal"] as const) {
      const manifest = finalizedManifest();
      verifyFinalizedDeploymentManifest(manifest);
      const entry = manifest.contracts.fraudProofTransitionTrace!;
      const addresses = entry.eventHistoryRetentionAddresses as Record<
        string,
        string
      >;
      addresses[key] = credentialToAddress("Preview", {
        type: "Script",
        hash: "ab".repeat(28),
      });
      expect(verifyFinalizedDeploymentManifest(manifest)).toBe(manifest);
      delete addresses[key];
      expect(() => verifyFinalizedDeploymentManifest(manifest)).toThrow(
        /eventHistoryRetentionAddresses/,
      );
    }
  });
  for (const name of [
    "fraudProofFabricatedDeposit",
    "fraudProofFabricatedWithdrawal",
  ])
    it(`${name} bounds and retention address are required`, () => {
      const manifest = finalizedManifest();
      verifyFinalizedDeploymentManifest(manifest);
      const entry = manifest.contracts[name]!;
      entry.eventHistoryBounds = { ...bounds, maxPayloadNodes: "513" };
      // Syntax acceptance is separate from reapplying the blueprint.
      expect(verifyFinalizedDeploymentManifest(manifest)).toBe(manifest);
      entry.eventHistoryRetentionAddress = credentialToAddress("Preprod", {
        type: "Script",
        hash: "ef".repeat(28),
      });
      expect(verifyFinalizedDeploymentManifest(manifest)).toBe(manifest);
      delete entry.eventHistoryRetentionAddress;
      expect(() => verifyFinalizedDeploymentManifest(manifest)).toThrow(
        /eventHistoryRetentionAddress/,
      );
      entry.eventHistoryRetentionAddress = credentialToAddress("Preprod", {
        type: "Script",
        hash: "ef".repeat(28),
      });
      delete entry.eventHistoryBounds;
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
    it(`${name} requires a recipe`, () => {
      const manifest = finalizedManifest();
      const entry = manifest.contracts[name]!;
      entry.eventHistoryRecipe = {
        ...entry.eventHistoryRecipe!,
        protectionDurationMs: "2001",
      };
      expect(verifyFinalizedDeploymentManifest(manifest)).toBe(manifest);
      delete entry.eventHistoryRecipe;
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
      expect(() => verifyFinalizedDeploymentManifest(manifest)).toThrow(
        /history recipe/u,
      );
    });
  }
});
