import { describe, expect, it } from "vitest";

import {
  computeDeploymentManifestId,
  verifyDeploymentManifestIdentity,
  verifyFinalizedDeploymentManifest,
} from "../src/deployment-manifest-identity.js";
import { finalizedManifest } from "./deployment-manifest-identity.finalized-manifest.js";

const T = { slot: 4_200, blockHash: "ab".repeat(32) };

const rehashed = (identity: Record<string, unknown>) => ({
  ...identity,
  manifestId: computeDeploymentManifestId(identity),
});

const withoutId = () => {
  const { manifestId: _manifestId, ...identity } = finalizedManifest();
  return identity;
};

describe("the deployment manifest's L1 origin", () => {
  it("M-d: a manifest without l1Origin keeps the id it had before the key existed", () => {
    // Computed by the identity code before l1Origin was a manifest key.
    expect(
      computeDeploymentManifestId({
        schemaVersion: "midgard-deployment-manifest-v1",
        network: "Preprod",
        hubOracleOneShot: { txHash: "11".repeat(32), outputIndex: 0 },
        l1Finality: { confirmationDepth: 10, commitEventDepth: 3 },
      }),
    ).toBe("27205e037e4531db395981ab5d385a1fde2634232d3a22b209d57865913d834c");
    const manifest = finalizedManifest();
    expect(Object.keys(manifest)).not.toContain("l1Origin");
    expect(verifyFinalizedDeploymentManifest(manifest)).toBe(manifest);
  });

  it("M-c: a manifest that records its L1 origin verifies, and the origin is part of its id", () => {
    const manifest = rehashed({ ...withoutId(), l1Origin: T });
    expect(verifyFinalizedDeploymentManifest(manifest).l1Origin).toEqual(T);
    expect(manifest.manifestId).not.toBe(finalizedManifest().manifestId);
    expect(() =>
      verifyDeploymentManifestIdentity({
        ...manifest,
        l1Origin: { ...T, slot: T.slot + 1 },
      }),
    ).toThrow(/manifest id mismatch/u);
  });

  it.each([
    ["the text form", "4200.".concat("ab".repeat(32))],
    ["an extra key", { ...T, height: 7 }],
    ["an uppercase hash", { ...T, blockHash: "AB".repeat(32) }],
    ["a negative slot", { ...T, slot: -1 }],
    ["null", null],
  ])(
    "M-c: a manifest whose l1Origin is %s is refused, even when rehashed",
    (_name, l1Origin) => {
      expect(() =>
        verifyDeploymentManifestIdentity(
          rehashed({ ...withoutId(), l1Origin }),
        ),
      ).toThrow(/l1Origin/u);
    },
  );

  it("M-c: any other unknown root key is still refused", () => {
    expect(() =>
      verifyDeploymentManifestIdentity(rehashed({ ...withoutId(), origin: T })),
    ).toThrow(/value\.origin is unexpected/u);
  });
});
