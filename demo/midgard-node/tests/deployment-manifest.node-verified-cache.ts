import { describe, expect, it } from "vitest";

import { parseDeploymentManifestValue } from "../src/deployment-manifest.js";
import {
  canonicalIdentity,
  canonicalManifest,
  withId,
} from "./deployment-manifest.canonical-identity.js";

/**
 * `parseDeploymentManifestValue` skips the node's common checks for a
 * manifestId that already passed them in this process. These pin the two
 * properties that make the skip sound.
 */
describe("node-verified manifest cache", () => {
  it("never remembers a manifest whose node-side validation failed", () => {
    // The core's finalized verification accepts any positive zstd level;
    // only the node's common checks cap it at 19, so a remembered failure
    // would let the second parse through.
    const identity = canonicalIdentity();
    const rejected = withId({
      ...identity,
      da: {
        ...identity.da,
        transportProfile: { ...identity.da.transportProfile, zstdLevel: 22 },
      },
    });
    for (let attempt = 0; attempt < 2; attempt += 1)
      expect(() => parseDeploymentManifestValue(rejected)).toThrow(
        /zstdLevel must not exceed 19/u,
      );
  });

  it("re-verifies identity on a remembered manifestId and refuses changed content under it", () => {
    const manifest = canonicalManifest();
    expect(parseDeploymentManifestValue(manifest)).toEqual(manifest);
    expect(parseDeploymentManifestValue(manifest)).toEqual(manifest);
    // Valid content, so only the identity hash tells it apart. Identity is
    // re-hashed before the cache is consulted, so a remembered id never
    // vouches for content it was not computed from.
    const tampered = { ...manifest, updatedAt: "2099-01-01T00:00:00.000Z" };
    expect(() => parseDeploymentManifestValue(tampered)).toThrow(
      /Deployment manifest id mismatch/u,
    );
  });
});
