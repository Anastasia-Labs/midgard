import { describe, expect, it } from "vitest";

import {
  verifyDeploymentManifestIdentity,
  verifyFinalizedDeploymentManifest,
} from "../src/deployment-manifest-identity.js";
import { finalizedManifest } from "./deployment-manifest-identity.finalized-manifest.js";

const T = { slot: 4_200, blockHash: "ab".repeat(32) };

describe("the deployment manifest's L1 origin", () => {
  it("M-d: a manifest without l1Origin verifies", () => {
    const manifest = finalizedManifest();
    expect(Object.keys(manifest)).not.toContain("l1Origin");
    expect(verifyFinalizedDeploymentManifest(manifest)).toBe(manifest);
  });

  it("M-c: a manifest that records its L1 origin verifies", () => {
    const manifest = { ...finalizedManifest(), l1Origin: T };
    expect(verifyFinalizedDeploymentManifest(manifest).l1Origin).toEqual(T);
  });

  it.each([
    ["the text form", "4200.".concat("ab".repeat(32))],
    ["an extra key", { ...T, height: 7 }],
    ["an uppercase hash", { ...T, blockHash: "AB".repeat(32) }],
    ["a negative slot", { ...T, slot: -1 }],
    ["null", null],
  ])("M-c: a manifest whose l1Origin is %s is refused", (_name, l1Origin) => {
    expect(() =>
      verifyDeploymentManifestIdentity({ ...finalizedManifest(), l1Origin }),
    ).toThrow(/l1Origin/u);
  });

  it.each(["origin", "manifestId"])(
    "M-c: the unknown root key %s is refused",
    (key) => {
      expect(() =>
        verifyDeploymentManifestIdentity({ ...finalizedManifest(), [key]: T }),
      ).toThrow(new RegExp(`value\\.${key} is unexpected`, "u"));
    },
  );
});
