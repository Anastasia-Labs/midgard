import { describe, expect, expectTypeOf, it } from "vitest";

import {
  computeDeploymentManifestId,
  computeDeploymentManifestJsonDigest,
  verifyDeploymentManifestIdentity,
  verifyFinalizedDeploymentManifest,
} from "../src/deployment-manifest-identity.js";
import { finalizedManifest } from "./deployment-manifest-identity.finalized-manifest.js";

describe("finalized deployment manifest", () => {
  it("returns a typed view of the original document, including on repeated verification", () => {
    const document = finalizedManifest();
    const verified = verifyFinalizedDeploymentManifest(document);
    expectTypeOf(verified.network).toEqualTypeOf<
      "Mainnet" | "Preprod" | "Preview" | "Custom"
    >();
    expectTypeOf(verified.artifacts.blueprintHash).toEqualTypeOf<string>();
    expectTypeOf(
      verified.cardanoProtocolParameters.snapshot.maxTxSize,
    ).toEqualTypeOf<string>();
    expect(verified).toBe(document);
    expect(verifyFinalizedDeploymentManifest(document)).toBe(document);
    expect(Object.isFrozen(document)).toBe(false);
    expect(Object.isFrozen(document.artifacts)).toBe(false);
  });

  it.each([
    [
      "network",
      (manifest: ReturnType<typeof finalizedManifest>) => {
        manifest.network = "unsupported";
      },
      /compiled profile/,
    ],
    [
      "blueprint identity",
      (manifest: ReturnType<typeof finalizedManifest>) => {
        manifest.artifacts.blueprintHash = "00";
      },
      /blueprintHash/,
    ],
    [
      "protocol parameter snapshot",
      (manifest: ReturnType<typeof finalizedManifest>) => {
        manifest.cardanoProtocolParameters.snapshot.maxTxSize = "0";
        manifest.cardanoProtocolParameters.digest =
          computeDeploymentManifestJsonDigest(
            manifest.cardanoProtocolParameters.snapshot,
          );
      },
      /bounds must be positive/,
    ],
    [
      "contract hash",
      (manifest: ReturnType<typeof finalizedManifest>) => {
        manifest.contracts.hubOracleMint.scriptHash = "00".repeat(28);
      },
      /scriptHash mismatch/,
    ],
    [
      "missing DA bond pool contract",
      (manifest: ReturnType<typeof finalizedManifest>) => {
        delete manifest.contracts.daBondPoolMint;
      },
      /contracts\.daBondPoolMint is required/,
    ],
    [
      "availability contract outside the four arms",
      (manifest: ReturnType<typeof finalizedManifest>) => {
        manifest.contracts.availabilityChallengeExtraWithdraw = {
          ...manifest.contracts.availabilityChallengeTimeoutWithdraw!,
        };
      },
      /contracts\.availabilityChallengeExtraWithdraw is unexpected/,
    ],
    [
      "reference publication",
      (manifest: ReturnType<typeof finalizedManifest>) => {
        manifest.referenceScripts["hub-oracle minting"].status = "pending";
      },
      /status must be confirmed/,
    ],
    [
      "required initialization",
      (manifest: ReturnType<typeof finalizedManifest>) => {
        manifest.steps.initProtocol.status = "pending";
      },
      /initProtocol.status must be complete/,
    ],
    [
      "optional step status",
      (manifest: ReturnType<typeof finalizedManifest>) => {
        manifest.steps.operatorActivation.status = "unsupported";
      },
      /operatorActivation.status is unsupported/,
    ],
  ] as const)(
    "rejects invalid %s even with a matching document ID",
    (_name, mutate, error) => {
      const manifest = finalizedManifest();
      mutate(manifest);
      const { manifestId: _manifestId, ...identity } = manifest;
      manifest.manifestId = computeDeploymentManifestId(identity);
      // Identity-only verification deliberately does not claim finalization.
      if (_name !== "network") {
        expect(verifyDeploymentManifestIdentity(manifest)).toBe(manifest);
      }
      expect(() => verifyFinalizedDeploymentManifest(manifest)).toThrow(error);
    },
  );

  it("rechecks document identity after a previously verified caller-owned value changes", () => {
    const manifest = finalizedManifest();
    verifyFinalizedDeploymentManifest(manifest);
    manifest.artifacts.blueprintHash = "66".repeat(32);
    expect(() => verifyFinalizedDeploymentManifest(manifest)).toThrow(
      /id mismatch/,
    );
  });
});
