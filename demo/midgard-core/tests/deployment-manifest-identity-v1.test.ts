import { describe, expect, it } from "vitest";

import {
  MIDGARD_CONSENSUS_PROFILE_V1,
  MIDGARD_CONSENSUS_PROFILE_V1_DIGEST,
  MIDGARD_DEPLOYMENT_MANIFEST_V1_SCHEMA_VERSION,
} from "../src/consensus-profile-v1.js";
import {
  computeDeploymentManifestV1Id,
  computeDeploymentManifestV1JsonDigest,
  normalizeDeploymentManifestV1JsonValue,
  verifyDeploymentManifestV1Identity,
} from "../src/deployment-manifest-identity-v1.js";

const identityInput = () => ({
  schemaVersion: MIDGARD_DEPLOYMENT_MANIFEST_V1_SCHEMA_VERSION,
  consensusProfile: MIDGARD_CONSENSUS_PROFILE_V1,
  consensusProfileDigest: MIDGARD_CONSENSUS_PROFILE_V1_DIGEST,
  network: "Preprod",
  cardanoProtocolParameters: {},
  genesis: {},
  createdAt: "2026-07-24T00:00:00.000Z",
  updatedAt: "2026-07-24T00:00:00.000Z",
  referenceScriptDeployAddress: "addr_test1reference",
  hubOracleOneShot: {},
  referenceScriptAuthPolicy: {},
  contracts: {},
  referenceScripts: {},
  da: {},
  proofEvidence: {},
  steps: {},
  validationDispute: {},
});

describe("DeploymentManifestV1 shared identity", () => {
  it("owns canonical JSON normalization and digest vectors", () => {
    const normalized = normalizeDeploymentManifestV1JsonValue({
      z: [1, 2n],
      a: { y: true, x: null },
    });
    expect(normalized).toEqual({
      z: [1, "2"],
      a: { y: true, x: null },
    });
    expect(computeDeploymentManifestV1JsonDigest(normalized)).toBe(
      "ccff47a9e0ebd42629b30db95fa7988b032093e903958b916820987a100d7cb4",
    );
    expect(
      computeDeploymentManifestV1JsonDigest({
        a: { x: null, y: true },
        z: [1, "2"],
      }),
    ).toBe("ccff47a9e0ebd42629b30db95fa7988b032093e903958b916820987a100d7cb4");
    expect(
      computeDeploymentManifestV1JsonDigest({
        a: { x: null, y: false },
        z: [1, "2"],
      }),
    ).not.toBe(
      "ccff47a9e0ebd42629b30db95fa7988b032093e903958b916820987a100d7cb4",
    );
  });

  it("rejects values outside the canonical JSON boundary", () => {
    expect(() =>
      normalizeDeploymentManifestV1JsonValue({ missing: undefined }),
    ).toThrow(/value\.missing must not be undefined/u);
    expect(() =>
      normalizeDeploymentManifestV1JsonValue({ invalid: Number.NaN }),
    ).toThrow(/must contain only finite numbers/u);
    expect(() => computeDeploymentManifestV1JsonDigest({ raw: 2n })).toThrow(
      /must contain only JSON-safe values/u,
    );
  });

  it("recomputes the exact full-manifest identity", () => {
    const identity = identityInput();
    const manifest = {
      ...identity,
      manifestId: computeDeploymentManifestV1Id(identity),
    };
    expect(verifyDeploymentManifestV1Identity(manifest)).toEqual(manifest);
  });

  it("rejects tampering, missing fields, and extra fields", () => {
    const identity = identityInput();
    const manifest = {
      ...identity,
      manifestId: computeDeploymentManifestV1Id(identity),
    };
    expect(() =>
      verifyDeploymentManifestV1Identity({
        ...manifest,
        network: "Preview",
      }),
    ).toThrow(/id mismatch/u);

    const { da: _da, ...missingDa } = manifest;
    expect(() => verifyDeploymentManifestV1Identity(missingDa)).toThrow(
      /value\.da is required/u,
    );
    expect(() =>
      verifyDeploymentManifestV1Identity({
        ...manifest,
        historicalVersion: 9,
      }),
    ).toThrow(/value\.historicalVersion is unexpected/u);
  });
});
