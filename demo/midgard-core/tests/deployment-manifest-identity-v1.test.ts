import { describe, expect, it } from "vitest";

import {
  MIDGARD_CONSENSUS_PROFILE_V1,
  MIDGARD_CONSENSUS_PROFILE_V1_DIGEST,
  MIDGARD_DEPLOYMENT_MANIFEST_V1_SCHEMA_VERSION,
} from "../src/consensus-profile-v1.js";
import {
  assertDeploymentMarkerV1Matches,
  computeDeploymentManifestV1Id,
  computeDeploymentManifestV1JsonDigest,
  makeDeploymentMarkerV1,
  MIDGARD_DEPLOYMENT_MARKER_V1_SCHEMA_VERSION,
  normalizeDeploymentManifestV1JsonValue,
  parseDeploymentMarkerV1,
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
    expect(manifest.manifestId).toBe(
      "55247a9e51dc3588485ec8d431fd67e725f55d79bbc2af055b4b444b2d2ed8a5",
    );
    expect(verifyDeploymentManifestV1Identity(manifest)).toEqual(manifest);
  });

  it("owns the sole exact DeploymentMarkerV1 boundary", () => {
    const manifestId = computeDeploymentManifestV1Id(identityInput());
    const marker = makeDeploymentMarkerV1(manifestId);
    expect(marker).toEqual({
      schemaVersion: MIDGARD_DEPLOYMENT_MARKER_V1_SCHEMA_VERSION,
      manifestId,
    });
    expect(parseDeploymentMarkerV1(marker)).toEqual(marker);
    expect(
      assertDeploymentMarkerV1Matches(marker, marker, "Postgres"),
    ).toEqual(marker);
    expect(() =>
      parseDeploymentMarkerV1({ ...marker, historicalVersion: 9 }),
    ).toThrow(/exactly schemaVersion and manifestId/u);
    expect(() =>
      parseDeploymentMarkerV1({ manifestId: marker.manifestId }),
    ).toThrow(/exactly schemaVersion and manifestId/u);
    expect(() =>
      assertDeploymentMarkerV1Matches(
        marker,
        makeDeploymentMarkerV1("ff".repeat(32)),
        "DA store",
      ),
    ).toThrow(
      `DA store deployment marker mismatch: expected ${marker.manifestId}, found ${"ff".repeat(32)}`,
    );
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
