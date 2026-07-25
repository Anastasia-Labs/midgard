import { describe, expect, it } from "vitest";

import {
  MIDGARD_CONSENSUS_PROFILE_V1,
  MIDGARD_CONSENSUS_PROFILE_V1_DIGEST,
  MIDGARD_DEPLOYMENT_MANIFEST_V1_SCHEMA_VERSION,
} from "../src/consensus-profile-v1.js";
import {
  computeDeploymentManifestV1Id,
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
