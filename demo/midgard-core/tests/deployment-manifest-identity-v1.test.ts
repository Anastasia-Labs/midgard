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
  DEPLOYMENT_MANIFEST_V1_CONTRACT_NAMES,
  type DeploymentManifestV1FraudProofCatalogueIdentity,
  makeDeploymentMarkerV1,
  MIDGARD_DEPLOYMENT_MARKER_V1_SCHEMA_VERSION,
  normalizeDeploymentManifestV1JsonValue,
  parseDeploymentMarkerV1,
  verifyDeploymentManifestV1FraudProofCatalogueIdentity,
  verifyDeploymentManifestV1Identity,
} from "../src/deployment-manifest-identity-v1.js";

const CATALOGUE_SCRIPT_HASH =
  "bddf4b5c833decbf82201931cffc54f7c7dc51e4e6743a25a95aa2c0";

const catalogueFixture =
  (): DeploymentManifestV1FraudProofCatalogueIdentity => ({
    root: "774e736e182076083ce8877862af62e180bd6172c516b59d13b63b521c13da90",
    categories: {
      doubleSpend: {
        categoryId: "00000000",
        scriptHash: CATALOGUE_SCRIPT_HASH,
        membershipProofCbor:
          "9fd8799f005f58406fa3aa4d13788081359c25cc0ba03c3077ced23f838de4beaf0b76dcb45c7e5754d72578de167a69d537ef3491e6ed444adc3df26db7f28d94d53007e351757f58400eb923b0cbd24df54401d998531feead35a47a99f4deed205de4af81120f97610000000000000000000000000000000000000000000000000000000000000000ffffd87b9f0058202dc3142460b6ec3760730ad389b82dd8fc365bcb606458d640c813a4fa4543d45820a7be719f5f57828776b40db4e2476aeb846750aa270d80c950d836d48f337850ffff",
      },
      nonExistentInput: {
        categoryId: "00000001",
        scriptHash: CATALOGUE_SCRIPT_HASH,
        membershipProofCbor:
          "9fd8799f005f5840d039221220fdb68d23a3906e113c7763f15cf545c7a23bb0d1148533a5066e9856827e97f383699af00c904f3847e46e0eb329b2f0946b5062376cbbad1a601958400eb923b0cbd24df54401d998531feead35a47a99f4deed205de4af81120f97610000000000000000000000000000000000000000000000000000000000000000ffffd87b9f005820ee008edbcebc51812e09a76640db05559631cf9730f2fddd093fc72d072d323b5820a7be719f5f57828776b40db4e2476aeb846750aa270d80c950d836d48f337850ffff",
      },
      nonExistentInputNoIndex: {
        categoryId: "00000002",
        scriptHash: CATALOGUE_SCRIPT_HASH,
        membershipProofCbor:
          "9fd8799f005f5840d039221220fdb68d23a3906e113c7763f15cf545c7a23bb0d1148533a5066e986f6e7565843b178ca5455479111ae382832684b6964410b11ec083f070a8e7155840268dc64c1b55751769c2bc8df6dae3284bc32b4ee88c6e260c77d2684b258bcf0000000000000000000000000000000000000000000000000000000000000000ffffd87b9f0058208df5e96b9134dbf02ce0619afa67c94a9d09abbe241505f82935aef5d3dc39cc5820a7be719f5f57828776b40db4e2476aeb846750aa270d80c950d836d48f337850ffff",
      },
      invalidRange: {
        categoryId: "00000003",
        scriptHash: CATALOGUE_SCRIPT_HASH,
        membershipProofCbor:
          "9fd8799f005f5840d039221220fdb68d23a3906e113c7763f15cf545c7a23bb0d1148533a5066e9856827e97f383699af00c904f3847e46e0eb329b2f0946b5062376cbbad1a601958400eb923b0cbd24df54401d998531feead35a47a99f4deed205de4af81120f97610000000000000000000000000000000000000000000000000000000000000000ffffd87b9f005820ef9660efe206d50189739680bd866387c07a061a7b5de18f85b6af241f1686385820a7be719f5f57828776b40db4e2476aeb846750aa270d80c950d836d48f337850ffff",
      },
      transitionTrace: {
        categoryId: "00000004",
        scriptHash: CATALOGUE_SCRIPT_HASH,
        membershipProofCbor:
          "9fd8799f005f5840d039221220fdb68d23a3906e113c7763f15cf545c7a23bb0d1148533a5066e986f6e7565843b178ca5455479111ae382832684b6964410b11ec083f070a8e7155840268dc64c1b55751769c2bc8df6dae3284bc32b4ee88c6e260c77d2684b258bcf0000000000000000000000000000000000000000000000000000000000000000ffffd87b9f0058208bb58512fc6c43f79b51020d086948648b960d5ee65bb6a9a737e2af545177265820a7be719f5f57828776b40db4e2476aeb846750aa270d80c950d836d48f337850ffff",
      },
      zeroInput: {
        categoryId: "00000005",
        scriptHash: CATALOGUE_SCRIPT_HASH,
        membershipProofCbor:
          "9fd8799f005f58406fa3aa4d13788081359c25cc0ba03c3077ced23f838de4beaf0b76dcb45c7e578476d066f488ae1a81a182c62b717083e50cdc523255e84416a17797ead539ec58400eb923b0cbd24df54401d998531feead35a47a99f4deed205de4af81120f97610000000000000000000000000000000000000000000000000000000000000000ffffff",
      },
      validationTraceDispute: {
        categoryId: "00000006",
        scriptHash: CATALOGUE_SCRIPT_HASH,
        membershipProofCbor:
          "9fd8799f005f5840d039221220fdb68d23a3906e113c7763f15cf545c7a23bb0d1148533a5066e986f6e7565843b178ca5455479111ae382832684b6964410b11ec083f070a8e7155840d45eddb1c64a9ffe78bfe2e9f34c30ede448d90138915a74c4c7110dc7fed8420000000000000000000000000000000000000000000000000000000000000000ffffff",
      },
      daHashPreimage: {
        categoryId: "00000007",
        scriptHash: CATALOGUE_SCRIPT_HASH,
        membershipProofCbor:
          "9fd8799f005f58406fa3aa4d13788081359c25cc0ba03c3077ced23f838de4beaf0b76dcb45c7e5754d72578de167a69d537ef3491e6ed444adc3df26db7f28d94d53007e351757f58400eb923b0cbd24df54401d998531feead35a47a99f4deed205de4af81120f97610000000000000000000000000000000000000000000000000000000000000000ffffd87b9f00582023f14560688b1f176c498bd359a2cd02d67082f59614f39ef984cfb47fe8699d5820a7be719f5f57828776b40db4e2476aeb846750aa270d80c950d836d48f337850ffff",
      },
    },
  });

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
  it("includes the zero-input fraud-proof validator in the canonical registry", () => {
    expect(DEPLOYMENT_MANIFEST_V1_CONTRACT_NAMES).toContain(
      "fraudProofZeroInput",
    );
  });

  it("authenticates the exact seven-entry fraud-proof catalogue root and proofs", () => {
    const catalogue = catalogueFixture();
    expect(
      verifyDeploymentManifestV1FraudProofCatalogueIdentity(catalogue),
    ).toEqual(catalogue);
  });

  it("rejects catalogue root, position, value, proof, and category-set tampering", () => {
    const catalogue = catalogueFixture();

    expect(() =>
      verifyDeploymentManifestV1FraudProofCatalogueIdentity({
        ...catalogue,
        root: "ff".repeat(32),
      }),
    ).toThrow(/catalogue root mismatch/u);

    expect(() =>
      verifyDeploymentManifestV1FraudProofCatalogueIdentity({
        ...catalogue,
        categories: {
          ...catalogue.categories,
          nonExistentInputNoIndex: {
            ...catalogue.categories.nonExistentInputNoIndex,
            categoryId: "00000003",
          },
        },
      }),
    ).toThrow(/nonExistentInputNoIndex\.categoryId must be 00000002/u);

    expect(() =>
      verifyDeploymentManifestV1FraudProofCatalogueIdentity({
        ...catalogue,
        categories: {
          ...catalogue.categories,
          zeroInput: {
            ...catalogue.categories.zeroInput,
            scriptHash: "aa".repeat(28),
          },
        },
      }),
    ).toThrow(/catalogue root mismatch/u);

    expect(() =>
      verifyDeploymentManifestV1FraudProofCatalogueIdentity({
        ...catalogue,
        categories: {
          ...catalogue.categories,
          invalidRange: {
            ...catalogue.categories.invalidRange,
            membershipProofCbor:
              catalogue.categories.doubleSpend.membershipProofCbor,
          },
        },
      }),
    ).toThrow(/invalidRange\.membershipProofCbor does not prove membership/u);

    const { validationTraceDispute: _missing, ...missingCategory } =
      catalogue.categories;
    expect(() =>
      verifyDeploymentManifestV1FraudProofCatalogueIdentity({
        ...catalogue,
        categories:
          missingCategory as DeploymentManifestV1FraudProofCatalogueIdentity["categories"],
      }),
    ).toThrow(/validationTraceDispute is required/u);

    expect(() =>
      verifyDeploymentManifestV1FraudProofCatalogueIdentity({
        ...catalogue,
        categories: {
          ...catalogue.categories,
          historicalCategory: catalogue.categories.doubleSpend,
        } as DeploymentManifestV1FraudProofCatalogueIdentity["categories"],
      }),
    ).toThrow(/historicalCategory is unexpected/u);
  });

  it("rejects malformed categories at the exported catalogue boundary", () => {
    const catalogue = catalogueFixture();
    const { membershipProofCbor: _proof, ...missingProof } =
      catalogue.categories.doubleSpend;
    expect(() =>
      verifyDeploymentManifestV1FraudProofCatalogueIdentity({
        ...catalogue,
        categories: {
          ...catalogue.categories,
          doubleSpend:
            missingProof as DeploymentManifestV1FraudProofCatalogueIdentity["categories"]["doubleSpend"],
        },
      }),
    ).toThrow(/doubleSpend\.membershipProofCbor is required/u);

    expect(() =>
      verifyDeploymentManifestV1FraudProofCatalogueIdentity({
        ...catalogue,
        categories: {
          ...catalogue.categories,
          doubleSpend: {
            ...catalogue.categories.doubleSpend,
            scriptHash: "AA".repeat(28),
          },
        },
      }),
    ).toThrow(/doubleSpend\.scriptHash must be lowercase canonical hex/u);

    expect(() =>
      verifyDeploymentManifestV1FraudProofCatalogueIdentity({
        ...catalogue,
        categories: {
          ...catalogue.categories,
          doubleSpend: {
            ...catalogue.categories.doubleSpend,
            membershipProofCbor: "f",
          },
        },
      }),
    ).toThrow(
      /doubleSpend\.membershipProofCbor must be lowercase canonical hex/u,
    );
  });

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
    // Rebound 2026-08-01: the identity input embeds
    // MIDGARD_CONSENSUS_PROFILE_V1, whose committed constants changed in
    // 4a4bc660 (five-stage envelope measurement block); the old pin
    // predated that commit.
    expect(manifest.manifestId).toBe(
      "f081db0f2852f2fc04136919d4e83b10e7d5066e59f88759187eed54bc87bc9e",
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
    expect(assertDeploymentMarkerV1Matches(marker, marker, "Postgres")).toEqual(
      marker,
    );
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
