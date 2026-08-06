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
  DEPLOYMENT_MANIFEST_V1_FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER,
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
    root: "b5a265573c875c48a999adb3672a77c070a0511e1662e406ec8c7d81b8689134",
    categories: {
      doubleSpend: {
        categoryId: "00000000",
        scriptHash: CATALOGUE_SCRIPT_HASH,
        membershipProofCbor:
          "9fd8799f005f58406c68a2a4f4f27ea8d142298bd80d981c5f1d3b59b9a0350daebcb33d9208c28ddcfcfab97aaa44dc41df27d64e820305294530e4fe9f91d8123b6a25ea2219bb58400eb923b0cbd24df54401d998531feead35a47a99f4deed205de4af81120f97610000000000000000000000000000000000000000000000000000000000000000ffffd87b9f0058202dc3142460b6ec3760730ad389b82dd8fc365bcb606458d640c813a4fa4543d45820a7be719f5f57828776b40db4e2476aeb846750aa270d80c950d836d48f337850ffff",
      },
      nonExistentInput: {
        categoryId: "00000001",
        scriptHash: CATALOGUE_SCRIPT_HASH,
        membershipProofCbor:
          "9fd8799f005f5840a6a7e1bb1258694956a6ba73b622f9b8d71c5f050195154c1f1618c99118519d25eed1d22dc811002d5ba3054e53d7e3f413e4b2881894176b3fe780e9e1e5a85840c04222813484c756a270f64bae84fd4a45e6057ef56152c80f0d8bc654d2bcfb0000000000000000000000000000000000000000000000000000000000000000ffffd87b9f005820ee008edbcebc51812e09a76640db05559631cf9730f2fddd093fc72d072d323b5820a7be719f5f57828776b40db4e2476aeb846750aa270d80c950d836d48f337850ffff",
      },
      nonExistentInputNoIndex: {
        categoryId: "00000002",
        scriptHash: CATALOGUE_SCRIPT_HASH,
        membershipProofCbor:
          "9fd8799f005f5840a6a7e1bb1258694956a6ba73b622f9b8d71c5f050195154c1f1618c99118519d1ae7640097a0021bd299b4eec6122fbff160e7ce080385c39ff0637d0928a5df5840ad77f6ea9287f8d6db4368967b65bb0817db64d29f1d75d122d7d7395f9ba1a60000000000000000000000000000000000000000000000000000000000000000ffffd87b9f0058208df5e96b9134dbf02ce0619afa67c94a9d09abbe241505f82935aef5d3dc39cc5820a7be719f5f57828776b40db4e2476aeb846750aa270d80c950d836d48f337850ffff",
      },
      invalidRange: {
        categoryId: "00000003",
        scriptHash: CATALOGUE_SCRIPT_HASH,
        membershipProofCbor:
          "9fd8799f005f5840a6a7e1bb1258694956a6ba73b622f9b8d71c5f050195154c1f1618c99118519d25eed1d22dc811002d5ba3054e53d7e3f413e4b2881894176b3fe780e9e1e5a85840c04222813484c756a270f64bae84fd4a45e6057ef56152c80f0d8bc654d2bcfb0000000000000000000000000000000000000000000000000000000000000000ffffd87b9f005820ef9660efe206d50189739680bd866387c07a061a7b5de18f85b6af241f1686385820a7be719f5f57828776b40db4e2476aeb846750aa270d80c950d836d48f337850ffff",
      },
      transitionTrace: {
        categoryId: "00000004",
        scriptHash: CATALOGUE_SCRIPT_HASH,
        membershipProofCbor:
          "9fd8799f005f5840a6a7e1bb1258694956a6ba73b622f9b8d71c5f050195154c1f1618c99118519d1ae7640097a0021bd299b4eec6122fbff160e7ce080385c39ff0637d0928a5df5840ad77f6ea9287f8d6db4368967b65bb0817db64d29f1d75d122d7d7395f9ba1a60000000000000000000000000000000000000000000000000000000000000000ffffd87b9f0058208bb58512fc6c43f79b51020d086948648b960d5ee65bb6a9a737e2af545177265820a7be719f5f57828776b40db4e2476aeb846750aa270d80c950d836d48f337850ffff",
      },
      zeroInput: {
        categoryId: "00000005",
        scriptHash: CATALOGUE_SCRIPT_HASH,
        membershipProofCbor:
          "9fd8799f005f58406c68a2a4f4f27ea8d142298bd80d981c5f1d3b59b9a0350daebcb33d9208c28d8476d066f488ae1a81a182c62b717083e50cdc523255e84416a17797ead539ec58400eb923b0cbd24df54401d998531feead35a47a99f4deed205de4af81120f9761289ceb19f2805a0a26806adfd3ea28927aa9f9548145b4d92f52c8ba31e64b7affffff",
      },
      validationTraceDispute: {
        categoryId: "00000006",
        scriptHash: CATALOGUE_SCRIPT_HASH,
        membershipProofCbor:
          "9fd8799f005f5840a6a7e1bb1258694956a6ba73b622f9b8d71c5f050195154c1f1618c99118519d1ae7640097a0021bd299b4eec6122fbff160e7ce080385c39ff0637d0928a5df5840d45eddb1c64a9ffe78bfe2e9f34c30ede448d90138915a74c4c7110dc7fed8420000000000000000000000000000000000000000000000000000000000000000ffffd87b9f005820b9df94ec34957f7cb56fdf5b9bb762c2356174d37bc18884d07f7912e1e911135820a7be719f5f57828776b40db4e2476aeb846750aa270d80c950d836d48f337850ffff",
      },
      daHashPreimage: {
        categoryId: "00000007",
        scriptHash: CATALOGUE_SCRIPT_HASH,
        membershipProofCbor:
          "9fd8799f005f58406c68a2a4f4f27ea8d142298bd80d981c5f1d3b59b9a0350daebcb33d9208c28ddcfcfab97aaa44dc41df27d64e820305294530e4fe9f91d8123b6a25ea2219bb58400eb923b0cbd24df54401d998531feead35a47a99f4deed205de4af81120f97610000000000000000000000000000000000000000000000000000000000000000ffffd87b9f00582023f14560688b1f176c498bd359a2cd02d67082f59614f39ef984cfb47fe8699d5820a7be719f5f57828776b40db4e2476aeb846750aa270d80c950d836d48f337850ffff",
      },
      noReferenceInput: {
        categoryId: "00000008",
        scriptHash: CATALOGUE_SCRIPT_HASH,
        membershipProofCbor:
          "9fd8799f005f5840a6a7e1bb1258694956a6ba73b622f9b8d71c5f050195154c1f1618c99118519d1ae7640097a0021bd299b4eec6122fbff160e7ce080385c39ff0637d0928a5df5840d45eddb1c64a9ffe78bfe2e9f34c30ede448d90138915a74c4c7110dc7fed8420000000000000000000000000000000000000000000000000000000000000000ffffd87b9f005820ba0ca986830632d31981be61644386b7d1206a0a3b806368b80c3449633672045820a7be719f5f57828776b40db4e2476aeb846750aa270d80c950d836d48f337850ffff",
      },
      referenceInputNoIdx: {
        categoryId: "00000009",
        scriptHash: CATALOGUE_SCRIPT_HASH,
        membershipProofCbor:
          "9fd8799f005f58406c68a2a4f4f27ea8d142298bd80d981c5f1d3b59b9a0350daebcb33d9208c28d8476d066f488ae1a81a182c62b717083e50cdc523255e84416a17797ead539ec58400eb923b0cbd24df54401d998531feead35a47a99f4deed205de4af81120f97618477b3a47a28a0da0c8f592df1055d6afae1e64fe456b3030062d47af8b1ffe9ffffff",
      },
      invalidSignature: {
        categoryId: "0000000a",
        scriptHash: CATALOGUE_SCRIPT_HASH,
        membershipProofCbor:
          "9fd8799f005f5840a6a7e1bb1258694956a6ba73b622f9b8d71c5f050195154c1f1618c99118519d25eed1d22dc811002d5ba3054e53d7e3f413e4b2881894176b3fe780e9e1e5a85840cb4cbd809e8abb415df606bb52334d9bd8d5eb6ed33c8256a6b5f186fd80bf350000000000000000000000000000000000000000000000000000000000000000ffffff",
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
  it("includes every registered fraud-proof validator in the canonical registry", () => {
    expect(DEPLOYMENT_MANIFEST_V1_CONTRACT_NAMES).toContain(
      "fraudProofZeroInput",
    );
    // #547 appended the Q18/Q31/Q15 first-step validators. The registry is
    // append-only, so each must be present and the catalogue order must name
    // exactly the same set of categories in the same positions.
    expect(DEPLOYMENT_MANIFEST_V1_CONTRACT_NAMES).toContain(
      "fraudProofNoReferenceInput",
    );
    expect(DEPLOYMENT_MANIFEST_V1_CONTRACT_NAMES).toContain(
      "fraudProofReferenceInputNoIdx",
    );
    expect(DEPLOYMENT_MANIFEST_V1_CONTRACT_NAMES).toContain(
      "fraudProofInvalidSignature",
    );
    expect(
      DEPLOYMENT_MANIFEST_V1_FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER.slice(-3),
    ).toEqual(["noReferenceInput", "referenceInputNoIdx", "invalidSignature"]);
  });

  it("authenticates the exact eleven-entry fraud-proof catalogue root and proofs", () => {
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
