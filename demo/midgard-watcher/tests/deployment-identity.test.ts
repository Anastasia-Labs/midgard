import {
  createHash,
  generateKeyPairSync,
  type KeyObject,
  sign,
} from "node:crypto";

import { describe, expect, it } from "vitest";

import {
  MIDGARD_CONSENSUS_PROFILE_V1,
  MIDGARD_CONSENSUS_PROFILE_V1_DIGEST,
} from "../../midgard-core/src/consensus-profile-v1.js";
import {
  DA_RUNTIME_MANIFEST_V1_SCHEMA_VERSION,
  DA_TRANSPORT_LIMITS_V1,
  DA_TRANSPORT_V1_PROTOCOL_VERSION,
} from "../../midgard-core/src/da-transport.js";
import {
  computeDeploymentManifestV1Id,
  computeDeploymentManifestV1JsonDigest,
  DEPLOYMENT_MANIFEST_V1_CONTRACT_NAMES,
  DEPLOYMENT_MANIFEST_V1_REFERENCE_SCRIPT_CONTRACT_BY_ROLE,
  DEPLOYMENT_MANIFEST_V1_REFERENCE_SCRIPT_TOKEN_NAMES,
  DEPLOYMENT_MANIFEST_V1_STEP_NAMES,
  makeDeploymentMarkerV1,
} from "../../midgard-core/src/deployment-manifest-identity-v1.js";
import {
  makeWatcherDeploymentIdentitySignaturePayloadV1,
  verifyWatcherDeploymentIdentityV1,
  WATCHER_DEPLOYMENT_RELEASE_BINDINGS_V1_SCHEMA_VERSION,
  WATCHER_SIGNED_DEPLOYMENT_IDENTITY_V1_SCHEMA_VERSION,
  watcherDeploymentIdentityDiagnostic,
  WatcherDeploymentIdentityError,
  type WatcherDeploymentIdentityPolicyV1,
} from "../src/deployment-identity.js";
import { canonicalFraudProofCatalogueFixture } from "./canonical-fraud-proof-catalogue.js";

const NATIVE_SCRIPT_CBOR = `8200581c${"00".repeat(28)}`;
const NATIVE_SCRIPT_HASH =
  "9dcfe5a661b6bc3af0999d06416d95842ba7c693dc0e246f5e0a5e33";
const CONTRACT_SCRIPT_CBOR = "01";
const CONTRACT_SCRIPT_HASH =
  "bddf4b5c833decbf82201931cffc54f7c7dc51e4e6743a25a95aa2c0";
const DA_VKEY = "44".repeat(32);
const DA_SIGNERS_HASH =
  "0395256ce5d90f07504b614b9e70e29a06fdd69cef6b01f6018615164125a5c5";
const RELEASE_DIGEST = "66".repeat(32);
const BLUEPRINT_HASH = "55".repeat(32);
const RULE_BUNDLE_COMMITMENT = "77".repeat(32);

type MutableRecord = Record<string, any>;

const referenceOutRefByContract = new Map<
  string,
  { txHash: string; outputIndex: number }
>(
  Object.values(DEPLOYMENT_MANIFEST_V1_REFERENCE_SCRIPT_CONTRACT_BY_ROLE).map(
    (contractName, outputIndex) => [
      contractName,
      { txHash: "22".repeat(32), outputIndex },
    ],
  ),
);

const canonicalIdentity = (): MutableRecord => {
  const contracts = Object.fromEntries(
    DEPLOYMENT_MANIFEST_V1_CONTRACT_NAMES.map((contractName) => [
      contractName,
      {
        refScriptUTxO: referenceOutRefByContract.get(contractName) ?? null,
        contract: {
          type:
            contractName === "referenceScriptAuthMint" ? "Native" : "PlutusV3",
          cborHex:
            contractName === "referenceScriptAuthMint"
              ? NATIVE_SCRIPT_CBOR
              : CONTRACT_SCRIPT_CBOR,
        },
        scriptHash:
          contractName === "referenceScriptAuthMint"
            ? NATIVE_SCRIPT_HASH
            : CONTRACT_SCRIPT_HASH,
      },
    ]),
  ) as MutableRecord;
  contracts.fraudProofCatalogueMint.fraudProofCatalogue =
    canonicalFraudProofCatalogueFixture(contracts);
  const referenceScripts = Object.fromEntries(
    Object.entries(
      DEPLOYMENT_MANIFEST_V1_REFERENCE_SCRIPT_CONTRACT_BY_ROLE,
    ).map(([role, contractName]) => {
      const outRef = referenceOutRefByContract.get(contractName);
      if (outRef === undefined) {
        throw new Error("Missing test reference outref");
      }
      const tokenName =
        DEPLOYMENT_MANIFEST_V1_REFERENCE_SCRIPT_TOKEN_NAMES[
          role as keyof typeof DEPLOYMENT_MANIFEST_V1_REFERENCE_SCRIPT_TOKEN_NAMES
        ];
      return [
        role,
        {
          status: "confirmed",
          roleUnit:
            NATIVE_SCRIPT_HASH + Buffer.from(tokenName, "utf8").toString("hex"),
          scriptHash: contracts[contractName].scriptHash,
          outRef: `${outRef.txHash}#${outRef.outputIndex.toString()}`,
        },
      ];
    }),
  );
  const parameters = {
    maxTxSize: 16_384,
    maxValueSize: 5_000,
    maxTxExUnits: { memory: "16500000", steps: "10000000000" },
  };
  return {
    schemaVersion: "midgard-deployment-manifest-v1",
    consensusProfile: MIDGARD_CONSENSUS_PROFILE_V1,
    consensusProfileDigest: MIDGARD_CONSENSUS_PROFILE_V1_DIGEST,
    network: "Preprod",
    cardanoProtocolParameters: {
      snapshot: parameters,
      digest: computeDeploymentManifestV1JsonDigest(parameters),
    },
    genesis: {
      headerHash: "00".repeat(28),
      utxoSetDigest: computeDeploymentManifestV1JsonDigest([]),
    },
    createdAt: "2026-07-28T00:00:00.000Z",
    updatedAt: "2026-07-28T00:00:00.000Z",
    referenceScriptDeployAddress: "addr_test1vcanonical",
    hubOracleOneShot: {
      txHash: "11".repeat(32),
      outputIndex: 0,
      outRef: `${"11".repeat(32)}#0`,
      status: "consumed_by_init",
    },
    referenceScriptAuthPolicy: {
      policyId: NATIVE_SCRIPT_HASH,
      nativeScript: {
        type: "Native",
        cborHex: NATIVE_SCRIPT_CBOR,
        expiresAtSlot: 1,
        expiresAtUnixTime: 1,
        timelockDurationMs: 1,
      },
      tokenNames: DEPLOYMENT_MANIFEST_V1_REFERENCE_SCRIPT_TOKEN_NAMES,
      postTimelockAudit: {
        required: true,
        rule: "No authenticated reference-script output may change.",
      },
    },
    contracts,
    referenceScripts,
    da: {
      committeeVkeys: [DA_VKEY],
      committeeSignersHash: DA_SIGNERS_HASH,
      threshold: 1,
      transportProfile: {
        protocolVersion: DA_TRANSPORT_V1_PROTOCOL_VERSION,
        runtimeManifestSchemaVersion: DA_RUNTIME_MANIFEST_V1_SCHEMA_VERSION,
        envelopeEncoding: "identity",
        zstdLevel: 3,
        limits: DA_TRANSPORT_LIMITS_V1,
        retentionDays: DA_TRANSPORT_LIMITS_V1.minimumRetentionDays,
      },
    },
    proofEvidence: {
      digest: RELEASE_DIGEST,
      blueprintHash: BLUEPRINT_HASH,
    },
    steps: Object.fromEntries(
      DEPLOYMENT_MANIFEST_V1_STEP_NAMES.map((stepName) => [
        stepName,
        {
          status:
            stepName === "prepareHubOracleNonce" ||
            stepName === "deployNodeRuntimeReferenceScripts" ||
            stepName === "initProtocol"
              ? "complete"
              : "pending",
        },
      ]),
    ),
    validationDispute: {
      version: MIDGARD_CONSENSUS_PROFILE_V1.validationDisputeVersion,
      responseWindowMs:
        MIDGARD_CONSENSUS_PROFILE_V1.limits.validationDisputeResponseWindowMs,
      maxBisectionRounds:
        MIDGARD_CONSENSUS_PROFILE_V1.limits.maxValidationBisectionRounds,
      maturityMs: MIDGARD_CONSENSUS_PROFILE_V1.limits.blockMaturityMs,
    },
  };
};

const withManifestId = (identity: MutableRecord): MutableRecord => ({
  ...identity,
  manifestId: computeDeploymentManifestV1Id(identity),
});

const makeTrustRoot = () => {
  const { privateKey, publicKey } = generateKeyPairSync("ed25519");
  const publicKeySpkiDer = publicKey.export({
    format: "der",
    type: "spki",
  });
  const publicKeySpkiDerHex = publicKeySpkiDer.toString("hex");
  const trustRootId = createHash("sha256")
    .update(publicKeySpkiDer)
    .digest("hex");
  return {
    privateKey,
    trustRoot: { trustRootId, publicKeySpkiDerHex },
  };
};

const appliedScriptHashes = (manifest: MutableRecord): Record<string, string> =>
  Object.fromEntries(
    DEPLOYMENT_MANIFEST_V1_CONTRACT_NAMES.map((contractName) => [
      contractName,
      manifest.contracts[contractName].scriptHash,
    ]),
  );

const referenceScriptPolicy = (
  manifest: MutableRecord,
): Record<string, { scriptHash: string; outRef: string }> =>
  Object.fromEntries(
    Object.keys(DEPLOYMENT_MANIFEST_V1_REFERENCE_SCRIPT_CONTRACT_BY_ROLE).map(
      (role) => [
        role,
        {
          scriptHash: manifest.referenceScripts[role].scriptHash,
          outRef: manifest.referenceScripts[role].outRef,
        },
      ],
    ),
  );

const cataloguePolicy = (manifest: MutableRecord) => {
  const catalogue =
    manifest.contracts.fraudProofCatalogueMint.fraudProofCatalogue;
  return {
    root: catalogue.root as string,
    categories: Object.fromEntries(
      Object.entries(catalogue.categories as MutableRecord).map(
        ([category, value]) => [
          category,
          {
            categoryId: value.categoryId as string,
            scriptHash: value.scriptHash as string,
          },
        ],
      ),
    ),
  } as WatcherDeploymentIdentityPolicyV1["fraudProofCatalogue"];
};

const makeFixture = () => {
  const manifest = withManifestId(canonicalIdentity());
  const programCommitments = {
    "validation-machine-v1": "88".repeat(32),
    "transition-order-v1": "99".repeat(32),
  };
  const releaseBindings = {
    schemaVersion: WATCHER_DEPLOYMENT_RELEASE_BINDINGS_V1_SCHEMA_VERSION,
    ruleBundleCommitment: RULE_BUNDLE_COMMITMENT,
    programCommitments,
    da: {
      mode: "authenticated_committee_v1",
      identityDigest: computeDeploymentManifestV1JsonDigest(manifest.da),
    },
    releaseEvidence: {
      digest: RELEASE_DIGEST,
      blueprintHash: BLUEPRINT_HASH,
    },
  };
  const { privateKey, trustRoot } = makeTrustRoot();
  const signedIdentity: MutableRecord = {
    schemaVersion: WATCHER_SIGNED_DEPLOYMENT_IDENTITY_V1_SCHEMA_VERSION,
    manifest,
    releaseBindings,
    attestation: {
      algorithm: "ed25519",
      trustRootId: trustRoot.trustRootId,
      signature: "",
    },
  };
  const policy: WatcherDeploymentIdentityPolicyV1 = {
    network: "Preprod",
    hubOracleOneShotOutRef: manifest.hubOracleOneShot.outRef,
    appliedScriptHashes: appliedScriptHashes(manifest),
    referenceScripts: referenceScriptPolicy(manifest),
    fraudProofCatalogue: cataloguePolicy(manifest),
    ruleBundleCommitment: RULE_BUNDLE_COMMITMENT,
    programCommitments,
    daMode: "authenticated_committee_v1",
    daIdentityDigest: releaseBindings.da.identityDigest,
    releaseEvidenceDigest: RELEASE_DIGEST,
    blueprintHash: BLUEPRINT_HASH,
  };
  const resign = (
    identity: MutableRecord = signedIdentity,
    signingKey: KeyObject = privateKey,
  ): void => {
    identity.attestation.signature = sign(
      null,
      makeWatcherDeploymentIdentitySignaturePayloadV1(
        identity.manifest.manifestId,
        identity.releaseBindings,
      ),
      signingKey,
    ).toString("hex");
  };
  resign();
  return {
    signedIdentity,
    policy,
    trustRoot,
    privateKey,
    resign,
    durableMarker: makeDeploymentMarkerV1(manifest.manifestId),
  };
};

const rejection = (
  action: () => unknown,
  code: WatcherDeploymentIdentityError["code"],
  path: string | RegExp,
): WatcherDeploymentIdentityError => {
  try {
    action();
  } catch (error) {
    expect(error).toBeInstanceOf(WatcherDeploymentIdentityError);
    const deploymentError = error as WatcherDeploymentIdentityError;
    expect(deploymentError.code).toBe(code);
    if (typeof path === "string") {
      expect(deploymentError.path).toBe(path);
    } else {
      expect(deploymentError.path).toMatch(path);
    }
    return deploymentError;
  }
  throw new Error("Expected watcher deployment identity rejection");
};

describe("watcher deployment identity", () => {
  it("verifies the exact signed release identity and durable marker", () => {
    const fixture = makeFixture();

    expect(
      verifyWatcherDeploymentIdentityV1({
        signedIdentity: fixture.signedIdentity,
        policy: fixture.policy,
        trustRoots: [fixture.trustRoot],
        durableMarker: fixture.durableMarker,
      }),
    ).toEqual({
      manifestId: fixture.signedIdentity.manifest.manifestId,
      network: "Preprod",
      trustRootId: fixture.trustRoot.trustRootId,
      releaseEvidenceDigest: RELEASE_DIGEST,
      ruleBundleCommitment: RULE_BUNDLE_COMMITMENT,
      programCommitments:
        fixture.signedIdentity.releaseBindings.programCommitments,
      durableMarker: fixture.durableMarker,
    });
  });

  it("binds zero-input and validation-trace catalogue entries to their deployed contracts", () => {
    const fixture = makeFixture();
    const categories = fixture.policy.fraudProofCatalogue.categories;

    expect(categories.zeroInput).toEqual({
      categoryId: "00000005",
      scriptHash: fixture.policy.appliedScriptHashes.fraudProofZeroInput,
    });
    expect(categories.validationTraceDispute).toEqual({
      categoryId: "00000006",
      scriptHash: fixture.policy.appliedScriptHashes.validationTraceDispute,
    });

    fixture.policy = {
      ...fixture.policy,
      fraudProofCatalogue: {
        ...fixture.policy.fraudProofCatalogue,
        categories: {
          ...categories,
          zeroInput: {
            ...categories.zeroInput,
            scriptHash: NATIVE_SCRIPT_HASH,
          },
        },
      },
    };
    rejection(
      () =>
        verifyWatcherDeploymentIdentityV1({
          signedIdentity: fixture.signedIdentity,
          policy: fixture.policy,
          trustRoots: [fixture.trustRoot],
          durableMarker: fixture.durableMarker,
        }),
      "mismatched_identity",
      "$.manifest.contracts.fraudProofCatalogueMint.fraudProofCatalogue.categories.zeroInput",
    );
  });

  it("requires the exact durable deployment marker", () => {
    const fixture = makeFixture();
    const verify = (durableMarker: unknown) =>
      verifyWatcherDeploymentIdentityV1({
        signedIdentity: fixture.signedIdentity,
        policy: fixture.policy,
        trustRoots: [fixture.trustRoot],
        durableMarker,
      });

    rejection(() => verify(null), "missing_durable_marker", "$.durableMarker");
    rejection(
      () => verify(makeDeploymentMarkerV1("aa".repeat(32))),
      "durable_marker_mismatch",
      "$.durableMarker",
    );
  });

  it("rejects an untrusted signer and any signature mutation", () => {
    const fixture = makeFixture();
    const other = makeTrustRoot();
    const verify = () =>
      verifyWatcherDeploymentIdentityV1({
        signedIdentity: fixture.signedIdentity,
        policy: fixture.policy,
        trustRoots: [fixture.trustRoot],
        durableMarker: fixture.durableMarker,
      });

    fixture.signedIdentity.attestation.trustRootId =
      other.trustRoot.trustRootId;
    rejection(verify, "untrusted_signer", "$.attestation.trustRootId");

    fixture.signedIdentity.attestation.trustRootId =
      fixture.trustRoot.trustRootId;
    fixture.signedIdentity.attestation.signature = "00".repeat(64);
    rejection(verify, "invalid_signature", "$.attestation.signature");
  });

  it("rejects unknown, missing, and malformed signed-identity fields", () => {
    const fixture = makeFixture();
    const verify = () =>
      verifyWatcherDeploymentIdentityV1({
        signedIdentity: fixture.signedIdentity,
        policy: fixture.policy,
        trustRoots: [fixture.trustRoot],
        durableMarker: fixture.durableMarker,
      });

    fixture.signedIdentity.historicalVersion = 9;
    rejection(verify, "unknown_field", "$.historicalVersion");
    delete fixture.signedIdentity.historicalVersion;

    delete fixture.signedIdentity.releaseBindings.da;
    rejection(verify, "missing_field", "$.releaseBindings.da");
    fixture.signedIdentity.releaseBindings.da = {
      mode: "operator_private",
      identityDigest: "aa".repeat(32),
    };
    rejection(verify, "invalid_field", "$.releaseBindings.da.mode");
  });

  it.each([
    [
      "canonical feature removal",
      (manifest: MutableRecord) => {
        manifest.consensusProfile.features =
          manifest.consensusProfile.features.slice(1);
      },
    ],
    [
      "applied script-byte drift",
      (manifest: MutableRecord) => {
        manifest.contracts.payoutSpend.contract.cborHex = "02";
      },
    ],
    [
      "nested legacy manifest field",
      (manifest: MutableRecord) => {
        manifest.hubOracleOneShot.legacyNonceVersion = 2;
      },
    ],
  ])("rejects %s at the canonical manifest boundary", (_label, mutate) => {
    const fixture = makeFixture();
    const signedIdentity = structuredClone(fixture.signedIdentity);
    mutate(signedIdentity.manifest);
    const { manifestId: _manifestId, ...identity } = signedIdentity.manifest;
    signedIdentity.manifest.manifestId =
      computeDeploymentManifestV1Id(identity);
    fixture.resign(signedIdentity);

    rejection(
      () =>
        verifyWatcherDeploymentIdentityV1({
          signedIdentity,
          policy: fixture.policy,
          trustRoots: [fixture.trustRoot],
          durableMarker: fixture.durableMarker,
        }),
      "canonical_manifest_invalid",
      "$.manifest",
    );
  });

  it.each([
    [
      "network",
      (fixture: ReturnType<typeof makeFixture>) => {
        fixture.signedIdentity.manifest.network = "Preview";
        const { manifestId: _manifestId, ...identity } =
          fixture.signedIdentity.manifest;
        fixture.signedIdentity.manifest.manifestId =
          computeDeploymentManifestV1Id(identity);
        fixture.resign();
      },
      "$.manifest.network",
    ],
    [
      "one-shot",
      (fixture: ReturnType<typeof makeFixture>) => {
        fixture.policy = {
          ...fixture.policy,
          hubOracleOneShotOutRef: `${"aa".repeat(32)}#0`,
        };
      },
      "$.manifest.hubOracleOneShot",
    ],
    [
      "applied script hash",
      (fixture: ReturnType<typeof makeFixture>) => {
        fixture.policy = {
          ...fixture.policy,
          appliedScriptHashes: {
            ...fixture.policy.appliedScriptHashes,
            payoutSpend: "aa".repeat(28),
          },
        };
      },
      "$.manifest.contracts",
    ],
    [
      "reference script",
      (fixture: ReturnType<typeof makeFixture>) => {
        const role = Object.keys(fixture.policy.referenceScripts)[0];
        fixture.policy = {
          ...fixture.policy,
          referenceScripts: {
            ...fixture.policy.referenceScripts,
            [role]: {
              ...fixture.policy.referenceScripts[role],
              outRef: `${"aa".repeat(32)}#0`,
            },
          },
        };
      },
      /^[$]\.manifest\.referenceScripts\./u,
    ],
    [
      "catalogue",
      (fixture: ReturnType<typeof makeFixture>) => {
        fixture.policy = {
          ...fixture.policy,
          fraudProofCatalogue: {
            ...fixture.policy.fraudProofCatalogue,
            root: "aa".repeat(32),
          },
        };
      },
      "$.manifest.contracts.fraudProofCatalogueMint.fraudProofCatalogue.root",
    ],
    [
      "rule bundle",
      (fixture: ReturnType<typeof makeFixture>) => {
        fixture.policy = {
          ...fixture.policy,
          ruleBundleCommitment: "aa".repeat(32),
        };
      },
      "$.releaseBindings",
    ],
    [
      "program commitments",
      (fixture: ReturnType<typeof makeFixture>) => {
        fixture.policy = {
          ...fixture.policy,
          programCommitments: {
            ...fixture.policy.programCommitments,
            "transition-order-v1": "aa".repeat(32),
          },
        };
      },
      "$.releaseBindings",
    ],
    [
      "DA identity",
      (fixture: ReturnType<typeof makeFixture>) => {
        fixture.policy = {
          ...fixture.policy,
          daIdentityDigest: "aa".repeat(32),
        };
      },
      "$.releaseBindings.da",
    ],
    [
      "release evidence",
      (fixture: ReturnType<typeof makeFixture>) => {
        fixture.policy = {
          ...fixture.policy,
          releaseEvidenceDigest: "aa".repeat(32),
        };
      },
      "$.releaseBindings.releaseEvidence",
    ],
  ])("fails closed on a %s mismatch", (_label, mutate, expectedPath) => {
    const fixture = makeFixture();
    mutate(fixture);

    const error = rejection(
      () =>
        verifyWatcherDeploymentIdentityV1({
          signedIdentity: fixture.signedIdentity,
          policy: fixture.policy,
          trustRoots: [fixture.trustRoot],
          durableMarker: fixture.durableMarker,
        }),
      "mismatched_identity",
      expectedPath,
    );
    expect(error.code).toBe("mismatched_identity");
  });

  it("keeps signature and trust-root bytes out of diagnostics", () => {
    const fixture = makeFixture();
    fixture.signedIdentity.attestation.signature = "00".repeat(64);
    const error = rejection(
      () =>
        verifyWatcherDeploymentIdentityV1({
          signedIdentity: fixture.signedIdentity,
          policy: fixture.policy,
          trustRoots: [fixture.trustRoot],
          durableMarker: fixture.durableMarker,
        }),
      "invalid_signature",
      "$.attestation.signature",
    );
    const diagnostic = watcherDeploymentIdentityDiagnostic(error);

    expect(JSON.stringify(diagnostic)).not.toContain(
      fixture.signedIdentity.attestation.signature,
    );
    expect(JSON.stringify(diagnostic)).not.toContain(
      fixture.trustRoot.publicKeySpkiDerHex,
    );
  });
});
