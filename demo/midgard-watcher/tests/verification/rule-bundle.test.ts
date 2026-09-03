import { createHash, generateKeyPairSync, sign } from "node:crypto";

import {
  MIDGARD_CONSENSUS_FEATURES,
  MIDGARD_CONSENSUS_LIMITS,
  MIDGARD_CONSENSUS_PROFILE,
  MIDGARD_CONSENSUS_PROFILE_DIGEST,
} from "@al-ft/midgard-core/consensus-profile";
import {
  DA_RUNTIME_MANIFEST_SCHEMA_VERSION,
  DA_TRANSPORT_LIMITS,
  DA_TRANSPORT_PROTOCOL_VERSION,
} from "@al-ft/midgard-core/da-transport";
import {
  computeDeploymentManifestId,
  computeDeploymentManifestJsonDigest,
  DEPLOYMENT_MANIFEST_CONTRACT_NAMES,
  DEPLOYMENT_MANIFEST_ECONOMICS_BY_PROFILE,
  DEPLOYMENT_MANIFEST_L1_FINALITY,
  DEPLOYMENT_MANIFEST_REFERENCE_SCRIPT_CONTRACT_BY_ROLE,
  DEPLOYMENT_MANIFEST_REFERENCE_SCRIPT_TOKEN_NAMES,
  DEPLOYMENT_MANIFEST_STEP_NAMES,
  makeDeploymentMarker,
} from "@al-ft/midgard-core/deployment-manifest-identity";
import { MidgardValidationPhase } from "@al-ft/midgard-core/validation-trace";
import { validatorToScriptHash } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  makeWatcherDeploymentIdentitySignaturePayload,
  verifyWatcherDeploymentIdentity,
  WATCHER_DEPLOYMENT_RELEASE_BINDINGS_SCHEMA_VERSION,
  WATCHER_SIGNED_DEPLOYMENT_IDENTITY_SCHEMA_VERSION,
  WatcherDeploymentIdentityError,
  type WatcherDeploymentIdentityErrorCode,
  type WatcherDeploymentIdentityPolicy,
  type WatcherDeploymentTrustRoot,
} from "../../src/runtime/deployment-identity.js";
import {
  computeWatcherRuleBundleCommitment,
  encodeWatcherRuleBundle,
  loadWatcherRuleBundle,
  makeWatcherCanonicalRuleBundle,
  parseWatcherRuleBundle,
  WATCHER_RULE_BUNDLE_REJECTION_SELECTION,
  WATCHER_RULE_BUNDLE_TRANSITION_PRIORITY,
  WATCHER_RULE_BUNDLE_VALIDATION_PHASE_PRIORITY,
  WatcherRuleBundleError,
  type WatcherRuleBundleErrorCode,
} from "../../src/verification/rule-bundle.js";
import { canonicalFraudProofCatalogueFixture } from "../canonical-fraud-proof-catalogue.js";
import { WATCHER_TEST_CARDANO_PROTOCOL_PARAMETERS } from "../support/deployment-authority-fixture.js";

const h32 = (byte: string): string => byte.repeat(64);

const NATIVE_SCRIPT_CBOR = `8200581c${"00".repeat(28)}`;
const NATIVE_SCRIPT_HASH =
  "9dcfe5a661b6bc3af0999d06416d95842ba7c693dc0e246f5e0a5e33";
const DA_VKEY = "44".repeat(32);
const DA_SIGNERS_HASH =
  "0395256ce5d90f07504b614b9e70e29a06fdd69cef6b01f6018615164125a5c5";
const RELEASE_DIGEST = h32("6");
const BLUEPRINT_HASH = h32("5");

const TARGET_PARAMETERS = Object.freeze({
  coinsPerUtxoByte: "4310",
  maxTxExUnits: Object.freeze({
    memory: "16500000",
    steps: "10000000000",
  }),
  maxTxSize: 16_384,
  maxValueSize: 5_000,
  minFeeA: 44,
  minFeeB: 155_381,
  prices: Object.freeze({
    memory: 0.0577,
    steps: 0.000_072_1,
  }),
});

type MutableRecord = Record<string, any>;

const referenceOutRefByContract = new Map<
  string,
  { txHash: string; outputIndex: number }
>(
  Object.values(DEPLOYMENT_MANIFEST_REFERENCE_SCRIPT_CONTRACT_BY_ROLE).map(
    (contractName, outputIndex) => [
      contractName,
      { txHash: h32("2"), outputIndex },
    ],
  ),
);

const canonicalManifestIdentity = (): MutableRecord => {
  const contracts = Object.fromEntries(
    DEPLOYMENT_MANIFEST_CONTRACT_NAMES.map((contractName, index) => {
      const contractScriptCbor = (index + 1).toString(16).padStart(2, "0");
      return [
        contractName,
        {
          refScriptUTxO: referenceOutRefByContract.get(contractName) ?? null,
          contract: {
            type:
              contractName === "referenceScriptAuthMint"
                ? "Native"
                : "PlutusV3",
            cborHex:
              contractName === "referenceScriptAuthMint"
                ? NATIVE_SCRIPT_CBOR
                : contractScriptCbor,
          },
          scriptHash:
            contractName === "referenceScriptAuthMint"
              ? NATIVE_SCRIPT_HASH
              : validatorToScriptHash({
                  type: "PlutusV3",
                  script: contractScriptCbor,
                }),
        },
      ];
    }),
  ) as MutableRecord;
  contracts.fraudProofCatalogueMint.fraudProofCatalogue =
    canonicalFraudProofCatalogueFixture(contracts);
  const referenceScripts = Object.fromEntries(
    Object.entries(DEPLOYMENT_MANIFEST_REFERENCE_SCRIPT_CONTRACT_BY_ROLE).map(
      ([role, contractName]) => {
        const outRef = referenceOutRefByContract.get(contractName);
        if (outRef === undefined) {
          throw new Error("Missing canonical test reference outref");
        }
        const tokenName =
          DEPLOYMENT_MANIFEST_REFERENCE_SCRIPT_TOKEN_NAMES[
            role as keyof typeof DEPLOYMENT_MANIFEST_REFERENCE_SCRIPT_TOKEN_NAMES
          ];
        return [
          role,
          {
            status: "confirmed",
            roleUnit:
              NATIVE_SCRIPT_HASH +
              Buffer.from(tokenName, "utf8").toString("hex"),
            scriptHash: contracts[contractName].scriptHash,
            outRef: `${outRef.txHash}#${outRef.outputIndex.toString()}`,
          },
        ];
      },
    ),
  );
  return {
    schemaVersion: "midgard-deployment-manifest-v1",
    consensusProfile: MIDGARD_CONSENSUS_PROFILE,
    consensusProfileDigest: MIDGARD_CONSENSUS_PROFILE_DIGEST,
    network: "Preprod",
    cardanoProtocolParameters: {
      snapshot: WATCHER_TEST_CARDANO_PROTOCOL_PARAMETERS,
      digest: computeDeploymentManifestJsonDigest(
        WATCHER_TEST_CARDANO_PROTOCOL_PARAMETERS,
      ),
    },
    genesis: {
      headerHash: "00".repeat(28),
      utxoSetDigest: computeDeploymentManifestJsonDigest([]),
    },
    createdAt: "2026-07-28T00:00:00.000Z",
    updatedAt: "2026-07-28T00:00:00.000Z",
    referenceScriptDeployAddress: "addr_test1vcanonical",
    hubOracleOneShot: {
      txHash: h32("1"),
      outputIndex: 0,
      outRef: `${h32("1")}#0`,
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
      tokenNames: DEPLOYMENT_MANIFEST_REFERENCE_SCRIPT_TOKEN_NAMES,
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
        protocolVersion: DA_TRANSPORT_PROTOCOL_VERSION,
        runtimeManifestSchemaVersion: DA_RUNTIME_MANIFEST_SCHEMA_VERSION,
        envelopeEncoding: "identity",
        zstdLevel: 3,
        limits: DA_TRANSPORT_LIMITS,
        retentionDays: DA_TRANSPORT_LIMITS.minimumRetentionDays,
      },
    },
    proofEvidence: {
      digest: RELEASE_DIGEST,
      blueprintHash: BLUEPRINT_HASH,
    },
    steps: Object.fromEntries(
      DEPLOYMENT_MANIFEST_STEP_NAMES.map((stepName) => [
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
      version: MIDGARD_CONSENSUS_PROFILE.validationDisputeVersion,
      responseWindowMs:
        MIDGARD_CONSENSUS_PROFILE.limits.validationDisputeResponseWindowMs,
      maxBisectionRounds:
        MIDGARD_CONSENSUS_PROFILE.limits.maxValidationBisectionRounds,
      maturityMs: MIDGARD_CONSENSUS_PROFILE.limits.blockMaturityMs,
    },
    economics:
      DEPLOYMENT_MANIFEST_ECONOMICS_BY_PROFILE["bounded-acceptance-v1"],
    l1Finality: DEPLOYMENT_MANIFEST_L1_FINALITY,
    availabilityChallenge: {
      responseClasses: {
        smallPayloadMaxBytes: 65_536,
        smallResponseWindowMs: 3_600_000,
        fullPayloadMaxBytes: 67_108_864,
        fullResponseWindowMs: 172_800_000,
      },
      responseGeometry: {
        chunkByteLength: 14_020,
        trancheByteLength: 4_194_304,
        maxTrancheCount: 16,
      },
      daBondLovelace: 10_000_000_000,
      challengerBondLovelace: 10_000_000_000,
      maxOpenFeeLovelace: 500_000,
      maxPublicationFeeLovelace: 500_000,
      maxSettlementFeeLovelace: 500_000,
      maxCloseFeeLovelace: 1_000_000,
      maxTimeoutFeeLovelace: 1_200_000,
      bondOwnerCredential: "77".repeat(28),
    },
  };
};

const withManifestId = (identity: MutableRecord): MutableRecord => ({
  ...identity,
  manifestId: computeDeploymentManifestId(identity),
});

const makeTrustRoot = (): {
  readonly privateKey: ReturnType<typeof generateKeyPairSync>["privateKey"];
  readonly trustRoot: WatcherDeploymentTrustRoot;
} => {
  const { privateKey, publicKey } = generateKeyPairSync("ed25519");
  const publicKeySpkiDer = publicKey.export({
    format: "der",
    type: "spki",
  });
  const publicKeySpkiDerHex = publicKeySpkiDer.toString("hex");
  return {
    privateKey,
    trustRoot: {
      trustRootId: createHash("sha256").update(publicKeySpkiDer).digest("hex"),
      publicKeySpkiDerHex,
    },
  };
};

const appliedScriptHashes = (manifest: MutableRecord): Record<string, string> =>
  Object.fromEntries(
    DEPLOYMENT_MANIFEST_CONTRACT_NAMES.map((contractName) => [
      contractName,
      manifest.contracts[contractName].scriptHash,
    ]),
  );

const referenceScriptPolicy = (
  manifest: MutableRecord,
): Record<string, { scriptHash: string; outRef: string }> =>
  Object.fromEntries(
    Object.keys(DEPLOYMENT_MANIFEST_REFERENCE_SCRIPT_CONTRACT_BY_ROLE).map(
      (role) => [
        role,
        {
          scriptHash: manifest.referenceScripts[role].scriptHash,
          outRef: manifest.referenceScripts[role].outRef,
        },
      ],
    ),
  );

const cataloguePolicy = (
  manifest: MutableRecord,
): WatcherDeploymentIdentityPolicy["fraudProofCatalogue"] => {
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
  } as WatcherDeploymentIdentityPolicy["fraudProofCatalogue"];
};

type SignedAuthorityFixture = Readonly<{
  signedIdentity: MutableRecord;
  policy: WatcherDeploymentIdentityPolicy;
  trustRoots: readonly WatcherDeploymentTrustRoot[];
  durableMarker: ReturnType<typeof makeDeploymentMarker>;
}>;

const fixture = () => {
  const manifest = withManifestId(canonicalManifestIdentity());
  const programCommitments = Object.freeze({
    "transition-order-v1": h32("8"),
    "validation-machine-v1": h32("9"),
  });
  const bundle = makeWatcherCanonicalRuleBundle({
    constructionIdentity: {
      manifestId: manifest.manifestId,
      network: "Preprod",
      releaseEvidenceDigest: RELEASE_DIGEST,
      programCommitments,
    },
    targetParameterSnapshot: TARGET_PARAMETERS,
  });
  const ruleBundleCommitment = computeWatcherRuleBundleCommitment(bundle);
  const releaseBindings = {
    schemaVersion: WATCHER_DEPLOYMENT_RELEASE_BINDINGS_SCHEMA_VERSION,
    ruleBundleCommitment,
    programCommitments,
    da: {
      mode: "authenticated_committee_v1",
      identityDigest: computeDeploymentManifestJsonDigest(manifest.da),
    },
    releaseEvidence: {
      digest: RELEASE_DIGEST,
      blueprintHash: BLUEPRINT_HASH,
    },
  };
  const { privateKey, trustRoot } = makeTrustRoot();
  const signedIdentity: MutableRecord = {
    schemaVersion: WATCHER_SIGNED_DEPLOYMENT_IDENTITY_SCHEMA_VERSION,
    manifest,
    releaseBindings,
    attestation: {
      algorithm: "ed25519",
      trustRootId: trustRoot.trustRootId,
      signature: "",
    },
  };
  const policy: WatcherDeploymentIdentityPolicy = {
    network: "Preprod",
    hubOracleOneShotOutRef: manifest.hubOracleOneShot.outRef,
    appliedScriptHashes: appliedScriptHashes(manifest),
    referenceScripts: referenceScriptPolicy(manifest),
    fraudProofCatalogue: cataloguePolicy(manifest),
    ruleBundleCommitment,
    programCommitments,
    daMode: "authenticated_committee_v1",
    daIdentityDigest: releaseBindings.da.identityDigest,
    releaseEvidenceDigest: RELEASE_DIGEST,
    blueprintHash: BLUEPRINT_HASH,
  };
  signedIdentity.attestation.signature = sign(
    null,
    makeWatcherDeploymentIdentitySignaturePayload(
      manifest.manifestId,
      releaseBindings,
    ),
    privateKey,
  ).toString("hex");
  const authority: SignedAuthorityFixture = Object.freeze({
    signedIdentity,
    policy,
    trustRoots: Object.freeze([trustRoot]),
    durableMarker: makeDeploymentMarker(manifest.manifestId),
  });
  return {
    authority,
    bundle,
    verifiedIdentity: verifyWatcherDeploymentIdentity(authority),
  };
};

type Mutable<T> = T extends readonly (infer Entry)[]
  ? Mutable<Entry>[]
  : T extends object
    ? { -readonly [Key in keyof T]: Mutable<T[Key]> }
    : T;

const clone = <T>(value: T): Mutable<T> =>
  JSON.parse(JSON.stringify(value)) as Mutable<T>;

const rejected = (
  action: () => unknown,
  code: WatcherRuleBundleErrorCode,
  path: string,
): WatcherRuleBundleError => {
  try {
    action();
  } catch (error) {
    expect(error).toBeInstanceOf(WatcherRuleBundleError);
    const ruleError = error as WatcherRuleBundleError;
    expect(ruleError.code).toBe(code);
    expect(ruleError.path).toBe(path);
    return ruleError;
  }
  throw new Error("Expected canonical V1 rule-bundle rejection");
};

const authorityRejected = (
  action: () => unknown,
  code: WatcherDeploymentIdentityErrorCode,
  path: string,
): WatcherDeploymentIdentityError => {
  try {
    action();
  } catch (error) {
    expect(error).toBeInstanceOf(WatcherDeploymentIdentityError);
    const authorityError = error as WatcherDeploymentIdentityError;
    expect(authorityError.code).toBe(code);
    expect(authorityError.path).toBe(path);
    return authorityError;
  }
  throw new Error("Expected signed W02 deployment-authority rejection");
};

describe("watcher canonical V1 rule bundle", () => {
  it("loads the one exact W02-bound V1 profile, features, parameters, priorities, and programs", () => {
    const { authority, bundle, verifiedIdentity } = fixture();
    const loaded = loadWatcherRuleBundle({
      ...authority,
      ruleBundle: bundle,
    });

    expect(loaded.ruleBundleCommitment).toBe(
      verifiedIdentity.ruleBundleCommitment,
    );
    expect(loaded.ruleBundle.consensusProfileDigest).toBe(
      MIDGARD_CONSENSUS_PROFILE_DIGEST,
    );
    expect(loaded.ruleBundle.features).toEqual(
      MIDGARD_CONSENSUS_FEATURES.map((featureId) => ({
        featureId,
        enabled: true,
      })),
    );
    expect(loaded.ruleBundle.limits).toBe(MIDGARD_CONSENSUS_LIMITS);
    expect(loaded.ruleBundle.targetParameters).toEqual({
      snapshot: TARGET_PARAMETERS,
      digest: computeDeploymentManifestJsonDigest(TARGET_PARAMETERS),
    });
    expect(loaded.ruleBundle.transitionPriority).toBe(
      WATCHER_RULE_BUNDLE_TRANSITION_PRIORITY,
    );
    expect(loaded.ruleBundle.validation.phasePriority).toBe(
      WATCHER_RULE_BUNDLE_VALIDATION_PHASE_PRIORITY,
    );
    expect(loaded.ruleBundle.validation.rejectionSelection).toBe(
      WATCHER_RULE_BUNDLE_REJECTION_SELECTION,
    );
    expect(loaded.ruleBundle.programCommitments).toEqual(
      verifiedIdentity.programCommitments,
    );
    expect(Object.isFrozen(loaded)).toBe(true);
    expect(Object.isFrozen(loaded.ruleBundle)).toBe(true);
    expect(Object.isFrozen(loaded.ruleBundle.features)).toBe(true);
    expect(Object.isFrozen(loaded.ruleBundle.targetParameters.snapshot)).toBe(
      true,
    );
  });

  it("has deterministic bytes and survives exact JSON restart serialization", () => {
    const { authority, bundle, verifiedIdentity } = fixture();
    const firstBytes = encodeWatcherRuleBundle(bundle);
    const restarted = JSON.parse(firstBytes.toString("utf8")) as unknown;
    const loaded = loadWatcherRuleBundle({
      ...clone(authority),
      ruleBundle: restarted,
    });

    expect(encodeWatcherRuleBundle(loaded.ruleBundle)).toEqual(firstBytes);
    expect(computeWatcherRuleBundleCommitment(restarted)).toBe(
      verifiedIdentity.ruleBundleCommitment,
    );
    expect(Object.keys(loaded.ruleBundle.targetParameters.snapshot)).toEqual(
      Object.keys(TARGET_PARAMETERS).sort(),
    );
  });

  it("rejects unknown, adjacent, missing, and extra V1 bundle shapes", () => {
    const { bundle } = fixture();

    const unknownVersion = clone(bundle) as Record<string, unknown>;
    unknownVersion.ruleBundleVersion = 2;
    rejected(
      () => parseWatcherRuleBundle(unknownVersion),
      "unsupported_version",
      "$.ruleBundleVersion",
    );

    const adjacentSchema = clone(bundle) as Record<string, unknown>;
    adjacentSchema.schemaVersion = "midgard-watcher-rule-bundle-v2";
    rejected(
      () => parseWatcherRuleBundle(adjacentSchema),
      "unsupported_version",
      "$.schemaVersion",
    );

    const missing = clone(bundle) as Record<string, unknown>;
    delete missing.validation;
    rejected(
      () => parseWatcherRuleBundle(missing),
      "missing_field",
      "$.validation",
    );

    const extra = clone(bundle) as Record<string, unknown>;
    extra.compatibility = true;
    rejected(
      () => parseWatcherRuleBundle(extra),
      "unknown_field",
      "$.compatibility",
    );

    rejected(() => parseWatcherRuleBundle([bundle]), "invalid_field", "$");
  });

  it("rejects every feature-set weakening, extension, duplication, or reordering", () => {
    const { bundle } = fixture();

    const disabled = clone(bundle);
    disabled.features[0]!.enabled = false as true;
    rejected(
      () => parseWatcherRuleBundle(disabled),
      "disabled_feature",
      "$.features[0].enabled",
    );

    const unknown = clone(bundle);
    unknown.features[0]!.featureId =
      "watcher_only_feature" as (typeof unknown.features)[number]["featureId"];
    rejected(
      () => parseWatcherRuleBundle(unknown),
      "unknown_feature",
      "$.features[0].featureId",
    );

    const missing = clone(bundle);
    missing.features.pop();
    rejected(
      () => parseWatcherRuleBundle(missing),
      "feature_set_mismatch",
      "$.features",
    );

    const duplicate = clone(bundle);
    duplicate.features[1] = clone(duplicate.features[0]!);
    rejected(
      () => parseWatcherRuleBundle(duplicate),
      "feature_set_mismatch",
      "$.features",
    );

    const extra = clone(bundle);
    extra.features.push(clone(extra.features[0]!));
    rejected(
      () => parseWatcherRuleBundle(extra),
      "feature_set_mismatch",
      "$.features",
    );

    const reordered = clone(bundle);
    [reordered.features[0], reordered.features[1]] = [
      reordered.features[1]!,
      reordered.features[0]!,
    ];
    rejected(
      () => parseWatcherRuleBundle(reordered),
      "feature_set_mismatch",
      "$.features",
    );
  });

  it("rejects profile, limit, target-parameter, transition, and validation drift", () => {
    const { bundle } = fixture();

    const profile = clone(bundle);
    profile.consensusProfileDigest = h32("a");
    rejected(
      () => parseWatcherRuleBundle(profile),
      "consensus_profile_mismatch",
      "$.consensusProfileDigest",
    );

    const limit = clone(bundle);
    (limit.limits as Record<string, number>).maxL2TransactionCount += 1;
    rejected(
      () => parseWatcherRuleBundle(limit),
      "consensus_profile_mismatch",
      "$.limits.maxL2TransactionCount",
    );

    const target = clone(bundle);
    (target.targetParameters.snapshot as Record<string, unknown>).minFeeA = 45;
    rejected(
      () => parseWatcherRuleBundle(target),
      "target_parameters_mismatch",
      "$.targetParameters.digest",
    );

    const transition = clone(bundle);
    [transition.transitionPriority[0], transition.transitionPriority[1]] = [
      transition.transitionPriority[1]!,
      transition.transitionPriority[0]!,
    ];
    rejected(
      () => parseWatcherRuleBundle(transition),
      "transition_priority_mismatch",
      "$.transitionPriority",
    );

    const validation = clone(bundle);
    [
      validation.validation.phasePriority[0],
      validation.validation.phasePriority[1],
    ] = [
      validation.validation.phasePriority[1]!,
      validation.validation.phasePriority[0]!,
    ];
    rejected(
      () => parseWatcherRuleBundle(validation),
      "validation_priority_mismatch",
      "$.validation.phasePriority",
    );

    const selection = clone(bundle);
    selection.validation.rejectionSelection =
      "watcher_first_observed_rejection_v1" as typeof selection.validation.rejectionSelection;
    rejected(
      () => parseWatcherRuleBundle(selection),
      "validation_priority_mismatch",
      "$.validation.rejectionSelection",
    );
  });

  it("rejects program and rule-bundle commitment drift independently", () => {
    const { authority, bundle } = fixture();

    const program = clone(bundle);
    program.programCommitments["validation-machine-v1"] = h32("a");
    rejected(
      () =>
        loadWatcherRuleBundle({
          ...authority,
          ruleBundle: program,
        }),
      "program_commitment_mismatch",
      "$.ruleBundle.programCommitments",
    );

    const missingProgram = clone(bundle);
    delete missingProgram.programCommitments["validation-machine-v1"];
    rejected(
      () =>
        loadWatcherRuleBundle({
          ...authority,
          ruleBundle: missingProgram,
        }),
      "program_commitment_mismatch",
      "$.ruleBundle.programCommitments",
    );

    const extraProgram = clone(bundle);
    extraProgram.programCommitments["watcher-folklore-v1"] = h32("b");
    rejected(
      () =>
        loadWatcherRuleBundle({
          ...authority,
          ruleBundle: extraProgram,
        }),
      "program_commitment_mismatch",
      "$.ruleBundle.programCommitments",
    );

    const content = clone(bundle);
    (content.targetParameters.snapshot as Record<string, unknown>).minFeeA = 45;
    content.targetParameters.digest = computeDeploymentManifestJsonDigest(
      content.targetParameters.snapshot,
    );
    rejected(
      () =>
        loadWatcherRuleBundle({
          ...authority,
          ruleBundle: content,
        }),
      "rule_bundle_commitment_mismatch",
      "$.ruleBundle",
    );
  });

  it("rejects deployment, release, network, and durable-marker cross-binding drift", () => {
    const { authority, bundle } = fixture();

    const deployment = clone(bundle);
    deployment.deploymentManifestId = h32("a");
    rejected(
      () =>
        loadWatcherRuleBundle({
          ...authority,
          ruleBundle: deployment,
        }),
      "deployment_identity_mismatch",
      "$.ruleBundle",
    );

    const network = clone(bundle);
    network.network = "Preview";
    rejected(
      () =>
        loadWatcherRuleBundle({
          ...authority,
          ruleBundle: network,
        }),
      "deployment_identity_mismatch",
      "$.ruleBundle",
    );

    const release = clone(bundle);
    release.releaseEvidenceDigest = h32("b");
    rejected(
      () =>
        loadWatcherRuleBundle({
          ...authority,
          ruleBundle: release,
        }),
      "deployment_identity_mismatch",
      "$.ruleBundle",
    );

    authorityRejected(
      () =>
        loadWatcherRuleBundle({
          ...authority,
          durableMarker: makeDeploymentMarker(h32("c")),
          ruleBundle: bundle,
        }),
      "durable_marker_mismatch",
      "$.durableMarker",
    );
  });

  it("requires raw signed W02 authority and rejects forged summaries, signatures, policies, and trust roots", () => {
    const { authority, bundle, verifiedIdentity } = fixture();

    authorityRejected(
      () =>
        loadWatcherRuleBundle({
          ...authority,
          signedIdentity: verifiedIdentity,
          ruleBundle: bundle,
        }),
      "unknown_field",
      "$.manifestId",
    );

    const invalidSignature = clone(authority);
    const signature = invalidSignature.signedIdentity.attestation
      .signature as string;
    invalidSignature.signedIdentity.attestation.signature = `${signature.startsWith("0") ? "1" : "0"}${signature.slice(1)}`;
    authorityRejected(
      () =>
        loadWatcherRuleBundle({
          ...invalidSignature,
          ruleBundle: bundle,
        }),
      "invalid_signature",
      "$.attestation.signature",
    );

    const mismatchedPolicy = clone(authority.policy);
    mismatchedPolicy.ruleBundleCommitment = h32("a");
    authorityRejected(
      () =>
        loadWatcherRuleBundle({
          ...authority,
          policy: mismatchedPolicy,
          ruleBundle: bundle,
        }),
      "mismatched_identity",
      "$.releaseBindings",
    );

    authorityRejected(
      () =>
        loadWatcherRuleBundle({
          ...authority,
          trustRoots: [makeTrustRoot().trustRoot],
          ruleBundle: bundle,
        }),
      "untrusted_signer",
      "$.attestation.trustRootId",
    );
  });

  it("derives the validation priority from the production canonical phase codes", () => {
    const order = Object.entries(MidgardValidationPhase)
      .sort((left, right) => left[1] - right[1])
      .map(([phase]) => phase);
    expect(WATCHER_RULE_BUNDLE_VALIDATION_PHASE_PRIORITY).toEqual(order);
    expect(order).toEqual([
      "canonicalDecode",
      "compactBinding",
      "staticLedgerRules",
      "inputSets",
      "signatures",
      "phaseANativeScripts",
      "phaseAScriptPreconditions",
      "resolveInputs",
      "scriptSources",
      "nativeScripts",
      "scriptIntegrity",
      "cek",
      "valueAndMint",
      "ledgerDelta",
      "terminal",
    ]);
  });
});
