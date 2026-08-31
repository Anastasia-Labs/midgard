/**
 * The single signed-deployment-authority fixture the watcher indexer suites
 * share.
 *
 * Every authenticated indexer suite needs the same thing before it can assert
 * anything: a deployment manifest whose contracts, reference scripts, DA
 * identity and release bindings all hang together, signed by a trust root the
 * watcher will accept. That fixture was copied into
 * `settlement-indexer`, `state-queue-indexer`, `user-event-indexer` and
 * `proof-thread-indexer` (and again into `w15-`/`w16-authority-scenarios`),
 * so a manifest-shape change had to be made six times or the suites silently
 * disagreed about what a valid deployment looks like. This module is the one
 * copy.
 *
 * Note this is deliberately NOT the same fixture as
 * `watcher-opaque-authority-harness.ts`'s
 * `createWatcherAuthorityDeploymentFixtureV1`. That one signs with a fixed
 * ed25519 key and freezes its result, so every call yields an identical
 * attestation; this one calls `generateKeyPairSync` per call and leaves the
 * result mutable, which is what the indexer suites' forgery and
 * substitution tests depend on. The two are not interchangeable.
 */
import { createHash, generateKeyPairSync, sign } from "node:crypto";

import { validatorToScriptHash } from "@lucid-evolution/lucid";

import {
  MIDGARD_CONSENSUS_PROFILE_V1,
  MIDGARD_CONSENSUS_PROFILE_V1_DIGEST,
} from "../../../midgard-core/src/consensus-profile-v1.js";
import {
  DA_RUNTIME_MANIFEST_V1_SCHEMA_VERSION,
  DA_TRANSPORT_LIMITS_V1,
  DA_TRANSPORT_V1_PROTOCOL_VERSION,
} from "../../../midgard-core/src/da-transport.js";
import {
  computeDeploymentManifestV1Id,
  computeDeploymentManifestV1JsonDigest,
  DEPLOYMENT_MANIFEST_V1_CONTRACT_NAMES,
  DEPLOYMENT_MANIFEST_V1_ECONOMICS_BY_PROFILE,
  DEPLOYMENT_MANIFEST_V1_L1_FINALITY,
  DEPLOYMENT_MANIFEST_V1_REFERENCE_SCRIPT_CONTRACT_BY_ROLE,
  DEPLOYMENT_MANIFEST_V1_REFERENCE_SCRIPT_TOKEN_NAMES,
  DEPLOYMENT_MANIFEST_V1_STEP_NAMES,
  makeDeploymentMarkerV1,
} from "../../../midgard-core/src/deployment-manifest-identity-v1.js";
import {
  makeWatcherDeploymentIdentitySignaturePayloadV1,
  verifyWatcherDeploymentIdentityV1,
  WATCHER_DEPLOYMENT_RELEASE_BINDINGS_V1_SCHEMA_VERSION,
  WATCHER_SIGNED_DEPLOYMENT_IDENTITY_V1_SCHEMA_VERSION,
  type WatcherDeploymentIdentityPolicyV1,
} from "../../src/deployment-identity.js";
import { canonicalFraudProofCatalogueFixture } from "../canonical-fraud-proof-catalogue.js";

export const h28 = (byte: string): string => byte.repeat(28);
export const h32 = (byte: string): string => byte.repeat(32);
export const sha256 = (bytes: Uint8Array): string =>
  createHash("sha256").update(bytes).digest("hex");
export const asWireValue = <T>(value: T): T =>
  JSON.parse(JSON.stringify(value)) as T;

export const NATIVE_SCRIPT_CBOR = `8200581c${"00".repeat(28)}`;
export const NATIVE_SCRIPT_HASH =
  "9dcfe5a661b6bc3af0999d06416d95842ba7c693dc0e246f5e0a5e33";
export const DA_SIGNERS_HASH =
  "0395256ce5d90f07504b614b9e70e29a06fdd69cef6b01f6018615164125a5c5";

/** The release/rule-bundle bytes the settlement, state-queue and user-event
 * suites pin. The proof-thread suite pins its own — see the options below. */
export const WATCHER_AUTHORITY_RELEASE_DIGEST_V1 = h32("22");
export const WATCHER_AUTHORITY_BLUEPRINT_HASH_V1 = h32("55");
export const WATCHER_AUTHORITY_RULE_BUNDLE_COMMITMENT_V1 = h32("44");
export const WATCHER_AUTHORITY_PROGRAM_COMMITMENTS_V1 = {
  "validation-machine-v1": h32("88"),
  "transition-order-v1": h32("99"),
};

type MutableRecord = Record<string, any>;

const deepFreezeFixtureV1 = <T>(value: T): T => {
  if (value !== null && typeof value === "object" && !Object.isFrozen(value)) {
    Object.freeze(value);
    for (const nested of Object.values(value)) {
      deepFreezeFixtureV1(nested);
    }
  }
  return value;
};

export type AuthorityContractFixtureV1 = Readonly<{
  refScriptUTxO: Readonly<{ txHash: string; outputIndex: number }> | null;
  contract: Readonly<{ type: string; cborHex: string }>;
  scriptHash: string;
}> & {
  fraudProofCatalogue?: ReturnType<typeof canonicalFraudProofCatalogueFixture>;
};

export type AuthorityReferenceScriptFixtureV1 = Readonly<{
  status: string;
  roleUnit: string;
  scriptHash: string;
  outRef: string;
}>;

export type WatcherAuthorityContractSetV1 = Readonly<{
  contracts: Record<string, AuthorityContractFixtureV1>;
  fraudProofCatalogue: ReturnType<typeof canonicalFraudProofCatalogueFixture>;
  referenceScripts: Record<string, AuthorityReferenceScriptFixtureV1>;
}>;

/**
 * The applied contracts, catalogue and reference scripts, built without
 * committing to a release identity yet.
 *
 * Split out because the proof-thread suite derives its
 * `proof-thread-catalogue-v1` program commitment from the catalogue's own
 * category script hashes — it has to see the contracts before it can say what
 * the manifest commits to.
 */
const buildWatcherAuthorityContractsV1 = (): WatcherAuthorityContractSetV1 => {
  const referenceOutRefByContract = new Map<
    string,
    { txHash: string; outputIndex: number }
  >(
    Object.values(DEPLOYMENT_MANIFEST_V1_REFERENCE_SCRIPT_CONTRACT_BY_ROLE).map(
      (contractName, outputIndex) => [
        contractName,
        { txHash: h32("12"), outputIndex },
      ],
    ),
  );
  const contracts = Object.fromEntries(
    DEPLOYMENT_MANIFEST_V1_CONTRACT_NAMES.map((contractName, index) => {
      const native = contractName === "referenceScriptAuthMint";
      const script = native
        ? NATIVE_SCRIPT_CBOR
        : (index + 1).toString(16).padStart(2, "0");
      return [
        contractName,
        {
          refScriptUTxO: referenceOutRefByContract.get(contractName) ?? null,
          contract: { type: native ? "Native" : "PlutusV3", cborHex: script },
          scriptHash: native
            ? NATIVE_SCRIPT_HASH
            : validatorToScriptHash({ type: "PlutusV3", script }),
        },
      ];
    }),
  ) as Record<string, AuthorityContractFixtureV1>;
  const fraudProofCatalogue = canonicalFraudProofCatalogueFixture(contracts);
  const catalogueContract = contracts.fraudProofCatalogueMint;
  if (catalogueContract === undefined) {
    throw new Error("authority catalogue contract is missing");
  }
  catalogueContract.fraudProofCatalogue = fraudProofCatalogue;
  const referenceScripts = Object.fromEntries(
    Object.entries(
      DEPLOYMENT_MANIFEST_V1_REFERENCE_SCRIPT_CONTRACT_BY_ROLE,
    ).map(([role, contractName]) => {
      const outRef = referenceOutRefByContract.get(contractName)!;
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
  ) as Record<string, AuthorityReferenceScriptFixtureV1>;
  return { contracts, fraudProofCatalogue, referenceScripts };
};

let cachedWatcherAuthorityContractsV1: WatcherAuthorityContractSetV1 | null =
  null;

export const makeWatcherAuthorityContractsV1 =
  (): WatcherAuthorityContractSetV1 => {
    cachedWatcherAuthorityContractsV1 ??= deepFreezeFixtureV1(
      buildWatcherAuthorityContractsV1(),
    );
    return structuredClone(cachedWatcherAuthorityContractsV1);
  };

export type WatcherDeploymentAuthorityFixtureOptionsV1 = Readonly<{
  /** Reuse an already-built contract set, so the caller can derive program
   * commitments from the catalogue it is about to commit to. */
  contractSet?: WatcherAuthorityContractSetV1;
  releaseDigest?: string;
  blueprintHash?: string;
  ruleBundleCommitment?: string;
  programCommitments?: Readonly<Record<string, string>>;
}>;

export const WATCHER_TEST_CARDANO_PROTOCOL_PARAMETERS_V1 = Object.freeze({
  minFeeA: "44",
  minFeeB: "155381",
  priceMemory: Object.freeze({ numerator: "577", denominator: "10000" }),
  priceSteps: Object.freeze({ numerator: "721", denominator: "10000000" }),
  coinsPerUtxoByte: "4310",
  collateralPercentage: "150",
  maxCollateralInputs: "3",
  maxTxSize: "16384",
  maxValueSize: "5000",
  maxTxExUnits: Object.freeze({
    memory: "16500000",
    steps: "10000000000",
  }),
  referenceScriptFee: Object.freeze({
    base: Object.freeze({ numerator: "15", denominator: "1" }),
    range: "25600",
    multiplier: Object.freeze({ numerator: "6", denominator: "5" }),
    maximumSizeBytes: "204800",
  }),
});

const buildWatcherDeploymentAuthorityFixtureV1 = (
  options: WatcherDeploymentAuthorityFixtureOptionsV1 = {},
) => {
  const releaseDigest =
    options.releaseDigest ?? WATCHER_AUTHORITY_RELEASE_DIGEST_V1;
  const blueprintHash =
    options.blueprintHash ?? WATCHER_AUTHORITY_BLUEPRINT_HASH_V1;
  const ruleBundleCommitment =
    options.ruleBundleCommitment ?? WATCHER_AUTHORITY_RULE_BUNDLE_COMMITMENT_V1;
  const programCommitments =
    options.programCommitments ?? WATCHER_AUTHORITY_PROGRAM_COMMITMENTS_V1;
  const { contracts, fraudProofCatalogue, referenceScripts } =
    options.contractSet ?? makeWatcherAuthorityContractsV1();
  const parameters = WATCHER_TEST_CARDANO_PROTOCOL_PARAMETERS_V1;
  const hubOracleOneShot = {
    txHash: h32("11"),
    outputIndex: 0,
    outRef: `${h32("11")}#0`,
    status: "consumed_by_init",
  };
  const daIdentity = {
    committeeVkeys: [h32("44")],
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
  };
  const identity: MutableRecord = {
    schemaVersion: "midgard-deployment-manifest-v1",
    consensusProfile: MIDGARD_CONSENSUS_PROFILE_V1,
    consensusProfileDigest: MIDGARD_CONSENSUS_PROFILE_V1_DIGEST,
    network: "Preprod",
    cardanoProtocolParameters: {
      snapshot: parameters,
      digest: computeDeploymentManifestV1JsonDigest(parameters),
    },
    genesis: {
      headerHash: h28("00"),
      utxoSetDigest: computeDeploymentManifestV1JsonDigest([]),
    },
    createdAt: "2026-07-28T00:00:00.000Z",
    updatedAt: "2026-07-28T00:00:00.000Z",
    referenceScriptDeployAddress: "addr_test1vcanonical",
    hubOracleOneShot,
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
    da: daIdentity,
    proofEvidence: {
      digest: releaseDigest,
      blueprintHash,
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
    economics:
      DEPLOYMENT_MANIFEST_V1_ECONOMICS_BY_PROFILE["bounded-acceptance-v1"],
    l1Finality: DEPLOYMENT_MANIFEST_V1_L1_FINALITY,
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
      bondOwnerCredential: h28("77"),
    },
  };
  const manifestId = computeDeploymentManifestV1Id(identity);
  const manifest: MutableRecord = {
    ...identity,
    manifestId,
  };
  const releaseBindings = {
    schemaVersion: WATCHER_DEPLOYMENT_RELEASE_BINDINGS_V1_SCHEMA_VERSION,
    ruleBundleCommitment,
    programCommitments,
    da: {
      mode: "authenticated_committee_v1",
      identityDigest: computeDeploymentManifestV1JsonDigest(daIdentity),
    },
    releaseEvidence: {
      digest: releaseDigest,
      blueprintHash,
    },
  };
  const { privateKey, publicKey } = generateKeyPairSync("ed25519");
  const publicKeySpkiDerHex = publicKey
    .export({ format: "der", type: "spki" })
    .toString("hex");
  const trustRootId = sha256(Buffer.from(publicKeySpkiDerHex, "hex"));
  const signedIdentity: MutableRecord = {
    schemaVersion: WATCHER_SIGNED_DEPLOYMENT_IDENTITY_V1_SCHEMA_VERSION,
    manifest,
    releaseBindings,
    attestation: {
      algorithm: "ed25519",
      trustRootId,
      signature: "",
    },
  };
  signedIdentity.attestation.signature = sign(
    null,
    makeWatcherDeploymentIdentitySignaturePayloadV1(
      manifestId,
      releaseBindings,
    ),
    privateKey,
  ).toString("hex");
  const deploymentPolicy: WatcherDeploymentIdentityPolicyV1 = {
    network: "Preprod",
    hubOracleOneShotOutRef: hubOracleOneShot.outRef,
    appliedScriptHashes: Object.fromEntries(
      DEPLOYMENT_MANIFEST_V1_CONTRACT_NAMES.map((name) => [
        name,
        contracts[name].scriptHash,
      ]),
    ),
    referenceScripts: Object.fromEntries(
      Object.keys(DEPLOYMENT_MANIFEST_V1_REFERENCE_SCRIPT_CONTRACT_BY_ROLE).map(
        (role) => [
          role,
          {
            scriptHash: referenceScripts[role]!.scriptHash,
            outRef: referenceScripts[role]!.outRef,
          },
        ],
      ),
    ),
    fraudProofCatalogue: {
      root: fraudProofCatalogue.root,
      categories: Object.fromEntries(
        Object.entries(fraudProofCatalogue.categories).map(([name, value]) => {
          const category = value as {
            readonly categoryId?: unknown;
            readonly scriptHash?: unknown;
          };
          if (
            typeof category.categoryId !== "string" ||
            typeof category.scriptHash !== "string"
          ) {
            throw new Error("authority catalogue category is malformed");
          }
          return [
            name,
            {
              categoryId: category.categoryId,
              scriptHash: category.scriptHash,
            },
          ];
        }),
      ),
    } as WatcherDeploymentIdentityPolicyV1["fraudProofCatalogue"],
    ruleBundleCommitment,
    programCommitments,
    daMode: "authenticated_committee_v1",
    daIdentityDigest: releaseBindings.da.identityDigest,
    releaseEvidenceDigest: releaseDigest,
    blueprintHash,
  };
  const trustRoots = [{ trustRootId, publicKeySpkiDerHex }];
  const marker = makeDeploymentMarkerV1(manifestId);
  const result = verifyWatcherDeploymentIdentityV1({
    signedIdentity,
    policy: deploymentPolicy,
    trustRoots,
    durableMarker: marker,
  });
  return {
    signedIdentity,
    policy: deploymentPolicy,
    trustRoots,
    result,
    marker,
    contracts,
  };
};

type WatcherDeploymentAuthorityFixtureV1 = ReturnType<
  typeof buildWatcherDeploymentAuthorityFixtureV1
>;

let cachedDefaultWatcherDeploymentAuthorityFixtureV1: WatcherDeploymentAuthorityFixtureV1 | null =
  null;

const cloneDefaultWatcherDeploymentAuthorityFixtureV1 = (
  fixture: WatcherDeploymentAuthorityFixtureV1,
): WatcherDeploymentAuthorityFixtureV1 => {
  const mutable = structuredClone({
    signedIdentity: fixture.signedIdentity,
    policy: fixture.policy,
    trustRoots: fixture.trustRoots,
    marker: fixture.marker,
    contracts: fixture.contracts,
  });
  return {
    ...mutable,
    // The verifier result is deliberately shared: it is frozen and its live
    // authority is module-admitted, so a structural clone would be invalid.
    result: fixture.result,
  };
};

export const makeWatcherDeploymentAuthorityFixtureV1 = (
  options: WatcherDeploymentAuthorityFixtureOptionsV1 = {},
): WatcherDeploymentAuthorityFixtureV1 => {
  if (Object.keys(options).length !== 0) {
    return buildWatcherDeploymentAuthorityFixtureV1(options);
  }
  cachedDefaultWatcherDeploymentAuthorityFixtureV1 ??= deepFreezeFixtureV1(
    buildWatcherDeploymentAuthorityFixtureV1(),
  );
  return cloneDefaultWatcherDeploymentAuthorityFixtureV1(
    cachedDefaultWatcherDeploymentAuthorityFixtureV1,
  );
};

/** The default-parameter authority the settlement, state-queue and user-event
 * suites all pin. */
export const makeDeploymentAuthority = () =>
  makeWatcherDeploymentAuthorityFixtureV1();
