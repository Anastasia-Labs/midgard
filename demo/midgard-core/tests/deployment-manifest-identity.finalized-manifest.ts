import { Trie } from "@aiken-lang/merkle-patricia-forestry";
import {
  credentialToAddress,
  validatorToScriptHash,
} from "@lucid-evolution/lucid";
import { blake2b } from "@noble/hashes/blake2.js";
import { bytesToHex, hexToBytes } from "@noble/hashes/utils.js";
import { beforeAll } from "vitest";

import {
  MIDGARD_CONSENSUS_PROFILE,
  MIDGARD_CONSENSUS_PROFILE_DIGEST,
  MIDGARD_DEPLOYMENT_MANIFEST_SCHEMA_VERSION,
} from "../src/consensus-profile.js";
import {
  DA_RUNTIME_MANIFEST_SCHEMA_VERSION,
  DA_TRANSPORT_LIMITS,
  DA_TRANSPORT_PROTOCOL_VERSION,
} from "../src/da-transport.js";
import {
  computeDeploymentManifestId,
  computeDeploymentManifestJsonDigest,
  DEPLOYMENT_MANIFEST_CONTRACT_NAMES,
  DEPLOYMENT_MANIFEST_ECONOMICS_BY_PROFILE,
  DEPLOYMENT_MANIFEST_FRAUD_PROOF_CATALOGUE_CATEGORY_IDS,
  DEPLOYMENT_MANIFEST_FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER,
  DEPLOYMENT_MANIFEST_L1_FINALITY,
  DEPLOYMENT_MANIFEST_REFERENCE_SCRIPT_CONTRACT_BY_ROLE,
  DEPLOYMENT_MANIFEST_REFERENCE_SCRIPT_TOKEN_NAMES,
  type DeploymentManifestFraudProofCatalogueIdentity,
} from "../src/deployment-manifest-identity.js";
import {
  daBondManifestAmounts,
  SELECTED_DEPLOYMENT_PROFILE,
  SELECTED_DEPLOYMENT_PROFILE_DIGEST,
} from "../src/deployment-profile.js";

let generatedCatalogueFixture: DeploymentManifestFraudProofCatalogueIdentity;

const CATALOGUE_FIXTURE_SCRIPT_HASH =
  "bddf4b5c833decbf82201931cffc54f7c7dc51e4e6743a25a95aa2c0";

const catalogueFixtureKey = (categoryId: string): Buffer =>
  Buffer.concat([Buffer.from([0x44]), Buffer.from(categoryId, "hex")]);

beforeAll(async () => {
  const value = Buffer.from(`581c${CATALOGUE_FIXTURE_SCRIPT_HASH}`, "hex");
  const trie = await Trie.fromList(
    DEPLOYMENT_MANIFEST_FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER.map(
      (categoryName) => ({
        key: catalogueFixtureKey(
          DEPLOYMENT_MANIFEST_FRAUD_PROOF_CATALOGUE_CATEGORY_IDS[categoryName],
        ),
        value,
      }),
    ),
  );
  const categories: Record<
    string,
    DeploymentManifestFraudProofCatalogueIdentity["categories"][keyof DeploymentManifestFraudProofCatalogueIdentity["categories"]]
  > = {};
  for (const categoryName of DEPLOYMENT_MANIFEST_FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER) {
    const categoryId =
      DEPLOYMENT_MANIFEST_FRAUD_PROOF_CATALOGUE_CATEGORY_IDS[categoryName];
    const proof = await trie.prove(catalogueFixtureKey(categoryId));
    categories[categoryName] = {
      categoryId,
      scriptHash: CATALOGUE_FIXTURE_SCRIPT_HASH,
      membershipProofCbor: proof.toCBOR().toString("hex"),
    };
  }
  generatedCatalogueFixture = {
    root: Buffer.from(trie.hash).toString("hex"),
    categories:
      categories as DeploymentManifestFraudProofCatalogueIdentity["categories"],
  };
});

export const catalogueFixture =
  (): DeploymentManifestFraudProofCatalogueIdentity =>
    generatedCatalogueFixture;

export const identityInput = () => ({
  schemaVersion: MIDGARD_DEPLOYMENT_MANIFEST_SCHEMA_VERSION,
  consensusProfile: MIDGARD_CONSENSUS_PROFILE,
  consensusProfileDigest: MIDGARD_CONSENSUS_PROFILE_DIGEST,
  deploymentProfile: SELECTED_DEPLOYMENT_PROFILE,
  deploymentProfileDigest: SELECTED_DEPLOYMENT_PROFILE_DIGEST,
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
  artifacts: {},
  steps: {},
  validationDispute: {},
  l1Finality: DEPLOYMENT_MANIFEST_L1_FINALITY,
  economics: DEPLOYMENT_MANIFEST_ECONOMICS_BY_PROFILE["bounded-acceptance-v1"],
  availabilityChallenge: {
    responseClasses: {
      smallPayloadMaxBytes: 65_536,
      smallResponseWindowMs:
        SELECTED_DEPLOYMENT_PROFILE.timing.da_small_response_window_ms,
      fullPayloadMaxBytes: 67_108_864,
      fullResponseWindowMs:
        SELECTED_DEPLOYMENT_PROFILE.timing.da_full_response_window_ms,
    },
    responseGeometry: {
      chunkByteLength: 14_020,
      trancheByteLength: 4_194_304,
      maxTrancheCount: 16,
    },
    ...daBondManifestAmounts(),
    challengerBondLovelace: 10_000_000_000,
    maxOpenFeeLovelace: 500_000,
    maxPublicationFeeLovelace: 500_000,
    maxSettlementFeeLovelace: 500_000,
    maxCloseFeeLovelace: 1_000_000,
    maxTimeoutFeeLovelace: 1_200_000,
  },
});

// A complete document built independently of the node parser and SDK builders.
export const finalizedManifest = () => {
  const referenceOutRefs = new Map<
    string,
    { txHash: string; outputIndex: number }
  >(
    Object.values(DEPLOYMENT_MANIFEST_REFERENCE_SCRIPT_CONTRACT_BY_ROLE).map(
      (name, outputIndex) => [name, { txHash: "22".repeat(32), outputIndex }],
    ),
  );
  const policy = { type: "Native" as const, script: "820501" };
  const policyId = validatorToScriptHash(policy);
  const snapshot = {
    minFeeA: "44",
    minFeeB: "155381",
    priceMemory: { numerator: "577", denominator: "10000" },
    priceSteps: { numerator: "721", denominator: "10000000" },
    coinsPerUtxoByte: "4310",
    collateralPercentage: "150",
    maxCollateralInputs: "3",
    maxTxSize: "16384",
    maxValueSize: "5000",
    maxTxExUnits: { memory: "16500000", steps: "10000000000" },
    referenceScriptFee: {
      base: { numerator: "15", denominator: "1" },
      range: "25600",
      multiplier: { numerator: "6", denominator: "5" },
      maximumSizeBytes: "204800",
    },
  };
  const document = {
    ...identityInput(),
    cardanoProtocolParameters: {
      snapshot,
      digest: computeDeploymentManifestJsonDigest(snapshot),
    },
    genesis: { headerHash: "00".repeat(28), utxoSetDigest: "33".repeat(32) },
    hubOracleOneShot: {
      txHash: "11".repeat(32),
      outputIndex: 0,
      outRef: `${"11".repeat(32)}#0`,
      status: "consumed_by_init",
    },
    referenceScriptAuthPolicy: {
      policyId,
      nativeScript: {
        type: policy.type,
        cborHex: policy.script,
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
    contracts: Object.fromEntries(
      DEPLOYMENT_MANIFEST_CONTRACT_NAMES.map((name) => [
        name,
        {
          refScriptUTxO: referenceOutRefs.get(name) ?? null,
          contract:
            name === "referenceScriptAuthMint"
              ? { type: policy.type, cborHex: policy.script }
              : { type: "PlutusV3", cborHex: "01" },
          scriptHash:
            name === "referenceScriptAuthMint"
              ? policyId
              : CATALOGUE_FIXTURE_SCRIPT_HASH,
          ...(name === "depositMint" || name === "withdrawalMint"
            ? {
                eventHistoryRecipe: {
                  kind:
                    name === "depositMint"
                      ? ("Deposit" as const)
                      : ("Withdrawal" as const),
                  hubPolicyId: CATALOGUE_FIXTURE_SCRIPT_HASH,
                  initializationNonce: {
                    txHash: "11".repeat(32),
                    outputIndex: 0,
                  },
                  protectionDurationMs: "2000",
                  bounds: {
                    inlineLimitBytes: "512",
                    maxPayloadBytes: "5000",
                    maxPayloadNodes: "512",
                  },
                },
              }
            : {}),
          ...(name === "fraudProofFabricatedDeposit" ||
          name === "fraudProofFabricatedWithdrawal"
            ? {
                eventHistoryRetentionAddress: credentialToAddress("Preview", {
                  type: "Script",
                  hash: "ee".repeat(28),
                }),
                eventHistoryBounds: {
                  inlineLimitBytes: "512",
                  maxPayloadBytes: "5000",
                  maxPayloadNodes: "512",
                },
              }
            : {}),
          ...(name === "fraudProofTransitionTrace"
            ? {
                eventHistoryBounds: {
                  inlineLimitBytes: "512",
                  maxPayloadBytes: "5000",
                  maxPayloadNodes: "512",
                },
                eventHistoryRetentionAddresses: {
                  deposit: credentialToAddress("Preview", {
                    type: "Script",
                    hash: "ee".repeat(28),
                  }),
                  withdrawal: credentialToAddress("Preview", {
                    type: "Script",
                    hash: "ef".repeat(28),
                  }),
                },
              }
            : {}),
          ...(name === "fraudProofCatalogueMint"
            ? { fraudProofCatalogue: catalogueFixture() }
            : {}),
        },
      ]),
    ),
    referenceScripts: Object.fromEntries(
      Object.entries(DEPLOYMENT_MANIFEST_REFERENCE_SCRIPT_CONTRACT_BY_ROLE).map(
        ([role, name]) => {
          const outRef = referenceOutRefs.get(name)!;
          const tokenName =
            DEPLOYMENT_MANIFEST_REFERENCE_SCRIPT_TOKEN_NAMES[
              role as keyof typeof DEPLOYMENT_MANIFEST_REFERENCE_SCRIPT_TOKEN_NAMES
            ];
          return [
            role,
            {
              status: "confirmed",
              roleUnit: policyId + Buffer.from(tokenName).toString("hex"),
              scriptHash:
                name === "referenceScriptAuthMint"
                  ? policyId
                  : CATALOGUE_FIXTURE_SCRIPT_HASH,
              outRef: `${outRef.txHash}#${outRef.outputIndex}`,
            },
          ];
        },
      ),
    ),
    da: {
      committeeVkeys: ["44".repeat(32)],
      committeeSignersHash: bytesToHex(
        blake2b(hexToBytes("44".repeat(32)), { dkLen: 32 }),
      ),
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
    artifacts: { blueprintHash: "55".repeat(32) },
    steps: {
      prepareHubOracleNonce: { status: "complete" },
      deployNodeRuntimeReferenceScripts: { status: "complete" },
      initProtocol: { status: "complete" },
      availabilityRegistration: { status: "complete" },
      phasRegistration: { status: "pending" },
      operatorRegistration: { status: "pending" },
      operatorActivation: { status: "pending" },
    },
    validationDispute: {
      version: MIDGARD_CONSENSUS_PROFILE.validationDisputeVersion,
      responseWindowMs:
        MIDGARD_CONSENSUS_PROFILE.limits.validationDisputeResponseWindowMs,
      maxBisectionRounds:
        MIDGARD_CONSENSUS_PROFILE.limits.maxValidationBisectionRounds,
      maturityMs: MIDGARD_CONSENSUS_PROFILE.limits.blockMaturityMs,
    },
  };
  return { ...document, manifestId: computeDeploymentManifestId(document) };
};
