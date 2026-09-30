import {
  MIDGARD_CONSENSUS_PROFILE,
  MIDGARD_CONSENSUS_PROFILE_DIGEST,
} from "@al-ft/midgard-core/consensus-profile";
import {
  DA_RUNTIME_MANIFEST_SCHEMA_VERSION,
  DA_TRANSPORT_LIMITS,
  DA_TRANSPORT_PROTOCOL_VERSION,
} from "@al-ft/midgard-core/da-transport";
import type { DeploymentManifest } from "@al-ft/midgard-core/deployment-manifest-identity";
import {
  DEPLOYMENT_MANIFEST_ECONOMICS_BY_PROFILE,
  DEPLOYMENT_MANIFEST_L1_FINALITY,
} from "@al-ft/midgard-core/deployment-manifest-identity";
import {
  SELECTED_DEPLOYMENT_PROFILE,
  SELECTED_DEPLOYMENT_PROFILE_DIGEST,
} from "@al-ft/midgard-core/deployment-profile";
import {
  FRAUD_PROOF_CATALOGUE_CATEGORY_IDS,
  FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER,
  type FraudProofCatalogueDeploymentInfo,
  REFERENCE_SCRIPT_AUTH_TOKEN_NAMES,
  referenceScriptAuthUnit,
} from "@al-ft/midgard-sdk";
import {
  credentialToAddress,
  validatorToScriptHash,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { beforeAll } from "vitest";

import {
  computeDeploymentManifestDaCommitteeSignersHash,
  computeDeploymentManifestId,
  computeDeploymentManifestJsonDigest,
  DEPLOYMENT_MANIFEST_CONTRACT_NAMES,
  DEPLOYMENT_MANIFEST_REFERENCE_SCRIPT_CONTRACT_BY_ROLE,
  DEPLOYMENT_MANIFEST_REFERENCE_SCRIPT_ROLES,
  DEPLOYMENT_MANIFEST_SCHEMA_VERSION,
  normalizeDeploymentManifestJsonValue,
} from "../src/deployment-manifest.js";
import { buildFraudProofCatalogueDeploymentInfo } from "../src/transactions/initialization.js";
import { TEST_AVAILABILITY_CHALLENGE } from "./helpers/availability-challenge.js";

const NATIVE_SCRIPT_CBOR = "820501";

const NATIVE_SCRIPT_HASH = validatorToScriptHash({
  type: "Native",
  script: NATIVE_SCRIPT_CBOR,
});

const CONTRACT_SCRIPT_CBOR = "01";

const CONTRACT_SCRIPT_HASH = validatorToScriptHash({
  type: "PlutusV3",
  script: CONTRACT_SCRIPT_CBOR,
});

let CANONICAL_FRAUD_PROOF_CATALOGUE: FraudProofCatalogueDeploymentInfo;

beforeAll(async () => {
  CANONICAL_FRAUD_PROOF_CATALOGUE = await Effect.runPromise(
    buildFraudProofCatalogueDeploymentInfo(
      FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER.map(
        (categoryName) =>
          [
            Buffer.from(
              FRAUD_PROOF_CATALOGUE_CATEGORY_IDS[categoryName],
              "hex",
            ),
            { spendingScriptHash: CONTRACT_SCRIPT_HASH } as never,
            categoryName,
          ] as const,
      ),
    ),
  );
});

const DA_VKEY = "44".repeat(32);

export const CARDANO_PARAMETERS = {
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
} as const;

export const canonicalIdentity = (): Omit<DeploymentManifest, "manifestId"> => {
  const referenceOutRefByContract = new Map<
    string,
    { readonly txHash: string; readonly outputIndex: number }
  >(
    Object.values(DEPLOYMENT_MANIFEST_REFERENCE_SCRIPT_CONTRACT_BY_ROLE).map(
      (contractName, outputIndex) => [
        contractName,
        { txHash: "22".repeat(32), outputIndex },
      ],
    ),
  );
  const contracts: Record<string, DeploymentManifest["contracts"][string]> =
    Object.fromEntries(
      DEPLOYMENT_MANIFEST_CONTRACT_NAMES.map((contractName) => [
        contractName,
        {
          refScriptUTxO: referenceOutRefByContract.get(contractName) ?? null,
          ...(contractName === "depositMint" ||
          contractName === "withdrawalMint"
            ? {
                eventHistoryRecipe: {
                  kind:
                    contractName === "depositMint"
                      ? ("Deposit" as const)
                      : ("Withdrawal" as const),
                  hubPolicyId: CONTRACT_SCRIPT_HASH,
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
          ...(contractName === "fraudProofFabricatedDeposit" ||
          contractName === "fraudProofFabricatedWithdrawal"
            ? {
                eventHistoryBounds: {
                  inlineLimitBytes: "512",
                  maxPayloadBytes: "5000",
                  maxPayloadNodes: "512",
                },
                eventHistoryRetentionAddress: credentialToAddress("Preview", {
                  type: "Script",
                  hash: "ee".repeat(28),
                }),
              }
            : {}),
          ...(contractName === "fraudProofTransitionTrace"
            ? {
                eventHistoryBounds: {
                  inlineLimitBytes: "512",
                  maxPayloadBytes: "5000",
                  maxPayloadNodes: "512",
                },
                eventHistoryRetentionAddresses: {
                  deposit: credentialToAddress("Preview", {
                    type: "Script",
                    hash: "dd".repeat(28),
                  }),
                  withdrawal: credentialToAddress("Preview", {
                    type: "Script",
                    hash: "ee".repeat(28),
                  }),
                },
              }
            : {}),
          contract: {
            type:
              contractName === "referenceScriptAuthMint"
                ? ("Native" as const)
                : ("PlutusV3" as const),
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
    );
  contracts.fraudProofCatalogueMint = {
    ...contracts.fraudProofCatalogueMint,
    fraudProofCatalogue: CANONICAL_FRAUD_PROOF_CATALOGUE,
  };
  const referenceScripts = Object.fromEntries(
    DEPLOYMENT_MANIFEST_REFERENCE_SCRIPT_ROLES.map((role) => {
      const contractName =
        DEPLOYMENT_MANIFEST_REFERENCE_SCRIPT_CONTRACT_BY_ROLE[
          role as keyof typeof DEPLOYMENT_MANIFEST_REFERENCE_SCRIPT_CONTRACT_BY_ROLE
        ];
      return [
        role,
        {
          status: "confirmed" as const,
          roleUnit: referenceScriptAuthUnit(
            NATIVE_SCRIPT_HASH,
            role as keyof typeof REFERENCE_SCRIPT_AUTH_TOKEN_NAMES,
          ),
          scriptHash: contracts[contractName].scriptHash,
          outRef: `${referenceOutRefByContract.get(contractName)!.txHash}#${referenceOutRefByContract.get(contractName)!.outputIndex.toString()}`,
        },
      ];
    }),
  );
  const steps: DeploymentManifest["steps"] = {
    prepareHubOracleNonce: { status: "complete" },
    deployNodeRuntimeReferenceScripts: { status: "complete" },
    initProtocol: { status: "complete" },
    availabilityRegistration: { status: "complete" },
    phasRegistration: { status: "pending" },
    operatorRegistration: { status: "pending" },
    operatorActivation: { status: "pending" },
  };
  return {
    schemaVersion: DEPLOYMENT_MANIFEST_SCHEMA_VERSION,
    consensusProfile: MIDGARD_CONSENSUS_PROFILE,
    consensusProfileDigest: MIDGARD_CONSENSUS_PROFILE_DIGEST,
    deploymentProfile: SELECTED_DEPLOYMENT_PROFILE,
    deploymentProfileDigest: SELECTED_DEPLOYMENT_PROFILE_DIGEST,
    network: "Preprod",
    cardanoProtocolParameters: {
      snapshot: CARDANO_PARAMETERS,
      digest: computeDeploymentManifestJsonDigest(CARDANO_PARAMETERS),
    },
    genesis: {
      headerHash: "00".repeat(28),
      utxoSetDigest: computeDeploymentManifestJsonDigest(
        normalizeDeploymentManifestJsonValue([]),
      ),
    },
    createdAt: "2026-07-24T00:00:00.000Z",
    updatedAt: "2026-07-24T00:00:00.000Z",
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
      tokenNames: REFERENCE_SCRIPT_AUTH_TOKEN_NAMES,
      postTimelockAudit: {
        required: true,
        rule: "No authenticated reference-script output may change.",
      },
    },
    contracts,
    referenceScripts,
    da: {
      committeeVkeys: [DA_VKEY],
      committeeSignersHash: computeDeploymentManifestDaCommitteeSignersHash([
        DA_VKEY,
      ]),
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
    artifacts: {
      blueprintHash: "55".repeat(32),
    },
    validationDispute: {
      version: MIDGARD_CONSENSUS_PROFILE.validationDisputeVersion,
      responseWindowMs:
        MIDGARD_CONSENSUS_PROFILE.limits.validationDisputeResponseWindowMs,
      maxBisectionRounds:
        MIDGARD_CONSENSUS_PROFILE.limits.maxValidationBisectionRounds,
      maturityMs: MIDGARD_CONSENSUS_PROFILE.limits.blockMaturityMs,
    },
    l1Finality: DEPLOYMENT_MANIFEST_L1_FINALITY,
    economics:
      DEPLOYMENT_MANIFEST_ECONOMICS_BY_PROFILE["bounded-acceptance-v1"],
    availabilityChallenge: TEST_AVAILABILITY_CHALLENGE,
    steps,
  };
};

export const withId = (
  identity: Omit<DeploymentManifest, "manifestId">,
): DeploymentManifest => ({
  ...identity,
  manifestId: computeDeploymentManifestId(identity),
});

export const canonicalManifest = (): DeploymentManifest =>
  withId(canonicalIdentity());
