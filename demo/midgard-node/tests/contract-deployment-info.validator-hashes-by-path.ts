import {
  DA_RUNTIME_MANIFEST_SCHEMA_VERSION,
  DA_TRANSPORT_LIMITS,
  DA_TRANSPORT_PROTOCOL_VERSION,
} from "@al-ft/midgard-core/da-transport";
import { DEPLOYMENT_MANIFEST_ECONOMICS_BY_PROFILE } from "@al-ft/midgard-core/deployment-manifest-identity";
import type {
  MidgardValidators,
  ReferenceScriptAuthPolicyDeploymentInfo,
} from "@al-ft/midgard-sdk";
import { REFERENCE_SCRIPT_AUTH_TOKEN_NAMES } from "@al-ft/midgard-sdk";
import { validatorToScriptHash } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import {
  buildContractDeploymentInfoFromContracts,
  type DeploymentManifestBuildContext,
  type DeploymentManifestIdentityContext,
} from "../src/commands/contract-deployment-info.js";
import {
  computeDeploymentManifestDaCommitteeSignersHash,
  computeDeploymentManifestJsonDigest,
  DEPLOYMENT_MANIFEST_REFERENCE_SCRIPT_CONTRACT_BY_ROLE,
  normalizeDeploymentManifestJsonValue,
} from "../src/deployment-manifest.js";
import {
  buildFraudProofCatalogueDeploymentInfo,
  fraudProofsToIndexedValidators,
} from "../src/transactions/initialization.js";
import { TEST_AVAILABILITY_CHALLENGE } from "./helpers/availability-challenge.js";

export const testReferenceScriptAuthPolicy = (
  _policyId: string,
  _cborHex: string,
): ReferenceScriptAuthPolicyDeploymentInfo => ({
  policyId: validatorToScriptHash({
    type: "Native",
    script: "820500",
  }),
  nativeScript: {
    type: "Native",
    cborHex: "820500",
    expiresAtSlot: 0,
    expiresAtUnixTime: 0,
    timelockDurationMs: 1,
  },
  tokenNames: REFERENCE_SCRIPT_AUTH_TOKEN_NAMES,
  postTimelockAudit: {
    required: true,
    rule: "test fixture",
  },
});

export const ONE_SHOT_TX_HASH = "ab".repeat(32);

const TEST_DA_VKEY = "11".repeat(32);

export const TEST_CARDANO_PARAMETERS = {
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

const TEST_MANIFEST_IDENTITY_CONTEXT: DeploymentManifestIdentityContext = {
  availabilityChallenge: TEST_AVAILABILITY_CHALLENGE,
  economics: DEPLOYMENT_MANIFEST_ECONOMICS_BY_PROFILE["bounded-acceptance-v1"],
  cardanoProtocolParameters: {
    snapshot: TEST_CARDANO_PARAMETERS,
    digest: computeDeploymentManifestJsonDigest(TEST_CARDANO_PARAMETERS),
  },
  genesis: {
    headerHash: "00".repeat(28),
    utxoSetDigest: computeDeploymentManifestJsonDigest(
      normalizeDeploymentManifestJsonValue([]),
    ),
  },
  da: {
    committeeVkeys: [TEST_DA_VKEY],
    committeeSignersHash: computeDeploymentManifestDaCommitteeSignersHash([
      TEST_DA_VKEY,
    ]),
    threshold: 1,
    transportProfile: {
      protocolVersion: DA_TRANSPORT_PROTOCOL_VERSION,
      runtimeManifestSchemaVersion: DA_RUNTIME_MANIFEST_SCHEMA_VERSION,
      envelopeEncoding: "identity" as const,
      zstdLevel: 3,
      limits: DA_TRANSPORT_LIMITS,
      retentionDays: DA_TRANSPORT_LIMITS.minimumRetentionDays,
    },
  },
  artifacts: {
    blueprintHash: "22".repeat(32),
  },
};

/**
 * Every applied script in a validator bundle, keyed by its object path, and
 * the paths of members that fail closed (accessors that throw on read).
 */
export const validatorHashesByPath = (
  root: unknown,
): {
  readonly hashes: Record<string, string>;
  readonly failClosed: readonly string[];
} => {
  const hashes: Record<string, string> = {};
  const failClosed: string[] = [];
  const visit = (value: unknown, path: string): void => {
    if (typeof value !== "object" || value === null) return;
    const descriptors = Object.getOwnPropertyDescriptors(value);
    for (const field of [
      "spendingScriptHash",
      "withdrawalScriptHash",
      "policyId",
    ]) {
      const hash: unknown = descriptors[field]?.value;
      if (typeof hash === "string") hashes[`${path}.${field}`] = hash;
    }
    for (const [key, descriptor] of Object.entries(descriptors)) {
      if (key.endsWith("Script")) continue;
      if (descriptor.get !== undefined) {
        failClosed.push(`${path}.${key}`);
        continue;
      }
      visit(descriptor.value, `${path}.${key}`);
    }
  };
  visit(root, "$");
  return { hashes, failClosed };
};

export const testFraudProofCatalogue = (contracts: MidgardValidators) =>
  buildFraudProofCatalogueDeploymentInfo(
    fraudProofsToIndexedValidators(contracts.fraudProofs),
  );

export const buildFinalizedContractDeploymentInfo = (
  contracts: MidgardValidators,
  authPolicy: ReferenceScriptAuthPolicyDeploymentInfo,
) =>
  Effect.map(testFraudProofCatalogue(contracts), (fraudProofCatalogue) =>
    buildContractDeploymentInfoFromContracts(
      contracts,
      authPolicy,
      new Map(
        Object.values(
          DEPLOYMENT_MANIFEST_REFERENCE_SCRIPT_CONTRACT_BY_ROLE,
        ).map((contractName, outputIndex) => [
          contractName,
          { txHash: "33".repeat(32), outputIndex },
        ]),
      ),
      fraudProofCatalogue,
    ),
  );

export const TEST_FINALIZED_MANIFEST_BUILD_CONTEXT = {
  network: "Preprod",
  ...TEST_MANIFEST_IDENTITY_CONTEXT,
  referenceScriptDeployAddress: "addr_test1reference",
  hubOracleOneShotTxHash: ONE_SHOT_TX_HASH,
  hubOracleOneShotOutputIndex: 0,
  hubOracleOneShotStatus: "consumed_by_init",
  steps: {
    initProtocol: { status: "complete", txHash: "cd".repeat(32) },
    availabilityRegistration: { status: "complete" },
  },
} satisfies DeploymentManifestBuildContext;
