import { createHash, generateKeyPairSync } from "node:crypto";

import {
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
} from "@al-ft/midgard-core/deployment-manifest-identity";
import {
  daBondManifestAmounts,
  SELECTED_DEPLOYMENT_PROFILE,
  SELECTED_DEPLOYMENT_PROFILE_DIGEST,
} from "@al-ft/midgard-core/deployment-profile";
import { validatorToScriptHash } from "@lucid-evolution/lucid";

import { type WatcherDeploymentIdentityPolicy } from "../../src/runtime/deployment-identity.js";
import {
  canonicalFraudProofCatalogueFixture,
  positionalContractScriptCbor,
  positionalContractScriptHash,
} from "../canonical-fraud-proof-catalogue.js";
import { addWatcherHistoryFixtureMetadata } from "../support/deployment-authority-fixture.js";

const NATIVE_SCRIPT_CBOR = "820501";

export const NATIVE_SCRIPT_HASH = validatorToScriptHash({
  type: "Native",
  script: NATIVE_SCRIPT_CBOR,
});

const DA_VKEY = "44".repeat(32);

const DA_SIGNERS_HASH =
  "0395256ce5d90f07504b614b9e70e29a06fdd69cef6b01f6018615164125a5c5";

export const BLUEPRINT_HASH = "55".repeat(32);

export const RULE_BUNDLE_COMMITMENT = "77".repeat(32);

export type MutableRecord = Record<string, any>;

export const referenceOutRefByContract = new Map<
  string,
  { txHash: string; outputIndex: number }
>(
  Object.values(DEPLOYMENT_MANIFEST_REFERENCE_SCRIPT_CONTRACT_BY_ROLE).map(
    (contractName, outputIndex) => [
      contractName,
      { txHash: "22".repeat(32), outputIndex },
    ],
  ),
);

export const canonicalIdentity = (): MutableRecord => {
  const contracts = Object.fromEntries(
    DEPLOYMENT_MANIFEST_CONTRACT_NAMES.map((contractName) => {
      const scriptName =
        contractName === "depositSpend"
          ? "depositMint"
          : contractName === "withdrawalSpend"
            ? "withdrawalMint"
            : contractName;
      const contractScriptCbor = positionalContractScriptCbor(scriptName);
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
              : positionalContractScriptHash(scriptName),
        },
      ];
    }),
  ) as MutableRecord;
  addWatcherHistoryFixtureMetadata(contracts, {
    txHash: "11".repeat(32),
    outputIndex: 0,
  });
  contracts.fraudProofCatalogueMint.fraudProofCatalogue =
    canonicalFraudProofCatalogueFixture(contracts);
  const referenceScripts = Object.fromEntries(
    Object.entries(DEPLOYMENT_MANIFEST_REFERENCE_SCRIPT_CONTRACT_BY_ROLE).map(
      ([role, contractName]) => {
        const outRef = referenceOutRefByContract.get(contractName);
        if (outRef === undefined) {
          throw new Error("Missing test reference outref");
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
  const parameters = {
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
  return {
    schemaVersion: "midgard-deployment-manifest-v1",
    consensusProfile: MIDGARD_CONSENSUS_PROFILE,
    consensusProfileDigest: MIDGARD_CONSENSUS_PROFILE_DIGEST,
    deploymentProfile: SELECTED_DEPLOYMENT_PROFILE,
    deploymentProfileDigest: SELECTED_DEPLOYMENT_PROFILE_DIGEST,
    network: "Preprod",
    cardanoProtocolParameters: {
      snapshot: parameters,
      digest: computeDeploymentManifestJsonDigest(parameters),
    },
    genesis: {
      headerHash: "00".repeat(28),
      utxoSetDigest: computeDeploymentManifestJsonDigest([]),
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
    artifacts: {
      blueprintHash: BLUEPRINT_HASH,
    },
    steps: Object.fromEntries(
      DEPLOYMENT_MANIFEST_STEP_NAMES.map((stepName) => [
        stepName,
        {
          status:
            stepName === "prepareHubOracleNonce" ||
            stepName === "deployNodeRuntimeReferenceScripts" ||
            stepName === "initProtocol" ||
            stepName === "availabilityRegistration"
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
  };
};

export const withManifestId = (identity: MutableRecord): MutableRecord => ({
  ...identity,
  manifestId: computeDeploymentManifestId(identity),
});

export const makeTrustRoot = () => {
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

export const appliedScriptHashes = (
  manifest: MutableRecord,
): Record<string, string> =>
  Object.fromEntries(
    DEPLOYMENT_MANIFEST_CONTRACT_NAMES.map((contractName) => [
      contractName,
      manifest.contracts[contractName].scriptHash,
    ]),
  );

export const referenceScriptPolicy = (
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

export const cataloguePolicy = (manifest: MutableRecord) => {
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
