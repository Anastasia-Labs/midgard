import {
  DA_RUNTIME_MANIFEST_V1_SCHEMA_VERSION,
  DA_TRANSPORT_LIMITS_V1,
  DA_TRANSPORT_V1_PROTOCOL_VERSION,
} from "@al-ft/midgard-core/da-transport";
import { DEPLOYMENT_MANIFEST_V1_ECONOMICS_BY_PROFILE } from "@al-ft/midgard-core/deployment-manifest-identity-v1";
import {
  REFERENCE_SCRIPT_AUTH_TOKEN_NAMES,
  type ReferenceScriptAuthPolicyDeploymentInfo,
} from "@al-ft/midgard-sdk";
import { validatorToScriptHash } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import {
  buildContractDeploymentInfoFromContracts,
  buildDeploymentManifestV1,
  type DeploymentManifestV1IdentityContext,
} from "@/commands/contract-deployment-info.js";
import {
  computeDeploymentManifestV1DaCommitteeSignersHash,
  computeDeploymentManifestV1JsonDigest,
  DEPLOYMENT_MANIFEST_V1_REFERENCE_SCRIPT_CONTRACT_BY_ROLE,
  type DeploymentManifestV1Value,
  normalizeDeploymentManifestV1JsonValue,
  parseDeploymentManifestV1Value,
} from "@/deployment-manifest-v1.js";
import { AlwaysSucceedsContract } from "@/services/always-succeeds.js";
import {
  buildFraudProofCatalogueDeploymentInfo,
  fraudProofsToIndexedValidators,
} from "@/transactions/initialization.js";

import { TEST_AVAILABILITY_CHALLENGE_V1 } from "./availability-challenge-v1.js";
import { TEST_CARDANO_PROTOCOL_PARAMETERS_V1 } from "./cardano-protocol-parameters-v1.js";

const DA_VKEY = "44".repeat(32);
const CARDANO_PARAMETERS = TEST_CARDANO_PROTOCOL_PARAMETERS_V1;
const IDENTITY: DeploymentManifestV1IdentityContext = {
  availabilityChallenge: TEST_AVAILABILITY_CHALLENGE_V1,
  economics:
    DEPLOYMENT_MANIFEST_V1_ECONOMICS_BY_PROFILE["bounded-acceptance-v1"],
  cardanoProtocolParameters: {
    snapshot: CARDANO_PARAMETERS,
    digest: computeDeploymentManifestV1JsonDigest(CARDANO_PARAMETERS),
  },
  genesis: {
    headerHash: "00".repeat(28),
    utxoSetDigest: computeDeploymentManifestV1JsonDigest(
      normalizeDeploymentManifestV1JsonValue([]),
    ),
  },
  da: {
    committeeVkeys: [DA_VKEY],
    committeeSignersHash: computeDeploymentManifestV1DaCommitteeSignersHash([
      DA_VKEY,
    ]),
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
  proofEvidence: { digest: null, blueprintHash: "55".repeat(32) },
};

export const makeFinalizedDeploymentManifestV1Fixture =
  async (): Promise<DeploymentManifestV1Value> => {
    const contracts = await Effect.runPromise(
      AlwaysSucceedsContract.pipe(
        Effect.provide(AlwaysSucceedsContract.Default),
      ),
    );
    const nativeScriptCbor = `8200581c${"00".repeat(28)}`;
    const referenceScriptAuthPolicy: ReferenceScriptAuthPolicyDeploymentInfo = {
      policyId: validatorToScriptHash({
        type: "Native",
        script: nativeScriptCbor,
      }),
      nativeScript: {
        type: "Native",
        cborHex: nativeScriptCbor,
        expiresAtSlot: 0,
        expiresAtUnixTime: 0,
        timelockDurationMs: 1,
      },
      tokenNames: REFERENCE_SCRIPT_AUTH_TOKEN_NAMES,
      postTimelockAudit: { required: true, rule: "test fixture" },
    };
    const referenceScriptOutRefs = new Map(
      Object.values(
        DEPLOYMENT_MANIFEST_V1_REFERENCE_SCRIPT_CONTRACT_BY_ROLE,
      ).map((contractName, index) => [
        contractName,
        {
          txHash: (index + 1).toString(16).padStart(2, "0").repeat(32),
          outputIndex: 0,
        },
      ]),
    );
    const catalogue = await Effect.runPromise(
      buildFraudProofCatalogueDeploymentInfo(
        fraudProofsToIndexedValidators(contracts.fraudProofs),
      ),
    );
    return parseDeploymentManifestV1Value(
      buildDeploymentManifestV1(
        buildContractDeploymentInfoFromContracts(
          contracts,
          referenceScriptAuthPolicy,
          referenceScriptOutRefs,
          catalogue,
        ),
        {
          network: "Preprod",
          ...IDENTITY,
          referenceScriptDeployAddress: "addr_test1reference",
          hubOracleOneShotTxHash: "ab".repeat(32),
          hubOracleOneShotOutputIndex: 0,
          hubOracleOneShotStatus: "consumed_by_init",
          steps: { initProtocol: { status: "complete" } },
        },
      ),
    );
  };
