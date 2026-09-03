import {
  DA_RUNTIME_MANIFEST_SCHEMA_VERSION,
  DA_TRANSPORT_LIMITS,
  DA_TRANSPORT_PROTOCOL_VERSION,
} from "@al-ft/midgard-core/da-transport";
import { DEPLOYMENT_MANIFEST_ECONOMICS_BY_PROFILE } from "@al-ft/midgard-core/deployment-manifest-identity";
import {
  REFERENCE_SCRIPT_AUTH_TOKEN_NAMES,
  type ReferenceScriptAuthPolicyDeploymentInfo,
} from "@al-ft/midgard-sdk";
import { validatorToScriptHash } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import {
  buildContractDeploymentInfoFromContracts,
  buildDeploymentManifest,
  type DeploymentManifestIdentityContext,
} from "../../src/commands/contract-deployment-info.js";
import {
  computeDeploymentManifestDaCommitteeSignersHash,
  computeDeploymentManifestJsonDigest,
  DEPLOYMENT_MANIFEST_REFERENCE_SCRIPT_CONTRACT_BY_ROLE,
  type DeploymentManifestValue,
  normalizeDeploymentManifestJsonValue,
  parseDeploymentManifestValue,
} from "../../src/deployment-manifest.js";
import {
  buildFraudProofCatalogueDeploymentInfo,
  fraudProofsToIndexedValidators,
} from "../../src/transactions/initialization.js";
import { TEST_AVAILABILITY_CHALLENGE } from "./availability-challenge.js";
import { TEST_CARDANO_PROTOCOL_PARAMETERS } from "./cardano-protocol-parameters.js";
import { loadRealMidgardContractsForTest } from "./real-midgard-contracts.js";

const DA_VKEY = "44".repeat(32);
const CARDANO_PARAMETERS = TEST_CARDANO_PROTOCOL_PARAMETERS;
const IDENTITY: DeploymentManifestIdentityContext = {
  availabilityChallenge: TEST_AVAILABILITY_CHALLENGE,
  economics: DEPLOYMENT_MANIFEST_ECONOMICS_BY_PROFILE["bounded-acceptance-v1"],
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
  proofEvidence: { digest: null, blueprintHash: "55".repeat(32) },
};

export const makeFinalizedDeploymentManifestFixture =
  async (): Promise<DeploymentManifestValue> => {
    const contracts = await loadRealMidgardContractsForTest({
      txHash: "ab".repeat(32),
      outputIndex: 0,
    });
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
      Object.values(DEPLOYMENT_MANIFEST_REFERENCE_SCRIPT_CONTRACT_BY_ROLE).map(
        (contractName, index) => [
          contractName,
          {
            txHash: (index + 1).toString(16).padStart(2, "0").repeat(32),
            outputIndex: 0,
          },
        ],
      ),
    );
    const catalogue = await Effect.runPromise(
      buildFraudProofCatalogueDeploymentInfo(
        fraudProofsToIndexedValidators(contracts.fraudProofs),
      ),
    );
    return parseDeploymentManifestValue(
      buildDeploymentManifest(
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
