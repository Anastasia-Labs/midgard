import { decodeMidgardNativeScript } from "@al-ft/midgard-core/codec/native-script";
import {
  MIDGARD_CONSENSUS_PROFILE,
  MIDGARD_CONSENSUS_PROFILE_DIGEST,
  MIDGARD_DEPLOYMENT_MANIFEST_SCHEMA_VERSION,
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
  parseDeploymentManifestEventHistoryRecipe,
} from "@al-ft/midgard-core/deployment-manifest-identity";
import {
  daBondManifestAmounts,
  SELECTED_DEPLOYMENT_PROFILE,
  SELECTED_DEPLOYMENT_PROFILE_DIGEST,
} from "@al-ft/midgard-core/deployment-profile";
import { validatorToScriptHash } from "@lucid-evolution/lucid";
import { blake2b } from "@noble/hashes/blake2.js";
import { bytesToHex, hexToBytes } from "@noble/hashes/utils.js";

import {
  buildCanonicalFraudProofCatalogueFixture,
  isRecord,
  requireExactKeys,
  requireFixtureContract,
  requireFixtureContracts,
  requireFixtureOutRef,
  requireFixtureScript,
  requireFixtureScriptHash,
  requireNullRefScriptUTxO,
} from "./deployment-fixture.build-canonical-fraud-proof-catalogue-fixture.js";

export const buildDaDeploymentFixture = async (
  fixture: Record<string, unknown>,
): Promise<Record<string, unknown>> => {
  const artifacts = fixture.artifacts;
  if (!isRecord(artifacts)) {
    throw new Error("DA deployment fixture artifacts must be an object");
  }
  requireExactKeys(
    artifacts,
    ["blueprintHash"],
    "DA deployment fixture artifacts",
  );
  const blueprintHash = artifacts.blueprintHash;
  if (
    typeof blueprintHash !== "string" ||
    !/^[0-9a-f]{64}$/u.test(blueprintHash)
  ) {
    throw new Error(
      "DA deployment fixture artifacts.blueprintHash must be 32-byte lowercase hex",
    );
  }
  const fixtureContracts = requireFixtureContracts(fixture);
  const referenceScriptContractNames = new Set<string>(
    Object.values(DEPLOYMENT_MANIFEST_REFERENCE_SCRIPT_CONTRACT_BY_ROLE),
  );
  const contracts = Object.fromEntries(
    DEPLOYMENT_MANIFEST_CONTRACT_NAMES.map((contractName) => {
      const { source, metadata } = requireFixtureContract(
        fixtureContracts[contractName],
        contractName,
      );
      const sourceContract = requireFixtureScript(
        source.contract,
        contractName,
      );
      const sourceScriptHash = requireFixtureScriptHash(
        source.scriptHash,
        contractName,
      );
      const derivedScriptHash = validatorToScriptHash({
        type: sourceContract.type,
        script: sourceContract.cborHex,
      });
      if (derivedScriptHash !== sourceScriptHash) {
        throw new Error(
          `DA deployment fixture contracts.${contractName}.scriptHash mismatch: expected ${derivedScriptHash}`,
        );
      }
      const refScriptUTxO = referenceScriptContractNames.has(contractName)
        ? requireFixtureOutRef(source.refScriptUTxO, contractName)
        : requireNullRefScriptUTxO(source.refScriptUTxO, contractName);
      return [
        contractName,
        {
          refScriptUTxO,
          contract: sourceContract,
          scriptHash: sourceScriptHash,
          ...metadata,
        },
      ];
    }),
  ) as Record<string, Record<string, unknown>>;
  const referenceScriptAuthContract = contracts.referenceScriptAuthMint!;
  const referenceScriptAuthScript = referenceScriptAuthContract.contract as {
    readonly type: string;
    readonly cborHex: string;
  };
  if (referenceScriptAuthScript.type !== "Native") {
    throw new Error(
      "DA deployment fixture contracts.referenceScriptAuthMint.contract.type must be Native",
    );
  }
  const referenceScriptAuthPolicyId =
    referenceScriptAuthContract.scriptHash as string;
  const nativeScriptCbor = referenceScriptAuthScript.cborHex;
  const { script: nativeScript } = decodeMidgardNativeScript(
    Buffer.from(nativeScriptCbor, "hex"),
  );
  if (nativeScript.type !== "before") {
    throw new Error(
      "DA codec fixture requires a historical time-only auth policy",
    );
  }
  const expiresAtSlot = Number(nativeScript.slot);
  contracts.fraudProofCatalogueMint = {
    ...contracts.fraudProofCatalogueMint,
    fraudProofCatalogue: await buildCanonicalFraudProofCatalogueFixture({
      mintItemNonCanonical: contracts.fraudProofMintItemNonCanonical!
        .scriptHash as string,
      doubleSpend: contracts.fraudProofDoubleSpend!.scriptHash as string,
      nonExistentInput: contracts.fraudProofNonExistentInput!
        .scriptHash as string,
      nonExistentInputNoIndex: contracts.fraudProofNonExistentInputNoIndex!
        .scriptHash as string,
      invalidRange: contracts.fraudProofInvalidRange!.scriptHash as string,
      transitionTrace: contracts.fraudProofTransitionTrace!
        .scriptHash as string,
      zeroInput: contracts.fraudProofZeroInput!.scriptHash as string,
      validationTraceDispute: contracts.validationTraceDispute!
        .scriptHash as string,
      daHashPreimage: contracts.fraudProofDaHashPreimage!.scriptHash as string,
      noReferenceInput: contracts.fraudProofNoReferenceInput!
        .scriptHash as string,
      referenceInputNoIdx: contracts.fraudProofReferenceInputNoIdx!
        .scriptHash as string,
      invalidSignature: contracts.fraudProofInvalidSignature!
        .scriptHash as string,
      fabricatedDeposit: contracts.fraudProofFabricatedDeposit!
        .scriptHash as string,
      fabricatedWithdrawal: contracts.fraudProofFabricatedWithdrawal!
        .scriptHash as string,
      nativeScriptDecoding: contracts.fraudProofNativeScriptDecoding!
        .scriptHash as string,
      missingSignature: contracts.fraudProofMissingSignature!
        .scriptHash as string,
      withdrawnReferenceInput: contracts.fraudProofWithdrawnReferenceInput!
        .scriptHash as string,
      canonicalDecodability: contracts.fraudProofCanonicalDecodability!
        .scriptHash as string,
      committedFieldShape: contracts.fraudProofCommittedFieldShape!
        .scriptHash as string,
      minFee: contracts.fraudProofMinFee!.scriptHash as string,
      withdrawalMistag: contracts.fraudProofWithdrawalMistag!
        .scriptHash as string,
      doubleWithdraw: contracts.fraudProofDoubleWithdraw!.scriptHash as string,
      l2TxMistag: contracts.fraudProofL2TxMistag!.scriptHash as string,
      withdrawnInput: contracts.fraudProofWithdrawnInput!.scriptHash as string,
      valueNotPreserved: contracts.fraudProofValueNotPreserved!
        .scriptHash as string,
      inputSetUniqueness: contracts.fraudProofInputSetUniqueness!
        .scriptHash as string,
      mintAuthorization: contracts.fraudProofMintAuthorization!
        .scriptHash as string,
      networkId: contracts.fraudProofNetworkId!.scriptHash as string,
      nativeScriptInvalid: contracts.fraudProofNativeScriptInvalid!
        .scriptHash as string,
      minAda: contracts.fraudProofMinAda!.scriptHash as string,
      fieldPreimageLengthMismatch: contracts
        .fraudProofFieldPreimageLengthMismatch!.scriptHash as string,
      fieldItemWidthIllegal: contracts.fraudProofFieldItemWidthIllegal!
        .scriptHash as string,
      witnessScriptDecoding: contracts.fraudProofWitnessScriptDecoding!
        .scriptHash as string,
      scriptIntegrityHashMissing: contracts
        .fraudProofScriptIntegrityHashMissing!.scriptHash as string,
      transactionOutputNonCanonical: contracts
        .fraudProofTransactionOutputNonCanonical!.scriptHash as string,
      resolvedOutputNonCanonical: contracts
        .fraudProofResolvedOutputNonCanonical!.scriptHash as string,
      mintDeclaredAssetLimit: contracts.fraudProofMintDeclaredAssetLimit!
        .scriptHash as string,
      spendInputSignerMissing: contracts.fraudProofSpendInputSignerMissing!
        .scriptHash as string,
      protectedOutputSignerMissing: contracts
        .fraudProofProtectedOutputSignerMissing!.scriptHash as string,
      observersForbiddenOnUntaggedNetwork: contracts
        .fraudProofObserversForbiddenOnUntaggedNetwork!.scriptHash as string,
      observerOrderInvalid: contracts.fraudProofObserverOrderInvalid!
        .scriptHash as string,
      redeemerCanonicity: contracts.fraudProofRedeemerCanonicity!
        .scriptHash as string,
      outputReferenceScriptDecoding: contracts
        .fraudProofOutputReferenceScriptDecoding!.scriptHash as string,
      executionSourceScriptDecoding: contracts
        .fraudProofExecutionSourceScriptDecoding!.scriptHash as string,
      receivePurposeLanguage: contracts.fraudProofReceivePurposeLanguage!
        .scriptHash as string,
      unusedScriptWitness: contracts.fraudProofUnusedScriptWitness!
        .scriptHash as string,
      missingScriptSource: contracts.fraudProofMissingScriptSource!
        .scriptHash as string,
      missingRedeemer: contracts.fraudProofMissingRedeemer!
        .scriptHash as string,
      unusedRedeemer: contracts.fraudProofUnusedRedeemer!.scriptHash as string,
      executionNativeScriptInvalid: contracts
        .fraudProofExecutionNativeScriptInvalid!.scriptHash as string,
      scriptIntegrityHashMismatch: contracts
        .fraudProofScriptIntegrityHashMismatch!.scriptHash as string,
      distinctAssetAccumulationLimit: contracts
        .fraudProofDistinctAssetAccumulationLimit!.scriptHash as string,
    }),
  };
  const referenceScripts = Object.fromEntries(
    Object.entries(DEPLOYMENT_MANIFEST_REFERENCE_SCRIPT_CONTRACT_BY_ROLE).map(
      ([role, contractName]) => {
        const contract = contracts[contractName]!;
        const outRef = contract.refScriptUTxO as {
          readonly txHash: string;
          readonly outputIndex: number;
        };
        const tokenName =
          DEPLOYMENT_MANIFEST_REFERENCE_SCRIPT_TOKEN_NAMES[
            role as keyof typeof DEPLOYMENT_MANIFEST_REFERENCE_SCRIPT_TOKEN_NAMES
          ];
        return [
          role,
          {
            status: "confirmed",
            roleUnit:
              referenceScriptAuthPolicyId +
              Buffer.from(tokenName, "utf8").toString("hex"),
            scriptHash: contract.scriptHash,
            outRef: `${outRef.txHash}#${outRef.outputIndex.toString()}`,
          },
        ];
      },
    ),
  );
  const committeeVkey = "01".repeat(32);
  const cardanoProtocolParameterSnapshot = {
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
  const historyInitializationNonce = parseDeploymentManifestEventHistoryRecipe(
    contracts.depositMint!.eventHistoryRecipe,
  ).initializationNonce;
  const identityInput = {
    schemaVersion: MIDGARD_DEPLOYMENT_MANIFEST_SCHEMA_VERSION,
    consensusProfile: MIDGARD_CONSENSUS_PROFILE,
    consensusProfileDigest: MIDGARD_CONSENSUS_PROFILE_DIGEST,
    deploymentProfile: SELECTED_DEPLOYMENT_PROFILE,
    deploymentProfileDigest: SELECTED_DEPLOYMENT_PROFILE_DIGEST,
    network: "Preprod",
    cardanoProtocolParameters: {
      snapshot: cardanoProtocolParameterSnapshot,
      digest: computeDeploymentManifestJsonDigest(
        cardanoProtocolParameterSnapshot,
      ),
    },
    genesis: {
      headerHash: "00".repeat(28),
      utxoSetDigest:
        "4f53cda18c2baa0c0354bb5f9a3ecbe5ed12ab4d8e11ba873c2f11161202b945",
    },
    createdAt: "2026-07-24T00:00:00.000Z",
    updatedAt: "2026-07-24T00:00:00.000Z",
    referenceScriptDeployAddress: "addr_test1reference",
    hubOracleOneShot: {
      txHash: historyInitializationNonce.txHash,
      outputIndex: historyInitializationNonce.outputIndex,
      outRef: `${historyInitializationNonce.txHash}#${historyInitializationNonce.outputIndex.toString()}`,
      status: "consumed_by_init",
    },
    referenceScriptAuthPolicy: {
      policyId: referenceScriptAuthPolicyId,
      nativeScript: {
        type: "Native",
        cborHex: nativeScriptCbor,
        expiresAtSlot,
        expiresAtUnixTime: expiresAtSlot,
        timelockDurationMs: 1,
      },
      tokenNames: DEPLOYMENT_MANIFEST_REFERENCE_SCRIPT_TOKEN_NAMES,
      postTimelockAudit: {
        required: true,
        rule: "fixture audit",
      },
    },
    contracts,
    referenceScripts,
    da: {
      committeeVkeys: [committeeVkey],
      committeeSignersHash: bytesToHex(
        blake2b(hexToBytes(committeeVkey), { dkLen: 32 }),
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
    artifacts: {
      blueprintHash,
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
    l1Finality: DEPLOYMENT_MANIFEST_L1_FINALITY,
    economics:
      DEPLOYMENT_MANIFEST_ECONOMICS_BY_PROFILE["bounded-acceptance-v1"],
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
      challengerBondLovelace: 12_000_000_000,
      maxOpenFeeLovelace: 2_000_000,
      maxPublicationFeeLovelace: 2_000_000,
      maxSettlementFeeLovelace: 2_000_000,
      maxCloseFeeLovelace: 2_000_000,
      maxTimeoutFeeLovelace: 3_000_000,
    },
  };
  return {
    ...identityInput,
    manifestId: computeDeploymentManifestId(identityInput),
  };
};
