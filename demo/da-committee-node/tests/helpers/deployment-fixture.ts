import { readFile, writeFile } from "node:fs/promises";

import { Store, Trie } from "@aiken-lang/merkle-patricia-forestry";
import {
  MIDGARD_CONSENSUS_PROFILE,
  MIDGARD_CONSENSUS_PROFILE_DIGEST,
  MIDGARD_DEPLOYMENT_MANIFEST_SCHEMA_VERSION,
  MIDGARD_RELEASE_EVIDENCE_DIGEST,
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
  FRAUD_PROOF_CATALOGUE_CATEGORY_IDS,
  FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER,
  FRAUD_PROOF_CATALOGUE_ID_BYTE_COUNT,
  type FraudProofCatalogueDeploymentInfo,
  ScriptHashSchema,
} from "@al-ft/midgard-sdk";
import { Data, validatorToScriptHash } from "@lucid-evolution/lucid";
import { blake2b } from "@noble/hashes/blake2.js";
import { bytesToHex, hexToBytes } from "@noble/hashes/utils.js";

import {
  type MidgardNodeDeployment,
  parseMidgardNodeDeploymentInfo,
} from "../../src/l1/deployment.js";

const FIXTURE_URL = new URL(
  "../fixtures/da-contract-deployment-info.json",
  import.meta.url,
);

type FraudProofCatalogueCategoryName =
  (typeof FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER)[number];

const catalogueKeySchema = Data.Bytes({
  minLength: FRAUD_PROOF_CATALOGUE_ID_BYTE_COUNT,
  maxLength: FRAUD_PROOF_CATALOGUE_ID_BYTE_COUNT,
});

export const buildCanonicalFraudProofCatalogueFixture = async (
  scriptHashes: Readonly<Record<FraudProofCatalogueCategoryName, string>>,
): Promise<FraudProofCatalogueDeploymentInfo> => {
  const categories = Object.fromEntries(
    FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER.map((categoryName) => [
      categoryName,
      {
        categoryId: FRAUD_PROOF_CATALOGUE_CATEGORY_IDS[categoryName],
        scriptHash: scriptHashes[categoryName],
        membershipProofCbor: "",
      },
    ]),
  ) as FraudProofCatalogueDeploymentInfo["categories"];
  const store = new Store(undefined);
  await store.ready();
  const trie = new Trie(store);
  for (const categoryName of FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER) {
    const category = categories[categoryName];
    await trie.insert(
      Buffer.from(
        Data.to(category.categoryId as never, catalogueKeySchema),
        "hex",
      ),
      Buffer.from(
        Data.to(category.scriptHash as never, ScriptHashSchema),
        "hex",
      ),
    );
  }
  for (const categoryName of FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER) {
    const category = categories[categoryName];
    const proof = await trie.prove(
      Buffer.from(
        Data.to(category.categoryId as never, catalogueKeySchema),
        "hex",
      ),
    );
    (
      categories as Record<
        string,
        {
          categoryId: string;
          scriptHash: string;
          membershipProofCbor: string;
        }
      >
    )[categoryName] = {
      ...category,
      membershipProofCbor: proof.toCBOR().toString("hex"),
    };
  }
  if (trie.hash === null) {
    throw new Error(
      "Canonical fraud-proof catalogue fixture is unexpectedly empty",
    );
  }
  return {
    root: Buffer.from(trie.hash).toString("hex"),
    categories,
  };
};

export const readDaDeploymentFixture = async (): Promise<
  Record<string, unknown>
> => {
  const fixture = JSON.parse(await readFile(FIXTURE_URL, "utf8")) as Record<
    string,
    unknown
  >;
  return buildDaDeploymentFixture(fixture);
};

export const buildDaDeploymentFixture = async (
  fixture: Record<string, unknown>,
): Promise<Record<string, unknown>> => {
  const fixtureContracts = requireFixtureContracts(fixture);
  const referenceScriptContractNames = new Set<string>(
    Object.values(DEPLOYMENT_MANIFEST_REFERENCE_SCRIPT_CONTRACT_BY_ROLE),
  );
  const contracts = Object.fromEntries(
    DEPLOYMENT_MANIFEST_CONTRACT_NAMES.map((contractName) => {
      const source = requireFixtureContract(
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
  contracts.fraudProofCatalogueMint = {
    ...contracts.fraudProofCatalogueMint,
    fraudProofCatalogue: await buildCanonicalFraudProofCatalogueFixture({
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
      missingNativeScriptTx: contracts.fraudProofMissingNativeScriptTx!
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
      crossBlockDuplicateEvent: contracts.fraudProofCrossBlockDuplicateEvent!
        .scriptHash as string,
      l2TxMistag: contracts.fraudProofL2TxMistag!.scriptHash as string,
      withdrawnInput: contracts.fraudProofWithdrawnInput!.scriptHash as string,
      valueNotPreserved: contracts.fraudProofValueNotPreserved!
        .scriptHash as string,
      inputSetUniqueness: contracts.fraudProofInputSetUniqueness!
        .scriptHash as string,
      mintAuthorization: contracts.fraudProofMintAuthorization!
        .scriptHash as string,
      networkId: contracts.fraudProofNetworkId!.scriptHash as string,
      missingNativeScriptUtxo: contracts.fraudProofMissingNativeScriptUtxo!
        .scriptHash as string,
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
  const identityInput = {
    schemaVersion: MIDGARD_DEPLOYMENT_MANIFEST_SCHEMA_VERSION,
    consensusProfile: MIDGARD_CONSENSUS_PROFILE,
    consensusProfileDigest: MIDGARD_CONSENSUS_PROFILE_DIGEST,
    network: "Preview",
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
      txHash: "09".repeat(32),
      outputIndex: 0,
      outRef: `${"09".repeat(32)}#0`,
      status: "consumed_by_init",
    },
    referenceScriptAuthPolicy: {
      policyId: referenceScriptAuthPolicyId,
      nativeScript: {
        type: "Native",
        cborHex: nativeScriptCbor,
        expiresAtSlot: 1,
        expiresAtUnixTime: 1,
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
    proofEvidence: {
      digest: MIDGARD_RELEASE_EVIDENCE_DIGEST,
      blueprintHash: "00".repeat(32),
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
    l1Finality: DEPLOYMENT_MANIFEST_L1_FINALITY,
    economics:
      DEPLOYMENT_MANIFEST_ECONOMICS_BY_PROFILE["bounded-acceptance-v1"],
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
  return {
    ...identityInput,
    manifestId: computeDeploymentManifestId(identityInput),
  };
};

const requireFixtureContracts = (
  fixture: Record<string, unknown>,
): Record<string, Record<string, unknown>> => {
  const value = fixture.contracts;
  if (!isRecord(value)) {
    throw new Error("DA deployment fixture contracts must be an object");
  }
  const expected = new Set<string>(DEPLOYMENT_MANIFEST_CONTRACT_NAMES);
  const missing = DEPLOYMENT_MANIFEST_CONTRACT_NAMES.filter(
    (contractName) => !Object.hasOwn(value, contractName),
  );
  if (missing.length > 0) {
    throw new Error(
      `DA deployment fixture is missing contract records: ${missing.join(", ")}`,
    );
  }
  const unexpected = Object.keys(value).filter((key) => !expected.has(key));
  if (unexpected.length > 0) {
    throw new Error(
      `DA deployment fixture has unexpected contract records: ${unexpected.join(", ")}`,
    );
  }
  return value as Record<string, Record<string, unknown>>;
};

const requireFixtureContract = (
  value: unknown,
  contractName: string,
): Record<string, unknown> => {
  if (!isRecord(value)) {
    throw new Error(
      `DA deployment fixture contracts.${contractName} must be an object`,
    );
  }
  requireExactKeys(
    value,
    ["refScriptUTxO", "contract", "scriptHash"],
    `DA deployment fixture contracts.${contractName}`,
  );
  return value;
};

const requireFixtureScript = (
  value: unknown,
  contractName: string,
): {
  readonly type: "Native" | "PlutusV1" | "PlutusV2" | "PlutusV3";
  readonly cborHex: string;
} => {
  if (!isRecord(value)) {
    throw new Error(
      `DA deployment fixture contracts.${contractName}.contract must be an object`,
    );
  }
  requireExactKeys(
    value,
    ["type", "cborHex"],
    `DA deployment fixture contracts.${contractName}.contract`,
  );
  if (
    value.type !== "Native" &&
    value.type !== "PlutusV1" &&
    value.type !== "PlutusV2" &&
    value.type !== "PlutusV3"
  ) {
    throw new Error(
      `DA deployment fixture contracts.${contractName}.contract.type is unsupported`,
    );
  }
  if (
    typeof value.cborHex !== "string" ||
    value.cborHex.length === 0 ||
    value.cborHex.length % 2 !== 0 ||
    !/^[0-9a-f]+$/u.test(value.cborHex)
  ) {
    throw new Error(
      `DA deployment fixture contracts.${contractName}.contract.cborHex must be lowercase hex`,
    );
  }
  return { type: value.type, cborHex: value.cborHex };
};

const requireFixtureScriptHash = (
  value: unknown,
  contractName: string,
): string => {
  if (typeof value !== "string" || !/^[0-9a-f]{56}$/u.test(value)) {
    throw new Error(
      `DA deployment fixture contracts.${contractName}.scriptHash must be 28-byte lowercase hex`,
    );
  }
  return value;
};

const requireFixtureOutRef = (
  value: unknown,
  contractName: string,
): { readonly txHash: string; readonly outputIndex: number } => {
  if (!isRecord(value)) {
    throw new Error(
      `DA deployment fixture contracts.${contractName}.refScriptUTxO must be an object`,
    );
  }
  requireExactKeys(
    value,
    ["txHash", "outputIndex"],
    `DA deployment fixture contracts.${contractName}.refScriptUTxO`,
  );
  if (
    typeof value.txHash !== "string" ||
    !/^[0-9a-f]{64}$/u.test(value.txHash)
  ) {
    throw new Error(
      `DA deployment fixture contracts.${contractName}.refScriptUTxO.txHash must be 32-byte lowercase hex`,
    );
  }
  if (
    typeof value.outputIndex !== "number" ||
    !Number.isSafeInteger(value.outputIndex) ||
    value.outputIndex < 0
  ) {
    throw new Error(
      `DA deployment fixture contracts.${contractName}.refScriptUTxO.outputIndex must be a non-negative integer`,
    );
  }
  return { txHash: value.txHash, outputIndex: value.outputIndex };
};

const requireNullRefScriptUTxO = (
  value: unknown,
  contractName: string,
): null => {
  if (value !== null) {
    throw new Error(
      `DA deployment fixture contracts.${contractName}.refScriptUTxO must be null because the contract has no reference-script role`,
    );
  }
  return null;
};

const requireExactKeys = (
  value: Record<string, unknown>,
  keys: readonly string[],
  fieldName: string,
): void => {
  const expected = new Set(keys);
  for (const key of Object.keys(value)) {
    if (!expected.has(key)) {
      throw new Error(`${fieldName}.${key} is unexpected`);
    }
  }
  for (const key of keys) {
    if (!Object.hasOwn(value, key)) {
      throw new Error(`${fieldName}.${key} is required`);
    }
  }
};

const isRecord = (value: unknown): value is Record<string, unknown> =>
  typeof value === "object" && value !== null && !Array.isArray(value);

export const loadDaDeploymentFixture = async (
  network: string,
): Promise<MidgardNodeDeployment> => {
  const deployment = parseMidgardNodeDeploymentInfo(
    await readDaDeploymentFixture(),
    network,
  );
  return deployment;
};

export const writeDaDeploymentFixture = async (path: string): Promise<void> => {
  await writeFile(path, JSON.stringify(await readDaDeploymentFixture()));
};
