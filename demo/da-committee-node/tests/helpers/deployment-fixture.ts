import { readFile, writeFile } from "node:fs/promises";

import { Store, Trie } from "@aiken-lang/merkle-patricia-forestry";
import {
  MIDGARD_CONSENSUS_PROFILE_V1,
  MIDGARD_CONSENSUS_PROFILE_V1_DIGEST,
  MIDGARD_DEPLOYMENT_MANIFEST_V1_SCHEMA_VERSION,
  MIDGARD_V1_RELEASE_EVIDENCE_DIGEST,
} from "@al-ft/midgard-core/consensus-profile-v1";
import {
  DA_RUNTIME_MANIFEST_V1_SCHEMA_VERSION,
  DA_TRANSPORT_LIMITS_V1,
  DA_TRANSPORT_V1_PROTOCOL_VERSION,
} from "@al-ft/midgard-core/da-transport";
import {
  computeDeploymentManifestV1Id,
  computeDeploymentManifestV1JsonDigest,
  DEPLOYMENT_MANIFEST_V1_CONTRACT_NAMES,
  DEPLOYMENT_MANIFEST_V1_REFERENCE_SCRIPT_CONTRACT_BY_ROLE,
  DEPLOYMENT_MANIFEST_V1_REFERENCE_SCRIPT_TOKEN_NAMES,
  DEPLOYMENT_MANIFEST_V1_STEP_NAMES,
} from "@al-ft/midgard-core/deployment-manifest-identity-v1";
import {
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

const catalogueCategoryId = (index: number): string => {
  const bytes = Buffer.alloc(FRAUD_PROOF_CATALOGUE_ID_BYTE_COUNT);
  bytes.writeUInt32BE(index);
  return bytes.toString("hex");
};

const catalogueKeySchema = Data.Bytes({
  minLength: FRAUD_PROOF_CATALOGUE_ID_BYTE_COUNT,
  maxLength: FRAUD_PROOF_CATALOGUE_ID_BYTE_COUNT,
});

export const buildCanonicalFraudProofCatalogueFixture = async (
  scriptHashes: Readonly<Record<FraudProofCatalogueCategoryName, string>>,
): Promise<FraudProofCatalogueDeploymentInfo> => {
  const categories = Object.fromEntries(
    FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER.map((categoryName, index) => [
      categoryName,
      {
        categoryId: catalogueCategoryId(index),
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
    Object.values(DEPLOYMENT_MANIFEST_V1_REFERENCE_SCRIPT_CONTRACT_BY_ROLE),
  );
  const contracts = Object.fromEntries(
    DEPLOYMENT_MANIFEST_V1_CONTRACT_NAMES.map((contractName) => {
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
    }),
  };
  const referenceScripts = Object.fromEntries(
    Object.entries(
      DEPLOYMENT_MANIFEST_V1_REFERENCE_SCRIPT_CONTRACT_BY_ROLE,
    ).map(([role, contractName]) => {
      const contract = contracts[contractName]!;
      const outRef = contract.refScriptUTxO as {
        readonly txHash: string;
        readonly outputIndex: number;
      };
      const tokenName =
        DEPLOYMENT_MANIFEST_V1_REFERENCE_SCRIPT_TOKEN_NAMES[
          role as keyof typeof DEPLOYMENT_MANIFEST_V1_REFERENCE_SCRIPT_TOKEN_NAMES
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
    }),
  );
  const committeeVkey = "01".repeat(32);
  const cardanoProtocolParameterSnapshot = { maxTxSize: 16_384 };
  const identityInput = {
    schemaVersion: MIDGARD_DEPLOYMENT_MANIFEST_V1_SCHEMA_VERSION,
    consensusProfile: MIDGARD_CONSENSUS_PROFILE_V1,
    consensusProfileDigest: MIDGARD_CONSENSUS_PROFILE_V1_DIGEST,
    network: "Preview",
    cardanoProtocolParameters: {
      snapshot: cardanoProtocolParameterSnapshot,
      digest: computeDeploymentManifestV1JsonDigest(
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
      tokenNames: DEPLOYMENT_MANIFEST_V1_REFERENCE_SCRIPT_TOKEN_NAMES,
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
        protocolVersion: DA_TRANSPORT_V1_PROTOCOL_VERSION,
        runtimeManifestSchemaVersion: DA_RUNTIME_MANIFEST_V1_SCHEMA_VERSION,
        envelopeEncoding: "identity",
        zstdLevel: 3,
        limits: DA_TRANSPORT_LIMITS_V1,
        retentionDays: DA_TRANSPORT_LIMITS_V1.minimumRetentionDays,
      },
    },
    proofEvidence: {
      digest: MIDGARD_V1_RELEASE_EVIDENCE_DIGEST,
      blueprintHash: "00".repeat(32),
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
  };
  return {
    ...identityInput,
    manifestId: computeDeploymentManifestV1Id(identityInput),
  };
};

const requireFixtureContracts = (
  fixture: Record<string, unknown>,
): Record<string, Record<string, unknown>> => {
  const value = fixture.contracts;
  if (!isRecord(value)) {
    throw new Error("DA deployment fixture contracts must be an object");
  }
  const expected = new Set<string>(DEPLOYMENT_MANIFEST_V1_CONTRACT_NAMES);
  const missing = DEPLOYMENT_MANIFEST_V1_CONTRACT_NAMES.filter(
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
