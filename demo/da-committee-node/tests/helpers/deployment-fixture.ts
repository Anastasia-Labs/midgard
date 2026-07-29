import { readFile, writeFile } from "node:fs/promises";

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
import { Store, Trie } from "@aiken-lang/merkle-patricia-forestry";
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

type LucidDataSchema = Parameters<typeof Data.to>[1];

const FRAUD_PROOF_CATALOGUE_CATEGORY_CONTRACTS = [
  ["doubleSpend", "fraudProofDoubleSpend"],
  ["nonExistentInput", "fraudProofNonExistentInput"],
  ["nonExistentInputNoIndex", "fraudProofNonExistentInputNoIndex"],
  ["invalidRange", "fraudProofInvalidRange"],
  ["transitionTrace", "fraudProofTransitionTrace"],
  ["zeroInput", "fraudProofZeroInput"],
  ["validationTraceDispute", "validationTraceDispute"],
] as const;

const encodeFraudProofCatalogueBytes = (
  value: string,
  exactByteLength: number,
): Buffer =>
  Buffer.from(
    Data.to(
      value,
      Data.Bytes({
        minLength: exactByteLength,
        maxLength: exactByteLength,
      }) as unknown as LucidDataSchema,
    ),
    "hex",
  );

export const readDaDeploymentFixture = async (): Promise<
  Record<string, unknown>
> => {
  const fixture = JSON.parse(await readFile(FIXTURE_URL, "utf8")) as Record<
    string,
    unknown
  >;
  const fixtureContracts = fixture.contracts as Record<
    string,
    Record<string, unknown>
  >;
  const templateContract = fixtureContracts.stateQueueSpend!;
  const nativeScriptCbor = `8200581c${"00".repeat(28)}`;
  const referenceScriptAuthPolicyId = validatorToScriptHash({
    type: "Native",
    script: nativeScriptCbor,
  });
  const referenceScriptContractNames = new Set<string>(
    Object.values(DEPLOYMENT_MANIFEST_V1_REFERENCE_SCRIPT_CONTRACT_BY_ROLE),
  );
  const contracts = Object.fromEntries(
    DEPLOYMENT_MANIFEST_V1_CONTRACT_NAMES.map((contractName, index) => {
      if (contractName === "referenceScriptAuthMint") {
        return [
          contractName,
          {
            refScriptUTxO: {
              txHash: (index + 10).toString(16).padStart(2, "0").repeat(32),
              outputIndex: 0,
            },
            contract: {
              type: "Native",
              cborHex: nativeScriptCbor,
            },
            scriptHash: referenceScriptAuthPolicyId,
          },
        ];
      }
      const existing = fixtureContracts[contractName];
      const source = existing ?? templateContract;
      return [
        contractName,
        {
          ...source,
          refScriptUTxO: referenceScriptContractNames.has(contractName)
            ? (existing?.refScriptUTxO ?? {
                txHash: (index + 10).toString(16).padStart(2, "0").repeat(32),
                outputIndex: 0,
              })
            : null,
          contract: {
            ...(source.contract as Record<string, unknown>),
          },
        },
      ];
    }),
  ) as Record<string, Record<string, unknown>>;
  const fraudProofCatalogueCategories = Object.fromEntries(
    FRAUD_PROOF_CATALOGUE_CATEGORY_CONTRACTS.map(
      ([categoryName, contractName], index) => [
        categoryName,
        {
          categoryId: index.toString(16).padStart(8, "0"),
          scriptHash: contracts[contractName]!.scriptHash as string,
          membershipProofCbor: "",
        },
      ],
    ),
  ) as Record<
    (typeof FRAUD_PROOF_CATALOGUE_CATEGORY_CONTRACTS)[number][0],
    {
      readonly categoryId: string;
      readonly scriptHash: string;
      membershipProofCbor: string;
    }
  >;
  const fraudProofCatalogueStore = new Store(undefined);
  await fraudProofCatalogueStore.ready();
  const fraudProofCatalogueTrie = new Trie(fraudProofCatalogueStore);
  for (const [categoryName] of FRAUD_PROOF_CATALOGUE_CATEGORY_CONTRACTS) {
    const category = fraudProofCatalogueCategories[categoryName];
    await fraudProofCatalogueTrie.insert(
      encodeFraudProofCatalogueBytes(category.categoryId, 4),
      encodeFraudProofCatalogueBytes(category.scriptHash, 28),
    );
  }
  for (const [categoryName] of FRAUD_PROOF_CATALOGUE_CATEGORY_CONTRACTS) {
    const category = fraudProofCatalogueCategories[categoryName];
    const proof = await fraudProofCatalogueTrie.prove(
      encodeFraudProofCatalogueBytes(category.categoryId, 4),
    );
    category.membershipProofCbor = proof.toCBOR().toString("hex");
  }
  if (fraudProofCatalogueTrie.hash == null) {
    throw new Error("DA deployment fixture fraud-proof catalogue is empty");
  }
  contracts.fraudProofCatalogueMint = {
    ...contracts.fraudProofCatalogueMint,
    fraudProofCatalogue: {
      root: Buffer.from(fraudProofCatalogueTrie.hash).toString("hex"),
      categories: fraudProofCatalogueCategories,
    },
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
