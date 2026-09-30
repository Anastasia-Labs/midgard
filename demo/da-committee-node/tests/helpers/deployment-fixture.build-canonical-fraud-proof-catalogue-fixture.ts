import { Store, Trie } from "@aiken-lang/merkle-patricia-forestry";
import {
  DEPLOYMENT_MANIFEST_CONTRACT_NAMES,
  type DeploymentManifestContractEntry,
  parseDeploymentManifestEventHistoryBounds,
  parseDeploymentManifestEventHistoryRecipe,
  parseDeploymentManifestEventHistoryRetentionAddress,
  parseDeploymentManifestEventHistoryRetentionAddresses,
} from "@al-ft/midgard-core/deployment-manifest-identity";
import {
  FRAUD_PROOF_CATALOGUE_CATEGORY_IDS,
  FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER,
  FRAUD_PROOF_CATALOGUE_ID_BYTE_COUNT,
  type FraudProofCatalogueDeploymentInfo,
  ScriptHashSchema,
} from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";

export const FIXTURE_URL = new URL(
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

export const requireFixtureContracts = (
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

type FixtureHistoryMetadata = Pick<
  DeploymentManifestContractEntry,
  | "eventHistoryRecipe"
  | "eventHistoryBounds"
  | "eventHistoryRetentionAddress"
  | "eventHistoryRetentionAddresses"
>;

export const requireFixtureContract = (
  value: unknown,
  contractName: string,
): {
  readonly source: Record<string, unknown>;
  readonly metadata: FixtureHistoryMetadata;
} => {
  if (!isRecord(value)) {
    throw new Error(
      `DA deployment fixture contracts.${contractName} must be an object`,
    );
  }
  const field = `DA deployment fixture contracts.${contractName}`;
  let metadata: FixtureHistoryMetadata = {};
  if (contractName === "depositMint" || contractName === "withdrawalMint") {
    metadata = {
      eventHistoryRecipe: parseDeploymentManifestEventHistoryRecipe(
        value.eventHistoryRecipe,
        `${field}.eventHistoryRecipe`,
      ),
    };
  } else if (
    contractName === "fraudProofFabricatedDeposit" ||
    contractName === "fraudProofFabricatedWithdrawal"
  ) {
    metadata = {
      eventHistoryBounds: parseDeploymentManifestEventHistoryBounds(
        value.eventHistoryBounds,
        `${field}.eventHistoryBounds`,
      ),
      eventHistoryRetentionAddress:
        parseDeploymentManifestEventHistoryRetentionAddress(
          value.eventHistoryRetentionAddress,
        ),
    };
  } else if (contractName === "fraudProofTransitionTrace") {
    metadata = {
      eventHistoryBounds: parseDeploymentManifestEventHistoryBounds(
        value.eventHistoryBounds,
        `${field}.eventHistoryBounds`,
      ),
      eventHistoryRetentionAddresses:
        parseDeploymentManifestEventHistoryRetentionAddresses(
          value.eventHistoryRetentionAddresses,
        ),
    };
  }
  requireExactKeys(
    value,
    ["refScriptUTxO", "contract", "scriptHash", ...Object.keys(metadata)],
    field,
  );
  return { source: value, metadata };
};

export const requireFixtureScript = (
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

export const requireFixtureScriptHash = (
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

export const requireFixtureOutRef = (
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

export const requireNullRefScriptUTxO = (
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

export const requireExactKeys = (
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

export const isRecord = (value: unknown): value is Record<string, unknown> =>
  typeof value === "object" && value !== null && !Array.isArray(value);
