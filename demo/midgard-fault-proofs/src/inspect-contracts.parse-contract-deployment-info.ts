import { Trie } from "@aiken-lang/merkle-patricia-forestry";
import {
  parseDeploymentManifestEventHistoryBounds,
  parseDeploymentManifestEventHistoryRetentionAddress,
  parseDeploymentManifestEventHistoryRetentionAddresses,
} from "@al-ft/midgard-core/deployment-manifest-identity";
import { formatUnknownError } from "@al-ft/midgard-core/error-format";
import { asLucidSchema } from "@al-ft/midgard-core/lucid-data";
import {
  EMPTY_MERKLE_TREE_ROOT,
  FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER,
  FRAUD_PROOF_CATALOGUE_ID_BYTE_COUNT,
  type FraudProofCatalogueCategoryDeploymentInfo,
  type FraudProofCatalogueCategoryName,
  type FraudProofCatalogueDeploymentInfo,
  Proof,
  ScriptHashSchema,
} from "@al-ft/midgard-sdk";
import { Data, Network } from "@lucid-evolution/lucid";

import {
  type ContractDeploymentInfo,
  type ContractDeploymentInfoEntry,
  DEFAULT_FAULT_PROOF_NETWORK,
  expectedFraudProofCategoryId,
  NETWORKS,
} from "./inspect-contracts.inspect-contracts-output.js";
import {
  parseSafeNonNegativeInteger,
  parseStrictHex,
  requireRecord,
} from "./json-file.js";

const FraudProofCatalogueIdSchema = Data.Bytes({
  minLength: FRAUD_PROOF_CATALOGUE_ID_BYTE_COUNT,
  maxLength: FRAUD_PROOF_CATALOGUE_ID_BYTE_COUNT,
});

export const parseNetwork = (network: string | undefined): Network => {
  const resolved = network ?? DEFAULT_FAULT_PROOF_NETWORK;
  if (!NETWORKS.has(resolved as Network)) {
    throw new Error(
      `Unsupported network "${resolved}". Expected one of: ${[...NETWORKS].join(", ")}`,
    );
  }
  return resolved as Network;
};

export const normalizeHex = (
  value: unknown,
  label: string,
  byteLength?: number,
): string =>
  parseStrictHex(value, {
    byteCount: byteLength,
    typeError: `${label} must be a hex string`,
    invalidError:
      byteLength === undefined
        ? `${label} must be even-length hex`
        : `${label} must be ${byteLength.toString()} bytes of hex`,
  });

const parseCatalogueCategoryDeploymentInfo = (
  value: unknown,
  label: string,
): FraudProofCatalogueCategoryDeploymentInfo => {
  const candidate = requireRecord(value, label) as {
    readonly categoryId?: unknown;
    readonly scriptHash?: unknown;
    readonly membershipProofCbor?: unknown;
  };
  const membershipProofCbor = normalizeHex(
    candidate.membershipProofCbor,
    `${label}.membershipProofCbor`,
  );
  try {
    Data.from(membershipProofCbor, Proof);
  } catch (cause) {
    throw new Error(
      `${label}.membershipProofCbor is not a valid Proof CBOR: ${formatUnknownError(cause)}`,
    );
  }
  return {
    categoryId: normalizeHex(
      candidate.categoryId,
      `${label}.categoryId`,
      FRAUD_PROOF_CATALOGUE_ID_BYTE_COUNT,
    ),
    scriptHash: normalizeHex(candidate.scriptHash, `${label}.scriptHash`, 28),
    membershipProofCbor,
  };
};

const parseFraudProofCatalogueDeploymentInfo = (
  value: unknown,
): FraudProofCatalogueDeploymentInfo => {
  const candidate = requireRecord(value, "fraudProofCatalogue") as {
    readonly root?: unknown;
    readonly categories?: unknown;
  };
  const rawCategories = requireRecord(
    candidate.categories,
    "fraudProofCatalogue.categories",
  );
  const canonicalCategoryNames = new Set<string>(
    FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER,
  );
  for (const rawCategoryName of Object.keys(rawCategories)) {
    if (!canonicalCategoryNames.has(rawCategoryName)) {
      throw new Error(
        `fraudProofCatalogue.categories contains unsupported category ${rawCategoryName}`,
      );
    }
  }
  const seenCategoryIds = new Map<string, FraudProofCatalogueCategoryName>();
  const categories = Object.fromEntries(
    FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER.map((name) => {
      const category = rawCategories[name];
      if (category === undefined) {
        throw new Error(`fraudProofCatalogue.categories.${name} is missing`);
      }
      const parsedCategory = parseCatalogueCategoryDeploymentInfo(
        category,
        `fraudProofCatalogue.categories.${name}`,
      );
      const previousCategory = seenCategoryIds.get(parsedCategory.categoryId);
      if (previousCategory !== undefined) {
        throw new Error(
          `fraudProofCatalogue.categories.${name}.categoryId duplicates fraudProofCatalogue.categories.${previousCategory}.categoryId`,
        );
      }
      seenCategoryIds.set(parsedCategory.categoryId, name);
      const expectedCategoryId = expectedFraudProofCategoryId(name);
      if (parsedCategory.categoryId !== expectedCategoryId) {
        throw new Error(
          `fraudProofCatalogue.categories.${name}.categoryId must be ${expectedCategoryId}, got ${parsedCategory.categoryId}`,
        );
      }
      return [name, parsedCategory];
    }),
  ) as FraudProofCatalogueDeploymentInfo["categories"];

  return {
    root: normalizeHex(candidate.root, "fraudProofCatalogue.root", 32),
    categories,
  };
};

export const fraudProofCatalogueCategories = (
  catalogue: FraudProofCatalogueDeploymentInfo,
): Readonly<
  Partial<
    Record<
      FraudProofCatalogueCategoryName,
      FraudProofCatalogueCategoryDeploymentInfo
    >
  >
> => catalogue.categories;

const parseRefScriptUTxO = (
  value: unknown,
  label: string,
): ContractDeploymentInfoEntry["refScriptUTxO"] => {
  if (value === undefined) {
    return undefined;
  }
  if (value === null) {
    return null;
  }
  const candidate = requireRecord(value, label) as {
    readonly txHash?: unknown;
    readonly outputIndex?: unknown;
  };
  return {
    txHash: normalizeHex(candidate.txHash, `${label}.txHash`, 32),
    outputIndex: Number(
      parseSafeNonNegativeInteger(
        candidate.outputIndex,
        `${label}.outputIndex`,
      ),
    ),
  };
};

const parseDeploymentContract = (
  value: unknown,
  label: string,
): ContractDeploymentInfoEntry["contract"] => {
  if (value === undefined) {
    return undefined;
  }
  const candidate = requireRecord(value, label) as {
    readonly type?: unknown;
    readonly cborHex?: unknown;
  };
  if (
    candidate.type !== "PlutusV1" &&
    candidate.type !== "PlutusV2" &&
    candidate.type !== "PlutusV3" &&
    candidate.type !== "Native"
  ) {
    throw new Error(`${label}.type is not a supported script type`);
  }
  return {
    type: candidate.type,
    cborHex: normalizeHex(candidate.cborHex, `${label}.cborHex`),
  };
};

export const encodeCatalogueKey = (categoryId: string): Buffer =>
  Buffer.from(
    Data.to(categoryId, asLucidSchema(FraudProofCatalogueIdSchema)),
    "hex",
  );

export const encodeCatalogueValue = (scriptHash: string): Buffer =>
  Buffer.from(Data.to(scriptHash, asLucidSchema(ScriptHashSchema)), "hex");

export const trieRootHex = (trie: Trie): string => {
  const hash = trie.hash;
  if (hash == null) {
    return EMPTY_MERKLE_TREE_ROOT;
  }
  return Buffer.from(hash).toString("hex");
};

/** Script reapplication below binds these declared integers to deployed hashes. */
export const contractDeploymentHistoryBounds = (
  info: ContractDeploymentInfo,
  family: "fabricatedDeposit" | "fabricatedWithdrawal" | "transitionTrace",
) => {
  const name =
    family === "fabricatedDeposit"
      ? "fraudProofFabricatedDeposit"
      : family === "fabricatedWithdrawal"
        ? "fraudProofFabricatedWithdrawal"
        : "fraudProofTransitionTrace";
  const bounds = parseDeploymentManifestEventHistoryBounds(
    info[name]?.eventHistoryBounds,
    `contracts.${name}.eventHistoryBounds`,
  );
  return {
    inlineLimitBytes: BigInt(bounds.inlineLimitBytes),
    maxPayloadBytes: BigInt(bounds.maxPayloadBytes),
    maxPayloadNodes: BigInt(bounds.maxPayloadNodes),
  };
};

export const parseContractDeploymentInfo = (
  value: unknown,
): ContractDeploymentInfo => {
  const manifest = requireRecord(value, "Contract deployment info");
  if (manifest.referenceScriptAuthPolicy === undefined) {
    throw new Error(
      "Contract deployment info is missing referenceScriptAuthPolicy.",
    );
  }
  const rawEntries = requireRecord(
    manifest.contracts,
    "Contract deployment info.contracts",
  );

  const entries: Record<string, ContractDeploymentInfoEntry> = {};
  for (const [name, entry] of Object.entries(rawEntries)) {
    const candidate = requireRecord(entry, `Deployment entry "${name}"`) as {
      readonly scriptHash?: unknown;
      readonly refScriptUTxO?: unknown;
      readonly contract?: unknown;
      readonly fraudProofCatalogue?: unknown;
      readonly eventHistoryBounds?: unknown;
      readonly eventHistoryRetentionAddress?: unknown;
      readonly eventHistoryRetentionAddresses?: unknown;
    };
    entries[name] = {
      scriptHash: normalizeHex(
        candidate.scriptHash,
        `Deployment entry "${name}".scriptHash`,
        28,
      ),
      refScriptUTxO: parseRefScriptUTxO(
        candidate.refScriptUTxO,
        `Deployment entry "${name}".refScriptUTxO`,
      ),
      ...(candidate.contract !== undefined
        ? {
            contract: parseDeploymentContract(
              candidate.contract,
              `Deployment entry "${name}".contract`,
            ),
          }
        : {}),
      ...(candidate.eventHistoryRetentionAddresses === undefined
        ? {}
        : {
            eventHistoryRetentionAddresses:
              parseDeploymentManifestEventHistoryRetentionAddresses(
                candidate.eventHistoryRetentionAddresses,
              ),
          }),
      ...(candidate.eventHistoryRetentionAddress === undefined
        ? {}
        : {
            eventHistoryRetentionAddress:
              parseDeploymentManifestEventHistoryRetentionAddress(
                candidate.eventHistoryRetentionAddress,
              ),
          }),
      ...(candidate.eventHistoryBounds === undefined
        ? {}
        : {
            eventHistoryBounds: parseDeploymentManifestEventHistoryBounds(
              candidate.eventHistoryBounds,
              `contracts.${name}.eventHistoryBounds`,
            ),
          }),
      ...(candidate.fraudProofCatalogue !== undefined
        ? {
            fraudProofCatalogue: parseFraudProofCatalogueDeploymentInfo(
              candidate.fraudProofCatalogue,
            ),
          }
        : {}),
    };
  }

  return entries;
};
