import { Store, Trie } from "@aiken-lang/merkle-patricia-forestry";
import { MIDGARD_CONSENSUS_LIMITS_V1 } from "@al-ft/midgard-core/consensus-profile-v1";
import { formatUnknownError } from "@al-ft/midgard-core/error-format";
import {
  buildFaultProofContracts,
  EMPTY_MERKLE_TREE_ROOT,
  FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER,
  FRAUD_PROOF_CATALOGUE_ID_BYTE_COUNT,
  type FraudProofCatalogueCategoryDeploymentInfo,
  type FraudProofCatalogueCategoryName,
  type FraudProofCatalogueDeploymentInfo,
  parseFaultProofBlueprint,
  Proof,
  REFERENCE_SCRIPT_AUTH_TOKEN_NAMES,
  type ReferenceScriptAuthTokenTarget,
  ScriptHashSchema,
} from "@al-ft/midgard-sdk";
import {
  Data,
  mintingPolicyToId,
  Network,
  type Script,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";

import {
  parseSafeNonNegativeInteger,
  parseStrictHex,
  readJsonFile,
  requireRecord,
} from "./json-file.js";

export type ContractDeploymentInfoEntry = {
  readonly scriptHash: string;
  readonly refScriptUTxO?: {
    readonly txHash: string;
    readonly outputIndex: number;
  } | null;
  readonly contract?: {
    readonly type: Script["type"];
    readonly cborHex: string;
  };
  readonly fraudProofCatalogue?: FraudProofCatalogueDeploymentInfo;
};

export type ContractDeploymentInfo = Readonly<
  Record<string, ContractDeploymentInfoEntry>
>;

export type InspectContractsParams = {
  readonly blueprint: unknown;
  readonly deploymentInfo: unknown;
  readonly network?: Network;
};

export type InspectContractsFromFilesParams = {
  readonly blueprintPath: string;
  readonly deploymentInfoPath: string;
  readonly network?: Network;
};

export type InspectContractsOutput = {
  readonly network: Network;
  /**
   * Necessary-only L1 envelope audit over every parameterized spending
   * validator selected by the fault-proof contract builder. Passing this
   * audit does not satisfy proof-fit: the complete concrete transaction,
   * including framing, witnesses, redeemers, and outputs, must also fit.
   */
  readonly l1SpendingScriptEnvelopeNecessaryCondition: {
    readonly maxTransactionBytes: number;
    readonly appliedSpendingScriptCount: number;
    readonly allAppliedSpendingScriptsWithinEnvelope: boolean;
    readonly oversizedAppliedSpendingScripts: readonly InspectContractsOversizedSpendingScript[];
  };
  readonly computationThread: {
    readonly policyId: string;
  };
  readonly fraudProof: {
    readonly policyId: string;
    readonly address: string;
    readonly spendingScriptHash: string;
  };
  readonly fraudProofCatalogue: {
    readonly root: string | null;
    readonly derivedRoot: string | null;
    readonly rootMatchesDerived: boolean | null;
    readonly initReady: boolean;
    readonly doubleSpend: InspectContractsCatalogueCategoryOutput;
    readonly nonExistentInput: InspectContractsCatalogueCategoryOutput;
    readonly invalidRange: InspectContractsCatalogueCategoryOutput;
    readonly zeroInput: InspectContractsCatalogueCategoryOutput;
    readonly transitionTrace: InspectContractsCatalogueCategoryOutput;
    readonly validationTraceDispute: InspectContractsCatalogueCategoryOutput;
  };
  readonly doubleSpend: {
    readonly categoryFirstStepHash: string;
    readonly deploymentDoubleSpendScriptHash: string | null;
    readonly deploymentDoubleSpendMatchesFirstStep: boolean | null;
    readonly steps: readonly [
      InspectContractsStepOutput,
      InspectContractsStepOutput,
      InspectContractsStepOutput,
      InspectContractsStepOutput,
    ];
  };
  readonly nonExistentInput: {
    readonly categoryFirstStepHash: string;
    readonly deploymentNonExistentInputScriptHash: string | null;
    readonly deploymentNonExistentInputMatchesFirstStep: boolean | null;
    readonly steps: readonly [
      InspectContractsStepOutput,
      InspectContractsStepOutput,
      InspectContractsStepOutput,
      InspectContractsStepOutput,
    ];
  };
  readonly invalidRange: {
    readonly categoryFirstStepHash: string;
    readonly deploymentInvalidRangeScriptHash: string | null;
    readonly deploymentInvalidRangeMatchesFirstStep: boolean | null;
    readonly steps: readonly [
      InspectContractsStepOutput,
      InspectContractsStepOutput,
    ];
  };
  readonly zeroInput: {
    readonly categoryFirstStepHash: string;
    readonly deploymentZeroInputScriptHash: string | null;
    readonly deploymentZeroInputMatchesFirstStep: boolean | null;
    readonly steps: readonly [
      InspectContractsStepOutput,
      InspectContractsStepOutput,
    ];
  };
  readonly transitionTrace: {
    readonly categoryFirstStepHash: string;
    readonly deploymentTransitionTraceScriptHash: string | null;
    readonly deploymentTransitionTraceMatchesFirstStep: boolean | null;
    readonly steps: readonly [
      InspectContractsStepOutput,
      InspectContractsStepOutput,
      InspectContractsStepOutput,
      InspectContractsStepOutput,
      InspectContractsStepOutput,
      InspectContractsStepOutput,
      InspectContractsStepOutput,
      InspectContractsStepOutput,
      InspectContractsStepOutput,
    ];
  };
  readonly validationTraceDispute: {
    readonly categoryFirstStepHash: string;
    readonly deploymentValidationTraceDisputeScriptHash: string | null;
    readonly deploymentValidationTraceDisputeMatchesFirstStep: boolean | null;
    readonly steps: readonly [
      InspectContractsStepOutput,
      ...InspectContractsStepOutput[],
    ];
  };
};

export type InspectContractsStepOutput = {
  readonly name:
    | "step01"
    | "step02"
    | "step03"
    | "step04"
    | "route"
    | "control"
    | "withdrawal"
    | "forced"
    | "accepted"
    | "deposit"
    | "l1Event"
    | "duplicate"
    | "dispute"
    | "source"
    | "game"
    | "boundary"
    | "timeout"
    | "award"
    | `semantic-resolver-${number}`
    | `prepare-resolver-${number}`
    | `direct-resolver-${number}`;
  readonly scriptHash: string;
  readonly address: string;
  readonly standaloneScriptBytes: number;
  /**
   * A necessary condition only. The script must leave at least one byte for
   * the rest of the complete L1 transaction; full proof-fit is stricter.
   */
  readonly withinL1TransactionByteEnvelopeNecessaryCondition: boolean;
};

export type InspectContractsProofCategory =
  | "doubleSpend"
  | "nonExistentInput"
  | "invalidRange"
  | "zeroInput"
  | "transitionTrace"
  | "validationTraceDispute";

export type InspectContractsOversizedSpendingScript = {
  readonly category: InspectContractsProofCategory;
  readonly name: InspectContractsStepOutput["name"];
  readonly scriptHash: string;
  readonly standaloneScriptBytes: number;
};

export type InspectContractsCatalogueCategoryOutput = {
  readonly categoryId: string | null;
  readonly expectedCategoryId: string | null;
  readonly categoryIdMatchesExpected: boolean | null;
  readonly scriptHash: string | null;
  readonly scriptHashMatchesFirstStep: boolean | null;
  readonly membershipProofCbor: string | null;
  readonly membershipProofMatchesDerived: boolean | null;
  readonly ready: boolean;
};

export type ImplementedFraudProofCategoryName =
  | "doubleSpend"
  | "nonExistentInput"
  | "invalidRange"
  | "zeroInput"
  | "transitionTrace"
  | "validationTraceDispute";

export const expectedFraudProofCategoryId = (
  name: FraudProofCatalogueCategoryName,
): string => {
  const index = FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER.indexOf(name);
  if (index < 0) {
    throw new Error(`Unknown fraud-proof catalogue category "${name}"`);
  }
  const bytes = Buffer.alloc(FRAUD_PROOF_CATALOGUE_ID_BYTE_COUNT);
  bytes.writeUInt32BE(index);
  return bytes.toString("hex");
};

export const DEFAULT_FAULT_PROOF_NETWORK: Network = "Preprod";

const NETWORKS = new Set<Network>(["Mainnet", "Preview", "Preprod"]);

const FraudProofCatalogueIdSchema = Data.Bytes({
  minLength: FRAUD_PROOF_CATALOGUE_ID_BYTE_COUNT,
  maxLength: FRAUD_PROOF_CATALOGUE_ID_BYTE_COUNT,
});
type LucidDataSchema = Parameters<typeof Data.to>[1];

export const parseNetwork = (network: string | undefined): Network => {
  const resolved = network ?? DEFAULT_FAULT_PROOF_NETWORK;
  if (!NETWORKS.has(resolved as Network)) {
    throw new Error(
      `Unsupported network "${resolved}". Expected one of: ${[...NETWORKS].join(", ")}`,
    );
  }
  return resolved as Network;
};

const normalizeHex = (
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

const fraudProofCatalogueCategories = (
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

const encodeCatalogueKey = (categoryId: string): Buffer =>
  Buffer.from(
    Data.to(
      categoryId,
      FraudProofCatalogueIdSchema as unknown as LucidDataSchema,
    ),
    "hex",
  );

const encodeCatalogueValue = (scriptHash: string): Buffer =>
  Buffer.from(
    Data.to(scriptHash, ScriptHashSchema as unknown as LucidDataSchema),
    "hex",
  );

const trieRootHex = (trie: Trie): string => {
  const hash = trie.hash;
  if (hash == null) {
    return EMPTY_MERKLE_TREE_ROOT;
  }
  return Buffer.from(hash).toString("hex");
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

export const parseContractDeploymentReferenceScriptAuthPolicyId = (
  value: unknown,
  target: ReferenceScriptAuthTokenTarget,
): string => {
  const manifest = requireRecord(value, "Contract deployment info");
  const policy = requireRecord(
    manifest.referenceScriptAuthPolicy,
    "Contract deployment info.referenceScriptAuthPolicy",
  );
  const policyId = normalizeHex(
    policy.policyId,
    "Contract deployment info.referenceScriptAuthPolicy.policyId",
    28,
  );
  const nativeScript = requireRecord(
    policy.nativeScript,
    "Contract deployment info.referenceScriptAuthPolicy.nativeScript",
  );
  if (nativeScript.type !== "Native") {
    throw new Error(
      "Contract deployment info.referenceScriptAuthPolicy.nativeScript.type must be Native",
    );
  }
  const cborHex = normalizeHex(
    nativeScript.cborHex,
    "Contract deployment info.referenceScriptAuthPolicy.nativeScript.cborHex",
  );
  let derivedPolicyId: string;
  try {
    derivedPolicyId = mintingPolicyToId({
      type: "Native",
      script: cborHex,
    });
  } catch (cause) {
    throw new Error(
      `Contract deployment info.referenceScriptAuthPolicy.nativeScript.cborHex is invalid: ${formatUnknownError(cause)}`,
    );
  }
  if (derivedPolicyId !== policyId) {
    throw new Error(
      `Contract deployment info.referenceScriptAuthPolicy.policyId mismatch: declared=${policyId}, derived=${derivedPolicyId}`,
    );
  }
  const tokenNames = requireRecord(
    policy.tokenNames,
    "Contract deployment info.referenceScriptAuthPolicy.tokenNames",
  );
  const expectedTokenName = REFERENCE_SCRIPT_AUTH_TOKEN_NAMES[target];
  if (tokenNames[target] !== expectedTokenName) {
    throw new Error(
      `Contract deployment info.referenceScriptAuthPolicy.tokenNames.${target} must equal ${expectedTokenName}`,
    );
  }
  return policyId;
};

const requireDeploymentScriptHash = (
  deploymentInfo: ContractDeploymentInfo,
  name: string,
): string => {
  const entry = deploymentInfo[name];
  if (entry === undefined) {
    throw new Error(`Deployment info is missing "${name}"`);
  }
  return entry.scriptHash;
};

const optionalDeploymentScriptHash = (
  deploymentInfo: ContractDeploymentInfo,
  name: string,
): string | null => deploymentInfo[name]?.scriptHash ?? null;

const expectScriptHash = (
  label: string,
  actual: string,
  expected: string,
): void => {
  if (actual !== expected) {
    throw new Error(
      `${label} mismatch: derived=${actual} deployment=${expected}`,
    );
  }
};

const emptyCatalogueCategoryInspection =
  (): InspectContractsCatalogueCategoryOutput => ({
    categoryId: null,
    scriptHash: null,
    scriptHashMatchesFirstStep: null,
    membershipProofCbor: null,
    membershipProofMatchesDerived: null,
    expectedCategoryId: null,
    categoryIdMatchesExpected: null,
    ready: false,
  });

export type FraudProofCatalogueCategoryReadiness = {
  readonly category: FraudProofCatalogueCategoryDeploymentInfo;
  readonly expectedCategoryId: string;
  readonly categoryIdMatchesExpected: boolean;
  readonly scriptHashMatchesFirstStep: boolean;
  readonly membershipProofMatchesDerived: boolean;
  readonly ready: boolean;
};

export const inspectFraudProofCatalogueCategoryReadiness = async ({
  catalogue,
  categoryName,
  expectedFirstStepHash,
  deploymentMatchesFirstStep,
}: {
  readonly catalogue: FraudProofCatalogueDeploymentInfo;
  readonly categoryName: FraudProofCatalogueCategoryName;
  readonly expectedFirstStepHash: string;
  readonly deploymentMatchesFirstStep: boolean | null;
}): Promise<{
  readonly derivedRoot: string;
  readonly rootMatchesDerived: boolean;
  readonly categoryReadiness: FraudProofCatalogueCategoryReadiness;
}> => {
  const store = new Store(undefined);
  await store.ready();
  const trie = new Trie(store);
  const categories = fraudProofCatalogueCategories(catalogue);
  for (const currentCategoryName of FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER) {
    const currentCategory = categories[currentCategoryName];
    if (currentCategory === undefined) {
      throw new Error(
        `fraudProofCatalogue.categories.${currentCategoryName} is missing`,
      );
    }
    await trie.insert(
      encodeCatalogueKey(currentCategory.categoryId),
      encodeCatalogueValue(currentCategory.scriptHash),
    );
  }

  const derivedRoot = trieRootHex(trie);
  const rootMatchesDerived = catalogue.root === derivedRoot;
  const category = categories[categoryName];
  if (category === undefined) {
    throw new Error(
      `fraudProofCatalogue.categories.${categoryName} is missing`,
    );
  }
  const proof = await trie.prove(encodeCatalogueKey(category.categoryId));
  const derivedProofCbor = proof.toCBOR().toString("hex");
  const expectedCategoryId = expectedFraudProofCategoryId(categoryName);
  const categoryIdMatchesExpected = category.categoryId === expectedCategoryId;
  const scriptHashMatchesFirstStep =
    category.scriptHash === expectedFirstStepHash;
  const membershipProofMatchesDerived =
    category.membershipProofCbor === derivedProofCbor;

  return {
    derivedRoot,
    rootMatchesDerived,
    categoryReadiness: {
      category,
      expectedCategoryId,
      categoryIdMatchesExpected,
      scriptHashMatchesFirstStep,
      membershipProofMatchesDerived,
      ready:
        deploymentMatchesFirstStep === true &&
        rootMatchesDerived &&
        categoryIdMatchesExpected &&
        scriptHashMatchesFirstStep &&
        membershipProofMatchesDerived,
    },
  };
};

export const assertFraudProofCatalogueCategoryReady = async ({
  catalogue,
  categoryName,
  expectedFirstStepHash,
  deploymentMatchesFirstStep,
}: {
  readonly catalogue: FraudProofCatalogueDeploymentInfo;
  readonly categoryName: FraudProofCatalogueCategoryName;
  readonly expectedFirstStepHash: string;
  readonly deploymentMatchesFirstStep: boolean | null;
}): Promise<FraudProofCatalogueCategoryDeploymentInfo> => {
  const { rootMatchesDerived, categoryReadiness } =
    await inspectFraudProofCatalogueCategoryReadiness({
      catalogue,
      categoryName,
      expectedFirstStepHash,
      deploymentMatchesFirstStep,
    });
  if (!rootMatchesDerived) {
    throw new Error(
      `Fraud-proof catalogue root mismatch: deployment=${catalogue.root}, derived from categories differs.`,
    );
  }
  if (!categoryReadiness.categoryIdMatchesExpected) {
    throw new Error(
      `fraudProofCatalogue.categories.${categoryName}.categoryId must be ${categoryReadiness.expectedCategoryId}, got ${categoryReadiness.category.categoryId}`,
    );
  }
  if (!categoryReadiness.scriptHashMatchesFirstStep) {
    throw new Error(
      `fraudProofCatalogue.categories.${categoryName}.scriptHash mismatch: deployment=${categoryReadiness.category.scriptHash}, derived=${expectedFirstStepHash}.`,
    );
  }
  if (!categoryReadiness.membershipProofMatchesDerived) {
    throw new Error(
      `fraudProofCatalogue.categories.${categoryName}.membershipProofCbor does not match the derived catalogue proof.`,
    );
  }
  if (!categoryReadiness.ready) {
    throw new Error(
      `Fraud-proof catalogue category ${categoryName} is not ready for Init.`,
    );
  }
  return categoryReadiness.category;
};

const inspectFraudProofCatalogue = (
  catalogue: FraudProofCatalogueDeploymentInfo | undefined,
  expectedFirstStepHashes: Readonly<
    Record<ImplementedFraudProofCategoryName, string>
  >,
  deploymentMatchesFirstStep: Readonly<
    Record<ImplementedFraudProofCategoryName, boolean | null>
  >,
): Effect.Effect<InspectContractsOutput["fraudProofCatalogue"], Error> => {
  if (catalogue === undefined) {
    return Effect.succeed({
      root: null,
      derivedRoot: null,
      rootMatchesDerived: null,
      initReady: false,
      doubleSpend: emptyCatalogueCategoryInspection(),
      nonExistentInput: emptyCatalogueCategoryInspection(),
      invalidRange: emptyCatalogueCategoryInspection(),
      zeroInput: emptyCatalogueCategoryInspection(),
      transitionTrace: emptyCatalogueCategoryInspection(),
      validationTraceDispute: emptyCatalogueCategoryInspection(),
    });
  }

  return Effect.tryPromise({
    try: async () => {
      const store = new Store(undefined);
      await store.ready();
      const trie = new Trie(store);
      const categories = fraudProofCatalogueCategories(catalogue);
      for (const categoryName of FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER) {
        const category = categories[categoryName];
        if (category === undefined) {
          throw new Error(
            `fraudProofCatalogue.categories.${categoryName} is missing`,
          );
        }
        await trie.insert(
          encodeCatalogueKey(category.categoryId),
          encodeCatalogueValue(category.scriptHash),
        );
      }

      const derivedRoot = trieRootHex(trie);
      const rootMatchesDerived = catalogue.root === derivedRoot;
      const inspectCategory = async (
        name: ImplementedFraudProofCategoryName,
      ): Promise<InspectContractsCatalogueCategoryOutput> => {
        const category = categories[name];
        if (category === undefined) {
          return emptyCatalogueCategoryInspection();
        }
        const expectedCategoryId = expectedFraudProofCategoryId(name);
        const proof = await trie.prove(encodeCatalogueKey(category.categoryId));
        const derivedProofCbor = proof.toCBOR().toString("hex");
        const categoryIdMatchesExpected =
          category.categoryId === expectedCategoryId;
        const scriptHashMatchesFirstStep =
          category.scriptHash === expectedFirstStepHashes[name];
        const membershipProofMatchesDerived =
          category.membershipProofCbor === derivedProofCbor;
        return {
          categoryId: category.categoryId,
          expectedCategoryId,
          categoryIdMatchesExpected,
          scriptHash: category.scriptHash,
          scriptHashMatchesFirstStep,
          membershipProofCbor: category.membershipProofCbor,
          membershipProofMatchesDerived,
          ready:
            deploymentMatchesFirstStep[name] === true &&
            rootMatchesDerived &&
            categoryIdMatchesExpected &&
            scriptHashMatchesFirstStep &&
            membershipProofMatchesDerived,
        };
      };

      const doubleSpend = await inspectCategory("doubleSpend");
      const nonExistentInput = await inspectCategory("nonExistentInput");
      const invalidRange = await inspectCategory("invalidRange");
      const zeroInput = await inspectCategory("zeroInput");
      const transitionTrace = await inspectCategory("transitionTrace");
      const validationTraceDispute = await inspectCategory(
        "validationTraceDispute",
      );
      const implementedCategories: readonly ImplementedFraudProofCategoryName[] =
        [
          "doubleSpend",
          "nonExistentInput",
          "invalidRange",
          "zeroInput",
          "transitionTrace",
          ...("validationTraceDispute" in catalogue.categories
            ? (["validationTraceDispute"] as const)
            : []),
        ];
      const implementedCategoriesReady = implementedCategories.every((name) => {
        return {
          doubleSpend,
          nonExistentInput,
          invalidRange,
          zeroInput,
          transitionTrace,
          validationTraceDispute,
        }[name].ready;
      });

      return {
        root: catalogue.root,
        derivedRoot,
        rootMatchesDerived,
        initReady: rootMatchesDerived && implementedCategoriesReady,
        doubleSpend,
        nonExistentInput,
        invalidRange,
        zeroInput,
        transitionTrace,
        validationTraceDispute,
      };
    },
    catch: (cause) =>
      new Error(
        `Failed to inspect fraud-proof catalogue deployment info: ${formatUnknownError(cause)}`,
      ),
  });
};

export const inspectContracts = ({
  blueprint,
  deploymentInfo,
  network = DEFAULT_FAULT_PROOF_NETWORK,
}: InspectContractsParams): Effect.Effect<InspectContractsOutput, Error> =>
  Effect.gen(function* () {
    const parsedBlueprint = parseFaultProofBlueprint(blueprint);
    const parsedDeploymentInfo = parseContractDeploymentInfo(deploymentInfo);

    const hubOraclePolicyId = requireDeploymentScriptHash(
      parsedDeploymentInfo,
      "hubOracleMint",
    );
    const fraudProofCataloguePolicyId = requireDeploymentScriptHash(
      parsedDeploymentInfo,
      "fraudProofCatalogueMint",
    );
    const deploymentFraudProofPolicyId = requireDeploymentScriptHash(
      parsedDeploymentInfo,
      "fraudProofMint",
    );
    const deploymentFraudProofSpendingHash = requireDeploymentScriptHash(
      parsedDeploymentInfo,
      "fraudProofSpend",
    );
    const deploymentDoubleSpendScriptHash = optionalDeploymentScriptHash(
      parsedDeploymentInfo,
      "fraudProofDoubleSpend",
    );
    const deploymentNonExistentInputScriptHash = optionalDeploymentScriptHash(
      parsedDeploymentInfo,
      "fraudProofNonExistentInput",
    );
    const deploymentInvalidRangeScriptHash = optionalDeploymentScriptHash(
      parsedDeploymentInfo,
      "fraudProofInvalidRange",
    );
    const deploymentZeroInputScriptHash = optionalDeploymentScriptHash(
      parsedDeploymentInfo,
      "fraudProofZeroInput",
    );
    const deploymentTransitionTraceScriptHash = optionalDeploymentScriptHash(
      parsedDeploymentInfo,
      "fraudProofTransitionTrace",
    );
    const deploymentValidationTraceDisputeScriptHash =
      optionalDeploymentScriptHash(
        parsedDeploymentInfo,
        "validationTraceDispute",
      );
    const deployedFraudProofCatalogue =
      parsedDeploymentInfo.fraudProofCatalogueMint?.fraudProofCatalogue;

    const contracts = yield* buildFaultProofContracts({
      blueprint: parsedBlueprint,
      network,
      hubOraclePolicyId,
      fraudProofCataloguePolicyId,
    });

    expectScriptHash(
      "fraudProofMint.scriptHash",
      contracts.fraudProof.policyId,
      deploymentFraudProofPolicyId,
    );
    expectScriptHash(
      "fraudProofSpend.scriptHash",
      contracts.fraudProof.spendingScriptHash,
      deploymentFraudProofSpendingHash,
    );

    const [step01, step02, step03, step04] = contracts.doubleSpend.steps;
    const [
      nonExistentInputStep01,
      nonExistentInputStep02,
      nonExistentInputStep03,
      nonExistentInputStep04,
    ] = contracts.nonExistentInput.steps;
    const stepOutput = (
      name: InspectContractsStepOutput["name"],
      step: typeof step01,
    ): InspectContractsStepOutput => {
      const standaloneScriptBytes = Buffer.from(
        step.spendingScriptCBOR,
        "hex",
      ).byteLength;
      return {
        name,
        scriptHash: step.spendingScriptHash,
        address: step.spendingScriptAddress,
        standaloneScriptBytes,
        withinL1TransactionByteEnvelopeNecessaryCondition:
          standaloneScriptBytes <
          MIDGARD_CONSENSUS_LIMITS_V1.minSupportedL1MaxTxBytes,
      };
    };
    const steps: InspectContractsOutput["doubleSpend"]["steps"] = [
      stepOutput("step01", step01),
      stepOutput("step02", step02),
      stepOutput("step03", step03),
      stepOutput("step04", step04),
    ];
    const nonExistentInputSteps: InspectContractsOutput["nonExistentInput"]["steps"] =
      [
        stepOutput("step01", nonExistentInputStep01),
        stepOutput("step02", nonExistentInputStep02),
        stepOutput("step03", nonExistentInputStep03),
        stepOutput("step04", nonExistentInputStep04),
      ];
    const invalidRangeSteps: InspectContractsOutput["invalidRange"]["steps"] = [
      stepOutput("step01", contracts.invalidRange.steps[0]),
      stepOutput("step02", contracts.invalidRange.steps[1]),
    ];
    const zeroInputSteps: InspectContractsOutput["zeroInput"]["steps"] = [
      stepOutput("step01", contracts.zeroInput.steps[0]),
      stepOutput("step02", contracts.zeroInput.steps[1]),
    ];
    const transitionTraceSteps: InspectContractsOutput["transitionTrace"]["steps"] =
      [
        stepOutput("route", contracts.transitionTrace.route),
        stepOutput("control", contracts.transitionTrace.finals[0]),
        stepOutput("source", contracts.transitionTrace.finals[1]),
        stepOutput("withdrawal", contracts.transitionTrace.finals[2]),
        stepOutput("forced", contracts.transitionTrace.finals[3]),
        stepOutput("accepted", contracts.transitionTrace.finals[4]),
        stepOutput("deposit", contracts.transitionTrace.finals[5]),
        stepOutput("l1Event", contracts.transitionTrace.finals[6]),
        stepOutput("duplicate", contracts.transitionTrace.finals[7]),
      ];
    const validationTraceDisputeSteps: InspectContractsOutput["validationTraceDispute"]["steps"] =
      [
        stepOutput("dispute", contracts.validationTraceDispute.opener),
        stepOutput("source", contracts.validationTraceDispute.source),
        stepOutput("game", contracts.validationTraceDispute.game),
        stepOutput("boundary", contracts.validationTraceDispute.boundary),
        stepOutput("timeout", contracts.validationTraceDispute.timeout),
        stepOutput("award", contracts.validationTraceDispute.award),
        ...contracts.validationTraceDispute.semanticResolvers.map(
          (resolver, resolverIndex) =>
            stepOutput(`semantic-resolver-${resolverIndex}`, resolver),
        ),
        ...contracts.validationTraceDispute.prepareResolvers.map(
          (resolver, resolverIndex) =>
            stepOutput(`prepare-resolver-${resolverIndex}`, resolver),
        ),
        ...contracts.validationTraceDispute.directResolvers.map(
          (resolver, resolverIndex) =>
            stepOutput(`direct-resolver-${resolverIndex}`, resolver),
        ),
      ];
    const categorizedAppliedSpendingScripts: readonly {
      readonly category: InspectContractsProofCategory;
      readonly step: InspectContractsStepOutput;
    }[] = [
      ...steps.map((step) => ({ category: "doubleSpend" as const, step })),
      ...nonExistentInputSteps.map((step) => ({
        category: "nonExistentInput" as const,
        step,
      })),
      ...invalidRangeSteps.map((step) => ({
        category: "invalidRange" as const,
        step,
      })),
      ...zeroInputSteps.map((step) => ({
        category: "zeroInput" as const,
        step,
      })),
      ...transitionTraceSteps.map((step) => ({
        category: "transitionTrace" as const,
        step,
      })),
      ...validationTraceDisputeSteps.map((step) => ({
        category: "validationTraceDispute" as const,
        step,
      })),
    ];
    const oversizedAppliedSpendingScripts =
      categorizedAppliedSpendingScripts.flatMap(({ category, step }) =>
        step.withinL1TransactionByteEnvelopeNecessaryCondition
          ? []
          : [
              {
                category,
                name: step.name,
                scriptHash: step.scriptHash,
                standaloneScriptBytes: step.standaloneScriptBytes,
              },
            ],
      );
    const categoryFirstStepHash =
      contracts.doubleSpend.firstStep.spendingScriptHash;
    const nonExistentInputCategoryFirstStepHash =
      contracts.nonExistentInput.firstStep.spendingScriptHash;
    const invalidRangeCategoryFirstStepHash =
      contracts.invalidRange.firstStep.spendingScriptHash;
    const zeroInputCategoryFirstStepHash =
      contracts.zeroInput.firstStep.spendingScriptHash;
    const transitionTraceCategoryFirstStepHash =
      contracts.transitionTrace.firstStep.spendingScriptHash;
    const validationTraceDisputeCategoryFirstStepHash =
      contracts.validationTraceDispute.firstStep.spendingScriptHash;
    const deploymentDoubleSpendMatchesFirstStep =
      deploymentDoubleSpendScriptHash === null
        ? null
        : deploymentDoubleSpendScriptHash === categoryFirstStepHash;
    const deploymentNonExistentInputMatchesFirstStep =
      deploymentNonExistentInputScriptHash === null
        ? null
        : deploymentNonExistentInputScriptHash ===
          nonExistentInputCategoryFirstStepHash;
    const deploymentInvalidRangeMatchesFirstStep =
      deploymentInvalidRangeScriptHash === null
        ? null
        : deploymentInvalidRangeScriptHash ===
          invalidRangeCategoryFirstStepHash;
    const deploymentZeroInputMatchesFirstStep =
      deploymentZeroInputScriptHash === null
        ? null
        : deploymentZeroInputScriptHash === zeroInputCategoryFirstStepHash;
    const deploymentTransitionTraceMatchesFirstStep =
      deploymentTransitionTraceScriptHash === null
        ? null
        : deploymentTransitionTraceScriptHash ===
          transitionTraceCategoryFirstStepHash;
    const deploymentValidationTraceDisputeMatchesFirstStep =
      deploymentValidationTraceDisputeScriptHash === null
        ? null
        : deploymentValidationTraceDisputeScriptHash ===
          validationTraceDisputeCategoryFirstStepHash;
    const fraudProofCatalogue = yield* inspectFraudProofCatalogue(
      deployedFraudProofCatalogue,
      {
        doubleSpend: categoryFirstStepHash,
        nonExistentInput: nonExistentInputCategoryFirstStepHash,
        invalidRange: invalidRangeCategoryFirstStepHash,
        zeroInput: zeroInputCategoryFirstStepHash,
        transitionTrace: transitionTraceCategoryFirstStepHash,
        validationTraceDispute: validationTraceDisputeCategoryFirstStepHash,
      },
      {
        doubleSpend: deploymentDoubleSpendMatchesFirstStep,
        nonExistentInput: deploymentNonExistentInputMatchesFirstStep,
        invalidRange: deploymentInvalidRangeMatchesFirstStep,
        zeroInput: deploymentZeroInputMatchesFirstStep,
        transitionTrace: deploymentTransitionTraceMatchesFirstStep,
        validationTraceDispute:
          deploymentValidationTraceDisputeMatchesFirstStep,
      },
    );

    return {
      network,
      l1SpendingScriptEnvelopeNecessaryCondition: {
        maxTransactionBytes:
          MIDGARD_CONSENSUS_LIMITS_V1.minSupportedL1MaxTxBytes,
        appliedSpendingScriptCount: categorizedAppliedSpendingScripts.length,
        allAppliedSpendingScriptsWithinEnvelope:
          oversizedAppliedSpendingScripts.length === 0,
        oversizedAppliedSpendingScripts,
      },
      computationThread: {
        policyId: contracts.computationThread.policyId,
      },
      fraudProof: {
        policyId: contracts.fraudProof.policyId,
        address: contracts.fraudProof.spendingScriptAddress,
        spendingScriptHash: contracts.fraudProof.spendingScriptHash,
      },
      fraudProofCatalogue,
      doubleSpend: {
        categoryFirstStepHash,
        deploymentDoubleSpendScriptHash,
        deploymentDoubleSpendMatchesFirstStep,
        steps,
      },
      nonExistentInput: {
        categoryFirstStepHash: nonExistentInputCategoryFirstStepHash,
        deploymentNonExistentInputScriptHash,
        deploymentNonExistentInputMatchesFirstStep,
        steps: nonExistentInputSteps,
      },
      invalidRange: {
        categoryFirstStepHash: invalidRangeCategoryFirstStepHash,
        deploymentInvalidRangeScriptHash,
        deploymentInvalidRangeMatchesFirstStep,
        steps: invalidRangeSteps,
      },
      zeroInput: {
        categoryFirstStepHash: zeroInputCategoryFirstStepHash,
        deploymentZeroInputScriptHash,
        deploymentZeroInputMatchesFirstStep,
        steps: zeroInputSteps,
      },
      transitionTrace: {
        categoryFirstStepHash: transitionTraceCategoryFirstStepHash,
        deploymentTransitionTraceScriptHash,
        deploymentTransitionTraceMatchesFirstStep,
        steps: transitionTraceSteps,
      },
      validationTraceDispute: {
        categoryFirstStepHash: validationTraceDisputeCategoryFirstStepHash,
        deploymentValidationTraceDisputeScriptHash,
        deploymentValidationTraceDisputeMatchesFirstStep,
        steps: validationTraceDisputeSteps,
      },
    };
  });

export const inspectContractsFromFiles = async ({
  blueprintPath,
  deploymentInfoPath,
  network = DEFAULT_FAULT_PROOF_NETWORK,
}: InspectContractsFromFilesParams): Promise<InspectContractsOutput> => {
  const [blueprint, deploymentInfo] = await Promise.all([
    readJsonFile(blueprintPath),
    readJsonFile(deploymentInfoPath),
  ]);

  return await Effect.runPromise(
    inspectContracts({ blueprint, deploymentInfo, network }),
  );
};
