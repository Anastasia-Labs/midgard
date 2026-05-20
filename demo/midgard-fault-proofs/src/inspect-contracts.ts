import { Effect } from "effect";
import { Network, Data, type Script } from "@lucid-evolution/lucid";
import { Store, Trie } from "@aiken-lang/merkle-patricia-forestry";
import {
  FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER,
  FRAUD_PROOF_CATALOGUE_ID_BYTE_COUNT,
  EMPTY_MERKLE_TREE_ROOT,
  type FraudProofCatalogueCategoryDeploymentInfo,
  type FraudProofCatalogueDeploymentInfo,
  Proof,
  ScriptHashSchema,
  buildDoubleSpendFaultProofContracts,
  parseFaultProofBlueprint,
} from "@al-ft/midgard-sdk";
import {
  parseSafeNonNegativeInteger,
  parseStrictHex,
  readJsonFile,
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
    readonly doubleSpend: {
      readonly categoryId: string | null;
      readonly scriptHash: string | null;
      readonly scriptHashMatchesFirstStep: boolean | null;
      readonly membershipProofCbor: string | null;
      readonly membershipProofMatchesDerived: boolean | null;
    };
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
};

export type InspectContractsStepOutput = {
  readonly name: "step01" | "step02" | "step03" | "step04";
  readonly scriptHash: string;
  readonly address: string;
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
  byteLength: number,
): string =>
  parseStrictHex(value, {
    byteCount: byteLength,
    typeError: `${label} must be a hex string`,
    invalidError: `${label} must be ${byteLength.toString()} bytes of hex`,
  });

const normalizeVariableHex = (value: unknown, label: string): string =>
  parseStrictHex(value, {
    typeError: `${label} must be a hex string`,
    invalidError: `${label} must be even-length hex`,
  });

const parseCatalogueCategoryDeploymentInfo = (
  value: unknown,
  label: string,
): FraudProofCatalogueCategoryDeploymentInfo => {
  if (typeof value !== "object" || value === null || Array.isArray(value)) {
    throw new Error(`${label} must be an object`);
  }
  const candidate = value as {
    readonly categoryId?: unknown;
    readonly scriptHash?: unknown;
    readonly membershipProofCbor?: unknown;
  };
  const membershipProofCbor = normalizeVariableHex(
    candidate.membershipProofCbor,
    `${label}.membershipProofCbor`,
  );
  try {
    Data.from(membershipProofCbor, Proof);
  } catch (cause) {
    throw new Error(
      `${label}.membershipProofCbor is not a valid Proof CBOR: ${String(cause)}`,
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
  if (typeof value !== "object" || value === null || Array.isArray(value)) {
    throw new Error("fraudProofCatalogue must be an object");
  }
  const candidate = value as {
    readonly root?: unknown;
    readonly categories?: unknown;
  };
  if (
    typeof candidate.categories !== "object" ||
    candidate.categories === null ||
    Array.isArray(candidate.categories)
  ) {
    throw new Error("fraudProofCatalogue.categories must be an object");
  }
  const rawCategories = candidate.categories as Record<string, unknown>;
  const categories = Object.fromEntries(
    FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER.map((name) => {
      const category = rawCategories[name];
      if (category === undefined) {
        throw new Error(`fraudProofCatalogue.categories.${name} is missing`);
      }
      return [
        name,
        parseCatalogueCategoryDeploymentInfo(
          category,
          `fraudProofCatalogue.categories.${name}`,
        ),
      ];
    }),
  ) as FraudProofCatalogueDeploymentInfo["categories"];

  return {
    root: normalizeHex(candidate.root, "fraudProofCatalogue.root", 32),
    categories,
  };
};

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
  if (typeof value !== "object" || Array.isArray(value)) {
    throw new Error(`${label} must be an object or null`);
  }
  const candidate = value as {
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
  if (typeof value !== "object" || value === null || Array.isArray(value)) {
    throw new Error(`${label} must be an object`);
  }
  const candidate = value as {
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
    cborHex: normalizeVariableHex(candidate.cborHex, `${label}.cborHex`),
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
  if (hash === null || hash === undefined) {
    return EMPTY_MERKLE_TREE_ROOT;
  }
  return Buffer.from(hash).toString("hex");
};

export const parseContractDeploymentInfo = (
  value: unknown,
): ContractDeploymentInfo => {
  if (typeof value !== "object" || value === null || Array.isArray(value)) {
    throw new Error("Contract deployment info must be a JSON object");
  }

  const entries: Record<string, ContractDeploymentInfoEntry> = {};
  for (const [name, entry] of Object.entries(value)) {
    if (typeof entry !== "object" || entry === null) {
      throw new Error(`Deployment entry "${name}" must be an object`);
    }
    const candidate = entry as {
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

const requireDeploymentScriptHash = (
  deploymentInfo: ContractDeploymentInfo,
  name: string,
): string => {
  const entry = deploymentInfo[name];
  if (entry === undefined) {
    throw new Error(`Deployment info is missing "${name}"`);
  }
  return entry.scriptHash.toLowerCase();
};

const optionalDeploymentScriptHash = (
  deploymentInfo: ContractDeploymentInfo,
  name: string,
): string | null => deploymentInfo[name]?.scriptHash.toLowerCase() ?? null;

const expectScriptHash = (
  label: string,
  actual: string,
  expected: string,
): void => {
  if (actual.toLowerCase() !== expected.toLowerCase()) {
    throw new Error(
      `${label} mismatch: derived=${actual.toLowerCase()} deployment=${expected.toLowerCase()}`,
    );
  }
};

const inspectFraudProofCatalogue = (
  catalogue: FraudProofCatalogueDeploymentInfo | undefined,
  expectedDoubleSpendFirstStepHash: string,
  deploymentDoubleSpendMatchesFirstStep: boolean | null,
): Effect.Effect<InspectContractsOutput["fraudProofCatalogue"], Error> => {
  if (catalogue === undefined) {
    return Effect.succeed({
      root: null,
      derivedRoot: null,
      rootMatchesDerived: null,
      initReady: false,
      doubleSpend: {
        categoryId: null,
        scriptHash: null,
        scriptHashMatchesFirstStep: null,
        membershipProofCbor: null,
        membershipProofMatchesDerived: null,
      },
    });
  }

  return Effect.tryPromise({
    try: async () => {
      const store = new Store(undefined);
      await store.ready();
      const trie = new Trie(store);
      for (const categoryName of FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER) {
        const category = catalogue.categories[categoryName];
        await trie.insert(
          encodeCatalogueKey(category.categoryId),
          encodeCatalogueValue(category.scriptHash),
        );
      }

      const derivedRoot = trieRootHex(trie);
      const doubleSpend = catalogue.categories.doubleSpend;
      const doubleSpendProof = await trie.prove(
        encodeCatalogueKey(doubleSpend.categoryId),
      );
      const derivedDoubleSpendProofCbor = doubleSpendProof
        .toCBOR()
        .toString("hex");
      const scriptHashMatchesFirstStep =
        doubleSpend.scriptHash === expectedDoubleSpendFirstStepHash;
      const rootMatchesDerived = catalogue.root === derivedRoot;
      const membershipProofMatchesDerived =
        doubleSpend.membershipProofCbor === derivedDoubleSpendProofCbor;

      return {
        root: catalogue.root,
        derivedRoot,
        rootMatchesDerived,
        initReady:
          deploymentDoubleSpendMatchesFirstStep === true &&
          scriptHashMatchesFirstStep &&
          rootMatchesDerived &&
          membershipProofMatchesDerived,
        doubleSpend: {
          categoryId: doubleSpend.categoryId,
          scriptHash: doubleSpend.scriptHash,
          scriptHashMatchesFirstStep,
          membershipProofCbor: doubleSpend.membershipProofCbor,
          membershipProofMatchesDerived,
        },
      };
    },
    catch: (cause) =>
      new Error(
        `Failed to inspect fraud-proof catalogue deployment info: ${String(cause)}`,
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
    const deployedFraudProofCatalogue =
      parsedDeploymentInfo.fraudProofCatalogueMint?.fraudProofCatalogue;

    const contracts = yield* buildDoubleSpendFaultProofContracts({
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
    const stepOutput = (
      name: InspectContractsStepOutput["name"],
      step: typeof step01,
    ): InspectContractsStepOutput => ({
      name,
      scriptHash: step.spendingScriptHash,
      address: step.spendingScriptAddress,
    });
    const steps: InspectContractsOutput["doubleSpend"]["steps"] = [
      stepOutput("step01", step01),
      stepOutput("step02", step02),
      stepOutput("step03", step03),
      stepOutput("step04", step04),
    ];
    const categoryFirstStepHash =
      contracts.doubleSpend.firstStep.spendingScriptHash;
    const deploymentDoubleSpendMatchesFirstStep =
      deploymentDoubleSpendScriptHash === null
        ? null
        : deploymentDoubleSpendScriptHash === categoryFirstStepHash;
    const fraudProofCatalogue = yield* inspectFraudProofCatalogue(
      deployedFraudProofCatalogue,
      categoryFirstStepHash,
      deploymentDoubleSpendMatchesFirstStep,
    );

    return {
      network,
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
