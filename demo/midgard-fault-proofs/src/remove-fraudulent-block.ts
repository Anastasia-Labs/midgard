import { formatUnknownError } from "@al-ft/midgard-core";
import {
  ACTIVE_OPERATOR_NODE_ASSET_NAME_PREFIX,
  ACTIVE_OPERATORS_ROOT_ASSET_NAME,
  ActiveOperatorMintRedeemer,
  type ActiveOperatorMintRedeemer as ActiveOperatorMintRedeemerData,
  buildDoubleSpendFaultProofContracts,
  buildInvalidRangeFaultProofContracts,
  buildNonExistentInputFaultProofContracts,
  buildTransitionTraceFaultProofContracts,
  buildValidationTraceDisputeFaultProofContracts,
  buildZeroInputFaultProofContracts,
  type DoubleSpendFaultProofContracts,
  encodeLinkedListNodeView,
  FRAUD_PROOF_CATALOGUE_ID_BYTE_COUNT,
  type FraudProofCatalogueCategoryName,
  FraudProofTokenDatum,
  getHeaderV1FromStateQueueDatum,
  getLinkedListNodeViewFromUTxO,
  hashBlockHeaderV1,
  HUB_ORACLE_ASSET_NAME,
  incompleteRemoveFraudulentBlocksLinkTxProgram,
  incompleteRemoveLastFraudulentBlockHeaderTxProgram,
  type InvalidRangeFaultProofContracts,
  type LinkedListNodeView,
  type NonExistentInputFaultProofContracts,
  type OutputReference,
  outputReferenceFromUTxO,
  parseFaultProofBlueprint,
  REGISTERED_OPERATORS_ROOT_ASSET_NAME,
  requireInputIndex,
  requireMintRedeemerIndex,
  requireOwnMintPurpose,
  requireOwnSpendPurpose,
  requireReferenceInputIndex,
  requireSpendRedeemerIndex,
  requireUniqueOutputIndex,
  RETIRED_OPERATOR_NODE_ASSET_NAME_PREFIX,
  RETIRED_OPERATORS_ROOT_ASSET_NAME,
  RetiredOperatorMintRedeemer,
  type RetiredOperatorMintRedeemer as RetiredOperatorMintRedeemerData,
  SCHEDULER_ASSET_NAME,
  SchedulerDatum,
  SchedulerSpendRedeemer,
  type SchedulerSpendRedeemer as SchedulerSpendRedeemerData,
  type SlashingApproach as SlashingApproachData,
  STATE_QUEUE_NODE_ASSET_NAME_PREFIX,
  STATE_QUEUE_ROOT_ASSET_NAME,
  StateQueueRedeemer,
  type StateQueueRedeemer as StateQueueRedeemerData,
  type StateQueueRemoveReferenceScriptUTxOs,
  type StateQueueUTxO,
  type TransitionTraceFaultProofContracts,
  utxoToStateQueueUTxO,
  type ValidationTraceDisputeFaultProofContracts,
  type ZeroInputFaultProofContracts,
} from "@al-ft/midgard-sdk";
import {
  type BuildTxWithRedeemer,
  credentialToAddress,
  Data,
  type LucidEvolution,
  type Network,
  type Script,
  scriptHashToCredential,
  type SpendingValidator,
  toUnit,
  type TxOutput,
  type UTxO,
  validatorToAddress,
  validatorToScriptHash,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";

import {
  type ContractDeploymentInfo,
  parseContractDeploymentInfo,
} from "./inspect-contracts.js";
import { parseHex } from "./json-file.js";
import {
  DEFAULT_CONFIRMATION_POLL_MS,
  fetchUtxoByOutRef,
  makeLucidForSubmit,
  outRefLabel,
  readJsonFile,
  requireDeploymentScriptHash,
  requireMatchingScriptHash,
  requireSingletonUtxo,
  type ResolvedProverSigner,
  resolveProverSigner,
  type SubmitProviderConfig,
} from "./runtime.js";
import { selectFeeInput } from "./submit-step-01.js";

const DEFAULT_REMOVE_VALIDITY_WINDOW_MS = 300_000n;
const DEFAULT_REMOVE_VALIDITY_BACKDATE_MS = 120_000n;
const DEFAULT_NODE_ADMIN_KEY_ENV = "MIDGARD_NODE_ADMIN_KEY";
const STATE_QUEUE_MUTATION_LEASE_ENDPOINT = "stateQueueMutationLease";
const FAULT_PROOF_LEASE_HOLDER = "fault_proof_removal";

const REFERENCE_SCRIPT_NAMES = [
  "stateQueueSpend",
  "stateQueueMint",
  "activeOperatorsSpend",
  "activeOperatorsMint",
  "retiredOperatorsSpend",
  "retiredOperatorsMint",
  "schedulerSpend",
] as const;

type ReferenceScriptName = (typeof REFERENCE_SCRIPT_NAMES)[number];

type DeploymentScriptName = ReferenceScriptName | "registeredOperatorsSpend";

type RemoveFraudulentBlockContracts = {
  readonly stateQueuePolicyId: string;
  readonly stateQueueAddress: string;
  readonly stateQueueSpendingScript: Script;
  readonly stateQueueMintingScript: Script;
  readonly activeOperatorsPolicyId: string;
  readonly activeOperatorsAddress: string;
  readonly activeOperatorsSpendingScript: Script;
  readonly activeOperatorsMintingScript: Script;
  readonly retiredOperatorsPolicyId: string;
  readonly retiredOperatorsAddress: string;
  readonly retiredOperatorsSpendingScript: Script;
  readonly retiredOperatorsMintingScript: Script;
  readonly schedulerPolicyId: string;
  readonly schedulerAddress: string;
  readonly schedulerSpendingScript: Script;
  readonly hubOraclePolicyId: string;
  readonly registeredOperatorsPolicyId: string;
  readonly registeredOperatorsAddress: string;
  readonly fraudProofPolicyId: string;
  readonly fraudProofAddress: string;
  readonly fraudCategoryId: string;
  readonly fraudCategory: RemoveFraudulentBlockFraudCategory;
};

type RemoveFraudulentBlockLayout = {
  readonly fraudProofRefInputIndex: bigint;
  readonly stateQueueRedeemerTxInfoIndex: bigint;
  readonly activeOperatorsRedeemerTxInfoIndex?: bigint;
  readonly retiredOperatorsRedeemerTxInfoIndex?: bigint;
  readonly activeOperatorsElementRefInputIndex?: bigint;
  readonly retiredOperatorsElementRefInputIndex?: bigint;
  readonly anchorElementOutputIndex?: bigint;
  readonly fraudulentNodeOutputIndex?: bigint;
};

type OperatorSlashingLayout = {
  readonly activeOperatorsRedeemerTxInfoIndex?: bigint;
  readonly retiredOperatorsRedeemerTxInfoIndex?: bigint;
  readonly stateQueueRedeemerTxInfoIndex: bigint;
  readonly operatorDirectoryAnchorInputOutRef: OutputReference;
  readonly operatorDirectoryAnchorOutputIndex: bigint;
  readonly schedulerRefInputIndex?: bigint;
  readonly schedulerInputIndex?: bigint;
  readonly schedulerOutputIndex?: bigint;
  readonly schedulerRedeemerTxInfoIndex?: bigint;
  readonly activeOperatorsLastNodeRefInputIndex?: bigint;
  readonly hubOracleRefInputIndex: bigint;
  readonly registeredOperatorsRootRefInputIndex?: bigint;
};

const REMOVE_LAYOUT_KEYS = [
  "fraudProofRefInputIndex",
  "stateQueueRedeemerTxInfoIndex",
  "activeOperatorsRedeemerTxInfoIndex",
  "retiredOperatorsRedeemerTxInfoIndex",
  "activeOperatorsElementRefInputIndex",
  "retiredOperatorsElementRefInputIndex",
  "anchorElementOutputIndex",
  "fraudulentNodeOutputIndex",
] as const satisfies readonly (keyof RemoveFraudulentBlockLayout)[];

type OperatorListEntry = {
  readonly utxo: UTxO;
  readonly view: LinkedListNodeView;
};

type OperatorListRemovalPlan = {
  readonly anchor: OperatorListEntry;
  readonly node: OperatorListEntry;
  readonly lastNodeAfterRemoval?: OperatorListEntry;
};

type StateQueueTopology = {
  readonly root: StateQueueUTxO;
  readonly ordered: readonly StateQueueUTxO[];
  readonly nodeByHeaderHash: ReadonlyMap<string, StateQueueUTxO>;
  readonly predecessorByHeaderHash: ReadonlyMap<string, StateQueueUTxO>;
  readonly successorByHeaderHash: ReadonlyMap<string, StateQueueUTxO>;
};

type RemoveTransactionKind = "remove-successor" | "remove-target";

type RemoveTransactionResult = {
  readonly kind: RemoveTransactionKind;
  readonly txHash: string;
  readonly removedHeaderHash: string;
  readonly removedOperator: string;
  readonly stateQueueBlockOutRef: string;
  readonly operatorNodeOutRef: string | null;
  readonly registeredOperatorsRootOutRef: string | null;
  readonly slashingApproach:
    | "SlashActiveOperator"
    | "SlashRetiredOperator"
    | "OperatorAlreadySlashed";
  readonly layout: Record<keyof RemoveFraudulentBlockLayout, string | null>;
};

type SchedulerRemovalPlan =
  | {
      readonly kind: "inactive";
    }
  | {
      readonly kind: "goToAnchor";
      readonly newOperator: string;
      readonly removedNodeIsLast: boolean;
    }
  | {
      readonly kind: "rewind";
      readonly newOperator?: string;
      readonly removedNodeIsLast: boolean;
    };

type RemoveFraudulentBlockSlashing = Parameters<
  typeof incompleteRemoveLastFraudulentBlockHeaderTxProgram
>[2]["slashing"];

type OperatorSlashingLayoutContext =
  | {
      readonly operatorDirectoryAnchor: UTxO;
      readonly operatorDirectoryNode: UTxO;
      readonly scheduler: UTxO;
      readonly schedulerPlan: SchedulerRemovalPlan;
      readonly hubOracle: UTxO;
      readonly registeredOperatorsRoot?: UTxO;
      readonly activeOperatorsLastNode?: UTxO;
      readonly operatorDirectoryAnchorUnit: string;
      readonly slashedOperatorDirectory: "active";
      readonly schedulerUnit: string;
      readonly contracts: RemoveFraudulentBlockContracts;
    }
  | {
      readonly operatorDirectoryAnchor: UTxO;
      readonly operatorDirectoryNode: UTxO;
      readonly hubOracle: UTxO;
      readonly operatorDirectoryAnchorUnit: string;
      readonly slashedOperatorDirectory: "retired";
      readonly contracts: RemoveFraudulentBlockContracts;
    };

export type RemoveFraudulentBlockCliConfig = SubmitProviderConfig & {
  readonly blueprintPath: string;
  readonly deploymentInfoPath: string;
  readonly walletSeedPhrase?: string;
  readonly walletSeedPhraseEnv?: string;
  readonly walletPrivateKey?: string;
  readonly walletPrivateKeyEnv?: string;
  readonly fraudCategory?: RemoveFraudulentBlockFraudCategory;
  readonly fraudulentHeaderHash: string;
  readonly awaitConfirmation?: boolean;
  readonly midgardNodeUrl?: string;
  readonly midgardNodeAdminKey?: string;
  readonly midgardNodeAdminKeyEnv?: string;
  readonly stateQueueLeaseTtlMs?: number;
};

export type RemoveFraudulentBlockFraudCategory = Extract<
  FraudProofCatalogueCategoryName,
  | "doubleSpend"
  | "nonExistentInput"
  | "invalidRange"
  | "transitionTrace"
  | "zeroInput"
  | "validationTraceDispute"
>;

export type StateQueueMutationLease = {
  readonly token: string;
  readonly source: string;
  readonly renew: () => Promise<void>;
  readonly release: () => Promise<void>;
  readonly fail: (error: string) => Promise<void>;
};

export type StateQueueMutationLeaseCoordinator = {
  readonly acquire: () => Promise<StateQueueMutationLease>;
};

export type SubmitRemoveFraudulentBlockResult = {
  readonly txHash: string;
  readonly walletSource: string;
  readonly proverAddress: string;
  readonly fraudProver: string;
  readonly fraudCategory: RemoveFraudulentBlockFraudCategory;
  readonly fraudCategoryId: string;
  readonly fraudulentHeaderHash: string;
  readonly stateQueueBlockOutRef: string;
  readonly stateQueueRootOutRef: string;
  readonly fraudProofOutRef: string;
  readonly activeOperatorsRootOutRef: string | null;
  readonly activeOperatorNodeOutRef: string | null;
  readonly schedulerOutRef: string;
  readonly hubOracleOutRef: string;
  readonly registeredOperatorsRootOutRef: string | null;
  readonly referenceScriptOutRefs: Readonly<
    Record<ReferenceScriptName, string | null>
  >;
  readonly transactions: readonly RemoveTransactionResult[];
  readonly layout: Record<keyof RemoveFraudulentBlockLayout, string | null>;
  readonly awaitedConfirmation: boolean;
  readonly stateQueueMutationLease: {
    readonly token: string;
    readonly source: string;
    readonly released: boolean;
  } | null;
};

const normalizeNonEmpty = (value: string | undefined): string | undefined => {
  const trimmed = value?.trim() ?? "";
  return trimmed.length === 0 ? undefined : trimmed;
};

const normalizeNodeUrl = (url: string): string => {
  const trimmed = url.trim();
  if (trimmed.length === 0) {
    throw new Error("--midgard-node-url must not be empty.");
  }
  return trimmed.replace(/\/+$/, "");
};

const parsePositiveSafeInteger = (
  value: number | undefined,
  label: string,
): number | undefined => {
  if (value === undefined) {
    return undefined;
  }
  if (!Number.isSafeInteger(value) || value <= 0) {
    throw new Error(`${label} must be a positive safe integer.`);
  }
  return value;
};

const resolveNodeAdminKey = (
  config: RemoveFraudulentBlockCliConfig,
  env: NodeJS.ProcessEnv = process.env,
): string => {
  const envName =
    normalizeNonEmpty(config.midgardNodeAdminKeyEnv) ??
    DEFAULT_NODE_ADMIN_KEY_ENV;
  const resolved =
    normalizeNonEmpty(config.midgardNodeAdminKey) ??
    normalizeNonEmpty(env[envName]);
  if (resolved === undefined) {
    throw new Error(
      `Midgard node admin key is required for coordinated state-queue removal; pass --midgard-node-admin-key or set ${envName}.`,
    );
  }
  return resolved;
};

const readJsonResponse = async (response: Response): Promise<unknown> => {
  const text = await response.text();
  if (text.trim().length === 0) {
    return {};
  }
  try {
    return JSON.parse(text);
  } catch {
    return { error: text };
  }
};

const responseError = (json: unknown): string => {
  if (typeof json === "object" && json !== null && "error" in json) {
    const error = (json as { readonly error?: unknown }).error;
    if (typeof error === "string") {
      return error;
    }
  }
  return JSON.stringify(json);
};

export const createHttpStateQueueMutationLeaseCoordinator = ({
  midgardNodeUrl,
  adminKey,
  ttlMs,
}: {
  readonly midgardNodeUrl: string;
  readonly adminKey: string;
  readonly ttlMs?: number;
}): StateQueueMutationLeaseCoordinator => {
  const nodeUrl = normalizeNodeUrl(midgardNodeUrl);
  const normalizedTtlMs = parsePositiveSafeInteger(
    ttlMs,
    "--state-queue-lease-ttl-ms",
  );
  const endpoint = `${nodeUrl}/${STATE_QUEUE_MUTATION_LEASE_ENDPOINT}`;
  const post = async (body: Record<string, unknown>): Promise<unknown> => {
    const response = await fetch(endpoint, {
      method: "POST",
      headers: {
        "content-type": "application/json",
        "x-midgard-admin-key": adminKey,
      },
      body: JSON.stringify(body),
    });
    const json = await readJsonResponse(response);
    if (!response.ok) {
      throw new Error(
        `POST /${STATE_QUEUE_MUTATION_LEASE_ENDPOINT} ${String(
          body.action,
        )} failed with HTTP ${response.status.toString()}: ${responseError(json)}`,
      );
    }
    return json;
  };

  return {
    acquire: async () => {
      const json = await post({
        action: "acquire",
        holder: FAULT_PROOF_LEASE_HOLDER,
        ...(normalizedTtlMs === undefined ? {} : { ttlMs: normalizedTtlMs }),
      });
      if (
        typeof json !== "object" ||
        json === null ||
        (json as { readonly status?: unknown }).status !== "acquired" ||
        typeof (json as { readonly token?: unknown }).token !== "string"
      ) {
        throw new Error(
          `Unexpected state-queue mutation lease acquire response: ${JSON.stringify(json)}`,
        );
      }
      const token = (json as { readonly token: string }).token;
      const leaseBody = (action: "renew" | "release" | "fail", extra = {}) =>
        post({
          action,
          token,
          ...(normalizedTtlMs === undefined ? {} : { ttlMs: normalizedTtlMs }),
          ...extra,
        });
      return {
        token,
        source: nodeUrl,
        renew: async () => {
          await leaseBody("renew");
        },
        release: async () => {
          await leaseBody("release");
        },
        fail: async (error: string) => {
          await leaseBody("fail", { error });
        },
      };
    },
  };
};

const resolveStateQueueLeaseCoordinator = (
  config: RemoveFraudulentBlockCliConfig,
): StateQueueMutationLeaseCoordinator | undefined => {
  if (config.midgardNodeUrl === undefined) {
    return undefined;
  }
  return createHttpStateQueueMutationLeaseCoordinator({
    midgardNodeUrl: config.midgardNodeUrl,
    adminKey: resolveNodeAdminKey(config),
    ttlMs: config.stateQueueLeaseTtlMs,
  });
};

const requireOutputIndexByUnit = ({
  outputs,
  address,
  unit,
  label,
}: {
  readonly outputs: readonly TxOutput[];
  readonly address: string;
  readonly unit: string;
  readonly label: string;
}): bigint =>
  requireUniqueOutputIndex(
    outputs,
    (output) =>
      output.address === address && (output.assets[unit] ?? 0n) === 1n,
    label,
  );

const layoutToJson = (
  layout: RemoveFraudulentBlockLayout,
): Record<keyof RemoveFraudulentBlockLayout, string | null> =>
  Object.fromEntries(
    REMOVE_LAYOUT_KEYS.map((key) => [
      key,
      layout[key] === undefined ? null : layout[key].toString(),
    ]),
  ) as Record<keyof RemoveFraudulentBlockLayout, string | null>;

const requireDeploymentScript = (
  deploymentInfo: ContractDeploymentInfo,
  name: DeploymentScriptName,
): Script => {
  const entry = deploymentInfo[name];
  if (entry === undefined) {
    throw new Error(`Deployment info is missing "${name}"`);
  }
  if (entry.contract === undefined) {
    throw new Error(
      `Deployment info entry "${name}" is missing contract CBOR; regenerate deployment info from the current live deployment.`,
    );
  }
  const script = {
    type: entry.contract.type,
    script: entry.contract.cborHex,
  } as Script;
  requireMatchingScriptHash({
    label: `${name} script`,
    deployed: entry.scriptHash,
    derived: validatorToScriptHash(script),
  });
  return script;
};

const buildRemovalContracts = async ({
  blueprint,
  deploymentInfo,
  network,
  fraudCategory,
}: {
  readonly blueprint: unknown;
  readonly deploymentInfo: ContractDeploymentInfo;
  readonly network: Network;
  readonly fraudCategory: RemoveFraudulentBlockFraudCategory;
}): Promise<RemoveFraudulentBlockContracts> => {
  const hubOraclePolicyId = requireDeploymentScriptHash(
    deploymentInfo,
    "hubOracleMint",
  );
  const fraudProofCataloguePolicyId = requireDeploymentScriptHash(
    deploymentInfo,
    "fraudProofCatalogueMint",
  );
  const parsedBlueprint = parseFaultProofBlueprint(blueprint);
  let categoryContracts:
    | DoubleSpendFaultProofContracts
    | NonExistentInputFaultProofContracts
    | InvalidRangeFaultProofContracts
    | TransitionTraceFaultProofContracts
    | ZeroInputFaultProofContracts
    | ValidationTraceDisputeFaultProofContracts;
  let expectedCategoryDeploymentEntry:
    | "fraudProofDoubleSpend"
    | "fraudProofNonExistentInput"
    | "fraudProofInvalidRange"
    | "fraudProofTransitionTrace"
    | "fraudProofZeroInput"
    | "validationTraceDispute";
  let derivedCategoryFirstStepHash: string;
  if (fraudCategory === "doubleSpend") {
    const doubleSpendContracts = await Effect.runPromise(
      buildDoubleSpendFaultProofContracts({
        blueprint: parsedBlueprint,
        network,
        hubOraclePolicyId,
        fraudProofCataloguePolicyId,
      }),
    );
    categoryContracts = doubleSpendContracts;
    expectedCategoryDeploymentEntry = "fraudProofDoubleSpend";
    derivedCategoryFirstStepHash =
      doubleSpendContracts.doubleSpend.firstStep.spendingScriptHash;
  } else if (fraudCategory === "nonExistentInput") {
    const nonExistentInputContracts = await Effect.runPromise(
      buildNonExistentInputFaultProofContracts({
        blueprint: parsedBlueprint,
        network,
        hubOraclePolicyId,
        fraudProofCataloguePolicyId,
      }),
    );
    categoryContracts = nonExistentInputContracts;
    expectedCategoryDeploymentEntry = "fraudProofNonExistentInput";
    derivedCategoryFirstStepHash =
      nonExistentInputContracts.nonExistentInput.firstStep.spendingScriptHash;
  } else if (fraudCategory === "invalidRange") {
    const invalidRangeContracts = await Effect.runPromise(
      buildInvalidRangeFaultProofContracts({
        blueprint: parsedBlueprint,
        network,
        hubOraclePolicyId,
        fraudProofCataloguePolicyId,
      }),
    );
    categoryContracts = invalidRangeContracts;
    expectedCategoryDeploymentEntry = "fraudProofInvalidRange";
    derivedCategoryFirstStepHash =
      invalidRangeContracts.invalidRange.firstStep.spendingScriptHash;
  } else if (fraudCategory === "transitionTrace") {
    const transitionTraceContracts = await Effect.runPromise(
      buildTransitionTraceFaultProofContracts({
        blueprint: parsedBlueprint,
        network,
        hubOraclePolicyId,
        fraudProofCataloguePolicyId,
      }),
    );
    categoryContracts = transitionTraceContracts;
    expectedCategoryDeploymentEntry = "fraudProofTransitionTrace";
    derivedCategoryFirstStepHash =
      transitionTraceContracts.transitionTrace.firstStep.spendingScriptHash;
  } else if (fraudCategory === "validationTraceDispute") {
    const validationTraceDisputeContracts = await Effect.runPromise(
      buildValidationTraceDisputeFaultProofContracts({
        blueprint: parsedBlueprint,
        network,
        hubOraclePolicyId,
        fraudProofCataloguePolicyId,
      }),
    );
    categoryContracts = validationTraceDisputeContracts;
    expectedCategoryDeploymentEntry = "validationTraceDispute";
    derivedCategoryFirstStepHash =
      validationTraceDisputeContracts.validationTraceDispute.firstStep
        .spendingScriptHash;
  } else {
    const zeroInputContracts = await Effect.runPromise(
      buildZeroInputFaultProofContracts({
        blueprint: parsedBlueprint,
        network,
        hubOraclePolicyId,
        fraudProofCataloguePolicyId,
      }),
    );
    categoryContracts = zeroInputContracts;
    expectedCategoryDeploymentEntry = "fraudProofZeroInput";
    derivedCategoryFirstStepHash =
      zeroInputContracts.zeroInput.firstStep.spendingScriptHash;
  }
  requireMatchingScriptHash({
    label: "fraudProofMint policy",
    deployed: requireDeploymentScriptHash(deploymentInfo, "fraudProofMint"),
    derived: categoryContracts.fraudProof.policyId,
  });
  requireMatchingScriptHash({
    label: "fraudProofSpend script",
    deployed: requireDeploymentScriptHash(deploymentInfo, "fraudProofSpend"),
    derived: categoryContracts.fraudProof.spendingScriptHash,
  });
  requireMatchingScriptHash({
    label: `${expectedCategoryDeploymentEntry} step-01 script`,
    deployed: requireDeploymentScriptHash(
      deploymentInfo,
      expectedCategoryDeploymentEntry,
    ),
    derived: derivedCategoryFirstStepHash,
  });
  const categoryId = (
    deploymentInfo.fraudProofCatalogueMint?.fraudProofCatalogue?.categories as
      | Readonly<
          Partial<
            Record<
              RemoveFraudulentBlockFraudCategory,
              { readonly categoryId: string }
            >
          >
        >
      | undefined
  )?.[fraudCategory]?.categoryId;
  if (categoryId === undefined) {
    throw new Error(
      `Deployment info is missing fraudProofCatalogueMint.fraudProofCatalogue.categories.${fraudCategory}.`,
    );
  }

  const stateQueueSpendingScript = requireDeploymentScript(
    deploymentInfo,
    "stateQueueSpend",
  );
  const stateQueueMintingScript = requireDeploymentScript(
    deploymentInfo,
    "stateQueueMint",
  );
  const activeOperatorsSpendingScript = requireDeploymentScript(
    deploymentInfo,
    "activeOperatorsSpend",
  );
  const activeOperatorsMintingScript = requireDeploymentScript(
    deploymentInfo,
    "activeOperatorsMint",
  );
  const retiredOperatorsSpendingScript = requireDeploymentScript(
    deploymentInfo,
    "retiredOperatorsSpend",
  );
  const retiredOperatorsMintingScript = requireDeploymentScript(
    deploymentInfo,
    "retiredOperatorsMint",
  );
  const schedulerSpendingScript = requireDeploymentScript(
    deploymentInfo,
    "schedulerSpend",
  );
  const activeOperatorsPolicyId = requireDeploymentScriptHash(
    deploymentInfo,
    "activeOperatorsMint",
  );
  const retiredOperatorsPolicyId = requireDeploymentScriptHash(
    deploymentInfo,
    "retiredOperatorsMint",
  );
  const schedulerPolicyId = requireDeploymentScriptHash(
    deploymentInfo,
    "schedulerMint",
  );
  const registeredOperatorsPolicyId = requireDeploymentScriptHash(
    deploymentInfo,
    "registeredOperatorsMint",
  );

  return {
    stateQueuePolicyId: requireDeploymentScriptHash(
      deploymentInfo,
      "stateQueueMint",
    ),
    stateQueueAddress: validatorToAddress(
      network,
      stateQueueSpendingScript as SpendingValidator,
    ),
    stateQueueSpendingScript,
    stateQueueMintingScript,
    activeOperatorsPolicyId,
    activeOperatorsAddress: validatorToAddress(
      network,
      activeOperatorsSpendingScript as SpendingValidator,
    ),
    activeOperatorsSpendingScript,
    activeOperatorsMintingScript,
    retiredOperatorsPolicyId,
    retiredOperatorsAddress: validatorToAddress(
      network,
      retiredOperatorsSpendingScript as SpendingValidator,
    ),
    retiredOperatorsSpendingScript,
    retiredOperatorsMintingScript,
    schedulerPolicyId,
    schedulerAddress: validatorToAddress(
      network,
      schedulerSpendingScript as SpendingValidator,
    ),
    schedulerSpendingScript,
    hubOraclePolicyId,
    registeredOperatorsPolicyId,
    registeredOperatorsAddress: validatorToAddress(
      network,
      requireDeploymentScript(
        deploymentInfo,
        "registeredOperatorsSpend",
      ) as SpendingValidator,
    ),
    fraudProofPolicyId: categoryContracts.fraudProof.policyId,
    fraudProofAddress: categoryContracts.fraudProof.spendingScriptAddress,
    fraudCategoryId: categoryId,
    fraudCategory,
  };
};

const requireDeploymentReferenceScript = async ({
  lucid,
  deploymentInfo,
  name,
}: {
  readonly lucid: LucidEvolution;
  readonly deploymentInfo: ContractDeploymentInfo;
  readonly name: ReferenceScriptName;
}): Promise<UTxO> => {
  const entry = deploymentInfo[name];
  if (entry === undefined) {
    throw new Error(`Deployment info is missing "${name}"`);
  }
  if (entry.refScriptUTxO == null) {
    throw new Error(
      `Deployment info entry "${name}" is missing refScriptUTxO; publish reference scripts and regenerate deployment info before live removal.`,
    );
  }
  const utxo = await fetchUtxoByOutRef({
    lucid,
    outRef: entry.refScriptUTxO,
    label: `${name} reference-script UTxO`,
  });
  if (utxo.scriptRef == null) {
    throw new Error(
      `${name} reference-script UTxO ${outRefLabel(utxo)} does not carry a reference script.`,
    );
  }
  const scriptRef = utxo.scriptRef;
  requireMatchingScriptHash({
    label: `${name} reference script`,
    deployed: entry.scriptHash,
    derived: validatorToScriptHash(scriptRef),
  });
  return utxo;
};

const resolveReferenceScripts = async ({
  lucid,
  deploymentInfo,
  requireReferenceScripts,
}: {
  readonly lucid: LucidEvolution;
  readonly deploymentInfo: ContractDeploymentInfo;
  readonly requireReferenceScripts: boolean;
}): Promise<StateQueueRemoveReferenceScriptUTxOs | undefined> => {
  if (!requireReferenceScripts) {
    return undefined;
  }
  const [
    stateQueueSpend,
    stateQueueMint,
    activeOperatorsSpend,
    activeOperatorsMint,
    retiredOperatorsSpend,
    retiredOperatorsMint,
    schedulerSpend,
  ] = await Promise.all(
    REFERENCE_SCRIPT_NAMES.map((name) =>
      requireDeploymentReferenceScript({ lucid, deploymentInfo, name }),
    ),
  );
  return {
    stateQueueSpend,
    stateQueueMint,
    activeOperatorsSpend,
    activeOperatorsMint,
    retiredOperatorsSpend,
    retiredOperatorsMint,
    schedulerSpend,
  };
};

const referenceScriptOutRefs = (
  referenceScripts: StateQueueRemoveReferenceScriptUTxOs | undefined,
): Readonly<Record<ReferenceScriptName, string | null>> =>
  Object.fromEntries(
    REFERENCE_SCRIPT_NAMES.map((name) => {
      const utxo = referenceScripts?.[name];
      return [name, utxo === undefined ? null : outRefLabel(utxo)] as const;
    }),
  ) as Readonly<Record<ReferenceScriptName, string | null>>;

type SchedulerDatumValue =
  | "NoActiveOperators"
  | {
      readonly ActiveOperator: {
        readonly operator: string;
        readonly start_time: bigint;
      };
    };

const decodeSchedulerDatum = (schedulerUtxo: UTxO): SchedulerDatumValue => {
  if (schedulerUtxo.datum == null) {
    throw new Error(
      `Scheduler UTxO ${outRefLabel(schedulerUtxo)} is missing datum.`,
    );
  }
  return Data.from(schedulerUtxo.datum, SchedulerDatum) as SchedulerDatumValue;
};

const nodeKeyValue = (nodeKey: LinkedListNodeView["key"]): string | null =>
  nodeKey === "Empty" ? null : nodeKey.Key.key;

const nextKeyValue = (nodeView: LinkedListNodeView): string | null =>
  nodeView.next === "Empty" ? null : nodeView.next.Key.key;

const compareHexByteStrings = (left: string, right: string): number =>
  Buffer.from(left, "hex").compare(Buffer.from(right, "hex"));

const requireStateQueueHeaderHash = async (
  stateQueueNode: StateQueueUTxO,
): Promise<string> => {
  if (stateQueueNode.datum.key === "Empty") {
    throw new Error(
      `State-queue UTxO ${outRefLabel(stateQueueNode.utxo)} is the confirmed-state root, not a block node.`,
    );
  }
  if (
    !stateQueueNode.assetName.startsWith(STATE_QUEUE_NODE_ASSET_NAME_PREFIX)
  ) {
    throw new Error(
      `State-queue block ${outRefLabel(stateQueueNode.utxo)} has unexpected asset name ${stateQueueNode.assetName}.`,
    );
  }
  const assetHeaderHash = stateQueueNode.assetName.slice(
    STATE_QUEUE_NODE_ASSET_NAME_PREFIX.length,
  );
  if (stateQueueNode.datum.key.Key.key !== assetHeaderHash) {
    throw new Error(
      `State-queue block ${outRefLabel(stateQueueNode.utxo)} has key ${stateQueueNode.datum.key.Key.key} but asset header hash ${assetHeaderHash}.`,
    );
  }
  const header = await Effect.runPromise(
    getHeaderV1FromStateQueueDatum(stateQueueNode.datum),
  );
  const computedHeaderHash = await Effect.runPromise(hashBlockHeaderV1(header));
  if (computedHeaderHash !== assetHeaderHash) {
    throw new Error(
      `State-queue block datum hash mismatch for ${outRefLabel(stateQueueNode.utxo)}: asset=${assetHeaderHash}, computed=${computedHeaderHash}.`,
    );
  }
  return assetHeaderHash;
};

const hasStateQueuePolicyAsset = (
  utxo: UTxO,
  stateQueuePolicyId: string,
): boolean =>
  Object.entries(utxo.assets).some(
    ([unit, quantity]) =>
      quantity !== 0n &&
      unit !== "lovelace" &&
      unit.startsWith(stateQueuePolicyId),
  );

const loadStateQueueTopology = async ({
  lucid,
  stateQueueAddress,
  stateQueuePolicyId,
}: {
  readonly lucid: LucidEvolution;
  readonly stateQueueAddress: string;
  readonly stateQueuePolicyId: string;
}): Promise<StateQueueTopology> => {
  const allUtxos = await lucid.utxosAt(stateQueueAddress);
  const stateQueueUtxos = await Promise.all(
    allUtxos
      .filter((utxo) => hasStateQueuePolicyAsset(utxo, stateQueuePolicyId))
      .map((utxo) =>
        Effect.runPromise(utxoToStateQueueUTxO(utxo, stateQueuePolicyId)),
      ),
  );
  const roots = stateQueueUtxos.filter((entry) => entry.datum.key === "Empty");
  if (roots.length !== 1) {
    throw new Error(
      `Expected exactly one state-queue root UTxO, found ${roots.length.toString()}.`,
    );
  }

  const root = roots[0]!;
  if (root.assetName !== STATE_QUEUE_ROOT_ASSET_NAME) {
    throw new Error(
      `State-queue root ${outRefLabel(root.utxo)} has unexpected asset name ${root.assetName}.`,
    );
  }
  const nodeByHeaderHash = new Map<string, StateQueueUTxO>();
  for (const entry of stateQueueUtxos) {
    if (entry.datum.key === "Empty") {
      continue;
    }
    const headerHash = await requireStateQueueHeaderHash(entry);
    if (nodeByHeaderHash.has(headerHash)) {
      throw new Error(`State queue contains duplicate block ${headerHash}.`);
    }
    nodeByHeaderHash.set(headerHash, entry);
  }

  const ordered: StateQueueUTxO[] = [root];
  const predecessorByHeaderHash = new Map<string, StateQueueUTxO>();
  const successorByHeaderHash = new Map<string, StateQueueUTxO>();
  const visited = new Set<string>();
  let predecessor = root;
  let currentKey = nextKeyValue(root.datum);
  while (currentKey !== null) {
    if (visited.has(currentKey)) {
      throw new Error(`State queue contains a cycle at ${currentKey}.`);
    }
    visited.add(currentKey);
    const current = nodeByHeaderHash.get(currentKey);
    if (current === undefined) {
      throw new Error(`State queue points to missing block ${currentKey}.`);
    }
    ordered.push(current);
    predecessorByHeaderHash.set(currentKey, predecessor);
    const successorKey = nextKeyValue(current.datum);
    if (successorKey !== null) {
      const successor = nodeByHeaderHash.get(successorKey);
      if (successor === undefined) {
        throw new Error(
          `State queue block ${currentKey} points to missing block ${successorKey}.`,
        );
      }
      successorByHeaderHash.set(currentKey, successor);
    }
    predecessor = current;
    currentKey = successorKey;
  }

  if (visited.size !== nodeByHeaderHash.size) {
    const unreachable = [...nodeByHeaderHash.keys()].filter(
      (headerHash) => !visited.has(headerHash),
    );
    throw new Error(
      `State queue contains unreachable block(s): ${unreachable.join(", ")}.`,
    );
  }

  return {
    root,
    ordered,
    nodeByHeaderHash,
    predecessorByHeaderHash,
    successorByHeaderHash,
  };
};

const activeOperatorUnit = (policyId: string, operator: string): string =>
  toUnit(policyId, ACTIVE_OPERATOR_NODE_ASSET_NAME_PREFIX + operator);

const isOperatorListUnit = ({
  unit,
  policyId,
  rootAssetName,
  nodeAssetNamePrefix,
}: {
  readonly unit: string;
  readonly policyId: string;
  readonly rootAssetName: string;
  readonly nodeAssetNamePrefix: string;
}): boolean =>
  unit.startsWith(policyId) &&
  (unit.slice(policyId.length) === rootAssetName ||
    unit.slice(policyId.length).startsWith(nodeAssetNamePrefix));

const hasOperatorListToken = ({
  utxo,
  policyId,
  rootAssetName,
  nodeAssetNamePrefix,
}: {
  readonly utxo: UTxO;
  readonly policyId: string;
  readonly rootAssetName: string;
  readonly nodeAssetNamePrefix: string;
}): boolean =>
  Object.entries(utxo.assets).some(
    ([unit, quantity]) =>
      quantity === 1n &&
      isOperatorListUnit({
        unit,
        policyId,
        rootAssetName,
        nodeAssetNamePrefix,
      }),
  );

const loadOperatorList = async ({
  lucid,
  address,
  policyId,
  rootAssetName,
  nodeAssetNamePrefix,
  label,
}: {
  readonly lucid: LucidEvolution;
  readonly address: string;
  readonly policyId: string;
  readonly rootAssetName: string;
  readonly nodeAssetNamePrefix: string;
  readonly label: string;
}): Promise<readonly OperatorListEntry[]> => {
  const utxos = await lucid.utxosAt(address);
  const entries = await Promise.all(
    utxos
      .filter((utxo) =>
        hasOperatorListToken({
          utxo,
          policyId,
          rootAssetName,
          nodeAssetNamePrefix,
        }),
      )
      .map(async (utxo) => ({
        utxo,
        view: await Effect.runPromise(getLinkedListNodeViewFromUTxO(utxo)),
      })),
  );
  const roots = entries.filter((entry) => entry.view.key === "Empty");
  if (roots.length !== 1) {
    throw new Error(
      `Expected exactly one ${label} root UTxO, found ${roots.length.toString()}.`,
    );
  }
  return entries;
};

const resolveOperatorRemovalPlan = ({
  entries,
  operator,
  label,
}: {
  readonly entries: readonly OperatorListEntry[];
  readonly operator: string;
  readonly label: string;
}): OperatorListRemovalPlan | undefined => {
  const root = entries.find((entry) => entry.view.key === "Empty");
  if (root === undefined) {
    throw new Error(`${label} list is missing its root.`);
  }
  const nodesByKey = new Map<string, OperatorListEntry>();
  for (const entry of entries) {
    const key = nodeKeyValue(entry.view.key);
    if (key === null) {
      continue;
    }
    if (nodesByKey.has(key)) {
      throw new Error(`${label} list contains duplicate key ${key}.`);
    }
    nodesByKey.set(key, entry);
  }

  let anchor: OperatorListEntry = root;
  let currentKey = nextKeyValue(root.view);
  let removed: OperatorListEntry | undefined;
  let lastNode: OperatorListEntry | undefined;
  const visited = new Set<string>();
  while (currentKey !== null) {
    if (visited.has(currentKey)) {
      throw new Error(`${label} list contains a cycle at key ${currentKey}.`);
    }
    visited.add(currentKey);
    const current = nodesByKey.get(currentKey);
    if (current === undefined) {
      throw new Error(`${label} list points to missing node ${currentKey}.`);
    }
    if (currentKey === operator) {
      removed = current;
    }
    const nextKey = nextKeyValue(current.view);
    if (nextKey === null) {
      lastNode = current;
    }
    if (removed === undefined) {
      anchor = current;
    }
    currentKey = nextKey;
  }

  if (visited.size !== nodesByKey.size) {
    const unreachable = [...nodesByKey.keys()].filter(
      (key) => !visited.has(key),
    );
    throw new Error(
      `${label} list contains unreachable node(s): ${unreachable.join(", ")}.`,
    );
  }

  if (removed === undefined) {
    return undefined;
  }
  const lastNodeAfterRemoval =
    lastNode === undefined || nodeKeyValue(lastNode.view.key) === operator
      ? undefined
      : lastNode;
  return { anchor, node: removed, lastNodeAfterRemoval };
};

const resolveNonMembershipWitness = ({
  entries,
  operator,
  label,
}: {
  readonly entries: readonly OperatorListEntry[];
  readonly operator: string;
  readonly label: string;
}): OperatorListEntry => {
  const root = entries.find((entry) => entry.view.key === "Empty");
  if (root === undefined) {
    throw new Error(`${label} list is missing its root.`);
  }
  let witness = root;
  let currentKey = nextKeyValue(root.view);
  const nodesByKey = new Map<string, OperatorListEntry>();
  for (const entry of entries) {
    const key = nodeKeyValue(entry.view.key);
    if (key !== null) {
      nodesByKey.set(key, entry);
    }
  }
  const visited = new Set<string>();
  while (currentKey !== null) {
    if (visited.has(currentKey)) {
      throw new Error(`${label} list contains a cycle at key ${currentKey}.`);
    }
    visited.add(currentKey);
    const current = nodesByKey.get(currentKey);
    if (current === undefined) {
      throw new Error(`${label} list points to missing node ${currentKey}.`);
    }
    const comparison = compareHexByteStrings(operator, currentKey);
    if (comparison === 0) {
      throw new Error(`${label} list still contains operator ${operator}.`);
    }
    if (comparison < 0) {
      return witness;
    }
    witness = current;
    currentKey = nextKeyValue(current.view);
  }
  return witness;
};

const resolveSchedulerRemovalPlan = ({
  schedulerUtxo,
  operator,
  removalPlan,
}: {
  readonly schedulerUtxo: UTxO;
  readonly operator: string;
  readonly removalPlan: OperatorListRemovalPlan;
}): SchedulerRemovalPlan => {
  const schedulerDatum = decodeSchedulerDatum(schedulerUtxo);
  if (
    schedulerDatum === "NoActiveOperators" ||
    schedulerDatum.ActiveOperator.operator !== operator
  ) {
    return { kind: "inactive" };
  }
  const anchorKey = nodeKeyValue(removalPlan.anchor.view.key);
  const removedNodeIsLast = nextKeyValue(removalPlan.node.view) === null;
  if (anchorKey !== null) {
    return { kind: "goToAnchor", newOperator: anchorKey, removedNodeIsLast };
  }
  return {
    kind: "rewind",
    newOperator:
      removalPlan.lastNodeAfterRemoval === undefined
        ? undefined
        : (nodeKeyValue(removalPlan.lastNodeAfterRemoval.view.key) ??
          undefined),
    removedNodeIsLast,
  };
};

type SlashingTxPlan = {
  readonly approach: RemoveTransactionResult["slashingApproach"];
  readonly removedOperatorNodeOutRef: string | null;
  readonly registeredOperatorsRootOutRef: string | null;
  readonly buildSlashing: (txValidTo: bigint) => RemoveFraudulentBlockSlashing;
  readonly additionalRefInputs: readonly UTxO[];
};

const requireLayoutIndex = (
  value: bigint | undefined,
  label: string,
): bigint => {
  if (value === undefined) {
    throw new Error(`Missing ${label} in remove-fraudulent-block layout.`);
  }
  return value;
};

const requireLayoutUtxo = (utxo: UTxO | undefined, label: string): UTxO => {
  if (utxo === undefined) {
    throw new Error(
      `Missing ${label} in remove-fraudulent-block layout context.`,
    );
  }
  return utxo;
};

const makeLayoutRedeemer = <T>({
  layoutContext,
  encode,
  schema,
  bindOwnPurpose,
}: {
  readonly layoutContext: OperatorSlashingLayoutContext;
  readonly encode: (layout: OperatorSlashingLayout) => T;
  readonly schema: T;
  readonly bindOwnPurpose: (ctx: Parameters<BuildTxWithRedeemer>[0]) => void;
}): BuildTxWithRedeemer =>
  ((ctx) => {
    bindOwnPurpose(ctx);
    const layout = deriveOperatorSlashingLayoutFromRedeemerContext({
      ctx,
      ...layoutContext,
    });
    return Data.to(encode(layout) as never, schema as never);
  }) satisfies BuildTxWithRedeemer;

const makeActiveOperatorsMintRedeemerFromPlan =
  ({
    operator,
    schedulerPlan,
    activeOperatorAnchorKey,
  }: {
    readonly operator: string;
    readonly schedulerPlan: SchedulerRemovalPlan;
    readonly activeOperatorAnchorKey: string | null;
  }) =>
  (layout: OperatorSlashingLayout): ActiveOperatorMintRedeemerData => {
    const operatorRemovalSchedulerSync =
      schedulerPlan.kind === "inactive"
        ? {
            ShowOperatorIsInactive: {
              scheduler_ref_input_index: requireLayoutIndex(
                layout.schedulerRefInputIndex,
                "schedulerRefInputIndex",
              ),
            },
          }
        : {
            ShowSchedulerIsAdvancing: {
              scheduler_input_index: requireLayoutIndex(
                layout.schedulerInputIndex,
                "schedulerInputIndex",
              ),
              scheduler_redeemer_index: requireLayoutIndex(
                layout.schedulerRedeemerTxInfoIndex,
                "schedulerRedeemerTxInfoIndex",
              ),
              removing_operators_anchor_element_key: activeOperatorAnchorKey,
              removing_operator_is_the_last_member:
                schedulerPlan.removedNodeIsLast,
            },
          };

    return {
      SlashOperator: {
        slashing_arguments: {
          slashed_operator: operator,
          hub_oracle_ref_input_index: layout.hubOracleRefInputIndex,
          slashed_operator_anchor_element_input_outref:
            layout.operatorDirectoryAnchorInputOutRef,
          slashed_operator_anchor_element_output_index:
            layout.operatorDirectoryAnchorOutputIndex,
          slashing_reason: {
            SlashOperatorForBadState: {
              state_queue_redeemer_index: layout.stateQueueRedeemerTxInfoIndex,
            },
          },
        },
        operator_removal_scheduler_sync: operatorRemovalSchedulerSync,
      },
    };
  };

const makeRetiredOperatorsMintRedeemerFromPlan =
  ({ operator }: { readonly operator: string }) =>
  (layout: OperatorSlashingLayout): RetiredOperatorMintRedeemerData => ({
    SlashOperator: {
      slashing_arguments: {
        slashed_operator: operator,
        hub_oracle_ref_input_index: layout.hubOracleRefInputIndex,
        slashed_operator_anchor_element_input_outref:
          layout.operatorDirectoryAnchorInputOutRef,
        slashed_operator_anchor_element_output_index:
          layout.operatorDirectoryAnchorOutputIndex,
        slashing_reason: {
          SlashOperatorForBadState: {
            state_queue_redeemer_index: layout.stateQueueRedeemerTxInfoIndex,
          },
        },
      },
    },
  });

const makeSchedulerSpendRedeemerFromPlan =
  ({
    schedulerPlan,
  }: {
    readonly schedulerPlan: Exclude<SchedulerRemovalPlan, { kind: "inactive" }>;
  }) =>
  (layout: OperatorSlashingLayout): SchedulerSpendRedeemerData => {
    const scheduler_input_index = requireLayoutIndex(
      layout.schedulerInputIndex,
      "schedulerInputIndex",
    );
    const scheduler_output_index = requireLayoutIndex(
      layout.schedulerOutputIndex,
      "schedulerOutputIndex",
    );
    return {
      scheduler_input_index,
      scheduler_output_index,
      advancing_approach:
        schedulerPlan.kind === "goToAnchor"
          ? {
              GoToNextDueToOperatorRemoval: {
                active_operators_mint_redeemer_index: requireLayoutIndex(
                  layout.activeOperatorsRedeemerTxInfoIndex,
                  "activeOperatorsRedeemerTxInfoIndex",
                ),
                removal_reason: "OperatorSlashing",
              },
            }
          : {
              RewindDueToOperatorRemoval: {
                active_operators_mint_redeemer_index: requireLayoutIndex(
                  layout.activeOperatorsRedeemerTxInfoIndex,
                  "activeOperatorsRedeemerTxInfoIndex",
                ),
                m_active_operators_last_node_ref_input_index:
                  layout.activeOperatorsLastNodeRefInputIndex ?? null,
                removal_reason: "OperatorSlashing",
                registered_element_ref_input_index: requireLayoutIndex(
                  layout.registeredOperatorsRootRefInputIndex,
                  "registeredOperatorsRootRefInputIndex",
                ),
              },
            },
    };
  };

type OperatorSlashingPlan =
  | {
      readonly approach: "SlashActiveOperator";
      readonly removalPlan: OperatorListRemovalPlan;
      readonly schedulerUtxo: UTxO;
      readonly schedulerPlan: SchedulerRemovalPlan;
      readonly anchorKey: string | null;
      readonly anchorUnit: string;
      readonly activeOperatorsLastNode?: UTxO;
    }
  | {
      readonly approach: "SlashRetiredOperator";
      readonly removalPlan: OperatorListRemovalPlan;
      readonly anchorUnit: string;
    }
  | {
      readonly approach: "OperatorAlreadySlashed";
      readonly activeWitness: OperatorListEntry;
      readonly retiredWitness: OperatorListEntry;
    };

const resolveOperatorSlashingPlan = async ({
  lucid,
  contracts,
  operator,
  schedulerUtxo,
  activeOperatorsRootUnit,
  retiredOperatorsRootUnit,
}: {
  readonly lucid: LucidEvolution;
  readonly contracts: RemoveFraudulentBlockContracts;
  readonly operator: string;
  readonly schedulerUtxo: UTxO;
  readonly activeOperatorsRootUnit: string;
  readonly retiredOperatorsRootUnit: string;
}): Promise<OperatorSlashingPlan> => {
  const activeEntries = await loadOperatorList({
    lucid,
    address: contracts.activeOperatorsAddress,
    policyId: contracts.activeOperatorsPolicyId,
    rootAssetName: ACTIVE_OPERATORS_ROOT_ASSET_NAME,
    nodeAssetNamePrefix: ACTIVE_OPERATOR_NODE_ASSET_NAME_PREFIX,
    label: "active-operators",
  });
  const activePlan = resolveOperatorRemovalPlan({
    entries: activeEntries,
    operator,
    label: "active-operators",
  });
  if (activePlan !== undefined) {
    const schedulerPlan = resolveSchedulerRemovalPlan({
      schedulerUtxo,
      operator,
      removalPlan: activePlan,
    });
    const anchorKey = nodeKeyValue(activePlan.anchor.view.key);
    const anchorUnit =
      anchorKey === null
        ? activeOperatorsRootUnit
        : activeOperatorUnit(contracts.activeOperatorsPolicyId, anchorKey);

    return {
      approach: "SlashActiveOperator",
      removalPlan: activePlan,
      schedulerUtxo,
      schedulerPlan,
      anchorKey,
      anchorUnit,
      activeOperatorsLastNode:
        schedulerPlan.kind === "rewind" &&
        schedulerPlan.newOperator !== undefined
          ? activePlan.lastNodeAfterRemoval?.utxo
          : undefined,
    };
  }

  const retiredEntries = await loadOperatorList({
    lucid,
    address: contracts.retiredOperatorsAddress,
    policyId: contracts.retiredOperatorsPolicyId,
    rootAssetName: RETIRED_OPERATORS_ROOT_ASSET_NAME,
    nodeAssetNamePrefix: RETIRED_OPERATOR_NODE_ASSET_NAME_PREFIX,
    label: "retired-operators",
  });
  const retiredPlan = resolveOperatorRemovalPlan({
    entries: retiredEntries,
    operator,
    label: "retired-operators",
  });
  if (retiredPlan !== undefined) {
    const anchorKey = nodeKeyValue(retiredPlan.anchor.view.key);
    const anchorUnit =
      anchorKey === null
        ? retiredOperatorsRootUnit
        : toUnit(
            contracts.retiredOperatorsPolicyId,
            RETIRED_OPERATOR_NODE_ASSET_NAME_PREFIX + anchorKey,
          );
    return {
      approach: "SlashRetiredOperator",
      removalPlan: retiredPlan,
      anchorUnit,
    };
  }

  return {
    approach: "OperatorAlreadySlashed",
    activeWitness: resolveNonMembershipWitness({
      entries: activeEntries,
      operator,
      label: "active-operators",
    }),
    retiredWitness: resolveNonMembershipWitness({
      entries: retiredEntries,
      operator,
      label: "retired-operators",
    }),
  };
};

const buildActiveSlashingInputs = ({
  plan,
  operator,
  contracts,
  hubOracleUtxo,
  registeredOperatorsRootUtxo,
}: {
  readonly plan: Extract<
    OperatorSlashingPlan,
    { readonly approach: "SlashActiveOperator" }
  >;
  readonly operator: string;
  readonly contracts: RemoveFraudulentBlockContracts;
  readonly hubOracleUtxo: UTxO;
  readonly registeredOperatorsRootUtxo?: UTxO;
}): SlashingTxPlan => {
  const schedulerUnit = toUnit(
    contracts.schedulerPolicyId,
    SCHEDULER_ASSET_NAME,
  );
  const registeredOperatorsRoot =
    plan.schedulerPlan.kind === "rewind"
      ? requireLayoutUtxo(
          registeredOperatorsRootUtxo,
          "registered-operators root",
        )
      : undefined;
  return {
    approach: "SlashActiveOperator",
    removedOperatorNodeOutRef: outRefLabel(plan.removalPlan.node.utxo),
    registeredOperatorsRootOutRef:
      registeredOperatorsRoot === undefined
        ? null
        : outRefLabel(registeredOperatorsRoot),
    buildSlashing: (txValidTo) => {
      const activeLayoutContext: OperatorSlashingLayoutContext = {
        operatorDirectoryAnchor: plan.removalPlan.anchor.utxo,
        operatorDirectoryNode: plan.removalPlan.node.utxo,
        scheduler: plan.schedulerUtxo,
        schedulerPlan: plan.schedulerPlan,
        hubOracle: hubOracleUtxo,
        activeOperatorsLastNode: plan.activeOperatorsLastNode,
        operatorDirectoryAnchorUnit: plan.anchorUnit,
        slashedOperatorDirectory: "active",
        schedulerUnit,
        ...(registeredOperatorsRoot === undefined
          ? {}
          : { registeredOperatorsRoot }),
        contracts,
      };
      const schedulerSpend =
        plan.schedulerPlan.kind === "inactive"
          ? undefined
          : {
              input: plan.schedulerUtxo,
              redeemer: makeLayoutRedeemer({
                layoutContext: activeLayoutContext,
                encode: makeSchedulerSpendRedeemerFromPlan({
                  schedulerPlan: plan.schedulerPlan,
                }),
                schema: SchedulerSpendRedeemer,
                bindOwnPurpose: (ctx) =>
                  requireOwnSpendPurpose(
                    ctx,
                    plan.schedulerUtxo,
                    "remove-fraudulent-block scheduler",
                  ),
              }),
              script: contracts.schedulerSpendingScript,
              continuedOutput: {
                address: contracts.schedulerAddress,
                datum:
                  plan.schedulerPlan.newOperator === undefined
                    ? Data.to("NoActiveOperators", SchedulerDatum)
                    : Data.to(
                        {
                          ActiveOperator: {
                            operator: plan.schedulerPlan.newOperator,
                            start_time: txValidTo,
                          },
                        },
                        SchedulerDatum,
                      ),
                assets: plan.schedulerUtxo.assets,
              },
            };
      return {
        kind: "slashActiveOperator",
        activeOperatorsAssetsToBurn: {
          [activeOperatorUnit(contracts.activeOperatorsPolicyId, operator)]:
            -1n,
        },
        activeOperatorsMintRedeemer: makeLayoutRedeemer({
          layoutContext: activeLayoutContext,
          encode: makeActiveOperatorsMintRedeemerFromPlan({
            operator,
            schedulerPlan: plan.schedulerPlan,
            activeOperatorAnchorKey: plan.anchorKey,
          }),
          schema: ActiveOperatorMintRedeemer,
          bindOwnPurpose: (ctx) =>
            requireOwnMintPurpose(
              ctx,
              contracts.activeOperatorsPolicyId,
              "remove-fraudulent-block active-operators slash",
            ),
        }),
        activeOperatorsMintingScript: contracts.activeOperatorsMintingScript,
        activeOperatorInputs: [
          plan.removalPlan.anchor.utxo,
          plan.removalPlan.node.utxo,
        ],
        activeOperatorSpendingScript: contracts.activeOperatorsSpendingScript,
        activeOperatorSpendRedeemer: "ListStateTransition",
        continuedActiveOperatorAnchorOutput: {
          address: contracts.activeOperatorsAddress,
          datum: encodeLinkedListNodeView({
            ...plan.removalPlan.anchor.view,
            next: plan.removalPlan.node.view.next,
          }),
          assets: plan.removalPlan.anchor.utxo.assets,
        },
        ...(schedulerSpend === undefined ? {} : { schedulerSpend }),
      };
    },
    additionalRefInputs: [
      hubOracleUtxo,
      ...(plan.schedulerPlan.kind === "inactive" ? [plan.schedulerUtxo] : []),
      ...(registeredOperatorsRoot === undefined
        ? []
        : [registeredOperatorsRoot]),
      ...(plan.activeOperatorsLastNode === undefined
        ? []
        : [plan.activeOperatorsLastNode]),
    ],
  };
};

const buildRetiredSlashingInputs = ({
  plan,
  operator,
  contracts,
  hubOracleUtxo,
}: {
  readonly plan: Extract<
    OperatorSlashingPlan,
    { readonly approach: "SlashRetiredOperator" }
  >;
  readonly operator: string;
  readonly contracts: RemoveFraudulentBlockContracts;
  readonly hubOracleUtxo: UTxO;
}): SlashingTxPlan => ({
  approach: "SlashRetiredOperator",
  removedOperatorNodeOutRef: outRefLabel(plan.removalPlan.node.utxo),
  registeredOperatorsRootOutRef: null,
  buildSlashing: () => {
    const retiredLayoutContext: OperatorSlashingLayoutContext = {
      operatorDirectoryAnchor: plan.removalPlan.anchor.utxo,
      operatorDirectoryNode: plan.removalPlan.node.utxo,
      operatorDirectoryAnchorUnit: plan.anchorUnit,
      slashedOperatorDirectory: "retired",
      hubOracle: hubOracleUtxo,
      contracts,
    };
    return {
      kind: "slashRetiredOperator",
      retiredOperatorsAssetsToBurn: {
        [toUnit(
          contracts.retiredOperatorsPolicyId,
          RETIRED_OPERATOR_NODE_ASSET_NAME_PREFIX + operator,
        )]: -1n,
      },
      retiredOperatorsMintRedeemer: makeLayoutRedeemer({
        layoutContext: retiredLayoutContext,
        encode: makeRetiredOperatorsMintRedeemerFromPlan({ operator }),
        schema: RetiredOperatorMintRedeemer,
        bindOwnPurpose: (ctx) =>
          requireOwnMintPurpose(
            ctx,
            contracts.retiredOperatorsPolicyId,
            "remove-fraudulent-block retired-operators slash",
          ),
      }),
      retiredOperatorsMintingScript: contracts.retiredOperatorsMintingScript,
      retiredOperatorInputs: [
        plan.removalPlan.anchor.utxo,
        plan.removalPlan.node.utxo,
      ],
      retiredOperatorSpendingScript: contracts.retiredOperatorsSpendingScript,
      continuedRetiredOperatorAnchorOutput: {
        address: contracts.retiredOperatorsAddress,
        datum: encodeLinkedListNodeView({
          ...plan.removalPlan.anchor.view,
          next: plan.removalPlan.node.view.next,
        }),
        assets: plan.removalPlan.anchor.utxo.assets,
      },
    };
  },
  additionalRefInputs: [hubOracleUtxo],
});

const buildAlreadySlashedInputs = ({
  plan,
}: {
  readonly plan: Extract<
    OperatorSlashingPlan,
    { readonly approach: "OperatorAlreadySlashed" }
  >;
}): SlashingTxPlan => ({
  approach: "OperatorAlreadySlashed",
  removedOperatorNodeOutRef: null,
  registeredOperatorsRootOutRef: null,
  buildSlashing: () => ({
    kind: "operatorAlreadySlashed",
    activeOperatorsElementRefInput: plan.activeWitness.utxo,
    retiredOperatorsElementRefInput: plan.retiredWitness.utxo,
  }),
  additionalRefInputs: [],
});

const buildSlashingInputs = ({
  plan,
  operator,
  contracts,
  hubOracleUtxo,
  registeredOperatorsRootUtxo,
}: {
  readonly plan: OperatorSlashingPlan;
  readonly operator: string;
  readonly contracts: RemoveFraudulentBlockContracts;
  readonly hubOracleUtxo: UTxO;
  readonly registeredOperatorsRootUtxo?: UTxO;
}): SlashingTxPlan => {
  switch (plan.approach) {
    case "SlashActiveOperator":
      return buildActiveSlashingInputs({
        plan,
        operator,
        contracts,
        hubOracleUtxo,
        registeredOperatorsRootUtxo,
      });
    case "SlashRetiredOperator":
      return buildRetiredSlashingInputs({
        plan,
        operator,
        contracts,
        hubOracleUtxo,
      });
    case "OperatorAlreadySlashed":
      return buildAlreadySlashedInputs({ plan });
  }
};

const deriveOperatorSlashingLayoutFromRedeemerContext = ({
  ctx,
  ...layoutContext
}: OperatorSlashingLayoutContext & {
  readonly ctx: Parameters<BuildTxWithRedeemer>[0];
}): OperatorSlashingLayout => {
  const {
    operatorDirectoryAnchor,
    hubOracle,
    operatorDirectoryAnchorUnit,
    slashedOperatorDirectory,
    contracts,
  } = layoutContext;
  const slashingPolicyId =
    slashedOperatorDirectory === "active"
      ? contracts.activeOperatorsPolicyId
      : contracts.retiredOperatorsPolicyId;
  const slashingDirectoryLabel =
    slashedOperatorDirectory === "active"
      ? "active-operators"
      : "retired-operators";
  const slashingRedeemerTxInfoIndex = requireMintRedeemerIndex(
    ctx,
    slashingPolicyId,
    `${slashingDirectoryLabel} slash`,
  );
  const baseLayout: OperatorSlashingLayout = {
    ...(slashedOperatorDirectory === "active"
      ? { activeOperatorsRedeemerTxInfoIndex: slashingRedeemerTxInfoIndex }
      : { retiredOperatorsRedeemerTxInfoIndex: slashingRedeemerTxInfoIndex }),
    stateQueueRedeemerTxInfoIndex: requireMintRedeemerIndex(
      ctx,
      contracts.stateQueuePolicyId,
      "state-queue remove fraudulent block",
    ),
    operatorDirectoryAnchorInputOutRef: outputReferenceFromUTxO(
      operatorDirectoryAnchor,
    ),
    operatorDirectoryAnchorOutputIndex: requireOutputIndexByUnit({
      outputs: ctx.outputs,
      address:
        slashedOperatorDirectory === "active"
          ? contracts.activeOperatorsAddress
          : contracts.retiredOperatorsAddress,
      unit: operatorDirectoryAnchorUnit,
      label: `${slashingDirectoryLabel} anchor continuation`,
    }),
    hubOracleRefInputIndex: requireReferenceInputIndex(
      ctx,
      hubOracle,
      "hub-oracle reference input",
    ),
  };

  if (slashedOperatorDirectory === "retired") {
    return baseLayout;
  }

  const {
    scheduler,
    schedulerPlan,
    registeredOperatorsRoot,
    activeOperatorsLastNode,
    schedulerUnit,
  } = layoutContext;
  return {
    ...baseLayout,
    ...(schedulerPlan.kind === "inactive"
      ? {
          schedulerRefInputIndex: requireReferenceInputIndex(
            ctx,
            scheduler,
            "scheduler reference input",
          ),
        }
      : {
          schedulerInputIndex: requireInputIndex(
            ctx,
            scheduler,
            "scheduler input",
          ),
          schedulerOutputIndex: requireOutputIndexByUnit({
            outputs: ctx.outputs,
            address: contracts.schedulerAddress,
            unit: schedulerUnit,
            label: "scheduler continuation",
          }),
          schedulerRedeemerTxInfoIndex: requireSpendRedeemerIndex(
            ctx,
            scheduler,
            "scheduler spend redeemer",
          ),
        }),
    ...(activeOperatorsLastNode === undefined
      ? {}
      : {
          activeOperatorsLastNodeRefInputIndex: requireReferenceInputIndex(
            ctx,
            activeOperatorsLastNode,
            "active-operators last-node reference input",
          ),
        }),
    ...(schedulerPlan.kind === "rewind"
      ? {
          registeredOperatorsRootRefInputIndex: requireReferenceInputIndex(
            ctx,
            requireLayoutUtxo(
              registeredOperatorsRoot,
              "registered-operators root",
            ),
            "registered-operators root reference input",
          ),
        }
      : {}),
  };
};

const resolveStateQueueSlashingApproach = ({
  ctx,
  slashing,
  contracts,
}: {
  readonly ctx: Parameters<BuildTxWithRedeemer>[0];
  readonly slashing: RemoveFraudulentBlockSlashing;
  readonly contracts: RemoveFraudulentBlockContracts;
}): {
  readonly slashingApproach: SlashingApproachData;
  readonly layout: Partial<RemoveFraudulentBlockLayout>;
} => {
  switch (slashing.kind) {
    case "slashActiveOperator": {
      const activeOperatorsRedeemerTxInfoIndex = requireMintRedeemerIndex(
        ctx,
        contracts.activeOperatorsPolicyId,
        "remove-fraudulent-block active-operators slash",
      );
      return {
        slashingApproach: {
          SlashActiveOperator: {
            active_operators_redeemer_index: activeOperatorsRedeemerTxInfoIndex,
          },
        },
        layout: { activeOperatorsRedeemerTxInfoIndex },
      };
    }
    case "slashRetiredOperator": {
      const retiredOperatorsRedeemerTxInfoIndex = requireMintRedeemerIndex(
        ctx,
        contracts.retiredOperatorsPolicyId,
        "remove-fraudulent-block retired-operators slash",
      );
      return {
        slashingApproach: {
          SlashRetiredOperator: {
            retired_operators_redeemer_index:
              retiredOperatorsRedeemerTxInfoIndex,
          },
        },
        layout: { retiredOperatorsRedeemerTxInfoIndex },
      };
    }
    case "operatorAlreadySlashed": {
      const activeOperatorsElementRefInputIndex = requireReferenceInputIndex(
        ctx,
        slashing.activeOperatorsElementRefInput,
        "remove-fraudulent-block active-operators non-membership witness",
      );
      const retiredOperatorsElementRefInputIndex = requireReferenceInputIndex(
        ctx,
        slashing.retiredOperatorsElementRefInput,
        "remove-fraudulent-block retired-operators non-membership witness",
      );
      return {
        slashingApproach: {
          OperatorAlreadySlashed: {
            active_operators_element_ref_input_index:
              activeOperatorsElementRefInputIndex,
            retired_operators_element_ref_input_index:
              retiredOperatorsElementRefInputIndex,
          },
        },
        layout: {
          activeOperatorsElementRefInputIndex,
          retiredOperatorsElementRefInputIndex,
        },
      };
    }
  }
};

const makeStateQueueRemoveMintRedeemer = ({
  kind,
  anchor,
  removed,
  fraudulentOperator,
  fraudulentBlocksHeaderHash,
  fraudProofRefInput,
  slashing,
  contracts,
  onLayout,
}: {
  readonly kind: RemoveTransactionKind;
  readonly anchor: StateQueueUTxO;
  readonly removed: StateQueueUTxO;
  readonly fraudulentOperator: string;
  readonly fraudulentBlocksHeaderHash: string;
  readonly fraudProofRefInput: UTxO;
  readonly slashing: RemoveFraudulentBlockSlashing;
  readonly contracts: RemoveFraudulentBlockContracts;
  readonly onLayout: (layout: RemoveFraudulentBlockLayout) => void;
}): BuildTxWithRedeemer =>
  ((ctx) => {
    requireOwnMintPurpose(
      ctx,
      contracts.stateQueuePolicyId,
      "remove-fraudulent-block state-queue burn",
    );
    const stateQueueRedeemerTxInfoIndex = requireMintRedeemerIndex(
      ctx,
      contracts.stateQueuePolicyId,
      "remove-fraudulent-block state-queue burn",
    );
    const { slashingApproach, layout: slashingLayout } =
      resolveStateQueueSlashingApproach({ ctx, slashing, contracts });
    const fraudulentNodeInput = kind === "remove-successor" ? anchor : removed;
    const fraudProofRefInputIndex = requireReferenceInputIndex(
      ctx,
      fraudProofRefInput,
      "remove-fraudulent-block fraud-proof token",
    );
    const continuedAnchorUnit = toUnit(
      contracts.stateQueuePolicyId,
      anchor.assetName,
    );
    const commonLayout = {
      fraudProofRefInputIndex,
      stateQueueRedeemerTxInfoIndex,
      ...slashingLayout,
    };
    const commonRedeemer = {
      fraudulent_operator: fraudulentOperator,
      fraudulent_blocks_header_hash: fraudulentBlocksHeaderHash,
      slashing_approach: slashingApproach,
      fraud_proof_ref_input_index: fraudProofRefInputIndex,
    };

    if (kind === "remove-successor") {
      const fraudulentNodeOutputIndex = requireOutputIndexByUnit({
        outputs: ctx.outputs,
        address: contracts.stateQueueAddress,
        unit: continuedAnchorUnit,
        label: "remove-fraudulent-block continued fraud-proved node",
      });
      onLayout({
        ...commonLayout,
        fraudulentNodeOutputIndex,
      });
      return Data.to(
        {
          RemoveFraudulentBlockHeader: {
            ...commonRedeemer,
            block_removal_approach: {
              RemoveFraudulentBlocksLink: {
                fraudulent_node_input_outref: outputReferenceFromUTxO(
                  fraudulentNodeInput.utxo,
                ),
                fraudulent_node_output_index: fraudulentNodeOutputIndex,
              },
            },
          },
        } satisfies StateQueueRedeemerData,
        StateQueueRedeemer,
      );
    }

    const anchorElementOutputIndex = requireOutputIndexByUnit({
      outputs: ctx.outputs,
      address: contracts.stateQueueAddress,
      unit: continuedAnchorUnit,
      label: "remove-fraudulent-block continued predecessor anchor",
    });
    onLayout({
      ...commonLayout,
      anchorElementOutputIndex,
    });
    return Data.to(
      {
        RemoveFraudulentBlockHeader: {
          ...commonRedeemer,
          block_removal_approach: {
            RemoveLastFraudulentBlock: {
              anchor_element_input_outref: outputReferenceFromUTxO(anchor.utxo),
              anchor_element_output_index: anchorElementOutputIndex,
            },
          },
        },
      } satisfies StateQueueRedeemerData,
      StateQueueRedeemer,
    );
  }) satisfies BuildTxWithRedeemer;

export const submitRemoveFraudulentBlock = async ({
  lucid,
  blueprint,
  deploymentInfo,
  network,
  signer,
  fraudCategory = "doubleSpend",
  fraudulentHeaderHash,
  awaitConfirmation = true,
  requireReferenceScripts = true,
  validFrom,
  validTo,
  stateQueueMutationLeaseCoordinator,
}: {
  readonly lucid: LucidEvolution;
  readonly blueprint: unknown;
  readonly deploymentInfo: unknown;
  readonly network: Network;
  readonly signer: ResolvedProverSigner;
  readonly fraudCategory?: RemoveFraudulentBlockFraudCategory;
  readonly fraudulentHeaderHash: string;
  readonly awaitConfirmation?: boolean;
  readonly requireReferenceScripts?: boolean;
  readonly validFrom?: bigint;
  readonly validTo?: bigint;
  readonly stateQueueMutationLeaseCoordinator?: StateQueueMutationLeaseCoordinator;
}): Promise<SubmitRemoveFraudulentBlockResult> => {
  const headerHash = parseHex(
    fraudulentHeaderHash,
    "--fraudulent-header-hash",
    28,
  );
  const parsedDeploymentInfo = parseContractDeploymentInfo(deploymentInfo);
  const contracts = await buildRemovalContracts({
    blueprint,
    deploymentInfo: parsedDeploymentInfo,
    network,
    fraudCategory,
  });
  if (
    contracts.fraudCategoryId.length !==
    FRAUD_PROOF_CATALOGUE_ID_BYTE_COUNT * 2
  ) {
    throw new Error(
      `${contracts.fraudCategory} fraud-proof category id has invalid length.`,
    );
  }

  const referenceScripts = await resolveReferenceScripts({
    lucid,
    deploymentInfo: parsedDeploymentInfo,
    requireReferenceScripts,
  });
  signer.selectWallet(lucid);

  const fraudProofAssetName = contracts.fraudCategoryId + headerHash;
  const fraudProofUnit = toUnit(
    contracts.fraudProofPolicyId,
    fraudProofAssetName,
  );
  const activeOperatorsRootUnit = toUnit(
    contracts.activeOperatorsPolicyId,
    ACTIVE_OPERATORS_ROOT_ASSET_NAME,
  );
  const retiredOperatorsRootUnit = toUnit(
    contracts.retiredOperatorsPolicyId,
    RETIRED_OPERATORS_ROOT_ASSET_NAME,
  );
  const schedulerUnit = toUnit(
    contracts.schedulerPolicyId,
    SCHEDULER_ASSET_NAME,
  );
  const hubOracleUnit = toUnit(
    contracts.hubOraclePolicyId,
    HUB_ORACLE_ASSET_NAME,
  );
  const stateQueueConfig = {
    stateQueueAddress: contracts.stateQueueAddress,
    stateQueuePolicyId: contracts.stateQueuePolicyId,
  } as const;

  const [
    fraudProofUtxo,
    activeOperatorsRootUtxo,
    schedulerUtxo,
    hubOracleUtxo,
  ] = await Promise.all([
    requireSingletonUtxo({
      lucid,
      address: contracts.fraudProofAddress,
      unit: fraudProofUnit,
      label: "fraud-proof token",
    }),
    requireSingletonUtxo({
      lucid,
      address: contracts.activeOperatorsAddress,
      unit: activeOperatorsRootUnit,
      label: "active-operators root",
    }),
    requireSingletonUtxo({
      lucid,
      address: contracts.schedulerAddress,
      unit: schedulerUnit,
      label: "scheduler",
    }),
    requireSingletonUtxo({
      lucid,
      address: credentialToAddress(
        network,
        scriptHashToCredential(contracts.hubOraclePolicyId),
      ),
      unit: hubOracleUnit,
      label: "hub oracle",
    }),
  ]);

  if (fraudProofUtxo.datum == null) {
    throw new Error(
      `Fraud-proof token UTxO ${outRefLabel(fraudProofUtxo)} is missing datum.`,
    );
  }
  const fraudProofDatum = Data.from(fraudProofUtxo.datum, FraudProofTokenDatum);
  if (fraudProofDatum.fraud_prover !== signer.paymentKeyHash) {
    throw new Error(
      `Fraud-proof token prover ${fraudProofDatum.fraud_prover} does not match signer ${signer.paymentKeyHash}.`,
    );
  }

  let topology = await loadStateQueueTopology({
    lucid,
    stateQueueAddress: contracts.stateQueueAddress,
    stateQueuePolicyId: contracts.stateQueuePolicyId,
  });
  const initialTarget = topology.nodeByHeaderHash.get(headerHash);
  if (initialTarget === undefined) {
    throw new Error(`State queue does not contain block ${headerHash}.`);
  }
  const initialStateQueueRootOutRef = outRefLabel(topology.root.utxo);
  const initialTargetOutRef = outRefLabel(initialTarget.utxo);
  const initialTargetHasSuccessor =
    topology.successorByHeaderHash.has(headerHash);
  if (!awaitConfirmation && initialTargetHasSuccessor) {
    throw new Error(
      "Removing a non-tail fraudulent block requires --await-confirmation so each successor removal can be confirmed and refetched before the next transaction.",
    );
  }
  let stateQueueMutationLease: StateQueueMutationLease | undefined;
  let stateQueueMutationLeaseReleased = false;
  if (initialTargetHasSuccessor) {
    if (stateQueueMutationLeaseCoordinator === undefined) {
      throw new Error(
        "Removing a non-tail fraudulent block requires a live Midgard node state-queue mutation lease. Pass --midgard-node-url and --midgard-node-admin-key so block commitment and merge workers are excluded during multi-step removal.",
      );
    }
    stateQueueMutationLease =
      await stateQueueMutationLeaseCoordinator.acquire();
    try {
      topology = await loadStateQueueTopology({
        lucid,
        stateQueueAddress: contracts.stateQueueAddress,
        stateQueuePolicyId: contracts.stateQueuePolicyId,
      });
      if (!topology.nodeByHeaderHash.has(headerHash)) {
        throw new Error(
          `State queue no longer contains block ${headerHash} after acquiring the mutation lease.`,
        );
      }
    } catch (error) {
      await stateQueueMutationLease.fail(formatUnknownError(error));
      throw error;
    }
  }

  const txValidityWindow = () => {
    const now = BigInt(Date.now());
    return {
      txValidFrom: validFrom ?? now - DEFAULT_REMOVE_VALIDITY_BACKDATE_MS,
      txValidTo: validTo ?? now + DEFAULT_REMOVE_VALIDITY_WINDOW_MS,
    };
  };
  const loadRegisteredOperatorsRootUtxo = () =>
    requireSingletonUtxo({
      lucid,
      address: contracts.registeredOperatorsAddress,
      unit: toUnit(
        contracts.registeredOperatorsPolicyId,
        REGISTERED_OPERATORS_ROOT_ASSET_NAME,
      ),
      label: "registered-operators root",
    });

  const submitRemovalTransaction = async ({
    kind,
    anchor,
    removed,
  }: {
    readonly kind: RemoveTransactionKind;
    readonly anchor: StateQueueUTxO;
    readonly removed: StateQueueUTxO;
  }): Promise<RemoveTransactionResult> => {
    await stateQueueMutationLease?.renew();
    const removedHeaderHash = await requireStateQueueHeaderHash(removed);
    const removedHeader = await Effect.runPromise(
      getHeaderV1FromStateQueueDatum(removed.datum),
    );
    const fraudulentOperator = removedHeader.operatorVkey;
    const currentSchedulerUtxo = await requireSingletonUtxo({
      lucid,
      address: contracts.schedulerAddress,
      unit: schedulerUnit,
      label: "scheduler",
    });
    let txLayout: RemoveFraudulentBlockLayout | undefined;
    const operatorSlashingPlan = await resolveOperatorSlashingPlan({
      lucid,
      contracts,
      operator: fraudulentOperator,
      schedulerUtxo: currentSchedulerUtxo,
      activeOperatorsRootUnit,
      retiredOperatorsRootUnit,
    });
    const registeredOperatorsRootForSlashing =
      operatorSlashingPlan.approach === "SlashActiveOperator" &&
      operatorSlashingPlan.schedulerPlan.kind === "rewind"
        ? await loadRegisteredOperatorsRootUtxo()
        : undefined;
    const slashingPlan = buildSlashingInputs({
      plan: operatorSlashingPlan,
      operator: fraudulentOperator,
      contracts,
      hubOracleUtxo,
      ...(registeredOperatorsRootForSlashing === undefined
        ? {}
        : { registeredOperatorsRootUtxo: registeredOperatorsRootForSlashing }),
    });
    const { txValidFrom, txValidTo } = txValidityWindow();
    const feeInput = selectFeeInput(await lucid.wallet().getUtxos());
    const slashing = slashingPlan.buildSlashing(txValidTo);
    const stateQueueMintRedeemer = makeStateQueueRemoveMintRedeemer({
      kind,
      anchor,
      removed,
      fraudulentOperator,
      fraudulentBlocksHeaderHash: headerHash,
      fraudProofRefInput: fraudProofUtxo,
      slashing,
      contracts,
      onLayout: (layout) => {
        txLayout = layout;
      },
    });
    const tx =
      kind === "remove-successor"
        ? incompleteRemoveFraudulentBlocksLinkTxProgram(
            lucid,
            stateQueueConfig,
            {
              fraudulentBlockUTxO: anchor,
              removedBlockUTxO: removed,
              additionalInputs: [feeInput],
              validFrom: txValidFrom,
              validTo: txValidTo,
              fraudulentOperator,
              fraudulentBlocksHeaderHash: headerHash,
              fraudProofRefInput: fraudProofUtxo,
              additionalRefInputs: slashingPlan.additionalRefInputs,
              slashing,
              stateQueueSpendingScript: contracts.stateQueueSpendingScript,
              stateQueueMintingScript: contracts.stateQueueMintingScript,
              referenceScripts,
              stateQueueMintRedeemer,
            },
          )
        : incompleteRemoveLastFraudulentBlockHeaderTxProgram(
            lucid,
            stateQueueConfig,
            {
              anchorUTxO: anchor,
              fraudulentBlockUTxO: removed,
              additionalInputs: [feeInput],
              validFrom: txValidFrom,
              validTo: txValidTo,
              fraudulentOperator,
              fraudulentBlocksHeaderHash: headerHash,
              fraudProofRefInput: fraudProofUtxo,
              additionalRefInputs: slashingPlan.additionalRefInputs,
              slashing,
              stateQueueSpendingScript: contracts.stateQueueSpendingScript,
              stateQueueMintingScript: contracts.stateQueueMintingScript,
              referenceScripts,
              stateQueueMintRedeemer,
            },
          );
    const unsigned = await tx.addSignerKey(signer.paymentKeyHash).complete({
      localUPLCEval: true,
    });
    if (txLayout === undefined) {
      throw new Error(
        "BuildTxWithRedeemer did not resolve remove-fraudulent-block layout.",
      );
    }

    const signed = await unsigned.sign.withWallet().complete();
    const txHash = await signed.submit();
    if (awaitConfirmation) {
      await lucid.awaitTx(txHash, DEFAULT_CONFIRMATION_POLL_MS);
    }
    await stateQueueMutationLease?.renew();
    return {
      kind,
      txHash,
      removedHeaderHash,
      removedOperator: fraudulentOperator,
      stateQueueBlockOutRef: outRefLabel(removed.utxo),
      operatorNodeOutRef: slashingPlan.removedOperatorNodeOutRef,
      registeredOperatorsRootOutRef: slashingPlan.registeredOperatorsRootOutRef,
      slashingApproach: slashingPlan.approach,
      layout: layoutToJson(txLayout),
    };
  };

  try {
    const transactions: RemoveTransactionResult[] = [];
    while (true) {
      const proofedBlock = topology.nodeByHeaderHash.get(headerHash);
      if (proofedBlock === undefined) {
        throw new Error(
          `State queue no longer contains fraud-proved block ${headerHash} before final removal.`,
        );
      }
      const successor = topology.successorByHeaderHash.get(headerHash);
      if (successor === undefined) {
        break;
      }
      transactions.push(
        await submitRemovalTransaction({
          kind: "remove-successor",
          anchor: proofedBlock,
          removed: successor,
        }),
      );
      topology = await loadStateQueueTopology({
        lucid,
        stateQueueAddress: contracts.stateQueueAddress,
        stateQueuePolicyId: contracts.stateQueuePolicyId,
      });
    }

    const finalTarget = topology.nodeByHeaderHash.get(headerHash);
    if (finalTarget === undefined) {
      throw new Error(
        `State queue no longer contains fraud-proved block ${headerHash}.`,
      );
    }
    const finalAnchor = topology.predecessorByHeaderHash.get(headerHash);
    if (finalAnchor === undefined) {
      throw new Error(
        `State queue block ${headerHash} is not reachable from the confirmed-state root.`,
      );
    }
    transactions.push(
      await submitRemovalTransaction({
        kind: "remove-target",
        anchor: finalAnchor,
        removed: finalTarget,
      }),
    );
    const finalTransaction = transactions[transactions.length - 1]!;
    if (stateQueueMutationLease !== undefined) {
      await stateQueueMutationLease.release();
      stateQueueMutationLeaseReleased = true;
    }

    return {
      txHash: finalTransaction.txHash,
      walletSource: signer.source,
      proverAddress: signer.address,
      fraudProver: signer.paymentKeyHash,
      fraudCategory: contracts.fraudCategory,
      fraudCategoryId: contracts.fraudCategoryId,
      fraudulentHeaderHash: headerHash,
      stateQueueBlockOutRef: initialTargetOutRef,
      stateQueueRootOutRef: initialStateQueueRootOutRef,
      fraudProofOutRef: outRefLabel(fraudProofUtxo),
      activeOperatorsRootOutRef: outRefLabel(activeOperatorsRootUtxo),
      activeOperatorNodeOutRef:
        finalTransaction.slashingApproach === "SlashActiveOperator"
          ? finalTransaction.operatorNodeOutRef
          : null,
      schedulerOutRef: outRefLabel(schedulerUtxo),
      hubOracleOutRef: outRefLabel(hubOracleUtxo),
      registeredOperatorsRootOutRef:
        transactions.find((tx) => tx.registeredOperatorsRootOutRef !== null)
          ?.registeredOperatorsRootOutRef ?? null,
      referenceScriptOutRefs: referenceScriptOutRefs(referenceScripts),
      transactions,
      layout: finalTransaction.layout,
      awaitedConfirmation: awaitConfirmation,
      stateQueueMutationLease:
        stateQueueMutationLease === undefined
          ? null
          : {
              token: stateQueueMutationLease.token,
              source: stateQueueMutationLease.source,
              released: stateQueueMutationLeaseReleased,
            },
    };
  } catch (error) {
    if (
      stateQueueMutationLease !== undefined &&
      !stateQueueMutationLeaseReleased
    ) {
      await stateQueueMutationLease.fail(formatUnknownError(error));
    }
    throw error;
  }
};

export const submitRemoveFraudulentBlockFromFiles = async (
  config: RemoveFraudulentBlockCliConfig,
): Promise<SubmitRemoveFraudulentBlockResult> => {
  const stateQueueMutationLeaseCoordinator =
    resolveStateQueueLeaseCoordinator(config);
  const [lucid, blueprint, deploymentInfo] = await Promise.all([
    makeLucidForSubmit(config),
    readJsonFile(config.blueprintPath),
    readJsonFile(config.deploymentInfoPath),
  ]);
  const signer = resolveProverSigner(config);
  return await submitRemoveFraudulentBlock({
    lucid,
    blueprint,
    deploymentInfo,
    network: config.network,
    signer,
    fraudCategory: config.fraudCategory,
    fraudulentHeaderHash: config.fraudulentHeaderHash,
    awaitConfirmation: config.awaitConfirmation,
    requireReferenceScripts: true,
    stateQueueMutationLeaseCoordinator,
  });
};
