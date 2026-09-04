import { formatUnknownError } from "@al-ft/midgard-core";
import { parseDeploymentManifestEconomics } from "@al-ft/midgard-core/deployment-manifest-identity";
import {
  ACTIVE_OPERATOR_NODE_ASSET_NAME_PREFIX,
  ACTIVE_OPERATORS_ROOT_ASSET_NAME,
  ActiveOperatorMintRedeemer,
  type ActiveOperatorMintRedeemer as ActiveOperatorMintRedeemerData,
  type EmulatorStateQueueRemoveSlashingParams,
  encodeLinkedListNodeView,
  fetchCorrectionLockUTxOProgram,
  FRAUD_PROOF_CATALOGUE_CATEGORY_IDS,
  FRAUD_PROOF_CATALOGUE_ID_BYTE_COUNT,
  FraudProofTokenDatum,
  type FraudProverRewardPlan,
  getHeaderFromStateQueueDatum,
  getLinkedListNodeViewFromUTxO,
  hashBlockHeader,
  HUB_ORACLE_ASSET_NAME,
  incompleteRemoveFraudulentBlocksLinkTxProgram,
  incompleteRemoveLastFraudulentBlockHeaderTxProgram,
  type LinkedListNodeView,
  type OutputReference,
  outputReferenceFromUTxO,
  REGISTERED_OPERATORS_ROOT_ASSET_NAME,
  requireInputIndex,
  requireMintRedeemerIndex,
  requireOwnMintPurpose,
  requireOwnSpendPurpose,
  requireReferenceInputIndex,
  requireSpendRedeemerIndex,
  requireUniqueOutputIndex,
  resolveFraudProverRewardOutputIndex,
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
  utxoToStateQueueUTxO,
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
  resolveFaultProofDeploymentContracts,
  resolveProverSigner,
  type SubmitProviderConfig,
  type SupportedFaultProofCategoryName,
} from "./runtime.js";
import { selectFeeInput } from "./submit-step-01.js";
import {
  CapturedLocallyEvaluatedTransaction,
  type FraudProofPreSubmitBoundary,
  reachFraudProofPreSubmitBoundary,
  workflowReferenceScriptsUsedByTransaction,
} from "./workflow/transaction-boundary.js";

export const STATE_QUEUE_REMOVAL_VALIDITY_WINDOW_MS = 300_000n;
export const STATE_QUEUE_REMOVAL_VALIDITY_BACKDATE_MS = 120_000n;
const DEFAULT_NODE_ADMIN_KEY_ENV = "MIDGARD_NODE_ADMIN_KEY";
const STATE_QUEUE_MUTATION_LEASE_ENDPOINT = "stateQueueMutationLease";
const FAULT_PROOF_LEASE_HOLDER = "fault_proof_removal";

export type FraudSlashEconomicsPolicy = Readonly<{
  profile: "public-preprod-launch-v1" | "bounded-acceptance-v1";
  requiredBondLovelace: bigint;
  slashingPenaltyLovelace: bigint;
  inactivitySlashingPenaltyLovelace: bigint;
  fraudProverRewardLovelace: bigint;
  proverCollateralFloorLovelace: bigint;
}>;

export const fraudSlashEconomicsFromDeploymentManifest = (
  deploymentInfo: unknown,
): FraudSlashEconomicsPolicy => {
  if (
    typeof deploymentInfo !== "object" ||
    deploymentInfo === null ||
    Array.isArray(deploymentInfo) ||
    (Object.getPrototypeOf(deploymentInfo) !== Object.prototype &&
      Object.getPrototypeOf(deploymentInfo) !== null)
  ) {
    throw new Error("Deployment manifest must be an object.");
  }
  const manifest = deploymentInfo as { readonly economics?: unknown };
  const economics = parseDeploymentManifestEconomics(manifest.economics);
  return {
    profile: economics.profile,
    requiredBondLovelace: BigInt(economics.requiredBondLovelace),
    slashingPenaltyLovelace: BigInt(economics.slashingPenaltyLovelace),
    inactivitySlashingPenaltyLovelace: BigInt(
      economics.inactivitySlashingPenaltyLovelace,
    ),
    fraudProverRewardLovelace: BigInt(economics.fraudProverRewardLovelace),
    proverCollateralFloorLovelace: BigInt(
      economics.proverCollateralFloorLovelace,
    ),
  };
};

export const resolveFraudSlashEconomics = (
  economics: FraudSlashEconomicsPolicy,
  operatorNodeLovelace: bigint,
): Readonly<{
  requiredBondLovelace: bigint;
  fraudProverRewardLovelace: bigint;
  exactFeeLovelace: bigint;
  tranche: "full" | "partially-inactivity-slashed";
}> => {
  const partialBond =
    economics.requiredBondLovelace -
    economics.inactivitySlashingPenaltyLovelace;
  if (
    economics.requiredBondLovelace !==
      economics.slashingPenaltyLovelace + economics.fraudProverRewardLovelace ||
    economics.inactivitySlashingPenaltyLovelace <= 0n ||
    economics.inactivitySlashingPenaltyLovelace >=
      economics.slashingPenaltyLovelace
  ) {
    throw new Error("Deployment economics violate F04 slash relations.");
  }
  if (operatorNodeLovelace === economics.requiredBondLovelace) {
    return {
      requiredBondLovelace: economics.requiredBondLovelace,
      fraudProverRewardLovelace: economics.fraudProverRewardLovelace,
      exactFeeLovelace: economics.slashingPenaltyLovelace,
      tranche: "full",
    };
  }
  if (operatorNodeLovelace === partialBond) {
    return {
      requiredBondLovelace: economics.requiredBondLovelace,
      fraudProverRewardLovelace: economics.fraudProverRewardLovelace,
      exactFeeLovelace:
        economics.slashingPenaltyLovelace -
        economics.inactivitySlashingPenaltyLovelace,
      tranche: "partially-inactivity-slashed",
    };
  }
  throw new Error(
    `Operator bond must be exactly ${economics.requiredBondLovelace.toString()} or ${partialBond.toString()} lovelace; found ${operatorNodeLovelace.toString()}.`,
  );
};

export const fraudRemovalUsesWalletCoinSelection = (
  approach:
    | "SlashActiveOperator"
    | "SlashRetiredOperator"
    | "OperatorAlreadySlashed",
): boolean => approach === "OperatorAlreadySlashed";

const utxoLovelace = (utxo: UTxO): bigint => utxo.assets.lovelace ?? 0n;

const sumUtxoLovelace = (utxos: readonly UTxO[]): bigint =>
  utxos.reduce((total, utxo) => total + utxoLovelace(utxo), 0n);

const assertExactFraudSlashLovelaceConservation = ({
  stateQueueAnchor,
  removedStateQueueNode,
  slashing,
  economics,
}: {
  readonly stateQueueAnchor: StateQueueUTxO;
  readonly removedStateQueueNode: StateQueueUTxO;
  readonly slashing: Exclude<
    EmulatorStateQueueRemoveSlashingParams,
    { readonly kind: "operatorAlreadySlashed" }
  >;
  readonly economics: ReturnType<typeof resolveFraudSlashEconomics>;
}): void => {
  const stateQueueInputLovelace =
    utxoLovelace(stateQueueAnchor.utxo) +
    utxoLovelace(removedStateQueueNode.utxo);
  const stateQueueOutputLovelace = stateQueueInputLovelace;
  const rewardLovelace = slashing.fraudProverReward?.lovelace;
  if (rewardLovelace !== economics.fraudProverRewardLovelace) {
    throw new Error(
      `Fraud slash reward must conserve exactly ${economics.fraudProverRewardLovelace.toString()} lovelace; found ${rewardLovelace?.toString() ?? "none"}.`,
    );
  }

  const operatorInputLovelace =
    slashing.kind === "slashActiveOperator"
      ? sumUtxoLovelace([
          ...slashing.activeOperatorInputs,
          ...(slashing.schedulerSpend === undefined
            ? []
            : [slashing.schedulerSpend.input]),
        ])
      : sumUtxoLovelace(slashing.retiredOperatorInputs);
  const operatorOutputLovelace =
    slashing.kind === "slashActiveOperator"
      ? (slashing.continuedActiveOperatorAnchorOutput?.assets.lovelace ?? 0n) +
        (slashing.schedulerSpend?.continuedOutput.assets.lovelace ?? 0n)
      : (slashing.continuedRetiredOperatorAnchorOutput?.assets.lovelace ?? 0n);
  const totalInputs = stateQueueInputLovelace + operatorInputLovelace;
  const totalOutputsAndFee =
    stateQueueOutputLovelace +
    operatorOutputLovelace +
    rewardLovelace +
    economics.exactFeeLovelace;
  if (totalInputs !== totalOutputsAndFee) {
    throw new Error(
      `Fraud slash lovelace is not exactly conserved: inputs=${totalInputs.toString()}, outputs_and_fee=${totalOutputsAndFee.toString()}, residual=${(totalInputs - totalOutputsAndFee).toString()}.`,
    );
  }
};

const REFERENCE_SCRIPT_NAMES = [
  "correctionLockSpend",
  "stateQueueSpend",
  "stateQueueMint",
  "stateQueueFraudRemovalWithdraw",
  "activeOperatorsSpend",
  "activeOperatorsMint",
  "retiredOperatorsSpend",
  "retiredOperatorsMint",
  "schedulerSpend",
] as const;

const STATE_QUEUE_REMOVE_REFERENCE_SCRIPT_NAMES = [
  "correctionLockSpend",
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
  readonly correctionLockAddress: string;
  readonly correctionLockSpendingScript: Script;
  readonly stateQueuePolicyId: string;
  readonly stateQueueAddress: string;
  readonly stateQueueSpendingScript: Script;
  readonly stateQueueMintingScript: Script;
  readonly stateQueueFraudRemovalWithdrawalScript: Script;
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
  readonly fraudCategory: RemoveFraudulentBlockCategoryLabel;
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

export type RemoveFraudulentBlockFraudCategory =
  SupportedFaultProofCategoryName;

/**
 * Explicit already-resolved category record for fault-proof families that
 * predate their catalogue registration (none at present; the mechanism stays
 * for the next pre-registration family).
 * These families have no SDK contract-chain builder and no category id in any
 * deployment manifest yet, so removal cannot resolve them the canonical way;
 * per the families' submitter convention the caller supplies the
 * already-resolved facts instead, and every fail-closed check the canonical
 * path runs still runs: the shared fraud-proof pair is checked against the
 * `fraudProofMint`/`fraudProofSpend` deployment entries, the step-01 hash
 * against the named deployment entry, and the category id against collisions
 * with every canonical registered id. The on-chain removal handler is
 * category-agnostic — it authenticates the fraud-proof reference input by
 * policy id and reads the header hash off the asset-name suffix — so no
 * category-specific script participates in the removal transaction itself.
 */
export type RemoveFraudulentBlockExplicitCategory = {
  /** Category label used in failure messages and the result payload. */
  readonly name: string;
  /**
   * The 4-byte hex category id the family's computation thread and
   * fraud-proof token were minted under.
   */
  readonly categoryId: string;
  /**
   * Deployment-manifest entry whose `scriptHash` pins the family's step-01
   * spending script.
   */
  readonly firstStepDeploymentEntry: string;
  /** The step-01 spending-script hash of the already-resolved family chain. */
  readonly firstStepScriptHash: string;
  /** The shared fraud-proof pair the family chain was parameterized with. */
  readonly fraudProof: {
    readonly policyId: string;
    readonly spendingScriptHash: string;
    readonly spendingScriptAddress: string;
  };
};

/**
 * A canonical removable category name, or the label of an explicit
 * pre-registration category. The `string & {}` half keeps the canonical
 * literals in editor completion without narrowing away explicit labels.
 */
export type RemoveFraudulentBlockCategoryLabel =
  | RemoveFraudulentBlockFraudCategory
  | (string & {});

export type StateQueueMutationLease = {
  readonly token: string;
  readonly source: string;
  readonly renew: () => Promise<void>;
  readonly release: () => Promise<void>;
  readonly fail: (error: string) => Promise<void>;
};

export type StateQueueMutationLeaseIdentity = Pick<
  StateQueueMutationLease,
  "token" | "source"
>;

export type StateQueueMutationLeaseCoordinator = {
  readonly acquire: () => Promise<StateQueueMutationLease>;
  /** Reconstructs the exact journaled fencing lease; never acquires a new one. */
  readonly resume?: (
    identity: StateQueueMutationLeaseIdentity,
  ) => Promise<StateQueueMutationLease>;
};

export type SubmitRemoveFraudulentBlockResult = {
  readonly txHash: string;
  readonly walletSource: string;
  readonly proverAddress: string;
  readonly fraudProver: string;
  readonly fraudCategory: RemoveFraudulentBlockCategoryLabel;
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
  holder = FAULT_PROOF_LEASE_HOLDER,
}: {
  readonly midgardNodeUrl: string;
  readonly adminKey: string;
  readonly ttlMs?: number;
  readonly holder?: string;
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
  const leaseForToken = (token: string): StateQueueMutationLease => {
    if (token.trim().length === 0 || token.trim() !== token) {
      throw new Error("State-queue mutation lease token must be canonical");
    }
    const leaseBody = (
      action: "renew" | "release" | "fail",
      extra: Record<string, unknown> = {},
    ) =>
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
  };

  return {
    acquire: async () => {
      const json = await post({
        action: "acquire",
        holder,
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
      return leaseForToken(token);
    },
    resume: async ({ token, source }) => {
      if (source !== nodeUrl) {
        throw new Error(
          `Refusing state-queue mutation lease from a different coordinator: expected=${nodeUrl} actual=${source}`,
        );
      }
      return leaseForToken(token);
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
  readonly deploymentInfo: unknown;
  readonly network: Network;
  readonly fraudCategory: RemoveFraudulentBlockFraudCategory;
}): Promise<RemoveFraudulentBlockContracts> => {
  const resolved = await resolveFaultProofDeploymentContracts({
    blueprint,
    deploymentInfo,
    network,
    categoryName: fraudCategory,
    requireFraudProofSpend: true,
  });

  return assembleRemovalContracts({
    deploymentInfo: resolved.deploymentInfo,
    network,
    hubOraclePolicyId: resolved.hubOraclePolicyId,
    fraudProofPolicyId: resolved.contracts.fraudProof.policyId,
    fraudProofAddress: resolved.contracts.fraudProof.spendingScriptAddress,
    fraudCategoryId: resolved.category.categoryId,
    fraudCategory,
  });
};

/**
 * Explicit-category counterpart of `buildRemovalContracts`: a
 * pre-registration family has no SDK builder and no catalogue entry, so the
 * caller's already-resolved facts stand in for the canonical resolution —
 * while every fail-closed cross-check the canonical path performs still runs
 * against the deployment manifest: the shared fraud-proof pair against the
 * `fraudProofMint`/`fraudProofSpend` entries, the step-01 hash against the
 * entry the record names, and the category id against every canonical
 * registered id (a collision would mean the "pre-registration" id actually
 * belongs to a registered family, which must resolve canonically).
 */
const buildExplicitRemovalContracts = ({
  deploymentInfo,
  network,
  category,
}: {
  readonly deploymentInfo: unknown;
  readonly network: Network;
  readonly category: RemoveFraudulentBlockExplicitCategory;
}): RemoveFraudulentBlockContracts => {
  const parsedDeploymentInfo = parseContractDeploymentInfo(deploymentInfo);
  const hubOraclePolicyId = requireDeploymentScriptHash(
    parsedDeploymentInfo,
    "hubOracleMint",
  );
  const deployedFraudProofPolicyId = requireDeploymentScriptHash(
    parsedDeploymentInfo,
    "fraudProofMint",
  );
  if (category.fraudProof.policyId !== deployedFraudProofPolicyId) {
    throw new Error(
      `${category.name} explicit category names fraud-proof policy ` +
        `${category.fraudProof.policyId}, but the deployment's fraudProofMint ` +
        `entry pins ${deployedFraudProofPolicyId}.`,
    );
  }
  const deployedFraudProofSpendHash = requireDeploymentScriptHash(
    parsedDeploymentInfo,
    "fraudProofSpend",
  );
  if (category.fraudProof.spendingScriptHash !== deployedFraudProofSpendHash) {
    throw new Error(
      `${category.name} explicit category names fraud-proof spending script ` +
        `${category.fraudProof.spendingScriptHash}, but the deployment's ` +
        `fraudProofSpend entry pins ${deployedFraudProofSpendHash}.`,
    );
  }
  const deployedFirstStepHash = requireDeploymentScriptHash(
    parsedDeploymentInfo,
    category.firstStepDeploymentEntry,
  );
  if (category.firstStepScriptHash !== deployedFirstStepHash) {
    throw new Error(
      `${category.name} explicit category names step-01 script ` +
        `${category.firstStepScriptHash}, but the deployment's ` +
        `${category.firstStepDeploymentEntry} entry pins ` +
        `${deployedFirstStepHash}.`,
    );
  }
  for (const [registeredName, registeredId] of Object.entries(
    FRAUD_PROOF_CATALOGUE_CATEGORY_IDS,
  )) {
    if (registeredId === category.categoryId) {
      throw new Error(
        `${category.name} explicit category id ${category.categoryId} ` +
          `collides with the registered ${registeredName} category; a ` +
          `registered family must resolve through the canonical catalogue.`,
      );
    }
  }
  return assembleRemovalContracts({
    deploymentInfo: parsedDeploymentInfo,
    network,
    hubOraclePolicyId,
    fraudProofPolicyId: category.fraudProof.policyId,
    fraudProofAddress: category.fraudProof.spendingScriptAddress,
    fraudCategoryId: category.categoryId,
    fraudCategory: category.name,
  });
};

/**
 * The category-independent half of removal-contract resolution: every
 * script, address and policy id here comes straight out of the deployment
 * manifest, with the already-verified category facts passed through.
 */
const assembleRemovalContracts = ({
  deploymentInfo,
  network,
  hubOraclePolicyId,
  fraudProofPolicyId,
  fraudProofAddress,
  fraudCategoryId,
  fraudCategory,
}: {
  readonly deploymentInfo: ContractDeploymentInfo;
  readonly network: Network;
  readonly hubOraclePolicyId: string;
  readonly fraudProofPolicyId: string;
  readonly fraudProofAddress: string;
  readonly fraudCategoryId: string;
  readonly fraudCategory: RemoveFraudulentBlockCategoryLabel;
}): RemoveFraudulentBlockContracts => {
  const stateQueueSpendingScript = requireDeploymentScript(
    deploymentInfo,
    "stateQueueSpend",
  );
  const stateQueueMintingScript = requireDeploymentScript(
    deploymentInfo,
    "stateQueueMint",
  );
  const stateQueueFraudRemovalWithdrawalScript = requireDeploymentScript(
    deploymentInfo,
    "stateQueueFraudRemovalWithdraw",
  );
  const correctionLockSpendingScript = requireDeploymentScript(
    deploymentInfo,
    "correctionLockSpend",
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
    correctionLockAddress: validatorToAddress(
      network,
      correctionLockSpendingScript as SpendingValidator,
    ),
    correctionLockSpendingScript,
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
    stateQueueFraudRemovalWithdrawalScript,
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
    fraudProofPolicyId,
    fraudProofAddress,
    fraudCategoryId,
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
}): Promise<
  StateQueueRemoveReferenceScriptUTxOs & {
    readonly stateQueueFraudRemovalWithdraw: UTxO;
  }
> => {
  const stateQueueFraudRemovalWithdraw = await requireDeploymentReferenceScript(
    {
      lucid,
      deploymentInfo,
      name: "stateQueueFraudRemovalWithdraw",
    },
  );
  if (!requireReferenceScripts) {
    return { stateQueueFraudRemovalWithdraw };
  }
  const [
    correctionLockSpend,
    stateQueueSpend,
    stateQueueMint,
    activeOperatorsSpend,
    activeOperatorsMint,
    retiredOperatorsSpend,
    retiredOperatorsMint,
    schedulerSpend,
  ] = await Promise.all(
    STATE_QUEUE_REMOVE_REFERENCE_SCRIPT_NAMES.map((name) =>
      requireDeploymentReferenceScript({ lucid, deploymentInfo, name }),
    ),
  );
  return {
    stateQueueFraudRemovalWithdraw,
    correctionLockSpend,
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
  referenceScripts:
    | (StateQueueRemoveReferenceScriptUTxOs & {
        readonly stateQueueFraudRemovalWithdraw: UTxO;
      })
    | undefined,
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
    getHeaderFromStateQueueDatum(stateQueueNode.datum),
  );
  const computedHeaderHash = await Effect.runPromise(hashBlockHeader(header));
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
  fraudProverReward,
}: {
  readonly plan: Extract<
    OperatorSlashingPlan,
    { readonly approach: "SlashActiveOperator" }
  >;
  readonly operator: string;
  readonly contracts: RemoveFraudulentBlockContracts;
  readonly hubOracleUtxo: UTxO;
  readonly registeredOperatorsRootUtxo?: UTxO;
  readonly fraudProverReward?: FraudProverRewardPlan;
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
        ...(fraudProverReward === undefined ? {} : { fraudProverReward }),
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
  fraudProverReward,
}: {
  readonly plan: Extract<
    OperatorSlashingPlan,
    { readonly approach: "SlashRetiredOperator" }
  >;
  readonly operator: string;
  readonly contracts: RemoveFraudulentBlockContracts;
  readonly hubOracleUtxo: UTxO;
  readonly fraudProverReward?: FraudProverRewardPlan;
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
      ...(fraudProverReward === undefined ? {} : { fraudProverReward }),
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
  fraudProverReward,
}: {
  readonly plan: OperatorSlashingPlan;
  readonly operator: string;
  readonly contracts: RemoveFraudulentBlockContracts;
  readonly hubOracleUtxo: UTxO;
  readonly registeredOperatorsRootUtxo?: UTxO;
  /**
   * D3 reward routing. Only the bond-consuming approaches can carry it;
   * `OperatorAlreadySlashed` consumes no bond and pays no reward, which is the
   * D4 exclusivity ruling expressed in the redeemer's own shape.
   */
  readonly fraudProverReward?: FraudProverRewardPlan;
}): SlashingTxPlan => {
  switch (plan.approach) {
    case "SlashActiveOperator":
      return buildActiveSlashingInputs({
        plan,
        operator,
        contracts,
        hubOracleUtxo,
        registeredOperatorsRootUtxo,
        ...(fraudProverReward === undefined ? {} : { fraudProverReward }),
      });
    case "SlashRetiredOperator":
      return buildRetiredSlashingInputs({
        plan,
        operator,
        contracts,
        hubOracleUtxo,
        ...(fraudProverReward === undefined ? {} : { fraudProverReward }),
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
            m_fraud_prover_reward_output_index:
              resolveFraudProverRewardOutputIndex(
                ctx,
                slashing.fraudProverReward,
                "remove-fraudulent-block active-operator fraud-prover reward",
              ),
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
            m_fraud_prover_reward_output_index:
              resolveFraudProverRewardOutputIndex(
                ctx,
                slashing.fraudProverReward,
                "remove-fraudulent-block retired-operator fraud-prover reward",
              ),
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
  yieldRefInput,
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
  readonly yieldRefInput: UTxO;
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
      yield_to_ref_input_index: requireReferenceInputIndex(
        ctx,
        yieldRefInput,
        "remove-fraudulent-block yield",
      ),
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
  fraudProverRewardLovelace,
  preSubmitBoundary,
}: {
  readonly lucid: LucidEvolution;
  readonly blueprint: unknown;
  readonly deploymentInfo: unknown;
  readonly network: Network;
  readonly signer: ResolvedProverSigner;
  /**
   * Canonical catalogue category resolved from the production manifest, or —
   * for a family that predates its catalogue registration — the explicit
   * already-resolved category record
   * (see {@link RemoveFraudulentBlockExplicitCategory}).
   */
  readonly fraudCategory?:
    | RemoveFraudulentBlockFraudCategory
    | RemoveFraudulentBlockExplicitCategory;
  readonly fraudulentHeaderHash: string;
  readonly awaitConfirmation?: boolean;
  readonly requireReferenceScripts?: boolean;
  readonly validFrom?: bigint;
  readonly validTo?: bigint;
  readonly stateQueueMutationLeaseCoordinator?: StateQueueMutationLeaseCoordinator;
  /**
   * Optional assertion of the deployment-manifest `fraudProverRewardLovelace`;
   * omission still routes the release profile's mandatory nonzero reward.
   */
  readonly fraudProverRewardLovelace?: bigint;
  /** Production workflow seam for each descendant/target removal tx. */
  readonly preSubmitBoundary?: FraudProofPreSubmitBoundary;
}): Promise<SubmitRemoveFraudulentBlockResult> => {
  const headerHash = parseHex(
    fraudulentHeaderHash,
    "--fraudulent-header-hash",
    28,
  );
  const parsedDeploymentInfo = parseContractDeploymentInfo(deploymentInfo);
  const deploymentEconomics =
    fraudSlashEconomicsFromDeploymentManifest(deploymentInfo);
  const contracts =
    typeof fraudCategory === "string"
      ? await buildRemovalContracts({
          blueprint,
          deploymentInfo,
          network,
          fraudCategory,
        })
      : buildExplicitRemovalContracts({
          deploymentInfo,
          network,
          category: fraudCategory,
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

  if (
    fraudProverRewardLovelace !== undefined &&
    fraudProverRewardLovelace !== deploymentEconomics.fraudProverRewardLovelace
  ) {
    throw new Error(
      `Fraud-prover reward must equal deployment profile ${deploymentEconomics.profile} amount ${deploymentEconomics.fraudProverRewardLovelace.toString()} lovelace; found ${fraudProverRewardLovelace.toString()}.`,
    );
  }
  const fraudProverRewardPlan = {
    proverEnterpriseAddress: credentialToAddress(network, {
      type: "Key" as const,
      hash: fraudProofDatum.fraud_prover,
    }),
    lovelace: deploymentEconomics.fraudProverRewardLovelace,
  };

  let topology = await loadStateQueueTopology({
    lucid,
    stateQueueAddress: contracts.stateQueueAddress,
    stateQueuePolicyId: contracts.stateQueuePolicyId,
  });
  const initialTarget = topology.nodeByHeaderHash.get(headerHash);
  if (initialTarget === undefined) {
    throw new Error(`State queue does not contain block ${headerHash}.`);
  }
  const fraudulentHeader = await Effect.runPromise(
    getHeaderFromStateQueueDatum(initialTarget.datum),
  );
  const fraudulentOperator = fraudulentHeader.operatorVkey;
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
      txValidFrom: validFrom ?? now - STATE_QUEUE_REMOVAL_VALIDITY_BACKDATE_MS,
      txValidTo: validTo ?? now + STATE_QUEUE_REMOVAL_VALIDITY_WINDOW_MS,
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
    const correctionLockInput = await Effect.runPromise(
      fetchCorrectionLockUTxOProgram(lucid, {
        correctionLockAddress: contracts.correctionLockAddress,
        hubOraclePolicyId: contracts.hubOraclePolicyId,
      }),
    );
    const removedHeaderHash = await requireStateQueueHeaderHash(removed);
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
    const slashEconomics =
      operatorSlashingPlan.approach === "OperatorAlreadySlashed"
        ? null
        : resolveFraudSlashEconomics(
            deploymentEconomics,
            operatorSlashingPlan.removalPlan.node.utxo.assets.lovelace ?? 0n,
          );
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
      ...(fraudProverRewardPlan === undefined
        ? {}
        : { fraudProverReward: fraudProverRewardPlan }),
    });
    const { txValidFrom, txValidTo } = txValidityWindow();
    // A legal operator bond tranche is exactly reward + slash fee.  Do not
    // add a wallet fee input to that branch: its change would be an unrelated
    // second payment to the prover whenever the submitter uses the prover's
    // enterprise wallet.  OperatorAlreadySlashed has no bond and still needs
    // an ordinary fee input for descendant cleanup transactions.
    const additionalInputs =
      slashEconomics === null
        ? [selectFeeInput(await lucid.wallet().getUtxos())]
        : [];
    const slashing = slashingPlan.buildSlashing(txValidTo);
    if (slashEconomics !== null) {
      if (slashing.kind === "operatorAlreadySlashed") {
        throw new Error(
          "Bond-backed fraud slash unexpectedly resolved to OperatorAlreadySlashed.",
        );
      }
      assertExactFraudSlashLovelaceConservation({
        stateQueueAnchor: anchor,
        removedStateQueueNode: removed,
        slashing,
        economics: slashEconomics,
      });
    }
    const stateQueueMintRedeemer = makeStateQueueRemoveMintRedeemer({
      kind,
      anchor,
      removed,
      fraudulentOperator,
      fraudulentBlocksHeaderHash: headerHash,
      fraudProofRefInput: fraudProofUtxo,
      yieldRefInput: referenceScripts.stateQueueFraudRemovalWithdraw,
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
              additionalInputs,
              validFrom: txValidFrom,
              validTo: txValidTo,
              fraudulentOperator,
              fraudulentBlocksHeaderHash: headerHash,
              fraudProofRefInput: fraudProofUtxo,
              fraudProofPolicyId: contracts.fraudProofPolicyId,
              hubOracleRefInput: hubOracleUtxo,
              correctionLockInput,
              correctionLockSpendingScript:
                contracts.correctionLockSpendingScript,
              additionalRefInputs: slashingPlan.additionalRefInputs,
              slashing,
              stateQueueSpendingScript: contracts.stateQueueSpendingScript,
              stateQueueMintingScript: contracts.stateQueueMintingScript,
              referenceScripts,
              yieldWitness: {
                referenceInput: referenceScripts.stateQueueFraudRemovalWithdraw,
                script: contracts.stateQueueFraudRemovalWithdrawalScript,
              },
              stateQueueMintRedeemer,
            },
          )
        : incompleteRemoveLastFraudulentBlockHeaderTxProgram(
            lucid,
            stateQueueConfig,
            {
              anchorUTxO: anchor,
              fraudulentBlockUTxO: removed,
              additionalInputs,
              validFrom: txValidFrom,
              validTo: txValidTo,
              fraudulentOperator,
              fraudulentBlocksHeaderHash: headerHash,
              fraudProofRefInput: fraudProofUtxo,
              fraudProofPolicyId: contracts.fraudProofPolicyId,
              hubOracleRefInput: hubOracleUtxo,
              correctionLockInput,
              correctionLockSpendingScript:
                contracts.correctionLockSpendingScript,
              additionalRefInputs: slashingPlan.additionalRefInputs,
              slashing,
              stateQueueSpendingScript: contracts.stateQueueSpendingScript,
              stateQueueMintingScript: contracts.stateQueueMintingScript,
              referenceScripts,
              yieldWitness: {
                referenceInput: referenceScripts.stateQueueFraudRemovalWithdraw,
                script: contracts.stateQueueFraudRemovalWithdrawalScript,
              },
              stateQueueMintRedeemer,
            },
          );
    const feeBoundTx =
      slashEconomics === null
        ? tx
        : tx.setMinFee(slashEconomics.exactFeeLovelace);
    const unsigned = await feeBoundTx.complete({
      // The bond-backed branch is already proven exactly balanced above.
      // Ordinary coin selection would add an unrelated wallet UTxO and let
      // CML absorb a sub-minimum change residual into the fee, violating the
      // exact F04 fee. Collateral selection remains independent and enabled.
      coinSelection: fraudRemovalUsesWalletCoinSelection(
        operatorSlashingPlan.approach,
      ),
      localUPLCEval: true,
    });
    if (txLayout === undefined) {
      throw new Error(
        "BuildTxWithRedeemer did not resolve remove-fraudulent-block layout.",
      );
    }

    const signed = await unsigned.sign.withWallet().complete();
    const expectedTxHash = await reachFraudProofPreSubmitBoundary({
      signed,
      referenceScripts: workflowReferenceScriptsUsedByTransaction({
        signed,
        candidates: [
          {
            role: "correction-lock-spend",
            utxo: referenceScripts?.correctionLockSpend,
            expectedScript: contracts.correctionLockSpendingScript,
          },
          {
            role: "state-queue-spend",
            utxo: referenceScripts?.stateQueueSpend,
            expectedScript: contracts.stateQueueSpendingScript,
          },
          {
            role: "state-queue-mint",
            utxo: referenceScripts?.stateQueueMint,
            expectedScript: contracts.stateQueueMintingScript,
          },
          {
            role: "active-operators-spend",
            utxo: referenceScripts?.activeOperatorsSpend,
            expectedScript: contracts.activeOperatorsSpendingScript,
          },
          {
            role: "active-operators-mint",
            utxo: referenceScripts?.activeOperatorsMint,
            expectedScript: contracts.activeOperatorsMintingScript,
          },
          {
            role: "retired-operators-spend",
            utxo: referenceScripts?.retiredOperatorsSpend,
            expectedScript: contracts.retiredOperatorsSpendingScript,
          },
          {
            role: "retired-operators-mint",
            utxo: referenceScripts?.retiredOperatorsMint,
            expectedScript: contracts.retiredOperatorsMintingScript,
          },
          {
            role: "scheduler-spend",
            utxo: referenceScripts?.schedulerSpend,
            expectedScript: contracts.schedulerSpendingScript,
          },
        ],
      }),
      boundary: preSubmitBoundary,
    });
    const txHash = await signed.submit();
    if (txHash !== expectedTxHash) {
      throw new Error(
        `Provider returned transaction hash ${txHash}, expected ${expectedTxHash}.`,
      );
    }
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
      proverAddress: fraudProverRewardPlan.proverEnterpriseAddress,
      fraudProver: fraudProofDatum.fraud_prover,
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
    // A production workflow capture deliberately stops after the exact signed
    // body has passed local evaluation. Its adapter retains and renews the
    // acquired lease across durable intent and submission, so failing it here
    // would reopen the append/removal race in that crash boundary.
    if (error instanceof CapturedLocallyEvaluatedTransaction) {
      throw error;
    }
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
