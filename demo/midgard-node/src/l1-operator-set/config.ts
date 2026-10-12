/**
 * What the node's operator set (NC14) reads from the deployment: the three
 * operator lists (registered, active, retired), the scheduler and the hub
 * oracle, each by its address and its authentication policy.
 */
import type { TrackedSet } from "@al-ft/midgard-l1-follower";
import * as SDK from "@al-ft/midgard-sdk";
import { getAddressDetails } from "@lucid-evolution/lucid";

/** The most nodes (root excluded) a healthy active or registered list holds. */
export const OPERATOR_LIST_MAX_NODES = 10_000;

export type OperatorSetContract = Readonly<{
  /** The spending address, as raw address bytes (hex). */
  address: string;
  /** The authentication policy (56 hex). */
  policyId: string;
}>;

export type OperatorListContract = OperatorSetContract &
  Readonly<{
    /** The asset-name prefix of a list node (hex). */
    nodePrefix: string;
    /** The asset name of the list root (hex). */
    rootAssetName: string;
  }>;

export type OperatorSetConfig = Readonly<{
  registered: OperatorListContract;
  active: OperatorListContract;
  retired: OperatorListContract;
  scheduler: OperatorSetContract;
  hubOracle: OperatorSetContract;
}>;

type Validator = Readonly<{ spendingScriptAddress: string; policyId: string }>;

const contractOf = (validator: Validator): OperatorSetContract => ({
  address: getAddressDetails(
    validator.spendingScriptAddress,
  ).address.hex.toLowerCase(),
  policyId: validator.policyId.toLowerCase(),
});

/** The operator set's config of a deployment. */
export const operatorSetConfig = (
  contracts: Readonly<{
    registeredOperators: Validator;
    activeOperators: Validator;
    retiredOperators: Validator;
    scheduler: Validator;
    hubOracle: Validator;
  }>,
): OperatorSetConfig => ({
  registered: {
    ...contractOf(contracts.registeredOperators),
    nodePrefix: SDK.REGISTERED_OPERATOR_NODE_ASSET_NAME_PREFIX,
    rootAssetName: SDK.REGISTERED_OPERATORS_ROOT_ASSET_NAME,
  },
  active: {
    ...contractOf(contracts.activeOperators),
    nodePrefix: SDK.ACTIVE_OPERATOR_NODE_ASSET_NAME_PREFIX,
    rootAssetName: SDK.ACTIVE_OPERATORS_ROOT_ASSET_NAME,
  },
  retired: {
    ...contractOf(contracts.retiredOperators),
    nodePrefix: SDK.RETIRED_OPERATOR_NODE_ASSET_NAME_PREFIX,
    rootAssetName: SDK.RETIRED_OPERATORS_ROOT_ASSET_NAME,
  },
  scheduler: contractOf(contracts.scheduler),
  hubOracle: contractOf(contracts.hubOracle),
});

const contracts = (config: OperatorSetConfig) => [
  config.registered,
  config.active,
  config.retired,
  config.scheduler,
  config.hubOracle,
];

/**
 * The follower tracked set the operator set needs: every output at the five
 * addresses, and the txs minting or burning under their policies (a slash
 * burns an active node and leaves no output at a list address).
 */
export const operatorSetTrackedSet = (
  config: OperatorSetConfig,
): TrackedSet => ({
  addresses: new Set(contracts(config).map((contract) => contract.address)),
  paymentCredentials: new Set(),
  policies: new Set(contracts(config).map((contract) => contract.policyId)),
});
