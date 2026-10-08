/**
 * Whether and how the node's L1 follower runs (N1), from the node's
 * configuration and its deployment.
 */
import type { OriginConfig } from "@al-ft/midgard-l1-follower";
import * as SDK from "@al-ft/midgard-sdk";

import {
  type EventProjectionConfig,
  eventProjectionConfigFromContracts,
} from "../l1-events/index.js";
import {
  type StateQueueProjectionConfig,
  stateQueueProjectionConfig,
} from "../l1-state-queue/index.js";
import type { NodeConfigDep } from "./config.js";

/** How the node's follower runs, or the missing piece that stops it. */
export type L1FollowerPlan =
  | Readonly<{ kind: "unconfigured"; detail: string }>
  | Readonly<{
      kind: "run";
      connectionString: string;
      socketPath: string;
      binaryPath: string;
      nodeConfigPath: string;
      origin: OriginConfig;
      /** k: the manifest's automaticRecoveryMaxDepth. */
      securityParameter: number;
      projection: EventProjectionConfig;
      /** The landed state queue (P1). */
      stateQueue: StateQueueProjectionConfig;
      hubOraclePolicyId: string;
    }>;

const HEX_32 = /^[0-9a-f]{64}$/u;

/** Whether and how the node's follower runs, from its configuration. */
export const l1FollowerPlan = (input: {
  readonly config: Pick<
    NodeConfigDep,
    | "L1_NATIVE_LEDGER"
    | "L1_ORIGIN"
    | "HUB_ORACLE_ONE_SHOT_TX_HASH"
    | "HUB_ORACLE_ONE_SHOT_OUTPUT_INDEX"
    | "NETWORK"
    | "POSTGRES_HOST"
    | "POSTGRES_PORT"
    | "POSTGRES_USER"
    | "POSTGRES_PASSWORD"
    | "POSTGRES_DB"
  >;
  readonly contracts: Parameters<typeof SDK.requireEventHistoryContracts>[0] &
    Readonly<{
      hubOracle: Readonly<{ policyId: string }>;
      stateQueue: Readonly<{ spendingScriptAddress: string; policyId: string }>;
    }>;
  readonly securityParameter: number;
}): L1FollowerPlan => {
  const { config } = input;
  const missing = (detail: string): L1FollowerPlan => ({
    kind: "unconfigured",
    detail,
  });
  if (config.L1_NATIVE_LEDGER === undefined)
    return missing(
      "no local node is configured (L1_NODE_SOCKET_PATH, L1_NODE_CONFIG_PATH, L1_NATIVE_CHAIN_SYNC_BINARY_PATH)",
    );
  if (config.L1_ORIGIN === null) return missing("L1_ORIGIN is not set");
  const oneShotTx = config.HUB_ORACLE_ONE_SHOT_TX_HASH.toLowerCase();
  const oneShotIndex = config.HUB_ORACLE_ONE_SHOT_OUTPUT_INDEX;
  if (
    !HEX_32.test(oneShotTx) ||
    !Number.isSafeInteger(oneShotIndex) ||
    oneShotIndex < 0
  )
    return missing(
      "HUB_ORACLE_ONE_SHOT_TX_HASH and HUB_ORACLE_ONE_SHOT_OUTPUT_INDEX are not set",
    );
  let projection: EventProjectionConfig;
  try {
    projection = eventProjectionConfigFromContracts(
      SDK.requireEventHistoryContracts(input.contracts),
      config.NETWORK === "Mainnet" ? 1 : 0,
    );
  } catch (error) {
    return missing(
      `the deployment's event lists are missing: ${error instanceof Error ? error.message : String(error)}`,
    );
  }
  const user = encodeURIComponent(config.POSTGRES_USER);
  const password = encodeURIComponent(config.POSTGRES_PASSWORD);
  const database = encodeURIComponent(config.POSTGRES_DB);
  return {
    kind: "run",
    connectionString: `postgres://${user}:${password}@${config.POSTGRES_HOST}:${config.POSTGRES_PORT.toString()}/${database}`,
    socketPath: config.L1_NATIVE_LEDGER.socketPath,
    binaryPath: config.L1_NATIVE_LEDGER.binaryPath,
    nodeConfigPath: config.L1_NATIVE_LEDGER.nodeConfigPath,
    origin: {
      origin: {
        slot: config.L1_ORIGIN.slot,
        hash: Buffer.from(config.L1_ORIGIN.blockHash, "hex"),
      },
      hubOracleOneShot: {
        txHash: Buffer.from(oneShotTx, "hex"),
        index: oneShotIndex,
      },
    },
    securityParameter: input.securityParameter,
    projection,
    stateQueue: stateQueueProjectionConfig(input.contracts.stateQueue),
    hubOraclePolicyId: input.contracts.hubOracle.policyId.toLowerCase(),
  };
};
