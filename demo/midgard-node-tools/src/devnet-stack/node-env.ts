import { existsSync } from "node:fs";
import { join } from "node:path";

import { formatL1Origin, type L1Origin } from "@al-ft/midgard-core/l1-origin";

import {
  assertNodeFollowerEnvironment,
  L1OriginUndeterminedError,
} from "../l1-origin.js";
import type { Identities } from "./identities.js";
import { type Layout, type RunEnv, servicePorts } from "./layout.js";

/**
 * Explicit parameters of a fresh deployment that the node refuses to default.
 * Event-history bounds and protection are the values the devnet journeys run
 * with; availability-challenge values are the operator example's.
 */
const DEPLOYMENT_PARAMETERS = {
  MIDGARD_EVENT_HISTORY_INLINE_LIMIT_BYTES: "512",
  MIDGARD_EVENT_HISTORY_MAX_PAYLOAD_BYTES: "5000",
  MIDGARD_EVENT_HISTORY_MAX_PAYLOAD_NODES: "512",
  MIDGARD_EVENT_HISTORY_PROTECTION_DURATION_MS: "2000",
  MIDGARD_DA_AVAILABILITY_CHUNK_BYTE_LENGTH: "14020",
  MIDGARD_DA_AVAILABILITY_TRANCHE_BYTE_LENGTH: "4194304",
  MIDGARD_DA_AVAILABILITY_MAX_TRANCHE_COUNT: "16",
  MIDGARD_DA_AVAILABILITY_CHALLENGER_BOND_LOVELACE: "10000000000",
  MIDGARD_DA_AVAILABILITY_MAX_OPEN_FEE_LOVELACE: "500000",
  MIDGARD_DA_AVAILABILITY_MAX_PUBLICATION_FEE_LOVELACE: "500000",
  MIDGARD_DA_AVAILABILITY_MAX_SETTLEMENT_FEE_LOVELACE: "500000",
  MIDGARD_DA_AVAILABILITY_MAX_CLOSE_FEE_LOVELACE: "1000000",
  MIDGARD_DA_AVAILABILITY_MAX_TIMEOUT_FEE_LOVELACE: "1200000",
} as const;

export const DA_THRESHOLD = 2;

export type Artifacts = {
  readonly nativeOwnerBinary: string;
  readonly nativeOwnerSha256: string;
  readonly transportBinary: string;
};

export type HubOracleOneShot = {
  readonly txHash: string;
  readonly outputIndex: number;
};

/**
 * The node's complete environment. It is built from the run's own records
 * only; nothing is inherited from the calling shell or a checkout `.env`.
 * `listen` runs the node's L1 follower, so it refuses an environment without
 * the hub-oracle one-shot and the run's recorded L1 origin.
 */
export const nodeEnvironment = (input: {
  readonly layout: Layout;
  readonly run: RunEnv;
  readonly identities: Identities;
  readonly artifacts: Artifacts;
  readonly oneShot?: HubOracleOneShot;
  /** The run's recorded L1 origin (deployment-origin.ts); `listen` needs it. */
  readonly l1Origin?: L1Origin;
  /** `listen` reads the producer DA manifest; commands read the contract one. */
  readonly role: "command" | "listen";
}): Record<string, string> => {
  const { layout, run, identities, artifacts, oneShot, l1Origin, role } = input;
  if (role === "listen" && (oneShot === undefined || l1Origin === undefined))
    throw new L1OriginUndeterminedError(
      "listen needs the run's hub-oracle nonce and its recorded L1 origin; run up first",
    );
  const ports = servicePorts(run);
  const contractManifest = existsSync(layout.contractManifest)
    ? layout.contractManifest
    : undefined;
  const deploymentManifest =
    role === "listen" ? layout.producerManifest : contractManifest;
  const env: Record<string, string> = {
    MIDGARD_DOTENV_MODE: "disabled",
    NETWORK: "Custom",
    MIDGARD_DEPLOYMENT_PROFILE: "local-devnet-testing",
    MIDGARD_REAL_BLUEPRINT_PATH: layout.blueprint,
    // Commands without a --run-state flag read it from here; the default is
    // relative to the node checkout, not this run.
    MIDGARD_RUN_STATE_PATH: layout.deploymentRunState,
    // The node reads and submits through the cardano-node socket.
    L1_NODE_SOCKET_PATH: layout.cardanoSocket,
    L1_NODE_CONFIG_PATH: layout.hostCardanoConfig,
    L1_NODE_TRANSPORT_BINARY_PATH: artifacts.transportBinary,
    L1_OPERATOR_SEED_PHRASE: identities.seeds.operator,
    L1_OPERATOR_SEED_PHRASE_FOR_MERGE_TX: identities.seeds.merge,
    L1_SETTLEMENT_SEED_PHRASE: identities.seeds.settlement,
    L1_REFERENCE_SCRIPT_SEED_PHRASE: identities.seeds.referenceScript,
    DA_COSIGNER_SEED_PHRASE: identities.seeds.daCosigner,
    DA_THRESHOLD: String(DA_THRESHOLD),
    DA_LIBP2P_PRIVATE_KEY_SOURCE: `seed:${identities.libp2p.producer}`,
    ...DEPLOYMENT_PARAMETERS,
    POSTGRES_HOST: "127.0.0.1",
    POSTGRES_PORT: String(run.postgresPort),
    POSTGRES_USER: run.postgresUser,
    POSTGRES_PASSWORD: run.postgresPassword,
    POSTGRES_DB: run.postgresDatabase,
    LEDGER_MPF_DB_PATH: join(layout.nodeData, "ledger-mpf-db"),
    TRANSACTIONS_MPF_DB_PATH: join(layout.nodeData, "transactions-mpf-db"),
    MPF_NATIVE_OWNER_BINARY_PATH: artifacts.nativeOwnerBinary,
    MPF_NATIVE_OWNER_BINARY_SHA256: artifacts.nativeOwnerSha256,
    MPF_NATIVE_OWNER_SIDECAR_PATH: join(
      layout.nodeData,
      "ledger-mpf-architecture-g.sidecar",
    ),
    PORT: String(ports.nodeHttp),
    MIDGARD_NODE_URL: `http://127.0.0.1:${ports.nodeHttp}`,
    PROM_METRICS_PORT: String(ports.nodeMetrics),
    ADMIN_API_KEY: identities.adminApiKey,
    RUN_GENESIS_ON_STARTUP: "false",
    // Small journeys never queue eight blocks; the default would park merges.
    MIN_QUEUE_LENGTH_FOR_MERGING: "1",
    ...(oneShot === undefined
      ? {}
      : {
          HUB_ORACLE_ONE_SHOT_TX_HASH: oneShot.txHash,
          HUB_ORACLE_ONE_SHOT_OUTPUT_INDEX: String(oneShot.outputIndex),
        }),
    ...(l1Origin === undefined ? {} : { L1_ORIGIN: formatL1Origin(l1Origin) }),
    ...(contractManifest === undefined
      ? {}
      : { MIDGARD_CONTRACT_DEPLOYMENT_INFO_PATH: contractManifest }),
    ...(deploymentManifest === undefined
      ? {}
      : { MIDGARD_DEPLOYMENT_MANIFEST_PATH: deploymentManifest }),
  };
  if (role === "listen") assertNodeFollowerEnvironment(env);
  return env;
};
