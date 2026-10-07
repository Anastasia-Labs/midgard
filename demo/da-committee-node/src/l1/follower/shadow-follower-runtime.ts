import { L1NodeTransport } from "@al-ft/l1-node-transport";
import {
  openPostgresFactStore,
  type OriginConfig,
} from "@al-ft/midgard-l1-follower";
import { projectionStoreOptions } from "@al-ft/midgard-l1-follower/shadow";
import { getAddressDetails } from "@lucid-evolution/lucid";

import type { LoadedCommitteeConfig } from "../../config.js";
import { committeeProjection } from "./projection.js";
import { runShadowFollower } from "./shadow-follower.js";

/**
 * Phase A only (deleted at the C1 cutover, when the follower becomes the
 * committee's L1 source): the shadow follower runs only when the operator
 * names its status file. Without it the committee runs exactly as before.
 */
export const SHADOW_STATUS_ENV = "MIDGARD_L1_FOLLOWER_SHADOW_STATUS";

export type ShadowFollowerHandle = Readonly<{
  /** Settles once the follower stopped and released its store and transport. */
  stop: () => Promise<void>;
}>;

export type ShadowFollowerPlan =
  | Readonly<{ kind: "off" }>
  | Readonly<{ kind: "unavailable"; reason: string }>
  | Readonly<{
      kind: "run";
      statusPath: string;
      databaseUrl: string;
      socketPath: string;
      binaryPath: string;
      networkMagic: number;
      origin: OriginConfig;
    }>;

const HEX_32 = /^[0-9a-f]{64}$/u;

const hubOracleOneShot = (
  info: Record<string, unknown>,
): OriginConfig["hubOracleOneShot"] | null => {
  const entry = info.hubOracleOneShot;
  if (typeof entry !== "object" || entry === null) return null;
  const { txHash, outputIndex } = entry as Record<string, unknown>;
  if (
    typeof txHash !== "string" ||
    !HEX_32.test(txHash) ||
    typeof outputIndex !== "number" ||
    !Number.isSafeInteger(outputIndex) ||
    outputIndex < 0
  )
    return null;
  return { txHash: Buffer.from(txHash, "hex"), index: outputIndex };
};

/**
 * Whether and how the shadow follower runs. It needs the configured origin
 * (`L1_ORIGIN`), the local node (the native ledger's socket and transport
 * binary), the Postgres local state (the follower's tables are Postgres
 * only) and the manifest's `hubOracleOneShot`. Anything missing leaves it
 * off with a reason; it never fails the committee's startup.
 */
export const shadowFollowerPlan = (
  config: LoadedCommitteeConfig,
  env: Readonly<Record<string, string | undefined>>,
): ShadowFollowerPlan => {
  const statusPath = env[SHADOW_STATUS_ENV];
  if (statusPath === undefined || statusPath === "") return { kind: "off" };
  const missing = (reason: string): ShadowFollowerPlan => ({
    kind: "unavailable",
    reason,
  });
  if (config.l1Origin === undefined) return missing("L1_ORIGIN is not set");
  if (config.nativeLedger === undefined)
    return missing("no local node ledger is configured");
  if (config.localState.kind !== "database")
    return missing("the follower's tables need the Postgres local state");
  const oneShot = hubOracleOneShot(config.contractDeploymentInfo);
  if (oneShot === null)
    return missing("the deployment info has no hubOracleOneShot outref");
  return {
    kind: "run",
    statusPath,
    databaseUrl: config.localState.url,
    socketPath: config.nativeLedger.socketPath,
    binaryPath: config.nativeLedger.binaryPath,
    networkMagic: config.cardanoL1Source.networkMagic,
    origin: {
      origin: {
        slot: config.l1Origin.slot,
        hash: Buffer.from(config.l1Origin.blockHash, "hex"),
      },
      hubOracleOneShot: oneShot,
    },
  };
};

const openShadow = (
  config: LoadedCommitteeConfig,
  plan: Extract<ShadowFollowerPlan, { kind: "run" }>,
  log: (line: string) => void,
) => {
  const projection = committeeProjection({
    stateQueueAddress: Buffer.from(
      getAddressDetails(config.stateQueueAddress).address.hex,
      "hex",
    ),
    stateQueuePolicyId: config.stateQueuePolicyId.toLowerCase(),
  });
  const transport = new L1NodeTransport({
    binaryPath: plan.binaryPath,
    socketPath: plan.socketPath,
    networkMagic: plan.networkMagic,
    onDiagnostic: (line) => log(`L1 follower shadow transport: ${line}`),
  });
  let store: ReturnType<typeof openPostgresFactStore>;
  try {
    store = openPostgresFactStore({
      ...projectionStoreOptions(
        [projection],
        {
          // k: the manifest's automaticRecoveryMaxDepth (plan §1, terms).
          securityParameter: config.automaticRecoveryMaxDepth,
          // The protocol-init tx qualifies through the hub oracle mint, so
          // `protocolInitStatus` can see the hubOracleOneShot spend.
          trackedSet: {
            addresses: new Set(),
            paymentCredentials: new Set(),
            policies: new Set([config.hubOraclePolicyId.toLowerCase()]),
          },
        },
        "postgres",
      ),
      connection: { connectionString: plan.databaseUrl, maxConnections: 4 },
    });
  } catch (error) {
    void transport.close();
    throw error;
  }
  const abort = new AbortController();
  const running = runShadowFollower({
    store,
    transport,
    origin: plan.origin,
    statusPath: plan.statusPath,
    signal: abort.signal,
    log: (line) => log(`L1 follower shadow: ${line}`),
  });
  return { store, transport, running, abort };
};

/**
 * Starts the committee's shadow follower in the background (phase A): its
 * own transport session and its own tables in the committee's Postgres
 * database, the committee projections current, and its interventions in
 * the status file. Nothing the committee does reads it yet.
 */
export const startShadowFollower = (
  config: LoadedCommitteeConfig,
  env: Readonly<Record<string, string | undefined>>,
  log: (line: string) => void,
): ShadowFollowerHandle | undefined => {
  const plan = shadowFollowerPlan(config, env);
  if (plan.kind === "off") return undefined;
  if (plan.kind === "unavailable") {
    log(`L1 follower shadow is off: ${plan.reason}`);
    return undefined;
  }
  let opened: ReturnType<typeof openShadow>;
  try {
    opened = openShadow(config, plan, log);
  } catch (error) {
    log(
      `L1 follower shadow is off: ${error instanceof Error ? error.message : String(error)}`,
    );
    return undefined;
  }
  const { store, transport, running, abort } = opened;
  return {
    stop: async () => {
      abort.abort();
      await running.catch(() => undefined);
      await store.close().catch(() => undefined);
      await transport.close().catch(() => undefined);
    },
  };
};
