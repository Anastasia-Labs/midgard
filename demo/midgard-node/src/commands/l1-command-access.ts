/**
 * A node command's L1 access, opened from the environment the node runs
 * with: the local node (`L1_NODE_SOCKET_PATH`, `L1_NODE_CONFIG_PATH`,
 * `L1_NODE_TRANSPORT_BINARY_PATH`) and the node database (`POSTGRES_*`, with
 * the node's defaults), whose follower store answers every tracked read. An
 * untracked address reads the local node's ledger at its tip. The same
 * `L1FollowerProvider` the running node reads through (`services/l1-provider.ts`).
 */
import type { NativeLedgerNetwork } from "@al-ft/midgard-core/native-reward-account";
import { DEFAULT_NODE_BEHIND_MS } from "@al-ft/midgard-l1-follower";
import * as LE from "@lucid-evolution/lucid";

import {
  NODE_L1_ACCESS_UNCONFIGURED,
  nodeDatabaseConnectionString,
  type NodeL1Access,
  openNodeL1Access,
} from "../services/l1-provider.js";
import { nativeLedgerSettingsFromEnv } from "../services/native-ledger.js";

const positiveInteger = (
  env: NodeJS.ProcessEnv,
  name: string,
  fallback: number,
): number => {
  const raw = env[name]?.trim();
  if (raw === undefined || raw === "") return fallback;
  const value = Number(raw);
  if (!Number.isSafeInteger(value) || value <= 0)
    throw new Error(`${name} must be a positive safe integer`);
  return value;
};

/** Opens the command's L1 access; the caller closes it. */
export const openCommandL1Access = async (input: {
  readonly network: NativeLedgerNetwork;
  readonly env?: NodeJS.ProcessEnv;
}): Promise<NodeL1Access> => {
  const env = input.env ?? process.env;
  const nativeLedger = nativeLedgerSettingsFromEnv(env);
  if (nativeLedger === undefined)
    throw new Error(
      `The L1 read needs the local node: ${NODE_L1_ACCESS_UNCONFIGURED}`,
    );
  return openNodeL1Access({
    nativeLedger,
    network: input.network,
    connectionString: nodeDatabaseConnectionString({
      POSTGRES_HOST: env.POSTGRES_HOST?.trim() || "postgres",
      POSTGRES_PORT: positiveInteger(env, "POSTGRES_PORT", 5432),
      POSTGRES_USER: env.POSTGRES_USER?.trim() || "postgres",
      POSTGRES_PASSWORD: env.POSTGRES_PASSWORD ?? "postgres",
      POSTGRES_DB: env.POSTGRES_DB?.trim() || "midgard",
    }),
    // The command adopts the protocol set the node's follower recorded.
    wallets: [],
    nodeBehindMaxMs: positiveInteger(
      env,
      "L1_NODE_BEHIND_MAX_MS",
      DEFAULT_NODE_BEHIND_MS,
    ),
  });
};

/** Lucid over the command's access, on the ledger's slot mapping. */
export const commandLucid = async (
  access: NodeL1Access,
  network: LE.Network,
  options: Omit<LE.LucidOptions, "slotConfig"> = {},
): Promise<LE.LucidEvolution> =>
  LE.Lucid(access.provider, network, {
    ...options,
    slotConfig: await access.slotConfig(),
  });

/** Runs `use` with a command access, closing it afterwards. */
export const withCommandL1Access = async <T>(
  input: Parameters<typeof openCommandL1Access>[0],
  use: (access: NodeL1Access) => Promise<T>,
): Promise<T> => {
  const access = await openCommandL1Access(input);
  try {
    return await use(access);
  } finally {
    await access.close();
  }
};
