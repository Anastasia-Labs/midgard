import { isAbsolute, normalize } from "node:path";

import {
  type NativeLedgerNetwork,
  readNativeLedgerGenesis,
} from "@al-ft/midgard-core/native-reward-account";

/**
 * The local node the node's L1 reads and submissions go through: its socket,
 * its config (for the network magic) and the transport sidecar binary.
 */
export type NativeLedgerSettings = Readonly<{
  socketPath: string;
  nodeConfigPath: string;
  binaryPath: string;
}>;

export const NATIVE_LEDGER_SETTING_NAMES = [
  "L1_NODE_SOCKET_PATH",
  "L1_NODE_CONFIG_PATH",
  "L1_NODE_TRANSPORT_BINARY_PATH",
] as const;

/** All three settings, or none; each an absolute canonical path. */
export const parseNativeLedgerSettings = (
  values: Readonly<
    Record<(typeof NATIVE_LEDGER_SETTING_NAMES)[number], string | undefined>
  >,
): NativeLedgerSettings | undefined => {
  const present = NATIVE_LEDGER_SETTING_NAMES.filter(
    (name) => (values[name]?.trim() ?? "") !== "",
  );
  if (present.length === 0) return undefined;
  if (present.length !== NATIVE_LEDGER_SETTING_NAMES.length)
    throw new Error(
      `${NATIVE_LEDGER_SETTING_NAMES.join(", ")} must be set together; missing ${NATIVE_LEDGER_SETTING_NAMES.filter((name) => !present.includes(name)).join(", ")}`,
    );
  const path = (name: (typeof NATIVE_LEDGER_SETTING_NAMES)[number]) => {
    const value = values[name]!.trim();
    if (!isAbsolute(value) || normalize(value) !== value)
      throw new Error(`${name} must be an absolute canonical path`);
    return value;
  };
  return {
    socketPath: path("L1_NODE_SOCKET_PATH"),
    nodeConfigPath: path("L1_NODE_CONFIG_PATH"),
    binaryPath: path("L1_NODE_TRANSPORT_BINARY_PATH"),
  };
};

export const nativeLedgerSettingsFromEnv = (
  env: NodeJS.ProcessEnv,
): NativeLedgerSettings | undefined =>
  parseNativeLedgerSettings({
    L1_NODE_SOCKET_PATH: env.L1_NODE_SOCKET_PATH,
    L1_NODE_CONFIG_PATH: env.L1_NODE_CONFIG_PATH,
    L1_NODE_TRANSPORT_BINARY_PATH: env.L1_NODE_TRANSPORT_BINARY_PATH,
  });

/**
 * The local node's network magic, from its config's Shelley genesis, checked
 * against the configured network exactly as the reward-account reads check
 * it. The L1 follower's chain-sync session handshakes with it. Only the static
 * config files are read: the node's socket need not exist yet (the transport
 * waits for it as `node_unreachable`).
 */
export const nativeLedgerNetworkMagic = async (
  settings: Pick<NativeLedgerSettings, "nodeConfigPath">,
  network: NativeLedgerNetwork,
): Promise<number> =>
  (
    await readNativeLedgerGenesis({
      nodeConfigPath: settings.nodeConfigPath,
      network,
    })
  ).networkMagic;
