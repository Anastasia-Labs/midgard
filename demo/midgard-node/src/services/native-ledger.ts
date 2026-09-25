import { isAbsolute, normalize } from "node:path";

import {
  nativeLedgerAuthoritySource,
  NativeLedgerKupmios,
  type NativeLedgerNetwork,
} from "@al-ft/midgard-core/native-reward-account";

/**
 * The local node whose ledger answers reward-account reads. Ogmios cannot:
 * it omits registered accounts without a stake-pool delegation, which is
 * every Midgard script reward account.
 */
export type NativeLedgerSettings = Readonly<{
  socketPath: string;
  nodeConfigPath: string;
  binaryPath: string;
}>;

export const NATIVE_LEDGER_SETTING_NAMES = [
  "L1_NODE_SOCKET_PATH",
  "L1_NODE_CONFIG_PATH",
  "L1_NATIVE_CHAIN_SYNC_BINARY_PATH",
] as const;

const NATIVE_LEDGER_AUTHORITY_ID = "local-cardano-node";
const NATIVE_LEDGER_QUERY_TIMEOUT_MS = 30_000;

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
    binaryPath: path("L1_NATIVE_CHAIN_SYNC_BINARY_PATH"),
  };
};

export const nativeLedgerSettingsFromEnv = (
  env: NodeJS.ProcessEnv,
): NativeLedgerSettings | undefined =>
  parseNativeLedgerSettings({
    L1_NODE_SOCKET_PATH: env.L1_NODE_SOCKET_PATH,
    L1_NODE_CONFIG_PATH: env.L1_NODE_CONFIG_PATH,
    L1_NATIVE_CHAIN_SYNC_BINARY_PATH: env.L1_NATIVE_CHAIN_SYNC_BINARY_PATH,
  });

/** Kupmios transport whose reward-account reads come from the local ledger. */
export const makeNodeKupmios = (input: {
  readonly kupoUrl: string;
  readonly ogmiosUrl: string;
  readonly network: NativeLedgerNetwork;
  readonly nativeLedger: NativeLedgerSettings | undefined;
}): NativeLedgerKupmios =>
  new NativeLedgerKupmios(
    input.kupoUrl,
    input.ogmiosUrl,
    nativeLedgerAuthoritySource(
      input.nativeLedger === undefined
        ? undefined
        : {
            ...input.nativeLedger,
            authorityNodeId: NATIVE_LEDGER_AUTHORITY_ID,
            network: input.network,
            timeoutMs: NATIVE_LEDGER_QUERY_TIMEOUT_MS,
          },
    ),
  );
