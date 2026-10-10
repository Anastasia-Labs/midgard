/**
 * A tool's L1 access (option E): every CLI command and tool reads L1 through
 * one of the tool adapters of the port (`../l1-access.ts`), chosen by
 * `--l1 node|kupmios|blockfrost` (or `L1_ACCESS`):
 *
 * - `node` (the default when `L1_NODE_SOCKET_PATH` is set): the local
 *   node's ledger over local state query (`services/l1-node-ledger-access.ts`),
 *   with no store, so a tool needs no follower schema and never takes a
 *   role's writer lease.
 * - `kupmios` (`L1_KUPO_URL`, `L1_OGMIOS_URL`) and `blockfrost`
 *   (`L1_BLOCKFROST_URL`, `L1_BLOCKFROST_PROJECT_ID`): an external provider
 *   (`../l1-external/`), loaded only by a dynamic import from here, so no
 *   role process ever has its client in its module graph.
 *
 * With nothing selected and no local node configured a tool refuses, naming
 * the options. `follower` is a role's access and refused here.
 */
import { DEFAULT_NODE_BEHIND_MS } from "@al-ft/midgard-l1-follower";
import type * as LE from "@lucid-evolution/lucid";

import { TOOL_L1_ACCESS_KINDS } from "../l1-access.js";
import type { BlockfrostAccess } from "../l1-external/blockfrost-access.js";
import type { KupmiosAccess } from "../l1-external/kupmios-access.js";
import {
  type NodeLedgerAccess,
  openNodeLedgerAccess,
} from "../services/l1-node-ledger-access.js";
import { NODE_L1_ACCESS_UNCONFIGURED } from "../services/l1-provider.js";
import {
  type NativeLedgerSettings,
  nativeLedgerSettingsFromEnv,
} from "../services/native-ledger.js";

/** A tool's L1 access: one of the three tool adapters. */
export type ToolL1Access = NodeLedgerAccess | KupmiosAccess | BlockfrostAccess;
export type ToolL1AccessKind = ToolL1Access["kind"];

const TOOL_OPTIONS = `--l1 ${TOOL_L1_ACCESS_KINDS.join("|")} (or L1_ACCESS)`;

/** A tool was started with no usable L1 access selection. */
export class ToolL1AccessRefusedError extends Error {
  override readonly name = "ToolL1AccessRefusedError";
  constructor(
    readonly reason:
      | "tool_l1_access_unselected"
      | "tool_l1_access_unknown"
      | "tool_l1_access_follower"
      | "tool_l1_access_incomplete",
    message: string,
  ) {
    super(message);
  }
}

const setting = (env: NodeJS.ProcessEnv, name: string): string | undefined => {
  const value = env[name]?.trim();
  return value === undefined || value === "" ? undefined : value;
};

/**
 * The tool adapter `env` selects: `L1_ACCESS` when set (`--l1` sets it), else
 * `node` when a local node socket is configured, else a refusal naming the
 * options.
 */
export const selectToolL1Access = (
  env: NodeJS.ProcessEnv,
): ToolL1AccessKind => {
  const selected = setting(env, "L1_ACCESS");
  if (selected === undefined) {
    if (setting(env, "L1_NODE_SOCKET_PATH") !== undefined) return "node";
    throw new ToolL1AccessRefusedError(
      "tool_l1_access_unselected",
      `No L1 access selected: pass ${TOOL_OPTIONS}; node is the default when L1_NODE_SOCKET_PATH is set`,
    );
  }
  if ((TOOL_L1_ACCESS_KINDS as readonly string[]).includes(selected))
    return selected as ToolL1AccessKind;
  if (selected === "follower")
    throw new ToolL1AccessRefusedError(
      "tool_l1_access_follower",
      `L1 access "follower" is a role's own access; a tool reads L1 through ${TOOL_OPTIONS}`,
    );
  throw new ToolL1AccessRefusedError(
    "tool_l1_access_unknown",
    `Unknown L1 access ${JSON.stringify(selected)}: pass ${TOOL_OPTIONS}`,
  );
};

const required = (
  env: NodeJS.ProcessEnv,
  kind: ToolL1AccessKind,
  names: readonly [string, string],
): [string, string] => {
  const missing = names.filter((name) => setting(env, name) === undefined);
  if (missing.length > 0)
    throw new ToolL1AccessRefusedError(
      "tool_l1_access_incomplete",
      `--l1 ${kind} needs ${missing.join(" and ")}`,
    );
  return [setting(env, names[0])!, setting(env, names[1])!];
};

const positiveInteger = (
  env: NodeJS.ProcessEnv,
  name: string,
  fallback: number,
): number => {
  const raw = setting(env, name);
  if (raw === undefined) return fallback;
  const value = Number(raw);
  if (!Number.isSafeInteger(value) || value <= 0)
    throw new Error(`${name} must be a positive safe integer`);
  return value;
};

export type OpenToolL1AccessInput = Readonly<{
  network: LE.Network;
  env?: NodeJS.ProcessEnv;
  /** The local node, when the caller already parsed it (the node config). */
  nativeLedger?: NativeLedgerSettings;
  /** The ledger-tip staleness bound for node-ledger submit slots (ms). */
  nodeBehindMaxMs?: number;
}>;

/** Opens the tool adapter `env` selects; the caller closes it. */
export const openToolL1Access = async (
  input: OpenToolL1AccessInput,
): Promise<ToolL1Access> => {
  const env = input.env ?? process.env;
  const kind = selectToolL1Access(env);
  switch (kind) {
    case "node": {
      const nativeLedger =
        input.nativeLedger ?? nativeLedgerSettingsFromEnv(env);
      if (nativeLedger === undefined)
        throw new ToolL1AccessRefusedError(
          "tool_l1_access_incomplete",
          `--l1 node needs the local node: ${NODE_L1_ACCESS_UNCONFIGURED}`,
        );
      return await openNodeLedgerAccess({
        nativeLedger,
        network: input.network,
        nodeBehindMaxMs:
          input.nodeBehindMaxMs ??
          positiveInteger(env, "L1_NODE_BEHIND_MAX_MS", DEFAULT_NODE_BEHIND_MS),
      });
    }
    case "kupmios": {
      const [kupoUrl, ogmiosUrl] = required(env, kind, [
        "L1_KUPO_URL",
        "L1_OGMIOS_URL",
      ]);
      const { openKupmiosAccess } = await import(
        "../l1-external/kupmios-access.js"
      );
      return openKupmiosAccess({ network: input.network, kupoUrl, ogmiosUrl });
    }
    case "blockfrost": {
      const network = input.network;
      if (network === "Custom")
        throw new ToolL1AccessRefusedError(
          "tool_l1_access_incomplete",
          "--l1 blockfrost serves named networks only; a Custom network needs --l1 node or --l1 kupmios",
        );
      const [url, projectId] = required(env, kind, [
        "L1_BLOCKFROST_URL",
        "L1_BLOCKFROST_PROJECT_ID",
      ]);
      const { openBlockfrostAccess } = await import(
        "../l1-external/blockfrost-access.js"
      );
      return openBlockfrostAccess({ network, url, projectId });
    }
  }
};

/** Lucid over a tool access, on the adapter's slot mapping. */
export const commandLucid = async (
  access: ToolL1Access,
  network: LE.Network,
  options: Omit<LE.LucidOptions, "slotConfig"> = {},
): Promise<LE.LucidEvolution> => await access.lucid(network, options);

/** Runs `use` with a tool access, closing it afterwards. */
export const withCommandL1Access = async <T>(
  input: OpenToolL1AccessInput,
  use: (access: ToolL1Access) => Promise<T>,
): Promise<T> => {
  const access = await openToolL1Access(input);
  try {
    return await use(access);
  } finally {
    await access.close();
  }
};
