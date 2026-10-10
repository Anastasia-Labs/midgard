/**
 * The node-ledger adapter of the L1-access port (`l1-access.ts`; option E),
 * the tools' default when a local node is configured: a `LedgerProvider`
 * over the local node's transport alone. UTxOs by address and outref over
 * local state query at one acquired point per build step, protocol
 * parameters and era history from the ledger, submission through
 * LocalTxSubmission, confirmation from the tx's outputs in the ledger. It
 * opens no store, so it needs no follower schema and never takes a role's
 * writer lease.
 *
 * - Tip (the clock): the ledger's chain point.
 * - View point: the build step's acquired point; the synchronized view
 *   point starts a new step at the tip, so reads after it answer at exactly
 *   that point.
 * - Submit slot: the ledger tip against wall time
 *   (`ledgerSubmitSlotSnapshot`).
 */
import {
  type L1NodeTransport,
  sharedL1NodeTransport,
  type TransportReadiness,
} from "@al-ft/l1-node-transport";
import type { NativeLedgerNetwork } from "@al-ft/midgard-core/native-reward-account";
import {
  fromTransportError,
  type LedgerPointStatus,
  LedgerProvider,
  type LedgerTip,
} from "@al-ft/midgard-l1-follower/provider";
import type { SlotConfig } from "@lucid-evolution/lucid";

import {
  type L1Access,
  type L1AccessAdapter,
  type L1ViewPoint,
  openL1Access,
} from "../l1-access.js";
import { ledgerSubmitSlotSnapshot } from "./l1-ledger-tip.js";
import {
  nativeLedgerNetworkMagic,
  type NativeLedgerSettings,
} from "./native-ledger.js";

/** The node-ledger adapter: the provider, its transport and the tip reads. */
export type NodeLedgerAccessAdapter = L1AccessAdapter &
  Readonly<{
    kind: "node";
    provider: LedgerProvider;
    transport: L1NodeTransport;
    /** The ledger's tip now: point and block number from one acquired state. */
    readTip: () => Promise<LedgerTip>;
    /** The ledger's tip now, as a view point. */
    ledgerTip: () => Promise<L1ViewPoint>;
    /** Where a point stands on the node's chain. */
    pointStatus: (
      point: Readonly<{ slot: number; hash: string }>,
    ) => Promise<LedgerPointStatus>;
    protocolParametersCbor: () => Promise<Uint8Array>;
    transportReadiness: () => TransportReadiness;
  }>;

export type NodeLedgerAccess = L1Access<NodeLedgerAccessAdapter>;

export type OpenNodeLedgerAccessInput = Readonly<{
  nativeLedger: NativeLedgerSettings;
  network: NativeLedgerNetwork;
  /** The ledger-tip staleness bound for submit-slot snapshots (ms). */
  nodeBehindMaxMs: number;
  /** Bound on one transport request (ms); the transport's default when absent. */
  requestTimeoutMs?: number;
  /** How long one build step reads at one acquired point (ms). */
  pinPointMs?: number;
  /** How long `awaitTx` waits for a tx's outputs (ms). */
  awaitTxTimeoutMs?: number;
  nowMs?: () => number;
}>;

const viewOf = (point: Readonly<{ slot: number; hash: string }>) => ({
  slot: point.slot,
  id: point.hash,
});

/** The node-ledger adapter over `transport`; the port is opened over it. */
export const nodeLedgerAccessOver = (
  transport: L1NodeTransport,
  input: Omit<OpenNodeLedgerAccessInput, "nativeLedger" | "network"> &
    Readonly<{ endpoint: string }>,
): NodeLedgerAccess => {
  const provider = new LedgerProvider({
    transport,
    ...(input.pinPointMs === undefined ? {} : { pinPointMs: input.pinPointMs }),
    ...(input.awaitTxTimeoutMs === undefined
      ? {}
      : { awaitTxTimeoutMs: input.awaitTxTimeoutMs }),
  });
  const nowMs = input.nowMs ?? Date.now;
  let slotConfig: Promise<SlotConfig> | undefined;
  const readSlotConfig = (): Promise<SlotConfig> => {
    slotConfig ??= provider.slotConfig().catch((error: unknown) => {
      slotConfig = undefined;
      throw error;
    });
    return slotConfig;
  };
  const readTip = () => provider.readTip();
  return openL1Access({
    kind: "node",
    provider,
    transport,
    endpoint: input.endpoint,
    slotConfig: readSlotConfig,
    tipSlot: async () => (await readTip()).slot,
    readTip,
    ledgerTip: async () => viewOf(await readTip()),
    pointStatus: (point) => provider.pointStatus(point),
    viewPoint: async () => viewOf(await provider.pinnedTip()),
    synchronizedViewPoint: async () => viewOf(await provider.repin()),
    submitSlotSnapshot: async () => {
      const config = await readSlotConfig();
      const tip = await readTip();
      return ledgerSubmitSlotSnapshot({
        slotConfig: config,
        ledgerTipSlot: tip.slot,
        nowMs: nowMs(),
        boundMs: input.nodeBehindMaxMs,
      });
    },
    protocolParametersCbor: async () => {
      try {
        return await transport.query({ query: "protocol_params" });
      } catch (error) {
        throw fromTransportError(error);
      }
    },
    transportReadiness: () => transport.readiness,
    // The shared transport idles unreferenced; there is nothing else to close.
    close: async () => undefined,
  });
};

/**
 * Opens the node-ledger adapter. Reads only the node's config files (for the
 * network magic) before returning: an unreachable node is a transient
 * failure of the reads, not of the open.
 */
export const openNodeLedgerAccess = async (
  input: OpenNodeLedgerAccessInput,
): Promise<NodeLedgerAccess> => {
  const networkMagic = await nativeLedgerNetworkMagic(
    input.nativeLedger,
    input.network,
  );
  const transport = sharedL1NodeTransport({
    binaryPath: input.nativeLedger.binaryPath,
    socketPath: input.nativeLedger.socketPath,
    networkMagic,
    ...(input.requestTimeoutMs === undefined
      ? {}
      : { requestTimeoutMs: input.requestTimeoutMs }),
  });
  return nodeLedgerAccessOver(transport, {
    ...input,
    endpoint: input.nativeLedger.socketPath,
  });
};

/** The settings missing from a node with no local node configured. */
export const NODE_L1_ACCESS_UNCONFIGURED =
  "no local node is configured (L1_NODE_SOCKET_PATH, L1_NODE_CONFIG_PATH, L1_NODE_TRANSPORT_BINARY_PATH)";

/**
 * Opens the node-ledger adapter from the node's configuration, for a tool
 * that runs under it (the L1 preflight, the commit-candidate probe): the
 * local node alone, never the node's follower store.
 */
export const openNodeLedgerAccessFromConfig = async (
  config: Readonly<{
    L1_NATIVE_LEDGER?: NativeLedgerSettings | undefined;
    NETWORK: NativeLedgerNetwork;
    L1_NODE_BEHIND_MAX_MS: number;
  }>,
): Promise<NodeLedgerAccess> => {
  if (config.L1_NATIVE_LEDGER === undefined)
    throw new Error(NODE_L1_ACCESS_UNCONFIGURED);
  return await openNodeLedgerAccess({
    nativeLedger: config.L1_NATIVE_LEDGER,
    network: config.NETWORK,
    nodeBehindMaxMs: config.L1_NODE_BEHIND_MAX_MS,
  });
};
