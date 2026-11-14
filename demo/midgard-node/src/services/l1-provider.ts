/**
 * The follower adapter of the L1-access port (`l1-access.ts`; plan §4.4,
 * N1), for role processes only (`listen` and its workers): one
 * `L1FollowerProvider` over the node's follower store (its tables in the
 * node database, read only here) and the local node's transport (local
 * state query, LocalTxSubmission, LocalTxMonitor). Nothing reads Kupo or
 * Ogmios. Tools use the node-ledger adapter (`l1-node-ledger-access.ts`)
 * or an external one, never this.
 *
 * - The store is opened as a reader: it is never started, so it takes no
 *   writer lease and runs no migration. The follower in the main thread is
 *   its only writer.
 * - It tracks the node's index (`nodeIndexSpec`) from the open: the
 *   protocol set the follower records and the own wallets. A read of
 *   either before the follower's first cursor is the transient
 *   `follower:not_initialized`, never a ledger scan.
 * - The submit-slot snapshot reads the ledger tip over local state query and
 *   takes `currentSlot` from wall time on the ledger's slot configuration. A
 *   ledger tip further behind wall time than `L1_NODE_BEHIND_MAX_MS` is the
 *   retryable `l1_node_behind`, the follower's own reason for it.
 * - Every failure is typed and transient where a retry can clear it; none
 *   ends the process.
 */
import {
  type L1NodeTransport,
  sharedL1NodeTransport,
  type TransportReadiness,
} from "@al-ft/l1-node-transport";
import {
  type FactStore,
  openPostgresFactStore,
} from "@al-ft/midgard-l1-follower";
import type { NativeLedgerNetwork } from "@al-ft/midgard-l1-follower/native-reward-account";
import {
  fromTransportError,
  L1FollowerProvider,
  L1ProviderTransientError,
} from "@al-ft/midgard-l1-follower/provider";
import type { SlotConfig } from "@lucid-evolution/lucid";

import {
  type L1Access,
  type L1AccessAdapter,
  type L1ViewPoint,
  openL1Access,
} from "../l1-access.js";
import { type NodeConfigDep } from "./config.js";
import type { NodeIndexSpec } from "./l1-follower.index-spec.js";
import {
  L1LedgerBehindError,
  ledgerSubmitSlotSnapshot,
  ledgerTipPointOf,
} from "./l1-ledger-tip.js";
import { NODE_L1_ACCESS_UNCONFIGURED } from "./l1-node-ledger-access.js";
import {
  nativeLedgerNetworkMagic,
  type NativeLedgerSettings,
} from "./native-ledger.js";

/** The follower store has no cursor yet: no view point to read at. */
const notInitialized = () =>
  new L1ProviderTransientError("follower", "not_initialized");

/**
 * The transient L1 read failure in `error` or its cause chain: a node,
 * sidecar, store or follower outage, or a ledger tip behind wall time. A
 * retry with backoff can clear it.
 */
export const transientL1ReadCause = (
  error: unknown,
): L1ProviderTransientError | L1LedgerBehindError | undefined => {
  let current: unknown = error;
  for (let depth = 0; depth < 8 && current !== undefined; depth += 1) {
    if (
      current instanceof L1ProviderTransientError ||
      current instanceof L1LedgerBehindError
    )
      return current;
    current =
      typeof current === "object" && current !== null && "cause" in current
        ? (current as { cause?: unknown }).cause
        : undefined;
  }
  return undefined;
};

/** A short failure kind for readiness: `l1_node_behind`, `transport:node_unreachable`, … */
export const l1ReadFailureKind = (error: unknown): string => {
  const transient = transientL1ReadCause(error);
  if (transient instanceof L1LedgerBehindError) return transient.reason;
  if (transient !== undefined) return `${transient.source}:${transient.reason}`;
  return "l1_read_failed";
};

export type { L1ViewPoint } from "../l1-access.js";
export {
  L1LedgerBehindError,
  ledgerSubmitSlotSnapshot,
  ledgerTipPointOf,
  wallSlotAt,
} from "./l1-ledger-tip.js";

/** The follower adapter (roles only): the store and transport around it. */
export type NodeL1AccessAdapter = L1AccessAdapter &
  Readonly<{
    kind: "follower";
    provider: L1FollowerProvider;
    store: FactStore;
    transport: L1NodeTransport;
    /** The local node's ledger tip, read now. */
    ledgerTip: () => Promise<L1ViewPoint>;
    protocolParametersCbor: () => Promise<Uint8Array>;
    transportReadiness: () => TransportReadiness;
  }>;

/**
 * The node's L1 access in a role: the follower adapter over the port. Its
 * clock is the follower's covered tip; its view point is the follower's
 * cursor; its synchronized view point is that cursor once it has reached the
 * node's ledger tip read first (so follower lag is never read as chain
 * state), the transient `follower:behind_node_tip` after
 * `VIEW_SYNC_TIMEOUT_MS`.
 */
export type NodeL1Access = L1Access<NodeL1AccessAdapter>;

const transportCall = async <T>(run: () => Promise<T>): Promise<T> => {
  try {
    return await run();
  } catch (error) {
    throw fromTransportError(error);
  }
};

/** How long a synchronized view point waits for the follower to reach the node's tip. */
export const VIEW_SYNC_TIMEOUT_MS = 60_000;
const VIEW_SYNC_POLL_MS = 500;

/** The Postgres connection string for the node database. */
export const nodeDatabaseConnectionString = (
  config: Pick<
    NodeConfigDep,
    | "POSTGRES_HOST"
    | "POSTGRES_PORT"
    | "POSTGRES_USER"
    | "POSTGRES_PASSWORD"
    | "POSTGRES_DB"
  >,
): string =>
  `postgres://${encodeURIComponent(config.POSTGRES_USER)}:${encodeURIComponent(
    config.POSTGRES_PASSWORD,
  )}@${config.POSTGRES_HOST}:${config.POSTGRES_PORT.toString()}/${encodeURIComponent(
    config.POSTGRES_DB,
  )}`;

/** What the node's L1 access is opened with, over any transport. */
export type NodeL1AccessOverInput = Readonly<{
  /** The node's index (`nodeIndexSpec`): its tracked set and own wallets. */
  index: Pick<NodeIndexSpec, "trackedSet" | "wallets">;
  connectionString: string;
  /** The local node's socket path, named in reports. */
  endpoint: string;
  /** The ledger-tip staleness bound for submit-slot snapshots (ms). */
  nodeBehindMaxMs: number;
  nowMs?: () => number;
}>;

export type OpenNodeL1AccessInput = Omit<NodeL1AccessOverInput, "endpoint"> &
  Readonly<{
    nativeLedger: NativeLedgerSettings;
    network: NativeLedgerNetwork;
    /** Bound on one transport request (ms); the transport's default when absent. */
    requestTimeoutMs?: number;
  }>;

/**
 * Opens the node's L1 access. Reads only the node's config files (for the
 * network magic) before returning: an unreachable node or database is a
 * transient failure of the reads, not of the open.
 */
export const openNodeL1Access = async (
  input: OpenNodeL1AccessInput,
): Promise<NodeL1Access> => {
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
  return nodeL1AccessOver(transport, {
    ...input,
    endpoint: input.nativeLedger.socketPath,
  });
};

/** The node's L1 access over `transport`; opening it reads nothing. */
export const nodeL1AccessOver = (
  transport: L1NodeTransport,
  input: NodeL1AccessOverInput,
): NodeL1Access => {
  const store = openPostgresFactStore({
    // Never started: the follower in the main thread is its only writer. Its
    // set is the one that follower records (`nodeIndexSpec`), so a protocol
    // read before the follower's first cursor is `follower:not_initialized`,
    // never a ledger scan.
    securityParameter: 1,
    trackedSet: input.index.trackedSet,
    wallets: input.index.wallets,
    connection: {
      connectionString: input.connectionString,
      maxConnections: 2,
    },
  });
  const provider = new L1FollowerProvider({ store, transport });
  const nowMs = input.nowMs ?? Date.now;
  const readSlotConfig = (): Promise<SlotConfig> => provider.cachedSlotConfig();
  const ledgerTip = async (): Promise<L1ViewPoint> =>
    ledgerTipPointOf(
      await transportCall(() => transport.query({ query: "chain_point" })),
    );
  const viewPoint = async (): Promise<L1ViewPoint> => {
    let cursor;
    try {
      cursor = await store.cursor();
    } catch (error) {
      throw new L1ProviderTransientError("store", "unavailable", {
        cause: error,
      });
    }
    if (cursor === null) throw notInitialized();
    return {
      slot: cursor.point.slot,
      id: cursor.point.hash.toString("hex"),
    };
  };
  return openL1Access({
    kind: "follower",
    provider,
    store,
    transport,
    ledgerTip,
    endpoint: input.endpoint,
    slotConfig: readSlotConfig,
    // The clock's tip is the follower's covered tip (N1), the same tip
    // `depth()` counts from.
    tipSlot: async () => (await viewPoint()).slot,
    submitSlotSnapshot: async () => {
      const config = await readSlotConfig();
      const tip = await ledgerTip();
      return ledgerSubmitSlotSnapshot({
        slotConfig: config,
        ledgerTipSlot: tip.slot,
        nowMs: nowMs(),
        boundMs: input.nodeBehindMaxMs,
      });
    },
    viewPoint,
    synchronizedViewPoint: async () => {
      const deadline = Date.now() + VIEW_SYNC_TIMEOUT_MS;
      const tip = await ledgerTip();
      for (;;) {
        const view = await viewPoint();
        if (
          view.slot > tip.slot ||
          (view.slot === tip.slot && view.id === tip.id)
        )
          return view;
        if (Date.now() >= deadline)
          throw new L1ProviderTransientError("follower", "behind_node_tip", {
            cause: new Error(
              `the follower is at slot ${view.slot.toString()}, the node's tip at ${tip.slot.toString()}`,
            ),
          });
        await new Promise((resolve) => setTimeout(resolve, VIEW_SYNC_POLL_MS));
      }
    },
    protocolParametersCbor: () =>
      transportCall(() => transport.query({ query: "protocol_params" })),
    transportReadiness: () => transport.readiness,
    close: () => store.close().catch(() => undefined),
  });
};

/** The node configuration the L1 access reads. */
export type NodeL1AccessConfig = Pick<
  NodeConfigDep,
  | "L1_NATIVE_LEDGER"
  | "NETWORK"
  | "L1_NODE_BEHIND_MAX_MS"
  | "POSTGRES_HOST"
  | "POSTGRES_PORT"
  | "POSTGRES_USER"
  | "POSTGRES_PASSWORD"
  | "POSTGRES_DB"
>;

/** Opens the node's L1 access from its configuration over its `index`. */
export const openNodeL1AccessFromConfig = async (
  config: NodeL1AccessConfig,
  index: NodeL1AccessOverInput["index"],
): Promise<NodeL1Access> => {
  if (config.L1_NATIVE_LEDGER === undefined)
    throw new Error(NODE_L1_ACCESS_UNCONFIGURED);
  return await openNodeL1Access({
    nativeLedger: config.L1_NATIVE_LEDGER,
    network: config.NETWORK,
    connectionString: nodeDatabaseConnectionString(config),
    index,
    nodeBehindMaxMs: config.L1_NODE_BEHIND_MAX_MS,
  });
};
