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
 * - The protocol tracked set is the follower's recorded one
 *   (`trackedSetRecord`), read until the follower has recorded it; the own
 *   wallets (`nodeSeededAddresses`) are tracked from the start, so a wallet
 *   read before the follower's first cursor is the transient
 *   `follower:not_initialized`, never a ledger answer.
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
import type { NativeLedgerNetwork } from "@al-ft/midgard-core/native-reward-account";
import {
  type FactStore,
  openPostgresFactStore,
  withTrackedAddresses,
} from "@al-ft/midgard-l1-follower";
import {
  fromTransportError,
  L1FollowerProvider,
  L1ProviderTransientError,
} from "@al-ft/midgard-l1-follower/provider";
import type {
  Address,
  Credential,
  OutRef,
  PolicyId,
  SlotConfig,
  Unit,
  UTxO,
} from "@lucid-evolution/lucid";

import {
  type L1Access,
  type L1AccessAdapter,
  type L1ViewPoint,
  openL1Access,
} from "../l1-access.js";
import { type NodeConfigDep } from "./config.node-config-dep.js";
import { nodeSeededAddresses } from "./intent-journal.tracked-set.js";
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

const EMPTY_TRACKED_SET = {
  addresses: new Set<string>(),
  paymentCredentials: new Set<string>(),
  policies: new Set<string>(),
};

/**
 * The follower provider with the node's tracked set: the follower's recorded
 * protocol set plus the node's own wallets, read before each UTxO query
 * until the follower has recorded it.
 */
export class NodeL1Provider extends L1FollowerProvider {
  readonly #store: FactStore;
  readonly #wallets: readonly Buffer[];
  #recorded = false;

  constructor(
    options: ConstructorParameters<typeof L1FollowerProvider>[0] &
      Readonly<{ wallets: readonly Buffer[] }>,
  ) {
    super(options);
    this.#store = options.store;
    this.#wallets = options.wallets;
  }

  /** Adopts the follower's recorded protocol set once it exists. */
  async refreshTrackedSet(): Promise<void> {
    if (this.#recorded) return;
    let record;
    try {
      record = await this.#store.trackedSetRecord();
    } catch {
      // No follower tables yet: wallets stay tracked, the read that follows
      // names the store or follower fault.
      return;
    }
    if (record === null) return;
    this.#store.setTrackedSet(
      withTrackedAddresses(
        {
          addresses: new Set(record.trackedSet.addresses),
          paymentCredentials: new Set(record.trackedSet.paymentCredentials),
          policies: new Set(record.trackedSet.policies),
        },
        this.#wallets,
      ),
    );
    this.#recorded = true;
  }

  override async getUtxos(
    addressOrCredential: Address | Credential,
  ): Promise<UTxO[]> {
    await this.refreshTrackedSet();
    return super.getUtxos(addressOrCredential);
  }

  override async getUtxosWithUnit(
    addressOrCredential: Address | Credential,
    unit: Unit,
  ): Promise<UTxO[]> {
    await this.refreshTrackedSet();
    return super.getUtxosWithUnit(addressOrCredential, unit);
  }

  override async getUtxosWithPolicy(
    addressOrCredential: Address | Credential,
    policyId: PolicyId,
  ): Promise<UTxO[]> {
    await this.refreshTrackedSet();
    return super.getUtxosWithPolicy(addressOrCredential, policyId);
  }

  override async getUtxoByUnit(unit: Unit): Promise<UTxO> {
    await this.refreshTrackedSet();
    return super.getUtxoByUnit(unit);
  }

  override async getUtxosByOutRef(outRefs: Array<OutRef>): Promise<UTxO[]> {
    await this.refreshTrackedSet();
    return super.getUtxosByOutRef(outRefs);
  }
}

/** The follower adapter (roles only): the store and transport around it. */
export type NodeL1AccessAdapter = L1AccessAdapter &
  Readonly<{
    kind: "follower";
    provider: NodeL1Provider;
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

export type OpenNodeL1AccessInput = Readonly<{
  nativeLedger: NativeLedgerSettings;
  network: NativeLedgerNetwork;
  connectionString: string;
  wallets: readonly Buffer[];
  /** The ledger-tip staleness bound for submit-slot snapshots (ms). */
  nodeBehindMaxMs: number;
  /** Bound on one transport request (ms); the transport's default when absent. */
  requestTimeoutMs?: number;
  nowMs?: () => number;
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
  const store = openPostgresFactStore({
    // Never started: the follower in the main thread records the protocol
    // set; this reader adopts it (`NodeL1Provider.refreshTrackedSet`).
    securityParameter: 1,
    trackedSet: EMPTY_TRACKED_SET,
    wallets: input.wallets,
    connection: {
      connectionString: input.connectionString,
      maxConnections: 2,
    },
  });
  const provider = new NodeL1Provider({
    store,
    transport,
    wallets: input.wallets,
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
    endpoint: input.nativeLedger.socketPath,
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
> &
  Parameters<typeof nodeSeededAddresses>[0];

export { NODE_L1_ACCESS_UNCONFIGURED };

/** Opens the node's L1 access from its configuration. */
export const openNodeL1AccessFromConfig = async (
  config: NodeL1AccessConfig,
): Promise<NodeL1Access> => {
  if (config.L1_NATIVE_LEDGER === undefined)
    throw new Error(NODE_L1_ACCESS_UNCONFIGURED);
  return await openNodeL1Access({
    nativeLedger: config.L1_NATIVE_LEDGER,
    network: config.NETWORK,
    connectionString: nodeDatabaseConnectionString(config),
    wallets: nodeSeededAddresses(config),
    nodeBehindMaxMs: config.L1_NODE_BEHIND_MAX_MS,
  });
};
