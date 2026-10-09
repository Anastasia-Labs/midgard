import { L1NodeTransport } from "@al-ft/l1-node-transport";
import {
  createWalletSeeder,
  type DepthParameters,
  type FactStore,
  type FactStoreOptions,
  followChain,
  FOLLOWER_CATCHING_UP,
  FOLLOWER_NODE_BEHIND,
  FOLLOWER_NODE_UNAVAILABLE,
  FOLLOWER_TRACKED_SET_CHANGED,
  FOLLOWER_WAITING,
  type FollowStatus,
  openPostgresFactStore,
  type PointStatus,
  projectionStoreOptions,
  WALLET_SEED_PENDING,
  type WalletSeeder,
  type WriterLease,
} from "@al-ft/midgard-l1-follower";
import { L1FollowerProvider } from "@al-ft/midgard-l1-follower/provider";
import { Lucid, type LucidEvolution } from "@lucid-evolution/lucid";
import { createScalusEvaluator } from "@lucid-evolution/scalus-uplc";

import type { LoadedCommitteeConfig } from "../../config.js";
import {
  addressBytes,
  type CommitteeL1FollowerPlan,
  committeeL1FollowerPlan,
  committeeTracked,
  lucidNetwork,
  ownWallets,
} from "./committee-follower-config.js";
import { type SignedHeader, type SlotTime, slotTimeMs } from "./obligations.js";
import {
  committeeProjection,
  type CommitteeView,
  readCommitteeView,
} from "./projection.js";
import {
  type CommitteeL1Retention,
  committeeL1Retention,
  type CommitteePinTargets,
  pruneAfterPins,
} from "./retention-pins.js";

/** The committee's follower is not configured; the detail names what is missing. */
export const L1_FOLLOWER_UNCONFIGURED = "l1_follower_unconfigured";

/**
 * The L1 node refused the node-to-client handshake. The detail is the
 * refusal the node returned; likely causes are a wrong
 * `CARDANO_NETWORK_MAGIC`, or no node-to-client version both sides speak.
 * The transport keeps redialing, so a node that answers again clears it, but
 * a wrong configuration does not clear by waiting: an intervention, never
 * the transient `l1_node_unavailable`.
 */
export const L1_NODE_HANDSHAKE_FAILED = "l1_node_handshake_failed";

/** A reason the committee's L1 source holds it unready. */
export type CommitteeL1Readiness = Readonly<{ reason: string; detail: string }>;

/**
 * What the committee reads from L1: the follower's named readiness reasons,
 * and the committee view at the follower's cursor. Every L1 decision the
 * committee makes reads it, and nothing else.
 */
export type CommitteeL1Source = Readonly<{
  parameters: DepthParameters;
  /** Every reason the follower holds the committee unready; empty when it is ready. */
  readiness(): readonly CommitteeL1Readiness[];
  /** The follower's cursor slot, or null before its first status. */
  cursorSlot(): number | null;
  /**
   * The committee view at the follower's cursor, null before the store is
   * initialized. Settles owed own-wallet seeds first. Throws on a transient
   * read failure (the database, the node's ledger state).
   */
  readView(
    options: Readonly<{
      signed: readonly SignedHeader[];
      exitsOf: readonly string[];
    }>,
  ): Promise<CommitteeView | null>;
  /** Where `point` stands on the follower's chain. */
  pointStatus(
    point: Readonly<{ slot: number; blockHash: string }>,
  ): Promise<PointStatus>;
}>;

/** The committee's running follower: its L1 source, store and provider. */
export type CommitteeL1Follower = Readonly<{
  source: CommitteeL1Source;
  /** Null when the follower is not configured. */
  store: FactStore | null;
  /**
   * The follower's provider: facts, the node's ledger state and its
   * LocalTxSubmission. Null when the follower is not configured.
   */
  provider: L1FollowerProvider | null;
  /**
   * The committee's retention pins on the follower's store; null when the
   * follower is not configured.
   */
  retention: CommitteeL1Retention | null;
  /** A Lucid client on the follower's provider; throws when not configured. */
  lucid(): Promise<LucidEvolution>;
  /** Settles once the follower stopped and released its store and transport. */
  stop(): Promise<void>;
}>;

const message = (error: unknown): string =>
  error instanceof Error ? error.message : String(error);

export const depthParameters = (
  config: LoadedCommitteeConfig,
): DepthParameters => ({
  confirmationDepth: config.finalityDepth,
  // k: the manifest's automaticRecoveryMaxDepth (plan §1, terms).
  securityParameter: config.automaticRecoveryMaxDepth,
});

/** A source that holds the committee unready with `reason`. */
const unconfiguredSource = (
  parameters: DepthParameters,
  reason: string,
): CommitteeL1Source => ({
  parameters,
  readiness: () => [{ reason: L1_FOLLOWER_UNCONFIGURED, detail: reason }],
  cursorSlot: () => null,
  readView: async () => null,
  pointStatus: async () => ({ kind: "not_initialized", detail: reason }),
});

export type CommitteeL1SourceParts = Readonly<{
  store: FactStore;
  parameters: DepthParameters;
  /** The follower loop's latest status, or null before its first one. */
  status: () => FollowStatus | null;
  /** Lucid's slot configuration, read from the node's ledger state. */
  slotTime: () => Promise<SlotTime>;
  seeder?: WalletSeeder;
}>;

/**
 * The committee's L1 source over a follower store: readiness from the loop's
 * status and the wallet seeder, and the view read at the store's cursor.
 */
export const committeeL1Source = (
  parts: CommitteeL1SourceParts,
): CommitteeL1Source => {
  /** The last seed attempt's pending reason, while a seed is owed. */
  let seedDetail = "own wallets not seeded yet";
  const followerReadiness = (): readonly CommitteeL1Readiness[] => {
    const status = parts.status();
    return status === null
      ? [
          {
            reason: FOLLOWER_CATCHING_UP,
            detail: "the follower has not started",
          },
        ]
      : status.readiness.map((entry) =>
          entry.reason === FOLLOWER_NODE_UNAVAILABLE &&
          status.node?.reason === "node_handshake_failed"
            ? {
                reason: L1_NODE_HANDSHAKE_FAILED,
                detail: `the L1 node refused the handshake: ${entry.detail}`,
              }
            : entry,
        );
  };
  const seedPending = (): CommitteeL1Readiness[] =>
    parts.seeder === undefined || parts.seeder.ready()
      ? []
      : [{ reason: WALLET_SEED_PENDING, detail: seedDetail }];
  return {
    parameters: parts.parameters,
    readiness: () => [...followerReadiness(), ...seedPending()],
    cursorSlot: () => parts.status()?.cursor?.slot ?? null,
    readView: async (options) => {
      // A seed is read at the store's cursor, so it waits for the follower.
      if (
        parts.seeder !== undefined &&
        !parts.seeder.ready() &&
        followerReadiness().length === 0
      ) {
        const seeded = await parts.seeder.step();
        if (seeded.kind === "pending")
          seedDetail = `${seeded.reason}: ${seeded.detail}`;
      }
      return readCommitteeView(parts.store, {
        parameters: parts.parameters,
        slotTime: await parts.slotTime(),
        signed: options.signed,
        exitsOf: options.exitsOf,
      });
    },
    pointStatus: (point) =>
      parts.store.pointStatus({
        slot: point.slot,
        hash: Buffer.from(point.blockHash, "hex"),
      }),
  };
};

/**
 * The first reason in `reasons` no wait clears (an intervention such as
 * `rollback_beyond_k`, a stuck point, no configuration), or undefined while
 * the follower only catches up, backs off, waits for the L1 node (down, or
 * behind wall-clock time), or replays a store reset from the origin
 * (`tracked_set_changed` clears once the replay reaches the node tip).
 */
export const committeeL1InterventionReason = (
  reasons: readonly CommitteeL1Readiness[],
): CommitteeL1Readiness | undefined =>
  reasons.find(
    ({ reason }) =>
      reason !== FOLLOWER_CATCHING_UP &&
      reason !== FOLLOWER_WAITING &&
      reason !== FOLLOWER_NODE_UNAVAILABLE &&
      reason !== FOLLOWER_NODE_BEHIND &&
      reason !== FOLLOWER_TRACKED_SET_CHANGED &&
      reason !== WALLET_SEED_PENDING,
  );

/**
 * Resolves once the follower holds the committee for nothing but an owed
 * wallet seed (the committee's next view read settles it). Never gives up:
 * while any other reason holds, it reports the reasons to `onHeld` on every
 * poll and waits, so a rollback beyond k or a missing configuration holds
 * the committee unready with the process up. `onHeld` may throw to stop the
 * wait (a one-shot run does on a reason no wait clears).
 */
export const untilCommitteeL1SourceReady = async (
  source: Pick<CommitteeL1Source, "readiness">,
  options: Readonly<{
    onHeld?: (reasons: readonly CommitteeL1Readiness[]) => void;
    pollMs?: number;
  }> = {},
): Promise<void> => {
  for (;;) {
    const reasons = source
      .readiness()
      .filter(({ reason }) => reason !== WALLET_SEED_PENDING);
    if (reasons.length === 0) return;
    options.onHeld?.(reasons);
    await new Promise((resolve) =>
      setTimeout(resolve, options.pollMs ?? 1_000),
    );
  }
};

/** The writer lease the committee holds for its follower, or null. */
type CommitteeWriterLease = () => Promise<WriterLease | null>;

type RunPlan = Extract<CommitteeL1FollowerPlan, { kind: "run" }>;

/** The follower's store, transport and provider, before any loop runs. */
type FollowerParts = Readonly<{
  store: FactStore;
  transport: L1NodeTransport;
  wallets: readonly Buffer[];
  provider: L1FollowerProvider;
  slotTime: () => Promise<SlotTime>;
  lucid: () => Promise<LucidEvolution>;
  close: () => Promise<void>;
}>;

/**
 * The committee follower's store options: the committee projection and its
 * tracked set, with the committee's own wallets tracked by address
 * (`wallets`: seeded, never in the store's tracked-set record).
 */
export const committeeFollowerStoreOptions = (
  config: LoadedCommitteeConfig,
  parameters: DepthParameters,
  wallets: readonly Buffer[],
  dialect: "postgres" | "sqlite",
): FactStoreOptions => {
  const options = projectionStoreOptions(
    [
      committeeProjection(
        {
          stateQueueAddress: addressBytes(config.stateQueueAddress),
          stateQueuePolicyId: config.stateQueuePolicyId.toLowerCase(),
        },
        committeeTracked(config),
      ),
    ],
    {
      securityParameter: parameters.securityParameter,
      trackedSet: {
        addresses: new Set(),
        paymentCredentials: new Set(),
        policies: new Set(),
      },
    },
    dialect,
  );
  return { ...options, wallets };
};

const openFollowerParts = async (
  config: LoadedCommitteeConfig,
  plan: RunPlan,
  parameters: DepthParameters,
  writerLease: CommitteeWriterLease,
  log: (line: string) => void,
): Promise<FollowerParts> => {
  const wallets = await ownWallets(config);
  const transport = new L1NodeTransport({
    binaryPath: plan.binaryPath,
    socketPath: plan.socketPath,
    networkMagic: plan.networkMagic,
    onDiagnostic: (line) => log(`L1 follower transport: ${line}`),
  });
  let store: FactStore;
  try {
    store = openPostgresFactStore({
      ...committeeFollowerStoreOptions(config, parameters, wallets, "postgres"),
      connection: {
        connectionString: plan.databaseUrl,
        maxConnections: 4,
        // A dropped connection fails the next query into the loop's backoff.
        onConnectionError: (error) =>
          log(`L1 follower: database connection lost: ${error.message}`),
      },
      writerLease,
    });
  } catch (error) {
    void transport.close();
    throw error;
  }
  const provider = new L1FollowerProvider({ store, transport });
  let slotConfig: Promise<SlotTime> | undefined;
  const slotTime = (): Promise<SlotTime> => {
    slotConfig ??= provider.slotConfig().catch((error: unknown) => {
      slotConfig = undefined;
      throw new Error(`L1 slot configuration unavailable: ${message(error)}`);
    });
    return slotConfig;
  };
  const network = lucidNetwork(config.network);
  return {
    store,
    transport,
    wallets,
    provider,
    slotTime,
    lucid: async () =>
      Lucid(provider, network, {
        slotConfig: await slotTime(),
        evaluator: createScalusEvaluator(),
      }),
    close: async () => {
      await store.close().catch(() => undefined);
      await transport.close().catch(() => undefined);
    },
  };
};

/**
 * Starts the committee's L1 follower: its own transport session and its own
 * tables in the committee's Postgres database, the committee projections
 * current, and its named reasons on `/readyz`. Its writer lease is
 * `writerLease`, the one the committee store's instance lock holds, so it
 * writes only while this process holds the store; while the lock is
 * suspended or passive, its loop waits on `store_locked`. Never throws for
 * a missing configuration: the committee then stays unready with
 * `l1_follower_unconfigured`.
 *
 * `records` reads every L1 point and submission the committee store names:
 * before each prune step the loop brings their retention pins current, so
 * no history a stored record reads again is pruned (plan §11).
 */
export const startCommitteeL1Follower = async (
  config: LoadedCommitteeConfig,
  writerLease: CommitteeWriterLease,
  log: (line: string) => void,
  records: () => Promise<CommitteePinTargets>,
): Promise<CommitteeL1Follower> => {
  const parameters = depthParameters(config);
  const plan = committeeL1FollowerPlan(config);
  if (plan.kind === "unconfigured") {
    log(`L1 follower is not configured: ${plan.reason}`);
    return {
      source: unconfiguredSource(parameters, plan.reason),
      store: null,
      provider: null,
      retention: null,
      lucid: () =>
        Promise.reject(
          new Error(`L1 follower is not configured: ${plan.reason}`),
        ),
      stop: async () => undefined,
    };
  }
  const parts = await openFollowerParts(
    config,
    plan,
    parameters,
    writerLease,
    log,
  );
  const { store } = parts;
  const seeder =
    parts.wallets.length === 0
      ? undefined
      : createWalletSeeder({
          store,
          ledger: parts.transport,
          wallets: parts.wallets,
        });
  const retention = committeeL1Retention(store);
  retention.bind("records", records);
  let status: FollowStatus | null = null;
  const abort = new AbortController();
  const running = followChain({
    store: pruneAfterPins(store, retention),
    transport: parts.transport,
    origin: plan.origin,
    signal: abort.signal,
    log: (line) => log(`L1 follower: ${line}`),
    // `l1_node_behind` at the follower's default bound: the tick holds
    // while it is set, and it clears when the node catches up.
    nodeBehind: {
      slotTime: async (slot) => slotTimeMs(slot, await parts.slotTime()),
    },
    onStatus: (next) => {
      status = next;
    },
  });
  return {
    source: committeeL1Source({
      store,
      parameters,
      status: () => status,
      slotTime: parts.slotTime,
      ...(seeder === undefined ? {} : { seeder }),
    }),
    store,
    provider: parts.provider,
    retention,
    lucid: parts.lucid,
    stop: async () => {
      abort.abort();
      await running.catch(() => undefined);
      seeder?.close();
      await parts.close();
    },
  };
};

/** A read-only Lucid client on the committee follower's facts, for a CLI. */
export type CommitteeL1Reader = Readonly<{
  lucid(): Promise<LucidEvolution>;
  close(): Promise<void>;
}>;

/**
 * Opens the committee follower's facts for reading from another process:
 * no loop runs and no write is possible (the writer lease is never held).
 * The facts are as current as the running committee's follower made them;
 * the own wallets' outputs are there once that follower seeded them.
 * Throws when the follower is not configured.
 */
export const openCommitteeL1Reader = async (
  config: LoadedCommitteeConfig,
  log: (line: string) => void,
): Promise<CommitteeL1Reader> => {
  const plan = committeeL1FollowerPlan(config);
  if (plan.kind === "unconfigured")
    throw new Error(`L1 follower is not configured: ${plan.reason}`);
  const parts = await openFollowerParts(
    config,
    plan,
    depthParameters(config),
    async () => null,
    log,
  );
  return { lucid: parts.lucid, close: parts.close };
};
