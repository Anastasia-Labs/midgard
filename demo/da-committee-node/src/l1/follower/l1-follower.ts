import { L1NodeTransport } from "@al-ft/l1-node-transport";
import {
  createWalletSeeder,
  type DepthParameters,
  type FactStore,
  followChain,
  FOLLOWER_CATCHING_UP,
  FOLLOWER_WAITING,
  type FollowReadiness,
  type FollowStatus,
  openPostgresFactStore,
  type PointStatus,
  projectionStoreOptions,
  WALLET_SEED_PENDING,
  type WalletSeeder,
  withTrackedAddresses,
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
import type { SignedHeader, SlotTime } from "./obligations.js";
import {
  committeeProjection,
  type CommitteeView,
  readCommitteeView,
} from "./projection.js";

/** The committee's follower is not configured; the detail names what is missing. */
export const L1_FOLLOWER_NOT_CONFIGURED = "l1_follower_not_configured";

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
  /** A Lucid client on the follower's provider; throws when not configured. */
  lucid(): Promise<LucidEvolution>;
  /** Settles once the follower stopped and released its store and transport. */
  stop(): Promise<void>;
}>;

const message = (error: unknown): string =>
  error instanceof Error ? error.message : String(error);

const depthParameters = (config: LoadedCommitteeConfig): DepthParameters => ({
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
  readiness: () => [{ reason: L1_FOLLOWER_NOT_CONFIGURED, detail: reason }],
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
  const followerReadiness = (): readonly FollowReadiness[] => {
    const status = parts.status();
    return status === null
      ? [
          {
            reason: FOLLOWER_CATCHING_UP,
            detail: "the follower has not started",
          },
        ]
      : status.readiness;
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
 * Resolves once the follower holds the committee for nothing but an owed
 * wallet seed (the committee's next view read settles it). While it only
 * catches up or backs off, it waits; any other reason (an intervention, a
 * stuck point, no configuration) no wait clears, so it throws that reason.
 */
export const untilCommitteeL1SourceReady = async (
  source: Pick<CommitteeL1Source, "readiness">,
  pollMs = 1_000,
): Promise<void> => {
  for (;;) {
    const reasons = source
      .readiness()
      .filter(({ reason }) => reason !== WALLET_SEED_PENDING);
    if (reasons.length === 0) return;
    const blocking = reasons.find(
      ({ reason }) =>
        reason !== FOLLOWER_CATCHING_UP && reason !== FOLLOWER_WAITING,
    );
    if (blocking !== undefined)
      throw new Error(`${blocking.reason}: ${blocking.detail}`);
    await new Promise((resolve) => setTimeout(resolve, pollMs));
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
  slotTime: () => Promise<SlotTime>;
  lucid: () => Promise<LucidEvolution>;
  close: () => Promise<void>;
}>;

const openFollowerParts = async (
  config: LoadedCommitteeConfig,
  plan: RunPlan,
  parameters: DepthParameters,
  writerLease: CommitteeWriterLease,
  log: (line: string) => void,
): Promise<FollowerParts> => {
  const projection = committeeProjection(
    {
      stateQueueAddress: addressBytes(config.stateQueueAddress),
      stateQueuePolicyId: config.stateQueuePolicyId.toLowerCase(),
    },
    committeeTracked(config),
  );
  const wallets = await ownWallets(config);
  const transport = new L1NodeTransport({
    binaryPath: plan.binaryPath,
    socketPath: plan.socketPath,
    networkMagic: plan.networkMagic,
    onDiagnostic: (line) => log(`L1 follower transport: ${line}`),
  });
  let store: FactStore;
  try {
    const options = projectionStoreOptions(
      [projection],
      {
        securityParameter: parameters.securityParameter,
        trackedSet: {
          addresses: new Set(),
          paymentCredentials: new Set(),
          policies: new Set(),
        },
      },
      "postgres",
    );
    store = openPostgresFactStore({
      ...options,
      trackedSet: withTrackedAddresses(options.trackedSet, wallets),
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
 * `l1_follower_not_configured`.
 */
export const startCommitteeL1Follower = async (
  config: LoadedCommitteeConfig,
  writerLease: CommitteeWriterLease,
  log: (line: string) => void,
): Promise<CommitteeL1Follower> => {
  const parameters = depthParameters(config);
  const plan = committeeL1FollowerPlan(config);
  if (plan.kind === "unconfigured") {
    log(`L1 follower is not configured: ${plan.reason}`);
    return {
      source: unconfiguredSource(parameters, plan.reason),
      store: null,
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
  let status: FollowStatus | null = null;
  const abort = new AbortController();
  const running = followChain({
    store,
    transport: parts.transport,
    origin: plan.origin,
    signal: abort.signal,
    log: (line) => log(`L1 follower: ${line}`),
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
