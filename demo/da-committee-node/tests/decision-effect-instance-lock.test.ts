import { afterAll, afterEach, describe, expect, it, vi } from "vitest";

import {
  type CommitteeStore,
  decisionEffectId,
  DecisionEffectInFlightError,
  type DecisionOutboxRecord,
  JsonFileCommitteeStore,
  type L1SourceState,
} from "../src/store.js";
import {
  PostgresCommitteeStore,
  type PostgresCommitteeStoreOptions,
} from "../src/store/postgres.js";
import { tempDir } from "./helpers.js";
import {
  postgresTestDatabases,
  terminateInstanceLockSessions,
} from "./helpers/postgres-database.js";

const openStores = new Set<CommitteeStore>();
const databases = postgresTestDatabases("committee_lease");

afterEach(async () => {
  await Promise.all([...openStores].map(async (store) => store.close?.()));
  openStores.clear();
});

afterAll(async () => {
  await databases.dropAll();
});

/**
 * One committee store location, opened as many times as a test needs: each
 * `open` is what a fresh committee node process does at startup, and `kill`
 * ends a live instance the way a crash does, leaving its rows as they are.
 */
type StoreLocation = {
  readonly open: (
    options?: PostgresCommitteeStoreOptions,
  ) => Promise<CommitteeStore>;
  readonly kill: (store: CommitteeStore) => Promise<void>;
  readonly terminateLockSession?: () => Promise<void>;
};

const jsonLocation = async (): Promise<StoreLocation> => {
  const dir = await tempDir();
  return {
    open: async () => {
      const store = await JsonFileCommitteeStore.open(dir);
      openStores.add(store);
      return store;
    },
    // The JSON store's lock file outlives a crash and needs explicit
    // stale-lock recovery; closing leaves the same durable rows behind.
    kill: async (store) => {
      openStores.delete(store);
      await store.close?.();
    },
  };
};

const postgresLocation = async (): Promise<StoreLocation> => {
  const database = await databases.create();
  const endedSessions = new Map<CommitteeStore, Promise<void>>();
  const terminateLockSession = async (): Promise<void> => {
    expect(await terminateInstanceLockSessions(database)).toBe(1);
  };
  return {
    open: async (options = {}) => {
      let signalEnded!: () => void;
      const ended = new Promise<void>((resolve) => {
        signalEnded = resolve;
      });
      const store = await PostgresCommitteeStore.open(database.url, {
        ...options,
        onInstanceLockSuspended: (error) => {
          options.onInstanceLockSuspended?.(error);
          signalEnded();
        },
      });
      openStores.add(store);
      endedSessions.set(store, ended);
      return store;
    },
    // The server ends the session that holds the instance lock, exactly as it
    // does when the process holding it dies; the process is gone, so the
    // store is closed before it could take the lock again.
    kill: async (store) => {
      await terminateLockSession();
      await endedSessions.get(store);
      openStores.delete(store);
      await store.close?.();
    },
    terminateLockSession,
  };
};

const deploymentFingerprint = "cd".repeat(32);
const headerHash = "12".repeat(28);
const stateQueueOutRef = `${"34".repeat(32)}#0`;

const reconcile: DecisionOutboxRecord = {
  schemaVersion: 1,
  effectId: decisionEffectId({
    deploymentFingerprint,
    headerHash,
    stateQueueOutRef,
    effectKind: "l1_reconcile",
  }),
  deploymentFingerprint,
  sourceMode: "local_node",
  network: "Preprod",
  effectKind: "l1_reconcile",
  headerHash,
  stateQueueOutRef,
  slot: 1,
  blockHash: "66".repeat(32),
  finalized: true,
  status: "pending",
  attemptCount: 1,
  createdAt: "2026-07-28T00:00:00.000Z",
  updatedAt: "2026-07-28T00:00:00.000Z",
};

const sourceState: L1SourceState = {
  schemaVersion: 1,
  sourceMode: "local_node",
  network: "Preprod",
  authoritySha256: "91".repeat(32),
  status: "healthy",
  observations: [
    {
      headerHash,
      stateQueueOutRef,
      stateQueueStatus: "attested",
      slot: 1,
      blockHash: "66".repeat(32),
      finalized: true,
      hasPersistedDecision: true,
    },
  ],
  observedAt: "2026-07-28T00:00:00.000Z",
};

/** Attempt `attemptCount`, begun `afterMs` after the first attempt. */
const attempt = (
  attemptCount: number,
  afterMs: number,
): DecisionOutboxRecord => ({
  ...reconcile,
  attemptCount,
  updatedAt: new Date(Date.parse(reconcile.updatedAt) + afterMs).toISOString(),
});

const complete = (
  store: CommitteeStore,
  expectedAttemptCount: number,
): Promise<void> =>
  store.completeDecisionEffect({
    effectId: reconcile.effectId,
    expectedAttemptCount,
    status: "reconciled",
    updatedAt: "2026-07-28T01:00:00.000Z",
  });

const stores = [
  ["the JSON file store", jsonLocation],
  ["the Postgres store", postgresLocation],
] as const;

describe("decision effect attempts across committee node processes", () => {
  it.each(stores)(
    "are reclaimed at once from a process that died mid-attempt, by %s",
    async (_label, location) => {
      const at = await location();
      const crashed = await at.open();
      await crashed.beginDecisionEffect({ effect: reconcile, sourceState });
      await at.kill(crashed);

      const restarted = await at.open();
      await expect(
        restarted.getDecisionOutbox(reconcile.effectId),
      ).resolves.toMatchObject({ status: "pending", attemptCount: 1 });
      // One millisecond after the dead attempt: there is no lease to wait out.
      await restarted.beginDecisionEffect({
        effect: attempt(2, 1),
        sourceState: (await restarted.getL1SourceState())!,
      });
      await expect(complete(restarted, 1)).rejects.toThrow(
        /does not match the pending attempt/u,
      );
      await complete(restarted, 2);
      await expect(
        restarted.getDecisionOutbox(reconcile.effectId),
      ).resolves.toMatchObject({ status: "reconciled", attemptCount: 2 });
    },
  );

  it.each(stores)(
    "are never run twice while live, however long they take, by %s",
    async (_label, location) => {
      const at = await location();
      const live = await at.open();
      await live.beginDecisionEffect({ effect: reconcile, sourceState });

      await expect(at.open()).rejects.toThrow(/already exclusively leased/u);

      // An hour later the attempt is still running in the live process, and
      // it is still the only attempt.
      const retry = live.beginDecisionEffect({
        effect: attempt(2, 60 * 60 * 1000),
        sourceState: (await live.getL1SourceState())!,
      });
      await expect(retry).rejects.toBeInstanceOf(DecisionEffectInFlightError);
      await expect(retry).rejects.toThrow(
        /still in flight in this committee node process/u,
      );
      await expect(
        live.getDecisionOutbox(reconcile.effectId),
      ).resolves.toMatchObject({ status: "pending", attemptCount: 1 });

      // A completion naming another attempt neither completes nor frees it.
      await expect(complete(live, 2)).rejects.toThrow(
        /does not match the pending attempt/u,
      );
      await expect(
        live.beginDecisionEffect({
          effect: attempt(2, 60 * 60 * 1000),
          sourceState: (await live.getL1SourceState())!,
        }),
      ).rejects.toBeInstanceOf(DecisionEffectInFlightError);

      await complete(live, 1);
      await live.beginDecisionEffect({
        effect: attempt(2, 60 * 60 * 1000),
        sourceState: (await live.getL1SourceState())!,
      });
      await complete(live, 2);
    },
  );

  it("refuse every effect while a Postgres store's lock session is gone, then resume exactly once when the lock is free again", async () => {
    const at = await postgresLocation();
    const onInstanceLockLost = vi.fn();
    const onInstanceLockSuspended = vi.fn();
    const onInstanceLockRestored = vi.fn();
    const live = await at.open({
      onInstanceLockLost,
      onInstanceLockSuspended,
      onInstanceLockRestored,
    });
    await at.terminateLockSession!();
    await vi.waitFor(() => {
      expect(onInstanceLockSuspended).toHaveBeenCalledOnce();
    });
    await expect(
      live.beginDecisionEffect({ effect: reconcile, sourceState }),
    ).rejects.toThrow(/suspended its instance lock/u);
    await expect(
      live.getDecisionOutbox(reconcile.effectId),
    ).resolves.toBeUndefined();

    await vi.waitFor(
      () => {
        expect(onInstanceLockRestored).toHaveBeenCalledOnce();
      },
      { timeout: 5_000 },
    );
    await live.beginDecisionEffect({ effect: reconcile, sourceState });
    await complete(live, 1);
    await expect(
      live.getDecisionOutbox(reconcile.effectId),
    ).resolves.toMatchObject({ status: "reconciled", attemptCount: 1 });
    expect(onInstanceLockLost).not.toHaveBeenCalled();
    // Held again: a second process is still refused.
    await expect(at.open()).rejects.toThrow(/already exclusively leased/u);
  });

  it("fail closed for good, and tell the process to stop, when another process holds the lock by the time it is tried again", async () => {
    const at = await postgresLocation();
    const onInstanceLockLost = vi.fn();
    const onInstanceLockRestored = vi.fn();
    const orphaned = await at.open({
      onInstanceLockLost,
      onInstanceLockRestored,
    });
    await at.terminateLockSession!();
    // A successor takes the free lock before the orphan tries again.
    const successor = await at.open();
    await vi.waitFor(
      () => {
        expect(onInstanceLockLost).toHaveBeenCalledOnce();
      },
      { timeout: 5_000 },
    );
    expect(onInstanceLockLost.mock.calls[0]?.[0]).toBeInstanceOf(Error);
    expect(onInstanceLockRestored).not.toHaveBeenCalled();
    await expect(
      orphaned.beginDecisionEffect({ effect: reconcile, sourceState }),
    ).rejects.toThrow(/lost its instance lock/u);
    await expect(complete(orphaned, 1)).rejects.toThrow(
      /lost its instance lock/u,
    );

    await successor.beginDecisionEffect({ effect: reconcile, sourceState });
    await complete(successor, 1);
  });
});
