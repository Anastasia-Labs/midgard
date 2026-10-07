import { afterAll, afterEach, describe, expect, it, vi } from "vitest";

import {
  type CommitteeStore,
  decisionEffectId,
  DecisionEffectInFlightError,
  type DecisionOutboxRecord,
  type L1SourceState,
} from "../src/store.js";
import {
  INSTANCE_LOCK_RECONNECT_MAX_MS,
  isInstanceLockHeldElsewhere,
} from "../src/store/postgres.instance-lock.js";
import {
  PostgresCommitteeStore,
  type PostgresCommitteeStoreOptions,
} from "../src/store/postgres.js";
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
  readonly terminateLockSession: () => Promise<void>;
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

describe("decision effect attempts across committee node processes", () => {
  it("are reclaimed at once from a process that died mid-attempt", async () => {
    const at = await postgresLocation();
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
  });

  it("are never run twice while live, however long they take", async () => {
    const at = await postgresLocation();
    const live = await at.open();
    await live.beginDecisionEffect({ effect: reconcile, sourceState });

    await expect(at.open()).rejects.toThrow(
      /held by another live committee node process/u,
    );

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
  });

  it("refuse every effect while a Postgres store's lock session is gone, then resume exactly once when the lock is free again", async () => {
    const at = await postgresLocation();
    const onInstanceLockHeldElsewhere = vi.fn();
    const onInstanceLockSuspended = vi.fn();
    const onInstanceLockRestored = vi.fn();
    const live = await at.open({
      onInstanceLockHeldElsewhere,
      onInstanceLockSuspended,
      onInstanceLockRestored,
    });
    await at.terminateLockSession();
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
    expect(onInstanceLockHeldElsewhere).not.toHaveBeenCalled();
    // Held again: a second process is still refused.
    await expect(at.open()).rejects.toThrow(
      /held by another live committee node process/u,
    );
  });

  it("keep a displaced member passive, refusing every effect, until the holder dies, then hand over within one reconnect with no effect lost or run twice", async () => {
    const at = await postgresLocation();
    const passiveEvents: string[] = [];
    const orphaned = await at.open({
      onInstanceLockHeldElsewhere: () => passiveEvents.push("held_elsewhere"),
      onInstanceLockRestored: () => passiveEvents.push("restored"),
    });
    await at.terminateLockSession();
    // A successor takes the free lock before the orphan tries again; the
    // orphan becomes the passive member and stays up.
    const active = await at.open();
    await vi.waitFor(
      () => {
        expect(passiveEvents).toEqual(["held_elsewhere"]);
      },
      { timeout: 5_000 },
    );
    await expect(
      orphaned.beginDecisionEffect({ effect: reconcile, sourceState }),
    ).rejects.toThrow(/is passive/u);
    await expect(complete(orphaned, 1)).rejects.toThrow(/is passive/u);
    // A third member starting now is refused as well, in the way startup
    // retries (`starting:store_instance_lock_held`), not as a fatal error.
    const refused = await at.open().catch((error: unknown) => error);
    expect(isInstanceLockHeldElsewhere(refused)).toBe(true);

    // The active member begins the effect and dies mid-attempt.
    await active.beginDecisionEffect({ effect: reconcile, sourceState });
    const diedAtMs = Date.now();
    await at.kill(active);

    // One reconnect later the passive member holds the lock: its retry
    // interval never exceeds the reconnect ceiling.
    await vi.waitFor(
      () => {
        expect(passiveEvents).toEqual(["held_elsewhere", "restored"]);
      },
      { timeout: INSTANCE_LOCK_RECONNECT_MAX_MS + 5_000, interval: 50 },
    );
    expect(Date.now() - diedAtMs).toBeLessThanOrEqual(
      INSTANCE_LOCK_RECONNECT_MAX_MS + 1_000,
    );
    // The dead attempt is there, exactly once, and is redone exactly once.
    await expect(
      orphaned.getDecisionOutbox(reconcile.effectId),
    ).resolves.toMatchObject({ status: "pending", attemptCount: 1 });
    await orphaned.beginDecisionEffect({
      effect: attempt(2, 1),
      sourceState: (await orphaned.getL1SourceState())!,
    });
    await expect(complete(orphaned, 1)).rejects.toThrow(
      /does not match the pending attempt/u,
    );
    await complete(orphaned, 2);
    await expect(
      orphaned.getDecisionOutbox(reconcile.effectId),
    ).resolves.toMatchObject({ status: "reconciled", attemptCount: 2 });
    await expect(orphaned.listDecisionOutbox(headerHash)).resolves.toHaveLength(
      1,
    );
  }, 60_000);
});
