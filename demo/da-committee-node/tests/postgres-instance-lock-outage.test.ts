import type { AddressInfo } from "node:net";
import { connect, createServer, type Server, type Socket } from "node:net";

import { makeDeploymentMarker } from "@al-ft/midgard-core/deployment-manifest-identity";
import { afterAll, afterEach, describe, expect, it, vi } from "vitest";

import { retryStartup } from "../src/startup.js";
import {
  decisionEffectId,
  type DecisionOutboxRecord,
  type L1SourceState,
} from "../src/store.js";
import { PostgresCommitteeStore } from "../src/store/postgres.js";
import { postgresTestDatabases } from "./helpers/postgres-database.js";

const databases = postgresTestDatabases("committee_lock_outage");
const cleanups: (() => Promise<void>)[] = [];

afterEach(async () => {
  for (const cleanup of cleanups.splice(0).reverse()) await cleanup();
});

afterAll(async () => {
  await databases.dropAll();
});

/**
 * A TCP hop in front of the test cluster that can go down and come back on
 * the same port: down, it ends every connection through it and refuses new
 * ones, as a Postgres restart or a network partition does.
 */
const outageProxy = async (upstream: URL) => {
  const sockets = new Set<Socket>();
  const clientSides = new Set<Socket>();
  let server: Server | undefined;
  let port = 0;
  const up = async (): Promise<void> => {
    if (server !== undefined) return;
    const listening = createServer((client) => {
      const target = connect(Number(upstream.port), upstream.hostname);
      for (const socket of [client, target]) {
        sockets.add(socket);
        socket.on("close", () => sockets.delete(socket));
        socket.on("error", () => undefined);
      }
      clientSides.add(client);
      client.on("close", () => clientSides.delete(client));
      client.pipe(target).pipe(client);
    });
    await new Promise<void>((resolve, reject) => {
      listening.once("error", reject);
      listening.listen(port, "127.0.0.1", resolve);
    });
    server = listening;
    port = (listening.address() as AddressInfo).port;
  };
  const down = async (): Promise<void> => {
    const closing = server;
    server = undefined;
    for (const socket of sockets) socket.destroy();
    await new Promise<void>((resolve) => {
      if (closing === undefined) resolve();
      else closing.close(() => resolve());
    });
  };
  /**
   * Breaks every connection on the client side only: the server keeps its
   * end open, so its sessions (and their advisory locks) live on, as after a
   * half-open connection drop.
   */
  const breakClientSides = (): void => {
    for (const socket of clientSides) socket.destroy();
  };
  await up();
  const url = new URL(upstream);
  url.port = port.toString();
  return { url: url.toString(), up, down, breakClientSides };
};

const deploymentFingerprint = "ce".repeat(32);
const headerHash = "13".repeat(28);
const stateQueueOutRef = `${"35".repeat(32)}#0`;
const effect: DecisionOutboxRecord = {
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
  blockHash: "67".repeat(32),
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
  authoritySha256: "92".repeat(32),
  status: "healthy",
  observations: [
    {
      headerHash,
      stateQueueOutRef,
      stateQueueStatus: "attested",
      slot: 1,
      blockHash: "67".repeat(32),
      finalized: true,
      hasPersistedDecision: true,
    },
  ],
  observedAt: "2026-07-28T00:00:00.000Z",
};

describe("Postgres store instance lock across a Postgres outage", () => {
  it("survives the outage without exiting: refuses effects while Postgres is unreachable, retries, and resumes once it is back", async () => {
    const database = await databases.create();
    const proxy = await outageProxy(new URL(database.url));
    const events: string[] = [];
    const store = await PostgresCommitteeStore.open(proxy.url, {
      onInstanceLockLost: () => events.push("lost"),
      onInstanceLockSuspended: () => events.push("suspended"),
      onInstanceLockRestored: () => events.push("restored"),
    });
    cleanups.push(async () => {
      await proxy.up();
      await store.close();
    });
    const stderr = vi
      .spyOn(process.stderr, "write")
      .mockImplementation(() => true);
    cleanups.push(async () => {
      stderr.mockRestore();
    });

    await proxy.down();
    await vi.waitFor(() => {
      expect(events).toEqual(["suspended"]);
    });
    // Down past the first reconnect attempt, which fails.
    await new Promise((resolve) => setTimeout(resolve, 1_500));
    expect(events).toEqual(["suspended"]);
    await expect(
      store.beginDecisionEffect({ effect, sourceState }),
    ).rejects.toThrow(/suspended its instance lock/u);

    await proxy.up();
    await vi.waitFor(
      () => {
        expect(events).toEqual(["suspended", "restored"]);
      },
      { timeout: 8_000 },
    );
    await store.beginDecisionEffect({ effect, sourceState });
    await store.completeDecisionEffect({
      effectId: effect.effectId,
      expectedAttemptCount: 1,
      status: "reconciled",
      updatedAt: "2026-07-28T01:00:00.000Z",
    });
    await expect(
      store.getDecisionOutbox(effect.effectId),
    ).resolves.toMatchObject({ status: "reconciled", attemptCount: 1 });
    expect(events).not.toContain("lost");
  }, 20_000);
});

describe("Postgres store instance lock after a half-open connection drop", () => {
  it("takes the lock back from its own session the server kept, without being told to stop", async () => {
    const database = await databases.create();
    const proxy = await outageProxy(new URL(database.url));
    const events: string[] = [];
    const store = await PostgresCommitteeStore.open(proxy.url, {
      onInstanceLockLost: () => events.push("lost"),
      onInstanceLockSuspended: () => events.push("suspended"),
      onInstanceLockRestored: () => events.push("restored"),
    });
    cleanups.push(async () => {
      await store.close();
      await proxy.down();
    });
    const stderr = vi
      .spyOn(process.stderr, "write")
      .mockImplementation(() => true);
    cleanups.push(async () => {
      stderr.mockRestore();
    });

    proxy.breakClientSides();
    await vi.waitFor(
      () => {
        expect(events).toEqual(["suspended", "restored"]);
      },
      { timeout: 10_000 },
    );
    await store.beginDecisionEffect({ effect, sourceState });
    await store.completeDecisionEffect({
      effectId: effect.effectId,
      expectedAttemptCount: 1,
      status: "reconciled",
      updatedAt: "2026-07-28T01:00:00.000Z",
    });
    expect(events).toEqual(["suspended", "restored"]);
    // Held again: a second process is still refused.
    await expect(PostgresCommitteeStore.open(database.url)).rejects.toThrow(
      /already exclusively leased/u,
    );
  }, 20_000);
});

describe("committee node startup against a Postgres store that is not ready yet", () => {
  const retrying = (attempt: () => Promise<PostgresCommitteeStore>) => {
    const reasons: string[] = [];
    let onSleep: () => Promise<void> = async () => undefined;
    const started = retryStartup({
      attempt,
      onFailure: (reason) => reasons.push(reason),
      write: () => undefined,
      sleep: () => onSleep(),
    });
    return {
      reasons,
      started,
      onSleep: (run: () => Promise<void>) => {
        onSleep = run;
      },
    };
  };

  it("waits for an unreachable Postgres and opens the store exactly once it answers", async () => {
    const database = await databases.create();
    const proxy = await outageProxy(new URL(database.url));
    await proxy.down();
    let attempts = 0;
    const run = retrying(() => {
      attempts += 1;
      return PostgresCommitteeStore.open(proxy.url);
    });
    run.onSleep(async () => {
      if (run.reasons.length === 2) await proxy.up();
    });
    const store = await run.started;
    cleanups.push(async () => {
      await store.close();
      await proxy.down();
    });

    expect(attempts).toBe(3);
    expect(run.reasons).toHaveLength(2);
    for (const reason of run.reasons) expect(reason).toMatch(/^starting:/u);
    await expect(store.getDeployment()).resolves.toBeUndefined();
  });

  it("waits while another live process holds the store's lock, and takes it once that process is gone", async () => {
    const database = await databases.create();
    const holder = await PostgresCommitteeStore.open(database.url);
    let attempts = 0;
    const run = retrying(() => {
      attempts += 1;
      return PostgresCommitteeStore.open(database.url);
    });
    run.onSleep(async () => {
      if (run.reasons.length === 2) await holder.close();
    });
    const store = await run.started;
    cleanups.push(() => store.close());

    expect(attempts).toBe(3);
    expect(run.reasons).toEqual([
      "starting:store_instance_lock_held",
      "starting:store_instance_lock_held",
    ]);
  });

  it("still refuses a store holding another deployment's state", async () => {
    const database = await databases.create();
    const store = await PostgresCommitteeStore.open(database.url);
    cleanups.push(() => store.close());
    const marker = (manifestId: string) => ({
      marker: makeDeploymentMarker(manifestId),
      manifestSha256: "aa".repeat(32),
      contractDeploymentInfoSha256: "bb".repeat(32),
      manifestRaw: "{}",
    });
    await store.initDeployment(marker("cc".repeat(32)));
    let attempts = 0;
    await expect(
      retryStartup({
        attempt: async () => {
          attempts += 1;
          await store.initDeployment(marker("dd".repeat(32)));
        },
        onFailure: () => undefined,
        write: () => undefined,
        sleep: async () => undefined,
      }),
    ).rejects.toThrow(/stale_deployment_state_requires_fresh_redeploy/u);
    expect(attempts).toBe(1);
  });
});
