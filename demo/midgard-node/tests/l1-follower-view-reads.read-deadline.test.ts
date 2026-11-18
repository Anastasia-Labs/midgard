/**
 * The node's reads that check the follower's view take `FOR SHARE`, so
 * Postgres runs them in a write transaction. They are reads all the same:
 * on a store connection the server never answers, each ends at the store's
 * read deadline and gives the connection up, as a plain read does. The
 * server's silence is a frozen TCP proxy in front of a test database.
 */
import {
  type FactStore,
  type IntentState,
  openPostgresFactStore,
} from "@al-ft/midgard-l1-follower";
import {
  SIM_ORIGIN,
  simStoreOptions,
} from "@al-ft/midgard-l1-follower/testing";
import { Effect } from "effect";
import { afterAll, describe, expect, it } from "vitest";

import { readLandedStateQueueFrom } from "../src/l1-state-queue/hook.js";
import { readQueueHistory } from "../src/landed-blocks/history.js";
import { nodeFamilyPredicate } from "../src/services/l1-follower.intent-predicates.js";
import { freezablePostgresProxy } from "./helpers/freezable-postgres-proxy.js";
import { testDatabases } from "./helpers/l1-events-store.js";
import { SIM_QUEUE_CONFIG } from "./helpers/state-queue-sim.fixtures.js";

const databases = testDatabases();

afterAll(async () => {
  await databases.dropAll();
});

const READ_DEADLINE_MS = 500;
const DEADLINE_PASSED = `the read had no answer within ${String(READ_DEADLINE_MS)} ms`;

/** `promise`'s outcome, or a failure naming `what` after `ms`. */
const within = <T>(what: string, ms: number, promise: Promise<T>) =>
  new Promise<T>((resolve, reject) => {
    const timer = setTimeout(
      () => reject(new Error(`${what} did not settle within ${String(ms)} ms`)),
      ms,
    );
    promise.then(
      (value) => {
        clearTimeout(timer);
        resolve(value);
      },
      (error: unknown) => {
        clearTimeout(timer);
        reject(error as Error);
      },
    );
  });

/**
 * Runs `read` against a store whose one connection is open and then goes
 * silent: how the read ended, and how the store's next read did.
 */
const onSilentConnection = async <T>(
  read: (store: FactStore) => Promise<T>,
): Promise<{ outcome: unknown; next: unknown }> => {
  const proxy = await freezablePostgresProxy(await databases.create());
  const store = openPostgresFactStore({
    ...simStoreOptions([], 6, "postgres"),
    connection: {
      connectionString: proxy.url,
      maxConnections: 1,
      readDeadlineMs: READ_DEADLINE_MS,
    },
  });
  const ping = () =>
    store.transaction("read", (tx) => tx.query("SELECT 1 AS one"));
  try {
    await ping();
    proxy.freeze();
    const outcome = await within("the read", 5_000, read(store)).then(
      () => new Error("the read answered on a silent connection"),
      (error: unknown) => error,
    );
    const next = await within(
      "the read after the deadline",
      5_000,
      ping(),
    ).then(
      () =>
        proxy.accepted() === 2 ? "answered on a fresh connection" : "answered",
      (error: unknown) => error,
    );
    return { outcome, next };
  } finally {
    await within("closing the store", 5_000, store.close()).catch(
      () => undefined,
    );
    await proxy.close();
  }
};

const VIEW = { generation: 0, point: SIM_ORIGIN.point, height: 0 };

describe("the node's view-checked reads on a store connection that goes silent", () => {
  it("ends a family predicate's read at the store's read deadline", async () => {
    const { outcome, next } = await onSilentConnection((store) =>
      nodeFamilyPredicate({
        store,
        stateQueue: SIM_QUEUE_CONFIG,
        operatorSet: null,
        slotToPosixMs: (slot) => slot * 1_000,
        commitEventDepth: 1,
      })({ intent: { family: "commit" } } as unknown as IntentState),
    );
    expect(outcome).toHaveProperty("message", DEADLINE_PASSED);
    expect(next).toBe("answered on a fresh connection");
  });

  it("ends the landed state queue read at the store's read deadline", async () => {
    const { outcome, next } = await onSilentConnection((store) =>
      readLandedStateQueueFrom(store, SIM_QUEUE_CONFIG, VIEW),
    );
    expect(outcome).toHaveProperty("message", DEADLINE_PASSED);
    expect(next).toBe("answered on a fresh connection");
  });

  it("ends the queue history read at the store's read deadline", async () => {
    const { outcome, next } = await onSilentConnection((store) =>
      Effect.runPromise(
        Effect.flip(readQueueHistory(store, SIM_QUEUE_CONFIG, VIEW)),
      ).then((error) => Promise.reject(error)),
    );
    expect(outcome).toHaveProperty("cause.message", DEADLINE_PASSED);
    expect(next).toBe("answered on a fresh connection");
  });
});
