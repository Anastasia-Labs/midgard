import { afterAll, describe, expect, it } from "vitest";

import { openPostgresFactStore } from "../src/index.js";
import { simStoreOptions } from "../src/testing/index.js";
import { FIXTURE_PROJECTION, SIM_K } from "./support/fork-sim.js";
import { freezableProxy, testDatabases } from "./support/postgres.js";

/**
 * A store read given an abort signal ends at the abort though the server
 * never answers it, and gives up its connection with it: on a pool of one
 * connection, the next read gets a fresh one and answers. A read waiting
 * for a free connection ends at its abort too. The server's silence is a
 * frozen TCP proxy in front of the test cluster.
 */
const databases = testDatabases();

afterAll(async () => {
  await databases.dropAll();
});

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
        reject(error);
      },
    );
  });

const pause = (ms: number): Promise<void> =>
  new Promise((resolve) => setTimeout(resolve, ms));

describe("store reads on Postgres: an aborted read ends", () => {
  it("ends a read the server never answers at its abort, and frees its connection for the next read", async () => {
    const database = await databases.create();
    const storeOptions = simStoreOptions(
      [FIXTURE_PROJECTION],
      SIM_K,
      "postgres",
    );
    // The writer creates the schema; the reader goes through the proxy on
    // a pool of one connection.
    const writer = openPostgresFactStore({
      ...storeOptions,
      connection: { connectionString: database.url, maxConnections: 1 },
    });
    const proxy = await freezableProxy(database);
    const reader = openPostgresFactStore({
      ...storeOptions,
      connection: { connectionString: proxy.url, maxConnections: 1 },
    });
    try {
      expect((await writer.start()).kind).toBe("ready");
      expect(await reader.cursor()).toBeNull();
      expect(proxy.accepted()).toBe(1);

      // The reader's one connection now hears nothing from the server.
      proxy.freeze();
      const stuck = new AbortController();
      let stuckSettled = false;
      const stuckRead = reader.cursor({ signal: stuck.signal });
      void stuckRead.then(
        () => {
          stuckSettled = true;
        },
        () => {
          stuckSettled = true;
        },
      );
      // A second read waits for the pool's one connection.
      const waiting = new AbortController();
      const waitingRead = reader.cursor({ signal: waiting.signal });
      await pause(200);
      expect(stuckSettled).toBe(false);

      waiting.abort(new Error("the waiting read's deadline passed"));
      await expect(
        within("the read waiting for a connection", 5_000, waitingRead),
      ).rejects.toThrow("the waiting read's deadline passed");

      stuck.abort(new Error("the stuck read's deadline passed"));
      await expect(
        within("the read the server never answers", 5_000, stuckRead),
      ).rejects.toThrow("the stuck read's deadline passed");

      // The stuck connection went with its read: the next read opens a
      // fresh one and answers.
      expect(
        await within("the read after the abort", 5_000, reader.cursor()),
      ).toBeNull();
      expect(proxy.accepted()).toBe(2);
    } finally {
      await within("closing the reader", 5_000, reader.close()).catch(
        () => undefined,
      );
      await writer.close();
      await proxy.close();
    }
  });
});
