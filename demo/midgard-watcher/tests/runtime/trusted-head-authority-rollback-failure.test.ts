import { DatabaseSync } from "node:sqlite";

import { afterEach, expect, it, vi } from "vitest";

import { TrustedHeadAuthorityUnavailableError } from "../../src/runtime/trusted-head-authority.exact-record.js";
import {
  createWatcherTrustedHeadAuthorityClient,
  openWatcherTrustedHeadAuthorityStore,
  startWatcherTrustedHeadAuthorityServer,
} from "../../src/runtime/trusted-head-authority.js";
import { authenticationKey, head } from "./trusted-head-authority.policy.js";
import { sqliteScene } from "./trusted-head-authority.sqlite-fixture.js";

afterEach(() => {
  vi.restoreAllMocks();
});

const secret = "synthetic-rollback-failure-authority-secret";

it("reopens its handle after COMMIT and ROLLBACK both fail, answers transient, and commits the next compare-and-swap", async () => {
  const scene = await sqliteScene();
  const first = head(scene.input.policy, 0, "77"),
    second = head(scene.input.policy, 1, "88");
  expect(
    await scene.store.compareAndSwap({
      expectedTrustedHead: null,
      nextTrustedHead: first,
    }),
  ).toEqual({ committed: true, head: first });

  // Fail the next COMMIT and its ROLLBACK without running either, so the
  // handle is left inside its write transaction, where every BEGIN fails.
  const exec = DatabaseSync.prototype.exec;
  let armed = true;
  const wedged: DatabaseSync[] = [];
  vi.spyOn(DatabaseSync.prototype, "exec").mockImplementation(function (
    this: DatabaseSync,
    sql: string,
  ) {
    if (armed && sql === "COMMIT") {
      wedged.push(this);
      throw new Error("injected COMMIT failure");
    }
    if (armed && sql === "ROLLBACK") {
      armed = false;
      throw new Error("injected ROLLBACK failure");
    }
    exec.call(this, sql);
  });

  const server = await startWatcherTrustedHeadAuthorityServer({
    endpoint: "http://127.0.0.1:0",
    httpSecret: secret,
    unsafeAllowEphemeralPortForTest: true,
    store: scene.store,
  });
  try {
    const cas = await fetch(`${server.endpoint}/v1/trusted-head/cas`, {
      method: "POST",
      headers: {
        authorization: `Bearer ${secret}`,
        "content-type": "application/json",
      },
      body: JSON.stringify({
        expectedTrustedHead: first,
        nextTrustedHead: second,
      }),
    });
    expect(armed).toBe(false);
    expect({ status: cas.status, body: await cas.json() }).toEqual({
      status: 503,
      body: { error: "unavailable" },
    });
    // The handle stuck in the transaction was closed, which discarded it.
    expect(wedged).toHaveLength(1);
    expect(() => wedged[0]!.exec("SELECT 1")).toThrow(/not open/u);

    const client = createWatcherTrustedHeadAuthorityClient({
      endpoint: server.endpoint,
      httpSecret: secret,
      policy: scene.input.policy,
      authenticationKey,
      requestTimeoutMs: 1000,
    });
    expect(await client.readCurrent()).toEqual(first);
    expect(
      await client.compareAndSwap({
        expectedTrustedHead: first,
        nextTrustedHead: second,
      }),
    ).toBe(true);
  } finally {
    await server.close();
  }
  const reader = await openWatcherTrustedHeadAuthorityStore(scene.input);
  try {
    expect(await reader.readCurrent()).toEqual(second);
  } finally {
    reader.close();
    scene.store.close();
  }
});

it("keeps answering transient while the handle cannot be reopened, then recovers", async () => {
  const scene = await sqliteScene();
  const first = head(scene.input.policy, 0, "77");
  const exec = DatabaseSync.prototype.exec;
  let failures = 2;
  let reopenFails = false;
  vi.spyOn(DatabaseSync.prototype, "exec").mockImplementation(function (
    this: DatabaseSync,
    sql: string,
  ) {
    if (failures > 0 && (sql === "COMMIT" || sql === "ROLLBACK")) {
      failures -= 1;
      if (failures === 0) reopenFails = true;
      throw new Error(`injected ${sql} failure`);
    }
    if (reopenFails && sql.startsWith("PRAGMA synchronous=FULL"))
      throw new Error("injected reopen failure");
    exec.call(this, sql);
  });
  await expect(
    scene.store.compareAndSwap({
      expectedTrustedHead: null,
      nextTrustedHead: first,
    }),
  ).rejects.toBeInstanceOf(TrustedHeadAuthorityUnavailableError);
  await expect(scene.store.readCurrent()).rejects.toThrow(
    /could not be reopened/u,
  );
  await expect(scene.store.readCurrent()).rejects.toBeInstanceOf(
    TrustedHeadAuthorityUnavailableError,
  );
  reopenFails = false;
  expect(await scene.store.readCurrent()).toBeNull();
  expect(
    await scene.store.compareAndSwap({
      expectedTrustedHead: null,
      nextTrustedHead: first,
    }),
  ).toEqual({ committed: true, head: first });
  scene.store.close();
});
