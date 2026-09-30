import { createHash } from "node:crypto";
import { readFile, rename, rm, writeFile } from "node:fs/promises";
import { join } from "node:path";

import { describe, expect, it } from "vitest";

import { type WatcherRollbackDurableTrustedHead } from "../../src/l1/rollback-engine.js";
import {
  createWatcherTrustedHeadAuthorityClient,
  openWatcherTrustedHeadAuthorityStore,
  startWatcherTrustedHeadAuthorityServer,
} from "../../src/runtime/trusted-head-authority.js";
import { watcherCanonicalJson } from "../../src/storage/durable-store.js";
import {
  authenticationKey,
  directory,
  head,
  hex32,
  policy,
  recordAuthenticationKey,
} from "./trusted-head-authority.policy.js";

describe("independent monotonic watcher trusted-head authority", () => {
  it("persists one authenticated contiguous chain and rejects stale/concurrent writes", async () => {
    const finalityPolicy = policy();
    const path = await directory();
    const store = await openWatcherTrustedHeadAuthorityStore({
      directory: path,
      policy: finalityPolicy,
      recordAuthenticationKey,
    });
    const first = head(finalityPolicy, 0, "10");
    const second = head(finalityPolicy, 1, "20");

    expect(
      await store.compareAndSwap({
        expectedTrustedHead: null,
        nextTrustedHead: first,
      }),
    ).toBe(true);
    expect(
      await store.compareAndSwap({
        expectedTrustedHead: null,
        nextTrustedHead: first,
      }),
    ).toBe(false);
    expect(
      await Promise.all([
        store.compareAndSwap({
          expectedTrustedHead: first,
          nextTrustedHead: second,
        }),
        store.compareAndSwap({
          expectedTrustedHead: first,
          nextTrustedHead: second,
        }),
      ]),
    ).toEqual(expect.arrayContaining([true, false]));
    expect(await store.readCurrent()).toEqual(second);

    const restarted = await openWatcherTrustedHeadAuthorityStore({
      directory: path,
      policy: finalityPolicy,
      recordAuthenticationKey,
    });
    expect(await restarted.readCurrent()).toEqual(second);
  });

  it("fails closed on forged, skipped, foreign-key and unknown directory records", async () => {
    const finalityPolicy = policy();
    const path = await directory();
    const store = await openWatcherTrustedHeadAuthorityStore({
      directory: path,
      policy: finalityPolicy,
      recordAuthenticationKey,
    });
    const first = head(finalityPolicy, 0, "10");
    expect(
      await store.compareAndSwap({
        expectedTrustedHead: null,
        nextTrustedHead: head(finalityPolicy, 2, "30"),
      }),
    ).toBe(false);
    expect(
      await store.compareAndSwap({
        expectedTrustedHead: null,
        nextTrustedHead: first,
      }),
    ).toBe(true);
    await writeFile(join(path, "operator-note"), "not authority", "utf8");
    await expect(
      openWatcherTrustedHeadAuthorityStore({
        directory: path,
        policy: finalityPolicy,
        recordAuthenticationKey,
      }),
    ).rejects.toThrow("unknown entry");
  });

  it("detects valid-watcher-head branch substitution, wrong sidecar key, gaps and truncation", async () => {
    const finalityPolicy = policy();
    const makeChain = async () => {
      const path = await directory();
      const store = await openWatcherTrustedHeadAuthorityStore({
        directory: path,
        policy: finalityPolicy,
        recordAuthenticationKey,
      });
      const first = head(finalityPolicy, 0, "10");
      const second = head(finalityPolicy, 1, "20");
      const third = head(finalityPolicy, 2, "30");
      expect(
        await store.compareAndSwap({
          expectedTrustedHead: null,
          nextTrustedHead: first,
        }),
      ).toBe(true);
      expect(
        await store.compareAndSwap({
          expectedTrustedHead: first,
          nextTrustedHead: second,
        }),
      ).toBe(true);
      expect(
        await store.compareAndSwap({
          expectedTrustedHead: second,
          nextTrustedHead: third,
        }),
      ).toBe(true);
      return { path, first, second, third };
    };

    const wrongKey = await makeChain();
    await expect(
      openWatcherTrustedHeadAuthorityStore({
        directory: wrongKey.path,
        policy: finalityPolicy,
        recordAuthenticationKey: Uint8Array.from(
          { length: 32 },
          (_, index) => (index + 97) % 256,
        ),
      }),
    ).rejects.toThrow("sidecar record is invalid");

    const middle = await makeChain();
    const middlePath = join(middle.path, "00000000000000000001.json");
    const middleRecord = JSON.parse(await readFile(middlePath, "utf8")) as {
      head: unknown;
    };
    middleRecord.head = head(finalityPolicy, 1, "a0");
    await writeFile(middlePath, watcherCanonicalJson(middleRecord), "utf8");
    await expect(
      openWatcherTrustedHeadAuthorityStore({
        directory: middle.path,
        policy: finalityPolicy,
        recordAuthenticationKey,
      }),
    ).rejects.toThrow("sidecar record MAC");

    const tail = await makeChain();
    const tailPath = join(tail.path, "00000000000000000002.json");
    const tailBytes = await readFile(tailPath, "utf8");
    await writeFile(tailPath, tailBytes.slice(0, -1), "utf8");
    await expect(
      openWatcherTrustedHeadAuthorityStore({
        directory: tail.path,
        policy: finalityPolicy,
        recordAuthenticationKey,
      }),
    ).rejects.toThrow("malformed");

    const gap = await makeChain();
    await rename(
      join(gap.path, "00000000000000000001.json"),
      join(gap.path, "00000000000000000004.json"),
    );
    await expect(
      openWatcherTrustedHeadAuthorityStore({
        directory: gap.path,
        policy: finalityPolicy,
        recordAuthenticationKey,
      }),
    ).rejects.toThrow("gap");
  });

  it("rechecks historical records across read batches on the same open store", async () => {
    const finalityPolicy = policy();
    const path = await directory();
    const store = await openWatcherTrustedHeadAuthorityStore({
      directory: path,
      policy: finalityPolicy,
      recordAuthenticationKey,
    });
    let current: WatcherRollbackDurableTrustedHead | null = null;
    for (let index = 0; index < 12; index++) {
      const next = head(finalityPolicy, index, "10");
      expect(
        await store.compareAndSwap({
          expectedTrustedHead: current,
          nextTrustedHead: next,
        }),
      ).toBe(true);
      current = next;
    }
    expect(await store.readCurrent()).toEqual(current);
    const historicalPath = join(path, "00000000000000000008.json");
    const original = await readFile(historicalPath, "utf8");
    const forged = JSON.parse(original) as { head: { headMac: string } };
    forged.head.headMac = hex32("ee");
    const mutations = [
      [watcherCanonicalJson(forged), /sidecar record MAC/u],
      [`${original}\n`, /non-canonical/u],
      [
        await readFile(join(path, "00000000000000000000.json"), "utf8"),
        /non-canonical/u,
      ],
      ["", /record size/u],
      [original.slice(0, -1), /malformed/u],
    ] as const;
    for (const [bytes, failure] of mutations) {
      await writeFile(historicalPath, bytes, "utf8");
      await expect(store.readCurrent()).rejects.toThrow(failure);
      await writeFile(historicalPath, original, "utf8");
      expect(await store.readCurrent()).toEqual(current);
    }
    const moved = join(path, "00000000000000000013.json");
    await rename(historicalPath, moved);
    await expect(store.readCurrent()).rejects.toThrow("gap");
    await rename(moved, historicalPath);
    const unknown = join(path, "operator-note");
    await writeFile(unknown, "not authority", "utf8");
    await expect(store.readCurrent()).rejects.toThrow("unknown entry");
    await rm(unknown);
    expect(await store.readCurrent()).toEqual(current);
  });

  it("exposes only authenticated loopback read and expected-prior CAS with read-back", async () => {
    const finalityPolicy = policy();
    const store = await openWatcherTrustedHeadAuthorityStore({
      directory: await directory(),
      policy: finalityPolicy,
      recordAuthenticationKey,
    });
    const server = await startWatcherTrustedHeadAuthorityServer({
      endpoint: "http://127.0.0.1:0",
      httpSecret: "authority-http-secret-with-sufficient-entropy",
      store,
      unsafeAllowEphemeralPortForTest: true,
    });
    try {
      const client = createWatcherTrustedHeadAuthorityClient({
        endpoint: server.endpoint,
        httpSecret: "authority-http-secret-with-sufficient-entropy",
        policy: finalityPolicy,
        authenticationKey,
        requestTimeoutMs: 2_000,
      });
      const first = head(finalityPolicy, 0, "10");
      expect(await client.readRecordAuthenticationKeyId()).toBe(
        createHash("sha256").update(recordAuthenticationKey).digest("hex"),
      );
      expect(await client.readCurrent()).toBeNull();
      expect(
        await client.compareAndSwap({
          expectedTrustedHead: null,
          nextTrustedHead: first,
        }),
      ).toBe(true);
      expect(await client.readCurrent()).toEqual(first);
      const poisoned = {
        ...head(finalityPolicy, 1, "20"),
        headMac: hex32("ff"),
      };
      const poisonedResponse = await fetch(
        `${server.endpoint}/v1/trusted-head/cas`,
        {
          method: "POST",
          headers: {
            authorization:
              "Bearer authority-http-secret-with-sufficient-entropy",
            "content-type": "application/json",
          },
          body: watcherCanonicalJson({
            expectedTrustedHead: first,
            nextTrustedHead: poisoned,
          }),
        },
      );
      expect(poisonedResponse.status).toBe(200);
      await expect(client.readCurrent()).rejects.toThrow("invalid head");
      await expect(
        fetch(`${server.endpoint}/v1/trusted-head`, {
          headers: { authorization: "Bearer wrong-secret-never-authorized" },
        }),
      ).resolves.toMatchObject({ status: 401 });
      await expect(
        fetch(`${server.endpoint}/v1/trusted-head`, {
          method: "DELETE",
          headers: {
            authorization:
              "Bearer authority-http-secret-with-sufficient-entropy",
          },
        }),
      ).resolves.toMatchObject({ status: 404 });
    } finally {
      await server.close();
    }
  });

  it("reports persistence failures as 500 without returning internal details", async () => {
    const server = await startWatcherTrustedHeadAuthorityServer({
      endpoint: "http://127.0.0.1:0",
      httpSecret: "authority-http-secret-with-sufficient-entropy",
      store: {
        readRecordAuthenticationKeyId: async () => hex32("99"),
        readCurrent: async () => {
          throw new Error("sensitive filesystem path and cause");
        },
        compareAndSwap: async () => false,
      },
      unsafeAllowEphemeralPortForTest: true,
    });
    try {
      const response = await fetch(`${server.endpoint}/v1/trusted-head`, {
        headers: {
          authorization: "Bearer authority-http-secret-with-sufficient-entropy",
        },
      });
      expect(response.status).toBe(500);
      expect(await response.json()).toEqual({ error: "persistence_failure" });
    } finally {
      await server.close();
    }
  });
});
