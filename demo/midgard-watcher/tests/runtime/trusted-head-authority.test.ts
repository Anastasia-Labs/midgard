import { createHash, createHmac } from "node:crypto";
import { mkdtemp, readFile, rename, rm, writeFile } from "node:fs/promises";
import { createServer, type RequestListener } from "node:http";
import { join } from "node:path";

import { makeDeploymentMarker } from "@al-ft/midgard-core/deployment-manifest-identity";
import { afterEach, describe, expect, it } from "vitest";

import {
  makeWatcherFinalityPolicy,
  type WatcherFinalityPolicy,
} from "../../src/l1/finality-engine.js";
import {
  WATCHER_ROLLBACK_DURABLE_TRUSTED_HEAD_SCHEMA_VERSION,
  type WatcherRollbackDurableTrustedHead,
} from "../../src/l1/rollback-engine.js";
import { WATCHER_CONFIG_SCHEMA_VERSION } from "../../src/runtime/config.js";
import {
  createWatcherTrustedHeadAuthorityClient,
  openWatcherTrustedHeadAuthorityStore,
  startWatcherTrustedHeadAuthorityServer,
} from "../../src/runtime/trusted-head-authority.js";
import { watcherCanonicalJson } from "../../src/storage/durable-store.js";

const hex32 = (byte: string): string => byte.repeat(32);
const authenticationKey = Uint8Array.from({ length: 32 }, (_, index) => index);
const recordAuthenticationKey = Uint8Array.from(
  { length: 32 },
  (_, index) => 255 - index,
);

const policy = (): WatcherFinalityPolicy => {
  const value = makeWatcherFinalityPolicy(
    {
      schemaVersion: WATCHER_CONFIG_SCHEMA_VERSION,
      mode: "acceptance",
      targetNetwork: "Preprod",
      l1: {
        source: {
          sourceMode: "external_providers",
          providers: [
            {
              identity: "provider-a",
              operatorIdentitySha256: hex32("11"),
              endpoint: "https://provider-a.example",
            },
            {
              identity: "provider-b",
              operatorIdentitySha256: hex32("22"),
              endpoint: "https://provider-b.example",
            },
          ],
        },
        requestTimeoutMs: 10_000,
        maxConcurrency: 4,
        finality: {
          depth: 30,
          rollback: {
            beforeFinality: "rewind",
            afterFinality: "quarantine",
            maxDepth: 30,
          },
        },
      },
      da: {
        peers: [
          {
            identity: "da-peer-a",
            multiaddr:
              "/dns4/da.example/tcp/443/p2p/12D3KooWAbcdefghijkmnopqrstuvwxyz12345",
          },
        ],
        requestTimeoutMs: 10_000,
        maxConcurrency: 4,
      },
      storage: {
        driver: "sqlite",
        path: "/var/lib/midgard-watcher/watcher.sqlite",
        rollbackAuthorityKeySource: {
          kind: "environment",
          variable: "MIDGARD_WATCHER_ROLLBACK_AUTHORITY_KEY",
        },
      },
      proverWallet: {
        keySource: {
          kind: "environment",
          variable: "MIDGARD_WATCHER_PROVER_KEY",
        },
      },
      deadlines: {
        daFetchMs: 60_000,
        daPublishMs: 60_000,
        proofConstructMs: 300_000,
        proofSubmitMs: 120_000,
      },
    },
    {
      manifestId: hex32("33"),
      network: "Preprod",
      trustRootId: hex32("44"),
      fundingProfileBundleDigest: "ab".repeat(32),
      blueprintHash: hex32("55"),
      ruleBundleCommitment: hex32("66"),
      programCommitments: { validation: hex32("77") },
      durableMarker: makeDeploymentMarker(hex32("33")),
    },
  );
  if (value === null) throw new Error("test finality policy was rejected");
  return value;
};

const head = (
  finalityPolicy: WatcherFinalityPolicy,
  revision: number,
  byte: string,
): WatcherRollbackDurableTrustedHead => {
  const canonical = {
    schemaVersion: WATCHER_ROLLBACK_DURABLE_TRUSTED_HEAD_SCHEMA_VERSION,
    policyDigest: finalityPolicy.policyDigest,
    deploymentMarker: finalityPolicy.deploymentMarker,
    authenticationKeyId: createHash("sha256")
      .update(authenticationKey)
      .digest("hex"),
    revision: revision.toString(),
    snapshotSha256: hex32(byte),
    authorityDigest: hex32(
      (Number.parseInt(byte, 16) + 1).toString(16).padStart(2, "0"),
    ),
  };
  return Object.freeze({
    ...canonical,
    headMac: createHmac("sha256", authenticationKey)
      .update(
        `${WATCHER_ROLLBACK_DURABLE_TRUSTED_HEAD_SCHEMA_VERSION}:${watcherCanonicalJson(canonical)}`,
        "utf8",
      )
      .digest("hex"),
  });
};

const directories: string[] = [];
const directory = async (): Promise<string> => {
  const value = await mkdtemp("/var/tmp/midgard-trusted-head-");
  directories.push(value);
  return value;
};

afterEach(async () => {
  await Promise.all(
    directories
      .splice(0)
      .map(async (path) => await rm(path, { recursive: true })),
  );
});

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

describe("trusted-head idempotent read transport recovery", () => {
  const withServer = async (
    listener: RequestListener,
    run: (endpoint: string) => Promise<void>,
  ) => {
    const server = createServer(listener);
    await new Promise<void>((resolve) =>
      server.listen(0, "127.0.0.1", resolve),
    );
    const address = server.address();
    if (address === null || typeof address === "string")
      throw new Error("test authority requires TCP");
    try {
      await run(`http://127.0.0.1:${address.port}`);
    } finally {
      server.closeAllConnections();
      await new Promise<void>((resolve, reject) =>
        server.close((error) => (error ? reject(error) : resolve())),
      );
    }
  };
  const client = (endpoint: string, requestTimeoutMs = 2_000) =>
    createWatcherTrustedHeadAuthorityClient({
      endpoint,
      httpSecret: "authority-http-secret-with-sufficient-entropy",
      policy: policy(),
      authenticationKey,
      requestTimeoutMs,
    });

  it("retries a closed socket for each GET and still authenticates the recovered head", async () => {
    const requests = new Map<string, number>();
    const finality = policy();
    const expected = head(finality, 0, "10");
    await withServer(
      (request, response) => {
        const path = request.url!;
        const count = (requests.get(path) ?? 0) + 1;
        requests.set(path, count);
        if (count === 1) {
          request.socket.destroy();
          return;
        }
        response.setHeader("content-type", "application/json");
        response.end(
          JSON.stringify(
            path === "/v1/identity"
              ? { recordAuthenticationKeyId: hex32("12") }
              : { head: expected },
          ),
        );
      },
      async (endpoint) => {
        const authority = client(endpoint);
        expect(await authority.readRecordAuthenticationKeyId()).toBe(
          hex32("12"),
        );
        expect(await authority.readCurrent()).toEqual(expected);
        expect([...requests.values()]).toEqual([2, 2]);
      },
    );
  });

  it("retries transport loss while reading a successful GET response body", async () => {
    let requests = 0;
    await withServer(
      (request, response) => {
        requests += 1;
        if (requests === 1) {
          response.writeHead(200, {
            "content-type": "application/json",
            "content-length": "100",
          });
          response.write('{"head":');
          setImmediate(() => request.socket.destroy());
          return;
        }
        response.end(JSON.stringify({ head: null }));
      },
      async (endpoint) => {
        expect(await client(endpoint).readCurrent()).toBeNull();
        expect(requests).toBe(2);
      },
    );
  });

  it("bounds persistent socket loss to three attempts under the original read deadline", async () => {
    let requests = 0;
    await withServer(
      (request) => {
        requests += 1;
        request.socket.destroy();
      },
      async (endpoint) => {
        await expect(client(endpoint).readCurrent()).rejects.toThrow(
          "fetch failed",
        );
        expect(requests).toBe(3);
        requests = 0;
        const started = performance.now();
        await expect(client(endpoint, 60).readCurrent()).rejects.toThrow();
        expect(requests).toBe(1);
        expect(performance.now() - started).toBeLessThan(500);
      },
    );
  });

  it.each<[number, string]>([
    [401, JSON.stringify({ error: "unauthorized" })],
    [500, JSON.stringify({ error: "persistence_failure" })],
    [200, "not json"],
    [200, JSON.stringify({ head: null, extra: true })],
    [
      200,
      JSON.stringify({
        head: { ...head(policy(), 0, "10"), headMac: hex32("00") },
      }),
    ],
  ])(
    "does not retry HTTP or invalid authority responses (%s, %s)",
    async (status, body) => {
      let requests = 0;
      await withServer(
        (_request, response) => {
          requests += 1;
          response.writeHead(status, { "content-type": "application/json" });
          response.end(body);
        },
        async (endpoint) => {
          await expect(client(endpoint).readCurrent()).rejects.toThrow();
          expect(requests).toBe(1);
        },
      );
    },
  );

  it("preserves the existing 409 response validation without retrying it", async () => {
    let requests = 0;
    await withServer(
      (_request, response) => {
        requests += 1;
        response.writeHead(409, { "content-type": "application/json" });
        response.end(JSON.stringify({ head: null }));
      },
      async (endpoint) => {
        expect(await client(endpoint).readCurrent()).toBeNull();
        expect(requests).toBe(1);
      },
    );
  });

  it("never retries an ambiguous CAS whose response connection was lost", async () => {
    let writes = 0;
    await withServer(
      (request) => {
        if (request.method === "POST") writes += 1;
        request.socket.destroy();
      },
      async (endpoint) => {
        await expect(
          client(endpoint).compareAndSwap({
            expectedTrustedHead: null,
            nextTrustedHead: head(policy(), 0, "10"),
          }),
        ).rejects.toThrow("fetch failed");
        expect(writes).toBe(1);
      },
    );
  });
});
