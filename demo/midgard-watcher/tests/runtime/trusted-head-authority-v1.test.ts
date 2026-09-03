import { createHash, createHmac } from "node:crypto";
import { mkdtemp, readFile, rename, rm, writeFile } from "node:fs/promises";
import { join } from "node:path";

import { makeDeploymentMarkerV1 } from "@al-ft/midgard-core/deployment-manifest-identity-v1";
import { afterEach, describe, expect, it } from "vitest";

import {
  makeWatcherFinalityPolicyV1,
  type WatcherFinalityPolicyV1,
} from "../../src/l1/finality-engine.js";
import {
  WATCHER_ROLLBACK_DURABLE_TRUSTED_HEAD_V1_SCHEMA_VERSION,
  type WatcherRollbackDurableTrustedHeadV1,
} from "../../src/l1/rollback-engine.js";
import { WATCHER_CONFIG_SCHEMA_VERSION } from "../../src/runtime/config.js";
import {
  createWatcherTrustedHeadAuthorityClientV1,
  openWatcherTrustedHeadAuthorityStoreV1,
  startWatcherTrustedHeadAuthorityServerV1,
} from "../../src/runtime/trusted-head-authority-v1.js";
import { watcherCanonicalJsonV1 } from "../../src/storage/durable-store.js";

const hex32 = (byte: string): string => byte.repeat(32);
const authenticationKey = Uint8Array.from({ length: 32 }, (_, index) => index);
const recordAuthenticationKey = Uint8Array.from(
  { length: 32 },
  (_, index) => 255 - index,
);

const policy = (): WatcherFinalityPolicyV1 => {
  const value = makeWatcherFinalityPolicyV1(
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
      releaseEvidenceDigest: hex32("55"),
      ruleBundleCommitment: hex32("66"),
      programCommitments: { validation: hex32("77") },
      durableMarker: makeDeploymentMarkerV1(hex32("33")),
    },
  );
  if (value === null) throw new Error("test finality policy was rejected");
  return value;
};

const head = (
  finalityPolicy: WatcherFinalityPolicyV1,
  revision: number,
  byte: string,
): WatcherRollbackDurableTrustedHeadV1 => {
  const canonical = {
    schemaVersion: WATCHER_ROLLBACK_DURABLE_TRUSTED_HEAD_V1_SCHEMA_VERSION,
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
        `${WATCHER_ROLLBACK_DURABLE_TRUSTED_HEAD_V1_SCHEMA_VERSION}:${watcherCanonicalJsonV1(canonical)}`,
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
    const store = await openWatcherTrustedHeadAuthorityStoreV1({
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

    const restarted = await openWatcherTrustedHeadAuthorityStoreV1({
      directory: path,
      policy: finalityPolicy,
      recordAuthenticationKey,
    });
    expect(await restarted.readCurrent()).toEqual(second);
  });

  it("fails closed on forged, skipped, foreign-key and unknown directory records", async () => {
    const finalityPolicy = policy();
    const path = await directory();
    const store = await openWatcherTrustedHeadAuthorityStoreV1({
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
      openWatcherTrustedHeadAuthorityStoreV1({
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
      const store = await openWatcherTrustedHeadAuthorityStoreV1({
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
      openWatcherTrustedHeadAuthorityStoreV1({
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
    await writeFile(middlePath, watcherCanonicalJsonV1(middleRecord), "utf8");
    await expect(
      openWatcherTrustedHeadAuthorityStoreV1({
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
      openWatcherTrustedHeadAuthorityStoreV1({
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
      openWatcherTrustedHeadAuthorityStoreV1({
        directory: gap.path,
        policy: finalityPolicy,
        recordAuthenticationKey,
      }),
    ).rejects.toThrow("gap");
  });

  it("exposes only authenticated loopback read and expected-prior CAS with read-back", async () => {
    const finalityPolicy = policy();
    const store = await openWatcherTrustedHeadAuthorityStoreV1({
      directory: await directory(),
      policy: finalityPolicy,
      recordAuthenticationKey,
    });
    const server = await startWatcherTrustedHeadAuthorityServerV1({
      endpoint: "http://127.0.0.1:0",
      httpSecret: "authority-http-secret-with-sufficient-entropy",
      store,
      unsafeAllowEphemeralPortForTest: true,
    });
    try {
      const client = createWatcherTrustedHeadAuthorityClientV1({
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
          body: watcherCanonicalJsonV1({
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
    const server = await startWatcherTrustedHeadAuthorityServerV1({
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
