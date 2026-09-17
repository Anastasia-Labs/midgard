import { execFileSync } from "node:child_process";
import { createHash, createHmac } from "node:crypto";
import { mkdtemp, readdir, rm } from "node:fs/promises";

import { makeDeploymentMarker } from "@al-ft/midgard-core/deployment-manifest-identity";
import { describe, expect, it } from "vitest";

import {
  makeWatcherFinalityPolicy,
  type WatcherFinalityPolicy,
} from "../../src/l1/finality-engine.js";
import {
  WATCHER_ROLLBACK_DURABLE_TRUSTED_HEAD_SCHEMA_VERSION,
  type WatcherRollbackDurableTrustedHead,
} from "../../src/l1/rollback-engine.js";
import { WATCHER_CONFIG_SCHEMA_VERSION } from "../../src/runtime/config.js";
import { createWatcherTrustedHeadAuthorityClient } from "../../src/runtime/trusted-head-authority.js";
import { watcherCanonicalJson } from "../../src/storage/durable-store.js";
import { startWatcherTrustedHeadAuthorityChildForTest } from "../support/trusted-head-process-fixture.js";

const hex32 = (byte: string): string => byte.repeat(32);
const authenticationKey = Uint8Array.from({ length: 32 }, (_, index) => index);

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

const inputFor = (
  directory: string,
  finalityPolicy: WatcherFinalityPolicy,
) => ({
  config: {
    schemaVersion:
      "midgard-watcher-trusted-head-authority-process-config-v1" as const,
    directory,
    endpoint: "http://127.0.0.1:0",
    policy: finalityPolicy,
    recordAuthenticationKeySource: {
      kind: "environment" as const,
      variable: "MIDGARD_TEST_RECORD_KEY",
    },
    httpBearerSecretSource: {
      kind: "environment" as const,
      variable: "MIDGARD_TEST_HTTP_KEY",
    },
  },
  unsafeEnvironmentForTest: {
    MIDGARD_TEST_RECORD_KEY: hex32("5c"),
    MIDGARD_TEST_HTTP_KEY: hex32("39"),
  },
  unsafeAllowEphemeralPortForTest: true as const,
});

describe("trusted-head authority child process fixture", () => {
  it("serves authenticated HTTP and preserves the append-only head across child restart", async () => {
    const directory = await mkdtemp("/var/tmp/midgard-trusted-head-child-");
    const finalityPolicy = policy();
    const input = inputFor(directory, finalityPolicy);
    let child:
      | Awaited<ReturnType<typeof startWatcherTrustedHeadAuthorityChildForTest>>
      | undefined;
    try {
      child = await startWatcherTrustedHeadAuthorityChildForTest(input);
      expect(child.processId).not.toBe(process.pid);
      const firstProcessId = child.processId;
      const clientFor = (endpoint: string) =>
        createWatcherTrustedHeadAuthorityClient({
          endpoint,
          httpSecret: hex32("39"),
          policy: finalityPolicy,
          authenticationKey,
          requestTimeoutMs: 1_000,
        });
      const client = clientFor(child.server.endpoint);
      expect(await client.readCurrent()).toBeNull();
      const first = head(finalityPolicy, 0, "10");
      expect(
        await client.compareAndSwap({
          expectedTrustedHead: null,
          nextTrustedHead: first,
        }),
      ).toBe(true);
      expect(await client.readCurrent()).toEqual(first);
      // Synchronous waiting blocks this process's event loop throughout the HTTP
      // request. The actual authority must still answer from its own process.
      const isolatedResponse = execFileSync(
        process.execPath,
        [
          "--input-type=module",
          "-e",
          `
        import { readFileSync } from "node:fs";
        const { endpoint, secret } = JSON.parse(readFileSync(0, "utf8"));
        const response = await fetch(endpoint + "/v1/trusted-head", {
          headers: { authorization: "Bearer " + secret }, signal: AbortSignal.timeout(1000),
        });
        if (!response.ok) throw new Error("authority HTTP read failed");
        process.stdout.write(await response.text());
      `,
        ],
        {
          input: JSON.stringify({
            endpoint: child.server.endpoint,
            secret: hex32("39"),
          }),
          timeout: 5_000,
          encoding: "utf8",
        },
      );
      expect(JSON.parse(isolatedResponse)).toEqual({ head: first });
      await child.close();
      await child.close();
      expect(
        (await readdir(directory)).filter((name) => name.endsWith(".json")),
      ).toHaveLength(1);
      child = await startWatcherTrustedHeadAuthorityChildForTest(input);
      expect(child.processId).not.toBe(firstProcessId);
      const restarted = clientFor(child.server.endpoint);
      expect(await restarted.readCurrent()).toEqual(first);
      expect(
        await restarted.compareAndSwap({
          expectedTrustedHead: null,
          nextTrustedHead: first,
        }),
      ).toBe(false);
    } finally {
      await child?.close();
      await rm(directory, { recursive: true, force: true });
    }
  });

  it("rejects child startup failure and closes its process", async () => {
    const directory = await mkdtemp("/var/tmp/midgard-trusted-head-child-");
    const input = inputFor(directory, policy());
    try {
      await expect(
        startWatcherTrustedHeadAuthorityChildForTest({
          ...input,
          unsafeEnvironmentForTest: {
            ...input.unsafeEnvironmentForTest,
            MIDGARD_TEST_RECORD_KEY: "invalid",
          },
        }),
      ).rejects.toThrow("trusted-head child startup failed");
    } finally {
      await rm(directory, { recursive: true, force: true });
    }
  });
});
