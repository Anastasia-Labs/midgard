import { makeDeploymentMarkerV1 } from "@al-ft/midgard-core/deployment-manifest-identity-v1";
import { describe, expect, it } from "vitest";

import {
  makeWatcherFinalityBootstrapStateV1,
  makeWatcherFinalityPolicyV1,
  type WatcherFinalityPolicyV1,
} from "../../src/l1/finality-engine.js";
import {
  initializeWatcherRollbackDurableAuthorityV1,
  type WatcherRollbackDurableTrustedHeadV1,
} from "../../src/l1/rollback-engine.js";
import { WATCHER_CONFIG_SCHEMA_VERSION } from "../../src/runtime/config.js";
import type { WatcherTrustedHeadAuthorityClientV1 } from "../../src/runtime/trusted-head-authority-v1.js";
import type { WatcherDurableAtomicBackend } from "../../src/storage/durable-store.js";
import { makeEmptyWatcherDurableStoreV1 } from "../../src/storage/durable-store.js";
import { createWatcherProductionDurableRuntimeV1 } from "../../src/storage/production-durable-runtime-v1.js";

const h32 = (byte: string) => byte.repeat(32);
const key = Uint8Array.from({ length: 32 }, (_, index) => index + 1);

const policy = (): WatcherFinalityPolicyV1 => {
  const marker = makeDeploymentMarkerV1(h32("11"));
  const result = makeWatcherFinalityPolicyV1(
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
              operatorIdentitySha256: h32("a1"),
              endpoint: "https://provider-a.example",
            },
            {
              identity: "provider-b",
              operatorIdentitySha256: h32("b2"),
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
              "/dns4/da-a.example/tcp/443/p2p/12D3KooWAbcdefghijkmnopqrstuvwxyz12345",
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
      manifestId: marker.manifestId,
      network: "Preprod",
      trustRootId: h32("22"),
      releaseEvidenceDigest: h32("33"),
      ruleBundleCommitment: h32("44"),
      programCommitments: { validation: h32("55") },
      durableMarker: marker,
    },
  );
  if (result === null) throw new Error("fixture policy failed");
  return result;
};

class MemoryBackend implements WatcherDurableAtomicBackend {
  bytes: Uint8Array | null = null;

  async read() {
    return this.bytes === null ? null : Uint8Array.from(this.bytes);
  }

  async compareAndSwap(expectedSha256: string | null, next: Uint8Array) {
    const current = this.bytes;
    const { createHash } = await import("node:crypto");
    const digest = (bytes: Uint8Array) =>
      createHash("sha256").update(bytes).digest("hex");
    if ((current === null ? null : digest(current)) !== expectedSha256) {
      return false;
    }
    this.bytes = Uint8Array.from(next);
    return true;
  }
}

const client = (options: {
  initial?: WatcherRollbackDurableTrustedHeadV1 | null;
  refuseCas?: boolean;
  poisonReadBack?: boolean;
}) => {
  let current = options.initial ?? null;
  let casCount = 0;
  const value: WatcherTrustedHeadAuthorityClientV1 = Object.freeze({
    readRecordAuthenticationKeyId: async () => h32("99"),
    readCurrent: async () =>
      options.poisonReadBack && current !== null
        ? { ...current, revision: (BigInt(current.revision) + 1n).toString() }
        : current,
    compareAndSwap: async ({ expectedTrustedHead, nextTrustedHead }) => {
      casCount += 1;
      if (
        options.refuseCas === true ||
        JSON.stringify(expectedTrustedHead) !== JSON.stringify(current)
      ) {
        return false;
      }
      current = nextTrustedHead;
      return true;
    },
  });
  return { value, current: () => current, casCount: () => casCount };
};

describe("production durable watcher runtime", () => {
  it("publishes and reads back epoch zero before returning an authority", async () => {
    const backend = new MemoryBackend();
    const sidecar = client({});
    const runtime = await createWatcherProductionDurableRuntimeV1({
      backend,
      policy: policy(),
      authenticationKey: key,
      client: sidecar.value,
    });

    expect(sidecar.casCount()).toBe(1);
    expect(sidecar.current()).toMatchObject({ revision: "0" });
    expect(runtime.read()).toMatchObject({
      currentStore: { revision: "0" },
      currentFinalityState: { phase: "unobserved" },
      authenticatedConsistencyHistory: [],
    });
  });

  it("recovers a crash after epoch-zero SQLite commit and before sidecar publication", async () => {
    const backend = new MemoryBackend();
    const finalityPolicy = policy();
    const bootstrap = makeWatcherFinalityBootstrapStateV1(finalityPolicy)!;
    await initializeWatcherRollbackDurableAuthorityV1({
      backend,
      policy: finalityPolicy,
      authenticationKey: key,
      trustedHead: null,
      bootstrapStore: makeEmptyWatcherDurableStoreV1(
        finalityPolicy.deploymentMarker,
      ),
      bootstrapFinalityState: bootstrap,
    });
    const sidecar = client({});

    const runtime = await createWatcherProductionDurableRuntimeV1({
      backend,
      policy: finalityPolicy,
      authenticationKey: key,
      client: sidecar.value,
    });
    expect(sidecar.casCount()).toBe(1);
    expect(runtime.read().currentFinalityState.phase).toBe("unobserved");
  });

  it("fails closed on CAS conflict or a poisoned read-back", async () => {
    await expect(
      createWatcherProductionDurableRuntimeV1({
        backend: new MemoryBackend(),
        policy: policy(),
        authenticationKey: key,
        client: client({ refuseCas: true }).value,
      }),
    ).rejects.toThrow("trusted-head direct-successor CAS conflicted");
    await expect(
      createWatcherProductionDurableRuntimeV1({
        backend: new MemoryBackend(),
        policy: policy(),
        authenticationKey: key,
        client: client({ poisonReadBack: true }).value,
      }),
    ).rejects.toThrow("trusted-head read-back differs");
  });
});
