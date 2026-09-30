import { makeDeploymentMarker } from "@al-ft/midgard-core/deployment-manifest-identity";
import { h32 } from "@al-ft/midgard-test-support/hex";

import {
  makeWatcherFinalityPolicy,
  type WatcherFinalityPolicy,
} from "../../src/l1/finality-engine.js";
import { type WatcherRollbackDurableTrustedHead } from "../../src/l1/rollback-engine.js";
import { WATCHER_CONFIG_SCHEMA_VERSION } from "../../src/runtime/config.js";
import type { WatcherTrustedHeadAuthorityClient } from "../../src/runtime/trusted-head-authority.js";
import {
  createWatcherDurableRuntime,
  persistWatcherUserEventCheckpoint,
} from "../../src/storage/durable-runtime.js";
import type { WatcherDurableAtomicBackend } from "../../src/storage/durable-store.js";
import {
  makeWatcherUserEventCheckpoint,
  WATCHER_USER_EVENT_CHECKPOINT_SCHEMA_VERSION,
  watcherUserEventArchiveDigest,
} from "../../src/storage/user-event-checkpoint.js";

export const key = Uint8Array.from({ length: 32 }, (_, index) => index + 1);

export const policy = (): WatcherFinalityPolicy => {
  const marker = makeDeploymentMarker(h32(0x11));
  const result = makeWatcherFinalityPolicy(
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
              operatorIdentitySha256: h32(0xa1),
              endpoint: "https://provider-a.example",
            },
            {
              identity: "provider-b",
              operatorIdentitySha256: h32(0xb2),
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
      trustRootId: h32(0x22),
      fundingProfileBundleDigest: "ab".repeat(32),
      blueprintHash: h32(0x33),
      ruleBundleCommitment: h32(0x44),
      programCommitments: { validation: h32(0x55) },
      durableMarker: marker,
    },
  );
  if (result === null) throw new Error("fixture policy failed");
  return result;
};

export class MemoryBackend implements WatcherDurableAtomicBackend {
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

export const client = (options: {
  initial?: WatcherRollbackDurableTrustedHead | null;
  refuseCas?: boolean;
  poisonReadBack?: boolean;
}) => {
  let current = options.initial ?? null;
  let casCount = 0;
  const value: WatcherTrustedHeadAuthorityClient = Object.freeze({
    readRecordAuthenticationKeyId: async () => h32(0x99),
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
  return {
    value,
    current: () => current,
    casCount: () => casCount,
    replace: (head: WatcherRollbackDurableTrustedHead | null) => {
      current = head;
    },
  };
};

export const checkpointFixture = async () => {
  const backend = new MemoryBackend();
  const sidecarOptions = { refuseCas: false, poisonReadBack: false };
  const sidecar = client(sidecarOptions);
  const finalityPolicy = policy();
  const objects = new Map<string, Uint8Array>();
  let onRead: (() => void) | undefined;
  const archive = {
    put: async (bytes: Uint8Array) => {
      const digest = watcherUserEventArchiveDigest(bytes);
      objects.set(digest, Uint8Array.from(bytes));
      return digest;
    },
    read: async (digest: string) => {
      onRead?.();
      const bytes = objects.get(digest);
      return bytes === undefined ? null : Uint8Array.from(bytes);
    },
  };
  const runtimeInput = {
    backend,
    policy: finalityPolicy,
    authenticationKey: key,
    client: sidecar.value,
    userEventArchive: archive,
  };
  const runtime = await createWatcherDurableRuntime(runtimeInput);
  const payload = new TextEncoder().encode('{"cursor":null}');
  const payloadDigest = await archive.put(payload);
  const frame = makeWatcherUserEventCheckpoint({
    schemaVersion: WATCHER_USER_EVENT_CHECKPOINT_SCHEMA_VERSION,
    deploymentMarker: finalityPolicy.deploymentMarker,
    network: finalityPolicy.network,
    blueprintHash: finalityPolicy.blueprintHash,
    finalityPolicyDigest: finalityPolicy.policyDigest,
    userEventPolicyDigest: h32(0x77),
    checkpointSequence: "0",
    predecessorCheckpointDigest: null,
    rollbackGeneration: "0",
    payloadDigest,
    requiredArchiveDigests: [payloadDigest],
  });
  const publish = () =>
    persistWatcherUserEventCheckpoint(runtime, {
      expectedCheckpointDigest: null,
      expectedCheckpointSequence: null,
      nextCheckpoint: frame,
    });
  return {
    runtime,
    runtimeInput,
    backend,
    sidecar,
    sidecarOptions,
    archive,
    objects,
    frame,
    payload,
    publish,
    setOnRead: (callback: () => void) => {
      onRead = callback;
    },
  };
};
