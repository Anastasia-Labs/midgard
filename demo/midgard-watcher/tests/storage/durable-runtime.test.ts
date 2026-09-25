import { makeDeploymentMarker } from "@al-ft/midgard-core/deployment-manifest-identity";
import { h32 } from "@al-ft/midgard-test-support/hex";
import { describe, expect, it } from "vitest";

import {
  makeWatcherFinalityBootstrapState,
  makeWatcherFinalityPolicy,
  type WatcherFinalityPolicy,
} from "../../src/l1/finality-engine.js";
import {
  initializeWatcherRollbackDurableAuthority,
  type WatcherRollbackDurableTrustedHead,
} from "../../src/l1/rollback-engine.js";
import { WATCHER_CONFIG_SCHEMA_VERSION } from "../../src/runtime/config.js";
import type { WatcherTrustedHeadAuthorityClient } from "../../src/runtime/trusted-head-authority.js";
import {
  createWatcherDurableRuntime,
  persistWatcherUserEventCheckpoint,
  readWatcherProtectedUserEventCheckpoint,
  readWatcherProtectedUserEventCheckpointReceipt,
} from "../../src/storage/durable-runtime.js";
import type { WatcherDurableAtomicBackend } from "../../src/storage/durable-store.js";
import { makeEmptyWatcherDurableStore } from "../../src/storage/durable-store.js";
import {
  makeWatcherUserEventCheckpoint,
  WATCHER_USER_EVENT_CHECKPOINT_SCHEMA_VERSION,
  watcherUserEventArchiveDigest,
} from "../../src/storage/user-event-checkpoint.js";
import { createSyntheticStateQueueObservationFixture } from "../support/state-queue-observation-fixture.js";

const key = Uint8Array.from({ length: 32 }, (_, index) => index + 1);

const policy = (): WatcherFinalityPolicy => {
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

const checkpointFixture = async () => {
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

describe("production durable watcher runtime", () => {
  it("reloads observation and canonical progress checkpoints with their exact store and finality bindings", async () => {
    const fixture = await createSyntheticStateQueueObservationFixture();
    try {
      const capture = await fixture.observeFresh();
      const finalityPolicy = makeWatcherFinalityPolicy(
        fixture.transport.watcherConfig,
        fixture.transport.deploymentIdentity,
      );
      if (finalityPolicy === null) throw new Error("Fixture policy rejected");
      const backend = new MemoryBackend();
      const sidecar = client({});
      const input = {
        backend,
        policy: finalityPolicy,
        authenticationKey: key,
        client: sidecar.value,
      };
      const runtime = await createWatcherDurableRuntime(input);
      const before = runtime.read();
      const initialFinality = runtime.readFinality();
      expect(initialFinality).toEqual(before.currentFinalityState);
      await expect(
        runtime.persistObservation({
          ...capture.localObservation,
          transportAttestations: [],
        }),
      ).rejects.toThrow(/authenticated/u);
      expect(runtime.read()).toEqual(before);
      expect(
        (await runtime.persistObservation(capture.localObservation))
          .persistence,
      ).toBe("committed");
      const observed = runtime.read();
      expect(runtime.readFinality()).toEqual(initialFinality);
      expect(observed.currentFinalityState).toEqual(
        before.currentFinalityState,
      );
      expect(observed.currentStore.l1Observations.length).toBeGreaterThan(0);
      const restarted = await createWatcherDurableRuntime(input);
      expect(restarted.read()).toEqual(observed);
      expect(
        (await restarted.persistObservation(capture.localObservation))
          .persistence,
      ).toBe("unchanged");
      const progress = await restarted.persistCanonicalProgress(
        capture.localObservation,
      );
      expect(progress.persistence).toBe("committed");
      if (progress.persistence !== "committed")
        throw new Error("Expected canonical progress");
      expect(restarted.read().currentFinalityState).toEqual(
        progress.finalityResult.state,
      );
      const currentFinality = restarted.readFinality();
      expect(currentFinality).toEqual(progress.finalityResult.state);
      expect(currentFinality.pending).not.toBe(
        restarted.readFinality().pending,
      );
      expect(initialFinality).toEqual(before.currentFinalityState);
      const progressed = await createWatcherDurableRuntime(input);
      expect(progressed.read()).toEqual(restarted.read());
      expect(progressed.readFinality()).toEqual(currentFinality);
      expect(progressed.read().currentFinalityState).not.toEqual(
        before.currentFinalityState,
      );
    } finally {
      await fixture.close();
    }
  });
  it("publishes checkpoint sequence zero independently of the global store revision and recovers it", async () => {
    const fixture = await checkpointFixture();
    const before = fixture.runtime.read();
    const result = await fixture.publish();
    expect(result.persistence).toBe("committed");
    const read = readWatcherProtectedUserEventCheckpointReceipt(
      result.protectedCheckpoint,
    );
    expect(read.checkpoint).toEqual(fixture.frame);
    expect(read.payload).toEqual(fixture.payload);
    expect(read.trustedHead.revision).toBe("1");
    expect(fixture.runtime.read()).toEqual(before);
    read.payload![0] = 0;
    expect(
      readWatcherProtectedUserEventCheckpointReceipt(result.protectedCheckpoint)
        .payload,
    ).toEqual(fixture.payload);
    expect(() =>
      readWatcherProtectedUserEventCheckpointReceipt({
        ...result.protectedCheckpoint,
      }),
    ).toThrow("not admitted");
    const restarted = await createWatcherDurableRuntime(fixture.runtimeInput);
    expect(
      readWatcherProtectedUserEventCheckpointReceipt(
        await readWatcherProtectedUserEventCheckpoint(restarted),
      ).checkpoint,
    ).toEqual(fixture.frame);
    const repeats = fixture.sidecar.casCount();
    const repeated = await persistWatcherUserEventCheckpoint(fixture.runtime, {
      expectedCheckpointDigest: fixture.frame.checkpointDigest,
      expectedCheckpointSequence: "0",
      nextCheckpoint: fixture.frame,
    });
    expect(repeated.persistence).toBe("unchanged");
    expect(fixture.sidecar.casCount()).toBe(repeats);
  });

  it("serializes competing checkpoints and revokes receipts on replacement or failure", async () => {
    const fixture = await checkpointFixture();
    const first = await fixture.publish();
    const next = makeWatcherUserEventCheckpoint({
      ...fixture.frame,
      checkpointSequence: "1",
      predecessorCheckpointDigest: fixture.frame.checkpointDigest,
    });
    await persistWatcherUserEventCheckpoint(fixture.runtime, {
      expectedCheckpointDigest: fixture.frame.checkpointDigest,
      expectedCheckpointSequence: "0",
      nextCheckpoint: next,
    });
    expect(() =>
      readWatcherProtectedUserEventCheckpointReceipt(first.protectedCheckpoint),
    ).toThrow("stale");
    const receipt = await readWatcherProtectedUserEventCheckpoint(
      fixture.runtime,
    );
    await expect(fixture.publish()).rejects.toThrow("CAS conflicted");
    expect(() =>
      readWatcherProtectedUserEventCheckpointReceipt(receipt),
    ).toThrow("stale");
    const race = await checkpointFixture();
    const results = await Promise.allSettled([race.publish(), race.publish()]);
    expect(results.map(({ status }) => status)).toEqual([
      "fulfilled",
      "rejected",
    ]);
  });

  it("requires actual archive bytes before publication and on startup", async () => {
    const fixture = await checkpointFixture();
    fixture.objects.clear();
    const before = await fixture.backend.read();
    await expect(fixture.publish()).rejects.toThrow("missing");
    expect(await fixture.backend.read()).toEqual(before);
    await fixture.archive.put(fixture.payload);
    await fixture.publish();
    const { userEventArchive: _archive, ...withoutArchive } =
      fixture.runtimeInput;
    await expect(createWatcherDurableRuntime(withoutArchive)).rejects.toThrow(
      "requires its archive",
    );
    fixture.objects.clear();
    await expect(
      createWatcherDurableRuntime(fixture.runtimeInput),
    ).rejects.toThrow("missing");
  });

  it("recovers an event snapshot committed before sidecar publication without issuing a receipt on failure", async () => {
    const fixture = await checkpointFixture();
    const receipt = await readWatcherProtectedUserEventCheckpoint(
      fixture.runtime,
    );
    fixture.sidecarOptions.refuseCas = true;
    await expect(fixture.publish()).rejects.toThrow("CAS conflicted");
    expect(fixture.sidecar.current()?.revision).toBe("0");
    expect(() =>
      readWatcherProtectedUserEventCheckpointReceipt(receipt),
    ).toThrow("stale");
    fixture.sidecarOptions.refuseCas = false;
    const recovered = await createWatcherDurableRuntime(fixture.runtimeInput);
    expect(fixture.sidecar.current()?.revision).toBe("1");
    expect(
      readWatcherProtectedUserEventCheckpointReceipt(
        await readWatcherProtectedUserEventCheckpoint(recovered),
      ).checkpoint,
    ).toEqual(fixture.frame);
  });

  it("refuses a head change during awaited archive validation", async () => {
    const fixture = await checkpointFixture();
    const first = await fixture.publish();
    fixture.setOnRead(() =>
      fixture.sidecar.replace({ ...fixture.sidecar.current()!, revision: "2" }),
    );
    await expect(
      readWatcherProtectedUserEventCheckpoint(fixture.runtime),
    ).rejects.toThrow("changed during checkpoint archive validation");
    expect(() =>
      readWatcherProtectedUserEventCheckpointReceipt(first.protectedCheckpoint),
    ).toThrow("stale");
  });
  it.each(["backend", "head", "archive"] as const)(
    "refuses %s tampering during checkpoint publication and revokes prior receipts",
    async (owner) => {
      const fixture = await checkpointFixture();
      const bootstrapBytes = await fixture.backend.read();
      const receipt = await readWatcherProtectedUserEventCheckpoint(
        fixture.runtime,
      );
      let tampered = false;
      fixture.setOnRead(() => {
        const head = fixture.sidecar.current();
        if (tampered || head?.revision !== "1") return;
        tampered = true;
        if (owner === "backend") {
          fixture.backend.bytes = bootstrapBytes;
        } else if (owner === "head") {
          fixture.sidecar.replace({ ...head, revision: "2" });
        } else {
          fixture.objects.set(fixture.frame.payloadDigest, new Uint8Array([0]));
        }
      });
      await expect(fixture.publish()).rejects.toThrow();
      expect(tampered).toBe(true);
      expect(() =>
        readWatcherProtectedUserEventCheckpointReceipt(receipt),
      ).toThrow("stale");
    },
  );

  it("publishes and reads back epoch zero before returning an authority", async () => {
    const backend = new MemoryBackend();
    const sidecar = client({});
    const runtime = await createWatcherDurableRuntime({
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
    const bootstrap = makeWatcherFinalityBootstrapState(finalityPolicy)!;
    await initializeWatcherRollbackDurableAuthority({
      backend,
      policy: finalityPolicy,
      authenticationKey: key,
      trustedHead: null,
      bootstrapStore: makeEmptyWatcherDurableStore(
        finalityPolicy.deploymentMarker,
      ),
      bootstrapFinalityState: bootstrap,
    });
    const sidecar = client({});

    const runtime = await createWatcherDurableRuntime({
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
      createWatcherDurableRuntime({
        backend: new MemoryBackend(),
        policy: policy(),
        authenticationKey: key,
        client: client({ refuseCas: true }).value,
      }),
    ).rejects.toThrow("trusted-head direct-successor CAS conflicted");
    await expect(
      createWatcherDurableRuntime({
        backend: new MemoryBackend(),
        policy: policy(),
        authenticationKey: key,
        client: client({ poisonReadBack: true }).value,
      }),
    ).rejects.toThrow("trusted-head read-back differs");
  });
});
