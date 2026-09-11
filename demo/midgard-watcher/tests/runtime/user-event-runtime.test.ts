import { createHash } from "node:crypto";
import { mkdtemp, readFile, rm, writeFile } from "node:fs/promises";
import { join } from "node:path";
import { fileURLToPath } from "node:url";

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
import { loadWatcherVerifiedDeploymentAuthority } from "../../src/runtime/deployment-authority.js";
import type { WatcherTrustedHeadAuthorityClient } from "../../src/runtime/trusted-head-authority.js";
import {
  assertWatcherUserEventRuntime,
  createWatcherUserEventRuntime,
} from "../../src/runtime/user-event-runtime.js";
import { createWatcherDurableRuntime } from "../../src/storage/durable-runtime.js";
import {
  type WatcherDurableAtomicBackend,
  type WatcherDurableStore,
  watcherSameCanonicalJson,
} from "../../src/storage/durable-store.js";
import { watcherUserEventArchiveDigest } from "../../src/storage/user-event-checkpoint.js";
import {
  computeWatcherRuleBundleCommitment,
  makeWatcherCanonicalRuleBundle,
} from "../../src/verification/rule-bundle.js";
import { WATCHER_TEST_CARDANO_PROTOCOL_PARAMETERS } from "../support/deployment-authority-fixture.js";
import { createSyntheticUserEventOriginFixture } from "../support/user-event-origin-fixture.js";
const sha256 = (bytes: Uint8Array): string =>
  createHash("sha256").update(bytes).digest("hex");
const h32 = (byte: string) => byte.repeat(32);
const durableFixture = async (
  policy: WatcherFinalityPolicy,
  bootstrapStore?: WatcherDurableStore,
) => {
  let bytes: Uint8Array | null = null;
  let currentHead: WatcherRollbackDurableTrustedHead | null = null;
  let failAfterCas = false;
  let failNextRead = false;
  let beforePut: (() => Promise<void>) | null = null;
  let casCount = 0;
  const backend: WatcherDurableAtomicBackend = {
    read: async () => (bytes === null ? null : Uint8Array.from(bytes)),
    compareAndSwap: async (expected, next) => {
      if ((bytes === null ? null : sha256(bytes)) !== expected) return false;
      bytes = Uint8Array.from(next);
      return true;
    },
  };
  const client: WatcherTrustedHeadAuthorityClient = {
    readRecordAuthenticationKeyId: async () => h32("99"),
    readCurrent: async () => {
      if (failNextRead) {
        failNextRead = false;
        throw new Error("fixture read-back interruption");
      }
      return currentHead;
    },
    compareAndSwap: async ({ expectedTrustedHead, nextTrustedHead }) => {
      if (!watcherSameCanonicalJson(expectedTrustedHead, currentHead))
        return false;
      currentHead = nextTrustedHead;
      casCount += 1;
      if (failAfterCas) {
        failAfterCas = false;
        failNextRead = true;
      }
      return true;
    },
  };
  const objects = new Map<string, Uint8Array>();
  const archive = {
    put: async (value: Uint8Array) => {
      await beforePut?.();
      const digest = watcherUserEventArchiveDigest(value);
      objects.set(digest, Uint8Array.from(value));
      return digest;
    },
    read: async (digest: string) => {
      const value = objects.get(digest);
      return value === undefined ? null : Uint8Array.from(value);
    },
  };
  const runtimeInput = {
    backend,
    policy,
    authenticationKey: Uint8Array.from({ length: 32 }, (_, index) => index + 1),
    client,
    userEventArchive: archive,
  };
  if (bootstrapStore !== undefined)
    await initializeWatcherRollbackDurableAuthority({
      backend,
      policy,
      authenticationKey: runtimeInput.authenticationKey,
      trustedHead: null,
      bootstrapStore,
      bootstrapFinalityState: makeWatcherFinalityBootstrapState(policy)!,
    });
  const runtime = await createWatcherDurableRuntime(runtimeInput);
  return {
    runtime,
    runtimeInput,
    archive,
    objects,
    casCount: () => casCount,
    interruptNextReadBack: () => {
      failAfterCas = true;
    },
    setBeforePut: (callback: (() => Promise<void>) | null) => {
      beforePut = callback;
    },
  };
};

const setup = async (
  nativeTipMode: "controlled" | "query_counter" = "query_counter",
) => {
  const construction = await createSyntheticUserEventOriginFixture({
    nativeTipBaseDepth: 100,
  });
  const identity = construction.deploymentIdentity;
  const ruleBundle = makeWatcherCanonicalRuleBundle({
    constructionIdentity: {
      manifestId: identity.manifestId,
      network: identity.network,
      blueprintHash: identity.blueprintHash,
      programCommitments: identity.programCommitments,
    },
    targetParameterSnapshot: WATCHER_TEST_CARDANO_PROTOCOL_PARAMETERS,
  });
  await construction.close();
  const fixture = await createSyntheticUserEventOriginFixture({
    nativeTipBaseDepth: 100,
    nativeTipMode,
    nativeStreamInitialAcknowledgement: true,
    ruleBundleCommitment: computeWatcherRuleBundleCommitment(ruleBundle),
  });
  const directory = await mkdtemp("/var/tmp/user-event-runtime-release-");
  const path = join(directory, "authority.json");
  const ruleBundlePath = join(directory, "rules.json");
  await writeFile(
    path,
    JSON.stringify({
      signedIdentity: fixture.deployment.signedIdentity,
      policy: fixture.deployment.policy,
      trustRoots: fixture.deployment.trustRoots,
      durableMarker: fixture.deployment.marker,
    }),
  );
  await writeFile(ruleBundlePath, JSON.stringify(ruleBundle));
  const deploymentAuthority = await loadWatcherVerifiedDeploymentAuthority({
    path,
    ruleBundlePath,
  });
  const durable = await durableFixture(
    makeWatcherFinalityPolicy(
      fixture.watcherConfig,
      deploymentAuthority.deploymentIdentity,
    )!,
  );
  const blueprintBytes = await readFile(
    process.env.MIDGARD_REAL_BLUEPRINT_PATH ??
      fileURLToPath(
        new URL("../../../../onchain/aiken/plutus.json", import.meta.url),
      ),
  );
  const input = {
    watcherConfig: fixture.watcherConfig,
    deploymentAuthority,
    blueprintBytes,
    nativeChainSyncBinaryPath: fixture.nativeChainSyncBinaryPath,
    runtime: durable.runtime,
    archive: durable.archive,
  };
  return {
    fixture,
    durable,
    input,
    close: async () => {
      await fixture.close();
      await rm(directory, { recursive: true, force: true });
    },
  };
};

describe("owned user-event runtime over actual synthetic native transport", () => {
  it("discovers signed Init, publishes empty blocks, reopens the protected history, and fails closed on source exit", async () => {
    const context = await setup();
    let runtime: Awaited<
      ReturnType<typeof createWatcherUserEventRuntime>
    > | null = null;
    try {
      runtime = await createWatcherUserEventRuntime(context.input);
      expect(() => assertWatcherUserEventRuntime(runtime!)).not.toThrow();
      expect(() => assertWatcherUserEventRuntime({ ...runtime! })).toThrow();
      expect(() =>
        Reflect.apply(runtime!.eventAuthority, runtime, [
          { kind: "deposit", eventId: h32("aa") },
        ]),
      ).toThrow();
      expect(runtime.read().currentPoint).toEqual(
        context.fixture.activationBlock.point,
      );
      await runtime.advanceThrough(context.fixture.emptySuccessorBlock.point);
      expect(runtime.read().currentPoint).toEqual(
        context.fixture.emptySuccessorBlock.point,
      );
      await runtime.advanceThrough(context.fixture.activationBlock.point);
      expect(runtime.read().currentPoint).toEqual(
        context.fixture.emptySuccessorBlock.point,
      );
      const before = context.durable.casCount();
      await runtime.close();
      await expect(runtime.done).resolves.toBeUndefined();
      const reopened = await createWatcherDurableRuntime(
        context.durable.runtimeInput,
      );
      runtime = await createWatcherUserEventRuntime({
        ...context.input,
        runtime: reopened,
      });
      expect(runtime.read().currentPoint).toEqual(
        context.fixture.emptySuccessorBlock.point,
      );
      expect(context.durable.casCount()).toBe(before);
      const failed = expect(runtime.done).rejects.toThrow();
      await context.fixture.exitNativeStream(7);
      await failed;
      expect(runtime.read().status).toBe("failed");
      expect(() => assertWatcherUserEventRuntime(runtime!)).toThrow();
      await expect(
        runtime.advanceThrough(context.fixture.emptySuccessorBlock.point),
      ).rejects.toThrow();
    } finally {
      await runtime?.close();
      await context.close();
    }
  }, 120_000);
});

const waitForQuery = async (
  fixture: Awaited<ReturnType<typeof setup>>["fixture"],
  blockHash: string,
  after: number,
  operation: Promise<unknown>,
) => {
  let failure: unknown;
  let finished = false;
  void operation.then(
    () => {
      finished = true;
    },
    (error: unknown) => {
      failure = error;
      finished = true;
    },
  );
  const deadline = performance.now() + 100_000;
  for (;;) {
    if (finished)
      throw (
        failure ??
        new Error(
          "Runtime operation finished before a required real TIP growth",
        )
      );
    const queries = (await fixture.readNativeQueries()).slice(after);
    if (
      queries.filter((query) => query.target.blockHash === blockHash).length >=
      3
    )
      return queries;
    if (performance.now() > deadline)
      throw new Error("Runtime never reached its second capture wave");
    await new Promise((resolve) => setTimeout(resolve, 10));
  }
};

describe("user-event runtime native acquisition batching", () => {
  it("retains every first capture when the native tip advances across a startup batch", async () => {
    const context = await setup();
    let runtime:
      | Awaited<ReturnType<typeof createWatcherUserEventRuntime>>
      | undefined;
    try {
      runtime = await createWatcherUserEventRuntime(context.input);
      const blocks = [context.fixture.emptySuccessorBlock];
      for (let index = 0; index < 3; index += 1)
        blocks.push(await context.fixture.makeBlock({ transactions: [] }));
      const before = (await context.fixture.readNativeQueries()).length;
      await runtime.advanceThrough(blocks.at(-1)!.point);
      expect(runtime.read().currentPoint).toEqual(blocks.at(-1)!.point);
      const queries = (await context.fixture.readNativeQueries()).slice(before);
      for (const block of blocks)
        expect(
          queries.filter(
            (query) => query.target.blockHash === block.point.blockHash,
          ),
        ).toHaveLength(4);
    } finally {
      await runtime?.close();
      await context.close();
    }
  });

  it("prefetches first observations while publishing each requested block only and discards them on rollback", async () => {
    const context = await setup("controlled");
    const { fixture } = context;
    let runtime: Awaited<
      ReturnType<typeof createWatcherUserEventRuntime>
    > | null = null;
    try {
      const blocks = [fixture.activationBlock, fixture.emptySuccessorBlock];
      for (let index = 2; index <= 8; index += 1)
        blocks.push(await fixture.makeBlock({ transactions: [] }));
      const last = blocks.at(-1)!.point;
      await fixture.setNativeTip({
        blockHash: h32("ed"),
        blockNo: (BigInt(last.blockNo) + 100n).toString(),
        slot: (BigInt(last.slot) + 600n).toString(),
      });
      const bootstrap = createWatcherUserEventRuntime(context.input);
      await waitForQuery(fixture, blocks[0]!.point.blockHash, 0, bootstrap);
      await fixture.growNativeTip();
      runtime = await bootstrap;

      let before = (await fixture.readNativeQueries()).length;
      const first = runtime.advanceThrough(blocks[1]!.point, {
        prefetch: true,
      });
      const firstWave = await waitForQuery(
        fixture,
        blocks[1]!.point.blockHash,
        before,
        first,
      );
      for (const block of blocks.slice(2))
        expect(
          firstWave.filter(
            (query) => query.target.blockHash === block.point.blockHash,
          ),
        ).toHaveLength(2);
      expect(runtime.read().currentPoint).toEqual(blocks[0]!.point);
      await fixture.growNativeTip();
      await first;
      expect(runtime.read().currentPoint).toEqual(blocks[1]!.point);

      // One actual tip growth completes multiple later calls, but each still
      // performs its own fresh native second capture before publication.
      for (const block of blocks.slice(2, 5)) {
        before = (await fixture.readNativeQueries()).length;
        await runtime.advanceThrough(block.point, { prefetch: true });
        expect(runtime.read().currentPoint).toEqual(block.point);
        const captures = (await fixture.readNativeQueries())
          .slice(before)
          .filter((query) => query.target.blockHash === block.point.blockHash);
        expect(captures).toHaveLength(2);
      }

      before = (await fixture.readNativeQueries()).length;
      const rollback = runtime.handleRollback({
        kind: "point",
        blockHash: blocks[4]!.point.blockHash,
        slot: blocks[4]!.point.slot,
      });
      await waitForQuery(fixture, blocks[4]!.point.blockHash, before, rollback);
      await fixture.growNativeTip();
      await rollback;
      before = (await fixture.readNativeQueries()).length;
      const resumed = runtime.advanceThrough(blocks[5]!.point, {
        prefetch: true,
      });
      await waitForQuery(fixture, blocks[5]!.point.blockHash, before, resumed);
      expect(runtime.read().currentPoint).toEqual(blocks[4]!.point);
      await fixture.growNativeTip();
      await resumed;
      for (const block of blocks.slice(6)) {
        await runtime.advanceThrough(block.point, { prefetch: true });
        expect(runtime.read().currentPoint).toEqual(block.point);
      }
    } finally {
      await runtime?.close();
      await context.close();
    }
  }, 120_000);

  it("crosses 128 blocks with shared first-wave tips, resumes a suspended head and restores its sealed history without replay", async () => {
    const context = await setup("controlled");
    const { fixture } = context;
    let runtime: Awaited<
      ReturnType<typeof createWatcherUserEventRuntime>
    > | null = null;
    try {
      const blocks = [fixture.activationBlock, fixture.emptySuccessorBlock];
      for (let index = 2; index <= 129; index++)
        blocks.push(await fixture.makeBlock({ transactions: [] }));
      const last = blocks.at(-1)!.point;
      await fixture.setNativeTip({
        blockHash: h32("ed"),
        blockNo: (BigInt(last.blockNo) + 100n).toString(),
        slot: (BigInt(last.slot) + 600n).toString(),
      });
      const bootstrap = createWatcherUserEventRuntime(context.input);
      await waitForQuery(fixture, blocks[0]!.point.blockHash, 0, bootstrap);
      await fixture.growNativeTip();
      runtime = await bootstrap;
      for (const [first, lastIndex] of [
        [1, 64],
        [65, 127],
      ] as const) {
        const before = (await fixture.readNativeQueries()).length;
        const advance = runtime.advanceThrough(blocks[lastIndex]!.point);
        const firstWave = await waitForQuery(
          fixture,
          blocks[first]!.point.blockHash,
          before,
          advance,
        );
        const tips = new Set<string>();
        for (const block of blocks.slice(first, lastIndex + 1)) {
          const captures = firstWave
            .filter((query) => query.target.blockHash === block.point.blockHash)
            .slice(0, 2);
          expect(captures).toHaveLength(2);
          for (const capture of captures) tips.add(JSON.stringify(capture.tip));
        }
        expect(tips.size).toBe(1);
        await fixture.growNativeTip();
        await advance;
        expect(runtime.read().currentPoint).toEqual(blocks[lastIndex]!.point);
      }
      let before = (await fixture.readNativeQueries()).length;
      const crossing = runtime.advanceThrough(blocks[129]!.point);
      await waitForQuery(
        fixture,
        blocks[127]!.point.blockHash,
        before,
        crossing,
      );
      await fixture.growNativeTip();
      before = (await fixture.readNativeQueries()).length;
      await waitForQuery(
        fixture,
        blocks[128]!.point.blockHash,
        before,
        crossing,
      );
      await fixture.growNativeTip();
      await crossing;
      expect(runtime.read().currentPoint).toEqual(blocks[129]!.point);
      await runtime.advanceThrough(blocks[20]!.point);
      expect(runtime.read().currentPoint).toEqual(blocks[129]!.point);
      before = (await fixture.readNativeQueries()).length;
      const priorGeneration = runtime.read().generation;
      const rollback = runtime.handleRollback({
        kind: "point",
        blockHash: last.blockHash,
        slot: last.slot,
      });
      expect(runtime.read().status).toBe("suspended");
      expect(runtime.read().generation).toBe(priorGeneration + 1);
      expect(() => assertWatcherUserEventRuntime(runtime!)).toThrow(
        /suspended/u,
      );
      await expect(runtime.advanceThrough(last)).rejects.toThrow();
      await waitForQuery(fixture, last.blockHash, before, rollback);
      await fixture.growNativeTip();
      await rollback;
      expect(runtime.read().status).toBe("ready");
      expect(runtime.read().generation).toBe(priorGeneration + 1);
      expect(() => assertWatcherUserEventRuntime(runtime!)).not.toThrow();
      await runtime.close();
      expect(() => assertWatcherUserEventRuntime(runtime!)).toThrow(
        /closed|abort/u,
      );
      const reopened = await createWatcherDurableRuntime(
        context.durable.runtimeInput,
      );
      before = (await fixture.readNativeQueries()).length;
      const restarting = createWatcherUserEventRuntime({
        ...context.input,
        runtime: reopened,
      });
      for (const index of [0, 129]) {
        await waitForQuery(
          fixture,
          blocks[index]!.point.blockHash,
          before,
          restarting,
        );
        await fixture.growNativeTip();
        before = (await fixture.readNativeQueries()).length;
      }
      runtime = await restarting;
      expect(runtime.read().currentPoint).toEqual(last);
      await runtime.advanceThrough(blocks[20]!.point);
      const successor = await fixture.makeBlock({
        transactions: [],
        parent: blocks[129]!,
      });
      before = (await fixture.readNativeQueries()).length;
      const interrupted = runtime.advanceThrough(successor.point);
      const interruptedRefusal = expect(interrupted).rejects.toThrow();
      await waitForQuery(
        fixture,
        successor.point.blockHash,
        before,
        interrupted,
      );
      before = (await fixture.readNativeQueries()).length;
      const recovering = runtime.handleRollback({
        kind: "point",
        blockHash: last.blockHash,
        slot: last.slot,
      });
      await interruptedRefusal;
      await waitForQuery(fixture, last.blockHash, before, recovering);
      await fixture.growNativeTip();
      await recovering;
      expect(runtime.read().currentPoint).toEqual(last);
      before = (await fixture.readNativeQueries()).length;
      const next = runtime.advanceThrough(successor.point);
      await waitForQuery(fixture, successor.point.blockHash, before, next);
      await fixture.growNativeTip();
      await next;
      expect(runtime.read().currentPoint).toEqual(successor.point);
      // A real native rollback frame also suspends the service between calls.
      before = (await fixture.readNativeQueries()).length;
      await fixture.rollbackNativeStream({
        blockHash: successor.point.blockHash,
        blockNo: successor.point.blockNo,
        slot: successor.point.slot,
      });
      await expect.poll(() => runtime!.read().status).toBe("suspended");
      expect(() => assertWatcherUserEventRuntime(runtime!)).toThrow();
      const monitoredRecovery = runtime.handleRollback({
        kind: "point",
        blockHash: successor.point.blockHash,
        slot: successor.point.slot,
      });
      await waitForQuery(
        fixture,
        successor.point.blockHash,
        before,
        monitoredRecovery,
      );
      await fixture.growNativeTip();
      await monitoredRecovery;
      expect(runtime.read().status).toBe("ready");
      const fork = { ...blocks[20]!.point, blockHash: h32("aa") };
      // Supply a geometrically valid point-id on the wrong accepted height.
      const { computeFraudProofRawL1PointId } = await import(
        "@al-ft/midgard-fault-proofs"
      );
      fork.pointId = computeFraudProofRawL1PointId(fork);
      await expect(runtime.advanceThrough(fork)).rejects.toThrow(
        /exact accepted block/u,
      );
      await expect(runtime.done).rejects.toThrow();
    } finally {
      await runtime?.close();
      await context.close();
    }
  }, 600_000);
});
