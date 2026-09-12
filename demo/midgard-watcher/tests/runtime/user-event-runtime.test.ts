import { createHash } from "node:crypto";
import { mkdtemp, readFile, rm, writeFile } from "node:fs/promises";
import { join } from "node:path";
import { fileURLToPath } from "node:url";

import { CML } from "@lucid-evolution/lucid";
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
import { readWatcherUserEventScriptBinding } from "../../src/runtime/deployment-identity.js";
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
import { createInMemoryWatcherUserEventCoverageStore } from "../../src/storage/user-event-coverage-store.js";
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
    // One store for the whole test so a restart restores the saved coverage.
    coverage: createInMemoryWatcherUserEventCoverageStore(),
  };
  const depositAddressHex = readWatcherUserEventScriptBinding({
    binding: fixture.scriptBinding,
    deploymentIdentity: fixture.deploymentIdentity,
  }).deposit.addressHex;
  let paymentIndex = 0;
  // A plain payment at the deposit credential: the relevance predicate matches
  // it, so the block is captured, yet the fold observes no event.
  const touchedBlock = async () => {
    const inputs = CML.TransactionInputList.new();
    inputs.add(
      CML.TransactionInput.new(
        CML.TransactionHash.from_hex(h32("c3")),
        BigInt(paymentIndex++),
      ),
    );
    const outputs = CML.TransactionOutputList.new();
    outputs.add(
      CML.TransactionOutput.new(
        CML.Address.from_hex(depositAddressHex),
        CML.Value.new(2_000_000n, CML.MultiAsset.new()),
      ),
    );
    return fixture.makeBlock({
      transactions: [
        CML.Transaction.new(
          CML.TransactionBody.new(inputs, outputs, 200_000n),
          CML.TransactionWitnessSet.new(),
          true,
        ).to_cbor_hex(),
      ],
    });
  };
  const capturesOf = (
    queries: readonly Awaited<
      ReturnType<typeof fixture.readNativeQueries>
    >[number][],
    blockHash: string,
  ) => queries.filter((query) => query.target.blockHash === blockHash);
  return {
    fixture,
    durable,
    input,
    touchedBlock,
    capturesOf,
    close: async () => {
      await fixture.close();
      await rm(directory, { recursive: true, force: true });
    },
  };
};

describe("owned user-event runtime over actual synthetic native transport", () => {
  it("discovers signed Init, covers quiet blocks without native requests, reopens the protected history with its coverage, and fails closed on source exit", async () => {
    const context = await setup();
    const { fixture, capturesOf } = context;
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
      expect(runtime.read()).toMatchObject({
        currentPoint: fixture.activationBlock.point,
        headCursor: fixture.activationBlock.point,
      });
      const before = (await fixture.readNativeQueries()).length;
      await runtime.advanceThrough(fixture.emptySuccessorBlock.point);
      expect(runtime.read()).toMatchObject({
        currentPoint: fixture.emptySuccessorBlock.point,
        headCursor: fixture.activationBlock.point,
      });
      expect(
        capturesOf(
          (await fixture.readNativeQueries()).slice(before),
          fixture.emptySuccessorBlock.point.blockHash,
        ),
      ).toHaveLength(0);
      await runtime.advanceThrough(fixture.activationBlock.point);
      expect(runtime.read().currentPoint).toEqual(
        fixture.emptySuccessorBlock.point,
      );
      const casBefore = context.durable.casCount();
      await runtime.close();
      await expect(runtime.done).resolves.toBeUndefined();
      const reopened = await createWatcherDurableRuntime(
        context.durable.runtimeInput,
      );
      runtime = await createWatcherUserEventRuntime({
        ...context.input,
        runtime: reopened,
      });
      expect(runtime.read()).toMatchObject({
        currentPoint: fixture.emptySuccessorBlock.point,
        headCursor: fixture.activationBlock.point,
      });
      expect(context.durable.casCount()).toBe(casBefore);
      const failed = expect(runtime.done).rejects.toThrow();
      await fixture.exitNativeStream(7);
      await failed;
      expect(runtime.read().status).toBe("failed");
      expect(() => assertWatcherUserEventRuntime(runtime!)).toThrow();
      await expect(
        runtime.advanceThrough(fixture.emptySuccessorBlock.point),
      ).rejects.toThrow();
    } finally {
      await runtime?.close();
      await context.close();
    }
  }, 120_000);
});

describe("user-event runtime coverage and capture", () => {
  it("captures only touched blocks, covers the quiet stretch in place, rewinds coverage on rollback, and restores it on restart", async () => {
    const context = await setup();
    const { fixture, capturesOf } = context;
    let runtime: Awaited<
      ReturnType<typeof createWatcherUserEventRuntime>
    > | null = null;
    try {
      runtime = await createWatcherUserEventRuntime(context.input);
      const quietA = fixture.emptySuccessorBlock;
      const quietB = await fixture.makeBlock({ transactions: [] });
      const touchedA = await context.touchedBlock();
      const quietC = await fixture.makeBlock({ transactions: [] });
      const touchedB = await context.touchedBlock();
      const quietD = await fixture.makeBlock({ transactions: [] });
      let before = (await fixture.readNativeQueries()).length;
      await runtime.advanceThrough(quietD.point);
      expect(runtime.read()).toMatchObject({
        currentPoint: quietD.point,
        headCursor: touchedB.point,
      });
      let queries = (await fixture.readNativeQueries()).slice(before);
      for (const quiet of [quietA, quietB, quietC, quietD])
        expect(capturesOf(queries, quiet.point.blockHash)).toHaveLength(0);
      for (const touched of [touchedA, touchedB])
        expect(capturesOf(queries, touched.point.blockHash)).toHaveLength(4);

      // Rolling back into the quiet stretch above the head observation
      // rewinds the checkpoint in place; the head observation survives.
      const quietE = await fixture.makeBlock({ transactions: [] });
      const quietF = await fixture.makeBlock({ transactions: [] });
      await runtime.advanceThrough(quietF.point);
      expect(runtime.read()).toMatchObject({
        currentPoint: quietF.point,
        headCursor: touchedB.point,
      });
      const priorGeneration = runtime.read().generation;
      await runtime.handleRollback({
        kind: "point",
        blockHash: quietD.point.blockHash,
        slot: quietD.point.slot,
      });
      expect(runtime.read()).toMatchObject({
        status: "ready",
        generation: priorGeneration + 1,
        currentPoint: quietD.point,
        headCursor: touchedB.point,
      });
      before = (await fixture.readNativeQueries()).length;
      await runtime.advanceThrough(quietF.point);
      expect(runtime.read()).toMatchObject({
        currentPoint: quietF.point,
        headCursor: touchedB.point,
      });
      queries = (await fixture.readNativeQueries()).slice(before);
      for (const quiet of [quietE, quietF])
        expect(capturesOf(queries, quiet.point.blockHash)).toHaveLength(0);

      // A restart restores the saved coverage above the sealed head.
      await runtime.close();
      const reopened = await createWatcherDurableRuntime(
        context.durable.runtimeInput,
      );
      before = (await fixture.readNativeQueries()).length;
      runtime = await createWatcherUserEventRuntime({
        ...context.input,
        runtime: reopened,
      });
      expect(runtime.read()).toMatchObject({
        status: "ready",
        currentPoint: quietF.point,
        headCursor: touchedB.point,
      });
      queries = (await fixture.readNativeQueries()).slice(before);
      for (const quiet of [quietC, quietD, quietE, quietF])
        expect(capturesOf(queries, quiet.point.blockHash)).toHaveLength(0);
      const quietG = await fixture.makeBlock({ transactions: [] });
      await runtime.advanceThrough(quietG.point);
      expect(runtime.read()).toMatchObject({
        currentPoint: quietG.point,
        headCursor: touchedB.point,
      });

      // A real native rollback frame suspends the service between calls.
      await fixture.rollbackNativeStream({
        blockHash: quietF.point.blockHash,
        blockNo: quietF.point.blockNo,
        slot: quietF.point.slot,
      });
      await expect.poll(() => runtime!.read().status).toBe("suspended");
      expect(() => assertWatcherUserEventRuntime(runtime!)).toThrow();
      await runtime.handleRollback({
        kind: "point",
        blockHash: quietF.point.blockHash,
        slot: quietF.point.slot,
      });
      expect(runtime.read()).toMatchObject({
        status: "ready",
        currentPoint: quietF.point,
        headCursor: touchedB.point,
      });

      // Below the head observation the runtime fails closed to restart
      // reconciliation, which drops observations above the fork.
      await expect(
        runtime.handleRollback({
          kind: "point",
          blockHash: quietB.point.blockHash,
          slot: quietB.point.slot,
        }),
      ).rejects.toThrow(/restart reconciliation/u);
      await runtime.close();
      runtime = await createWatcherUserEventRuntime({
        ...context.input,
        runtime: await createWatcherDurableRuntime(
          context.durable.runtimeInput,
        ),
      });
      expect(runtime.read()).toMatchObject({
        status: "ready",
        currentPoint: quietF.point,
        headCursor: touchedB.point,
      });

      // Below the release-final boundary the hash check is unnecessary: a
      // point there resolves as covered by height alone, with no request.
      const { computeFraudProofRawL1PointId } = await import(
        "@al-ft/midgard-fault-proofs"
      );
      const deep = { ...quietB.point, blockHash: h32("aa") };
      deep.pointId = computeFraudProofRawL1PointId(deep);
      before = (await fixture.readNativeQueries()).length;
      await runtime.advanceThrough(deep);
      expect((await fixture.readNativeQueries()).length).toBe(before);
      expect(runtime.read()).toMatchObject({
        status: "ready",
        currentPoint: quietF.point,
      });
      // A wrong hash at a height the runtime itself covered is refused
      // without any request; the caller asked for a point off the canonical
      // chain, so the runtime fails closed.
      const quietH = await fixture.makeBlock({ transactions: [] });
      await runtime.advanceThrough(quietH.point);
      const fork = { ...quietH.point, blockHash: h32("aa") };
      fork.pointId = computeFraudProofRawL1PointId(fork);
      before = (await fixture.readNativeQueries()).length;
      await expect(runtime.advanceThrough(fork)).rejects.toThrow(
        /exact accepted block/u,
      );
      expect((await fixture.readNativeQueries()).length).toBe(before);
      expect(runtime.read().status).toBe("failed");
      await expect(runtime.done).rejects.toThrow();
    } finally {
      await runtime?.close();
      await context.close();
    }
  }, 300_000);

  it("crosses 128 retained observations, rotates the sealed history, and restores it without replay", async () => {
    const context = await setup();
    const { fixture } = context;
    let runtime: Awaited<
      ReturnType<typeof createWatcherUserEventRuntime>
    > | null = null;
    try {
      runtime = await createWatcherUserEventRuntime(context.input);
      const blocks = [fixture.emptySuccessorBlock];
      for (let index = 1; index <= 130; index++)
        blocks.push(await context.touchedBlock());
      for (const index of [64, 127, 130]) {
        await runtime.advanceThrough(blocks[index]!.point);
        expect(runtime.read()).toMatchObject({
          currentPoint: blocks[index]!.point,
          headCursor: blocks[index]!.point,
        });
      }
      await runtime.advanceThrough(blocks[20]!.point);
      expect(runtime.read().currentPoint).toEqual(blocks[130]!.point);
      await runtime.close();
      const reopened = await createWatcherDurableRuntime(
        context.durable.runtimeInput,
      );
      runtime = await createWatcherUserEventRuntime({
        ...context.input,
        runtime: reopened,
      });
      expect(runtime.read()).toMatchObject({
        status: "ready",
        currentPoint: blocks[130]!.point,
        headCursor: blocks[130]!.point,
      });
      const successor = await fixture.makeBlock({ transactions: [] });
      await runtime.advanceThrough(successor.point);
      expect(runtime.read()).toMatchObject({
        currentPoint: successor.point,
        headCursor: blocks[130]!.point,
      });
    } finally {
      await runtime?.close();
      await context.close();
    }
  }, 600_000);
});
