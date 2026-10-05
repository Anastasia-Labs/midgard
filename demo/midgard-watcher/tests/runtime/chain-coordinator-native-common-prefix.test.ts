import { DatabaseSync } from "node:sqlite";

import { readAdmittedLocalKupmiosBoundary } from "@al-ft/midgard-fault-proofs";
import { describe, expect, it } from "vitest";

import { makeWatcherFinalityPolicy } from "../../src/l1/finality-engine.js";
import type { WatcherLocalKupmiosNativeObservationRuntime } from "../../src/l1/local-kupmios-native-observation.js";
import {
  assertWatcherLocalKupmiosNativeObservation,
  createWatcherLocalKupmiosNativeObservationRuntime,
  type WatcherLocalKupmiosNativeObservation,
} from "../../src/l1/local-kupmios-native-observation.js";
import type { WatcherNativeBlockAdmission } from "../../src/l1/native-block-admission.js";
import { admitWatcherNativeRollForwardBlock } from "../../src/l1/native-block-admission.js";
import type { WatcherNativeChainSyncEvent } from "../../src/l1/native-chain-sync.js";
import {
  openWatcherNativeExactPointQuery,
  readWatcherNativeExactPointQuery,
} from "../../src/l1/native-chain-sync.js";
import type { WatcherRollbackDurableTrustedHead } from "../../src/l1/rollback-engine.js";
import { unsafeCreateWatcherChainCoordinatorForTest } from "../../src/runtime/chain-coordinator.js";
import { oldestRetainedCanonicalHint } from "../../src/runtime/chain-coordinator.retained-canonical-prefix.js";
import { createWatcherSqliteBlockProgressStore } from "../../src/storage/block-progress-store.js";
import { createWatcherDurableRuntime } from "../../src/storage/durable-runtime.js";
import {
  hex32,
  MemoryRollbackAuthorityBackend,
  rollbackAuthorityKey,
} from "../l1/rollback-engine.test-tls-identities.js";
import { createSyntheticStateQueueObservationFixture } from "../support/state-queue-observation-fixture.js";
import type { SyntheticUserEventBlock } from "../support/user-event-origin-fixture.js";

describe("native reconnect common-prefix reconciliation", () => {
  it("scans a retained common child before recovering the actual replacement and resumes delivery", async () => {
    const fixture = await createSyntheticStateQueueObservationFixture();
    let stopCoordinator: (() => Promise<void>) | undefined;
    const sqlite = new DatabaseSync(":memory:");
    const closeObservations: Array<() => Promise<void>> = [];
    try {
      const nativeBlocks = new Map<
        string,
        Readonly<{
          block: WatcherNativeBlockAdmission;
          event: Extract<WatcherNativeChainSyncEvent, { kind: "roll_forward" }>;
        }>
      >();
      const observe = async (block: SyntheticUserEventBlock) => {
        const query = await openWatcherNativeExactPointQuery({
          binaryPath: fixture.transport.nativeChainSyncBinaryPath,
          watcherConfig: fixture.transport.watcherConfig,
          predecessor: {
            blockHash: block.parentPoint.blockHash,
            blockNo: block.parentPoint.blockNo,
            slot: block.parentPoint.slot,
          },
          target: {
            blockHash: block.point.blockHash,
            blockNo: block.point.blockNo,
            slot: block.point.slot,
          },
          timeoutMs: 60_000,
        });
        const details = readWatcherNativeExactPointQuery(query.receipt);
        nativeBlocks.set(block.point.blockHash, {
          block: admitWatcherNativeRollForwardBlock(details.event),
          event: details.event,
        });
        const local = await createWatcherLocalKupmiosNativeObservationRuntime({
          watcherConfig: fixture.transport.watcherConfig,
          deploymentIdentity: fixture.transport.deploymentIdentity,
          nativeAuthority: details.authority,
        });
        closeObservations.push(async () => {
          local.close();
          await query.close();
        });
        await readAdmittedLocalKupmiosBoundary({ source: local.rawSource });
        return local.observe({
          block: admitWatcherNativeRollForwardBlock(details.event),
          depth: details.depthAtObservedTip,
        });
      };
      const policy = makeWatcherFinalityPolicy(
        fixture.transport.watcherConfig,
        fixture.transport.deploymentIdentity,
      );
      if (policy === null) throw new Error("Expected signed local policy");
      const backend = new MemoryRollbackAuthorityBackend();
      let head: WatcherRollbackDurableTrustedHead | null = null;
      const client = {
        readRecordAuthenticationKeyId: async () => hex32("99"),
        readCurrent: async () => head,
        compareAndSwap: async (input: {
          expectedTrustedHead: WatcherRollbackDurableTrustedHead | null;
          nextTrustedHead: WatcherRollbackDurableTrustedHead;
        }) => {
          if (
            JSON.stringify(head) !== JSON.stringify(input.expectedTrustedHead)
          )
            return false;
          head = input.nextTrustedHead;
          return true;
        },
      };
      const runtime = await createWatcherDurableRuntime({
        backend,
        policy,
        authenticationKey: rollbackAuthorityKey,
        client,
      });
      const ancestor = fixture.initializationBlock;
      await runtime.persistCanonicalProgress(await observe(ancestor));
      await runtime.persistCanonicalProgress(await observe(ancestor));
      const middle = fixture.commitBlock;
      await runtime.persistCanonicalProgress(await observe(middle));
      await runtime.persistCanonicalProgress(await observe(middle));
      const releasedBlock = await fixture.transport.makeBlock({
        transactions: [],
        parent: middle,
      });
      await runtime.persistCanonicalProgress(await observe(releasedBlock));
      await runtime.persistCanonicalProgress(await observe(releasedBlock));
      const pendingBlock = await fixture.transport.makeBlock({
        transactions: [],
        parent: releasedBlock,
      });
      await runtime.persistCanonicalProgress(await observe(pendingBlock));
      const hint = oldestRetainedCanonicalHint(runtime);
      expect(hint).toEqual({
        blockHash: ancestor.point.blockHash,
        blockNo: ancestor.point.blockNo,
        slot: ancestor.point.slot,
      });
      const replacementBlock = await fixture.transport.makeBlock({
        transactions: [],
        parent: middle,
        slot: Number(pendingBlock.point.slot) + 1,
      });
      await fixture.transport.selectCanonicalBranch(replacementBlock.point);
      const progress = createWatcherSqliteBlockProgressStore({
        database: sqlite,
        authenticationKey: rollbackAuthorityKey,
      });
      for (const saved of [ancestor, middle, releasedBlock])
        progress.record({
          ...saved.point,
          parentBlockHash: saved.parentPoint.blockHash,
          relevance: "touched",
        });
      const synthetic = new Map(
        [ancestor, middle, releasedBlock, pendingBlock, replacementBlock].map(
          (saved) => [saved.point.blockHash, saved],
        ),
      );
      const included: string[] = [];
      const delivered: string[] = [];
      const coordinator = unsafeCreateWatcherChainCoordinatorForTest(
        {
          policy,
          durable: runtime,
          restartIntersection: { kind: "point", ...ancestor.point },
          observation: {
            observe: async ({
              block,
            }: {
              block: WatcherNativeBlockAdmission;
            }) => observe(synthetic.get(block.blockHash)!),
          } as unknown as WatcherLocalKupmiosNativeObservationRuntime,
          hooks: {
            onRollback: async () => undefined,
            onIncluded: async ({ nativeBlock }) => {
              included.push(nativeBlock.blockHash);
            },
            onFinalized: async ({ nativeBlock }) => {
              delivered.push(nativeBlock.blockHash);
            },
          },
        },
        {
          progress,
          admitRollForward: (event) => nativeBlocks.get(event.blockHash)!.block,
        },
      );
      stopCoordinator = () => coordinator.stop();
      await coordinator.handle({
        schemaVersion: "midgard-watcher-native-chain-sync-v1",
        kind: "roll_backward",
        point: { kind: "point", ...ancestor.point },
        tip: { kind: "point", ...replacementBlock.point },
      });
      await observe(middle);
      await coordinator.handle(nativeBlocks.get(middle.point.blockHash)!.event);
      expect(coordinator.status()).toMatchObject({
        rollbackPoint: {
          kind: "point",
          blockHash: middle.point.blockHash,
          slot: middle.point.slot,
        },
        quarantined: false,
        integrityHold: null,
      });
      expect(delivered).toEqual([]);
      expect(included).toEqual([]);
      expect(progress.readHead()?.blockHash).toBe(ancestor.point.blockHash);
      await observe(replacementBlock);
      await coordinator.handle(
        nativeBlocks.get(replacementBlock.point.blockHash)!.event,
      );
      expect(coordinator.status()).toMatchObject({
        rollbackPoint: null,
        quarantined: false,
        integrityHold: null,
      });
      expect(runtime.readFinality().phase).toBe("pending");
      expect(delivered).toEqual([middle.point.blockHash]);
      expect(progress.readHead()?.blockHash).toBe(middle.point.blockHash);
      expect(included).toEqual([replacementBlock.point.blockHash]);
      const child = await fixture.transport.makeBlock({
        transactions: [],
        parent: replacementBlock,
      });
      synthetic.set(child.point.blockHash, child);
      await fixture.transport.selectCanonicalBranch(child.point);
      await observe(child);
      await coordinator.handle(nativeBlocks.get(child.point.blockHash)!.event);
      expect(delivered).toContain(replacementBlock.point.blockHash);
      expect(coordinator.status().processedThrough?.blockHash).toBe(
        replacementBlock.point.blockHash,
      );
      expect(runtime.readFinality().phase).toBe("pending");
    } finally {
      await stopCoordinator?.();
      sqlite.close();
      for (const close of closeObservations.reverse()) await close();
      await fixture.close();
    }
  }, 120_000);

  it("reobserves a retained pending replacement after its logical rollback fence", async () => {
    const fixture = await createSyntheticStateQueueObservationFixture({
      nativeTipMode: "controlled",
    });
    const closeObservations: Array<() => Promise<void>> = [];
    let stopCoordinator: (() => Promise<void>) | undefined;
    try {
      const nativeBlocks = new Map<
        string,
        Readonly<{
          block: WatcherNativeBlockAdmission;
          event: Extract<WatcherNativeChainSyncEvent, { kind: "roll_forward" }>;
        }>
      >();
      const observe = async (block: SyntheticUserEventBlock) => {
        const query = await openWatcherNativeExactPointQuery({
          binaryPath: fixture.transport.nativeChainSyncBinaryPath,
          watcherConfig: fixture.transport.watcherConfig,
          predecessor: {
            blockHash: block.parentPoint.blockHash,
            blockNo: block.parentPoint.blockNo,
            slot: block.parentPoint.slot,
          },
          target: {
            blockHash: block.point.blockHash,
            blockNo: block.point.blockNo,
            slot: block.point.slot,
          },
          timeoutMs: 60_000,
        });
        const details = readWatcherNativeExactPointQuery(query.receipt);
        const admitted = admitWatcherNativeRollForwardBlock(details.event);
        nativeBlocks.set(block.point.blockHash, {
          block: admitted,
          event: details.event,
        });
        const owner: { local?: WatcherLocalKupmiosNativeObservationRuntime } =
          {};
        closeObservations.push(async () => {
          owner.local?.close();
          await query.close();
        });
        const local = await createWatcherLocalKupmiosNativeObservationRuntime({
          watcherConfig: fixture.transport.watcherConfig,
          deploymentIdentity: fixture.transport.deploymentIdentity,
          nativeAuthority: details.authority,
        });
        owner.local = local;
        await readAdmittedLocalKupmiosBoundary({ source: local.rawSource });
        return local.observe({
          block: admitted,
          depth: details.depthAtObservedTip,
        });
      };
      const policy = makeWatcherFinalityPolicy(
        fixture.transport.watcherConfig,
        fixture.transport.deploymentIdentity,
      );
      if (policy === null) throw new Error("Expected signed local policy");
      const backend = new MemoryRollbackAuthorityBackend();
      let head: WatcherRollbackDurableTrustedHead | null = null;
      const runtime = await createWatcherDurableRuntime({
        backend,
        policy,
        authenticationKey: rollbackAuthorityKey,
        client: {
          readRecordAuthenticationKeyId: async () => hex32("99"),
          readCurrent: async () => head,
          compareAndSwap: async ({ expectedTrustedHead, nextTrustedHead }) => {
            if (JSON.stringify(head) !== JSON.stringify(expectedTrustedHead))
              return false;
            head = nextTrustedHead;
            return true;
          },
        },
      });
      const ancestor = fixture.initializationBlock;
      expect(policy.confirmationDepth).toBe("10");
      expect(policy.maximumPreFinalityRollbackDepth).toBe("10");
      await fixture.transport.selectCanonicalBranch(ancestor.point);
      await runtime.persistCanonicalProgress(await observe(ancestor));
      await fixture.transport.growNativeTip(
        Number(policy.confirmationDepth) - 1,
      );
      await runtime.persistCanonicalProgress(await observe(ancestor));
      expect(runtime.readFinality()).toMatchObject({
        phase: "finalized",
        finalized: {
          blockHash: ancestor.point.blockHash,
          currentDepth: "10",
        },
      });
      const ancestorTip = nativeBlocks.get(ancestor.point.blockHash)!.event.tip;
      if (ancestorTip.kind !== "point") throw new Error("Expected native tip");
      expect(BigInt(ancestorTip.blockNo) - BigInt(ancestor.point.blockNo)).toBe(
        9n,
      );
      const orphan = await fixture.transport.makeBlock({
        transactions: [fixture.commitTransactionCbor],
        parent: ancestor,
      });
      await fixture.transport.selectCanonicalBranch(orphan.point);
      await runtime.persistCanonicalProgress(await observe(orphan));
      expect(runtime.readFinality()).toMatchObject({
        phase: "pending",
        pending: { blockHash: orphan.point.blockHash, currentDepth: "1" },
      });
      const replacement = await fixture.transport.makeBlock({
        transactions: [],
        parent: ancestor,
        slot: Number(orphan.point.slot) + 1,
      });
      await fixture.transport.selectCanonicalBranch(replacement.point);
      await observe(replacement);
      const native = nativeBlocks.get(replacement.point.blockHash)!;
      const issued: WatcherLocalKupmiosNativeObservation[] = [];
      const included: WatcherLocalKupmiosNativeObservation[] = [];
      let persistedGuard: (() => void) | undefined;
      let rollbacks = 0;
      const coordinator = unsafeCreateWatcherChainCoordinatorForTest(
        {
          policy,
          durable: {
            ...runtime,
            persistObservation: async (input) => {
              const result = await runtime.persistObservation(input);
              persistedGuard = input.assertCurrent;
              return result;
            },
          },
          restartIntersection: { kind: "point", ...ancestor.point },
          observation: {
            observe: async () => {
              const observation = await observe(replacement);
              issued.push(observation);
              return observation;
            },
          } as unknown as WatcherLocalKupmiosNativeObservationRuntime,
          hooks: {
            onIncluded: async ({ localObservation }) => {
              if (localObservation === null)
                throw new Error("Expected a touched replacement observation");
              included.push(localObservation);
            },
            onRollback: async () => {
              rollbacks += 1;
            },
            onFinalized: async () => {
              throw new Error("Replacement must retain first visibility");
            },
          },
        },
        { admitRollForward: () => native.block },
      );
      stopCoordinator = () => coordinator.stop();
      await coordinator.handle({
        schemaVersion: "midgard-watcher-native-chain-sync-v1",
        kind: "roll_backward",
        point: { kind: "point", ...ancestor.point },
        tip: native.event.tip,
      });
      await coordinator.handle(native.event);
      expect(coordinator.status()).toMatchObject({
        rollbackPoint: null,
        integrityHold: null,
        quarantined: false,
        deliveryHeld: true,
      });
      expect(rollbacks).toBe(1);
      expect(issued).toHaveLength(2);
      expect(issued[1]).not.toBe(issued[0]);
      for (const observation of issued)
        assertWatcherLocalKupmiosNativeObservation(observation, native.block);
      expect(persistedGuard).toBeTypeOf("function");
      expect(() => persistedGuard?.()).not.toThrow();
      expect(included).toHaveLength(0);
      expect(runtime.readFinality()).toMatchObject({
        phase: "pending",
        pending: { blockHash: replacement.point.blockHash },
      });
      await coordinator.resume();
      expect(coordinator.status()).toMatchObject({
        rollbackPoint: null,
        integrityHold: null,
        quarantined: false,
        deliveryHeld: false,
      });
      expect(rollbacks).toBe(1);
      expect(issued).toHaveLength(2);
      expect(() => persistedGuard?.()).not.toThrow();
      expect(included).toHaveLength(1);
      expect(() =>
        assertWatcherLocalKupmiosNativeObservation(included[0]!, native.block),
      ).not.toThrow();
      expect(runtime.readFinality()).toMatchObject({
        phase: "pending",
        pending: { blockHash: replacement.point.blockHash },
      });
      await coordinator.stop();
      stopCoordinator = undefined;
      expect(() => persistedGuard?.()).toThrow(
        "native observation generation changed",
      );
      expect(() =>
        assertWatcherLocalKupmiosNativeObservation(included[0]!, native.block),
      ).toThrow("native observation generation changed");
    } finally {
      await stopCoordinator?.();
      for (const close of closeObservations.reverse()) await close();
      await fixture.close();
    }
  }, 120_000);
});
