import { createHash } from "node:crypto";
import { once } from "node:events";
import { mkdtemp, readFile, rm } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";

import * as SDK from "@al-ft/midgard-sdk/core";
import { Effect } from "effect";
import { afterEach, beforeAll, describe, expect, it } from "vitest";

import { MidgardMpf } from "../src/mpf/store.midgard-mpf.js";
import {
  buildTransactionsSourceRoot,
  type NativeMpfBuildContext,
} from "../src/mpf/transition-trace.apply-trace-ledger-ops-to-mpf.js";
import { buildNativeRootProbe } from "../src/mpf/transition-trace.build-native-root-probe.js";
import { buildTransitionTraceResult } from "../src/mpf/transition-trace.build-transition-trace-result.js";
import { NativeMpfWorkerPortClient } from "../src/services/mpf-native-owner/client.js";
import {
  assertNativeOwnerRuntimeMemoryBudget,
  type NativeMpfEventOp,
  parseNativeOwnerCgroupMemoryLimit,
} from "../src/services/mpf-native-owner/service.normalize-owner-options.js";
import { openNativeOwnerLedger } from "../src/services/mpf-native-owner/service.open-ledger.js";
import { encodeNativeMpfEventLog } from "../src/services/mpf-native-owner/service.parse-promotion-records.js";
import { prepareEventFlatDigest } from "../src/workers/utils/mpf-event-flat-digest.js";
import {
  nativeOwnerBinaryPath,
  nativeOwnerBinaryPresent,
  warnNativeOwnerBinaryAbsent,
} from "./helpers/native-owner-binary.js";
import {
  createOwnerService,
  expectCountedLedger,
  leafKeys,
  leafRoot,
  ledgerStoreRecords,
  reachableNodes,
  refuseOutputs,
  seedLeafLedger,
  seedOwnerLedger,
} from "./helpers/native-owner-ledger.js";

// The service spawns the native owner binary; see the helper for the
// build/skip contract.
const binaryPath = nativeOwnerBinaryPath;
const binaryPresent = nativeOwnerBinaryPresent();
if (!binaryPresent) {
  warnNativeOwnerBinaryAbsent("mpf-native-owner-service");
}

const event = (
  deleted: Buffer,
  inserted: Buffer,
  value: Buffer,
): readonly NativeMpfEventOp[] => [
  { type: "delete", key: deleted },
  { type: "insert", key: inserted, value },
];

describe.skipIf(!binaryPresent)("production native MPF owner service", () => {
  const temporaryPaths: string[] = [];
  let binarySha256 = "";

  beforeAll(async () => {
    await prepareEventFlatDigest();
    binarySha256 = createHash("sha256")
      .update(await readFile(binaryPath))
      .digest("hex");
  });

  afterEach(async () => {
    await Promise.all(
      temporaryPaths
        .splice(0)
        .map((path) => rm(path, { recursive: true, force: true })),
    );
  });

  it("starts from genuine empty genesis and promotes insertion and final deletion across restart", async () => {
    const root = await mkdtemp(join(tmpdir(), "midgard-native-empty-"));
    temporaryPaths.push(root);
    const levelPath = join(root, "ledger");
    const emptyRoot = SDK.EMPTY_MERKLE_TREE_ROOT;
    await seedOwnerLedger(levelPath, emptyRoot);
    const options = { binaryPath, binarySha256 };
    const key = Buffer.alloc(32, 91);
    const value = Buffer.alloc(64, 92);
    let service = await createOwnerService(levelPath, options);
    try {
      expect(await service.diagnostics()).toMatchObject({
        durableRoot: emptyRoot,
        cachedNodes: 0,
      });
      const inserted = await service.fork(emptyRoot);
      const result = await service.applyEvents(
        inserted,
        encodeNativeMpfEventLog(emptyRoot, [[{ type: "insert", key, value }]]),
      );
      expect(result.candidateRoot).toBe(leafRoot(key, value));
      await service.promote(inserted);
      await service.close();
      service = await createOwnerService(levelPath, options);
      expect(await service.diagnostics()).toMatchObject({
        durableRoot: result.candidateRoot,
        cachedNodes: 0,
      });
      const deleted = await service.fork(result.candidateRoot);
      const removed = await service.applyEvents(
        deleted,
        encodeNativeMpfEventLog(result.candidateRoot, [
          [{ type: "delete", key }],
        ]),
      );
      expect(removed.candidateRoot).toBe(emptyRoot);
      await service.promote(deleted);
      await service.close();
      service = await createOwnerService(levelPath, options);
      expect(await service.diagnostics()).toMatchObject({
        durableRoot: emptyRoot,
        cachedNodes: 0,
      });
    } finally {
      await service.close();
    }
  });

  it("matches scratch-trie roots, promotes atomically, restarts, and replays recovery", async () => {
    const root = await mkdtemp(join(tmpdir(), "midgard-native-owner-"));
    temporaryPaths.push(root);
    const levelPath = join(root, "ledger");
    const key1 = Buffer.alloc(32, 1);
    const key2 = Buffer.alloc(32, 2);
    const key3 = Buffer.alloc(32, 3);
    const key4 = Buffer.alloc(32, 4);
    const value1 = Buffer.alloc(64, 11);
    const value2 = Buffer.alloc(64, 22);
    const value3 = Buffer.alloc(64, 33);
    const value4 = Buffer.alloc(64, 44);
    const baseRoot = await seedLeafLedger(levelPath, key1, value1);

    const events: readonly (readonly NativeMpfEventOp[])[] = [
      [{ type: "insert", key: key2, value: value2 }],
      [],
      [{ type: "insert", key: key3, value: value3 }],
    ];
    const sourceEvents = events.map((ledgerOps, index) => ({
      phase: "L2Transaction" as const,
      eventKey: {
        L2TransactionEventKey: {
          tx_id: Buffer.alloc(32, index + 1).toString("hex"),
        },
      } as SDK.EventKey,
      ledgerOps: ledgerOps.map((op) =>
        op.type === "insert"
          ? {
              type: "insert" as const,
              key: Buffer.from(op.key),
              value: Buffer.from(op.value),
            }
          : { type: "delete" as const, key: Buffer.from(op.key) },
      ),
    }));
    const reference = await Effect.runPromise(
      MidgardMpf.createScratch("reference-owner"),
    );
    await Effect.runPromise(reference.insert(key1, value1));
    const expectedRoots: string[] = [];
    for (const operations of events) {
      expectedRoots.push(
        (
          await Effect.runPromise(
            reference.applyBatch(
              operations.map((op) =>
                op.type === "insert"
                  ? {
                      type: "insert" as const,
                      key: Buffer.from(op.key),
                      value: Buffer.from(op.value),
                    }
                  : { type: "delete" as const, key: Buffer.from(op.key) },
              ),
            ),
          )
        ).toString("hex"),
      );
    }
    await Effect.runPromise(reference.close());
    const referenceTraceMpf = await Effect.runPromise(
      MidgardMpf.createScratch("reference-owner-transition-trace"),
    );
    await Effect.runPromise(referenceTraceMpf.insert(key1, value1));
    const expectedTrace = await Effect.runPromise(
      buildTransitionTraceResult({
        ledgerMpf: referenceTraceMpf,
        sourceEvents,
        withdrawalCount: 0,
        forcedTransactionCount: 0,
        l2TransactionCount: events.length,
        depositCount: 0,
      }),
    );
    await Effect.runPromise(referenceTraceMpf.close());

    let service = await createOwnerService(levelPath, {
      binaryPath,
      binarySha256,
      maxChunkBytes: 200,
    });
    expect(await service.diagnostics()).toMatchObject({
      durableRoot: baseRoot,
      cachedNodes: 0,
      activeGenerations: 0,
    });
    const handle = await service.fork(baseRoot);
    const nativeContext: NativeMpfBuildContext = {
      client: service,
      handle,
      ownerBinarySha256: binarySha256,
    };
    const transactionOps = events.map((_, index) => ({
      type: "insert" as const,
      key: Buffer.alloc(32, index + 71),
      value: Buffer.alloc(48, index + 81),
    }));
    const referenceTransactions = await Effect.runPromise(
      MidgardMpf.createScratch("reference-owner-transactions"),
    );
    await Effect.runPromise(referenceTransactions.applyBatch(transactionOps));
    const expectedRawTxRoot = await Effect.runPromise(
      referenceTransactions.rootHex(),
    );
    await Effect.runPromise(referenceTransactions.close());
    const expectedTxRoot = await Effect.runPromise(
      buildTransactionsSourceRoot(transactionOps),
    );
    const roots = await Effect.runPromise(
      buildNativeRootProbe({
        nativeMpf: nativeContext,
        sourceEvents,
        transactionOps,
      }),
    );
    const trace = roots.transitionTraceBuild;
    expect(nativeContext.eventRoots).toEqual(expectedRoots);
    expect(nativeContext.candidateRoot).toBe(expectedRoots.at(-1));
    expect(trace.finalUtxosRoot).toBe(expectedRoots.at(-1));
    expect(roots).toMatchObject({
      utxoRoot: expectedRoots.at(-1),
      rawTxRoot: expectedRawTxRoot,
      txRoot: expectedTxRoot,
      transitionTraceRoot: expectedTrace.transitionTraceRoot,
      eventToStepRoot: expectedTrace.eventToStepRoot,
      depositsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
      withdrawalsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
      forcedTransactionsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
    });
    expect(roots.rawTxRoot).toMatch(/^[0-9a-f]{64}$/);
    expect(roots.txRoot).toMatch(/^[0-9a-f]{64}$/);
    expect(roots.depositsRoot).toMatch(/^[0-9a-f]{64}$/);
    expect(roots.withdrawalsRoot).toMatch(/^[0-9a-f]{64}$/);
    expect(roots.forcedTransactionsRoot).toMatch(/^[0-9a-f]{64}$/);
    expect(roots.transitionRoots).toEqual(
      trace.transitionTraceMembers.map((member) => ({
        pre: member.value.pre_utxos_root,
        post: member.value.post_utxos_root,
      })),
    );
    const applied = {
      candidateRoot: nativeContext.candidateRoot!,
      eventRoots: nativeContext.eventRoots!,
      eventLogDigest: nativeContext.eventLogDigest!,
    };

    const worker = new NativeMpfWorkerPortClient(service.createWorkerPort());
    await worker.fork(baseRoot);
    worker.close();
    const workerReleaseDeadline = Date.now() + 2_000;
    while (
      (await service.diagnostics()).activeGenerations !== 1 &&
      Date.now() < workerReleaseDeadline
    ) {
      await new Promise((resolve) => setTimeout(resolve, 10));
    }
    expect((await service.diagnostics()).activeGenerations).toBe(1);

    const duplicateRequestPort = service.createWorkerPort();
    duplicateRequestPort.postMessage({
      requestId: 1,
      method: "fork",
      baseRoot,
    });
    await once(duplicateRequestPort, "message");
    expect((await service.diagnostics()).activeGenerations).toBe(2);
    duplicateRequestPort.postMessage({
      requestId: 1,
      method: "fork",
      baseRoot,
    });
    const duplicateReleaseDeadline = Date.now() + 2_000;
    while (
      (await service.diagnostics()).activeGenerations !== 1 &&
      Date.now() < duplicateReleaseDeadline
    ) {
      await new Promise((resolve) => setTimeout(resolve, 10));
    }
    expect((await service.diagnostics()).activeGenerations).toBe(1);

    const racingClosePort = service.createWorkerPort();
    racingClosePort.postMessage({ requestId: 1, method: "fork", baseRoot });
    racingClosePort.close();
    await new Promise((resolve) => setTimeout(resolve, 50));
    expect((await service.diagnostics()).activeGenerations).toBe(1);

    await service.promote(handle);
    expect(await service.diagnostics()).toMatchObject({
      durableRoot: applied.candidateRoot,
      activeGenerations: 0,
    });
    const retainedWorker = new NativeMpfWorkerPortClient(
      service.createWorkerPort(),
    );
    const retainedHandle = await retainedWorker.fork(applied.candidateRoot);
    await retainedWorker.retainForJournal(retainedHandle);
    retainedWorker.close();
    expect((await service.diagnostics()).activeGenerations).toBe(1);
    await service.discard(retainedHandle);
    expect((await service.diagnostics()).activeGenerations).toBe(0);
    await service.close();

    service = await createOwnerService(levelPath, {
      binaryPath,
      binarySha256,
      maxChunkBytes: 211,
    });
    expect((await service.diagnostics()).durableRoot).toBe(
      applied.candidateRoot,
    );
    const recoveryEvents = [event(key2, key4, value4)];
    const recoveryLog = encodeNativeMpfEventLog(
      applied.candidateRoot,
      recoveryEvents,
    );
    const recoveryHandle = await service.fork(applied.candidateRoot);
    const recoveryApplied = await service.applyEvents(
      recoveryHandle,
      recoveryLog,
    );
    await service.close();

    service = await createOwnerService(levelPath, { binaryPath, binarySha256 });
    await service.recover({
      schema: 1,
      ownerBinarySha256: binarySha256,
      baseRoot: applied.candidateRoot,
      candidateRoot: recoveryApplied.candidateRoot,
      eventLog: recoveryLog,
      eventLogDigest: recoveryApplied.eventLogDigest,
      eventRoots: Buffer.from(recoveryApplied.eventRoots.join(""), "hex"),
      eventCount: recoveryApplied.eventRoots.length,
    });
    expect((await service.diagnostics()).durableRoot).toBe(
      recoveryApplied.candidateRoot,
    );
    await service.close();

    expect((await ledgerStoreRecords(levelPath)).marker).toBe(
      recoveryApplied.candidateRoot,
    );
  });

  // A refused creation must release the LevelDB lock it was handed, or the
  // node's next start cannot open its ledger.
  const expectRefusedReleasingLedger = async (
    levelPath: string,
    options: Parameters<typeof createOwnerService>[1],
    refusal: RegExp,
  ) => {
    await expect(createOwnerService(levelPath, options)).rejects.toThrow(
      refusal,
    );
    await (await openNativeOwnerLedger(levelPath, refuseOutputs)).level.close();
  };

  it("refuses a native binary whose SHA differs from the pin and releases the ledger", async () => {
    const root = await mkdtemp(join(tmpdir(), "midgard-native-owner-sha-"));
    temporaryPaths.push(root);
    await expectRefusedReleasingLedger(
      join(root, "ledger"),
      { binaryPath, binarySha256: "00".repeat(32) },
      /binary SHA-256 mismatch/,
    );
  });

  it("enforces native frame and chunk caps and releases the ledger", async () => {
    const root = await mkdtemp(join(tmpdir(), "midgard-native-owner-caps-"));
    temporaryPaths.push(root);
    await expectRefusedReleasingLedger(
      join(root, "zero-chunk-level"),
      { binaryPath, binarySha256, maxChunkBytes: 0 },
      /maxChunkBytes must be a positive safe integer/,
    );
    await expectRefusedReleasingLedger(
      join(root, "oversized-frame-level"),
      { binaryPath, binarySha256, maxFrameBytes: 64 * 1024 * 1024 + 1 },
      /maxFrameBytes exceeds the compiled native owner cap/,
    );
  });

  it("requires explicit cgroup headroom above the V8 heap and the native owner's cache budget", () => {
    expect(parseNativeOwnerCgroupMemoryLimit("max\n")).toEqual({
      kind: "unlimited",
    });
    expect(parseNativeOwnerCgroupMemoryLimit("9223372036854771712")).toEqual({
      kind: "unlimited",
    });
    expect(parseNativeOwnerCgroupMemoryLimit("8589934592")).toEqual({
      kind: "finite",
      limitBytes: 8 * 1024 * 1024 * 1024,
    });
    expect(parseNativeOwnerCgroupMemoryLimit("0")).toBeUndefined();
    expect(() =>
      assertNativeOwnerRuntimeMemoryBudget({
        cgroup: {
          kind: "finite",
          limitBytes: 8 * 1024 * 1024 * 1024,
        },
        v8HeapLimitBytes: 7 * 1024 * 1024 * 1024,
      }),
    ).toThrow(/runtime memory budget is insufficient/);
    expect(() =>
      assertNativeOwnerRuntimeMemoryBudget({
        cgroup: {
          kind: "finite",
          limitBytes: 8 * 1024 * 1024 * 1024,
        },
        v8HeapLimitBytes: 4 * 1024 * 1024 * 1024,
      }),
    ).not.toThrow();
    expect(() =>
      assertNativeOwnerRuntimeMemoryBudget({
        cgroup: { kind: "unlimited" },
        v8HeapLimitBytes: 4 * 1024 * 1024 * 1024,
      }),
    ).not.toThrow();
    expect(() =>
      assertNativeOwnerRuntimeMemoryBudget({
        cgroup: { kind: "unavailable", containerized: false },
        v8HeapLimitBytes: 4 * 1024 * 1024 * 1024,
      }),
    ).not.toThrow();
    expect(() =>
      assertNativeOwnerRuntimeMemoryBudget({
        cgroup: { kind: "unavailable", containerized: true },
        v8HeapLimitBytes: 4 * 1024 * 1024 * 1024,
      }),
    ).toThrow(/cannot prove an enforceable container memory budget/);
  });

  it("supervises a killed native child and rejects handles from its stale epoch", async () => {
    const root = await mkdtemp(join(tmpdir(), "midgard-native-owner-restart-"));
    temporaryPaths.push(root);
    const levelPath = join(root, "ledger");
    const key1 = Buffer.alloc(32, 41);
    const key2 = Buffer.alloc(32, 42);
    const value1 = Buffer.alloc(64, 51);
    const value2 = Buffer.alloc(64, 52);
    const baseRoot = await seedLeafLedger(levelPath, key1, value1);

    const childPids: number[] = [];
    const service = await createOwnerService(levelPath, {
      binaryPath,
      binarySha256,
      onChildSpawnForTests(pid) {
        childPids.push(pid);
      },
    });
    const staleHandle = await service.fork(baseRoot);
    process.kill(childPids[0]!, "SIGKILL");
    const restartDeadline = Date.now() + 5_000;
    while (childPids.length < 2 && Date.now() < restartDeadline) {
      await new Promise((resolve) => setTimeout(resolve, 10));
    }
    expect(childPids).toHaveLength(2);
    const diagnostics = await service.diagnostics();
    expect(diagnostics.childRestarts).toBe(1);
    expect(diagnostics.durableRoot).toBe(baseRoot);
    expect(Buffer.from(diagnostics.ownerEpoch)).not.toEqual(
      Buffer.from(staleHandle.ownerEpoch),
    );
    const eventLog = encodeNativeMpfEventLog(baseRoot, [
      [{ type: "insert", key: key2, value: value2 }],
    ]);
    await expect(service.applyEvents(staleHandle, eventLog)).rejects.toThrow(
      /stale owner epoch/,
    );
    const handle = await service.fork(baseRoot);
    const applied = await service.applyEvents(handle, eventLog);
    await service.promote(handle);
    expect((await service.diagnostics()).durableRoot).toBe(
      applied.candidateRoot,
    );
    await service.close();
  });

  it("counts nodes two promotions share once per listing and keeps the first root open", async () => {
    const root = await mkdtemp(join(tmpdir(), "midgard-native-owner-two-"));
    temporaryPaths.push(root);
    const levelPath = join(root, "ledger");
    const [key1, key2, key3] = [1, 2, 3].map((n) => Buffer.alloc(32, n + 60));
    const value = Buffer.alloc(64, 70);
    const baseRoot = await seedLeafLedger(levelPath, key1!, value);
    const service = await createOwnerService(levelPath, {
      binaryPath,
      binarySha256,
    });
    const promote = async (from: string, key: Buffer) => {
      const handle = await service.fork(from);
      const applied = await service.applyEvents(
        handle,
        encodeNativeMpfEventLog(from, [[{ type: "insert", key, value }]]),
      );
      await service.promote(handle);
      return applied.candidateRoot;
    };
    const first = await promote(baseRoot, key2!);
    const second = await promote(first, key3!);
    await service.close();

    const records = await ledgerStoreRecords(levelPath, [first]);
    expect(records.marker).toBe(second);
    expectCountedLedger(records);
    const underSecond = reachableNodes(records, second);
    const shared = new Set(
      [...reachableNodes(records, first)].filter((hash) =>
        underSecond.has(hash),
      ),
    );
    // A shared node no shared branch lists is listed once under each root.
    const listedByShared = new Set(
      [...shared].flatMap((hash) => {
        const node = records.nodes.get(hash)!;
        return node.__kind === "Branch" ? node.children : [];
      }),
    );
    const topShared = [...shared].filter((hash) => !listedByShared.has(hash));
    expect(topShared.length).toBeGreaterThan(0);
    for (const hash of topShared)
      expect(records.counts.get(hash)).toBeGreaterThanOrEqual(2);
    expect(records.opened.get(first)).toBe(true);
    expect(leafKeys(records, first)).toEqual(
      new Set([key1!.toString("hex"), key2!.toString("hex")]),
    );
  });

  it("recovers the authoritative old-or-candidate marker across both promotion crash boundaries", async () => {
    for (const point of [
      "before_promotion_batch",
      "after_promotion_batch_before_ack",
    ] as const) {
      const root = await mkdtemp(
        join(tmpdir(), `midgard-native-owner-crash-${point}-`),
      );
      temporaryPaths.push(root);
      const levelPath = join(root, "ledger");
      const key1 = Buffer.alloc(32, 51);
      const key2 = Buffer.alloc(32, 52);
      const value1 = Buffer.alloc(64, 61);
      const value2 = Buffer.alloc(64, 62);
      const baseRoot = await seedLeafLedger(levelPath, key1, value1);

      const log = encodeNativeMpfEventLog(baseRoot, [
        [{ type: "insert", key: key2, value: value2 }],
      ]);
      let service = await createOwnerService(levelPath, {
        binaryPath,
        binarySha256,
        faultInjectionForTests(crashPoint) {
          if (crashPoint === point) throw new Error(`injected:${point}`);
        },
      });
      const handle = await service.fork(baseRoot);
      const applied = await service.applyEvents(handle, log);
      await expect(service.promote(handle)).rejects.toThrow(
        `injected:${point}`,
      );
      await service.close();

      // The promotion's write landed whole or not at all: nodes, counts,
      // retained root and marker together.
      const crashed = await ledgerStoreRecords(levelPath);
      expect(crashed.marker).toBe(
        point === "before_promotion_batch" ? baseRoot : applied.candidateRoot,
      );
      expectCountedLedger(crashed);
      expect(crashed.nodes.has(applied.candidateRoot)).toBe(
        point !== "before_promotion_batch",
      );

      service = await createOwnerService(levelPath, {
        binaryPath,
        binarySha256,
      });
      await service.recover({
        schema: 1,
        ownerBinarySha256: binarySha256,
        baseRoot,
        candidateRoot: applied.candidateRoot,
        eventLog: log,
        eventLogDigest: applied.eventLogDigest,
        eventRoots: Buffer.from(applied.eventRoots.join(""), "hex"),
        eventCount: applied.eventRoots.length,
      });
      expect((await service.diagnostics()).durableRoot).toBe(
        applied.candidateRoot,
      );
      await service.close();
      const recovered = await ledgerStoreRecords(levelPath);
      expect(recovered.marker).toBe(applied.candidateRoot);
      expectCountedLedger(recovered);
    }
  });
});
