import { createHash } from "node:crypto";
import { once } from "node:events";
import { mkdtemp, readFile, rm, writeFile } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";

import * as SDK from "@al-ft/midgard-sdk";
import { Effect } from "effect";
import { Level } from "level";
import { afterEach, beforeAll, describe, expect, it } from "vitest";

import {
  assertNativeOwnerRuntimeMemoryBudget,
  encodeNativeMpfEventLog,
  type NativeMpfEventOp,
  NativeMpfWorkerPortClient,
  parseNativeOwnerCgroupMemoryLimit,
  ProductionNativeMpfOwnerService,
} from "../src/services/mpf-native-owner/index.js";
import { MidgardMpf } from "../src/workers/utils/mpf.js";
import {
  buildNativeProductionRootProbe,
  buildTransactionsSourceRoot,
  buildTransitionTraceResult,
  type NativeMpfBuildContext,
} from "../src/workers/utils/mpf.js";
import {
  createEventFlatDigest,
  prepareEventFlatDigest,
} from "../src/workers/utils/mpf-event-flat-digest.js";
import {
  nativeOwnerBinaryPath,
  nativeOwnerBinaryPresent,
  warnNativeOwnerBinaryAbsent,
} from "./helpers/native-owner-binary.js";

// The service spawns the native owner binary; see the helper for the
// build/skip contract (#642).
const binaryPath = nativeOwnerBinaryPath;
const binaryPresent = nativeOwnerBinaryPresent();
if (!binaryPresent) {
  warnNativeOwnerBinaryAbsent("mpf-native-owner-service");
}

const digest = (...parts: readonly Uint8Array[]): Buffer => {
  const state = createEventFlatDigest();
  for (const part of parts) state.update(part);
  return state.digest();
};

const nibbles = (value: Uint8Array): string =>
  [...digest(value)]
    .flatMap((byte) => [byte >> 4, byte & 15])
    .map((value) => value.toString(16))
    .join("");

const packedNibbles = (value: string): Buffer =>
  Buffer.from(
    Array.from({ length: value.length / 2 }, (_, index) =>
      Number.parseInt(value.slice(index * 2, index * 2 + 2), 16),
    ),
  );

const leafRoot = (key: Buffer, value: Buffer): string => {
  const prefix = nibbles(key);
  return digest(
    Buffer.from([0xff]),
    packedNibbles(prefix),
    digest(value),
  ).toString("hex");
};

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

  it("matches legacy roots, promotes atomically, restarts, and replays recovery", async () => {
    const root = await mkdtemp(join(tmpdir(), "midgard-native-owner-"));
    temporaryPaths.push(root);
    const levelPath = join(root, "ledger");
    const sidecarPath = join(root, "ledger.sidecar");
    const key1 = Buffer.alloc(32, 1);
    const key2 = Buffer.alloc(32, 2);
    const key3 = Buffer.alloc(32, 3);
    const key4 = Buffer.alloc(32, 4);
    const value1 = Buffer.alloc(64, 11);
    const value2 = Buffer.alloc(64, 22);
    const value3 = Buffer.alloc(64, 33);
    const value4 = Buffer.alloc(64, 44);
    const baseRoot = leafRoot(key1, value1);
    const seed = new Level<string, unknown>(levelPath, {
      valueEncoding: "json",
    });
    await seed.open();
    await seed.batch([
      {
        type: "put",
        key: baseRoot,
        value: {
          __kind: "Leaf",
          prefix: nibbles(key1),
          key: key1.toString("hex"),
          value: value1.toString("hex"),
        },
      },
      { type: "put", key: "__root__", value: baseRoot },
    ]);
    await seed.close();

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
    const legacy = await Effect.runPromise(
      MidgardMpf.createScratch("legacy-owner"),
    );
    await Effect.runPromise(legacy.insert(key1, value1));
    const expectedRoots: string[] = [];
    for (const operations of events) {
      expectedRoots.push(
        (
          await Effect.runPromise(
            legacy.applyBatch(
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
    await Effect.runPromise(legacy.close());
    const legacyTraceMpf = await Effect.runPromise(
      MidgardMpf.createScratch("legacy-owner-transition-trace"),
    );
    await Effect.runPromise(legacyTraceMpf.insert(key1, value1));
    const expectedTrace = await Effect.runPromise(
      buildTransitionTraceResult({
        ledgerMpf: legacyTraceMpf,
        sourceEvents,
        withdrawalCount: 0,
        forcedTransactionCount: 0,
        l2TransactionCount: events.length,
        depositCount: 0,
      }),
    );
    await Effect.runPromise(legacyTraceMpf.close());

    let service = await ProductionNativeMpfOwnerService.create({
      levelPath,
      binaryPath,
      binarySha256,
      maxChunkBytes: 200,
      sidecarPath,
    });
    expect(await service.diagnostics()).toMatchObject({
      durableRoot: baseRoot,
      residentNodes: 1,
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
    const legacyTransactions = await Effect.runPromise(
      MidgardMpf.createScratch("legacy-owner-transactions"),
    );
    await Effect.runPromise(legacyTransactions.applyBatch(transactionOps));
    const expectedRawTxRoot = await Effect.runPromise(
      legacyTransactions.rootHex(),
    );
    await Effect.runPromise(legacyTransactions.close());
    const expectedTxRoot = await Effect.runPromise(
      buildTransactionsSourceRoot(transactionOps),
    );
    const productionRoots = await Effect.runPromise(
      buildNativeProductionRootProbe({
        nativeMpf: nativeContext,
        sourceEvents,
        transactionOps,
      }),
    );
    const trace = productionRoots.transitionTraceBuild;
    expect(nativeContext.eventRoots).toEqual(expectedRoots);
    expect(nativeContext.candidateRoot).toBe(expectedRoots.at(-1));
    expect(trace.finalUtxosRoot).toBe(expectedRoots.at(-1));
    expect(productionRoots).toMatchObject({
      utxoRoot: expectedRoots.at(-1),
      rawTxRoot: expectedRawTxRoot,
      txRoot: expectedTxRoot,
      transitionTraceRoot: expectedTrace.transitionTraceRoot,
      eventToStepRoot: expectedTrace.eventToStepRoot,
      depositsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
      withdrawalsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
      forcedTransactionsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
    });
    expect(productionRoots.rawTxRoot).toMatch(/^[0-9a-f]{64}$/);
    expect(productionRoots.txRoot).toMatch(/^[0-9a-f]{64}$/);
    expect(productionRoots.depositsRoot).toMatch(/^[0-9a-f]{64}$/);
    expect(productionRoots.withdrawalsRoot).toMatch(/^[0-9a-f]{64}$/);
    expect(productionRoots.forcedTransactionsRoot).toMatch(/^[0-9a-f]{64}$/);
    expect(productionRoots.transitionRoots).toEqual(
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

    service = await ProductionNativeMpfOwnerService.create({
      levelPath,
      binaryPath,
      binarySha256,
      maxChunkBytes: 211,
      sidecarPath,
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

    const corruptSidecar = await readFile(sidecarPath);
    corruptSidecar[corruptSidecar.length - 1] ^= 1;
    await writeFile(sidecarPath, corruptSidecar);

    service = await ProductionNativeMpfOwnerService.create({
      levelPath,
      binaryPath,
      binarySha256,
      sidecarPath,
    });
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

    const verify = new Level(levelPath, { valueEncoding: "json" });
    await verify.open();
    expect(await verify.get("__root__")).toBe(recoveryApplied.candidateRoot);
    await verify.close();
  });

  it("pins the native binary SHA before opening the durable Level path", async () => {
    const root = await mkdtemp(join(tmpdir(), "midgard-native-owner-sha-"));
    temporaryPaths.push(root);
    await expect(
      ProductionNativeMpfOwnerService.create({
        levelPath: join(root, "absent-level"),
        binaryPath,
        binarySha256: "00".repeat(32),
      }),
    ).rejects.toThrow(/binary SHA-256 mismatch/);
  });

  it("enforces native frame and chunk caps before opening Level", async () => {
    const root = await mkdtemp(join(tmpdir(), "midgard-native-owner-caps-"));
    temporaryPaths.push(root);
    await expect(
      ProductionNativeMpfOwnerService.create({
        levelPath: join(root, "zero-chunk-level"),
        binaryPath,
        binarySha256,
        maxChunkBytes: 0,
      }),
    ).rejects.toThrow(/maxChunkBytes must be a positive safe integer/);
    await expect(
      ProductionNativeMpfOwnerService.create({
        levelPath: join(root, "oversized-frame-level"),
        binaryPath,
        binarySha256,
        maxFrameBytes: 64 * 1024 * 1024 + 1,
      }),
    ).rejects.toThrow(/maxFrameBytes exceeds the compiled native owner cap/);
  });

  it("requires explicit cgroup headroom above the V8 heap and native owner cap", () => {
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
        v8HeapLimitBytes: 6 * 1024 * 1024 * 1024,
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
    const baseRoot = leafRoot(key1, value1);
    const seed = new Level<string, unknown>(levelPath, {
      valueEncoding: "json",
    });
    await seed.open();
    await seed.batch([
      {
        type: "put",
        key: baseRoot,
        value: {
          __kind: "Leaf",
          prefix: nibbles(key1),
          key: key1.toString("hex"),
          value: value1.toString("hex"),
        },
      },
      { type: "put", key: "__root__", value: baseRoot },
    ]);
    await seed.close();

    const childPids: number[] = [];
    const service = await ProductionNativeMpfOwnerService.create({
      levelPath,
      binaryPath,
      binarySha256,
      restartLimit: 1,
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
      const sidecarPath = join(root, "ledger.sidecar");
      const key1 = Buffer.alloc(32, 51);
      const key2 = Buffer.alloc(32, 52);
      const value1 = Buffer.alloc(64, 61);
      const value2 = Buffer.alloc(64, 62);
      const baseRoot = leafRoot(key1, value1);
      const seed = new Level<string, unknown>(levelPath, {
        valueEncoding: "json",
      });
      await seed.open();
      await seed.batch([
        {
          type: "put",
          key: baseRoot,
          value: {
            __kind: "Leaf",
            prefix: nibbles(key1),
            key: key1.toString("hex"),
            value: value1.toString("hex"),
          },
        },
        { type: "put", key: "__root__", value: baseRoot },
      ]);
      await seed.close();

      const log = encodeNativeMpfEventLog(baseRoot, [
        [{ type: "insert", key: key2, value: value2 }],
      ]);
      let service = await ProductionNativeMpfOwnerService.create({
        levelPath,
        sidecarPath,
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

      const inspect = new Level(levelPath, { valueEncoding: "json" });
      await inspect.open();
      expect(await inspect.get("__root__")).toBe(
        point === "before_promotion_batch" ? baseRoot : applied.candidateRoot,
      );
      await inspect.close();

      service = await ProductionNativeMpfOwnerService.create({
        levelPath,
        sidecarPath,
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
    }
  });
});
