import { createHash } from "node:crypto";
import { mkdtemp, readFile, rm } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";

import { LedgerNotRetained } from "@al-ft/midgard-fault-proofs/ledger-store";
import * as SDK from "@al-ft/midgard-sdk/core";
import { Level } from "level";
import { afterEach, beforeAll, describe, expect, it } from "vitest";

import {
  NativeMpfRestoreReadFailed,
  NativeMpfRootNotRetained,
} from "../src/services/mpf-native-owner/protocol.js";
import type { NativeMpfOwnerServiceOptions } from "../src/services/mpf-native-owner/service.js";
import { encodeNativeMpfEventLog } from "../src/services/mpf-native-owner/service.parse-promotion-records.js";
import { ProductionNativeMpfOwnerService } from "../src/services/mpf-native-owner/service.production-native-mpf-owner-service.js";
import { prepareEventFlatDigest } from "../src/workers/utils/mpf-event-flat-digest.js";
import { nativeOwnerBinaryPath } from "./helpers/native-owner-binary.js";
import { createOwnerService } from "./helpers/native-owner-ledger.js";
import {
  openCountingLedger,
  promoteBlock,
  referenceRoots,
  seedTrieLedger,
  trieBlocks,
  trieInsert,
} from "./helpers/native-owner-trie.js";

// This suite checks the native durable boundary with the actual pinned child.
describe("native canonical root recovery", () => {
  const paths: string[] = [];
  const services = new Set<ProductionNativeMpfOwnerService>();
  let binarySha256: string;
  const ancestorKey = Buffer.alloc(32, 41);
  const ancestorValue = Buffer.alloc(64, 51);

  beforeAll(async () => {
    await prepareEventFlatDigest();
    binarySha256 = createHash("sha256")
      .update(await readFile(nativeOwnerBinaryPath))
      .digest("hex");
  });

  afterEach(async () => {
    for (const service of services) await service.close();
    services.clear();
    await Promise.all(
      paths.splice(0).map((path) => rm(path, { recursive: true, force: true })),
    );
  });

  const open = async (
    options: NativeMpfOwnerServiceOptions & { readonly levelPath: string },
  ) => {
    const service = await createOwnerService(options.levelPath, options);
    services.add(service);
    return service;
  };
  const close = async (service: ProductionNativeMpfOwnerService) => {
    await service.close();
    services.delete(service);
  };
  const inspectStore = async (levelPath: string, recoveryId: string) => {
    const db = new Level<string, unknown>(levelPath, { valueEncoding: "json" });
    await db.open();
    try {
      return {
        root: await db.get("__root__"),
        receipt: await db.get(`__canonical_recovery__:${recoveryId}`),
      };
    } finally {
      await db.close();
    }
  };
  const fixture = async (
    faultInjectionForTests?: NativeMpfOwnerServiceOptions["faultInjectionForTests"],
  ) => {
    const directory = await mkdtemp(join(tmpdir(), "midgard-native-restore-"));
    paths.push(directory);
    const options = {
      levelPath: join(directory, "ledger"),
      binaryPath: nativeOwnerBinaryPath,
      binarySha256,
    };
    const seed = new Level<string, unknown>(options.levelPath, {
      valueEncoding: "json",
    });
    await seed.open();
    await seed.put("__root__", SDK.EMPTY_MERKLE_TREE_ROOT);
    await seed.close();
    const service = await open({ ...options, faultInjectionForTests });
    const first = await service.fork(SDK.EMPTY_MERKLE_TREE_ROOT);
    const ancestor = await service.applyEvents(
      first,
      encodeNativeMpfEventLog(SDK.EMPTY_MERKLE_TREE_ROOT, [
        [{ type: "insert", key: ancestorKey, value: ancestorValue }],
      ]),
    );
    await service.promote(first);
    const second = await service.fork(ancestor.candidateRoot);
    const descendant = await service.applyEvents(
      second,
      encodeNativeMpfEventLog(ancestor.candidateRoot, [
        [
          {
            type: "insert",
            key: Buffer.alloc(32, 42),
            value: Buffer.alloc(64, 52),
          },
        ],
      ]),
    );
    await service.promote(second);
    expect(descendant.candidateRoot).not.toBe(ancestor.candidateRoot);
    const plan = {
      recoveryId: Buffer.alloc(32, 61).toString("hex"),
      expectedRoot: descendant.candidateRoot,
      targetRoot: ancestor.candidateRoot,
    };
    expect((await service.diagnostics()).durableRoot).toBe(plan.expectedRoot);
    return { service, options, plan };
  };

  const assertAncestorContents = async (
    service: ProductionNativeMpfOwnerService,
    root: string,
  ) => {
    const generation = await service.fork(root);
    try {
      const removed = await service.applyEvents(
        generation,
        encodeNativeMpfEventLog(root, [[{ type: "delete", key: ancestorKey }]]),
      );
      // A marker-only change or descendant closure would leave a nonempty trie.
      expect(removed.candidateRoot).toBe(SDK.EMPTY_MERKLE_TREE_ROOT);
    } finally {
      await service.discard(generation);
    }
    expect((await service.diagnostics()).durableRoot).toBe(root);
  };

  it("checks out the retained ancestor in the same child and rejects displaced handles", async () => {
    const { service, plan } = await fixture();
    const old = await service.fork(plan.expectedRoot);
    await service.discard(old);
    const before = await service.diagnostics();
    await service.restoreCanonicalRoot(plan);
    const after = await service.diagnostics();
    expect(after.durableRoot).toBe(plan.targetRoot);
    expect(Buffer.from(after.ownerEpoch)).toEqual(
      Buffer.from(before.ownerEpoch),
    );
    expect(after.childRestarts).toBe(before.childRestarts);
    expect(after.activeGenerations).toBe(0);
    await expect(
      service.applyEvents(
        old,
        encodeNativeMpfEventLog(plan.expectedRoot, [[]]),
      ),
    ).rejects.toThrow(/generation handle is stale/);
    await expect(service.fork(plan.expectedRoot)).rejects.toThrow(
      /fork base is stale/,
    );
    await assertAncestorContents(service, plan.targetRoot);
  });

  it("moves a warm owner back to a retained root in the same child without reading a node, then reads only what the next block needs", async () => {
    const keyCount = 8_000;
    const [first, second] = trieBlocks({ keyCount, blocks: 2 });
    // Inserts only, so it applies wherever the first block left the keys.
    const replacement = [0, 1, 2].map((event) =>
      Array.from({ length: 16 }, (_, index) =>
        trieInsert(7_000_000 + event * 16 + index),
      ),
    );
    const forward = await referenceRoots(keyCount, [first!, second!]);
    const back = await referenceRoots(keyCount, [first!, replacement]);
    const directory = await mkdtemp(
      join(tmpdir(), "midgard-native-restore-warm-"),
    );
    paths.push(directory);
    const levelPath = join(directory, "ledger");
    const seeded = await seedTrieLedger(
      levelPath,
      { binaryPath: nativeOwnerBinaryPath, binarySha256 },
      keyCount,
    );
    const { ledger, nodeReads } = await openCountingLedger(levelPath);
    const childPids: number[] = [];
    const service = await ProductionNativeMpfOwnerService.create(ledger, {
      binaryPath: nativeOwnerBinaryPath,
      binarySha256,
      onChildSpawnForTests(pid) {
        childPids.push(pid);
      },
    });
    services.add(service);
    expect(await promoteBlock(service, seeded.root, first!)).toEqual(
      forward.blocks[0],
    );
    const targetRoot = forward.blocks[0]!.at(-1)!;
    expect(await promoteBlock(service, targetRoot, second!)).toEqual(
      forward.blocks[1],
    );
    const expectedRoot = forward.blocks[1]!.at(-1)!;
    const displaced = await service.fork(expectedRoot);
    await service.discard(displaced);
    const before = nodeReads();
    await service.restoreCanonicalRoot({
      recoveryId: Buffer.alloc(32, 63).toString("hex"),
      expectedRoot,
      targetRoot,
    });
    expect(nodeReads()).toBe(before);
    expect(childPids).toHaveLength(1);
    const after = await service.diagnostics();
    expect(after).toMatchObject({ durableRoot: targetRoot, childRestarts: 0 });
    expect(Buffer.from(after.ownerEpoch)).toEqual(
      Buffer.from(displaced.ownerEpoch),
    );
    await expect(
      service.applyEvents(
        displaced,
        encodeNativeMpfEventLog(expectedRoot, [[]]),
      ),
    ).rejects.toThrow(/generation handle is stale/);
    expect(await promoteBlock(service, targetRoot, replacement)).toEqual(
      back.blocks[1],
    );
    expect(nodeReads() - before).toBeLessThan(seeded.nodes / 5);
    expect(childPids).toHaveLength(1);
  });

  it("persists the exact plan and repeats it idempotently across restart", async () => {
    const { service, options, plan } = await fixture();
    await service.restoreCanonicalRoot(plan);
    const epoch = (await service.diagnostics()).ownerEpoch;
    await service.restoreCanonicalRoot(plan);
    expect(Buffer.from((await service.diagnostics()).ownerEpoch)).toEqual(
      Buffer.from(epoch),
    );
    await close(service);
    expect(await inspectStore(options.levelPath, plan.recoveryId)).toEqual({
      root: plan.targetRoot,
      receipt: JSON.stringify(plan),
    });
    const restarted = await open(options);
    await restarted.restoreCanonicalRoot(plan);
    await assertAncestorContents(restarted, plan.targetRoot);
  });

  it("captures the authorized plan before the caller can mutate its alias", async () => {
    const { service, options, plan } = await fixture();
    const original = { ...plan };
    const restoring = service.restoreCanonicalRoot(plan);
    plan.recoveryId = Buffer.alloc(32, 62).toString("hex");
    plan.expectedRoot = SDK.EMPTY_MERKLE_TREE_ROOT;
    plan.targetRoot = SDK.EMPTY_MERKLE_TREE_ROOT;
    await restoring;
    await assertAncestorContents(service, original.targetRoot);
    await close(service);
    expect(await inspectStore(options.levelPath, original.recoveryId)).toEqual({
      root: original.targetRoot,
      receipt: JSON.stringify(original),
    });
    expect(await inspectStore(options.levelPath, plan.recoveryId)).toEqual({
      root: original.targetRoot,
      receipt: undefined,
    });
  });

  it("refuses a stale expected root without changing the durable marker or writing a receipt", async () => {
    const { service, options, plan } = await fixture();
    await expect(
      service.restoreCanonicalRoot({
        ...plan,
        expectedRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
      }),
    ).rejects.toThrow(/base changed/);
    expect((await service.diagnostics()).durableRoot).toBe(plan.expectedRoot);
    await close(service);
    expect(await inspectStore(options.levelPath, plan.recoveryId)).toEqual({
      root: plan.expectedRoot,
      receipt: undefined,
    });
  });

  it("refuses a target root the store does not retain and leaves the current owner usable", async () => {
    const { service, options, plan } = await fixture();
    const targetRoot = Buffer.alloc(32, 99).toString("hex");
    const refusal = await service
      .restoreCanonicalRoot({ ...plan, targetRoot })
      .then(
        () => undefined,
        (error: unknown) => error,
      );
    expect(refusal).toBeInstanceOf(NativeMpfRootNotRetained);
    expect((refusal as Error).message).toBe(
      `Native MPF canonical recovery target root ${targetRoot} is not retained; refusing to restore`,
    );
    expect((refusal as Error).cause).toBeInstanceOf(LedgerNotRetained);
    expect((await service.diagnostics()).durableRoot).toBe(plan.expectedRoot);
    const generation = await service.fork(plan.expectedRoot);
    await service.discard(generation);
    await close(service);
    expect(await inspectStore(options.levelPath, plan.recoveryId)).toEqual({
      root: plan.expectedRoot,
      receipt: undefined,
    });
  });

  /** The owner's own store handle, which the restore reads the retained
   * roots through. */
  const storeOf = (service: ProductionNativeMpfOwnerService) =>
    (service as unknown as { db: Level<string, unknown> }).db;

  it("names a failed read of the retained roots as transient, changes nothing, and restores once the read succeeds", async () => {
    const { service, options, plan } = await fixture();
    // A new owner over the same store, whose ledger store has not read the
    // retained roots yet.
    await close(service);
    const restarted = await open(options);
    const store = storeOf(restarted);
    const keys = store.keys.bind(store);
    let failures = 1;
    store.keys = ((...args: Parameters<typeof store.keys>) => {
      if (failures-- > 0)
        throw Object.assign(new Error("IO error: injected read failure"), {
          code: "LEVEL_IO_ERROR",
        });
      return keys(...args);
    }) as typeof store.keys;
    const refusal = await restarted.restoreCanonicalRoot(plan).then(
      () => undefined,
      (error: unknown) => error,
    );
    expect(refusal).toBeInstanceOf(NativeMpfRestoreReadFailed);
    expect(refusal).not.toBeInstanceOf(NativeMpfRootNotRetained);
    expect((refusal as Error).message).toBe(
      `Native MPF canonical recovery could not read whether the native MPF store retains target root ${plan.targetRoot}: IO error: injected read failure`,
    );
    expect((await restarted.diagnostics()).durableRoot).toBe(plan.expectedRoot);
    // The next restore reads the retained roots and completes.
    await restarted.restoreCanonicalRoot(plan);
    await assertAncestorContents(restarted, plan.targetRoot);
    await close(restarted);
    expect(await inspectStore(options.levelPath, plan.recoveryId)).toEqual({
      root: plan.targetRoot,
      receipt: JSON.stringify(plan),
    });
  });

  it("requires active generations to drain before restoring the retained ancestor", async () => {
    const { service, plan } = await fixture();
    const generation = await service.fork(plan.expectedRoot);
    await expect(service.restoreCanonicalRoot(plan)).rejects.toThrow(
      /drained generations/,
    );
    expect((await service.diagnostics()).durableRoot).toBe(plan.expectedRoot);
    expect((await service.diagnostics()).activeGenerations).toBe(1);
    await service.discard(generation);
    await service.restoreCanonicalRoot(plan);
    await assertAncestorContents(service, plan.targetRoot);
  });

  it("restores while a diagnostics read is in flight, and answers the read", async () => {
    // A readiness probe is in flight when the restore arrives.
    const { service, plan } = await fixture();
    const reading = service.diagnostics();
    await service.restoreCanonicalRoot(plan);
    expect([plan.expectedRoot, plan.targetRoot]).toContain(
      (await reading).durableRoot,
    );
    await assertAncestorContents(service, plan.targetRoot);
  });

  it("still refuses to restore under an in-flight mutating operation", async () => {
    const { service, plan } = await fixture();
    const forking = service.fork(plan.expectedRoot);
    await expect(service.restoreCanonicalRoot(plan)).rejects.toThrow(
      /requires drained operations/,
    );
    await service.discard(await forking);
    expect((await service.diagnostics()).durableRoot).toBe(plan.expectedRoot);
  });

  it("rejects reuse of a recovery identity for a different plan after restart", async () => {
    const { service, options, plan } = await fixture();
    await service.restoreCanonicalRoot(plan);
    await close(service);
    const restarted = await open(options);
    await expect(
      restarted.restoreCanonicalRoot({
        ...plan,
        expectedRoot: plan.targetRoot,
        targetRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
      }),
    ).rejects.toThrow(/identifier conflicts/);
    await assertAncestorContents(restarted, plan.targetRoot);
    await close(restarted);
    expect(await inspectStore(options.levelPath, plan.recoveryId)).toEqual({
      root: plan.targetRoot,
      receipt: JSON.stringify(plan),
    });
  });

  for (const point of [
    "before_root_restore_batch",
    "after_root_restore_batch_before_ack",
  ] as const) {
    it(`recovers the exact durable root and plan after ${point}`, async () => {
      const { service, options, plan } = await fixture((observed) => {
        if (observed === point) throw new Error(`injected:${point}`);
      });
      expect(service.terminalFailure()).toBeUndefined();
      await expect(service.restoreCanonicalRoot(plan)).rejects.toThrow(
        `injected:${point}`,
      );
      if (point === "after_root_restore_batch_before_ack") {
        // Committed but not installed: the owner refuses every operation and
        // restarts its child from the durable marker, the target, in-process.
        const refusal = service.terminalFailure();
        expect(refusal?.message).toMatch(/not installed yet/);
        expect((refusal?.cause as Error | undefined)?.message).toBe(
          `injected:${point}`,
        );
        await expect(service.diagnostics()).rejects.toThrow(
          /not installed yet/,
        );
        await expect(service.fork(plan.expectedRoot)).rejects.toThrow(
          /not installed yet/,
        );
        const deadline = Date.now() + 30_000;
        while (service.terminalFailure() !== undefined && Date.now() < deadline)
          await new Promise((resolve) => setTimeout(resolve, 10));
        expect(service.terminalFailure()).toBeUndefined();
        expect((await service.diagnostics()).durableRoot).toBe(plan.targetRoot);
        // The retained plan now completes as already applied.
        await service.restoreCanonicalRoot(plan);
        await assertAncestorContents(service, plan.targetRoot);
      } else {
        // Refused before the marker moved: the old root still serves.
        expect(service.terminalFailure()).toBeUndefined();
      }
      await close(service);
      const committed = point === "after_root_restore_batch_before_ack";
      expect(await inspectStore(options.levelPath, plan.recoveryId)).toEqual({
        root: committed ? plan.targetRoot : plan.expectedRoot,
        receipt: committed ? JSON.stringify(plan) : undefined,
      });
      const restarted = await open(options);
      expect((await restarted.diagnostics()).durableRoot).toBe(
        committed ? plan.targetRoot : plan.expectedRoot,
      );
      await restarted.restoreCanonicalRoot(plan);
      await assertAncestorContents(restarted, plan.targetRoot);
      await close(restarted);
      expect(await inspectStore(options.levelPath, plan.recoveryId)).toEqual({
        root: plan.targetRoot,
        receipt: JSON.stringify(plan),
      });
    });
  }
});
