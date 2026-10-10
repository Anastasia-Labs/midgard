import { createHash } from "node:crypto";
import { mkdtemp, readFile, rm } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";

import * as SDK from "@al-ft/midgard-sdk";
import { Level } from "level";
import { afterEach, beforeAll, describe, expect, it } from "vitest";

import {
  encodeNativeMpfEventLog,
  ProductionNativeMpfOwnerService,
} from "../src/services/mpf-native-owner/index.js";
import {
  NativeMpfRestoreReadFailed,
  NativeMpfRootNotRetained,
} from "../src/services/mpf-native-owner/protocol.js";
import { NativeMpfClosureIncomplete } from "../src/services/mpf-native-owner/service.encode-stored-node.js";
import type { NativeMpfOwnerServiceOptions } from "../src/services/mpf-native-owner/service.js";
import { prepareEventFlatDigest } from "../src/workers/utils/mpf-event-flat-digest.js";
import { nativeOwnerBinaryPath } from "./helpers/native-owner-binary.js";

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

  const open = async (options: NativeMpfOwnerServiceOptions) => {
    const service = await ProductionNativeMpfOwnerService.create(options);
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
      sidecarPath: join(directory, "ledger.sidecar"),
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

  it("checks out the retained ancestor in a new epoch and rejects displaced handles", async () => {
    const { service, plan } = await fixture();
    const old = await service.fork(plan.expectedRoot);
    await service.discard(old);
    const before = await service.diagnostics();
    await service.restoreCanonicalRoot(plan);
    const after = await service.diagnostics();
    expect(after.durableRoot).toBe(plan.targetRoot);
    expect(Buffer.from(after.ownerEpoch)).not.toEqual(
      Buffer.from(before.ownerEpoch),
    );
    expect(after.activeGenerations).toBe(0);
    await expect(
      service.applyEvents(
        old,
        encodeNativeMpfEventLog(plan.expectedRoot, [[]]),
      ),
    ).rejects.toThrow(/stale owner epoch/);
    await assertAncestorContents(service, plan.targetRoot);
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

  it("refuses an unavailable target closure and leaves the current owner usable", async () => {
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
      `Native MPF canonical recovery target root ${targetRoot} is not retained in full; refusing to restore`,
    );
    expect((refusal as Error).cause).toBeInstanceOf(NativeMpfClosureIncomplete);
    expect(String((refusal as Error).cause)).toMatch(/missing record/);
    expect((await service.diagnostics()).durableRoot).toBe(plan.expectedRoot);
    const generation = await service.fork(plan.expectedRoot);
    await service.discard(generation);
    await close(service);
    expect(await inspectStore(options.levelPath, plan.recoveryId)).toEqual({
      root: plan.expectedRoot,
      receipt: undefined,
    });
  });

  /** The owner's own store handle, which the restore reads the target
   * closure through. */
  const storeOf = (service: ProductionNativeMpfOwnerService) =>
    (service as unknown as { db: Level<string, unknown> }).db;

  it("refuses a target whose closure holds a record that is not a node as not retained", async () => {
    const { service, options, plan } = await fixture();
    // The ancestor's root record, outside the live descendant's closure.
    await storeOf(service).put(plan.targetRoot, { corrupt: true });
    const refusal = await service.restoreCanonicalRoot(plan).then(
      () => undefined,
      (error: unknown) => error,
    );
    expect(refusal).toBeInstanceOf(NativeMpfRootNotRetained);
    expect(String((refusal as Error).cause)).toMatch(/malformed record/);
    expect((await service.diagnostics()).durableRoot).toBe(plan.expectedRoot);
    await close(service);
    expect(await inspectStore(options.levelPath, plan.recoveryId)).toEqual({
      root: plan.expectedRoot,
      receipt: undefined,
    });
  });

  it("names a failed read of the target closure as transient, changes nothing, and restores once the read succeeds", async () => {
    const { service, options, plan } = await fixture();
    const store = storeOf(service);
    const getMany = store.getMany.bind(store);
    let failures = 1;
    store.getMany = (async (keys: string[]) => {
      if (failures-- > 0)
        throw Object.assign(new Error("IO error: injected read failure"), {
          code: "LEVEL_IO_ERROR",
        });
      return getMany(keys);
    }) as typeof store.getMany;
    const refusal = await service.restoreCanonicalRoot(plan).then(
      () => undefined,
      (error: unknown) => error,
    );
    expect(refusal).toBeInstanceOf(NativeMpfRestoreReadFailed);
    expect(refusal).not.toBeInstanceOf(NativeMpfRootNotRetained);
    expect((refusal as Error).message).toBe(
      `Native MPF canonical recovery could not read target root ${plan.targetRoot}'s node closure from the native MPF store: IO error: injected read failure`,
    );
    expect((await service.diagnostics()).durableRoot).toBe(plan.expectedRoot);
    // The next restore reads the closure and completes.
    await service.restoreCanonicalRoot(plan);
    await assertAncestorContents(service, plan.targetRoot);
    await close(service);
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

  it("holds the restore until an in-flight diagnostics read has finished on the displaced child", async () => {
    // The read is parked after taking the current child and before asking it,
    // as a readiness probe is when the restore arrives. An ungated restore
    // would close that child under it and commit first.
    const events: string[] = [];
    let holdRead = false;
    let readParked!: () => void;
    const parked = new Promise<void>((resolve) => (readParked = resolve));
    let releaseRead!: () => void;
    const released = new Promise<void>((resolve) => (releaseRead = resolve));
    const { service, plan } = await fixture(async (point) => {
      if (point === "diagnostics_before_request" && holdRead) {
        holdRead = false;
        readParked();
        await released;
        events.push("read_released");
      }
      if (point === "before_root_restore_batch") {
        events.push("restore_batch");
        releaseRead();
      }
    });
    holdRead = true;
    const reading = service.diagnostics();
    await parked;
    const restoring = service.restoreCanonicalRoot(plan);
    // A gated restore stays parked behind the read for this whole interval.
    const timer = setTimeout(releaseRead, 2_000);
    try {
      expect((await reading).durableRoot).toBe(plan.expectedRoot);
      await restoring;
    } finally {
      clearTimeout(timer);
    }
    expect(events).toEqual(["read_released", "restore_batch"]);
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
