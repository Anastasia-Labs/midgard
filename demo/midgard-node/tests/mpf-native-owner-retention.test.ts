/**
 * The native owner's `retain` against the pinned child and the node's
 * LevelDB ledger store: what it keeps besides the roots it is given, how a
 * canonical restore treats a root it dropped, how a promotion runs beside
 * its deletion, and what it keeps of a ledger handed to it to persist.
 */
import { createHash } from "node:crypto";
import { mkdtemp, readFile, rm } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";

import { LedgerNotRetained } from "@al-ft/midgard-fault-proofs/ledger-store";
import * as SDK from "@al-ft/midgard-sdk/core";
import { afterEach, beforeAll, describe, expect, it } from "vitest";

import { RETAINED_ROOT_PREFIX } from "../src/mpf/ledger-store-level.js";
import { NativeMpfWorkerPortClient } from "../src/services/mpf-native-owner/client.js";
import { NativeMpfRootNotRetained } from "../src/services/mpf-native-owner/protocol.js";
import type {
  NativeMpfEventOp,
  NativeMpfOwnerServiceOptions,
  StoredNode,
} from "../src/services/mpf-native-owner/service.normalize-owner-options.js";
import { openNativeOwnerLedger } from "../src/services/mpf-native-owner/service.open-ledger.js";
import { encodeNativeMpfEventLog } from "../src/services/mpf-native-owner/service.parse-promotion-records.js";
import { ProductionNativeMpfOwnerService } from "../src/services/mpf-native-owner/service.production-native-mpf-owner-service.js";
import { prepareEventFlatDigest } from "../src/workers/utils/mpf-event-flat-digest.js";
import { simDigest, simOutput } from "./helpers/landed-blocks-sim.universe.js";
import {
  nativeOwnerBinaryPath,
  nativeOwnerBinaryPresent,
  warnNativeOwnerBinaryAbsent,
} from "./helpers/native-owner-binary.js";
import {
  refuseOutputs,
  seedOwnerLedger,
} from "./helpers/native-owner-ledger.js";
import { makeOutRefCbor } from "./midgard-output-helpers.js";

const binaryPresent = nativeOwnerBinaryPresent();
if (!binaryPresent) warnNativeOwnerBinaryAbsent("mpf-native-owner-retention");

const keyOf = (index: number) =>
  createHash("sha256").update(`retention-key-${index.toString()}`).digest();

const valueOf = (index: number, version: number) =>
  Buffer.concat([
    createHash("sha256")
      .update(`retention-value-${index.toString()}-${version.toString()}`)
      .digest(),
    Buffer.alloc(32, 7),
  ]);

const inserts = (from: number, count: number): NativeMpfEventOp[] =>
  Array.from({ length: count }, (_, offset) => ({
    type: "insert" as const,
    key: keyOf(from + offset),
    value: valueOf(from + offset, 0),
  }));

/** Gives key `index` the value of `version`. */
const rewrite = (index: number, version: number): NativeMpfEventOp[] => [
  { type: "delete", key: keyOf(index) },
  { type: "insert", key: keyOf(index), value: valueOf(index, version) },
];

const NODE_KEY = /^[0-9a-f]{64}$/;

/** A test's hold on a batch written to the ledger LevelDB: `next` writes it. */
type Intercept = (
  ops: readonly { type: string; key: string }[],
  next: () => Promise<unknown>,
) => Promise<unknown>;

/** The ledger LevelDB's retained roots, node keys, count keys and queued
 * deletions, and the nodes its retained roots reach. */
type Held = {
  readonly retained: Set<string>;
  readonly nodes: Set<string>;
  readonly counted: Set<string>;
  readonly dead: number;
  readonly reachable: Set<string>;
};

describe.skipIf(!binaryPresent)("native MPF owner retention", () => {
  const paths: string[] = [];
  const services = new Set<ProductionNativeMpfOwnerService>();
  let options: NativeMpfOwnerServiceOptions;

  beforeAll(async () => {
    await prepareEventFlatDigest();
    options = {
      binaryPath: nativeOwnerBinaryPath,
      binarySha256: createHash("sha256")
        .update(await readFile(nativeOwnerBinaryPath))
        .digest("hex"),
    };
  });

  afterEach(async () => {
    await Promise.all([...services].map((service) => service.close()));
    services.clear();
    await Promise.all(
      paths.splice(0).map((path) => rm(path, { recursive: true, force: true })),
    );
  });

  /**
   * An owner over a fresh ledger holding the empty root, with every batch
   * written to its LevelDB passed through `intercept` while one is set.
   */
  const openOwner = async (
    extra: Partial<NativeMpfOwnerServiceOptions> = {},
  ) => {
    const directory = await mkdtemp(join(tmpdir(), "midgard-native-retain-"));
    paths.push(directory);
    const levelPath = join(directory, "ledger");
    await seedOwnerLedger(levelPath, SDK.EMPTY_MERKLE_TREE_ROOT);
    const ledger = await openNativeOwnerLedger(levelPath, refuseOutputs);
    const { level } = ledger;
    const batch = level.batch.bind(level) as (ops: unknown) => Promise<unknown>;
    let intercept: Intercept | undefined;
    (level as unknown as { batch: (ops: unknown) => Promise<unknown> }).batch =
      (ops) =>
        intercept === undefined
          ? batch(ops)
          : intercept(ops as { type: string; key: string }[], () => batch(ops));
    const service = await ProductionNativeMpfOwnerService.create(ledger, {
      ...options,
      ...extra,
    });
    services.add(service);
    const promote = async (ops: readonly NativeMpfEventOp[]) => {
      const base = (await service.diagnostics()).durableRoot;
      const handle = await service.fork(base);
      const applied = await service.applyEvents(
        handle,
        encodeNativeMpfEventLog(base, [ops]),
      );
      await service.promote(handle);
      return applied.candidateRoot;
    };
    const records = async (): Promise<Held> => {
      const retained = new Set<string>();
      const nodes = new Set<string>();
      const counted = new Set<string>();
      let dead = 0;
      for await (const key of level.keys()) {
        if (NODE_KEY.test(key)) nodes.add(key);
        else if (key.startsWith(RETAINED_ROOT_PREFIX))
          retained.add(key.slice(RETAINED_ROOT_PREFIX.length));
        else if (key.startsWith("__refs__/")) counted.add(key.slice(9));
        else if (key.startsWith("__dead__/")) dead += 1;
      }
      const reachable = new Set<string>();
      const pending = [...retained];
      for (let hash = pending.pop(); hash !== undefined; hash = pending.pop()) {
        if (hash === SDK.EMPTY_MERKLE_TREE_ROOT || reachable.has(hash))
          continue;
        reachable.add(hash);
        const node = (await level.get(hash)) as StoredNode | undefined;
        expect(node, `node ${hash}`).toBeDefined();
        if (node?.__kind === "Branch")
          for (const child of node.children)
            if (child !== null) pending.push(child);
      }
      return { retained, nodes, counted, dead, reachable };
    };
    return {
      service,
      ledger,
      levelPath,
      promote,
      records,
      intercept: (next: Intercept | undefined) => {
        intercept = next;
      },
    };
  };

  /** The store holds exactly the nodes its retained roots reach, each
   * counted, with no deletion pending. */
  const expectExact = (held: Held) => {
    expect(held.nodes).toEqual(held.reachable);
    expect(held.counted).toEqual(held.nodes);
    expect(held.dead).toBe(0);
  };

  it("keeps the node count to the window's ledgers over more promotions than the window holds", async () => {
    const { service, promote, records } = await openOwner();
    const roots = [await promote(inserts(0, 48))];
    // Every node any promotion wrote.
    const written = new Set<string>();
    let held: Held | undefined;
    for (let version = 1; version <= 12; version += 1) {
      roots.push(await promote(rewrite(version % 48, version)));
      for (const hash of (await records()).nodes) written.add(hash);
      const window = roots.slice(-3);
      const { notRetained } = await service.retain(new Set(window));
      expect(notRetained).toEqual(new Set());
      held = await records();
      expect(held.retained).toEqual(new Set(window));
      expectExact(held);
    }
    expect(held!.nodes.size).toBeLessThan(written.size);
  });

  it("keeps its durable root, its marker and a leased fork base that the roots leave out", async () => {
    const injected = { armed: false };
    const owner = await openOwner({
      faultInjectionForTests(point) {
        if (injected.armed && point === "after_promotion_batch_before_ack")
          throw new Error("injected acknowledgement failure");
      },
    });
    const { service, promote, ledger } = owner;
    const leased = await promote(inserts(0, 8));
    const worker = new NativeMpfWorkerPortClient(service.createWorkerPort());
    const lease = await worker.fork(leased);
    const durable = await promote(rewrite(1, 1));
    const older = await promote(rewrite(2, 1));
    const newest = await promote(rewrite(3, 1));
    expect(older).not.toBe(newest);
    // The marker moves to the candidate; the durable root stays behind it.
    injected.armed = true;
    const handle = await service.fork(newest);
    const ahead = await service.applyEvents(
      handle,
      encodeNativeMpfEventLog(newest, [rewrite(4, 1)]),
    );
    await expect(service.promote(handle)).rejects.toThrow(
      /injected acknowledgement failure/,
    );
    expect((await service.diagnostics()).durableRoot).toBe(newest);
    expect(await ledger.level.get("__root__")).toBe(ahead.candidateRoot);

    await service.retain(new Set([durable]));
    const held = await owner.records();
    expect(held.retained).toEqual(
      new Set([durable, newest, ahead.candidateRoot, leased]),
    );
    expectExact(held);
    for (const root of [newest, ahead.candidateRoot, leased])
      await ledger.store.at(root);
    await expect(ledger.store.at(older)).rejects.toBeInstanceOf(
      LedgerNotRetained,
    );
    worker.close();
    expect(lease.baseRoot).toBe(leased);
  });

  /** Restores the owner from its durable root to `targetRoot`. */
  const restore = async (
    service: ProductionNativeMpfOwnerService,
    targetRoot: string,
  ) => {
    const expectedRoot = (await service.diagnostics()).durableRoot;
    await service.restoreCanonicalRoot({
      recoveryId: createHash("sha256")
        .update(`${expectedRoot}:${targetRoot}`)
        .digest("hex"),
      expectedRoot,
      targetRoot,
    });
  };

  it("restores a retained root, and refuses a dropped one whose nodes a failed deletion left, keeping the marker", async () => {
    const { service, promote, records, ledger, intercept } = await openOwner();
    const dropped = await promote(inserts(0, 48));
    const kept = await promote(rewrite(1, 1));
    const tip = await promote(rewrite(2, 1));
    // The replacement lands; the first deletion batch fails, so every node
    // of the dropped root stays.
    let writes = 0;
    intercept((_, next) =>
      ++writes === 2 ? Promise.reject(new Error("killed")) : next(),
    );
    await expect(service.retain(new Set([kept]))).rejects.toThrow("killed");
    intercept(undefined);
    expect((await records()).nodes).toContain(dropped);

    const refusal = await restore(service, dropped).then(
      () => undefined,
      (error: unknown) => error,
    );
    expect(refusal).toBeInstanceOf(NativeMpfRootNotRetained);
    expect((refusal as Error).cause).toBeInstanceOf(LedgerNotRetained);
    expect(await ledger.level.get("__root__")).toBe(tip);
    expect((await service.diagnostics()).durableRoot).toBe(tip);

    await restore(service, kept);
    expect(await ledger.level.get("__root__")).toBe(kept);
    expect(await promote(rewrite(3, 1))).not.toBe(kept);
    await service.retain(new Set());
    expectExact(await records());
  });

  it("lands a promotion called during a deletion after at most one deletion batch, and keeps its root", async () => {
    // Set for the promotion called during the deletion.
    let reachedAdopt: (() => void) | undefined;
    const owner = await openOwner({
      faultInjectionForTests(point) {
        if (point === "before_promotion_batch") reachedAdopt?.();
      },
    });
    const { service, promote, records, ledger, intercept } = owner;
    const dropped = await promote(inserts(0, 600));
    // A one-leaf ledger, sharing no node with the dropped one.
    const base = await promote([
      ...inserts(0, 600).map(({ key }) => ({ type: "delete" as const, key })),
      ...inserts(600, 1),
    ]);
    // Every batch from the retain on, by what it writes; the first deletion
    // batch waits until released.
    const writes: string[] = [];
    let release!: () => void;
    const released = new Promise<void>((resolve) => {
      release = resolve;
    });
    let blocked!: () => void;
    const deletionBlocked = new Promise<void>((resolve) => {
      blocked = resolve;
    });
    intercept(async (ops, next) => {
      const kind = ops.some(
        ({ key, type }) => type === "put" && key === "__root__",
      )
        ? "promotion"
        : ops.some(({ key, type }) => type === "del" && NODE_KEY.test(key))
          ? "deletion"
          : "replacement";
      writes.push(kind);
      if (kind === "deletion" && writes.length === 2) {
        blocked();
        await released;
      }
      return await next();
    });
    const adoptQueued = new Promise<void>((resolve) => {
      reachedAdopt = resolve;
    });
    const retaining = service.retain(new Set());
    await deletionBlocked;
    const promoting = promote(rewrite(600, 1));
    await adoptQueued;
    await new Promise((resolve) => setImmediate(resolve));
    release();
    const [candidate] = await Promise.all([promoting, retaining]);
    intercept(undefined);

    expect(writes.slice(0, 3)).toEqual([
      "replacement",
      "deletion",
      "promotion",
    ]);
    expect(writes.slice(3)).toContain("deletion");
    const held = await records();
    expect(held.retained).toEqual(new Set([base, candidate]));
    expect(held.nodes).not.toContain(dropped);
    expectExact(held);
    expect(await ledger.level.get("__root__")).toBe(candidate);
    await ledger.store.at(candidate);
  });

  it("keeps the root of a promotion called while a retain reads the marker", async () => {
    const { service, promote, records, ledger, intercept } = await openOwner();
    const base = await promote(inserts(0, 8));
    const handle = await service.fork(base);
    const { candidateRoot } = await service.applyEvents(
      handle,
      encodeNativeMpfEventLog(base, [rewrite(1, 1)]),
    );
    let adopted!: () => void;
    const adoptLanded = new Promise<void>((resolve) => {
      adopted = resolve;
    });
    intercept(async (ops, next) => {
      const result = await next();
      if (ops.some(({ key, type }) => type === "put" && key === "__root__"))
        adopted();
      return result;
    });
    // The retain's marker read answers what it read only once the promotion
    // has written, or after a second if the promotion waits for the retain.
    const { level } = ledger;
    const get = level.get.bind(level) as (key: string) => Promise<unknown>;
    let markerRead!: () => void;
    const readingMarker = new Promise<void>((resolve) => {
      markerRead = resolve;
    });
    let armed = true;
    (level as unknown as { get: (key: string) => Promise<unknown> }).get =
      async (key) => {
        if (!armed || key !== "__root__") return get(key);
        armed = false;
        const value = await get(key);
        markerRead();
        await Promise.race([
          adoptLanded,
          new Promise((resolve) => setTimeout(resolve, 1000)),
        ]);
        return value;
      };
    const retaining = service.retain(new Set());
    await readingMarker;
    await Promise.all([retaining, service.promote(handle)]);
    intercept(undefined);

    expect(await level.get("__root__")).toBe(candidateRoot);
    await ledger.store.at(candidateRoot);
    const held = await records();
    expect(held.retained).toEqual(new Set([base, candidateRoot]));
    expectExact(held);
  });

  it("refuses a restore called before a retain that drops its target has written", async () => {
    let restoreWrites: (() => void) | undefined;
    const owner = await openOwner({
      faultInjectionForTests(point) {
        if (point === "before_root_restore_batch") restoreWrites?.();
      },
    });
    const { service, promote, records, ledger, intercept } = owner;
    const dropped = await promote(inserts(0, 8));
    const tip = await promote(rewrite(1, 1));
    // The retain's first write, its replacement, waits until released.
    let release!: () => void;
    const released = new Promise<void>((resolve) => {
      release = resolve;
    });
    let blocked!: () => void;
    const replacementBlocked = new Promise<void>((resolve) => {
      blocked = resolve;
    });
    let writes = 0;
    intercept(async (_, next) => {
      if (++writes === 1) {
        blocked();
        await released;
      }
      return await next();
    });
    const retaining = service.retain(new Set());
    await replacementBlocked;
    const restoring = restore(service, dropped).then(
      () => undefined,
      (error: unknown) => error,
    );
    // Released once the restore is about to move the marker, or after two
    // seconds if it waits for the retain.
    await Promise.race([
      new Promise<void>((resolve) => {
        restoreWrites = resolve;
      }),
      new Promise((resolve) => setTimeout(resolve, 2000)),
    ]);
    release();
    await retaining;
    const refusal = await restoring;
    intercept(undefined);

    expect(refusal).toBeInstanceOf(NativeMpfRootNotRetained);
    expect(await ledger.level.get("__root__")).toBe(tip);
    expect((await service.diagnostics()).durableRoot).toBe(tip);
    const held = await records();
    expect(held.retained).toEqual(new Set([tip]));
    expectExact(held);
  });

  it("keeps a ledger it is handed to persist, without moving its durable root, until a retain leaves it out", async () => {
    const { service, records } = await openOwner();
    const durable = (await service.diagnostics()).durableRoot;
    const after = await (
      await service.ledger(durable)
    ).apply([
      {
        type: "insert",
        outRef: makeOutRefCbor(simDigest("retention:persisted"), 0),
        output: simOutput(3_000_000n),
      },
    ]);
    await expect(service.ledger(after.root)).rejects.toBeInstanceOf(
      LedgerNotRetained,
    );

    await service.persistLedger(after);
    expect((await service.ledger(after.root)).root).toBe(after.root);
    expect((await service.diagnostics()).durableRoot).toBe(durable);
    const kept = await records();
    expect(kept.retained.has(after.root)).toBe(true);
    expectExact(kept);

    await service.retain(new Set([durable]));
    await expect(service.ledger(after.root)).rejects.toBeInstanceOf(
      LedgerNotRetained,
    );
    expectExact(await records());
  });
});
