import { createHash } from "node:crypto";
import { mkdtemp, readFile, rm } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";

import * as SDK from "@al-ft/midgard-sdk";
import { Effect, Ref, Schedule } from "effect";
import { Level } from "level";
import { afterEach, beforeAll, describe, expect, it, vi } from "vitest";

import {
  NATIVE_MPF_PROMOTION_INDEX_CAP_EXCEEDED,
  NATIVE_MPF_PROMOTION_INDEX_CAP_SOURCE,
  nativeMpfOwnerSupervisorFiber,
} from "../src/fibers/native-mpf-owner-supervisor.js";
import { Globals } from "../src/services/globals.js";
import { activeLivenessReasons } from "../src/services/liveness-halt.js";
import {
  encodeNativeMpfEventLog,
  ProductionNativeMpfOwnerService,
} from "../src/services/mpf-native-owner/index.js";
import { NativeMpfPromotionIndexCapExceeded } from "../src/services/mpf-native-owner/protocol.js";
import { encodeStoredNode } from "../src/services/mpf-native-owner/service.encode-stored-node.js";
import {
  candidateFullIndexSize,
  fullIndexNearCapDetails,
} from "../src/services/mpf-native-owner/service.full-index-accounting.js";
import {
  type DecodedPromotionRecord,
  FULL_INDEX_HEADER_BYTES,
  type NativeMpfEventOp,
  type StoredNode,
  type StoredValue,
} from "../src/services/mpf-native-owner/service.normalize-owner-options.js";
import { prepareEventFlatDigest } from "../src/workers/utils/mpf-event-flat-digest.js";
import { nativeOwnerBinaryPath } from "./helpers/native-owner-binary.js";

const DEFAULT_RECORDS = 2_000_000;
const DEFAULT_BYTES = 512 * 1024 * 1024;

// The full-index caps the TypeScript owner reads at each use, so a test can
// lower one after the owner has opened and built its trie at the default. The
// record cap is also checked against the native child's at every child start,
// so a test lowers it only between a child start and the next.
const caps = vi.hoisted(() => ({
  records: 2_000_000,
  bytes: 512 * 1024 * 1024,
}));

vi.mock(
  "../src/services/mpf-native-owner/service.normalize-owner-options.js",
  async (importOriginal) => {
    const actual =
      await importOriginal<
        typeof import("../src/services/mpf-native-owner/service.normalize-owner-options.js")
      >();
    return {
      ...actual,
      get FULL_INDEX_MAX_RECORDS() {
        return caps.records;
      },
      get FULL_INDEX_MAX_BYTES() {
        return caps.bytes;
      },
    };
  },
);

const insert = (fill: number, valueBytes = 64): NativeMpfEventOp => ({
  type: "insert",
  key: Buffer.alloc(32, fill),
  value: Buffer.alloc(valueBytes, fill + 10),
});

const remove = (fill: number): NativeMpfEventOp => ({
  type: "delete",
  key: Buffer.alloc(32, fill),
});

const range = (from: number, to: number) =>
  Array.from({ length: to - from + 1 }, (_, index) => from + index);

/** One supervisor tick over `owner`: the liveness reasons readiness reports
 * under the promotion source afterwards. */
const superviseOnce = (owner: ProductionNativeMpfOwnerService) =>
  Effect.runPromise(
    Effect.gen(function* () {
      const globals = yield* Globals;
      yield* Ref.set(globals.NATIVE_MPF_OWNER, owner);
      yield* nativeMpfOwnerSupervisorFiber(Schedule.recurs(0));
      return (yield* activeLivenessReasons(globals)).filter(
        ({ source }) => source === NATIVE_MPF_PROMOTION_INDEX_CAP_SOURCE,
      );
    }).pipe(Effect.provide(Globals.Default)),
  );

// Promotion is the only way a root becomes durable, and a process start loads
// the durable root under the full-index caps. A root promotion accepts must
// therefore load at the next start under the same caps.
describe("native MPF promotion under the full-index caps", () => {
  const paths: string[] = [];
  const services = new Set<ProductionNativeMpfOwnerService>();
  let binarySha256: string;

  beforeAll(async () => {
    await prepareEventFlatDigest();
    binarySha256 = createHash("sha256")
      .update(await readFile(nativeOwnerBinaryPath))
      .digest("hex");
  });

  afterEach(async () => {
    caps.records = DEFAULT_RECORDS;
    caps.bytes = DEFAULT_BYTES;
    for (const service of services) await service.close();
    services.clear();
    await Promise.all(
      paths.splice(0).map((path) => rm(path, { recursive: true, force: true })),
    );
  });

  const open = async (options: {
    readonly levelPath: string;
    readonly sidecarPath: string;
    readonly binaryPath: string;
    readonly binarySha256: string;
  }) => {
    const service = await ProductionNativeMpfOwnerService.create(options);
    services.add(service);
    return service;
  };

  const shut = async (service: ProductionNativeMpfOwnerService) => {
    await service.close();
    services.delete(service);
  };

  const fixture = async () => {
    const directory = await mkdtemp(join(tmpdir(), "midgard-native-promo-"));
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
    return { options, service: await open(options) };
  };

  /** Forks `base` and applies one event of `ops`, at the default caps: the
   * event-log header carries the byte cap. */
  const apply = async (
    service: ProductionNativeMpfOwnerService,
    base: string,
    ...events: (readonly NativeMpfEventOp[])[]
  ) => {
    const handle = await service.fork(base);
    const applied = await service.applyEvents(
      handle,
      encodeNativeMpfEventLog(base, events),
    );
    return { handle, candidateRoot: applied.candidateRoot };
  };

  const promoteOutcome = (
    service: ProductionNativeMpfOwnerService,
    handle: Parameters<ProductionNativeMpfOwnerService["promote"]>[0],
  ) =>
    service.promote(handle).then(
      () => "accepted" as const,
      (error: unknown) => error,
    );

  it("every root promotion accepts loads at the next start under the same byte cap", async () => {
    const { options, service } = await fixture();
    const first = await apply(service, SDK.EMPTY_MERKLE_TREE_ROOT, [
      insert(41),
    ]);
    await service.promote(first.handle);
    const second = await apply(service, first.candidateRoot, [insert(42)]);
    // One leaf's full index (header plus a 200-byte leaf) fits; two leaves
    // under a branch do not.
    caps.bytes = 300;
    const outcome = await promoteOutcome(service, second.handle);
    await shut(service);
    const reopened = await open(options);
    expect((await reopened.diagnostics()).durableRoot).toBe(
      outcome === "accepted" ? second.candidateRoot : first.candidateRoot,
    );
  });

  it("refuses a candidate over the byte cap by that cap, changes nothing, holds the refusal on readiness, and clears it once a promotion fits", async () => {
    const { options, service } = await fixture();
    const first = await apply(service, SDK.EMPTY_MERKLE_TREE_ROOT, [
      insert(41),
    ]);
    await service.promote(first.handle);
    const one = service.fullIndexHealth();
    const second = await apply(service, first.candidateRoot, [insert(42)]);
    caps.bytes = 300;
    const refusal = await promoteOutcome(service, second.handle);
    expect(refusal).toBeInstanceOf(NativeMpfPromotionIndexCapExceeded);
    const exceeded = refusal as NativeMpfPromotionIndexCapExceeded;
    expect(exceeded.candidateRoot).toBe(second.candidateRoot);
    expect(exceeded.cap).toBe("FULL_INDEX_MAX_BYTES");
    expect(exceeded.limit).toBe(300);
    expect(exceeded.message).toContain(second.candidateRoot);
    expect(exceeded.message).toContain(
      "over the full-index byte cap FULL_INDEX_MAX_BYTES = 300",
    );
    // Nothing changed: the durable root, its size, and no generation left.
    const diagnostics = await service.diagnostics();
    expect(diagnostics.durableRoot).toBe(first.candidateRoot);
    expect(diagnostics.activeGenerations).toBe(0);
    expect(service.fullIndexHealth()).toEqual({
      ...one,
      promotionRefusal: exceeded,
    });
    const held = await superviseOnce(service);
    expect(held.map(({ reason }) => reason)).toEqual([
      NATIVE_MPF_PROMOTION_INDEX_CAP_EXCEEDED,
    ]);

    // A promotion that fits (here, one that shrinks the ledger) clears it.
    const shrunk = await apply(service, first.candidateRoot, [remove(41)]);
    await service.promote(shrunk.handle);
    expect(shrunk.candidateRoot).toBe(SDK.EMPTY_MERKLE_TREE_ROOT);
    expect(service.fullIndexHealth()).toEqual({
      bytes: FULL_INDEX_HEADER_BYTES,
      records: 0,
      promotionRefusal: undefined,
    });
    expect(await superviseOnce(service)).toEqual([]);

    // The refused size was the candidate's exact full index: under caps that
    // cover it the same candidate promotes to that size, and loads at it.
    caps.bytes = DEFAULT_BYTES;
    const again = await apply(service, SDK.EMPTY_MERKLE_TREE_ROOT, [
      insert(41),
      insert(42),
    ]);
    expect(again.candidateRoot).toBe(second.candidateRoot);
    await service.promote(again.handle);
    expect(service.fullIndexHealth().bytes).toBe(exceeded.observed);
    await shut(service);
    const reopened = await open(options);
    expect((await reopened.diagnostics()).durableRoot).toBe(
      second.candidateRoot,
    );
    expect(reopened.fullIndexHealth().bytes).toBe(exceeded.observed);
  });

  it("refuses a candidate over the record cap by that cap, and the store stays at the last promoted root", async () => {
    const { options, service } = await fixture();
    const first = await apply(service, SDK.EMPTY_MERKLE_TREE_ROOT, [
      insert(41),
    ]);
    await service.promote(first.handle);
    const second = await apply(service, first.candidateRoot, [insert(42)]);
    caps.records = 2;
    const refusal = await promoteOutcome(service, second.handle);
    expect(refusal).toBeInstanceOf(NativeMpfPromotionIndexCapExceeded);
    const exceeded = refusal as NativeMpfPromotionIndexCapExceeded;
    expect(exceeded.cap).toBe("FULL_INDEX_MAX_RECORDS");
    expect(exceeded.limit).toBe(2);
    // A branch over two leaves.
    expect(exceeded.observed).toBe(3);
    expect(exceeded.message).toContain(
      "over the full-index record cap FULL_INDEX_MAX_RECORDS = 2",
    );
    // The record cap is checked against the native child's at every start,
    // so the store is read back under the default caps.
    caps.records = DEFAULT_RECORDS;
    await shut(service);
    const reopened = await open(options);
    expect((await reopened.diagnostics()).durableRoot).toBe(
      first.candidateRoot,
    );
    expect(reopened.fullIndexHealth()).toEqual({
      bytes: expect.any(Number),
      records: 1,
      promotionRefusal: undefined,
    });
  });

  it("refuses a journal replay over a cap without changing the store, and the next replay that fits promotes it", async () => {
    const { service } = await fixture();
    const first = await apply(service, SDK.EMPTY_MERKLE_TREE_ROOT, [
      insert(41),
    ]);
    await service.promote(first.handle);
    // The journal's replay: one event from the durable root, as a commit
    // journals it.
    const eventLog = encodeNativeMpfEventLog(first.candidateRoot, [
      [insert(42)],
    ]);
    const handle = await service.fork(first.candidateRoot);
    const applied = await service.applyEvents(handle, eventLog);
    await service.discard(handle);
    const replay = {
      schema: 1,
      ownerBinarySha256: binarySha256,
      baseRoot: first.candidateRoot,
      candidateRoot: applied.candidateRoot,
      eventLog,
      eventLogDigest: applied.eventLogDigest,
      eventRoots: Buffer.from(applied.eventRoots.join(""), "hex"),
      eventCount: applied.eventRoots.length,
    } as const;
    caps.records = 2;
    const refusal = await service.recover(replay).then(
      () => "accepted" as const,
      (error: unknown) => error,
    );
    expect(refusal).toBeInstanceOf(NativeMpfPromotionIndexCapExceeded);
    expect((refusal as NativeMpfPromotionIndexCapExceeded).candidateRoot).toBe(
      applied.candidateRoot,
    );
    const diagnostics = await service.diagnostics();
    expect(diagnostics.durableRoot).toBe(first.candidateRoot);
    expect(diagnostics.activeGenerations).toBe(0);
    expect(service.fullIndexHealth().promotionRefusal).toBe(refusal);
    // The same replay under caps that cover it promotes, and clears the hold.
    caps.records = DEFAULT_RECORDS;
    await service.recover(replay);
    expect((await service.diagnostics()).durableRoot).toBe(
      applied.candidateRoot,
    );
    expect(service.fullIndexHealth().promotionRefusal).toBeUndefined();
  });

  it("accounts each promoted root's full index exactly as the next start loads it", async () => {
    const { options, service: seeded } = await fixture();
    await shut(seeded);
    const steps: (readonly NativeMpfEventOp[])[][] = [
      [range(1, 30).map((fill) => insert(fill, fill * 3))],
      [
        range(1, 10).map(remove),
        range(11, 15).map(remove),
        [
          ...range(11, 15).map((fill) => insert(fill, 7)),
          ...range(31, 40).map((fill) => insert(fill)),
        ],
      ],
      [
        range(16, 40)
          .filter((fill) => fill !== 20)
          .map(remove),
      ],
      [range(50, 55).map((fill) => insert(fill, 200))],
      [[...range(50, 55), 11, 12, 13, 14, 15, 20].map(remove)],
    ];
    let root = SDK.EMPTY_MERKLE_TREE_ROOT;
    for (const events of steps) {
      const service = await open(options);
      expect((await service.diagnostics()).durableRoot).toBe(root);
      const applied = await apply(service, root, ...events);
      await service.promote(applied.handle);
      const accounted = service.fullIndexHealth();
      root = applied.candidateRoot;
      await shut(service);
      // A start reads no sidecar for this root: it walks the store.
      const loaded = await open(options);
      expect((await loaded.diagnostics()).durableRoot).toBe(root);
      expect(loaded.fullIndexHealth()).toEqual(accounted);
      await shut(loaded);
    }
    expect(root).toBe(SDK.EMPTY_MERKLE_TREE_ROOT);
  });

  it("warns on readiness once the live root passes the warning fraction of a cap", async () => {
    const { service } = await fixture();
    const first = await apply(service, SDK.EMPTY_MERKLE_TREE_ROOT, [
      insert(41),
    ]);
    await service.promote(first.handle);
    const { bytes } = service.fullIndexHealth();
    caps.bytes = bytes + 1;
    expect(fullIndexNearCapDetails(service.fullIndexHealth())).toEqual([
      `native_mpf_full_index_near_cap:FULL_INDEX_MAX_BYTES:${bytes.toString()}:${(bytes + 1).toString()}`,
    ]);
    caps.bytes = bytes * 2;
    expect(fullIndexNearCapDetails(service.fullIndexHealth())).toEqual([]);
  });
});

// The candidate size from the replaced paths alone, over hand-built tries:
// a generated record the base already holds keeps its subtree, and a
// frontier node outside the base's closure falls back to a full walk.
describe("candidate full-index accounting", () => {
  const paths: string[] = [];

  afterEach(async () => {
    await Promise.all(
      paths.splice(0).map((path) => rm(path, { recursive: true, force: true })),
    );
  });

  const hash = (n: number) => n.toString(16).padStart(2, "0").repeat(32);
  const leaf = (n: number): StoredNode => ({
    __kind: "Leaf",
    prefix: "abc",
    key: n.toString(16).padStart(2, "0").repeat(4),
    value: n.toString(16).padStart(2, "0").repeat(n),
  });
  const branch = (children: readonly number[]): StoredNode => ({
    __kind: "Branch",
    prefix: "",
    children: Array.from({ length: 16 }, (_, slot) =>
      children[slot] === undefined ? null : hash(children[slot]),
    ),
    size: children.length,
  });
  const size = (nodes: ReadonlyMap<number, StoredNode>, ids: number[]) => ({
    bytes:
      FULL_INDEX_HEADER_BYTES +
      ids.reduce(
        (total, id) => total + encodeStoredNode(hash(id), nodes.get(id)).length,
        0,
      ),
    records: ids.length,
  });

  it("is the exact size of the candidate's closure", async () => {
    const directory = await mkdtemp(join(tmpdir(), "midgard-native-size-"));
    paths.push(directory);
    const db = new Level<string, StoredValue>(join(directory, "ledger"), {
      valueEncoding: "json",
    });
    await db.open();
    // Stored: base 1 -> [2, 3]; 9 is stored but outside the base closure.
    // Generated: 4 -> [2, 5], 6 -> [3, 5] (3 is generated too), 7 -> [2, 9].
    const nodes = new Map<number, StoredNode>([
      [1, branch([2, 3])],
      [2, leaf(2)],
      [3, leaf(3)],
      [5, leaf(5)],
      [9, leaf(9)],
      [4, branch([2, 5])],
      [6, branch([3, 5])],
      [7, branch([2, 9])],
    ]);
    for (const id of [1, 2, 3, 9]) await db.put(hash(id), nodes.get(id)!);
    const generated = (ids: number[]): DecodedPromotionRecord[] =>
      ids.map((id) => ({
        hash: Buffer.from(hash(id), "hex"),
        hashHex: hash(id),
        encoded: encodeStoredNode(hash(id), nodes.get(id)),
        stored: nodes.get(id)!,
      }));
    const base = size(nodes, [1, 2, 3]);
    try {
      const of = (candidate: number, records: number[]) =>
        candidateFullIndexSize({
          db,
          baseRoot: hash(1),
          base,
          candidateRoot: hash(candidate),
          records: generated(records),
        });
      expect(await of(4, [4, 5])).toEqual(size(nodes, [4, 2, 5]));
      expect(await of(6, [6, 3, 5])).toEqual(size(nodes, [6, 3, 5]));
      expect(await of(7, [7])).toEqual(size(nodes, [7, 2, 9]));
    } finally {
      await db.close();
    }
  });
});
