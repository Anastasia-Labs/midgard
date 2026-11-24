import { mkdtemp, rm } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";

import { ledgerEntryLeaf } from "@al-ft/midgard-fault-proofs/ledger-at";
import {
  createLedgerStore,
  createMemoryLedgerStorage,
  type Ledger,
} from "@al-ft/midgard-fault-proofs/ledger-store";
import { oracleLedgerRoot } from "@al-ft/midgard-fault-proofs/test-support/ledger-root-oracle";
import {
  appliedEntries,
  describeLedgerStoreConformance,
  event,
  expectLedgerMatches,
  type OpenedLedgerStorage,
  storedNodesOf,
  testLedgerEntries,
  testOutputOf,
  testOutRef,
} from "@al-ft/midgard-fault-proofs/test-support/ledger-store-conformance";
import { Effect, Option } from "effect";
import { Level } from "level";
import { afterEach, describe, expect, it } from "vitest";

import {
  createLevelLedgerStorage,
  RETAINED_ROOT_PREFIX,
} from "../src/mpf/ledger-store-level.js";
import { MidgardMpf } from "../src/mpf/store.midgard-mpf.js";
import {
  JSON_LEVEL_ENCODING_OPTS,
  ROOT_KEY,
} from "../src/mpf/store-primitives.js";
import type { MpfStoredValue } from "../src/mpf/types.js";
import type { StoredNode } from "../src/services/mpf-native-owner/service.normalize-owner-options.js";
import {
  type NativeOwnerLedger,
  openNativeOwnerLedger,
  writeGenesisLedger,
} from "../src/services/mpf-native-owner/service.open-ledger.js";
import { refuseOutputs } from "./helpers/native-owner-ledger.js";

const directories: string[] = [];
const freshPath = async () => {
  const directory = await mkdtemp(join(tmpdir(), "ledger-store-level-"));
  directories.push(directory);
  return join(directory, "mpf");
};

afterEach(async () => {
  await Promise.all(
    directories
      .splice(0)
      .map((directory) => rm(directory, { recursive: true, force: true })),
  );
});

const openLevel = async (path: string) => {
  const level = new Level<string, MpfStoredValue>(
    path,
    JSON_LEVEL_ENCODING_OPTS,
  );
  await level.open();
  return level;
};

/** The same database handle, typed as the native owner types its values. */
/** The node keys under each of `roots`, read node by node from `level`. */
const reachableFrom = async (
  level: Level<string, MpfStoredValue>,
  roots: readonly string[],
) => {
  const reachable = new Set<string>();
  const pending = [...roots];
  for (let hash = pending.pop(); hash !== undefined; hash = pending.pop()) {
    if (reachable.has(hash)) continue;
    reachable.add(hash);
    const node = (await level.get(hash)) as StoredNode | undefined;
    expect(node, `node ${hash}`).toBeDefined();
    if (node?.__kind === "Branch")
      for (const child of node.children)
        if (child !== null) pending.push(child);
  }
  return reachable;
};

const levelStorage = (level: Level<string, MpfStoredValue>) =>
  createLevelLedgerStorage({
    level,
    readOutputs: async (outRefs) => outRefs.map(testOutputOf),
  });

const openedAt = async (path: string): Promise<OpenedLedgerStorage> => {
  const level = await openLevel(path);
  return {
    storage: levelStorage(level),
    close: () => level.close(),
    reopen: async () => {
      await level.close();
      return await openedAt(path);
    },
  };
};

const openFresh = async () => openedAt(await freshPath());

// Outputs live outside this adapter, so its counts are compared with its own.
describeLedgerStoreConformance("leveldb", openFresh, openFresh);

describe("leveldb ledger storage beside the node's MPF readers", () => {
  const base = testLedgerEntries(0, 50);

  /** A store over a fresh database holding `base` and two descendants. */
  const written = async () => {
    const path = await freshPath();
    const level = await openLevel(path);
    const store = createLedgerStore(levelStorage(level));
    const a = await store.admit(base, await oracleLedgerRoot(base));
    const b = await a.apply(event([0, 1], [100]));
    await store.persist(b);
    const c = await a.apply(event([2, 3], [101]));
    await store.persist(c);
    return { path, level, store, a, b, c };
  };

  const proofOf = async (ledger: Ledger, key: Buffer) =>
    (
      await (await ledger.read([key])).get(key.toString("hex"))!.proof()
    ).toString("hex");

  it("leaves MidgardMpf.load reading the trie the ledger store wrote", async () => {
    const { path, level, store, b } = await written();
    const keys = [testOutRef(4), testOutRef(100), testOutRef(0)];
    const proofs = await Promise.all(
      keys.map(async (key) => await proofOf(await store.at(b.root), key)),
    );
    expect((await reachableFrom(level, [b.root])).size).toBeGreaterThan(0);
    let retainedKeys = 0;
    for await (const key of level.keys())
      if (key.startsWith(RETAINED_ROOT_PREFIX)) retainedKeys += 1;
    expect(retainedKeys).toBe(3);
    await level.close();

    const mpf = await Effect.runPromise(
      MidgardMpf.load("ledger-store-level", path, Buffer.from(b.root, "hex")),
    );
    try {
      expect(await Effect.runPromise(mpf.rootHex())).toBe(b.root);
      const held = await Effect.runPromise(mpf.get(testOutRef(4)));
      expect(Option.getOrThrow(held).toString("hex")).toBe(
        ledgerEntryLeaf(base[4]!, "test").leaf.toString("hex"),
      );
      for (const [index, key] of keys.slice(0, 2).entries())
        expect(
          (await Effect.runPromise(mpf.prove(key))).cbor.toString("hex"),
        ).toBe(proofs[index]);
    } finally {
      await Effect.runPromise(mpf.close());
    }
  });

  it("persist and retain never touch the __root__ marker, and a retain keeps it readable", async () => {
    const { path, level, store, a, b, c } = await written();
    // The marker the node's own writer keeps for its current trie.
    await level.put(ROOT_KEY, a.root);
    await store.retain(new Set([a.root, b.root]));
    await expect(store.at(c.root)).rejects.toThrow(/is not retained/);
    expect(await level.get(ROOT_KEY)).toBe(a.root);
    const reachable = await reachableFrom(level, [a.root, b.root]);
    expect((await levelStorage(level).counts()).nodes).toBe(reachable.size);
    await level.close();

    const mpf = await Effect.runPromise(
      MidgardMpf.create("ledger-store-level", path),
    );
    try {
      expect(await Effect.runPromise(mpf.rootHex())).toBe(a.root);
      expect(await Effect.runPromise(mpf.persistedRootHex())).toBe(a.root);
    } finally {
      await Effect.runPromise(mpf.close());
    }
  });

  /**
   * The nodes of `a` (`base`), and of `b` and `c` built on it, as a caller
   * that already holds them hands them to `adopt`; read from a memory store
   * that persisted them.
   */
  const heldNodes = async () => {
    const memory = createMemoryLedgerStorage();
    const store = createLedgerStore(memory);
    const a = await store.admit(base, await oracleLedgerRoot(base));
    const b = await a.apply(event([0, 1], [100]));
    await store.persist(b);
    const c = await a.apply(event([2, 3], [101]));
    await store.persist(c);
    const adoption = async (root: string) => ({
      root,
      input: { nodes: await storedNodesOf(memory, root) },
    });
    return {
      a: await adoption(a.root),
      b: await adoption(b.root),
      c: await adoption(c.root),
    };
  };

  /** Node keys, count keys and every key, of the whole database. */
  const keysOf = async (level: Level<string, MpfStoredValue>) => {
    const all: string[] = [];
    for await (const key of level.keys()) all.push(key);
    return {
      all,
      nodes: new Set(all.filter((key) => /^[0-9a-f]{64}$/.test(key))),
      counted: new Set(
        all
          .filter((key) => key.startsWith("__refs__/"))
          .map((key) => key.slice("__refs__/".length)),
      ),
    };
  };

  it("adopt moves __root__ to the adopted root and writes no node key without its count", async () => {
    const { a, b } = await heldNodes();
    const level = await openLevel(await freshPath());
    try {
      const store = createLedgerStore(levelStorage(level));
      await store.adopt(a.root, a.input);
      expect(await level.get(ROOT_KEY)).toBe(a.root);
      await store.adopt(b.root, b.input);
      expect(await level.get(ROOT_KEY)).toBe(b.root);
      const keys = await keysOf(level);
      expect(keys.counted).toEqual(keys.nodes);
      expect(keys.nodes).toEqual(await reachableFrom(level, [a.root, b.root]));
      await expectLedgerMatches(
        await store.at(b.root),
        appliedEntries(base, event([0, 1], [100])),
      );
    } finally {
      await level.close();
    }
  });

  it("a retain after adopt leaves no key of the root it drops", async () => {
    const { a, b, c } = await heldNodes();
    const level = await openLevel(await freshPath());
    try {
      const store = createLedgerStore(levelStorage(level));
      for (const { root, input } of [a, b, c]) await store.adopt(root, input);
      await store.retain(new Set([a.root, c.root]));
      const keys = await keysOf(level);
      expect(keys.all.filter((key) => key.includes(b.root))).toEqual([]);
      expect(keys.nodes).toEqual(await reachableFrom(level, [a.root, c.root]));
      expect(keys.counted).toEqual(keys.nodes);
      expect(await level.get(ROOT_KEY)).toBe(c.root);
    } finally {
      await level.close();
    }
  });

  const genesis = async ({ store }: NativeOwnerLedger) =>
    await writeGenesisLedger(store, base, await oracleLedgerRoot(base));

  /**
   * Opens the node's ledger at `path` with every write after the first
   * `survive` refused, as if the process died there.
   */
  const openDyingAfter = async (path: string, survive: number) => {
    const ledger = await openNativeOwnerLedger(path, refuseOutputs);
    const level = ledger.level as unknown as Record<
      string,
      (...args: unknown[]) => Promise<unknown>
    >;
    let writes = 0;
    for (const method of ["batch", "put", "del"]) {
      const write = level[method]!.bind(ledger.level);
      level[method] = (...args) =>
        writes++ < survive
          ? write(...args)
          : Promise.reject(new Error("killed"));
    }
    return ledger;
  };

  it("genesis killed at any write leaves an empty or whole store, and restarting completes it", async () => {
    const root = await oracleLedgerRoot(base);
    for (let survive = 0, killed = true; killed; survive += 1) {
      const path = await freshPath();
      const dying = await openDyingAfter(path, survive);
      killed = await genesis(dying).then(
        () => false,
        () => true,
      );
      await dying.level.close();
      const restarted = await openNativeOwnerLedger(path, refuseOutputs);
      const level = restarted.level as unknown as Level<string, MpfStoredValue>;
      try {
        if ((await level.get(ROOT_KEY)) === undefined) {
          expect((await keysOf(level)).all).toEqual([]);
          await genesis(restarted);
        }
        expect(await level.get(ROOT_KEY)).toBe(root);
        await restarted.store.at(root);
        const keys = await keysOf(level);
        expect(keys.counted).toEqual(keys.nodes);
        expect(keys.nodes).toEqual(await reachableFrom(level, [root]));
        const leaves = (await level.getMany([...keys.nodes])) as StoredNode[];
        expect(
          new Set(
            leaves.flatMap((node) =>
              node.__kind === "Leaf" ? [node.key] : [],
            ),
          ),
        ).toEqual(new Set(base.map(({ outRef }) => outRef.toString("hex"))));
      } finally {
        await restarted.level.close();
      }
    }
  });
});
