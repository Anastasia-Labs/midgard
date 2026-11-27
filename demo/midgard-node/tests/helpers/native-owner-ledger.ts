/**
 * The native owner's ledger LevelDB in tests: hand-built leaves, seeding
 * through the node's ledger store (so a seeded node is counted and its root
 * retained, as genesis leaves it), and the store's records read back.
 */
import { EMPTY_LEDGER_ROOT } from "@al-ft/midgard-fault-proofs/ledger-store";
import { expect } from "vitest";

import { RETAINED_ROOT_PREFIX } from "../../src/mpf/ledger-store-level.js";
import type {
  NativeMpfOwnerServiceOptions,
  StoredNode,
} from "../../src/services/mpf-native-owner/service.normalize-owner-options.js";
import {
  type LedgerOutputReader,
  openNativeOwnerLedger,
} from "../../src/services/mpf-native-owner/service.open-ledger.js";
import { ProductionNativeMpfOwnerService } from "../../src/services/mpf-native-owner/service.production-native-mpf-owner-service.js";
import { createEventFlatDigest } from "../../src/workers/utils/mpf-event-flat-digest.js";

/** An output reader for owner tests, none of which reads an output. */
export const refuseOutputs: LedgerOutputReader = () =>
  Promise.reject(new Error("This test reads no ledger outputs"));

/** Needs `prepareEventFlatDigest` first. */
export const digest = (...parts: readonly Uint8Array[]): Buffer => {
  const state = createEventFlatDigest();
  for (const part of parts) state.update(part);
  return state.digest();
};

export const nibbles = (value: Uint8Array): string =>
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

/** The root of the trie holding `key` alone. */
export const leafRoot = (key: Buffer, value: Buffer): string =>
  digest(
    Buffer.from([0xff]),
    packedNibbles(nibbles(key)),
    digest(value),
  ).toString("hex");

/**
 * Writes `root` with `nodes` through the node's ledger store, moving
 * `__root__` to it.
 */
export const seedOwnerLedger = async (
  levelPath: string,
  root: string,
  nodes: ReadonlyMap<string, StoredNode> = new Map(),
): Promise<void> => {
  const { level, store } = await openNativeOwnerLedger(
    levelPath,
    refuseOutputs,
  );
  try {
    await store.adopt(root, { nodes });
  } finally {
    await level.close();
  }
};

/** Seeds the trie holding `key` alone and answers its root. */
export const seedLeafLedger = async (
  levelPath: string,
  key: Buffer,
  value: Buffer,
): Promise<string> => {
  const root = leafRoot(key, value);
  const leaf: StoredNode = {
    __kind: "Leaf",
    prefix: nibbles(key),
    key: key.toString("hex"),
    value: value.toString("hex"),
  };
  await seedOwnerLedger(levelPath, root, new Map([[root, leaf]]));
  return root;
};

const NODE_KEY = /^[0-9a-f]{64}$/;
const COUNT_PREFIX = "__refs__/";

/**
 * The ledger LevelDB's records: the `__root__` marker, node keys, their
 * stored counts and the retained roots, with each node's count as the store
 * defines it (one per branch listing it, plus one for a retained root)
 * derived from the nodes alone. `opened` answers, for the marker and each of
 * `roots`, whether the store opens that root.
 */
export const ledgerStoreRecords = async (
  levelPath: string,
  roots: readonly string[] = [],
) => {
  const { level, store } = await openNativeOwnerLedger(
    levelPath,
    refuseOutputs,
  );
  try {
    const nodes = new Map<string, StoredNode>();
    const counts = new Map<string, number>();
    const retained = new Set<string>();
    for await (const [key, value] of level.iterator()) {
      if (NODE_KEY.test(key)) nodes.set(key, value as StoredNode);
      else if (key.startsWith(COUNT_PREFIX))
        counts.set(key.slice(COUNT_PREFIX.length), Number(value));
      else if (key.startsWith(RETAINED_ROOT_PREFIX))
        retained.add(key.slice(RETAINED_ROOT_PREFIX.length));
    }
    const expectedCounts = new Map<string, number>();
    const add = (hash: string, by: number) =>
      expectedCounts.set(hash, (expectedCounts.get(hash) ?? 0) + by);
    for (const [hash, node] of nodes) {
      add(hash, retained.has(hash) ? 1 : 0);
      if (node.__kind === "Branch")
        for (const child of node.children) if (child !== null) add(child, 1);
    }
    const marker = String(await level.get("__root__"));
    const opened = new Map<string, boolean>();
    for (const root of [marker, ...roots])
      opened.set(
        root,
        await store.at(root).then(
          () => true,
          () => false,
        ),
      );
    return { marker, nodes, counts, expectedCounts, retained, opened };
  } finally {
    await level.close();
  }
};

type LedgerStoreRecords = Awaited<ReturnType<typeof ledgerStoreRecords>>;

/** The node keys under `root`; a node missing from the store fails. */
export const reachableNodes = (
  records: LedgerStoreRecords,
  root: string,
): Set<string> => {
  const reached = new Set<string>();
  const visit = (hash: string): void => {
    if (hash === EMPTY_LEDGER_ROOT || reached.has(hash)) return;
    const node = records.nodes.get(hash);
    expect(node, `node ${hash} under ${root}`).toBeDefined();
    reached.add(hash);
    if (node?.__kind === "Branch")
      for (const child of node.children) if (child !== null) visit(child);
  };
  visit(root);
  return reached;
};

/** The hex keys of the leaves under `root`. */
export const leafKeys = (records: LedgerStoreRecords, root: string) =>
  new Set(
    [...reachableNodes(records, root)].flatMap((hash) => {
      const node = records.nodes.get(hash)!;
      return node.__kind === "Leaf" ? [node.key] : [];
    }),
  );

/**
 * The marker's root is retained and opens; every node key has its count and
 * every count its node key; each count is the one the nodes give; and every
 * node under the marker is held.
 */
export const expectCountedLedger = (records: LedgerStoreRecords): void => {
  expect(records.retained).toContain(records.marker);
  expect(records.opened.get(records.marker)).toBe(true);
  expect(new Set(records.counts.keys())).toEqual(new Set(records.nodes.keys()));
  expect(records.counts).toEqual(records.expectedCounts);
  for (const hash of reachableNodes(records, records.marker))
    expect(records.counts.get(hash), hash).toBeGreaterThan(0);
};

/**
 * Opens the ledger at `levelPath`, reading outputs through `readOutputs`,
 * and hands it to a new owner service.
 */
export const createOwnerService = async (
  levelPath: string,
  options: NativeMpfOwnerServiceOptions,
  readOutputs: LedgerOutputReader = refuseOutputs,
): Promise<ProductionNativeMpfOwnerService> =>
  ProductionNativeMpfOwnerService.create(
    await openNativeOwnerLedger(levelPath, readOutputs),
    options,
  );
