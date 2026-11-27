/**
 * A native owner ledger holding a trie of thousands of keys, blocks of
 * deletes and inserts over it, their roots from a scratch `MidgardMpf` (which
 * shares no code with the owner's child), and a ledger handle that counts the
 * trie nodes read through it.
 */
import { createHash } from "node:crypto";

import * as SDK from "@al-ft/midgard-sdk/core";
import { Effect } from "effect";

import { MidgardMpf } from "../../src/mpf/store.midgard-mpf.js";
import type {
  NativeMpfEventOp,
  NativeMpfOwnerServiceOptions,
} from "../../src/services/mpf-native-owner/service.normalize-owner-options.js";
import { openNativeOwnerLedger } from "../../src/services/mpf-native-owner/service.open-ledger.js";
import { encodeNativeMpfEventLog } from "../../src/services/mpf-native-owner/service.parse-promotion-records.js";
import type { ProductionNativeMpfOwnerService } from "../../src/services/mpf-native-owner/service.production-native-mpf-owner-service.js";
import {
  createOwnerService,
  ledgerStoreRecords,
  reachableNodes,
  refuseOutputs,
  seedOwnerLedger,
} from "./native-owner-ledger.js";

const NODE_KEY = /^[0-9a-f]{64}$/;

const filled = (label: string, index: number, length: number): Buffer => {
  const bytes = Buffer.alloc(length);
  for (let offset = 0; offset < length; offset += 32)
    createHash("sha256")
      .update(`${label}:${index.toString()}:${offset.toString()}`)
      .digest()
      .copy(bytes, offset);
  return bytes;
};

const trieKey = (index: number): Buffer => filled("key", index, 32);

/** Inserts key `index` with its value. */
export const trieInsert = (index: number): NativeMpfEventOp => ({
  type: "insert",
  key: trieKey(index),
  value: filled("value", index, 64),
});

export type TrieBlock = readonly (readonly NativeMpfEventOp[])[];

/**
 * `blocks` blocks over the trie holding keys `0..keyCount-1`, each of
 * `events` events that delete `perEvent` live keys and insert `perEvent` new
 * ones. Deterministic for the same arguments.
 */
export const trieBlocks = ({
  keyCount,
  blocks,
  events = 3,
  perEvent = 8,
  salt = 1,
}: {
  readonly keyCount: number;
  readonly blocks: number;
  readonly events?: number;
  readonly perEvent?: number;
  readonly salt?: number;
}): TrieBlock[] => {
  const live = Array.from({ length: keyCount }, (_, index) => index);
  let next = keyCount + salt * 1_000_000;
  let state = salt;
  const pick = () => {
    state = (state * 1_103_515_245 + 12_345) % 2_147_483_648;
    return live.splice(state % live.length, 1)[0]!;
  };
  return Array.from({ length: blocks }, () =>
    Array.from({ length: events }, () => {
      const deleted = Array.from({ length: perEvent }, pick);
      const inserted = Array.from({ length: perEvent }, () => next++);
      live.push(...inserted);
      return [
        ...deleted.map(
          (index): NativeMpfEventOp => ({
            type: "delete",
            key: trieKey(index),
          }),
        ),
        ...inserted.map(trieInsert),
      ];
    }),
  );
};

/**
 * The root of the trie holding keys `0..keyCount-1`, then the roots after
 * each event of each of `blocks`, from a scratch `MidgardMpf`.
 */
export const referenceRoots = async (
  keyCount: number,
  blocks: readonly TrieBlock[],
): Promise<{ readonly seeded: string; readonly blocks: string[][] }> => {
  const mpf = await Effect.runPromise(
    MidgardMpf.createScratch("native-owner-trie-reference"),
  );
  try {
    const applyEvent = async (event: readonly NativeMpfEventOp[]) => {
      await Effect.runPromise(
        mpf.applyBatch(
          event.map((op) =>
            op.type === "insert"
              ? {
                  type: "insert" as const,
                  key: Buffer.from(op.key),
                  value: Buffer.from(op.value),
                }
              : { type: "delete" as const, key: Buffer.from(op.key) },
          ),
        ),
      );
      return Effect.runPromise(mpf.rootHex());
    };
    const seeded = await applyEvent(
      Array.from({ length: keyCount }, (_, index) => trieInsert(index)),
    );
    const roots: string[][] = [];
    for (const block of blocks) {
      const blockRoots: string[] = [];
      for (const event of block) blockRoots.push(await applyEvent(event));
      roots.push(blockRoots);
    }
    return { seeded, blocks: roots };
  } finally {
    await Effect.runPromise(mpf.close());
  }
};

/**
 * Seeds the ledger at `levelPath` with the trie holding keys
 * `0..keyCount-1`, promoted by an owner from the empty root, and answers its
 * root and how many trie nodes it has.
 */
export const seedTrieLedger = async (
  levelPath: string,
  options: NativeMpfOwnerServiceOptions,
  keyCount: number,
): Promise<{ readonly root: string; readonly nodes: number }> => {
  const empty = SDK.EMPTY_MERKLE_TREE_ROOT;
  await seedOwnerLedger(levelPath, empty);
  const service = await createOwnerService(levelPath, options);
  let root: string;
  try {
    const handle = await service.fork(empty);
    const applied = await service.applyEvents(
      handle,
      encodeNativeMpfEventLog(empty, [
        Array.from({ length: keyCount }, (_, index) => trieInsert(index)),
      ]),
    );
    await service.promote(handle);
    root = applied.candidateRoot;
  } finally {
    await service.close();
  }
  return {
    root,
    nodes: reachableNodes(await ledgerStoreRecords(levelPath), root).size,
  };
};

/**
 * Opens the ledger at `levelPath` for an owner, counting the trie node keys
 * read through its LevelDB handle (`get` and `getMany`), which every reader
 * in the owner and its ledger store goes through.
 */
export const openCountingLedger = async (levelPath: string) => {
  const ledger = await openNativeOwnerLedger(levelPath, refuseOutputs);
  const { level } = ledger;
  let reads = 0;
  const get = level.get.bind(level);
  const getMany = level.getMany.bind(level);
  level.get = ((key: string, ...rest: unknown[]) => {
    if (NODE_KEY.test(key)) reads += 1;
    return (get as (...args: unknown[]) => unknown)(key, ...rest);
  }) as typeof level.get;
  level.getMany = ((keys: string[], ...rest: unknown[]) => {
    reads += keys.filter((key) => NODE_KEY.test(key)).length;
    return (getMany as (...args: unknown[]) => unknown)(keys, ...rest);
  }) as typeof level.getMany;
  return { ledger, nodeReads: () => reads };
};

/** Forks `from`, applies `block` and promotes it; answers the roots after
 * each event. */
export const promoteBlock = async (
  service: ProductionNativeMpfOwnerService,
  from: string,
  block: TrieBlock,
): Promise<readonly string[]> => {
  const handle = await service.fork(from);
  const applied = await service.applyEvents(
    handle,
    encodeNativeMpfEventLog(from, block),
  );
  await service.promote(handle);
  return applied.eventRoots;
};
