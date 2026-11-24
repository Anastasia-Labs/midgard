/**
 * The native owner's node cache over a trie of a few thousand nodes: start
 * reads no node, a block reads the nodes it needs and few others, the cache
 * keeps to its budget, and a node the store lacks is named. Roots are checked
 * against a scratch `MidgardMpf`.
 */
import { createHash } from "node:crypto";
import { mkdtemp, readFile, rm } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";

import { Level } from "level";
import { afterEach, beforeAll, describe, expect, it } from "vitest";

import { NativeMpfClosureIncomplete } from "../src/services/mpf-native-owner/protocol.js";
import { encodeNativeMpfEventLog } from "../src/services/mpf-native-owner/service.parse-promotion-records.js";
import { ProductionNativeMpfOwnerService } from "../src/services/mpf-native-owner/service.production-native-mpf-owner-service.js";
import { prepareEventFlatDigest } from "../src/workers/utils/mpf-event-flat-digest.js";
import {
  nativeOwnerBinaryPath,
  nativeOwnerBinaryPresent,
  warnNativeOwnerBinaryAbsent,
} from "./helpers/native-owner-binary.js";
import {
  createOwnerService,
  ledgerStoreRecords,
  reachableNodes,
} from "./helpers/native-owner-ledger.js";
import {
  openCountingLedger,
  promoteBlock,
  referenceRoots,
  seedTrieLedger,
  trieBlocks,
  trieInsert,
} from "./helpers/native-owner-trie.js";

const binaryPath = nativeOwnerBinaryPath;
const binaryPresent = nativeOwnerBinaryPresent();
if (!binaryPresent) {
  warnNativeOwnerBinaryAbsent("mpf-native-owner-node-cache");
}

describe.skipIf(!binaryPresent)("native MPF owner node cache", () => {
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

  // A trie of about eleven thousand nodes. A block of a few dozen keys reads
  // a small part of it: the nodes its apply asks for.
  const KEY_COUNT = 8_000;

  /** A ledger seeded with the trie of keys `0..KEY_COUNT-1`, and a new owner
   * over it whose node reads are counted. */
  const openTrieOwner = async (
    prefix: string,
    options: { readonly nodeCacheBytes?: number } = {},
  ) => {
    const directory = await mkdtemp(join(tmpdir(), prefix));
    temporaryPaths.push(directory);
    const levelPath = join(directory, "ledger");
    const seeded = await seedTrieLedger(
      levelPath,
      { binaryPath, binarySha256 },
      KEY_COUNT,
    );
    const { ledger, nodeReads } = await openCountingLedger(levelPath);
    const service = await ProductionNativeMpfOwnerService.create(ledger, {
      binaryPath,
      binarySha256,
      ...options,
    });
    return { levelPath, seeded, service, nodeReads };
  };

  it("starts over a seeded trie without reading a trie node", async () => {
    const { seeded, service, nodeReads } = await openTrieOwner(
      "midgard-native-owner-start-",
    );
    try {
      expect(seeded.nodes).toBeGreaterThan(KEY_COUNT);
      expect(nodeReads()).toBe(0);
      expect(await service.diagnostics()).toMatchObject({
        durableRoot: seeded.root,
        cachedNodes: 0,
        cachedBytes: 0,
      });
      expect(nodeReads()).toBe(0);
    } finally {
      await service.close();
    }
  });

  it("reads only the trie nodes the first block after start needs", async () => {
    const blocks = trieBlocks({ keyCount: KEY_COUNT, blocks: 1 });
    const reference = await referenceRoots(KEY_COUNT, blocks);
    const { seeded, service, nodeReads } = await openTrieOwner(
      "midgard-native-owner-first-block-",
    );
    try {
      expect(seeded.root).toBe(reference.seeded);
      expect(await promoteBlock(service, seeded.root, blocks[0]!)).toEqual(
        reference.blocks[0],
      );
      expect(nodeReads()).toBeGreaterThan(0);
      expect(nodeReads()).toBeLessThan(seeded.nodes / 5);
    } finally {
      await service.close();
    }
  });

  it("keeps its node cache within a budget smaller than one block needs, block after block", async () => {
    // Every cached node costs more than 100 bytes, so this budget holds
    // fewer than 41 nodes: far fewer than one block reads.
    const nodeCacheBytes = 4_096;
    const blocks = trieBlocks({ keyCount: KEY_COUNT, blocks: 8 });
    const reference = await referenceRoots(KEY_COUNT, blocks);
    const { seeded, service, nodeReads } = await openTrieOwner(
      "midgard-native-owner-small-cache-",
      { nodeCacheBytes },
    );
    try {
      let root = seeded.root;
      for (const [index, block] of blocks.entries()) {
        const before = nodeReads();
        expect(await promoteBlock(service, root, block)).toEqual(
          reference.blocks[index],
        );
        root = reference.blocks[index]!.at(-1)!;
        expect(nodeReads() - before).toBeGreaterThan(nodeCacheBytes / 100);
        const diagnostics = await service.diagnostics();
        expect(diagnostics.durableRoot).toBe(root);
        expect(diagnostics.cachedBytes).toBeLessThanOrEqual(nodeCacheBytes);
      }
    } finally {
      await service.close();
    }
  });

  it("names a trie node the store lacks, changes nothing, and still applies a block that does not need it", async () => {
    const directory = await mkdtemp(
      join(tmpdir(), "midgard-native-owner-missing-node-"),
    );
    temporaryPaths.push(directory);
    const levelPath = join(directory, "ledger");
    const seeded = await seedTrieLedger(
      levelPath,
      { binaryPath, binarySha256 },
      KEY_COUNT,
    );
    const lostKey = trieInsert(0).key;
    const records = await ledgerStoreRecords(levelPath);
    const lost = [...reachableNodes(records, seeded.root)].find((hash) => {
      const node = records.nodes.get(hash)!;
      return (
        node.__kind === "Leaf" &&
        node.key === Buffer.from(lostKey).toString("hex")
      );
    })!;
    expect(lost).toBeDefined();
    const level = new Level<string, unknown>(levelPath, {
      valueEncoding: "json",
    });
    await level.open();
    await level.del(lost);
    await level.close();

    const unaffected = [[trieInsert(5_000_000), trieInsert(5_000_001)]];
    const reference = await referenceRoots(KEY_COUNT, [unaffected]);
    const service = await createOwnerService(levelPath, {
      binaryPath,
      binarySha256,
    });
    try {
      const needing = await service.fork(seeded.root);
      const refusal = await service
        .applyEvents(
          needing,
          encodeNativeMpfEventLog(seeded.root, [
            [{ type: "delete", key: lostKey }],
          ]),
        )
        .then(
          () => undefined,
          (error: unknown) => error,
        );
      expect(refusal).toBeInstanceOf(NativeMpfClosureIncomplete);
      expect(refusal).toMatchObject({ root: seeded.root, missing: [lost] });
      await service.discard(needing);
      expect(await service.diagnostics()).toMatchObject({
        durableRoot: seeded.root,
        activeGenerations: 0,
      });
      expect(await promoteBlock(service, seeded.root, unaffected)).toEqual(
        reference.blocks[0],
      );
    } finally {
      await service.close();
    }
  });
});
