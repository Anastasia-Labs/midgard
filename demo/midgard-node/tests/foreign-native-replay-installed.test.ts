import { createHash } from "node:crypto";
import { mkdtemp, readFile, rm, writeFile } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";

import * as SDK from "@al-ft/midgard-sdk";
import { Effect } from "effect";
import { Level } from "level";
import { afterEach, beforeAll, describe, expect, it } from "vitest";

import { MidgardMpf } from "../src/mpf/index.js";
import {
  type ForeignNativeReplayBlock,
  prepareForeignNativeReplay,
} from "../src/services/foreign-native-replay.js";
import {
  encodeNativeMpfEventLog,
  type NativeMpfEventOp,
  NativeMpfWorkerPortClient,
  type PersistedNativeMpfReplay,
  ProductionNativeMpfOwnerService,
} from "../src/services/mpf-native-owner/index.js";
import type { NativeMpfOwnerServiceOptions } from "../src/services/mpf-native-owner/service.js";
import {
  digest,
  EVENT_LOG_DIGEST_DOMAIN,
} from "../src/services/mpf-native-owner/service.normalize-owner-options.js";
import { prepareEventFlatDigest } from "../src/workers/utils/mpf-event-flat-digest.js";
import {
  nativeOwnerBinaryPath,
  nativeOwnerBinaryPresent,
  warnNativeOwnerBinaryAbsent,
} from "./helpers/native-owner-binary.js";

const binaryPresent = nativeOwnerBinaryPresent();
if (!binaryPresent)
  warnNativeOwnerBinaryAbsent("foreign-native-replay-installed");

type ReplayArtifact = Omit<
  PersistedNativeMpfReplay,
  "eventLog" | "eventRoots"
> & {
  eventLog: string;
  eventRoots: string;
};

// This is an independent TypeScript trie oracle, not native apply output.
const eventRoots = async (
  events: readonly (readonly NativeMpfEventOp[])[],
): Promise<readonly string[]> => {
  const trie = await Effect.runPromise(
    MidgardMpf.createScratch("foreign-native-replay-oracle"),
  );
  try {
    const roots: string[] = [];
    for (const operations of events) {
      const root = await Effect.runPromise(
        trie.applyBatch(
          operations.map((operation) =>
            operation.type === "insert"
              ? {
                  type: "insert" as const,
                  key: Buffer.from(operation.key),
                  value: Buffer.from(operation.value),
                }
              : { type: "delete" as const, key: Buffer.from(operation.key) },
          ),
        ),
      );
      roots.push(root.toString("hex"));
    }
    return roots;
  } finally {
    await Effect.runPromise(trie.close());
  }
};

describe.skipIf(!binaryPresent)(
  "foreign replay with the pinned native owner",
  () => {
    const directories: string[] = [];
    const services = new Set<ProductionNativeMpfOwnerService>();
    let binarySha256: string;

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
        directories
          .splice(0)
          .map((directory) => rm(directory, { recursive: true, force: true })),
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

    const fixture = async () => {
      const directory = await mkdtemp(
        join(tmpdir(), "midgard-foreign-replay-"),
      );
      directories.push(directory);
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
      try {
        await seed.put("__root__", SDK.EMPTY_MERKLE_TREE_ROOT);
      } finally {
        await seed.close();
      }
      const keys = [41, 42, 43, 44].map((byte) => Buffer.alloc(32, byte));
      const values = [51, 52, 53, 54].map((byte) => Buffer.alloc(64, byte));
      const insert = (index: number): NativeMpfEventOp => ({
        type: "insert",
        key: keys[index]!,
        value: values[index]!,
      });
      // Foreign insertion/no-op, local replacement, then another foreign insert.
      // Multiple live leaves ensure the native replay must retain branch content.
      const events: readonly (readonly NativeMpfEventOp[])[] = [
        [insert(0), insert(1)],
        [],
        [{ type: "delete", key: keys[0]! }, insert(2)],
        [insert(3)],
      ];
      const childEvents: readonly (readonly NativeMpfEventOp[])[] = [
        [{ type: "delete", key: keys[1]! }],
        [
          { type: "delete", key: keys[2]! },
          { type: "delete", key: keys[3]! },
        ],
      ];
      const expected = await eventRoots([...events, ...childEvents]);
      const foreign = (
        index: number,
        parentRoot: string,
        blockEvents: typeof events,
        roots: readonly string[],
      ): ForeignNativeReplayBlock => ({
        kind: "foreign",
        headerHash: Buffer.alloc(28, index).toString("hex"),
        parentHeaderHash: Buffer.alloc(28, index - 1).toString("hex"),
        parentUtxosRoot: parentRoot,
        root: roots.at(-1)!,
        events: blockEvents.map((operations) =>
          operations.map((operation) => ({
            key: Buffer.from(operation.key).toString("hex"),
            output:
              operation.type === "insert" ? Buffer.from(operation.value) : null,
          })),
        ),
        eventRoots: roots,
      });
      const localLog = encodeNativeMpfEventLog(expected[1]!, [events[2]!]);
      const first = foreign(
        1,
        SDK.EMPTY_MERKLE_TREE_ROOT,
        events.slice(0, 2),
        expected.slice(0, 2),
      );
      const local: ForeignNativeReplayBlock = {
        kind: "local",
        headerHash: Buffer.alloc(28, 2).toString("hex"),
        parentHeaderHash: first.headerHash,
        parentUtxosRoot: expected[1]!,
        root: expected[2]!,
        events: [],
        eventRoots: [expected[2]!],
        nativeMpfReplay: {
          schema: 1,
          ownerBinarySha256: Buffer.from(binarySha256, "hex"),
          baseRoot: Buffer.from(expected[1]!, "hex"),
          candidateRoot: Buffer.from(expected[2]!, "hex"),
          eventLog: localLog,
          eventLogDigest: digest(EVENT_LOG_DIGEST_DOMAIN, localLog),
          eventRoots: Buffer.from(expected[2]!, "hex"),
          eventCount: 1,
        },
      };
      const last = foreign(3, local.root, [events[3]!], [expected[3]!]);
      const base = {
        headerHash: last.headerHash,
        root: last.root,
        importedBlocks: [first, local, last],
      };
      return { directory, options, events, childEvents, expected, base };
    };

    it("persists a foreign/local/foreign replay, recovers it in a new epoch, and continues a child fork", async () => {
      const f = await fixture();
      const owner = await open(f.options);
      const prepared = await Effect.runPromise(
        prepareForeignNativeReplay({
          owner,
          base: f.base,
          ownerBinarySha256: binarySha256,
        }),
      );
      if (prepared === undefined)
        throw new Error("expected a nonempty foreign replay");
      const expectedLog = encodeNativeMpfEventLog(
        SDK.EMPTY_MERKLE_TREE_ROOT,
        f.events,
      );
      expect(Buffer.from(prepared.replay.eventLog)).toEqual(expectedLog);
      expect(Buffer.from(prepared.replay.eventRoots)).toEqual(
        Buffer.from(f.expected.slice(0, 4).join(""), "hex"),
      );
      expect(prepared.replay).toMatchObject({
        schema: 1,
        ownerBinarySha256: binarySha256,
        baseRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
        candidateRoot: f.expected[3],
        eventCount: 4,
      });
      expect(await owner.diagnostics()).toMatchObject({
        durableRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
        activeGenerations: 1,
      });

      const artifact: ReplayArtifact = {
        ...prepared.replay,
        eventLog: Buffer.from(prepared.replay.eventLog).toString("hex"),
        eventRoots: Buffer.from(prepared.replay.eventRoots).toString("hex"),
      };
      const artifactPath = join(f.directory, "parent-replay.json");
      await writeFile(artifactPath, JSON.stringify(artifact));
      await close(owner);
      const saved = JSON.parse(
        await readFile(artifactPath, "utf8"),
      ) as ReplayArtifact;
      expect(saved).toEqual(artifact);
      const replay: PersistedNativeMpfReplay = {
        ...saved,
        eventLog: Buffer.from(saved.eventLog, "hex"),
        eventRoots: Buffer.from(saved.eventRoots, "hex"),
      };
      const restarted = await open(f.options);
      expect((await restarted.diagnostics()).durableRoot).toBe(
        SDK.EMPTY_MERKLE_TREE_ROOT,
      );
      await expect(
        restarted.applyEvents(prepared.handle, expectedLog),
      ).rejects.toThrow(/stale owner epoch/);
      // Only the parent calls recover/promote; the kernel merely prepares a fork.
      await restarted.recover(replay);
      expect(await restarted.diagnostics()).toMatchObject({
        durableRoot: f.expected[3],
        activeGenerations: 0,
      });
      await close(restarted);
      const promoted = await open(f.options);
      expect((await promoted.diagnostics()).durableRoot).toBe(f.expected[3]);
      const worker = new NativeMpfWorkerPortClient(promoted.createWorkerPort());
      try {
        const child = await worker.fork(f.base.root);
        const applied = await worker.applyEvents(
          child,
          encodeNativeMpfEventLog(f.base.root, f.childEvents),
        );
        expect(applied.eventRoots).toEqual(f.expected.slice(4));
        expect(applied.candidateRoot).toBe(SDK.EMPTY_MERKLE_TREE_ROOT);
        expect((await promoted.diagnostics()).durableRoot).toBe(f.base.root);
        await worker.discard(child);
      } finally {
        worker.close();
      }
      expect((await promoted.diagnostics()).activeGenerations).toBe(0);
    });

    it("refuses a false intermediate E1 root, discards the unjournalled fork, and leaves promotion unchanged", async () => {
      const f = await fixture();
      const owner = await open(f.options);
      // Keep the true final root and ancestry, but replace the first event root
      // with the independently computed root of a different ledger state.
      const first = f.base.importedBlocks[0]!;
      const wrongFirst: ForeignNativeReplayBlock = {
        ...first,
        eventRoots: [f.expected[2]!, first.eventRoots[1]!],
      };
      expect(wrongFirst.eventRoots[0]).not.toBe(first.eventRoots[0]);
      await expect(
        Effect.runPromise(
          prepareForeignNativeReplay({
            owner,
            ownerBinarySha256: binarySha256,
            base: {
              ...f.base,
              importedBlocks: [wrongFirst, ...f.base.importedBlocks.slice(1)],
            },
          }),
        ),
      ).rejects.toThrow(/independently verified E1 roots/);
      expect(await owner.diagnostics()).toMatchObject({
        durableRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
        activeGenerations: 0,
      });
      // A subsequent honest attempt can still allocate and apply normally.
      const accepted = await Effect.runPromise(
        prepareForeignNativeReplay({
          owner,
          base: f.base,
          ownerBinarySha256: binarySha256,
        }),
      );
      if (accepted === undefined) throw new Error("expected the honest replay");
      await owner.promote(accepted.handle);
      expect((await owner.diagnostics()).durableRoot).toBe(f.expected[3]);
    });
  },
);
