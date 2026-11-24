/**
 * The native trie owner at ledger scale: how long it takes to start on an
 * existing ledger, and what a block costs with its paths cached, with an
 * empty cache, and with a cache a tenth of the trie.
 *
 *   BENCH_OWNER_DIR    directory holding (or to hold) the ledger LevelDB
 *   BENCH_OWNER_UTXOS  ledger size, default 200000
 *   BENCH_OWNER_MODE   "build" writes the ledger; "measure" runs blocks on it
 *
 * `measure` changes the ledger, so run it on a copy of a built one. The
 * binary must be built first (`pnpm run native:mpf-owner:build`).
 */
import { createHash } from "node:crypto";
import { mkdirSync } from "node:fs";
import { join } from "node:path";
import { performance } from "node:perf_hooks";

import * as SDK from "@al-ft/midgard-sdk/core";
import { describe, expect, it } from "vitest";

import type { NativeMpfEventOp } from "../../src/services/mpf-native-owner/service.normalize-owner-options.js";
import { openNativeOwnerLedger } from "../../src/services/mpf-native-owner/service.open-ledger.js";
import { encodeNativeMpfEventLog } from "../../src/services/mpf-native-owner/service.parse-promotion-records.js";
import { ProductionNativeMpfOwnerService } from "../../src/services/mpf-native-owner/service.production-native-mpf-owner-service.js";
import { prepareEventFlatDigest } from "../../src/workers/utils/mpf-event-flat-digest.js";
import {
  nativeOwnerBinaryPath,
  nativeOwnerBinarySha256,
} from "../helpers/native-owner-binary.js";
import {
  refuseOutputs,
  seedOwnerLedger,
} from "../helpers/native-owner-ledger.js";
import { writeBenchmarkJson } from "./benchmark-utils.js";

const directory = process.env.BENCH_OWNER_DIR;
const utxos = Number(process.env.BENCH_OWNER_UTXOS ?? "200000");
const mode = process.env.BENCH_OWNER_MODE ?? "measure";

const OPS_PER_EVENT = 4;
const BUILD_BLOCK_OPS = 40_000;
const NODE_KEY = /^[0-9a-f]{64}$/;

const keyOf = (index: number) =>
  createHash("sha256").update(`bench-key-${index.toString()}`).digest();

const valueOf = (index: number) =>
  Buffer.concat([
    createHash("sha256").update(`bench-value-${index.toString()}`).digest(),
    Buffer.alloc(32, 7),
  ]);

const insert = (index: number): NativeMpfEventOp => ({
  type: "insert",
  key: keyOf(index),
  value: valueOf(index),
});

const remove = (index: number): NativeMpfEventOp => ({
  type: "delete",
  key: keyOf(index),
});

const events = (ops: readonly NativeMpfEventOp[]): NativeMpfEventOp[][] =>
  Array.from({ length: Math.ceil(ops.length / OPS_PER_EVENT) }, (_, index) =>
    ops.slice(index * OPS_PER_EVENT, (index + 1) * OPS_PER_EVENT),
  );

/** mulberry32: the same blocks on every run. */
const random = (seed: number) => () => {
  seed = (seed + 0x6d2b79f5) | 0;
  let t = Math.imul(seed ^ (seed >>> 15), 1 | seed);
  t = (t + Math.imul(t ^ (t >>> 7), 61 | t)) ^ t;
  return ((t ^ (t >>> 14)) >>> 0) / 4294967296;
};

type Reads = { calls: number; nodes: number };

const openOwner = async (
  levelPath: string,
  options: { readonly nodeCacheBytes?: number } = {},
) => {
  const ledger = await openNativeOwnerLedger(levelPath, refuseOutputs);
  const reads: Reads = { calls: 0, nodes: 0 };
  const { level } = ledger;
  const getMany = level.getMany.bind(level);
  (level as unknown as { getMany: typeof getMany }).getMany = ((
    keys: string[],
    ...rest: []
  ) => {
    const nodes = keys.filter((key) => NODE_KEY.test(key)).length;
    if (nodes > 0) {
      reads.calls += 1;
      reads.nodes += nodes;
    }
    return getMany(keys, ...rest);
  }) as typeof getMany;
  const started = performance.now();
  const service = await ProductionNativeMpfOwnerService.create(ledger, {
    binaryPath: nativeOwnerBinaryPath,
    binarySha256: nativeOwnerBinarySha256(),
    ...options,
  });
  return { service, reads, startMs: performance.now() - started };
};

type Owner = Awaited<ReturnType<typeof openOwner>>;

const childState = async ({ service }: Owner) => {
  const diagnostics = await service.diagnostics();
  return {
    cachedNodes: diagnostics.cachedNodes,
    cachedMiB: Math.round(diagnostics.cachedBytes / 2 ** 20),
    rssMiB: Math.round(diagnostics.rssBytes / 2 ** 20),
    peakRssMiB: Math.round(diagnostics.peakRssBytes / 2 ** 20),
  };
};

const runBlock = async (
  owner: Owner,
  label: string,
  ops: readonly NativeMpfEventOp[],
) => {
  const { service, reads } = owner;
  const base = (await service.diagnostics()).durableRoot;
  reads.calls = 0;
  reads.nodes = 0;
  const started = performance.now();
  const handle = await service.fork(base);
  const applied = await service.applyEvents(
    handle,
    encodeNativeMpfEventLog(base, events(ops)),
  );
  const appliedAt = performance.now();
  const { calls, nodes } = reads;
  await service.promote(handle);
  const promotedAt = performance.now();
  return {
    block: label,
    ops: ops.length,
    applyMs: Math.round(appliedAt - started),
    childSelectMs: Math.round(applied.proofArenaDurationNs / 1e6),
    childMutateMs: Math.round(applied.mutationDurationNs / 1e6),
    nodeReadRounds: calls,
    nodesRead: nodes,
    promoteMs: Math.round(promotedAt - appliedAt),
    ...(await childState(owner)),
  };
};

describe.skipIf(directory === undefined)("native owner at ledger scale", () => {
  const levelPath = join(directory ?? "", "ledger");

  it.skipIf(mode !== "build")(
    "builds the ledger",
    async () => {
      await prepareEventFlatDigest();
      mkdirSync(directory!, { recursive: true });
      await seedOwnerLedger(levelPath, SDK.EMPTY_MERKLE_TREE_ROOT);
      const owner = await openOwner(levelPath);
      const started = performance.now();
      for (let from = 0; from < utxos; from += BUILD_BLOCK_OPS) {
        const count = Math.min(BUILD_BLOCK_OPS, utxos - from);
        const row = await runBlock(
          owner,
          `build ${from.toString()}`,
          Array.from({ length: count }, (_, offset) => insert(from + offset)),
        );
        console.log(JSON.stringify(row));
      }
      console.log(
        JSON.stringify({ builtMs: Math.round(performance.now() - started) }),
      );
      await owner.service.close();
    },
    7_200_000,
  );

  it.skipIf(mode !== "measure")(
    "measures start and blocks",
    async () => {
      await prepareEventFlatDigest();
      const next = random(utxos);
      const live = Array.from({ length: utxos }, (_, index) => index);
      let created = utxos;
      /** Half spends of live outputs, half new outputs: the ledger stays its size. */
      const mixed = (ops: number): NativeMpfEventOp[] => {
        const block: NativeMpfEventOp[] = [];
        const added: number[] = [];
        for (let event = 0; event < ops / OPS_PER_EVENT; event += 1) {
          for (let spend = 0; spend < OPS_PER_EVENT / 2; spend += 1) {
            const at = Math.floor(next() * live.length);
            block.push(remove(live[at]!));
            live[at] = live[live.length - 1]!;
            live.pop();
          }
          for (let output = 0; output < OPS_PER_EVENT / 2; output += 1) {
            block.push(insert(created));
            added.push(created);
            created += 1;
          }
        }
        live.push(...added);
        return block;
      };
      const rows: unknown[] = [];
      const record = (row: Record<string, unknown>) => {
        rows.push(row);
        console.log(JSON.stringify(row));
      };
      const blocks = async (owner: Owner, label: string, shape: number[]) => {
        for (const ops of shape)
          record(
            await runBlock(owner, `${label} ${ops.toString()}`, mixed(ops)),
          );
      };

      let owner = await openOwner(levelPath);
      record({ start: "first", startMs: Math.round(owner.startMs) });
      record({ after: "start", ...(await childState(owner)) });
      await blocks(owner, "after start", [10_000, 10_000]);

      // Reads every live key's path without changing the ledger, so the
      // blocks after it find their paths in the child's cache.
      const warmStarted = performance.now();
      for (let from = 0; from < live.length; from += BUILD_BLOCK_OPS) {
        const base = (await owner.service.diagnostics()).durableRoot;
        const handle = await owner.service.fork(base);
        await owner.service.applyEvents(
          handle,
          encodeNativeMpfEventLog(base, [
            live.slice(from, from + BUILD_BLOCK_OPS).map(remove),
          ]),
        );
        await owner.service.discard(handle);
      }
      const whole = await childState(owner);
      record({
        after: "every path read",
        ms: Math.round(performance.now() - warmStarted),
        ...whole,
      });
      await blocks(owner, "paths held", [10_000, 10_000, 40_000, 40_000]);
      await owner.service.close();

      owner = await openOwner(levelPath);
      record({ start: "second", startMs: Math.round(owner.startMs) });
      await blocks(owner, "after start", [40_000, 40_000]);
      await owner.service.close();

      const nodeCacheBytes = Math.floor((whole.cachedMiB * 2 ** 20) / 10);
      owner = await openOwner(levelPath, { nodeCacheBytes });
      record({ start: "tenth of the trie", nodeCacheBytes });
      await blocks(owner, "tenth", [10_000, 10_000, 40_000, 40_000]);
      await owner.service.close();

      console.table(rows);
      writeBenchmarkJson(join(directory!, "report.json"), { utxos, rows });
      expect(rows.length).toBeGreaterThan(0);
    },
    7_200_000,
  );
});
