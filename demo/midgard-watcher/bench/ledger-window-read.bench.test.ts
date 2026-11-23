import { randomBytes } from "node:crypto";
import { mkdtemp, rm } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";
import { performance } from "node:perf_hooks";

import type { SqlValue } from "@al-ft/midgard-l1-follower";
import {
  SIM_ORIGIN,
  SimChain,
  simUniverse,
} from "@al-ft/midgard-l1-follower/testing";
import * as SDK from "@al-ft/midgard-sdk/core";
import { Data } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import { createWatcherLedgerWindowReader } from "../src/l1-follower/replay-source.window.js";
import { WATCHER_QUEUE_OUTPUTS_TABLE } from "../src/l1-follower/tables.js";
import {
  commitTx,
  initTx,
  queueState,
} from "../tests/support/l1-follower-state-queue-traffic.js";
import {
  applyAll,
  D,
  openStore,
} from "../tests/support/l1-follower-store-reset.js";

/**
 * The ledger window read once the follower holds node rows from the origin.
 * Every block has three node rows: created at its commit, re-output by its
 * attestation, relinked by the next commit and closed by its merge thirty
 * blocks later. The window covers the newest hundred blocks.
 *
 * Target: the warm median call at 10^5 blocks stays within
 * `MAX_GROWTH` times the warm median at 10^3 blocks; a read that scans every
 * node row grows about two hundred times over that range.
 *
 * Run from demo/: `pnpm --filter midgard-watcher bench`.
 */
const SMALL = 1_000;
const LARGE = 100_000;
const MAX_GROWTH = 3;
const WARM_CALLS = 21;
const BLOCK_SLOTS = 20;
const WINDOW_BLOCKS = 100;
const MATURITY_SLOTS = 30 * BLOCK_SLOTS;
const SLOT_CONFIG = { zeroTime: 0, zeroSlot: 0, slotLength: 1000 };

/** A node row as the projection writes it, to copy the other columns from. */
const templateRow = async () => {
  const store = openStore(":memory:");
  try {
    await store.start();
    await store.initialize(SIM_ORIGIN);
    const chain = new SimChain(simUniverse(), SIM_ORIGIN);
    const forward = (txs: Parameters<SimChain["forward"]>[0]) =>
      applyAll(store, [chain.forward(txs).event]);
    await forward([initTx(D)]);
    await forward([commitTx(queueState(chain, D)!, D)]);
    const [row] = await store.transaction("read", (tx) =>
      tx.query(
        `SELECT * FROM ${WATCHER_QUEUE_OUTPUTS_TABLE} WHERE kind = 'node'`,
      ),
    );
    return row!;
  } finally {
    await store.close();
  }
};

/** The warm median call and the window it reads, over `blocks` blocks. */
const warmMedianAt = async (
  blocks: number,
  template: Record<string, unknown>,
) => {
  const directory = await mkdtemp(join(tmpdir(), "watcher-ledger-window-"));
  const store = openStore(join(directory, "follower.sqlite"));
  try {
    await store.start();
    await store.initialize(SIM_ORIGIN);
    const templateNode = Data.from(
      Buffer.from(template.state_queue_node_cbor as Uint8Array).toString("hex"),
      SDK.StateQueueNode,
    );
    const columns = Object.keys(template);
    const insert = `INSERT INTO ${WATCHER_QUEUE_OUTPUTS_TABLE} (${columns.join(", ")}) VALUES (${columns.map(() => "?").join(", ")})`;
    const tipSlot = blocks * BLOCK_SLOTS;
    await store.transaction("write", async (tx) => {
      for (let i = 0; i < blocks; i += 1) {
        const node: SDK.StateQueueNode = {
          ...templateNode,
          header: {
            ...templateNode.header,
            utxosRoot: randomBytes(32).toString("hex"),
          },
          block_time: BigInt(i * BLOCK_SLOTS * SLOT_CONFIG.slotLength),
        };
        const headerHash = randomBytes(28);
        const commit = i * BLOCK_SLOTS;
        const tail = i === blocks - 1;
        const merge = commit + MATURITY_SLOTS;
        const spans: (readonly [number, number | null])[] = [
          [commit, commit + 1],
          [commit + 1, tail ? null : commit + BLOCK_SLOTS],
          ...(tail
            ? []
            : [
                [
                  commit + BLOCK_SLOTS,
                  merge <= tipSlot ? merge : null,
                ] as const,
              ]),
        ];
        for (const [index, [from, to]] of spans.entries()) {
          const values: Record<string, unknown> = {
            ...template,
            tx_hash: randomBytes(32),
            output_index: index,
            header_hash: headerHash,
            next_header_hash: tail ? null : randomBytes(28),
            state_queue_node_cbor: Buffer.from(
              Data.to(node, SDK.StateQueueNode),
              "hex",
            ),
            from_slot: from,
            to_slot: to,
          };
          await tx.query(
            insert,
            // A row read back from the table holds only SQL values.
            columns.map((column) => values[column]) as SqlValue[],
          );
        }
      }
      await tx.query("UPDATE l1_follower_cursor SET slot = ?", [tipSlot]);
    });
    const read = createWatcherLedgerWindowReader({
      store,
      slotConfig: SLOT_CONFIG,
      ledgerRetentionMs: WINDOW_BLOCKS * BLOCK_SLOTS * SLOT_CONFIG.slotLength,
    });
    const window = await read();
    const samples: number[] = [];
    for (let call = 0; call < WARM_CALLS; call += 1) {
      const started = performance.now();
      await read();
      samples.push(performance.now() - started);
    }
    samples.sort((left, right) => left - right);
    return { medianMs: samples[(WARM_CALLS - 1) / 2]!, window };
  } finally {
    await store.close();
    await rm(directory, { recursive: true, force: true });
  }
};

describe("watcher ledger window read", () => {
  it(`keeps the warm call at ${LARGE.toString()} blocks within ${MAX_GROWTH.toString()}x the call at ${SMALL.toString()}`, async () => {
    const template = await templateRow();
    const small = await warmMedianAt(SMALL, template);
    const large = await warmMedianAt(LARGE, template);
    // tip < block_time + retention holds for the newest 99 blocks.
    expect(small.window.blocks).toHaveLength(WINDOW_BLOCKS - 1);
    expect(large.window.blocks).toHaveLength(WINDOW_BLOCKS - 1);
    expect(large.window.newestRoot).toBeDefined();
    const report = JSON.stringify({
      smallMs: small.medianMs,
      largeMs: large.medianMs,
    });
    expect(large.medianMs, report).toBeLessThanOrEqual(
      MAX_GROWTH * small.medianMs,
    );
  });
});
