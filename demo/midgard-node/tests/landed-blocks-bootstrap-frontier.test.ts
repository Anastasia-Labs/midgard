/**
 * A fresh node's first landed-block run anchors `confirmed_ledger`, on the
 * node database with stub ports: at genesis it writes the configured
 * genesis ledger and sets the frontier at the queue's genesis root; at a
 * merged root whose ledger is empty it sets the frontier at that root and
 * writes no row; at a merged root whose retained lineage passes a header
 * whose ledger is empty it sets the frontier at that header and writes no
 * row. None of these reads `confirmed_ledger` in full
 * (`confirmed_ledger_full_scan_total`).
 */
import * as SDK from "@al-ft/midgard-sdk/core";
import { SqlClient } from "@effect/sql";
import { Effect, Metric } from "effect";
import { describe, expect, it } from "vitest";

import { fullScanCounter } from "../src/database/confirmedLedger.js";
import type { LandedStateQueueElement } from "../src/l1-state-queue/index.js";
import { processLandedQueue } from "../src/landed-blocks/process.js";
import { Frontier } from "../src/landed-blocks/store.js";
import {
  AFTER_F,
  block,
  confirmedAt,
  fixture,
  GENESIS,
  harness,
  hex,
  inNode,
  ports,
  queueOf,
  rootOf,
  sortedKeys,
} from "./landed-blocks-own.fixture.js";

const fullScans = Metric.value(fullScanCounter).pipe(
  Effect.map(({ count }) => count),
);

/** A queue output as the history retains it, created at `slot`. */
const createdAt = (
  element: LandedStateQueueElement,
  slot: number,
): LandedStateQueueElement => ({ ...element, created: { slot, txIndex: 0 } });

/** `confirmed_ledger` as `outref=output` keys, read with plain SQL (a
 * `ConfirmedLedgerDB.retrieve` would count as a full read). */
const confirmedRows = Effect.gen(function* () {
  const sql = yield* SqlClient.SqlClient;
  const rows = yield* sql<{
    outref: Buffer;
    output: Buffer;
  }>`SELECT outref, output FROM confirmed_ledger`;
  return rows.map((row) => `${hex(row.outref)}=${hex(row.output)}`).sort();
});

describe("a fresh node's confirmed-ledger frontier", () => {
  it("writes the configured genesis ledger and sets the frontier at genesis, reading confirmed_ledger in full zero times", async () => {
    const { genesisRoot, genesisState } = await fixture();
    const state = harness();
    await inNode(
      Effect.gen(function* () {
        const scans = yield* fullScans;
        const hold = yield* processLandedQueue(
          ports(state),
          queueOf(genesisState, []),
        );
        expect((yield* fullScans) - scans).toBe(0);
        expect(hold).toBeUndefined();
        expect(yield* Frontier.retrieve).toEqual({
          headerHash: SDK.GENESIS_HEADER_HASH,
          utxosRoot: genesisRoot,
        });
        expect(yield* confirmedRows).toEqual(sortedKeys(GENESIS));
      }),
    );
  });

  it("sets the frontier at a merged root whose ledger is empty and writes no row, reading confirmed_ledger in full zero times", async () => {
    const state = harness();
    await inNode(
      Effect.gen(function* () {
        const scans = yield* fullScans;
        const hold = yield* processLandedQueue(
          ports(state),
          queueOf(confirmedAt("genesis", SDK.EMPTY_MERKLE_TREE_ROOT), []),
        );
        expect((yield* fullScans) - scans).toBe(0);
        expect(hold).toBeUndefined();
        expect(yield* Frontier.retrieve).toEqual({
          headerHash: SDK.GENESIS_HEADER_HASH,
          utxosRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
        });
        expect(yield* confirmedRows).toEqual([]);
      }),
    );
  });

  it("sets the frontier at the latest header on the merged root's lineage whose ledger is empty and writes no row, reading confirmed_ledger in full zero times", async () => {
    const { genesisRoot, genesisState } = await fixture();
    const empty = SDK.EMPTY_MERKLE_TREE_ROOT;
    // genesis -> e (every output spent) -> b, merged at b.
    const e = block(
      11,
      { hash: SDK.GENESIS_HEADER_HASH, root: genesisRoot, endTime: 0n },
      empty,
    );
    const b = block(
      12,
      { hash: e.hash, root: empty, endTime: e.header.endTime },
      await rootOf(AFTER_F),
    );
    const landed = queueOf(genesisState, [e, b]);
    const merged = queueOf(confirmedAt(b, ""), []);
    const state = harness();
    state.history = [
      createdAt(landed.root!, 1),
      createdAt(landed.nodes[0]!, 2),
      createdAt(landed.nodes[1]!, 3),
      createdAt(merged.root!, 4),
    ];
    // Processing after the frontier stops at `b`'s payload.
    state.replayEnds = Effect.succeed({
      kind: "missing",
      detail: "DA payload not available yet",
    } as const);
    await inNode(
      Effect.gen(function* () {
        const scans = yield* fullScans;
        yield* processLandedQueue(ports(state), merged);
        expect((yield* fullScans) - scans).toBe(0);
        expect(yield* Frontier.retrieve).toEqual({
          headerHash: e.hash,
          utxosRoot: empty,
        });
        expect(yield* confirmedRows).toEqual([]);
      }),
    );
  });
});
