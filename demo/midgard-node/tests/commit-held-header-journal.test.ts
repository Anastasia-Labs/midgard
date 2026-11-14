/**
 * The journal insert over a dead journal with the same header hash. A
 * replacement built from identical content on the same base in the same
 * scheduler window has the dead commit's header hash.
 *
 * - Signed (`intended_tx_hash` set): its signed bytes can still land, and
 *   its block is then taken in like any other, so the insert keeps it,
 *   writes nothing and returns `held`. The next window's header (another
 *   block end time) is prepared beside it.
 * - Unsigned: nothing can land from it, so the insert replaces it.
 */

import { SqlClient } from "@effect/sql";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import { PendingBlockFinalizationsDB } from "../src/database/index.js";
import { prepareUnderTestLease } from "./helpers/mutation-lease.js";
import { insertCommitIntent } from "./helpers/own-block-facts.js";
import {
  header,
  isolatedDb,
  journalFixture,
} from "./helpers/own-journal-fixture.js";

const SIGNED_TX_HASH = Buffer.alloc(32, 9);

const run = <A, E, R>(effect: Effect.Effect<A, E, R>) =>
  Effect.runPromise(isolatedDb(effect) as Effect.Effect<A, unknown, never>);

/**
 * A dead journal for `headerHash` (it has no commit anchor), signed when
 * `signed` is set.
 */
const deadJournal = (headerHash: Buffer, signed: boolean) =>
  Effect.gen(function* () {
    yield* prepareUnderTestLease({
      ...journalFixture(headerHash),
      preparedTxHash: SIGNED_TX_HASH,
    });
    if (!signed) return;
    const sql = yield* SqlClient.SqlClient;
    yield* insertCommitIntent(
      headerHash,
      SIGNED_TX_HASH,
      Buffer.from("84a0a0f5f6", "hex"),
    );
    yield* sql`UPDATE pending_block_finalizations
      SET intended_tx_hash = ${SIGNED_TX_HASH}
      WHERE header_hash = ${headerHash}`;
  });

const rows = Effect.gen(function* () {
  const sql = yield* SqlClient.SqlClient;
  const found = yield* sql<{
    header_hash: Buffer;
    intended_tx_hash: Buffer | null;
  }>`SELECT header_hash, intended_tx_hash
    FROM pending_block_finalizations ORDER BY created_at, header_hash`;
  const dead = yield* PendingBlockFinalizationsDB.deadJournals;
  return found.map((row) => ({
    header: row.header_hash.toString("hex"),
    intended: row.intended_tx_hash?.toString("hex") ?? null,
    dead: dead.causes.has(row.header_hash.toString("hex")),
  }));
});

describe("journal insert over a dead journal with the same header", () => {
  it("keeps a signed dead journal and holds its header; the next window's header is prepared", async () => {
    const result = await run(
      Effect.gen(function* () {
        const sameWindow = header("same-window");
        yield* deadJournal(sameWindow, true);
        const before = yield* rows;
        const held = yield* prepareUnderTestLease(journalFixture(sameWindow));
        const afterHeld = yield* rows;
        const nextWindow = header("next-window");
        const prepared = yield* prepareUnderTestLease(
          journalFixture(nextWindow),
        );
        return {
          before,
          held,
          afterHeld,
          prepared,
          after: yield* rows,
          sameWindow: sameWindow.toString("hex"),
          nextWindow: nextWindow.toString("hex"),
        };
      }),
    );

    expect(result.held).toEqual({
      kind: "held",
      heldHeaderHash: Buffer.from(result.sameWindow, "hex"),
    });
    expect(result.before).toEqual([
      {
        header: result.sameWindow,
        intended: SIGNED_TX_HASH.toString("hex"),
        dead: true,
      },
    ]);
    expect(result.afterHeld).toEqual(result.before);
    expect(result.prepared).toEqual({ kind: "prepared" });
    expect(result.after).toEqual([
      ...result.before,
      { header: result.nextWindow, intended: null, dead: true },
    ]);
  });

  it("replaces an unsigned dead journal with the same header", async () => {
    const result = await run(
      Effect.gen(function* () {
        const sameWindow = header("same-window");
        yield* deadJournal(sameWindow, false);
        const before = yield* rows;
        const prepared = yield* prepareUnderTestLease(
          journalFixture(sameWindow),
        );
        return { before, prepared, after: yield* rows };
      }),
    );

    expect(result.before).toEqual([
      expect.objectContaining({ intended: null, dead: true }),
    ]);
    expect(result.prepared).toEqual({ kind: "prepared" });
    expect(result.after).toEqual([
      { header: result.before[0]!.header, intended: null, dead: true },
    ]);
  });
});
