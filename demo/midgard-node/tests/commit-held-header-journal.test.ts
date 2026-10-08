/**
 * The journal insert over an abandoned journal with the same header hash.
 * A replacement built from identical content on the same base in the same
 * scheduler window has the abandoned commit's header hash.
 *
 * - Signed abandoned (`intended_tx_hash` or `submitted_tx_hash` set): its
 *   signed bytes can still land and be revived from that journal, so the
 *   insert keeps it, writes nothing and returns `held`. The next window's
 *   header (another block end time) is prepared beside it.
 * - Unsigned abandoned: nothing can land from it, so the insert replaces it
 *   as before.
 */
import "./utils.js";

import { SqlClient } from "@effect/sql";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import * as PendingBlockFinalizationsDB from "../src/database/pendingBlockFinalizations.js";
import {
  header,
  isolatedDb,
  journalFixture,
  Status,
} from "./local-mutation-job-abandonment.journal-fixture.js";

const SIGNED_TX_HASH = Buffer.alloc(32, 9);

const run = <A, E, R>(effect: Effect.Effect<A, E, R>) =>
  Effect.runPromise(isolatedDb(effect) as Effect.Effect<A, unknown, never>);

/** An abandoned journal for `headerHash`, signed when `signed` is given. */
const abandonedJournal = (
  headerHash: Buffer,
  signed?: "intended" | "submitted",
) =>
  Effect.gen(function* () {
    yield* PendingBlockFinalizationsDB.preparePendingSubmission({
      ...journalFixture(headerHash),
      preparedTxHash: SIGNED_TX_HASH,
    });
    const sql = yield* SqlClient.SqlClient;
    if (signed === "intended")
      yield* sql`UPDATE pending_block_finalizations
        SET intended_tx_hash = ${SIGNED_TX_HASH},
          signed_tx_cbor = ${Buffer.from("84a0a0f5f6", "hex")}
        WHERE header_hash = ${headerHash}`;
    if (signed === "submitted")
      yield* sql`UPDATE pending_block_finalizations
        SET submitted_tx_hash = ${SIGNED_TX_HASH}
        WHERE header_hash = ${headerHash}`;
    yield* sql`UPDATE pending_block_finalizations
      SET status = ${Status.Abandoned}, updated_at = NOW()
      WHERE header_hash = ${headerHash}`;
  });

const rows = Effect.gen(function* () {
  const sql = yield* SqlClient.SqlClient;
  const found = yield* sql<{
    header_hash: Buffer;
    status: string;
    intended_tx_hash: Buffer | null;
    submitted_tx_hash: Buffer | null;
  }>`SELECT header_hash, status, intended_tx_hash, submitted_tx_hash
    FROM pending_block_finalizations ORDER BY created_at, header_hash`;
  return found.map((row) => ({
    header: row.header_hash.toString("hex"),
    status: row.status,
    intended: row.intended_tx_hash?.toString("hex") ?? null,
    submitted: row.submitted_tx_hash?.toString("hex") ?? null,
  }));
});

describe("journal insert over an abandoned journal with the same header", () => {
  it.each(["intended", "submitted"] as const)(
    "keeps a %s-signed abandoned journal and holds its header; the next window's header is prepared",
    async (signed) => {
      const result = await run(
        Effect.gen(function* () {
          const sameWindow = header("same-window");
          yield* abandonedJournal(sameWindow, signed);
          const before = yield* rows;
          const held =
            yield* PendingBlockFinalizationsDB.preparePendingSubmission(
              journalFixture(sameWindow),
            );
          const afterHeld = yield* rows;
          const nextWindow = header("next-window");
          const prepared =
            yield* PendingBlockFinalizationsDB.preparePendingSubmission(
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
      expect(result.before).toHaveLength(1);
      expect(result.before[0]).toMatchObject({
        header: result.sameWindow,
        status: Status.Abandoned,
        [signed]: SIGNED_TX_HASH.toString("hex"),
      });
      expect(result.afterHeld).toEqual(result.before);
      expect(result.prepared).toEqual({ kind: "prepared" });
      expect(result.after).toEqual([
        ...result.before,
        {
          header: result.nextWindow,
          status: Status.PendingSubmission,
          intended: null,
          submitted: null,
        },
      ]);
    },
  );

  it("replaces an unsigned abandoned journal with the same header", async () => {
    const result = await run(
      Effect.gen(function* () {
        const sameWindow = header("same-window");
        yield* abandonedJournal(sameWindow);
        const before = yield* rows;
        const prepared =
          yield* PendingBlockFinalizationsDB.preparePendingSubmission(
            journalFixture(sameWindow),
          );
        return { before, prepared, after: yield* rows };
      }),
    );

    expect(result.before).toEqual([
      expect.objectContaining({ status: Status.Abandoned, intended: null }),
    ]);
    expect(result.prepared).toEqual({ kind: "prepared" });
    expect(result.after).toEqual([
      {
        header: result.before[0]!.header,
        status: Status.PendingSubmission,
        intended: null,
        submitted: null,
      },
    ]);
  });
});
