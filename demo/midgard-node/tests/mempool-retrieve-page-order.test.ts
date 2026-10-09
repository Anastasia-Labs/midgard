/**
 * `retrievePage` (`src/database/mempool.ts`) reads pending mempool rows in
 * admission order: within one time stamp (one accepted batch) a parent
 * admitted before its child comes first whatever their tx ids, so a page cut
 * between them holds the parent, not the child alone. A row with no
 * admission comes after those with one.
 */
import "./utils.js";

import { SqlClient } from "@effect/sql";
import { it } from "@effect/vitest";
import { Effect } from "effect";
import { beforeAll, describe, expect } from "vitest";

import { MempoolDB, MigrationRunner } from "../src/database/index.js";
import type * as Tx from "../src/database/utils/tx.js";
import { provideDatabaseLayers, resetApplicationTables } from "./utils.js";

const isolatedDb = <A, E, R>(effect: Effect.Effect<A, E, R>) =>
  provideDatabaseLayers(
    Effect.gen(function* () {
      yield* resetApplicationTables;
      return yield* effect;
    }),
  );

beforeAll(async () => {
  await Effect.runPromise(
    provideDatabaseLayers(
      MigrationRunner.migrate({
        appVersion: "mempool-retrieve-page-order-test",
        actor: "mempool-retrieve-page-order-test",
      }),
    ),
  );
});

const BATCH = new Date("2026-10-09T12:00:00.000Z");
// The child's tx id sorts before its parent's.
const PARENT = Buffer.alloc(32, 0xf0);
const CHILD = Buffer.alloc(32, 0x10);
const UNADMITTED = Buffer.alloc(32, 0x01);

const seed = Effect.gen(function* () {
  const sql = yield* SqlClient.SqlClient;
  yield* sql`INSERT INTO mempool ${sql.insert(
    [PARENT, CHILD, UNADMITTED].map((tx_id) => ({
      tx_id,
      tx: Buffer.concat([tx_id, Buffer.from("tx")]),
      time_stamp_tz: BATCH,
    })),
  )}`;
  yield* sql`INSERT INTO tx_admissions ${sql.insert(
    [
      { tx_id: PARENT, arrival_seq: 10 },
      { tx_id: CHILD, arrival_seq: 20 },
    ].map((row) => ({
      ...row,
      status: "accepted",
      terminal_at: BATCH,
      submit_source: "native",
    })),
  )}`;
});

const ids = (entries: readonly Tx.EntryWithTimeStamp[]) =>
  entries.map((entry) => entry.tx_id.toString("hex"));

describe("the mempool page", () => {
  it.effect(
    "reads a batch in admission order, so a cut page holds a parent and not its child alone",
    () =>
      isolatedDb(
        Effect.gen(function* () {
          yield* seed;
          const first = yield* MempoolDB.retrievePage({ limit: 1 });
          expect(ids(first.entries)).toEqual([PARENT.toString("hex")]);

          const walked: Tx.EntryWithTimeStamp[] = [];
          let after: MempoolDB.MempoolCursor | undefined;
          do {
            const page = yield* MempoolDB.retrievePage({ after, limit: 1 });
            walked.push(...page.entries);
            after = page.nextCursor ?? undefined;
          } while (after !== undefined);
          expect(ids(walked)).toEqual(
            [PARENT, CHILD, UNADMITTED].map((id) => id.toString("hex")),
          );
          // Each entry is the row's own columns, nothing of the ordering.
          expect(Object.keys(walked[0]!).sort()).toEqual(
            ["time_stamp_tz", "tx", "tx_id"].sort(),
          );

          const two = yield* MempoolDB.retrievePage({ limit: 2 });
          expect(ids(two.entries)).toEqual(
            [PARENT, CHILD].map((id) => id.toString("hex")),
          );
        }),
      ),
  );
});
