/**
 * The own-block journal keeps a block's time (its state-queue node's
 * `block_time`) beside its end: a block committed after its end stores both,
 * and a block time before the end is refused.
 */
import { SqlClient } from "@effect/sql";
import { it } from "@effect/vitest";
import { Cause, Effect, Option } from "effect";
import { describe, expect } from "vitest";

import * as PendingBlockFinalizationsDB from "../src/database/pendingBlockFinalizations.js";
import type { DatabaseError } from "../src/database/utils/common.js";
import {
  header,
  isolatedDb,
  journalFixture,
} from "./local-mutation-job-abandonment.journal-fixture.js";

describe("pending journal block time", () => {
  it.effect("stores a block time after the block end", () =>
    isolatedDb(
      Effect.gen(function* () {
        const headerHash = header("catch-up");
        const input = journalFixture(headerHash);
        const blockTime = new Date(input.blockEndTime.getTime() + 3_600_000);
        yield* PendingBlockFinalizationsDB.preparePendingSubmission({
          ...input,
          blockTime,
        });
        const sql = yield* SqlClient.SqlClient;
        const [row] = yield* sql<{ block_end_time: Date; block_time: Date }>`
          SELECT block_end_time, block_time
          FROM ${sql(PendingBlockFinalizationsDB.tableName)}
          WHERE header_hash = ${headerHash}`;
        expect(row!.block_end_time.getTime()).toBe(
          input.blockEndTime.getTime(),
        );
        expect(row!.block_time.getTime()).toBe(blockTime.getTime());
      }),
    ),
  );

  it.effect("refuses a block time before the block end", () =>
    isolatedDb(
      Effect.gen(function* () {
        const input = journalFixture(header("early-block-time"));
        const exit = yield* Effect.exit(
          PendingBlockFinalizationsDB.preparePendingSubmission({
            ...input,
            blockTime: new Date(input.blockEndTime.getTime() - 1),
          }),
        );
        expect(exit._tag).toBe("Failure");
        if (exit._tag !== "Failure") return;
        const failure = Option.getOrThrow(Cause.failureOption(exit.cause));
        expect(String((failure as DatabaseError).cause)).toContain(
          "block time must not precede the block end",
        );
      }),
    ),
  );
});
