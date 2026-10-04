import * as SDK from "@al-ft/midgard-sdk";
import { Effect, Ref } from "effect";
import { describe, expect, it } from "vitest";

import { ForeignTipReconciliationsDB } from "../src/database/index.js";
import {
  FOREIGN_DA_RETRIEVAL_SOURCE,
  FOREIGN_DA_RETRIEVAL_UNAVAILABLE,
  foreignDaRowAction,
  retrieveFromPage,
  superviseForeignDaRetrieval,
} from "../src/fibers/foreign-da-reconciliation.js";
import { Globals } from "../src/services/globals.js";

const { Columns } = ForeignTipReconciliationsDB;
const empty = SDK.EMPTY_MERKLE_TREE_ROOT;
const entry = (blockingReason: string | null, depositCount = 0n) =>
  ({
    [Columns.BLOCKING_REASON]: blockingReason,
    [Columns.DEPOSITS_ROOT]: empty,
    [Columns.DEPOSIT_COUNT]: depositCount,
    [Columns.FORCED_TRANSACTIONS_ROOT]: empty,
    [Columns.FORCED_TRANSACTION_COUNT]: 0n,
    [Columns.WITHDRAWALS_ROOT]: empty,
    [Columns.WITHDRAWAL_COUNT]: 0n,
  }) as unknown as ForeignTipReconciliationsDB.Entry;

describe("foreign DA retrieval supervision", () => {
  it("never fetches a malformed header and leaves a payload already judged invalid alone", () => {
    for (const stored of [true, false])
      expect(foreignDaRowAction(entry(null, 1n), stored)).toBe("skip");
    // Its stored bytes replay the same verdict, and no download replaces them.
    expect(foreignDaRowAction(entry("invalid:payload_mismatch"), true)).toBe(
      "skip",
    );
    expect(foreignDaRowAction(entry("invalid:payload_mismatch"), false)).toBe(
      "fetch",
    );
    expect(foreignDaRowAction(entry("replay_required"), true)).toBe(
      "consume_stored",
    );
    expect(foreignDaRowAction(entry(null), false)).toBe("fetch");
  });

  it("passes over a row whose payload store refuses an overwrite, and stops at the first consumed row", async () => {
    const [refused, consumed, unvisited] = ["0a", "0b", "0c"].map((byte) =>
      Buffer.from(byte.repeat(32), "hex"),
    );
    const visited: Buffer[] = [];
    const cursor = await Effect.runPromise(
      retrieveFromPage([refused!, consumed!, unvisited!], (hash) =>
        Effect.gen(function* () {
          visited.push(hash);
          if (hash === refused)
            return yield* Effect.fail(
              new Error(
                "Refusing to overwrite DA payload because an existing payload for the header differs",
              ),
            );
          return hash === consumed;
        }),
      ),
    );
    expect(visited).toEqual([refused, consumed]);
    expect(cursor).toEqual(consumed);
  });

  it("retries a failing start without ending the node, holding a readiness reason until a start succeeds", async () => {
    await Effect.runPromise(
      Effect.gen(function* () {
        const globals = yield* Globals;
        const reason = Effect.map(
          Ref.get(globals.LIVENESS_REASONS),
          (reasons) => reasons.get(FOREIGN_DA_RETRIEVAL_SOURCE),
        );
        const seen: (string | undefined)[] = [];
        let attempts = 0;
        yield* superviseForeignDaRetrieval(
          globals,
          (started) =>
            Effect.gen(function* () {
              attempts += 1;
              seen.push(yield* reason);
              if (attempts === 1) return yield* Effect.fail("manifest missing");
              if (attempts === 2) return yield* Effect.die("transport crashed");
              yield* started;
              seen.push(yield* reason);
            }),
          { firstDelayMs: 1, maxDelayMs: 2 },
        );
        expect(attempts).toBe(3);
        expect(seen).toEqual([
          undefined,
          FOREIGN_DA_RETRIEVAL_UNAVAILABLE,
          FOREIGN_DA_RETRIEVAL_UNAVAILABLE,
          undefined,
        ]);
      }).pipe(Effect.provide(Globals.Default)),
    );
  });
});
