/**
 * A full verification run on the node database (`verifyConfirmedLedger`,
 * its roots derived in its worker thread) reads `confirmed_ledger` a page
 * at a time in trie order (`feedConfirmedLedgerLeaves`), on the snapshot
 * it reads the frontier and the landed rows on. Over a ledger of several
 * pages, with a processed row after the frontier that spends entries on two
 * pages and produces one, it passes with the roots the trie derives,
 * counts one full read of `confirmed_ledger` per run, and runs again in the
 * same way (its temporary table is gone with each run); an entry changed
 * outside a fold on the last page, which the row does not spend, diverges
 * at the frontier and the tail. The run at 300,000 entries is
 * `confirmed-ledger-verification.paged-at-size.bench.ts`.
 */
import { SqlClient } from "@effect/sql";
import { Effect, Metric } from "effect";
import { describe, expect, it } from "vitest";

import { ConfirmedLedgerDB } from "../src/database/index.js";
import { CONFIRMED_LEDGER_ENTRY_PAGE } from "../src/landed-blocks/ledger-leaves.js";
import { verifyConfirmedLedger } from "../src/mpf/confirmed-ledger-run.js";
import { withFollowerWrite } from "../src/services/follower-write-gate.js";
import { FRONTIER_HASH, run } from "./commit-base-resolver.fixture.js";
import {
  seedPaged,
  TAIL_HASH,
} from "./confirmed-ledger-verification.paged.fixture.js";
import { simOutput } from "./helpers/landed-blocks-sim.universe.js";

const fullReads = Metric.value(ConfirmedLedgerDB.fullScanCounter).pipe(
  Effect.map((state) => state.count),
);

describe("a full verification run, paged", { concurrent: false }, () => {
  it("passes over several pages with a processed row after the frontier, one full read per run", async () => {
    const count = 2 * CONFIRMED_LEDGER_ENTRY_PAGE + 123;
    const { r0, r1 } = await seedPaged(count);
    for (let pass = 0; pass < 2; pass += 1) {
      const before = await run(fullReads);
      const outcome = await run(verifyConfirmedLedger());
      expect(outcome).toMatchObject({
        kind: "verified",
        frontier: { headerHash: FRONTIER_HASH, utxosRoot: r0 },
        tail: { headerHash: TAIL_HASH, utxosRoot: r1 },
        entries: count,
      });
      expect((await run(fullReads)) - before).toBe(1);
    }
  }, 300_000);

  it("diverges at the frontier and the tail for an entry changed outside a fold on the last page", async () => {
    const count = 2 * CONFIRMED_LEDGER_ENTRY_PAGE + 123;
    const { confirmed } = await seedPaged(count);
    const changed = confirmed[count - 2]!;
    await run(
      withFollowerWrite(
        Effect.flatMap(
          SqlClient.SqlClient,
          (sql) => sql`UPDATE confirmed_ledger
            SET output = ${simOutput(9_000_000n)}
            WHERE outref = ${changed.outref}`,
        ),
      ),
    );
    const outcome = await run(verifyConfirmedLedger());
    expect(outcome).toMatchObject({
      kind: "diverged",
      divergences: [
        { check: "confirmed_ledger", headerHash: FRONTIER_HASH },
        { check: "confirmed_ledger_tail", headerHash: TAIL_HASH },
      ],
    });
  }, 300_000);
});
