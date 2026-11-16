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
 * at the frontier and the tail. At 300,000
 * entries, under the worker's fixed heap limit, it passes.
 */
import { SqlClient } from "@effect/sql";
import { Effect, Metric } from "effect";
import { describe, expect, it } from "vitest";

import { ConfirmedLedgerDB } from "../src/database/index.js";
import type * as Ledger from "../src/database/utils/ledger.js";
import { ledgerRows } from "../src/landed-blocks/ledger.js";
import { CONFIRMED_LEDGER_ENTRY_PAGE } from "../src/landed-blocks/ledger-leaves.js";
import { Frontier, insertRow } from "../src/landed-blocks/store.js";
import { confirmedLedgerRootsOf } from "../src/mpf/confirmed-ledger-roots.js";
import { verifyConfirmedLedger } from "../src/mpf/confirmed-ledger-run.js";
import { withFollowerWrite } from "../src/services/follower-write-gate.js";
import { FRONTIER_HASH, run } from "./commit-base-resolver.fixture.js";
import { simDigest, simOutput } from "./helpers/landed-blocks-sim.universe.js";
import { makeOutRefCbor } from "./midgard-output-helpers.js";
import { resetApplicationTables } from "./utils.js";

const TAIL_HASH = "b1".repeat(28);

const entry = (label: number): Ledger.MinimalEntry => ({
  outref: makeOutRefCbor(simDigest(`paged:${label.toString()}`), 0),
  output: simOutput(BigInt(2_000_000 + label)),
});

const rootOf = (confirmed: readonly Ledger.MinimalEntry[]) =>
  Effect.runPromise(confirmedLedgerRootsOf({ confirmed, delta: [] })).then(
    (roots) => roots.frontierRoot,
  );

const fullReads = Metric.value(ConfirmedLedgerDB.fullScanCounter).pipe(
  Effect.map((state) => state.count),
);

/** `confirmed_ledger` holds `count` entries at the frontier; the processed
 * row after it spends the first and the last and produces one. */
const seedPaged = async (count: number) => {
  const confirmed = Array.from({ length: count }, (_, label) => entry(label));
  const spent = [confirmed[0]!, confirmed[count - 1]!];
  const produced = entry(count);
  const r0 = await rootOf(confirmed);
  const r1 = await rootOf([
    ...confirmed.filter((each) => !spent.includes(each)),
    produced,
  ]);
  await run(
    withFollowerWrite(
      Effect.gen(function* () {
        yield* resetApplicationTables;
        for (let from = 0; from < count; from += 5_000)
          yield* ConfirmedLedgerDB.insertMultiple([
            ...(yield* ledgerRows(
              confirmed.slice(from, from + 5_000),
              new Map(),
            )),
          ]);
        yield* Frontier.upsert({ headerHash: FRONTIER_HASH, utxosRoot: r0 });
        yield* insertRow({
          headerHash: TAIL_HASH,
          parentHeaderHash: FRONTIER_HASH,
          parentUtxosRoot: r0,
          utxosRoot: r1,
          kind: "foreign",
          state: "processed",
          applied: true,
          spent: spent.map((each) => each.outref),
          produced: [produced],
          depositIds: [],
          withdrawals: [],
          forcedIds: [],
          txIds: [],
        });
      }),
    ),
  );
  return { confirmed, r0, r1 };
};

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

  it("passes at 300,000 entries under the worker's fixed heap limit", async () => {
    const count = 300_000;
    const { r0, r1 } = await seedPaged(count);
    const outcome = await run(verifyConfirmedLedger());
    expect(outcome).toMatchObject({
      kind: "verified",
      frontier: { utxosRoot: r0 },
      tail: { utxosRoot: r1 },
      entries: count,
    });
  }, 1_800_000);
});
