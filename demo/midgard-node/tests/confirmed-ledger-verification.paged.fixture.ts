/**
 * The node database of the paged verification runs
 * (`confirmed-ledger-verification.paged.test.ts`,
 * `confirmed-ledger-verification.paged-at-size.bench.ts`): a
 * `confirmed_ledger` of any number of entries at the frontier, and a
 * processed row after it.
 */
import { Effect } from "effect";

import { ConfirmedLedgerDB } from "../src/database/index.js";
import type * as Ledger from "../src/database/utils/ledger.js";
import { ledgerRows } from "../src/landed-blocks/ledger.js";
import { Frontier, insertRow } from "../src/landed-blocks/store.js";
import { confirmedLedgerRootsOf } from "../src/mpf/confirmed-ledger-roots.js";
import { withFollowerWrite } from "../src/services/follower-write-gate.js";
import { FRONTIER_HASH, run } from "./commit-base-resolver.fixture.js";
import { simDigest, simOutput } from "./helpers/landed-blocks-sim.universe.js";
import { makeOutRefCbor } from "./midgard-output-helpers.js";
import { resetApplicationTables } from "./utils.js";

export const TAIL_HASH = "b1".repeat(28);

const entry = (label: number): Ledger.MinimalEntry => ({
  outref: makeOutRefCbor(simDigest(`paged:${label.toString()}`), 0),
  output: simOutput(BigInt(2_000_000 + label)),
});

const rootOf = (confirmed: readonly Ledger.MinimalEntry[]) =>
  Effect.runPromise(confirmedLedgerRootsOf({ confirmed, delta: [] })).then(
    (roots) => roots.frontierRoot,
  );

/** `confirmed_ledger` holds `count` entries at the frontier; the processed
 * row after it spends the first and the last and produces one. */
export const seedPaged = async (count: number) => {
  const confirmed = Array.from({ length: count }, (_, label) => entry(label));
  const spent = [confirmed[0]!, confirmed[count - 1]!];
  const produced = entry(count);
  const r0 = await rootOf(confirmed);
  const r1 = await rootOf([
    ...confirmed.filter((each) => !spent.includes(each)),
    produced,
  ]);
  // The reset runs first, outside the gated write: a file run earlier on this
  // worker's database may have left the gate at an applied view, which refuses
  // a fixture's write until the reset restores the gate's seed row.
  await run(resetApplicationTables);
  await run(
    withFollowerWrite(
      Effect.gen(function* () {
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
