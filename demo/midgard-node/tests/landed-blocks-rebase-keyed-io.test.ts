/**
 * A landed-block processing run with the rebase it asks for (the driver's
 * recompute, `rebaseIfDue`), on a ledger that holds outputs no block, event
 * or pending transaction names, beside a deposit projected outside any
 * block, with the validation cache already loaded: the run scans
 * `confirmed_ledger` zero times (`confirmed_ledger_full_scan_total`),
 * reloads the cache zero times (`validation_ledger_cache_full_reload_count`),
 * and every statement it runs on `mempool_ledger` or `confirmed_ledger` is
 * keyed and names none of those outputs. A `mempool_ledger` row whose
 * contents the run leaves as they were is not rewritten (its `xmin` is
 * kept). The cache then holds what `mempool_ledger` holds spendable.
 */
import { SqlClient, Statement } from "@effect/sql";
import { Effect, Metric, Runtime } from "effect";
import { expect, it } from "vitest";

import { fullScanCounter } from "../src/database/confirmedLedger.js";
import { ConfirmedLedgerDB, MempoolLedgerDB } from "../src/database/index.js";
import type * as Ledger from "../src/database/utils/ledger.js";
import { ledgerRows } from "../src/landed-blocks/ledger.js";
import type { LandedBlockPorts } from "../src/landed-blocks/ports.js";
import { processLandedQueue } from "../src/landed-blocks/process.js";
import { Frontier, retrieveRows } from "../src/landed-blocks/store.js";
import type { Database } from "../src/services/database.js";
import { makeDriverRecompute } from "../src/services/l1-follower.recompute.js";
import { Lucid } from "../src/services/lucid.js";
import { mempoolLedgerCacheLayer } from "../src/services/mempool-ledger-cache.make-mempool-ledger-cache.js";
import {
  MempoolLedgerCache,
  validationLedgerCacheFullReloadCounter,
} from "../src/services/mempool-ledger-cache.mempool-ledger-cache-service.js";
import { MidgardContracts } from "../src/services/midgard-contracts.make-midgard-contract-runtime.js";
import { modelSlotLucid, testWrite } from "./helpers/driver-recompute.js";
import { writeFollowerView } from "./helpers/follower-view.js";
import { admitPending } from "./helpers/landed-blocks-sim.mempool.js";
import {
  block,
  confirmedAt,
  entry,
  harness,
  ledgerStoreOf,
  ports,
  queueOf,
  rootOf,
} from "./landed-blocks-own.fixture.js";
import {
  freshNative,
  hex,
  type Native,
  pendingTx,
  processOf,
  run,
} from "./landed-blocks-rebase.fixture.js";
import {
  depositOf,
  projectDeposit,
} from "./landed-blocks-rebase-equivalence.fixture.js";
import { resetApplicationTables } from "./utils.js";

const G0 = entry("io:g0", 2_000_000n);
const G1 = entry("io:g1", 2_000_001n);
const X1 = entry("io:x1", 3_000_000n);
const X2 = entry("io:x2", 3_000_001n);
/** Outputs no block, event or pending transaction names. */
const UNNAMED = Array.from({ length: 24 }, (_, index) =>
  entry(`io:unnamed:${index}`, 1_000_000n + BigInt(index)),
);
/** The output the pending transaction spends. */
const SPENT_BY_PENDING = entry("io:spent-by-pending", 1_500_000n);
const BASE = [G0, G1, SPENT_BY_PENDING, ...UNNAMED];
/** A deposit projected into the working ledger outside any block. */
const OUTSIDE = depositOf("io:outside", 7_000_000n);

/** A foreign block's replayed delta: it spends `spent` and produces `produced`. */
type Delta = Readonly<{
  spent: Ledger.MinimalEntry;
  produced: Ledger.MinimalEntry;
}>;

const without = (
  entries: readonly Ledger.MinimalEntry[],
  { spent, produced }: Delta,
) => [...entries.filter((item) => item !== spent), produced];

type Logged = Readonly<{ sql: string; params: readonly unknown[] }>;

it("processes a block and rebases onto it at the keys the rebase names, with no whole-ledger read and no cache reload", async () => {
  const baseRoot = await rootOf(BASE);
  const afterF1 = without(BASE, { spent: G0, produced: X1 });
  const afterF2 = without(afterF1, { spent: G1, produced: X2 });
  const genesis = confirmedAt("genesis", baseRoot);
  const f1 = block(
    41,
    { hash: genesis.headerHash, root: baseRoot, endTime: 0n },
    await rootOf(afterF1),
  );
  const f2 = block(
    42,
    { hash: f1.hash, root: f1.header.utxosRoot, endTime: f1.header.endTime },
    await rootOf(afterF2),
  );
  const deltas = new Map<string, Delta>([
    [f1.hash, { spent: G0, produced: X1 }],
    [f2.hash, { spent: G1, produced: X2 }],
  ]);
  const native: Native = {
    ...freshNative(),
    durableRoot: baseRoot,
    ledgers: {
      [baseRoot]: BASE,
      [f1.header.utxosRoot]: afterF1,
      [f2.header.utxosRoot]: afterF2,
    },
    next: {
      [baseRoot]: f1.header.utxosRoot,
      [f1.header.utxosRoot]: f2.header.utxosRoot,
    },
  };
  const globals = await processOf(native);
  const pending = pendingTx("io:pending", [SPENT_BY_PENDING.outref], 1);
  const unnamed = new Set(UNNAMED.map((item) => hex(item.outref)));

  const log: Logged[] = [];
  let logging = false;
  const logStatements: Statement.Statement.Transformer = (self) =>
    Effect.sync(() => {
      if (logging) {
        const [sql, params] = self.compile();
        log.push({ sql, params });
      }
      return self;
    });

  // The frontier's ledger, which the working ledger equals, a pending
  // transaction admitted on it and a deposit projected outside any block.
  await run(
    globals,
    Effect.gen(function* () {
      yield* resetApplicationTables;
      const sql = yield* SqlClient.SqlClient;
      yield* testWrite(
        Effect.gen(function* () {
          const rows = yield* ledgerRows(BASE, new Map());
          yield* ConfirmedLedgerDB.insertMultiple([...rows]);
          for (const row of rows)
            yield* sql`INSERT INTO mempool_ledger ${sql.insert({
              ...row,
              source_event_id: null,
            } as never)}`;
          yield* Frontier.upsert({
            headerHash: genesis.headerHash,
            utxosRoot: baseRoot,
          });
        }),
      );
      yield* admitPending([pending]);
    }),
  );
  await projectDeposit(globals, OUTSIDE);

  await run(
    globals,
    Effect.gen(function* () {
      const sql = yield* SqlClient.SqlClient;
      const runtime = yield* Effect.runtime<Database>();
      const cache = yield* MempoolLedgerCache;
      const recompute = yield* makeDriverRecompute({
        planCurrent: () =>
          Runtime.runPromise(runtime)(
            writeFollowerView(100, []).pipe(
              Effect.map((plan) => ({ kind: "ok", plan }) as const),
            ),
          ),
        initializeNativeOwner: false,
      });
      const state = harness();
      state.ledgers = ledgerStoreOf(BASE);
      const landed: LandedBlockPorts<never> = {
        ...ports(state),
        write: (work) => testWrite(work),
        replay: (input) =>
          Effect.gen(function* () {
            const delta = deltas.get(input.headerHash)!;
            const after = yield* Effect.promise(() =>
              input.parent.apply([
                { type: "delete", outRef: delta.spent.outref },
                {
                  type: "insert",
                  outRef: delta.produced.outref,
                  output: delta.produced.output,
                },
              ]),
            );
            return {
              kind: "replayed",
              after,
              spent: [delta.spent.outref],
              produced: [delta.produced],
              depositIds: [],
              withdrawals: [],
              forcedIds: [],
              named: { events: [], forced: [] },
              txIds: [],
              scriptChecks: {
                context: {
                  blockTimeMs: 0,
                  blockSlot: 0n,
                  expectedNetworkId: 0n,
                  minFeeA: 0n,
                  minFeeB: 0n,
                  l1SlotConfig: { zeroTime: 0, zeroSlot: 0, slotLength: 1_000 },
                },
                checks: [],
                outputs: new Map(),
              },
            } as const;
          }),
        rebase: recompute.rebaseIfDue,
      };

      /** The working rows by hex outref: contents and `xmin`. */
      const workingRows = sql<{
        outref: Buffer;
        tx_id: Buffer;
        output: Buffer;
        address: string;
        source_event_id: Buffer | null;
        xmin: string;
      }>`SELECT outref, tx_id, output, address, source_event_id,
          xmin::text AS xmin FROM mempool_ledger`.pipe(
        Effect.map(
          (rows) =>
            new Map(
              rows.map((row) => [
                hex(row.outref),
                {
                  contents: [
                    hex(row.tx_id),
                    hex(row.output),
                    row.address,
                    row.source_event_id && hex(row.source_event_id),
                  ].join(":"),
                  xmin: row.xmin,
                },
              ]),
            ),
        ),
      );
      // Every output the test writes, and what each holds spendable: in
      // `mempool_ledger`, and in the cache.
      const written = [X1, X2, ...BASE, ...pending.produced, OUTSIDE].map(
        (item) => hex(item.outref),
      );
      const spendableAtWritten = MempoolLedgerDB.retrieveSpendable.pipe(
        Effect.map((rows) => {
          const spendable = new Map(
            rows.map((row) => [
              hex(row[MempoolLedgerDB.Columns.OUTREF]),
              hex(row[MempoolLedgerDB.Columns.OUTPUT]),
            ]),
          );
          return written.map((key) => [key, spendable.get(key)]);
        }),
      );
      const cacheAtWritten = cache.withPhaseBLock(cache.currentState).pipe(
        Effect.map((current) =>
          written.map((key) => {
            const output = current.get(key);
            return [key, output && hex(output)];
          }),
        ),
      );

      // The cache loads once, in full.
      yield* recompute.run("load the cache");
      expect(yield* cacheAtWritten).toEqual(yield* spendableAtWritten);

      for (const [label, nodes] of [
        ["process f1", [f1]],
        ["process f2", [f1, f2]],
      ] as const) {
        const before = yield* workingRows;
        const scans = (yield* Metric.value(fullScanCounter)).count;
        const reloads = (yield* Metric.value(
          validationLedgerCacheFullReloadCounter,
        )).count;
        log.length = 0;
        logging = true;
        const held = yield* processLandedQueue(landed, queueOf(genesis, nodes));
        logging = false;
        expect(held, label).toBeUndefined();
        expect(
          (yield* retrieveRows).map((row) => row.applied),
          label,
        ).toEqual(nodes.map(() => true));
        expect(
          (yield* Metric.value(fullScanCounter)).count - scans,
          label,
        ).toBe(0);
        expect(
          (yield* Metric.value(validationLedgerCacheFullReloadCounter)).count -
            reloads,
          label,
        ).toBe(0n);

        const ledgerStatements = log.filter((statement) =>
          /\b(mempool_ledger|confirmed_ledger)\b/.test(statement.sql),
        );
        expect(ledgerStatements.length, label).toBeGreaterThan(0);
        for (const statement of ledgerStatements) {
          // Keyed: a select, update or delete names the rows it reads; an
          // insert writes the rows it lists.
          if (!/^\s*INSERT\b/i.test(statement.sql))
            expect(statement.sql, label).toMatch(/\bWHERE\b/i);
          const named = statement.params
            .filter((param): param is Uint8Array => param instanceof Uint8Array)
            .map((param) => Buffer.from(param).toString("hex"))
            .filter((param) => unnamed.has(param));
          expect(named, `${label}: ${statement.sql}`).toEqual([]);
        }
        const after = yield* workingRows;
        const kept = [...after].filter(
          ([key, row]) => before.get(key)?.contents === row.contents,
        );
        for (const key of unnamed)
          expect(after.get(key)?.contents, label).toBe(
            before.get(key)?.contents,
          );
        expect(kept.map(([key]) => key)).toContain(hex(OUTSIDE.outref));
        for (const [key, row] of kept)
          expect([key, row.xmin], label).toEqual([key, before.get(key)!.xmin]);
        expect(yield* cacheAtWritten, label).toEqual(yield* spendableAtWritten);
      }

      // The working ledger the two blocks, the pending transaction and the
      // deposit leave.
      expect([...(yield* workingRows).keys()].sort()).toEqual(
        [X1, X2, ...UNNAMED, ...pending.produced, OUTSIDE]
          .map((item) => hex(item.outref))
          .sort(),
      );
    }).pipe(
      Statement.withTransformer(logStatements),
      Effect.provide(mempoolLedgerCacheLayer),
      Effect.provideService(Lucid, modelSlotLucid),
      Effect.provideService(MidgardContracts, {} as MidgardContracts),
    ),
  );
}, 120_000);
