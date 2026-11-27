import { Effect, Metric } from "effect";

import { MempoolLedgerDB } from "../src/database/index.js";
import {
  type MempoolLedgerCacheService,
  type MempoolLedgerState,
  validationLedgerCacheFullReloadCounter,
} from "../src/services/mempool-ledger-cache.js";

export const row = (
  outref: Buffer,
  output: Buffer,
): MempoolLedgerDB.EntryWithTimeStamp => ({
  [MempoolLedgerDB.Columns.TX_ID]: Buffer.alloc(32, 0x77),
  [MempoolLedgerDB.Columns.OUTREF]: outref,
  [MempoolLedgerDB.Columns.OUTPUT]: output,
  [MempoolLedgerDB.Columns.ADDRESS]: "addr_test1_mempool_cache",
  [MempoolLedgerDB.Columns.SOURCE_EVENT_ID]: Buffer.alloc(32, 0x33),
  [MempoolLedgerDB.Columns.TIMESTAMPTZ]: new Date(0),
});

/**
 * What the working ledger answers at `outrefs` (the keys a test wrote), as a
 * map of the hex outrefs it holds, in the order given.
 */
export const snapshot =
  (outrefs: readonly Buffer[]) => (state: MempoolLedgerState) =>
    new Map(
      outrefs.flatMap((outref) => {
        const key = outref.toString("hex");
        const output = state.get(key);
        return output === undefined
          ? []
          : [[key, Buffer.from(output)] as const];
      }),
    );

/**
 * The cache's keyed read port over the table `rows()` returns, recording the
 * keys of each read (sorted) in `asked`.
 */
export const keyedReadsOver = (
  rows: () => readonly MempoolLedgerDB.EntryWithTimeStamp[],
) => {
  const asked: string[][] = [];
  const spendableAt = (keys: ReadonlySet<string>) =>
    Effect.sync(() => {
      asked.push([...keys].sort());
      return new Map(
        rows().flatMap((entry) => {
          const key = entry[MempoolLedgerDB.Columns.OUTREF].toString("hex");
          return keys.has(key)
            ? [[key, entry[MempoolLedgerDB.Columns.OUTPUT]] as const]
            : [];
        }),
      );
    });
  return { asked, spendableAt };
};

/** How many full reloads the validation caches made so far. */
export const fullReloads = Metric.value(
  validationLedgerCacheFullReloadCounter,
).pipe(Effect.map((state) => state.count));

/** The keyed read port of a cache whose test never makes a keyed read. */
export const noKeyedReads = () =>
  Effect.die(new Error("the cache made a keyed read this test never expects"));

/** `snapshot(outrefs)` of `service`'s working ledger, under the Phase B lock. */
export const readAt = (
  service: MempoolLedgerCacheService,
  outrefs: readonly Buffer[],
) =>
  service.withPhaseBLock(
    service.currentState.pipe(Effect.map(snapshot(outrefs))),
  );
