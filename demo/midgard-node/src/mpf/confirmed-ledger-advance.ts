/**
 * The verified confirmed ledger advances inductively between full runs. A
 * full run (`confirmed-ledger-verification.ts`) records the frontier, the
 * frontier generation it read and the digest of each landed delta it
 * checked to the tail. Each fold landed processing commits (`foldToRoot` in
 * `landed-blocks/fold.ts`, through its `folded` port) is logged, whether or
 * not it advances the record, and the record is replayed over the log
 * (`replayFolds`); a pass recorded later is replayed over it too, so a fold
 * made while a run was reading is applied to that run's pass. A logged fold
 * advances a record to the folded block, its root and the generation the
 * fold left, when:
 *
 * - the fold read, under its frontier lock, the record's generation (a
 *   compare-and-set: no write to `confirmed_ledger` or the frontier came
 *   between the verified state and the fold), so the record is the folded
 *   block's parent;
 * - the block builds on the record's header and root, the fold's base
 *   checks passed (it committed) and its delta passes the commit's apply
 *   guards (`deltaShapeFault`; with no duplicate outref, the fold's counts
 *   mean every output it spends was held and none it produces was);
 * - the block's stored delta digest is the one the record's run checked
 *   for it.
 *
 * A replay stops at the first fold that is not logged or fails one of
 * these; commits on a later frontier wait for the next full run. The log
 * holds at most `CONFIRMED_LEDGER_FOLD_LOG_CAP` folds: a fold past it drops
 * every fold logged before it, and makes a full run due, so a replay that
 * needed a dropped fold stops early and the run catches the record up.
 */
import { Effect, MutableRef, Ref } from "effect";

import { landedDeltaDigest } from "../landed-blocks/delta-digest.js";
import type { CommittedFold } from "../landed-blocks/ports.js";
import type { Globals } from "../services/globals.js";
import { deltaShapeFault } from "../workers/commit-block-header.landed-commit-base.js";
import type { VerifiedFrontier } from "../workers/utils/commit-block-header.js";

/** What the main thread records as verified: the frontier at its
 * generation, and the digest of each landed delta the run checked after it,
 * by header. */
export type VerifiedConfirmedLedger = VerifiedFrontier &
  Readonly<{ tailDigests: ReadonlyMap<string, string> }>;

/** Whether generation `left` is below `right`, as numbers. */
export const generationBelow = (left: string, right: string) =>
  BigInt(left) < BigInt(right);

/** A committed fold, as a replay applies it: the block it folded and the
 * root it built on, the frontier generations it read and left, its stored
 * delta's digest (`landedDeltaDigest`) and why that delta fails the
 * commit's apply guards on its own root (`deltaShapeFault`), if it does. */
export type LoggedFold = Readonly<{
  headerHash: string;
  utxosRoot: string;
  parentHeaderHash: string;
  parentUtxosRoot: string;
  readGeneration: string;
  generation: string;
  digest: string;
  shapeFault: string | undefined;
}>;

/** How many folds the log holds: a fold past it drops the log and makes a
 * full run due. */
export const CONFIRMED_LEDGER_FOLD_LOG_CAP = 256;

/** The fold log: the logged folds, oldest first, and whether a full run is
 * due because the log overflowed since the last run started. */
export type ConfirmedLedgerFoldLog = Readonly<{
  folds: readonly LoggedFold[];
  runDue: boolean;
}>;

export const emptyConfirmedLedgerFoldLog: ConfirmedLedgerFoldLog = {
  folds: [],
  runDue: false,
};

export const loggedFold = ({
  row,
  readGeneration,
  generation,
}: CommittedFold): LoggedFold => ({
  headerHash: row.headerHash,
  utxosRoot: row.utxosRoot,
  parentHeaderHash: row.parentHeaderHash,
  parentUtxosRoot: row.parentUtxosRoot,
  readGeneration,
  generation,
  digest: landedDeltaDigest(row),
  shapeFault: deltaShapeFault(row.parentUtxosRoot, row),
});

/** Where and why a replay stopped short of the logged fold on the record:
 * `routine` when the record's run did not check the fold's delta, else the
 * fold does not apply to the record. */
export type ReplayStop = Readonly<{
  fold: LoggedFold;
  fault: string;
  routine: boolean;
}>;

/** Why `fold`, which read `record`'s generation, does not advance it. */
const replayFault = (
  record: VerifiedConfirmedLedger,
  fold: LoggedFold,
): Omit<ReplayStop, "fold"> | undefined => {
  if (
    fold.parentHeaderHash !== record.headerHash ||
    fold.parentUtxosRoot !== record.utxosRoot
  )
    return {
      fault: `the block folds on ${fold.parentHeaderHash}, the verification is at ${record.headerHash}`,
      routine: false,
    };
  if (fold.shapeFault !== undefined)
    return { fault: `its delta ${fold.shapeFault}`, routine: false };
  const digest = record.tailDigests.get(fold.headerHash);
  if (digest === fold.digest) return undefined;
  return digest === undefined
    ? { fault: "the last verification did not check its delta", routine: true }
    : {
        fault: "its stored delta is not the one the last verification checked",
        routine: false,
      };
};

/**
 * `record` replayed over `folds`: while a fold read the record's
 * generation and applies to it (`replayFault`), the record moves to the
 * folded block, its root and the generation the fold left, keeping its
 * digests. Stops at the first generation no fold read, or at a fold that
 * does not apply (`stop`).
 */
export const replayFolds = (
  record: VerifiedConfirmedLedger,
  folds: readonly LoggedFold[],
): Readonly<{
  record: VerifiedConfirmedLedger;
  replayed: number;
  stop?: ReplayStop;
}> => {
  let current = record;
  for (let replayed = 0; replayed <= folds.length; replayed += 1) {
    const fold = folds.find(
      (each) => each.readGeneration === current.generation,
    );
    if (fold === undefined) return { record: current, replayed };
    const refused = replayFault(current, fold);
    if (refused !== undefined)
      return { record: current, replayed, stop: { fold, ...refused } };
    current = {
      ...current,
      headerHash: fold.headerHash,
      utxosRoot: fold.utxosRoot,
      generation: fold.generation,
    };
  }
  return { record: current, replayed: folds.length };
};

/** What a fold did: whether it overflowed the log, the record before it
 * and its replay over the log, if there was a record. */
type Advanced = Readonly<{
  overflowed: boolean;
  verified: VerifiedConfirmedLedger | undefined;
  replay: ReturnType<typeof replayFolds> | undefined;
}>;

/**
 * Logs `fold` and replays the verified record in `globals` over the log,
 * atomically with the recorder: the log is read and written only inside a
 * modify of the record's `Ref`. A fold past the log's cap drops the folds
 * logged before it and makes a full run due.
 */
export const advanceVerifiedConfirmedLedger = (
  globals: Pick<
    Globals,
    "VERIFIED_CONFIRMED_LEDGER" | "CONFIRMED_LEDGER_FOLD_LOG"
  >,
  fold: CommittedFold,
): Effect.Effect<void> =>
  Ref.modify(
    globals.VERIFIED_CONFIRMED_LEDGER,
    (verified): [Advanced, VerifiedConfirmedLedger | undefined] => {
      const logged = loggedFold(fold);
      const { folds, runDue } = MutableRef.get(
        globals.CONFIRMED_LEDGER_FOLD_LOG,
      );
      const overflowed = folds.length >= CONFIRMED_LEDGER_FOLD_LOG_CAP;
      MutableRef.set(globals.CONFIRMED_LEDGER_FOLD_LOG, {
        folds: overflowed ? [logged] : [...folds, logged],
        runDue: runDue || overflowed,
      });
      if (verified === undefined)
        return [{ overflowed, verified, replay: undefined }, verified];
      const replay = replayFolds(
        verified,
        MutableRef.get(globals.CONFIRMED_LEDGER_FOLD_LOG).folds,
      );
      return [{ overflowed, verified, replay }, replay.record];
    },
  ).pipe(
    Effect.flatMap(({ overflowed, verified, replay }) =>
      Effect.zipRight(
        overflowed
          ? Effect.logInfo(
              `confirmed_ledger fold log past ${CONFIRMED_LEDGER_FOLD_LOG_CAP.toString()} folds: dropped, a full verification run is due`,
            )
          : Effect.void,
        replay?.record.generation === fold.generation
          ? Effect.logDebug(
              `confirmed_ledger verification advanced to ${fold.row.headerHash} (generation ${fold.generation})`,
            )
          : replay?.stop?.fold.headerHash === fold.row.headerHash
            ? (replay.stop.routine ? Effect.logDebug : Effect.logWarning)(
                `confirmed_ledger verification not advanced to ${fold.row.headerHash}: ${replay.stop.fault}; commits on it wait for the next run`,
              )
            : Effect.logDebug(
                `confirmed_ledger verification not advanced to ${fold.row.headerHash}: ${verified === undefined ? "no verification is recorded" : `the fold read generation ${fold.readGeneration}, the verification is at ${verified.generation}`}; commits on it wait for the next run`,
              ),
      ),
    ),
  );

/** Whether a full run is due because the fold log overflowed since the
 * last run started. */
export const confirmedLedgerRunDue = (
  globals: Pick<Globals, "CONFIRMED_LEDGER_FOLD_LOG">,
): Effect.Effect<boolean> =>
  Effect.sync(() => MutableRef.get(globals.CONFIRMED_LEDGER_FOLD_LOG).runDue);

/** Marks a full run started: an overflow before it is caught by it. */
export const confirmedLedgerRunStarted = (
  globals: Pick<Globals, "CONFIRMED_LEDGER_FOLD_LOG">,
): Effect.Effect<void> =>
  Effect.sync(() =>
    MutableRef.update(globals.CONFIRMED_LEDGER_FOLD_LOG, (log) =>
      log.runDue ? { ...log, runDue: false } : log,
    ),
  );
