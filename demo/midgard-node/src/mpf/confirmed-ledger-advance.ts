/**
 * The verified confirmed ledger advances inductively between full runs. A
 * full run (`confirmed-ledger-verification.ts`) records the frontier, the
 * frontier generation it read and the digest of each landed delta it
 * checked to the tail. Once a fold landed processing ran commits
 * (`foldToRoot` in `landed-blocks/fold.ts`, through its `folded` port), the
 * record advances to the folded block, its root and the generation the
 * fold left, when:
 *
 * - the fold read, under its frontier lock, the recorded generation (a
 *   compare-and-set: no write to `confirmed_ledger` or the frontier came
 *   between the verified state and the fold), so the record is the folded
 *   block's parent;
 * - the fold's base checks passed (it committed) and the block's delta
 *   passes the commit's apply guards (`deltaShapeFault`; with no duplicate
 *   outref, the fold's counts mean every output it spends was held and none
 *   it produces was);
 * - the block's stored delta digest is the one the last run checked for it.
 *
 * Otherwise the record is unchanged, and commits on the new frontier wait
 * for the next full run.
 */
import { Effect, Ref } from "effect";

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

/** Why `fold` does not advance `verified`, if it does: `routine` when the
 * record is not at the fold's base (no run passed there yet, or the ledger
 * was written since), else the delta is not the one the run checked. */
const advanceFault = (
  verified: VerifiedConfirmedLedger | undefined,
  fold: CommittedFold,
): Readonly<{ fault: string; routine: boolean }> | undefined => {
  const { row } = fold;
  if (verified === undefined)
    return { fault: "no verification is recorded", routine: true };
  if (verified.generation !== fold.readGeneration)
    return {
      fault: `the fold read generation ${fold.readGeneration}, the verification is at ${verified.generation}`,
      routine: true,
    };
  if (
    verified.headerHash !== row.parentHeaderHash ||
    verified.utxosRoot !== row.parentUtxosRoot
  )
    return {
      fault: `the block folds on ${row.parentHeaderHash}, the verification is at ${verified.headerHash}`,
      routine: false,
    };
  const shape = deltaShapeFault(verified.utxosRoot, row);
  if (shape !== undefined)
    return { fault: `its delta ${shape}`, routine: false };
  const digest = verified.tailDigests.get(row.headerHash);
  if (digest === landedDeltaDigest(row)) return undefined;
  return digest === undefined
    ? { fault: "the last verification did not check its delta", routine: true }
    : {
        fault: "its stored delta is not the one the last verification checked",
        routine: false,
      };
};

/**
 * Advances the verified record in `globals` over `fold`, atomically with
 * the recorder (one `Ref`), or leaves it.
 */
export const advanceVerifiedConfirmedLedger = (
  globals: Pick<Globals, "VERIFIED_CONFIRMED_LEDGER">,
  fold: CommittedFold,
): Effect.Effect<void> =>
  Ref.modify(globals.VERIFIED_CONFIRMED_LEDGER, (verified) => {
    const refused = advanceFault(verified, fold);
    return refused !== undefined || verified === undefined
      ? [refused, verified]
      : [
          undefined,
          {
            ...verified,
            headerHash: fold.row.headerHash,
            utxosRoot: fold.row.utxosRoot,
            generation: fold.generation,
          },
        ];
  }).pipe(
    Effect.flatMap((refused) =>
      refused === undefined
        ? Effect.logDebug(
            `confirmed_ledger verification advanced to ${fold.row.headerHash} (generation ${fold.generation})`,
          )
        : (refused.routine ? Effect.logDebug : Effect.logWarning)(
            `confirmed_ledger verification not advanced to ${fold.row.headerHash}: ${refused.fault}; commits on it wait for the next run`,
          ),
    ),
  );
