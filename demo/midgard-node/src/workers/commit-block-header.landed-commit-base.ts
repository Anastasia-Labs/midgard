/**
 * The landed ledger a commit on a queue tail builds on, checked from the
 * verified frontier. The main thread verifies off the commit path that
 * `confirmed_ledger` derives the frontier's root and, with the processed
 * deltas, the queue tail's (`confirmed-ledger-verification.ts`), and the
 * worker receives the frontier it verified at. The tail's post-state is `confirmed_ledger` plus
 * the processed landed deltas from that frontier, so the commit checks only
 * those deltas: each row builds on the root before it, and its delta
 * applies cleanly (each outref once, none both spent and produced, every
 * output it spends held, none it produces held already, and the root
 * unchanged exactly when the delta is empty). Each row's own root was
 * derived when the row was processed, so the chain of roots ends at the
 * tail's root with no whole-ledger root derived here.
 */
import { Data, Effect } from "effect";

import {
  applyDelta,
  type LandedLedger,
  type LedgerMap,
  ledgerMap,
} from "../landed-blocks/ledger.js";
import type { LandedBlockRow } from "../landed-blocks/store.js";
import {
  type LedgerDivergenceCheck,
  LocalLedgerDivergenceError,
} from "../mpf/local-ledger-divergence.js";
import type { VerifiedFrontier } from "./utils/commit-block-header.js";

/** The landed frontier the commit builds from is not the verified one. */
export class CommitBaseUnverifiedError extends Data.TaggedError(
  "CommitBaseUnverifiedError",
)<{ readonly headerHash: string; readonly detail: string }> {}

/** The first outref `outRefs` names twice, as hex, if any. */
const repeated = (outRefs: readonly Buffer[]): string | undefined => {
  const seen = new Set<string>();
  for (const outRef of outRefs) {
    const key = outRef.toString("hex");
    if (seen.has(key)) return key;
    seen.add(key);
  }
  return undefined;
};

/**
 * What in `row`'s delta does not apply to `ledger`, the ledger at the root
 * `root` before it, if anything. A delta is a set of outrefs that went and
 * a set of outputs at outrefs that are new: each outref at most once, none
 * both spent and produced (an outref names one output, so no transaction
 * spends and re-creates it). It changes the root exactly when it is not
 * empty.
 */
const deltaFault = (
  ledger: LedgerMap,
  root: string,
  row: LandedBlockRow,
): string | undefined => {
  if (row.parentUtxosRoot !== root)
    return `builds on root ${row.parentUtxosRoot}, the root before it is ${root}`;
  const empty = row.spent.length === 0 && row.produced.length === 0;
  if (empty !== (row.utxosRoot === row.parentUtxosRoot))
    return empty
      ? `is empty, its root ${row.utxosRoot} is not the root ${row.parentUtxosRoot} before it`
      : `is not empty, its root is the root ${row.parentUtxosRoot} before it`;
  const spentTwice = repeated(row.spent);
  if (spentTwice !== undefined) return `spends ${spentTwice} twice`;
  const producedTwice = repeated(row.produced.map((entry) => entry.outref));
  if (producedTwice !== undefined) return `produces ${producedTwice} twice`;
  const spent = new Set(row.spent.map((outRef) => outRef.toString("hex")));
  const respent = row.produced.find((entry) =>
    spent.has(entry.outref.toString("hex")),
  );
  if (respent !== undefined)
    return `spends and produces ${respent.outref.toString("hex")}`;
  const missing = row.spent.find(
    (outRef) => !ledger.has(outRef.toString("hex")),
  );
  if (missing !== undefined)
    return `spends ${missing.toString("hex")}, which the ledger before it does not hold`;
  const present = row.produced.find((entry) =>
    ledger.has(entry.outref.toString("hex")),
  );
  return present === undefined
    ? undefined
    : `produces ${present.outref.toString("hex")}, which the ledger before it already holds`;
};

type LandedLedgerAt = {
  ledger: LedgerMap;
  root: string;
  row?: LandedBlockRow;
};

/**
 * The post-state of `headerHash` on `landed`, from `confirmed_ledger` at
 * its frontier: its ledger and root, and the row naming it (none at the
 * frontier), or undefined off the processed chain. Each row up to it is
 * checked to build on the root before it and to apply cleanly
 * (`deltaFault`); the first that does not fails as a divergence under
 * `check`.
 */
export const walkLandedLedger = (
  landed: LandedLedger,
  headerHash: string,
  check: LedgerDivergenceCheck,
): Effect.Effect<LandedLedgerAt | undefined, LocalLedgerDivergenceError> =>
  Effect.suspend(() => {
    const { frontier } = landed;
    const ledger = ledgerMap(landed.confirmed);
    if (frontier.headerHash === headerHash)
      return Effect.succeed({ ledger, root: frontier.utxosRoot });
    let root = frontier.utxosRoot;
    for (const row of landed.chain) {
      const fault = deltaFault(ledger, root, row);
      if (fault !== undefined)
        return Effect.fail(
          new LocalLedgerDivergenceError({
            check,
            headerHash: row.headerHash,
            root: row.utxosRoot,
            fault: `takes a delta that ${fault}`,
          }),
        );
      applyDelta(ledger, row);
      root = row.utxosRoot;
      if (row.headerHash === headerHash)
        return Effect.succeed({ ledger, root, row });
    }
    return Effect.succeed(undefined);
  });

type VerifiedLandedLedger = Effect.Effect<
  LandedLedgerAt | undefined,
  CommitBaseUnverifiedError | LocalLedgerDivergenceError
>;

/**
 * The post-state of `headerHash` on `landed`: its ledger and root, and the
 * row naming it (none at the frontier), or undefined off the processed
 * chain. Fails with `CommitBaseUnverifiedError` unless `landed`'s frontier is
 * `verified`, and with a `commit_base_landed` divergence on the first row up
 * to `headerHash` whose delta does not apply.
 */
export const verifiedLandedLedgerAt = (
  landed: LandedLedger,
  headerHash: string,
  verified: VerifiedFrontier | undefined,
): VerifiedLandedLedger =>
  Effect.suspend((): VerifiedLandedLedger => {
    const { frontier } = landed;
    if (
      verified?.headerHash !== frontier.headerHash ||
      verified.utxosRoot !== frontier.utxosRoot
    )
      return Effect.fail(
        new CommitBaseUnverifiedError({
          headerHash,
          detail:
            verified === undefined
              ? `no verification of confirmed_ledger at frontier ${frontier.headerHash} has passed`
              : `confirmed_ledger was last verified at frontier ${verified.headerHash}, the frontier is ${frontier.headerHash}`,
        }),
      );
    return walkLandedLedger(landed, headerHash, "commit_base_landed");
  });
