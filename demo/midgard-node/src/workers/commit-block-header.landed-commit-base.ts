/**
 * Where a commit on a queue tail builds, checked against the verified
 * frontier. The main thread verifies off the commit path that
 * `confirmed_ledger` derives the frontier's root and, with the processed
 * deltas, the queue tail's (`confirmed-ledger-verification.ts`), each delta
 * first checked to apply (`walkLandedLedger`), and the worker receives the
 * frontier it verified at, with the frontier generation it read (or the
 * frontier a clean fold advanced it to, `confirmed-ledger-advance.ts`). A
 * commit on a landed tail builds while the landed frontier is that one, at
 * that generation (`verifiedLandedRootAt`): the outputs it reads by out-ref
 * come from `confirmed_ledger` and the processed rows after the frontier,
 * which that run checked. The ledger itself is the ledger store's at the
 * tail's root.
 */
import { Data, Effect } from "effect";

import {
  applyDelta,
  type GeneratedLandedChain,
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
 * What in `row`'s delta is malformed on the root `root` before it, reading
 * no ledger, if anything. A delta is a set of outrefs that went and a set of
 * outputs at outrefs that are new: each outref at most once, none both
 * spent and produced (an outref names one output, so no transaction spends
 * and re-creates it). It builds on `root`, and changes the root exactly
 * when it is not empty.
 */
export const deltaShapeFault = (
  root: string,
  row: Pick<
    LandedBlockRow,
    "parentUtxosRoot" | "utxosRoot" | "spent" | "produced"
  >,
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
  return respent === undefined
    ? undefined
    : `spends and produces ${respent.outref.toString("hex")}`;
};

/**
 * What in `row`'s delta does not apply to `ledger`, the ledger at the root
 * `root` before it, if anything: a malformed delta (`deltaShapeFault`), an
 * output it spends that `ledger` does not hold, or one it produces that
 * `ledger` holds already.
 */
const deltaFault = (
  ledger: LedgerMap,
  root: string,
  row: LandedBlockRow,
): string | undefined => {
  const shape = deltaShapeFault(root, row);
  if (shape !== undefined) return shape;
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

/** Where a commit on a landed header builds: the header's post-state root
 * and the row naming it (none at the frontier). */
type LandedRootAt = { root: string; row?: LandedBlockRow };

type VerifiedLandedRoot = Effect.Effect<
  LandedRootAt | undefined,
  CommitBaseUnverifiedError
>;

/**
 * Where a commit on `headerHash` builds on `landed`: its post-state root
 * and the row naming it (none at the frontier), or undefined off the
 * processed chain, whatever the verification (the commit waits for
 * processing, not for it). Fails with `CommitBaseUnverifiedError` unless
 * `landed`'s frontier is `verified`, at the generation it was read at (the
 * same header and root at another generation is another ledger). It reads
 * no ledger row.
 */
export const verifiedLandedRootAt = (
  landed: GeneratedLandedChain,
  headerHash: string,
  verified: VerifiedFrontier | undefined,
): VerifiedLandedRoot =>
  Effect.suspend((): VerifiedLandedRoot => {
    const { frontier, generation } = landed;
    const row = landed.chain.find((each) => each.headerHash === headerHash);
    if (frontier.headerHash !== headerHash && row === undefined)
      return Effect.succeed(undefined);
    if (
      verified?.headerHash !== frontier.headerHash ||
      verified.utxosRoot !== frontier.utxosRoot ||
      verified.generation !== generation
    )
      return Effect.fail(
        new CommitBaseUnverifiedError({
          headerHash,
          detail:
            verified === undefined
              ? `no verification of confirmed_ledger at frontier ${frontier.headerHash} has passed`
              : `confirmed_ledger was last verified at frontier ${verified.headerHash} (generation ${verified.generation}), the frontier is ${frontier.headerHash} (generation ${generation})`,
        }),
      );
    return Effect.succeed(
      row === undefined
        ? { root: frontier.utxosRoot }
        : { root: row.utxosRoot, row },
    );
  });
