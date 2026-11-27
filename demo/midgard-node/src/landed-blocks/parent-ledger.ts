/**
 * The ledger a new landed block is checked or replayed on: its parent's,
 * from the node's ledger store by the parent's root. Processing persists
 * every block's ledger after it before recording the block, so the parent of
 * each block it reaches is retained unless a `retain` dropped it: a row that
 * relands after a rollback took it out of every retained window, or an old
 * processed row a later block built on. Then the parent's ledger is rebuilt
 * from the newest root the store retains on the processed chain to it, by
 * the rows' deltas (each proportional to its block). Processing has the
 * store hold the confirmed-ledger frontier's root the same way, along the
 * retained folds (`frontier-root.ts`). A ledger this node
 * cannot give is its own fault, never the block's: a retried local fault on
 * the block (`landed_block_local_fault`, with its alarm).
 */
import {
  type Ledger,
  LedgerMutationRefused,
  LedgerNotRetained,
} from "@al-ft/midgard-fault-proofs/ledger-store";
import { Effect } from "effect";

import type { DriverHold } from "../l1-events/driver.js";
import type { LandedNode } from "./history.js";
import type { LandedBlockPorts } from "./ports.js";
import {
  replayOutcomeAlarm,
  replayOutcomeHold,
} from "./replay-outcome-hold.js";
import type { LandedBlockRow } from "./store.js";

export type ParentLedger =
  | Readonly<{ kind: "ledger"; ledger: Ledger }>
  | Readonly<{ kind: "held"; hold: DriverHold }>;

/** Where a ledger is read from: the ledgers the node's store retains. */
type RetainedLedgers<R> = Readonly<{
  retainedLedger: (root: string) => Effect.Effect<Ledger, unknown, R>;
}>;

/** The retained ledger at `root`, or `undefined` when the store does not
 * retain it. */
const retained = <R>(ports: RetainedLedgers<R>, root: string) =>
  ports.retainedLedger(root).pipe(
    Effect.map((ledger): Ledger | undefined => ledger),
    Effect.catchIf(
      (error) => error instanceof LedgerNotRetained,
      () => Effect.succeed(undefined),
    ),
  );

/** `ledger` after `rows`' deltas in order, or why it is not their last
 * root: a delta the ledger refuses. */
const replayDeltas = (ledger: Ledger, rows: readonly LandedBlockRow[]) =>
  Effect.tryPromise({
    try: async () => {
      let after = ledger;
      for (const row of rows)
        after = await after.apply([
          ...row.spent.map((outRef) => ({ type: "delete", outRef }) as const),
          ...row.produced.map(
            (entry) =>
              ({
                type: "insert",
                outRef: entry.outref,
                output: entry.output,
              }) as const,
          ),
        ]);
      return after;
    },
    catch: (cause) => cause,
  });

/** The ledger at a root: the store's, rebuilt, or why this node cannot give
 * it. */
export type LedgerAt =
  | Readonly<{ kind: "retained"; ledger: Ledger }>
  /** Rebuilt by processed deltas; the store does not hold it yet. */
  | Readonly<{ kind: "rebuilt"; ledger: Ledger }>
  | Readonly<{ kind: "unavailable"; detail: string }>;

/**
 * The ledger at `root`. `chain` reads the chain of rows to `root`, in order,
 * each row's parent root that of the row before it and the last row's root
 * `root`; it is read only when the store does not retain `root`. The newest
 * root on it the store retains is where a rebuild starts. A delta the
 * rebuilt ledger refuses, or one that reaches another root, makes it
 * unavailable; any other failure fails.
 */
export const ledgerAt = <R, E, R2>(
  ports: RetainedLedgers<R>,
  root: string,
  chain: Effect.Effect<readonly LandedBlockRow[], E, R2>,
) =>
  Effect.gen(function* () {
    const found = yield* retained(ports, root);
    if (found !== undefined)
      return { kind: "retained", ledger: found } satisfies LedgerAt;
    const lineage = yield* chain;
    for (let from = lineage.length - 1; from >= 0; from--) {
      const base = yield* retained(ports, lineage[from]!.parentUtxosRoot);
      if (base === undefined) continue;
      const rebuilt = yield* Effect.either(
        replayDeltas(base, lineage.slice(from)),
      );
      if (rebuilt._tag === "Left") {
        if (!(rebuilt.left instanceof LedgerMutationRefused))
          return yield* Effect.fail(rebuilt.left);
        return {
          kind: "unavailable",
          detail: `the processed deltas to root ${root} do not apply to the retained ledger at ${base.root}: ${rebuilt.left.message}`,
        } satisfies LedgerAt;
      }
      if (rebuilt.right.root !== root)
        return {
          kind: "unavailable",
          detail: `the processed deltas from the retained ledger at ${base.root} reach root ${rebuilt.right.root}, not ${root}`,
        } satisfies LedgerAt;
      return { kind: "rebuilt", ledger: rebuilt.right } satisfies LedgerAt;
    }
    return {
      kind: "unavailable",
      detail: `the ledger store retains neither root ${root} nor any root on the processed chain to it`,
    } satisfies LedgerAt;
  });

/**
 * The ledger at `parent`'s root, or the local-fault hold on `node` when this
 * node cannot give it; `lineage` is the processed chain to `parent`
 * (`ledgerAt`).
 */
export const parentLedger = <R>(
  ports: LandedBlockPorts<R>,
  node: LandedNode,
  parent: Readonly<{ utxosRoot: string }>,
  lineage: readonly LandedBlockRow[],
) =>
  Effect.gen(function* () {
    const at = yield* ledgerAt(
      ports,
      parent.utxosRoot,
      Effect.succeed(lineage),
    );
    if (at.kind !== "unavailable")
      return { kind: "ledger", ledger: at.ledger } satisfies ParentLedger;
    const detail = `block ${node.headerHash}: its parent's ledger: ${at.detail}`;
    yield* replayOutcomeAlarm(node.headerHash, "local_fault", detail);
    return {
      kind: "held",
      hold: replayOutcomeHold("local_fault", detail, node.headerHash),
    } satisfies ParentLedger;
  });
