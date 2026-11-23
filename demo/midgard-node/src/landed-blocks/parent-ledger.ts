/**
 * The ledger a new landed block is checked or replayed on: its parent's,
 * from the node's ledger store by the parent's root. Processing persists
 * every block's ledger after it before recording the block, so the parent of
 * each block it reaches is retained unless a `retain` dropped it: a row that
 * relands after a rollback took it out of every retained window, or an old
 * processed row a later block built on. Then the parent's ledger is rebuilt
 * from the newest root the store retains on the processed chain to it, by
 * the rows' deltas (each proportional to its block). A ledger this node
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

/** The retained ledger at `root`, or `undefined` when the store does not
 * retain it. */
const retained = <R>(ports: LandedBlockPorts<R>, root: string) =>
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

/**
 * The ledger at `parent`'s root, or the local-fault hold on `node` when this
 * node cannot give it. `lineage` is the processed chain to `parent`, in
 * order, each row's parent root that of the row before it; the newest root on
 * it the store retains, the parent's own first, is where a rebuild starts.
 */
export const parentLedger = <R>(
  ports: LandedBlockPorts<R>,
  node: LandedNode,
  parent: Readonly<{ utxosRoot: string }>,
  lineage: readonly LandedBlockRow[],
) =>
  Effect.gen(function* () {
    const held = (detail: string) =>
      replayOutcomeAlarm(node.headerHash, "local_fault", detail).pipe(
        Effect.as<ParentLedger>({
          kind: "held",
          hold: replayOutcomeHold("local_fault", detail, node.headerHash),
        }),
      );
    const found = yield* retained(ports, parent.utxosRoot);
    if (found !== undefined)
      return { kind: "ledger", ledger: found } satisfies ParentLedger;
    for (let from = lineage.length - 1; from >= 0; from--) {
      const base = yield* retained(ports, lineage[from]!.parentUtxosRoot);
      if (base === undefined) continue;
      const rebuilt = yield* Effect.either(
        replayDeltas(base, lineage.slice(from)),
      );
      if (rebuilt._tag === "Left") {
        if (!(rebuilt.left instanceof LedgerMutationRefused))
          return yield* Effect.fail(rebuilt.left);
        return yield* held(
          `block ${node.headerHash}: the processed deltas to its parent's root ${parent.utxosRoot} do not apply to the retained ledger at ${base.root}: ${rebuilt.left.message}`,
        );
      }
      if (rebuilt.right.root !== parent.utxosRoot)
        return yield* held(
          `block ${node.headerHash}: the processed deltas from the retained ledger at ${base.root} reach root ${rebuilt.right.root}, not its parent's root ${parent.utxosRoot}`,
        );
      return { kind: "ledger", ledger: rebuilt.right } satisfies ParentLedger;
    }
    return yield* held(
      `block ${node.headerHash}: the ledger store retains neither its parent's root ${parent.utxosRoot} nor any root on the processed chain to it`,
    );
  });
