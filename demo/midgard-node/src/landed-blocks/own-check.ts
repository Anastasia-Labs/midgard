/**
 * The check an own landed block passes before processing adopts it from
 * this node's journal: the journal must describe the block (its base and
 * expected root), and its delta must reach the header's root on the
 * parent's ledger, which the node's ledger store holds by the parent's root
 * (`parent-ledger.ts`). The store's ledgers are this node's own, so a parent
 * ledger read from it is trusted and never checked again. A journal that
 * fails is a local fault on the view (`landed_block_own_journal_mismatch`),
 * never the block's.
 */
import {
  type Ledger,
  type LedgerMutation,
  LedgerMutationRefused,
} from "@al-ft/midgard-fault-proofs/ledger-store";
import { Effect } from "effect";

import { type DriverHold, notRetried } from "../l1-events/driver.js";
import type { LandedNode } from "./history.js";
import { LANDED_BLOCK_OWN_JOURNAL_MISMATCH } from "./holds.js";
import { parentLedger } from "./parent-ledger.js";
import type { LandedBlockPorts, OwnJournal } from "./ports.js";
import type { LandedBlockRow } from "./store.js";

/** `journal`'s delta as ledger edits: its spent out-refs, then its produced
 * entries (a journal's two sets are disjoint). */
const journalMutations = (journal: OwnJournal): LedgerMutation[] => [
  ...journal.spent.map((outRef) => ({ type: "delete", outRef }) as const),
  ...journal.produced.map(
    (entry) =>
      ({ type: "insert", outRef: entry.outref, output: entry.output }) as const,
  ),
];

/** The ledger `journal`'s delta gives on `ledger`, or why it gives none: the
 * ledger refuses an edit (a delete of an out-ref it does not hold, an insert
 * over one it holds). */
const journalAfter = (ledger: Ledger, journal: OwnJournal) =>
  Effect.tryPromise({
    try: () => ledger.apply(journalMutations(journal)),
    catch: (cause) => cause,
  }).pipe(
    Effect.map((after) => ({ kind: "after", after }) as const),
    Effect.catchIf(
      (error) => error instanceof LedgerMutationRefused,
      (error) =>
        Effect.succeed({
          kind: "refused",
          detail: (error as LedgerMutationRefused).message,
        } as const),
    ),
  );

/** A local fault on the view: the next follower change re-reads it. */
const mismatch = (detail: string): DriverHold =>
  notRetried({ reason: LANDED_BLOCK_OWN_JOURNAL_MISMATCH, detail });

/** What processing does with an own block: holds at it, or adopts it with
 * the ledger after it. */
export type OwnBlock =
  | Readonly<{ kind: "held"; hold: DriverHold }>
  | Readonly<{ kind: "after"; after: Ledger }>;

/**
 * Own block `node` on `parent`, whose journal is `journal`: the hold that
 * stops processing at it, or the ledger after it on the parent's ledger
 * (`lineage`: the processed chain to `parent`, `parent-ledger.ts`).
 */
export const ownBlock = <R>(
  ports: LandedBlockPorts<R>,
  node: LandedNode,
  parent: Readonly<{ headerHash: string; utxosRoot: string }>,
  lineage: readonly LandedBlockRow[],
  journal: OwnJournal,
) =>
  Effect.gen(function* () {
    const held = (hold: DriverHold): OwnBlock => ({ kind: "held", hold });
    if (
      journal.baseTailHeaderHash !== parent.headerHash ||
      journal.baseUtxosRoot !== parent.utxosRoot ||
      journal.expectedUtxosRoot !== node.header.utxosRoot
    )
      return held(
        mismatch(
          `own block ${node.headerHash}'s journal does not describe the landed block (base ${journal.baseTailHeaderHash}/${journal.baseUtxosRoot}, expected ${journal.expectedUtxosRoot})`,
        ),
      );
    const base = yield* parentLedger(ports, node, parent, lineage);
    if (base.kind === "held") return base;
    const applied = yield* journalAfter(base.ledger, journal);
    if (applied.kind === "refused")
      return held(
        mismatch(
          `own block ${node.headerHash}'s journal delta does not apply to its parent's ledger: ${applied.detail}`,
        ),
      );
    if (applied.after.root !== node.header.utxosRoot)
      return held(
        mismatch(
          `own block ${node.headerHash}'s journal delta reaches root ${applied.after.root} on its parent's ledger, its header commits ${node.header.utxosRoot}`,
        ),
      );
    return { kind: "after", after: applied.after } satisfies OwnBlock;
  });
