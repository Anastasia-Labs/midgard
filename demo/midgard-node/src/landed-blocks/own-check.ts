/**
 * The check an own landed block passes before processing adopts it from
 * this node's journal: the journal must describe the block (its base and
 * expected root), and its delta must reach the header's root on the
 * parent's ledger, which the node's ledger store holds by the parent's root.
 * The store's ledgers are this node's own, so a parent ledger read from it
 * is trusted and never checked again. A journal that fails is a local fault
 * on the view (`landed_block_own_journal_mismatch`), never the block's; a
 * parent root the store does not retain is a retried local fault on the
 * block (`landed_block_local_fault`, with its alarm).
 */
import {
  type LedgerMutation,
  LedgerMutationRefused,
  LedgerNotRetained,
} from "@al-ft/midgard-fault-proofs/ledger-store";
import { Effect } from "effect";

import { type DriverHold, notRetried } from "../l1-events/driver.js";
import type { LandedNode } from "./history.js";
import { LANDED_BLOCK_OWN_JOURNAL_MISMATCH } from "./holds.js";
import type { LandedBlockPorts, OwnJournal } from "./ports.js";
import {
  replayOutcomeAlarm,
  replayOutcomeHold,
} from "./replay-outcome-hold.js";

/** `journal`'s delta as ledger edits: its spent out-refs, then its produced
 * entries (a journal's two sets are disjoint). */
const journalMutations = (journal: OwnJournal): LedgerMutation[] => [
  ...journal.spent.map((outRef) => ({ type: "delete", outRef }) as const),
  ...journal.produced.map(
    (entry) =>
      ({ type: "insert", outRef: entry.outref, output: entry.output }) as const,
  ),
];

/** What `journal`'s delta gives on the retained ledger at its parent's root. */
type JournalRoot =
  | Readonly<{ kind: "root"; root: string }>
  | Readonly<{ kind: "not_retained" }>
  | Readonly<{ kind: "refused"; detail: string }>;

/**
 * The root `journal`'s delta reaches on the retained ledger at `root`, or
 * why it reaches none: the store does not retain `root`, or the ledger
 * refuses an edit (a delete of an out-ref it does not hold, an insert over
 * one it holds).
 */
const journalRoot = <R>(
  ports: LandedBlockPorts<R>,
  root: string,
  journal: OwnJournal,
) =>
  ports.retainedLedger(root).pipe(
    Effect.flatMap((ledger) =>
      Effect.tryPromise({
        try: () => ledger.apply(journalMutations(journal)),
        catch: (cause) => cause,
      }),
    ),
    Effect.map((after): JournalRoot => ({ kind: "root", root: after.root })),
    Effect.catchAll(
      (error): Effect.Effect<JournalRoot, unknown> =>
        error instanceof LedgerNotRetained
          ? Effect.succeed({ kind: "not_retained" })
          : error instanceof LedgerMutationRefused
            ? Effect.succeed({ kind: "refused", detail: error.message })
            : Effect.fail(error),
    ),
  );

/** A local fault on the view: the next follower change re-reads it. */
const mismatch = (detail: string): DriverHold =>
  notRetried({ reason: LANDED_BLOCK_OWN_JOURNAL_MISMATCH, detail });

/**
 * The hold that stops processing at own block `node` on `parent`, whose
 * journal is `journal`, or `undefined` when processing adopts it.
 */
export const ownBlockHold = <R>(
  ports: LandedBlockPorts<R>,
  node: LandedNode,
  parent: Readonly<{ headerHash: string; utxosRoot: string }>,
  journal: OwnJournal,
) =>
  Effect.gen(function* () {
    if (
      journal.baseTailHeaderHash !== parent.headerHash ||
      journal.baseUtxosRoot !== parent.utxosRoot ||
      journal.expectedUtxosRoot !== node.header.utxosRoot
    )
      return mismatch(
        `own block ${node.headerHash}'s journal does not describe the landed block (base ${journal.baseTailHeaderHash}/${journal.baseUtxosRoot}, expected ${journal.expectedUtxosRoot})`,
      );
    const after = yield* journalRoot(ports, parent.utxosRoot, journal);
    if (after.kind === "not_retained") {
      const detail = `own block ${node.headerHash}: the ledger store does not retain its parent's root ${parent.utxosRoot}`;
      yield* replayOutcomeAlarm(node.headerHash, "local_fault", detail);
      return replayOutcomeHold("local_fault", detail, node.headerHash);
    }
    if (after.kind === "refused")
      return mismatch(
        `own block ${node.headerHash}'s journal delta does not apply to its parent's ledger: ${after.detail}`,
      );
    if (after.root !== node.header.utxosRoot)
      return mismatch(
        `own block ${node.headerHash}'s journal delta reaches root ${after.root} on its parent's ledger, its header commits ${node.header.utxosRoot}`,
      );
    return undefined;
  });
