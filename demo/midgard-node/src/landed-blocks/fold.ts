/**
 * `confirmed_ledger` follows the merged queue root (plan §7.3, N3): every
 * processed block the root has passed is folded in, in queue order, and the
 * frontier names the header `confirmed_ledger` is at.
 *
 * - A foreign block folds its own net delta, once the working-ledger rebase
 *   applied it (its events' rows are then projected to it), and its events
 *   become terminal.
 * - This node's own block folds through its local merge finalization (the
 *   journal's delta, under its merge job), which the merge fiber may also run
 *   first; either way the job completes before the frontier moves.
 * - A root the frontier cannot reach (a merged block the node never
 *   processed, or a merge a rollback undid) holds `confirmed_ledger_behind`
 *   with the process up. Only a `confirmed_ledger` already at the root
 *   re-anchors the frontier; nothing is ever inverted here.
 */
import * as SDK from "@al-ft/midgard-sdk";
import { SqlClient } from "@effect/sql";
import { Effect } from "effect";

import {
  ConfirmedLedgerDB,
  DepositsDB,
  ForcedTransactionsDB,
  WithdrawalsDB,
} from "../database/index.js";
import { DatabaseError } from "../database/utils/common.js";
import type { DriverHold } from "../l1-events/driver.js";
import { computeLedgerMpfRootFromLedgerEntries } from "../mpf/ledger-hydration.js";
import { CONFIRMED_LEDGER_BEHIND } from "./holds.js";
import { depositOutputs, ledgerRows, processedChain } from "./ledger.js";
import type { LandedBlockPorts } from "./ports.js";
import {
  deleteRows,
  Frontier,
  frontierTableName,
  type HeaderRoot,
  type LandedBlockRow,
  retrieveRows,
} from "./store.js";

const behind = (detail: string): DriverHold => ({
  reason: CONFIRMED_LEDGER_BEHIND,
  detail,
});

const confirmedRoot = Effect.gen(function* () {
  const entries = yield* ConfirmedLedgerDB.retrieve;
  return {
    entries,
    root: yield* computeLedgerMpfRootFromLedgerEntries(entries),
  };
});

/** Locks the frontier row and fails if another writer moved it. */
const lockFrontier = (expected: HeaderRoot) =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const [row] = yield* sql<{ header_hash: Buffer }>`
      SELECT header_hash FROM node_confirmed_ledger_frontier FOR UPDATE`;
    if (
      row === undefined ||
      Buffer.from(row.header_hash).toString("hex") !== expected.headerHash
    )
      return yield* Effect.fail(
        new DatabaseError({
          table: frontierTableName,
          message: "The confirmed-ledger frontier moved during a fold",
          cause: expected.headerHash,
        }),
      );
    yield* sql`LOCK TABLE confirmed_ledger IN EXCLUSIVE MODE`;
  });

/**
 * Sets the frontier when there is none: at the merged root if
 * `confirmed_ledger` is there already, or at genesis with the configured
 * genesis ledger written in.
 */
export const bootstrapFrontier = <R>(
  ports: LandedBlockPorts<R>,
  root: HeaderRoot,
) =>
  Effect.gen(function* () {
    if ((yield* Frontier.retrieve) !== undefined) return undefined;
    const confirmed = yield* confirmedRoot;
    if (confirmed.root === root.utxosRoot) {
      yield* ports.write(Frontier.upsert(root));
      return undefined;
    }
    if (
      root.headerHash === SDK.GENESIS_HEADER_HASH &&
      confirmed.entries.length === 0
    ) {
      const genesis = yield* ports.genesis;
      if (
        (yield* computeLedgerMpfRootFromLedgerEntries(genesis)) ===
        root.utxosRoot
      ) {
        yield* ports.write(
          Effect.gen(function* () {
            yield* ConfirmedLedgerDB.insertMultiple([...genesis]);
            yield* Frontier.upsert(root);
          }),
        );
        return undefined;
      }
    }
    return behind(
      `confirmed_ledger (root ${confirmed.root}) is not at the merged queue root ${root.headerHash} (${root.utxosRoot}) and no processed block reaches it`,
    );
  });

/** Folds a foreign block the rebase applied: its delta and terminal events. */
const foldForeign = (frontier: HeaderRoot, row: LandedBlockRow) =>
  Effect.gen(function* () {
    yield* lockFrontier(frontier);
    const header = Buffer.from(row.headerHash, "hex");
    const outRefs = [
      ...row.spent,
      ...row.produced.map((entry) => entry.outref),
    ];
    if (outRefs.length > 0) yield* ConfirmedLedgerDB.clearUTxOs(outRefs);
    const deposits = yield* depositOutputs(row.depositIds);
    const rows = yield* ledgerRows(
      row.produced,
      new Map(
        [...deposits].map(([outRef, { txId }]) => [outRef, txId] as const),
      ),
    );
    if (rows.length > 0) yield* ConfirmedLedgerDB.insertMultiple(rows);
    yield* DepositsDB.markConsumedByEventIds(row.depositIds);
    yield* WithdrawalsDB.markFinalizedByEventIds(
      row.withdrawals.map((member) => Buffer.from(member.id, "hex")),
      header,
    );
    yield* ForcedTransactionsDB.markFinalizedByEventIds(row.forcedIds, header);
    yield* Frontier.upsert(row);
    yield* deleteRows([row.headerHash]);
  });

/** Moves the frontier past an own block whose merge finalization completed. */
const passOwn = (frontier: HeaderRoot, row: LandedBlockRow) =>
  Effect.gen(function* () {
    yield* lockFrontier(frontier);
    yield* Frontier.upsert(row);
    yield* deleteRows([row.headerHash]);
  });

export type FoldOutcome =
  | Readonly<{ kind: "at_root" }>
  /** The root is not on the processed chain from the frontier. */
  | Readonly<{ kind: "off_chain"; frontier: HeaderRoot }>
  /** A foreign block before the root waits for the rebase to apply it. */
  | Readonly<{ kind: "awaiting_rebase" }>
  | Readonly<{ kind: "held"; hold: DriverHold }>;

/** Folds every processed block up to the merged root `root`, in order. */
export const foldToRoot = <R>(ports: LandedBlockPorts<R>, root: HeaderRoot) =>
  Effect.gen(function* () {
    for (;;) {
      const frontier = yield* Frontier.retrieve;
      if (frontier === undefined)
        return {
          kind: "held",
          hold: behind("confirmed_ledger has no frontier yet"),
        } satisfies FoldOutcome;
      if (frontier.headerHash === root.headerHash) {
        if (frontier.utxosRoot !== root.utxosRoot)
          return {
            kind: "held",
            hold: behind(
              `the confirmed-ledger frontier ${frontier.headerHash} has root ${frontier.utxosRoot}, the queue root ${root.utxosRoot}`,
            ),
          } satisfies FoldOutcome;
        return { kind: "at_root" } satisfies FoldOutcome;
      }
      const chain = processedChain(yield* retrieveRows, frontier.headerHash);
      if (!chain.some((row) => row.headerHash === root.headerHash))
        return { kind: "off_chain", frontier } satisfies FoldOutcome;
      const next = chain[0]!;
      if (next.kind === "foreign") {
        if (!next.applied)
          return { kind: "awaiting_rebase" } satisfies FoldOutcome;
        yield* ports.write(foldForeign(frontier, next));
        continue;
      }
      if (yield* ports.ownMergeCompleted(next.headerHash)) {
        yield* ports.write(passOwn(frontier, next));
        continue;
      }
      const journal = yield* ports.ownJournal(next.headerHash);
      if (journal?.status !== "locally_applied")
        return {
          kind: "held",
          hold: behind(
            `own merged block ${next.headerHash} is not locally applied yet (journal ${journal?.status ?? "missing"})`,
          ),
        } satisfies FoldOutcome;
      const finalized = yield* Effect.either(
        ports.finalizeOwnMerge({
          headerHash: Buffer.from(next.headerHash, "hex"),
          headerUtxosRoot: next.utxosRoot,
        }),
      );
      if (finalized._tag === "Left")
        return {
          kind: "held",
          hold: behind(
            `own merged block ${next.headerHash} failed its local merge finalization: ${String(finalized.left)}`,
          ),
        } satisfies FoldOutcome;
      if (!(yield* ports.ownMergeCompleted(next.headerHash)))
        return {
          kind: "held",
          hold: behind(
            `own merged block ${next.headerHash} finalized without completing its merge job`,
          ),
        } satisfies FoldOutcome;
    }
  });

/**
 * Re-anchors the frontier at the merged root when `confirmed_ledger` is
 * already there (the root passed blocks this node never processed, and the
 * local merge finalization caught up); otherwise holds.
 */
export const reanchorFrontier = <R>(
  ports: LandedBlockPorts<R>,
  frontier: HeaderRoot,
  root: HeaderRoot,
) =>
  Effect.gen(function* () {
    const confirmed = yield* confirmedRoot;
    if (confirmed.root !== root.utxosRoot)
      return behind(
        `the merged queue root ${root.headerHash} is not reachable from the confirmed-ledger frontier ${frontier.headerHash} (a merge this node did not process, or one a rollback undid)`,
      );
    yield* ports.write(
      Effect.gen(function* () {
        yield* lockFrontier(frontier);
        yield* Frontier.upsert(root);
      }),
    );
    return undefined;
  });
