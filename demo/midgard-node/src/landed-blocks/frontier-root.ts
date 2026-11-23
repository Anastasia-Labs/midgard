/**
 * The ledger store holds the confirmed-ledger frontier's root, so a new
 * block whose parent is the frontier, with no processed row between them,
 * has a ledger to be checked or replayed on.
 *
 * Retention names the roots of the blocks inside the window, the frontier's
 * and the newest processed one. A block that left the window before it
 * merged may have lost its root by the time a fold makes it the frontier.
 * The frontier's root is then rebuilt from the newest root the store retains
 * on the retained folds to the frontier, by their stored deltas, and
 * persisted. When this node cannot give it, the fold goes on with a warning:
 * the confirmed ledger does not read the store, and a block built on that
 * root then holds its own local fault.
 */
import { SqlClient } from "@effect/sql";
import { Effect } from "effect";

import { sqlErrorToDatabaseError } from "../database/utils/common.js";
import { mergesTableName } from "./confirmed-merges.js";
import { ledgerAt } from "./parent-ledger.js";
import type { LandedBlockPorts } from "./ports.js";
import {
  decodeRow,
  type HeaderRoot,
  type LandedBlockRow,
  type RawRow,
} from "./store.js";

/**
 * The retained folds that lead to `frontier`, oldest first: each fold's
 * header is the next one's parent, and the last is the frontier's.
 */
const foldsTo = (frontier: HeaderRoot) =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const raw = yield* sql<Omit<RawRow, "state">>`
      SELECT header_hash, parent_header_hash, parent_utxos_root, utxos_root,
        kind, applied, spent, produced_outrefs, produced_outputs,
        deposit_ids, withdrawals, forced_ids, tx_ids
      FROM ${sql(mergesTableName)}`;
    const byHeader = new Map(
      raw.map((row) => {
        const fold = decodeRow({ ...row, state: "processed" });
        return [fold.headerHash, fold] as const;
      }),
    );
    const chain: LandedBlockRow[] = [];
    const seen = new Set<string>();
    for (
      let fold = byHeader.get(frontier.headerHash);
      fold !== undefined && !seen.has(fold.headerHash);
      fold = byHeader.get(fold.parentHeaderHash)
    ) {
      seen.add(fold.headerHash);
      chain.unshift(fold);
    }
    return chain;
  }).pipe(
    sqlErrorToDatabaseError(
      mergesTableName,
      "Failed to read the retained folds",
    ),
  );

/** Has the ledger store hold `frontier`'s root, or warns that it cannot. */
export const holdFrontierRoot = <R>(
  ports: LandedBlockPorts<R>,
  frontier: HeaderRoot,
) =>
  Effect.gen(function* () {
    const at = yield* ledgerAt(ports, frontier.utxosRoot, foldsTo(frontier));
    if (at.kind === "rebuilt") yield* ports.persistLedger(at.ledger);
    else if (at.kind === "unavailable")
      yield* Effect.logWarning(
        `the ledger store does not hold the confirmed-ledger frontier ${frontier.headerHash}'s root: ${at.detail}`,
      );
  });
