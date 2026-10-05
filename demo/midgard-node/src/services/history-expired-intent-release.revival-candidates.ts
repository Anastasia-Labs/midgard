import { SqlClient } from "@effect/sql";
import type { PgClient } from "@effect/sql-pg/PgClient";
import { Effect, Option } from "effect";

import * as Pending from "../database/pendingBlockFinalizations.js";
import { journalAbandonment } from "./canonical-journal-recovery.js";
import { C } from "./history-expired-intent-release.table.js";
import {
  loadStateQueueCorrectionObserverState,
  type StateQueueCorrectionRewindAuthority,
} from "./state-queue-correction-rewind.js";

/** This node's replacement-abandoned journals whose blocks the correction
 * observer's persisted view shows on a state queue: its cursor queue, a queue
 * a recorded transition saw, or a block a recorded merge folded. A block a
 * recorded correction removed is excluded: its members stay reopened, which
 * is what that correction's path does anyway. A hint only: it is not bound to
 * the checkpoint, so it only selects which blocks the checkpoint-bound
 * evidence is read for. Sorted by header. */
export const revivalCandidates = (
  authority: StateQueueCorrectionRewindAuthority,
  lock = false,
) =>
  Effect.gen(function* () {
    const observer = yield* loadStateQueueCorrectionObserverState(
      authority,
      lock,
    );
    if (observer.kind !== "observed") return [];
    const { cursorQueue, pending, admitted } = observer.state;
    const transitions = [...pending, ...admitted];
    const corrected = new Set(
      transitions
        .filter((transition) => transition.transitionKind !== "merge")
        .flatMap((transition) => transition.removedHeaderHashes),
    );
    const seen = new Set<string>();
    for (const node of [
      ...cursorQueue,
      ...transitions.flatMap((transition) => [
        ...transition.previousQueue,
        ...transition.nextQueue,
      ]),
    ])
      if (node.headerHash !== null) seen.add(node.headerHash);
    for (const transition of transitions)
      if (transition.transitionKind === "merge")
        for (const hash of transition.removedHeaderHashes) seen.add(hash);
    const hashes = [...seen].filter((hash) => !corrected.has(hash)).sort();
    if (hashes.length === 0) return [];
    const sql = yield* SqlClient.SqlClient;
    const pg = sql as PgClient;
    const rows = yield* sql<{ header_hash: Buffer }>`SELECT header_hash
      FROM pending_block_finalizations
      WHERE status = ${Pending.Status.Abandoned}
        AND header_hash = ANY(${pg.array(hashes.map((hash) => `\\x${hash}`))}::bytea[])
      ORDER BY header_hash`;
    const records: Pending.Record[] = [];
    for (const row of rows) {
      const found = yield* Pending.retrieveByHeaderHash(row.header_hash);
      if (
        Option.isSome(found) &&
        journalAbandonment(found.value) === "replacement"
      )
        records.push(found.value);
    }
    return records;
  });

export const revivalKey = (
  candidates: readonly Pending.Record[],
  point: { readonly id: string },
) =>
  `${candidates.map((record) => record[C.HEADER_HASH].toString("hex")).join(",")}@${point.id}`;
