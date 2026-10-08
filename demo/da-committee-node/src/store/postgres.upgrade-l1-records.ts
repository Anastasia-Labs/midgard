import type { Pool } from "pg";

import { parseDecisionOutboxRecord } from "../store.parse-decision-outbox-record.js";
import { parseL1SourceState } from "../store.parse-l1-source-state.js";
import { decodeRecord } from "./postgres.assert-postgres-decision-retry.js";
import type { PostgresStoreInstanceLock } from "./postgres.instance-lock.js";
import { fencedOpenTransaction } from "./postgres.open-checks.js";

/**
 * The readiness reason of a store holding a record this build cannot read
 * and the open upgrade cannot repair. The open throws it, so the node's
 * startup retry reports it on `/readyz`, the process up.
 */
export const COMMITTEE_STORE_RECORD_UNREADABLE =
  "committee_store_record_unreadable";

/**
 * Brings the committee's L1 records a store from before the follower wrote
 * (C1) to this build's format, at open, after the schema upgrade stripped
 * the quarantine fields from the decision outbox:
 *
 * - The L1 source state (class A, overwritten on every healthy tick) is
 *   deleted when it does not parse: a healthy row carrying the replay
 *   anchor, a quarantined row, an external-provider row, or an observation
 *   with an unknown status. The next healthy tick rebuilds it from the
 *   follower, which re-derives what the old source tracked.
 * - A pending decision outbox record (class B) written under the removed
 *   external-provider source mode is rewritten to `local_node`: the
 *   effect's execution-time checks gate it against the current source state
 *   and the member's own signature row, as they gate any pending row. Only
 *   a rewritten record that still does not parse refuses the open, with
 *   `committee_store_record_unreadable` naming it, and nothing is changed.
 *   Terminal records (published, reconciled, failed) are left as they are
 *   and never read here: the pending-record index serves the statement.
 *
 * The caller holds the store's instance lock; each write runs on a
 * connection the server confirms still holds it.
 */
export const upgradeCommitteeL1Records = async (
  pool: Pool,
  lock: Pick<PostgresStoreInstanceLock, "assertHeldAtServer">,
  write: (line: string) => void = (line) => process.stderr.write(line),
): Promise<void> => {
  const state = await pool.query<{ readonly record: unknown }>(
    "SELECT record FROM committee_l1_source_state WHERE id = 1",
  );
  const stored = state.rows[0];
  if (stored !== undefined) {
    try {
      parseL1SourceState(decodeRecord<unknown>(stored.record));
    } catch (error) {
      await fencedOpenTransaction(pool, lock, (client) =>
        client.query("DELETE FROM committee_l1_source_state WHERE id = 1"),
      );
      write(
        `${JSON.stringify({
          event: "committee_l1_source_state_discarded",
          detail: error instanceof Error ? error.message : String(error),
        })}\n`,
      );
    }
  }
  await fencedOpenTransaction(pool, lock, async (client) => {
    const rewritten = await client.query<{
      readonly effect_id: string;
      readonly record: unknown;
    }>(
      `UPDATE committee_decision_outbox
          SET record = jsonb_set(record, '{sourceMode}', '"local_node"')
        WHERE record->>'status' = 'pending'
          AND record->>'sourceMode' IS DISTINCT FROM 'local_node'
        RETURNING effect_id, record`,
    );
    const unreadable = rewritten.rows
      .filter(({ record }) => {
        try {
          parseDecisionOutboxRecord(decodeRecord<unknown>(record));
          return false;
        } catch {
          return true;
        }
      })
      .map(({ effect_id }) => effect_id)
      .sort();
    if (unreadable.length > 0)
      throw new Error(
        `${COMMITTEE_STORE_RECORD_UNREADABLE}: ${unreadable.length.toString()} pending decision outbox record(s) do not parse (first ${unreadable[0]!}); resolve or delete them`,
      );
    if (rewritten.rows.length > 0)
      write(
        `${JSON.stringify({
          event: "committee_decision_outbox_source_mode_rewritten",
          effects: rewritten.rows.map(({ effect_id }) => effect_id).sort(),
        })}\n`,
      );
  });
};
