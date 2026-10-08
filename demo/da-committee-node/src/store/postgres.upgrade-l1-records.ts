import type { Pool } from "pg";

import { parseL1SourceState } from "../store.parse-l1-source-state.js";
import { decodeRecord } from "./postgres.assert-postgres-decision-retry.js";

/**
 * The readiness reason of a store holding a record this build cannot read
 * and the open upgrade cannot repair. The open throws it, so the node's
 * startup retry reports it on `/readyz`, the process up, until an operator
 * resolves the rows it names.
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
 * - A decision outbox record (class B) is kept. One written under the
 *   removed external-provider source mode, the only difference left between
 *   the formats, cannot be read; the open refuses with
 *   `committee_store_record_unreadable`, naming it, and never deletes it.
 *
 * The caller holds the store's instance lock, so no other writer races
 * these statements.
 */
export const upgradeCommitteeL1Records = async (
  pool: Pool,
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
      await pool.query("DELETE FROM committee_l1_source_state WHERE id = 1");
      write(
        `${JSON.stringify({
          event: "committee_l1_source_state_discarded",
          detail: error instanceof Error ? error.message : String(error),
        })}\n`,
      );
    }
  }
  const outbox = await pool.query<{
    readonly count: string;
    readonly first: string | null;
  }>(
    `SELECT count(*) AS count, min(effect_id) AS first
       FROM committee_decision_outbox
      WHERE record->>'sourceMode' IS DISTINCT FROM 'local_node'`,
  );
  const unreadable = outbox.rows[0];
  if (unreadable !== undefined && unreadable.count !== "0")
    throw new Error(
      `${COMMITTEE_STORE_RECORD_UNREADABLE}: ${unreadable.count} decision outbox record(s) were written under a source mode other than local_node (first ${unreadable.first ?? "?"}); resolve or delete them`,
    );
};
