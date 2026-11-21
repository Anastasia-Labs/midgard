import type { Pool } from "pg";

import type { CommitteePinTargets } from "../l1/follower/retention-pins.js";

/**
 * Every L1 point a committee record stores and reads again (a slot with a
 * block hash): header observations, signature chain points, outbox effect
 * points, capacity evidence points, the retirement floor's points and the
 * L1 source state's observations.
 */
export const STORED_POINTS_SQL = `
SELECT 'state queue header ' || header_hash AS holder,
       (record->'observedChainPoint'->>'slot')::bigint AS slot,
       record->'observedChainPoint'->>'blockHash' AS block_hash
  FROM committee_state_queue_headers
UNION ALL
SELECT 'DA signature ' || header_hash || ':' || signer_index,
       (record->'l1ChainPoint'->>'slot')::bigint,
       record->'l1ChainPoint'->>'blockHash'
  FROM committee_da_signatures
UNION ALL
SELECT 'decision outbox ' || effect_id,
       (record->>'slot')::bigint, record->>'blockHash'
  FROM committee_decision_outbox
UNION ALL
SELECT 'capacity evidence ' || evidence_key,
       (record->'point'->>'slot')::bigint, record->'point'->>'blockHash'
  FROM committee_promise_capacity_evidence
UNION ALL
SELECT 'capacity evidence certification ' || evidence_key,
       (record->'certifiedAt'->>'slot')::bigint, record->'certifiedAt'->>'blockHash'
  FROM committee_promise_capacity_evidence
UNION ALL
SELECT 'retirement floor', (record->'point'->>'slot')::bigint, record->'point'->>'blockHash'
  FROM committee_retirement_metadata
UNION ALL
SELECT 'retirement floor certification', (record->'certifiedAt'->>'slot')::bigint,
       record->'certifiedAt'->>'blockHash'
  FROM committee_retirement_metadata
UNION ALL
SELECT 'retirement checkpoint', (record->'checkpoint'->'point'->>'slot')::bigint,
       record->'checkpoint'->'point'->>'blockHash'
  FROM committee_retirement_metadata
UNION ALL
SELECT 'L1 source state observation ' || (o->>'headerHash'),
       (o->>'slot')::bigint, o->>'blockHash'
  FROM committee_l1_source_state, jsonb_array_elements(record->'observations') o
`;

/**
 * What the committee's stored records read again from L1, as follower
 * retention pin targets: the slot of every stored point, the hash of every
 * L1 submission, and every stored header with the slot its record names.
 * A header's pin keeps its state-queue rows, from which its landing is
 * re-derived and its exit read.
 */
export const readCommitteeL1PinTargets = async (
  pool: Pick<Pool, "query">,
): Promise<CommitteePinTargets> => {
  const points = await pool.query<{ readonly slot: string }>(
    `SELECT DISTINCT slot FROM (${STORED_POINTS_SQL}) p
      WHERE slot IS NOT NULL AND block_hash IS NOT NULL`,
  );
  const submissions = await pool.query<{ readonly tx_hash: string }>(
    "SELECT DISTINCT tx_hash FROM committee_l1_submissions",
  );
  const headers = await pool.query<{
    readonly header_hash: string;
    readonly slot: string | null;
  }>(
    `SELECT header_hash, (record->'observedChainPoint'->>'slot')::bigint AS slot
       FROM committee_state_queue_headers`,
  );
  return {
    blocks: points.rows.map(({ slot }) => Number(slot)),
    txs: submissions.rows.map(({ tx_hash }) => tx_hash),
    headers: headers.rows.map(({ header_hash, slot }) => ({
      headerHash: header_hash,
      slot: slot === null ? 0 : Number(slot),
    })),
  };
};
