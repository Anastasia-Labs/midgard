-- class: D-x; retention: unchanged (`node_landed_blocks`)
-- An own block is applied locally by the working-ledger rebase that takes
-- it in (plan §8.3), as a foreign block is. Before that, block confirmation
-- applied it and its landed row was stored applied with no withdrawals. A
-- processed own row whose journal it had not yet applied (neither locally
-- applied nor abandoned) goes back to unapplied, with its journal's
-- withdrawals, so the next rebase applies it and marks its journal.
UPDATE node_landed_blocks l
  SET applied = false,
    withdrawals = COALESCE((
      SELECT jsonb_agg(jsonb_build_object(
          'id', encode(w.member_id, 'hex'),
          'validity', w.validity,
          'detail', w.validity_detail,
          'settlement', encode(w.payload_cbor, 'hex'))
        ORDER BY w.ordinal)
      FROM pending_block_finalization_withdrawals w
      WHERE w.header_hash = l.header_hash), '[]'::jsonb)
  WHERE l.kind = 'own'
    AND l.state = 'processed'
    AND l.applied
    AND EXISTS (SELECT 1 FROM pending_block_finalizations j
      WHERE j.header_hash = l.header_hash
        AND j.status NOT IN ('locally_applied', 'abandoned'));
-- A removed own row whose journal it had not yet applied: a rollback took
-- the block off the landed chain before it was applied here, so there is
-- nothing to revert. It goes, with any marks its block set, as a rollback
-- takes an unapplied row (an own row records no import checks): landed
-- again, the block is processed as a fresh own landing; if not, its
-- journal waits as a commit that has not landed does, until the rebase
-- disposes of it.
WITH removed AS (
  DELETE FROM node_landed_blocks l
    WHERE l.kind = 'own'
      AND l.state = 'removed'
      AND l.applied
      AND EXISTS (SELECT 1 FROM pending_block_finalizations j
        WHERE j.header_hash = l.header_hash
          AND j.status NOT IN ('locally_applied', 'abandoned'))
    RETURNING l.header_hash),
  mempool_marks AS (
    UPDATE mempool SET included_by = NULL
      WHERE included_by IN (SELECT header_hash FROM removed))
UPDATE processed_mempool SET included_by = NULL
  WHERE included_by IN (SELECT header_hash FROM removed);
