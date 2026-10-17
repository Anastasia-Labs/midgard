-- class: B; retention: unchanged (the own-block journal, `pending_block_finalizations`)
-- The own-block journal's terminal status `finalized` is renamed
-- `locally_applied` (plan §13.1, I1-E1 option B): the node applied the block
-- locally; whether it is final on L1 is derived from the follower's facts,
-- never stored. Existing rows are renamed in place.
ALTER TABLE pending_block_finalizations
  DROP CONSTRAINT pending_block_finalizations_status_check;
UPDATE pending_block_finalizations
  SET status = 'locally_applied'
  WHERE status = 'finalized';
ALTER TABLE pending_block_finalizations
  ADD CONSTRAINT pending_block_finalizations_status_check CHECK (status = ANY (ARRAY['pending_submission'::text, 'submitted_local_finalization_pending'::text, 'submitted_unconfirmed'::text, 'observed_waiting_stability'::text, 'locally_applied'::text, 'abandoned'::text]));
