-- class: B; retention: unchanged (a settlement attempt's row is never deleted: it keeps the signed body and its fee reservation)
-- A settlement attempt's L1 outcome is derived from the intent journal (plan
-- §8.2, §15 N6, D-N4) instead of stored once it reached cd: `confirmed`
-- goes. A pending attempt reads the journal's derived status (landed at
-- depth >= cd lets its job take the next phase, and a rollback reverts
-- that); `final` is written only once it landed more than k blocks deep (by
-- the follower's prune step, in the step that prunes its journal entry).
-- The rollback recovery a stored confirmation needed goes with it: the
-- restored-fee-coin index, the one-pending index (the derived check under
-- the owner-row lock replaces it), the recovery flag and the per-job
-- history-generation recheck.
DROP INDEX settlement_one_pending;
DROP INDEX settlement_confirmed_fee_inputs;
DROP INDEX settlement_jobs_generation;
ALTER TABLE settlement_attempts DROP CONSTRAINT settlement_attempts_status_check;
-- A confirmed attempt the journal still holds is derived from it again: it
-- is not yet final (the journal prunes a landed intent only past k). One the
-- journal does not hold landed past k and was pruned, or predates the
-- journal: it stays terminal, as it was. The follower's tables are installed
-- after the node migrations, so a database that never ran a follower has no
-- journal to read.
DO $$
BEGIN
  IF to_regclass('l1_intents') IS NOT NULL THEN
    EXECUTE $update$UPDATE settlement_attempts a SET status = 'pending'
      WHERE a.status = 'confirmed'
        AND EXISTS (SELECT 1 FROM l1_intents i WHERE i.tx_hash = decode(a.tx_hash, 'hex'))$update$;
  END IF;
END;
$$;
UPDATE settlement_attempts SET status = 'final' WHERE status = 'confirmed';
ALTER TABLE settlement_attempts ADD CONSTRAINT settlement_attempts_status_check
  CHECK (status IN ('pending', 'final', 'expired'));
ALTER TABLE settlement_attempts DROP COLUMN recovery;
ALTER TABLE settlement_jobs DROP COLUMN verified_generation;
CREATE INDEX settlement_attempts_open ON settlement_attempts (deployment_id, created_at, tx_hash) WHERE status = 'pending';
