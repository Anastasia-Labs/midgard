-- class: B; retention: unchanged (the own-block journal, `pending_block_finalizations`, loses its stored status; `local_mutation_jobs` goes)
-- An own block's status is derived from the facts (plan §8.2, §8.3, #753):
-- unlanded, landed, applied or folded, from the landed rows, the confirmed
-- ledger's folds and frontier and the queue terminals. No journal stores a
-- copy, so the status column goes, with the columns only the status machine
-- wrote: the submitted hash and the signed bytes (the intent journal's
-- `l1_intents` row holds them; `intended_tx_hash` is the link) and the
-- correction digest. `observed_confirmed_at_ms` stays: the rebase that takes
-- an own block's landing in records when it did (`own-blocks.ts`), so the
-- throughput metrics read a landed time that outlives the landed row's fold.
-- Block confirmation's local-finalization jobs go with `local_mutation_jobs`:
-- landing applies an own block, and the fold is derived.
--
-- The migration refuses a database it cannot carry over:
--
-- - MIGRATION_0037_SIGNED_JOURNAL_WITHOUT_INTENT: a journal in an active
--   status (pending submission with a signed intent, submitted, observed
--   waiting stability) whose signed commit the intent journal does not hold
--   with these exact bytes, or that records a submitted hash with no
--   intended one. Its commit may still be on its way to L1, and once the
--   signed bytes go, only the intent journal can say what became of it.
--   Locally applied and abandoned journals are exempt: nothing reads their
--   signed bytes again, and the follower prunes an intent row once it has
--   been terminal for k blocks, so a long-lived database holds such journals
--   with no intent row by design. An abandoned journal is not necessarily
--   dead once derived: one that is unlanded and whose block can still land
--   (its commit's intent not dead, its base still on the landed chain, see
--   `deadJournals`) is the live journal until its commit settles, which
--   derives it landed or dead.
-- - MIGRATION_0037_UNRECOVERABLE_LOCAL_FINALIZATION: an unfinished (running
--   or failed) local-finalization job that startup would have refused: its
--   id names no header, its journal is gone, or its journal was never
--   submitted or was abandoned.
-- - MIGRATION_0037_APPLIED_JOURNAL_UNDERIVABLE: a locally applied journal
--   that would derive neither applied nor folded. One whose landed row a
--   rollback removed derives unlanded, as it should, and is exempt.
-- - MIGRATION_0037_SUBMITTED_COMMIT_NOT_PREPARED: a journal that records a
--   submitted hash, no intended one, and a prepared hash other than the
--   submitted one. The submitted hash becomes its intended one below, which
--   must be the prepared one.
--
-- The follower's tables (`l1_intents`, `node_l1_queue_terminals`) are
-- installed after the node migrations, so a database that never ran a
-- follower has none to read.
DO $own_block_status$
DECLARE
    offending bytea;
    job text;
BEGIN
    SELECT p.header_hash INTO offending
    FROM pending_block_finalizations p
    WHERE p.status IN ('pending_submission',
        'submitted_local_finalization_pending',
        'submitted_unconfirmed', 'observed_waiting_stability')
      AND p.intended_tx_hash IS NULL AND p.submitted_tx_hash IS NOT NULL
    ORDER BY p.created_at, p.header_hash
    LIMIT 1;
    IF offending IS NULL THEN
        IF to_regclass('l1_intents') IS NULL THEN
            SELECT p.header_hash INTO offending
            FROM pending_block_finalizations p
            WHERE p.status IN ('pending_submission',
                'submitted_local_finalization_pending',
                'submitted_unconfirmed', 'observed_waiting_stability')
              AND p.intended_tx_hash IS NOT NULL
            ORDER BY p.created_at, p.header_hash
            LIMIT 1;
        ELSE
            EXECUTE $signed$SELECT p.header_hash
                FROM pending_block_finalizations p
                WHERE p.status IN ('pending_submission',
                    'submitted_local_finalization_pending',
                    'submitted_unconfirmed', 'observed_waiting_stability')
                  AND p.intended_tx_hash IS NOT NULL
                  AND NOT EXISTS (SELECT 1 FROM l1_intents intent
                    WHERE intent.tx_hash = p.intended_tx_hash
                      AND intent.tx_cbor = p.signed_tx_cbor)
                ORDER BY p.created_at, p.header_hash
                LIMIT 1$signed$ INTO offending;
        END IF;
    END IF;
    IF offending IS NOT NULL THEN
        RAISE EXCEPTION 'MIGRATION_0037_SIGNED_JOURNAL_WITHOUT_INTENT: journal % records a signed or submitted commit the intent journal does not hold with its exact bytes', encode(offending, 'hex')
            USING HINT = 'Run the release before this one until the commit lands or its intent is dead, then migrate again.';
    END IF;

    SELECT j.job_id INTO job
    FROM local_mutation_jobs j
    LEFT JOIN pending_block_finalizations p
      ON p.header_hash = CASE
        WHEN j.job_id ~ '^local_block_finalization:[0-9a-f]{56}$'
        THEN decode(substring(j.job_id FROM 26), 'hex') END
    WHERE j.kind = 'local_block_finalization'
      AND j.status IN ('running', 'failed')
      AND (p.header_hash IS NULL
        OR p.status NOT IN ('submitted_local_finalization_pending',
          'submitted_unconfirmed', 'observed_waiting_stability',
          'locally_applied'))
    ORDER BY j.job_id
    LIMIT 1;
    IF job IS NOT NULL THEN
        RAISE EXCEPTION 'MIGRATION_0037_UNRECOVERABLE_LOCAL_FINALIZATION: unfinished local mutation job % has no submitted journal to carry over', job
            USING HINT = 'Recover the job with the release before this one, then migrate again.';
    END IF;

    EXECUTE format($applied$SELECT p.header_hash
        FROM pending_block_finalizations p
        WHERE p.status = 'locally_applied'
          AND NOT EXISTS (SELECT 1 FROM node_landed_blocks landed
            WHERE landed.header_hash = p.header_hash
              AND (landed.state = 'removed'
                OR (landed.state = 'processed' AND landed.applied)))
          AND NOT EXISTS (SELECT 1 FROM node_confirmed_ledger_frontier frontier
            WHERE frontier.header_hash = p.header_hash)
          AND NOT EXISTS (SELECT 1 FROM node_confirmed_merges merged
            WHERE merged.header_hash = p.header_hash)
          AND NOT (NOT EXISTS (SELECT 1 FROM node_landed_blocks landed
              WHERE landed.header_hash = p.header_hash)
            AND %s)
        ORDER BY p.created_at, p.header_hash
        LIMIT 1$applied$,
        CASE WHEN to_regclass('node_l1_queue_terminals') IS NULL THEN 'FALSE'
        ELSE $merged$COALESCE((SELECT terminal.terminal_outcome = 'merged'
            FROM node_l1_queue_terminals terminal
            WHERE terminal.header_hash = p.header_hash
            ORDER BY terminal.height DESC, terminal.tx_index DESC
            LIMIT 1), FALSE)$merged$ END) INTO offending;
    IF offending IS NOT NULL THEN
        RAISE EXCEPTION 'MIGRATION_0037_APPLIED_JOURNAL_UNDERIVABLE: locally applied journal % is neither applied by a landed row nor folded', encode(offending, 'hex')
            USING HINT = 'Let the release before this one process the landed chain past the block, then migrate again.';
    END IF;

    SELECT p.header_hash INTO offending
    FROM pending_block_finalizations p
    WHERE p.intended_tx_hash IS NULL AND p.submitted_tx_hash IS NOT NULL
      AND p.prepared_tx_hash IS NOT NULL
      AND p.prepared_tx_hash <> p.submitted_tx_hash
    ORDER BY p.created_at, p.header_hash
    LIMIT 1;
    IF offending IS NOT NULL THEN
        RAISE EXCEPTION 'MIGRATION_0037_SUBMITTED_COMMIT_NOT_PREPARED: journal % records a submitted commit other than the one it prepared, and no intended one', encode(offending, 'hex')
            USING HINT = 'The migration cannot tell which of the two commits the journal signed; resolve the journal with the release before this one, then migrate again.';
    END IF;
END;
$own_block_status$;

-- An abandoned journal whose commit was never signed or submitted was never
-- broadcast. Without a status it would read as an unlanded journal, so it
-- goes, with its members, unless a landed row, fold or the frontier names
-- its header.
DELETE FROM pending_block_finalizations p
  WHERE p.status = 'abandoned'
    AND p.intended_tx_hash IS NULL
    AND p.submitted_tx_hash IS NULL
    AND NOT EXISTS (SELECT 1 FROM node_landed_blocks landed
      WHERE landed.header_hash = p.header_hash)
    AND NOT EXISTS (SELECT 1 FROM node_confirmed_merges merged
      WHERE merged.header_hash = p.header_hash)
    AND NOT EXISTS (SELECT 1 FROM node_confirmed_ledger_frontier frontier
      WHERE frontier.header_hash = p.header_hash);

-- A journal whose commit was submitted unjournaled (the fixture mode's
-- submission with no intended hash) records only the submitted hash: an
-- abandoned journal, or a locally applied one, whatever it derives (an
-- active one was refused above). Once that column goes it would read as
-- never signed, and the next commit tick deletes every unsigned journal
-- (`discardStoppedUnsignedJournals`), whatever it derives and before any
-- disposal effect of it is undone: an applied or folded journal would go
-- with the block it describes. Its submitted hash becomes its intended one,
-- so it derives as the signed journal it is; an unlanded one is dead once
-- its header is no live queue node (no intent row holds its commit), and
-- pruned once no disposal effect is left. Such a journal may have no
-- prepared hash, so the prepared hash is checked only where there is one
-- (one naming another commit was refused above); every journal the node
-- signs now has one (`recordSignedIntent` signs only the prepared hash).
ALTER TABLE pending_block_finalizations
  DROP CONSTRAINT pending_signed_intent_pair,
  DROP CONSTRAINT pending_signed_ack_matches_intent,
  DROP CONSTRAINT pending_signed_intent_matches_prepared,
  ADD CONSTRAINT pending_signed_intent_matches_prepared CHECK (
    intended_tx_hash IS NULL OR prepared_tx_hash IS NULL
      OR intended_tx_hash = prepared_tx_hash);
UPDATE pending_block_finalizations
  SET intended_tx_hash = submitted_tx_hash
  WHERE intended_tx_hash IS NULL AND submitted_tx_hash IS NOT NULL;

-- One live journal at a time is held by row locks taken before the prepare's
-- live check, not by an index over a stored status: in production the
-- follower write gate row (`withFollowerWrite`), in the fixture mode the
-- state-queue mutation lease row (`holdMutationLease`), both FOR UPDATE.
DROP INDEX uniq_pending_block_finalizations_single_active;
DROP INDEX idx_pending_block_finalizations_status;
ALTER TABLE pending_block_finalizations
  DROP CONSTRAINT pending_block_finalizations_submitted_tx_hash_key,
  DROP CONSTRAINT pending_block_finalizations_status_check,
  DROP COLUMN status,
  DROP COLUMN submitted_tx_hash,
  DROP COLUMN signed_tx_cbor,
  DROP COLUMN correction_transition_digest;
DROP INDEX idx_local_mutation_jobs_status_updated;
DROP TABLE local_mutation_jobs;
