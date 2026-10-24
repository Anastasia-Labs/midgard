-- class: B; retention: unchanged (one column on `pending_block_finalizations` and `da_payloads`, kept and deleted with the row)
-- A block's time (its state-queue node's `block_time`: the commit
-- transaction's inclusive validity upper bound) beside its header end. Every
-- clock that starts at the commit (maturity, the DA attestation deadline,
-- retention) counts from it; the header window is coverage only. A normal
-- block ends at its block time; a catch-up block ends earlier, on its last
-- event instant. Rows written before this migration were normal blocks, so
-- their block time is their end.
ALTER TABLE pending_block_finalizations ADD COLUMN block_time timestamp with time zone;
UPDATE pending_block_finalizations SET block_time = block_end_time;
ALTER TABLE pending_block_finalizations
  ALTER COLUMN block_time SET NOT NULL,
  ADD CONSTRAINT pending_block_finalizations_block_time_covers_end CHECK (
    block_end_time <= block_time);

ALTER TABLE da_payloads ADD COLUMN block_time timestamp with time zone;
UPDATE da_payloads SET block_time = block_end_time;
ALTER TABLE da_payloads
  ALTER COLUMN block_time SET NOT NULL,
  ADD CONSTRAINT da_payloads_block_time_covers_end CHECK (
    block_end_time <= block_time);
