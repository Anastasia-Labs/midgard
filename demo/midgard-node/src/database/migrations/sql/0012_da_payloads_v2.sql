DO $$
BEGIN
  IF EXISTS (SELECT 1 FROM pending_block_finalizations)
     OR EXISTS (SELECT 1 FROM da_payloads) THEN
    RAISE EXCEPTION 'Refusing to migrate existing DA payload or pending-finalization rows to V2 automatically; perform an explicit verified reset/redeploy for the new transition trace DA schema.';
  END IF;
END
$$;

ALTER TABLE pending_block_finalizations
  ADD COLUMN header_cbor BYTEA NOT NULL CHECK (octet_length(header_cbor) > 0),
  ADD COLUMN expected_transition_trace_root TEXT NOT NULL CHECK (expected_transition_trace_root ~ '^[0-9a-f]{64}$'),
  ADD COLUMN expected_event_to_step_root TEXT NOT NULL CHECK (expected_event_to_step_root ~ '^[0-9a-f]{64}$'),
  ADD COLUMN expected_withdrawal_count BIGINT NOT NULL CHECK (expected_withdrawal_count >= 0),
  ADD COLUMN expected_forced_transaction_count BIGINT NOT NULL CHECK (expected_forced_transaction_count >= 0),
  ADD COLUMN expected_l2_transaction_count BIGINT NOT NULL CHECK (expected_l2_transaction_count >= 0),
  ADD COLUMN expected_deposit_count BIGINT NOT NULL CHECK (expected_deposit_count >= 0),
  ADD COLUMN expected_total_event_count BIGINT NOT NULL CHECK (expected_total_event_count >= 0),
  ADD COLUMN expected_transition_step_count BIGINT NOT NULL CHECK (expected_transition_step_count >= 0),
  ADD CONSTRAINT pending_block_finalizations_expected_count_sum_check
    CHECK (
      expected_total_event_count =
        expected_withdrawal_count +
        expected_forced_transaction_count +
        expected_l2_transaction_count +
        expected_deposit_count
    ),
  ADD CONSTRAINT pending_block_finalizations_expected_trace_count_check
    CHECK (expected_transition_step_count = expected_total_event_count);

ALTER TABLE da_payloads
  DROP CONSTRAINT IF EXISTS da_payloads_version_check;

ALTER TABLE da_payloads
  ADD COLUMN forced_transactions_root TEXT NOT NULL CHECK (forced_transactions_root ~ '^[0-9a-f]{64}$'),
  ADD COLUMN transition_trace_root TEXT NOT NULL CHECK (transition_trace_root ~ '^[0-9a-f]{64}$'),
  ADD COLUMN event_to_step_root TEXT NOT NULL CHECK (event_to_step_root ~ '^[0-9a-f]{64}$'),
  ADD COLUMN withdrawal_count BIGINT NOT NULL CHECK (withdrawal_count >= 0),
  ADD COLUMN forced_transaction_count BIGINT NOT NULL CHECK (forced_transaction_count >= 0),
  ADD COLUMN l2_transaction_count BIGINT NOT NULL CHECK (l2_transaction_count >= 0),
  ADD COLUMN deposit_count BIGINT NOT NULL CHECK (deposit_count >= 0),
  ADD COLUMN total_event_count BIGINT NOT NULL CHECK (total_event_count >= 0),
  ADD COLUMN transition_step_count BIGINT NOT NULL CHECK (transition_step_count >= 0),
  ADD CONSTRAINT da_payloads_version_v2_check CHECK (version = 2),
  ADD CONSTRAINT da_payloads_count_sum_check
    CHECK (
      total_event_count =
        withdrawal_count +
        forced_transaction_count +
        l2_transaction_count +
        deposit_count
    ),
  ADD CONSTRAINT da_payloads_trace_count_check
    CHECK (transition_step_count = total_event_count);
