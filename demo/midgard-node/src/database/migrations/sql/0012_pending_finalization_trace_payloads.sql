DO $$
BEGIN
  IF EXISTS (SELECT 1 FROM pending_block_finalizations) THEN
    RAISE EXCEPTION 'Refusing to add transition-trace pending-finalization columns with existing pending-finalization rows; perform an explicit verified reset/redeploy for the new transition trace DA schema.';
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
