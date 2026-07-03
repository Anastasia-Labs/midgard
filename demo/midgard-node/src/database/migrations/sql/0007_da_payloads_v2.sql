CREATE TABLE da_payloads (
  header_hash BYTEA PRIMARY KEY CHECK (octet_length(header_hash) = 28),
  version INTEGER NOT NULL CONSTRAINT da_payloads_version_v2_check CHECK (version = 2),
  payload_cbor BYTEA NOT NULL CHECK (octet_length(payload_cbor) > 0),
  payload_sha256 BYTEA NOT NULL CHECK (octet_length(payload_sha256) = 32),
  utxos_root TEXT NOT NULL CHECK (utxos_root ~ '^[0-9a-f]{64}$'),
  forced_transactions_root TEXT NOT NULL CHECK (forced_transactions_root ~ '^[0-9a-f]{64}$'),
  transactions_root TEXT NOT NULL CHECK (transactions_root ~ '^[0-9a-f]{64}$'),
  deposits_root TEXT NOT NULL CHECK (deposits_root ~ '^[0-9a-f]{64}$'),
  withdrawals_root TEXT NOT NULL CHECK (withdrawals_root ~ '^[0-9a-f]{64}$'),
  transition_trace_root TEXT NOT NULL CHECK (transition_trace_root ~ '^[0-9a-f]{64}$'),
  event_to_step_root TEXT NOT NULL CHECK (event_to_step_root ~ '^[0-9a-f]{64}$'),
  withdrawal_count BIGINT NOT NULL CHECK (withdrawal_count >= 0),
  forced_transaction_count BIGINT NOT NULL CHECK (forced_transaction_count >= 0),
  l2_transaction_count BIGINT NOT NULL CHECK (l2_transaction_count >= 0),
  deposit_count BIGINT NOT NULL CHECK (deposit_count >= 0),
  total_event_count BIGINT NOT NULL CHECK (total_event_count >= 0),
  transition_step_count BIGINT NOT NULL CHECK (transition_step_count >= 0),
  block_start_time TIMESTAMPTZ NOT NULL,
  block_end_time TIMESTAMPTZ NOT NULL CHECK (block_end_time >= block_start_time),
  created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  CONSTRAINT da_payloads_count_sum_check
    CHECK (
      total_event_count =
        withdrawal_count +
        forced_transaction_count +
        l2_transaction_count +
        deposit_count
    ),
  CONSTRAINT da_payloads_trace_count_check
    CHECK (transition_step_count = total_event_count)
);

CREATE INDEX idx_da_payloads_created_at
  ON da_payloads (created_at);
