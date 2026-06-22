CREATE TABLE da_payloads (
  header_hash BYTEA PRIMARY KEY CHECK (octet_length(header_hash) = 28),
  version INTEGER NOT NULL CHECK (version = 1),
  payload_cbor BYTEA NOT NULL CHECK (octet_length(payload_cbor) > 0),
  payload_sha256 BYTEA NOT NULL CHECK (octet_length(payload_sha256) = 32),
  utxos_root TEXT NOT NULL CHECK (utxos_root ~ '^[0-9a-f]{64}$'),
  transactions_root TEXT NOT NULL CHECK (transactions_root ~ '^[0-9a-f]{64}$'),
  deposits_root TEXT NOT NULL CHECK (deposits_root ~ '^[0-9a-f]{64}$'),
  withdrawals_root TEXT NOT NULL CHECK (withdrawals_root ~ '^[0-9a-f]{64}$'),
  block_start_time TIMESTAMPTZ NOT NULL,
  block_end_time TIMESTAMPTZ NOT NULL CHECK (block_end_time >= block_start_time),
  created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
);

CREATE INDEX idx_da_payloads_created_at
  ON da_payloads (created_at);
