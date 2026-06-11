CREATE TABLE address_history (
  tx_id BYTEA NOT NULL,
  address TEXT NOT NULL,
  created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  UNIQUE (tx_id, address)
);

CREATE INDEX idx_address_history_created_at
  ON address_history (created_at);

CREATE TABLE blocks (
  height SERIAL PRIMARY KEY,
  header_hash BYTEA NOT NULL,
  tx_id BYTEA NOT NULL UNIQUE,
  time_stamp_tz TIMESTAMPTZ NOT NULL DEFAULT(NOW())
);

CREATE INDEX idx_blocks_header_hash
  ON blocks (header_hash);

CREATE INDEX idx_blocks_tx_id
  ON blocks (tx_id);

CREATE TABLE confirmed_ledger (
  tx_id BYTEA NOT NULL,
  outref BYTEA NOT NULL,
  output BYTEA NOT NULL,
  address TEXT NOT NULL,
  time_stamp_tz TIMESTAMPTZ NOT NULL DEFAULT(NOW()),
  PRIMARY KEY (outref)
);

CREATE INDEX idx_confirmed_ledger_address
  ON confirmed_ledger (address);

CREATE TABLE latest_ledger (
  tx_id BYTEA NOT NULL,
  outref BYTEA NOT NULL,
  output BYTEA NOT NULL,
  address TEXT NOT NULL,
  time_stamp_tz TIMESTAMPTZ NOT NULL DEFAULT(NOW()),
  PRIMARY KEY (outref)
);

CREATE INDEX idx_latest_ledger_address
  ON latest_ledger (address);

CREATE TABLE deposits_utxos (
  event_id BYTEA NOT NULL,
  event_info BYTEA NOT NULL,
  inclusion_time TIMESTAMPTZ NOT NULL,
  deposit_l1_tx_hash BYTEA NOT NULL,
  ledger_tx_id BYTEA NOT NULL,
  ledger_output BYTEA NOT NULL,
  ledger_address TEXT NOT NULL,
  projected_header_hash BYTEA,
  status TEXT NOT NULL,
  PRIMARY KEY (event_id),
  CHECK (status IN ('awaiting', 'projected', 'consumed')),
  CHECK (
    status <> 'awaiting'
    OR projected_header_hash IS NULL
  )
);

CREATE INDEX idx_deposits_utxos_status_inclusion_time_event_id
  ON deposits_utxos (status, inclusion_time, event_id);

CREATE INDEX idx_deposits_utxos_projected_header_hash
  ON deposits_utxos (projected_header_hash);

CREATE INDEX idx_deposits_utxos_deposit_l1_tx_hash
  ON deposits_utxos (deposit_l1_tx_hash);

CREATE TABLE immutable (
  tx_id BYTEA NOT NULL,
  tx BYTEA NOT NULL,
  time_stamp_tz TIMESTAMPTZ NOT NULL DEFAULT(NOW()),
  PRIMARY KEY (tx_id)
);

CREATE INDEX idx_immutable_time_stamp_tz
  ON immutable (time_stamp_tz);

CREATE TABLE mempool (
  tx_id BYTEA NOT NULL,
  tx BYTEA NOT NULL,
  time_stamp_tz TIMESTAMPTZ NOT NULL DEFAULT(NOW()),
  PRIMARY KEY (tx_id)
);

CREATE INDEX idx_mempool_time_stamp_tz
  ON mempool (time_stamp_tz);

CREATE TABLE processed_mempool (
  tx_id BYTEA NOT NULL,
  tx BYTEA NOT NULL,
  time_stamp_tz TIMESTAMPTZ NOT NULL DEFAULT(NOW()),
  PRIMARY KEY (tx_id)
);

CREATE INDEX idx_processed_mempool_time_stamp_tz
  ON processed_mempool (time_stamp_tz);

CREATE TABLE mempool_ledger (
  tx_id BYTEA NOT NULL,
  outref BYTEA NOT NULL,
  output BYTEA NOT NULL,
  address TEXT NOT NULL,
  source_event_id BYTEA,
  time_stamp_tz TIMESTAMPTZ NOT NULL DEFAULT(NOW()),
  PRIMARY KEY (outref),
  FOREIGN KEY (source_event_id)
    REFERENCES deposits_utxos(event_id)
    ON DELETE RESTRICT
);

CREATE INDEX idx_mempool_ledger_address
  ON mempool_ledger (address);

CREATE INDEX idx_mempool_ledger_source_event_id
  ON mempool_ledger (source_event_id);

CREATE UNIQUE INDEX uniq_mempool_ledger_source_event_id
  ON mempool_ledger (source_event_id);

CREATE TABLE mempool_tx_deltas (
  tx_id BYTEA NOT NULL,
  spent_cbor BYTEA NOT NULL,
  produced_cbor BYTEA NOT NULL,
  PRIMARY KEY (tx_id)
);

CREATE TABLE tx_rejections (
  tx_id BYTEA NOT NULL,
  reject_code TEXT NOT NULL,
  reject_detail TEXT,
  created_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
);

CREATE INDEX idx_tx_rejections_tx_id
  ON tx_rejections (tx_id);

CREATE INDEX idx_tx_rejections_created_at
  ON tx_rejections (created_at);

CREATE TABLE deposit_ingestion_cursor (
  cursor_name TEXT PRIMARY KEY,
  stable_tip_hash TEXT NOT NULL,
  stable_tip_slot BIGINT NOT NULL,
  stable_tip_time_ms BIGINT NOT NULL,
  scan_upper_bound_time_ms BIGINT NOT NULL,
  last_scanned_event_id BYTEA NOT NULL,
  updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
);

CREATE TABLE pending_block_finalizations (
  header_hash BYTEA PRIMARY KEY,
  submitted_tx_hash BYTEA UNIQUE,
  block_end_time TIMESTAMPTZ NOT NULL,
  status TEXT NOT NULL,
  observed_confirmed_at_ms BIGINT,
  created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  CHECK (status IN (
    'pending_submission',
    'submitted_local_finalization_pending',
    'submitted_unconfirmed',
    'observed_waiting_stability',
    'finalized',
    'abandoned'
  ))
);

CREATE TABLE pending_block_finalization_deposits (
  header_hash BYTEA NOT NULL REFERENCES pending_block_finalizations(header_hash) ON DELETE CASCADE,
  member_id BYTEA NOT NULL REFERENCES deposits_utxos(event_id) ON DELETE RESTRICT,
  ordinal INTEGER NOT NULL,
  PRIMARY KEY (header_hash, member_id),
  UNIQUE (header_hash, ordinal)
);

CREATE TABLE pending_block_finalization_txs (
  header_hash BYTEA NOT NULL REFERENCES pending_block_finalizations(header_hash) ON DELETE CASCADE,
  member_id BYTEA NOT NULL,
  ordinal INTEGER NOT NULL,
  PRIMARY KEY (header_hash, member_id),
  UNIQUE (header_hash, ordinal)
);

CREATE UNIQUE INDEX uniq_pending_block_finalizations_single_active
  ON pending_block_finalizations ((1))
  WHERE status IN (
    'pending_submission',
    'submitted_local_finalization_pending',
    'submitted_unconfirmed',
    'observed_waiting_stability'
  );

CREATE INDEX idx_pending_block_finalizations_status
  ON pending_block_finalizations (status);
