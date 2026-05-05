CREATE TABLE withdrawal_utxos (
  event_id BYTEA PRIMARY KEY,
  raw_event_info BYTEA NOT NULL,
  settlement_event_info BYTEA,
  inclusion_time TIMESTAMPTZ NOT NULL,
  withdrawal_l1_tx_hash BYTEA NOT NULL CHECK (octet_length(withdrawal_l1_tx_hash) = 32),
  withdrawal_l1_output_index INTEGER NOT NULL CHECK (withdrawal_l1_output_index >= 0),
  asset_name BYTEA NOT NULL CHECK (octet_length(asset_name) BETWEEN 1 AND 32),
  l2_outref BYTEA NOT NULL,
  l2_owner BYTEA NOT NULL CHECK (octet_length(l2_owner) = 28),
  l2_value BYTEA NOT NULL,
  l1_address BYTEA NOT NULL,
  l1_datum BYTEA NOT NULL,
  refund_address BYTEA NOT NULL,
  refund_datum BYTEA NOT NULL,
  validity TEXT,
  validity_detail JSONB NOT NULL DEFAULT '{}'::jsonb,
  projected_header_hash BYTEA,
  status TEXT NOT NULL,
  created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  UNIQUE (withdrawal_l1_tx_hash, withdrawal_l1_output_index),
  CHECK (status IN ('awaiting', 'projected', 'finalized')),
  CHECK (validity IS NULL OR validity IN (
    'WithdrawalIsValid',
    'NonExistentWithdrawalUtxo',
    'SpentWithdrawalUtxo',
    'IncorrectWithdrawalOwner',
    'IncorrectWithdrawalValue',
    'IncorrectWithdrawalSignature',
    'TooManyTokensInWithdrawal',
    'UnpayableWithdrawalValue'
  )),
  CHECK (status = 'awaiting' OR settlement_event_info IS NOT NULL),
  CHECK (status = 'awaiting' OR validity IS NOT NULL),
  CHECK (status <> 'awaiting' OR projected_header_hash IS NULL)
);

CREATE INDEX idx_withdrawal_utxos_status_inclusion_time_event_id
  ON withdrawal_utxos (status, inclusion_time, event_id);

CREATE INDEX idx_withdrawal_utxos_projected_header_hash
  ON withdrawal_utxos (projected_header_hash);

CREATE INDEX idx_withdrawal_utxos_withdrawal_l1_tx_hash
  ON withdrawal_utxos (withdrawal_l1_tx_hash);

CREATE INDEX idx_withdrawal_utxos_l2_outref
  ON withdrawal_utxos (l2_outref);

CREATE TABLE pending_block_finalization_withdrawals (
  header_hash BYTEA NOT NULL REFERENCES pending_block_finalizations(header_hash) ON DELETE CASCADE,
  member_id BYTEA NOT NULL REFERENCES withdrawal_utxos(event_id) ON DELETE RESTRICT,
  ordinal INTEGER NOT NULL,
  PRIMARY KEY (header_hash, member_id),
  UNIQUE (header_hash, ordinal)
);
