DO $$
BEGIN
  IF EXISTS (
    SELECT 1
    FROM pending_block_finalizations
    WHERE status IN (
      'pending_submission',
      'submitted_local_finalization_pending',
      'submitted_unconfirmed',
      'observed_waiting_stability'
    )
  ) THEN
    RAISE EXCEPTION 'Refusing to migrate active old-format pending block finalization journals; run an explicit verified recovery or perform a full local plus on-chain redeploy/reset.';
  END IF;
END
$$;

ALTER TABLE pending_block_finalizations
  ADD COLUMN state_queue_lease_token TEXT NOT NULL DEFAULT 'migration:no-active-record',
  ADD COLUMN base_snapshot_id TEXT NOT NULL DEFAULT 'migration:no-active-record',
  ADD COLUMN base_tail_out_ref TEXT NOT NULL DEFAULT 'migration:no-active-record',
  ADD COLUMN base_tail_header_hash BYTEA NOT NULL DEFAULT decode(repeat('00', 28), 'hex') CHECK (octet_length(base_tail_header_hash) = 28),
  ADD COLUMN base_tail_datum_cbor TEXT NOT NULL DEFAULT '',
  ADD COLUMN base_utxos_root TEXT NOT NULL DEFAULT '',
  ADD COLUMN base_transactions_root TEXT NOT NULL DEFAULT '',
  ADD COLUMN base_deposits_root TEXT NOT NULL DEFAULT '',
  ADD COLUMN base_withdrawals_root TEXT NOT NULL DEFAULT '',
  ADD COLUMN block_start_time TIMESTAMPTZ NOT NULL DEFAULT '-infinity',
  ADD COLUMN expected_utxos_root TEXT NOT NULL DEFAULT '',
  ADD COLUMN expected_transactions_root TEXT NOT NULL DEFAULT '',
  ADD COLUMN expected_deposits_root TEXT NOT NULL DEFAULT '',
  ADD COLUMN expected_withdrawals_root TEXT NOT NULL DEFAULT '';

ALTER TABLE pending_block_finalizations
  ALTER COLUMN state_queue_lease_token DROP DEFAULT,
  ALTER COLUMN base_snapshot_id DROP DEFAULT,
  ALTER COLUMN base_tail_out_ref DROP DEFAULT,
  ALTER COLUMN base_tail_header_hash DROP DEFAULT,
  ALTER COLUMN base_tail_datum_cbor DROP DEFAULT,
  ALTER COLUMN base_utxos_root DROP DEFAULT,
  ALTER COLUMN base_transactions_root DROP DEFAULT,
  ALTER COLUMN base_deposits_root DROP DEFAULT,
  ALTER COLUMN base_withdrawals_root DROP DEFAULT,
  ALTER COLUMN block_start_time DROP DEFAULT,
  ALTER COLUMN expected_utxos_root DROP DEFAULT,
  ALTER COLUMN expected_transactions_root DROP DEFAULT,
  ALTER COLUMN expected_deposits_root DROP DEFAULT,
  ALTER COLUMN expected_withdrawals_root DROP DEFAULT;

ALTER TABLE pending_block_finalization_deposits
  ADD COLUMN payload_cbor BYTEA NOT NULL DEFAULT '\x',
  ADD COLUMN payload_sha256 BYTEA NOT NULL DEFAULT decode(repeat('00', 32), 'hex') CHECK (octet_length(payload_sha256) = 32),
  ADD COLUMN source_table TEXT NOT NULL DEFAULT 'migration:no-active-record',
  ADD COLUMN source_id BYTEA NOT NULL DEFAULT '\x',
  ADD COLUMN source_time_stamp_tz TIMESTAMPTZ NOT NULL DEFAULT '-infinity';

ALTER TABLE pending_block_finalization_deposits
  ALTER COLUMN payload_cbor DROP DEFAULT,
  ALTER COLUMN payload_sha256 DROP DEFAULT,
  ALTER COLUMN source_table DROP DEFAULT,
  ALTER COLUMN source_id DROP DEFAULT,
  ALTER COLUMN source_time_stamp_tz DROP DEFAULT;

ALTER TABLE pending_block_finalization_withdrawals
  ADD COLUMN payload_cbor BYTEA NOT NULL DEFAULT '\x',
  ADD COLUMN payload_sha256 BYTEA NOT NULL DEFAULT decode(repeat('00', 32), 'hex') CHECK (octet_length(payload_sha256) = 32),
  ADD COLUMN source_table TEXT NOT NULL DEFAULT 'migration:no-active-record',
  ADD COLUMN source_id BYTEA NOT NULL DEFAULT '\x',
  ADD COLUMN source_time_stamp_tz TIMESTAMPTZ NOT NULL DEFAULT '-infinity';

ALTER TABLE pending_block_finalization_withdrawals
  ALTER COLUMN payload_cbor DROP DEFAULT,
  ALTER COLUMN payload_sha256 DROP DEFAULT,
  ALTER COLUMN source_table DROP DEFAULT,
  ALTER COLUMN source_id DROP DEFAULT,
  ALTER COLUMN source_time_stamp_tz DROP DEFAULT;

ALTER TABLE pending_block_finalization_txs
  ADD COLUMN payload_cbor BYTEA NOT NULL DEFAULT '\x',
  ADD COLUMN payload_sha256 BYTEA NOT NULL DEFAULT decode(repeat('00', 32), 'hex') CHECK (octet_length(payload_sha256) = 32),
  ADD COLUMN source_table TEXT NOT NULL DEFAULT 'migration:no-active-record',
  ADD COLUMN source_id BYTEA NOT NULL DEFAULT '\x',
  ADD COLUMN source_time_stamp_tz TIMESTAMPTZ NOT NULL DEFAULT '-infinity';

ALTER TABLE pending_block_finalization_txs
  ALTER COLUMN payload_cbor DROP DEFAULT,
  ALTER COLUMN payload_sha256 DROP DEFAULT,
  ALTER COLUMN source_table DROP DEFAULT,
  ALTER COLUMN source_id DROP DEFAULT,
  ALTER COLUMN source_time_stamp_tz DROP DEFAULT;
