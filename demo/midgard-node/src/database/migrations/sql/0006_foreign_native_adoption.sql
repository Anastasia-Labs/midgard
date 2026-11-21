-- Native promotion and SQL projection form one source-owned, resumable operation.
-- A request grants no native/cache mutation authority; recovery verifies it again.
CREATE TABLE foreign_native_adoptions (
  sequence BIGINT GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
  adoption_id BYTEA NOT NULL UNIQUE CHECK (octet_length(adoption_id) = 32),
  binding_digest BYTEA NOT NULL CHECK (octet_length(binding_digest) = 32),
  manifest_id BYTEA NOT NULL CHECK (octet_length(manifest_id) = 32),
  source_hash BYTEA NOT NULL CHECK (octet_length(source_hash) = 32),
  source_slot BIGINT NOT NULL CHECK (source_slot >= 0),
  source_height BIGINT NOT NULL CHECK (source_height >= 0),
  source_snapshot BYTEA NOT NULL CHECK (octet_length(source_snapshot) = 32),
  checkpoint_revision BIGINT NOT NULL CHECK (checkpoint_revision >= 0),
  header_hash BYTEA NOT NULL CHECK (octet_length(header_hash) = 28),
  target_root TEXT NOT NULL CHECK (target_root ~ '^[0-9a-f]{64}$'),
  state TEXT NOT NULL CHECK (state IN ('requested', 'prepared', 'applied', 'rewinding', 'rewound', 'discarded', 'sealed')),
  source_observation JSONB NOT NULL CHECK (jsonb_typeof(source_observation) = 'array'),
  replay_record JSONB,
  touched_outrefs BYTEA[],
  ledger_before JSONB CHECK (jsonb_typeof(ledger_before) = 'array'),
  ledger_after JSONB CHECK (jsonb_typeof(ledger_after) = 'array'),
  event_before JSONB,
  event_after JSONB,
  updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  CHECK ((state IN ('requested', 'discarded')) OR
    (replay_record IS NOT NULL AND touched_outrefs IS NOT NULL AND ledger_before IS NOT NULL AND ledger_after IS NOT NULL))
);
CREATE UNIQUE INDEX uniq_foreign_native_adoption_pending
  ON foreign_native_adoptions (binding_digest) WHERE state IN ('requested', 'prepared', 'rewinding');
CREATE INDEX idx_foreign_native_adoption_source
  ON foreign_native_adoptions (binding_digest, sequence) WHERE state IN ('requested', 'prepared', 'applied', 'rewinding');

-- Full verified ancestry is retained even when native already owns a suffix.
-- A later merge may remove these headers from the queue; their exact deltas
-- bridge ConfirmedLedger to the freshly observed confirmed header.
CREATE TABLE foreign_verified_segments (
  binding_digest BYTEA NOT NULL CHECK (octet_length(binding_digest) = 32),
  manifest_id BYTEA NOT NULL CHECK (octet_length(manifest_id) = 32),
  header_hash BYTEA NOT NULL CHECK (octet_length(header_hash) = 28),
  parent_header_hash BYTEA NOT NULL CHECK (octet_length(parent_header_hash) = 28),
  parent_root TEXT NOT NULL CHECK (parent_root ~ '^[0-9a-f]{64}$'),
  utxos_root TEXT NOT NULL CHECK (utxos_root ~ '^[0-9a-f]{64}$'),
  source_hash BYTEA NOT NULL CHECK (octet_length(source_hash) = 32),
  source_slot BIGINT NOT NULL CHECK (source_slot >= 0),
  source_snapshot BYTEA NOT NULL CHECK (octet_length(source_snapshot) = 32),
  segment_record TEXT NOT NULL,
  segment_digest BYTEA NOT NULL CHECK (octet_length(segment_digest) = 32),
  confirmed BOOLEAN NOT NULL DEFAULT FALSE,
  source_sealed BOOLEAN NOT NULL DEFAULT FALSE,
  PRIMARY KEY (binding_digest,header_hash)
);
CREATE INDEX idx_foreign_verified_segment_parent ON foreign_verified_segments (binding_digest,parent_header_hash);
CREATE TABLE foreign_confirmed_frontier (
  binding_digest BYTEA PRIMARY KEY CHECK (octet_length(binding_digest) = 32),
  manifest_id BYTEA NOT NULL CHECK (octet_length(manifest_id) = 32),
  header_hash BYTEA NOT NULL CHECK (octet_length(header_hash) = 28),
  utxos_root TEXT NOT NULL CHECK (utxos_root ~ '^[0-9a-f]{64}$')
);
