DO $$
BEGIN
  IF EXISTS (SELECT 1 FROM mempool LIMIT 1)
    OR EXISTS (SELECT 1 FROM processed_mempool LIMIT 1)
    OR EXISTS (SELECT 1 FROM immutable LIMIT 1)
    OR EXISTS (SELECT 1 FROM tx_rejections LIMIT 1)
  THEN
    RAISE EXCEPTION
      'refusing to add durable tx admissions over existing untracked tx state; restore a clean snapshot or write an explicit audited backfill migration';
  END IF;
END $$;

CREATE TYPE tx_admission_status AS ENUM (
  'queued',
  'validating',
  'accepted',
  'rejected'
);

CREATE TABLE tx_admissions (
  tx_id BYTEA PRIMARY KEY CHECK (octet_length(tx_id) = 32),
  tx_cbor BYTEA NOT NULL CHECK (octet_length(tx_cbor) > 0),
  tx_cbor_sha256 BYTEA NOT NULL CHECK (octet_length(tx_cbor_sha256) = 32),
  arrival_seq BIGSERIAL UNIQUE NOT NULL,
  status tx_admission_status NOT NULL,
  first_seen_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  last_seen_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  validation_started_at TIMESTAMPTZ,
  terminal_at TIMESTAMPTZ,
  lease_owner TEXT,
  lease_expires_at TIMESTAMPTZ,
  attempt_count INTEGER NOT NULL DEFAULT 0 CHECK (attempt_count >= 0),
  next_attempt_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  reject_code TEXT,
  reject_detail TEXT,
  submit_source TEXT NOT NULL,
  request_count BIGINT NOT NULL DEFAULT 1 CHECK (request_count >= 1),
  CHECK (submit_source IN ('native', 'cardano-converted', 'backfill')),
  CHECK (last_seen_at >= first_seen_at AND updated_at >= first_seen_at),
  CHECK (
    (
      status = 'validating'
      AND lease_owner IS NOT NULL
      AND lease_expires_at IS NOT NULL
      AND terminal_at IS NULL
    )
    OR (
      status <> 'validating'
      AND lease_owner IS NULL
      AND lease_expires_at IS NULL
    )
  ),
  CHECK (
    (status IN ('accepted', 'rejected') AND terminal_at IS NOT NULL)
    OR (status IN ('queued', 'validating') AND terminal_at IS NULL)
  ),
  CHECK (
    (status = 'rejected' AND reject_code IS NOT NULL)
    OR (status <> 'rejected' AND reject_code IS NULL AND reject_detail IS NULL)
  )
);

CREATE INDEX idx_tx_admissions_dequeue
  ON tx_admissions (next_attempt_at, arrival_seq)
  WHERE status IN ('queued', 'validating');

CREATE INDEX idx_tx_admissions_status_updated
  ON tx_admissions (status, updated_at);

CREATE INDEX idx_tx_admissions_lease
  ON tx_admissions (lease_expires_at)
  WHERE status = 'validating';

CREATE UNIQUE INDEX uniq_tx_rejections_tx_id
  ON tx_rejections (tx_id);
