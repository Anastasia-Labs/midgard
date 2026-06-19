CREATE TABLE IF NOT EXISTS deposit_submission_attempts (
  tx_hash BYTEA PRIMARY KEY CHECK (octet_length(tx_hash) = 32),
  deposit_event_id BYTEA NOT NULL,
  expected_deposit_out_ref TEXT NOT NULL,
  expected_l2_address TEXT NOT NULL,
  expected_lovelace TEXT NOT NULL,
  expected_assets JSONB NOT NULL,
  metadata JSONB NOT NULL,
  funding_out_refs JSONB NOT NULL,
  submitted_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  confirmation_status TEXT NOT NULL CHECK (
    confirmation_status IN (
      'submitted_confirmation_unknown',
      'confirmed',
      'reconciled_after_timeout',
      'ambiguous',
      'retry_allowed'
    )
  ),
  confirmed_at TIMESTAMPTZ,
  last_reconciled_at TIMESTAMPTZ,
  last_error TEXT,
  updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
);

CREATE INDEX IF NOT EXISTS idx_deposit_submission_attempts_deposit_event_id
  ON deposit_submission_attempts (deposit_event_id);

CREATE INDEX IF NOT EXISTS idx_deposit_submission_attempts_status_submitted_at
  ON deposit_submission_attempts (confirmation_status, submitted_at);
