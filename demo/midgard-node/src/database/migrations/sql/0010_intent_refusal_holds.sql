-- class: B; retention: one row per family while its refusal stands; deleted on that family's next successful journal record, or once a landed valid tx other than the refused one spends one of the refused tx's inputs (never on a timer)
-- An intent-journal refusal (§8.2) held as a named /readyz reason. Rows are
-- written by whichever process refused the record (the main process, the
-- commit and settlement workers) and read by the main process, whose
-- /readyz reports them; the database is the boundary they cross.
CREATE TABLE intent_refusal_holds (
  family text PRIMARY KEY,
  reason text NOT NULL,
  detail text NOT NULL,
  tx_hash bytea NOT NULL CHECK (octet_length(tx_hash) = 32),
  inputs bytea[] NOT NULL,
  raised_at timestamptz NOT NULL DEFAULT NOW()
);
