-- class: D; retention: an entry leaves on an accepting local verdict, its block's removal, or its deadline (block end + M/2); a block's row and outputs leave with its last entry
-- The import checker's durable set: each Plutus transaction of an imported
-- foreign block, applied by its committed verdict, until its script runs are
-- accepted locally, its block is removed, or its deadline passes. A locally
-- rejected entry stays, as `rejected`, until its deadline.
--
-- A transaction is stored once (its bytes and program-material sidecar), and
-- each output a block's checks spend or reference once per block; the
-- checker rebuilds the runs from them with the block's validation context.
CREATE TABLE node_import_check_blocks (
  header_hash BYTEA PRIMARY KEY,
  block_time_ms BIGINT NOT NULL,
  block_slot NUMERIC NOT NULL,
  network_id NUMERIC NOT NULL,
  min_fee_a NUMERIC NOT NULL,
  min_fee_b NUMERIC NOT NULL,
  slot_zero_time BIGINT NOT NULL,
  slot_zero_slot BIGINT NOT NULL,
  slot_length BIGINT NOT NULL,
  recorded_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
);

CREATE TABLE node_import_checks (
  header_hash BYTEA NOT NULL
    REFERENCES node_import_check_blocks (header_hash) ON DELETE CASCADE,
  tx_id BYTEA NOT NULL CHECK (octet_length(tx_id) = 32),
  block_position INTEGER NOT NULL CHECK (block_position >= 0),
  deadline_ms BIGINT NOT NULL,
  state TEXT NOT NULL CHECK (state IN ('pending', 'cap_skipped', 'cache_skipped',
    'faulted', 'rejected')),
  tx_cbor BYTEA NOT NULL,
  program_material_sidecar BYTEA NOT NULL,
  runs INTEGER NOT NULL CHECK (runs > 0),
  budget_cpu NUMERIC NOT NULL CHECK (budget_cpu >= 0),
  run_bytes BIGINT NOT NULL CHECK (run_bytes >= 0),
  -- The sha256 of each distinct script the runs execute, concatenated.
  script_hashes BYTEA NOT NULL CHECK (octet_length(script_hashes) % 32 = 0),
  PRIMARY KEY (header_hash, tx_id)
);

CREATE INDEX idx_node_import_checks_deadline ON node_import_checks (deadline_ms);
CREATE INDEX idx_node_import_checks_state_deadline
  ON node_import_checks (state, deadline_ms, header_hash, block_position);

CREATE TABLE node_import_check_outputs (
  header_hash BYTEA NOT NULL
    REFERENCES node_import_check_blocks (header_hash) ON DELETE CASCADE,
  out_ref BYTEA NOT NULL,
  output BYTEA NOT NULL,
  PRIMARY KEY (header_hash, out_ref)
);
