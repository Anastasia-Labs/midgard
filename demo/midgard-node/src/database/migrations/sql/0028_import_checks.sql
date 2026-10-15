-- class: D; retention: an entry leaves on its local verdict, its block's removal, or its deadline (block end + M/2)
-- The import checker's durable set: each Plutus transaction of an imported
-- foreign block, applied by its committed verdict, until its script runs get
-- a local verdict, its block is removed, or its deadline passes.
CREATE TABLE node_import_checks (
  header_hash BYTEA NOT NULL,
  tx_id BYTEA NOT NULL CHECK (octet_length(tx_id) = 32),
  block_position INTEGER NOT NULL CHECK (block_position >= 0),
  deadline_ms BIGINT NOT NULL,
  state TEXT NOT NULL CHECK (state IN ('pending', 'cap_skipped', 'faulted')),
  program_material_sidecar BYTEA,
  recorded_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  PRIMARY KEY (header_hash, tx_id)
);

CREATE INDEX idx_node_import_checks_deadline ON node_import_checks (deadline_ms);

CREATE TABLE node_import_check_runs (
  header_hash BYTEA NOT NULL,
  tx_id BYTEA NOT NULL,
  run_index INTEGER NOT NULL CHECK (run_index >= 0),
  execution_index NUMERIC NOT NULL,
  script_bytes BYTEA NOT NULL,
  context_cbor BYTEA NOT NULL,
  budget_cpu NUMERIC,
  budget_memory NUMERIC,
  max_steps BIGINT NOT NULL CHECK (max_steps > 0),
  PRIMARY KEY (header_hash, tx_id, run_index),
  FOREIGN KEY (header_hash, tx_id)
    REFERENCES node_import_checks (header_hash, tx_id) ON DELETE CASCADE,
  CHECK ((budget_cpu IS NULL) = (budget_memory IS NULL))
);
