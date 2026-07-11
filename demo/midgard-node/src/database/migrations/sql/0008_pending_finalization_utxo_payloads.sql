CREATE TABLE pending_block_finalization_utxos (
  header_hash BYTEA NOT NULL REFERENCES pending_block_finalizations(header_hash) ON DELETE CASCADE,
  outref BYTEA NOT NULL,
  ordinal INTEGER NOT NULL,
  output BYTEA NOT NULL,
  PRIMARY KEY (header_hash, outref),
  CONSTRAINT unique_pending_block_finalization_utxo_ordinal UNIQUE (header_hash, ordinal)
);
