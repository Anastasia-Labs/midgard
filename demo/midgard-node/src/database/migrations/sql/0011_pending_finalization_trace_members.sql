CREATE TABLE pending_block_finalization_transition_trace (
  header_hash BYTEA NOT NULL REFERENCES pending_block_finalizations(header_hash) ON DELETE CASCADE,
  member_id BYTEA NOT NULL,
  ordinal INTEGER NOT NULL CHECK (ordinal >= 0),
  payload_cbor BYTEA NOT NULL,
  payload_sha256 BYTEA NOT NULL CHECK (octet_length(payload_sha256) = 32),
  source_table TEXT NOT NULL,
  source_id BYTEA NOT NULL,
  source_time_stamp_tz TIMESTAMPTZ NOT NULL,
  PRIMARY KEY (header_hash, member_id),
  UNIQUE (header_hash, ordinal)
);

CREATE TABLE pending_block_finalization_event_to_step (
  header_hash BYTEA NOT NULL REFERENCES pending_block_finalizations(header_hash) ON DELETE CASCADE,
  member_id BYTEA NOT NULL,
  ordinal INTEGER NOT NULL CHECK (ordinal >= 0),
  payload_cbor BYTEA NOT NULL,
  payload_sha256 BYTEA NOT NULL CHECK (octet_length(payload_sha256) = 32),
  source_table TEXT NOT NULL,
  source_id BYTEA NOT NULL,
  source_time_stamp_tz TIMESTAMPTZ NOT NULL,
  PRIMARY KEY (header_hash, member_id),
  UNIQUE (header_hash, ordinal)
);
