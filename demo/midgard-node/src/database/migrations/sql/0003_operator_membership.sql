CREATE TABLE operator_membership_observations (
  manifest_id BYTEA NOT NULL CHECK (octet_length(manifest_id) = 32),
  operator_key BYTEA NOT NULL CHECK (octet_length(operator_key) = 28),
  active_block_hash BYTEA NOT NULL CHECK (octet_length(active_block_hash) = 32),
  active_block_slot BIGINT NOT NULL CHECK (active_block_slot >= 0),
  active_block_height BIGINT NOT NULL CHECK (active_block_height >= 0),
  PRIMARY KEY (manifest_id, operator_key)
);
