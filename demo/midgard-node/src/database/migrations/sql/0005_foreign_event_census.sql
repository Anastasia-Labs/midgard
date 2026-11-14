-- Source-owned completeness frontier and immutable forced admissions. These
-- survive user-event row consumption and journal body retention.
CREATE TABLE event_history_census_blocks (
  binding_digest BYTEA NOT NULL CHECK (octet_length(binding_digest) = 32),
  manifest_id BYTEA NOT NULL CHECK (octet_length(manifest_id) = 32),
  block_hash BYTEA NOT NULL CHECK (octet_length(block_hash) = 32),
  parent_hash BYTEA NOT NULL CHECK (octet_length(parent_hash) = 32),
  block_slot BIGINT NOT NULL CHECK (block_slot >= 0),
  block_height BIGINT NOT NULL CHECK (block_height >= 0),
  receipt_digest BYTEA NOT NULL CHECK (octet_length(receipt_digest) = 32),
  admissions_record TEXT NOT NULL,
  admissions_digest BYTEA NOT NULL CHECK (octet_length(admissions_digest) = 32),
  canonical BOOLEAN NOT NULL,
  PRIMARY KEY (binding_digest, block_hash)
);
CREATE UNIQUE INDEX uniq_event_history_census_canonical_height
  ON event_history_census_blocks (binding_digest, block_height) WHERE canonical;
CREATE TABLE event_history_census_frontier (
  binding_digest BYTEA PRIMARY KEY CHECK (octet_length(binding_digest) = 32),
  manifest_id BYTEA NOT NULL CHECK (octet_length(manifest_id) = 32),
  activation_hash BYTEA NOT NULL CHECK (octet_length(activation_hash) = 32),
  activation_height BIGINT NOT NULL CHECK (activation_height >= 0),
  activation_transaction_index BIGINT NOT NULL CHECK (activation_transaction_index >= 0),
  head_hash BYTEA NOT NULL CHECK (octet_length(head_hash) = 32),
  head_slot BIGINT NOT NULL CHECK (head_slot >= 0),
  head_height BIGINT NOT NULL CHECK (head_height >= 0),
  FOREIGN KEY (binding_digest, head_hash) REFERENCES event_history_census_blocks (binding_digest, block_hash)
);
