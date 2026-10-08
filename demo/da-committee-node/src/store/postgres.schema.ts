import { MIDGARD_DEPLOYMENT_MARKER_SCHEMA_VERSION } from "@al-ft/midgard-core/deployment-manifest-identity";
import type { MigrationSet } from "@al-ft/midgard-l1-follower";
import type { Pool } from "pg";

import { upgradeCommitteeL1Records } from "./postgres.upgrade-l1-records.js";

/**
 * Header statuses that are not yet final: the headers a readiness probe
 * inspects one by one. Every other status is history, read only by count.
 */
const OPEN_HEADER_PREDICATE = `record->>'status' IN ('unattested', 'attesting')`;

/**
 * A header whose L1 outcome is not settled: not recorded as merged or
 * removed. The committee writes a terminal record only once the exit is
 * final (deeper than k), so a terminal record never needs re-reading.
 */
export const UNSETTLED_HEADER_PREDICATE = `record->>'status' NOT IN ('merged', 'removed')`;

const SUBMITTED_PREDICATE = `record->>'resultStatus' IN ('submitted', 'confirmed')`;

/**
 * The committee store's tables, each with its class and retention rule
 * (plan §5.1, schema lint F2). The store applies this itself when it opens,
 * idempotently; the follower's migration runner never runs it, so
 * `follower reset --to-origin` (which deletes the catalogued A, D-t and D-x
 * tables) never reaches a committee store row.
 */
export const COMMITTEE_STORE_TABLES_SQL = `
-- class: B; retention: one row for the deployment's life, written once
CREATE TABLE IF NOT EXISTS committee_deployment (
  id integer PRIMARY KEY CHECK (id = 1),
  marker_schema_version text NOT NULL CHECK (marker_schema_version = '${MIDGARD_DEPLOYMENT_MARKER_SCHEMA_VERSION}'),
  manifest_id text NOT NULL CHECK (manifest_id ~ '^[0-9a-f]{64}$'),
  manifest_sha256 text NOT NULL CHECK (manifest_sha256 ~ '^[0-9a-f]{64}$'),
  contract_deployment_info_sha256 text NOT NULL CHECK (contract_deployment_info_sha256 ~ '^[0-9a-f]{64}$'),
  manifest_raw text NOT NULL,
  created_at timestamptz NOT NULL DEFAULT NOW(),
  updated_at timestamptz NOT NULL DEFAULT NOW()
);

-- class: B; retention: one row, the member's retirement floor and certificate, kept for the store's life
CREATE TABLE IF NOT EXISTS committee_retirement_metadata (
  id integer PRIMARY KEY CHECK (id = 1),
  record jsonb NOT NULL
);

-- class: A; retention: one row per observed state-queue header, pruned with its payload once retention allows
CREATE TABLE IF NOT EXISTS committee_state_queue_headers (
  header_hash text PRIMARY KEY,
  record jsonb NOT NULL,
  created_at timestamptz NOT NULL DEFAULT NOW(),
  updated_at timestamptz NOT NULL DEFAULT NOW()
);

-- class: A; retention: one row, overwritten on every healthy tick
CREATE TABLE IF NOT EXISTS committee_l1_source_state (
  id integer PRIMARY KEY CHECK (id = 1),
  record jsonb NOT NULL,
  created_at timestamptz NOT NULL DEFAULT NOW(),
  updated_at timestamptz NOT NULL DEFAULT NOW()
);

-- class: B; retention: one row per decision effect, kept until retirement
CREATE TABLE IF NOT EXISTS committee_decision_outbox (
  effect_id text PRIMARY KEY,
  header_hash text NOT NULL,
  record jsonb NOT NULL,
  created_at timestamptz NOT NULL DEFAULT NOW(),
  updated_at timestamptz NOT NULL DEFAULT NOW()
);

-- class: C; retention: RETENTION_DAYS from the manifest, and never before retention pruning allows
CREATE TABLE IF NOT EXISTS committee_da_payloads (
  header_hash text PRIMARY KEY,
  record jsonb NOT NULL,
  created_at timestamptz NOT NULL DEFAULT NOW(),
  updated_at timestamptz NOT NULL DEFAULT NOW()
);

-- class: A; retention: one row per capacity evidence key, kept until retirement
CREATE TABLE IF NOT EXISTS committee_promise_capacity_evidence (
  evidence_key text PRIMARY KEY CHECK (evidence_key ~ '^[0-9a-f]{64}$'),
  record jsonb NOT NULL
);

-- class: B; retention: one row per availability signature, the member's or a peer's, pruned with its payload; a row of the member's own (end_time_ms set) is its signed decision
CREATE TABLE IF NOT EXISTS committee_da_signatures (
  header_hash text NOT NULL,
  commitment_digest text NOT NULL CHECK (commitment_digest ~ '^[0-9a-f]{64}$'),
  signer_index integer NOT NULL CHECK (signer_index >= 0 AND signer_index <= 255),
  record jsonb NOT NULL,
  end_time_ms bigint CHECK (end_time_ms >= 0),
  created_at timestamptz NOT NULL DEFAULT NOW(),
  updated_at timestamptz NOT NULL DEFAULT NOW(),
  PRIMARY KEY (header_hash, commitment_digest, signer_index)
);

-- class: B; retention: one row per equivocation report, kept until retirement
CREATE TABLE IF NOT EXISTS committee_da_conflict_evidence (
  deployment_fingerprint text NOT NULL CHECK (deployment_fingerprint ~ '^[0-9a-f]{64}$'),
  evidence_hash text NOT NULL CHECK (evidence_hash ~ '^[0-9a-f]{64}$'),
  header_hash text NOT NULL CHECK (header_hash ~ '^[0-9a-f]{56}$'),
  commitment_digest text NOT NULL CHECK (commitment_digest ~ '^[0-9a-f]{64}$'),
  conflicting_commitment_digest text NOT NULL CHECK (conflicting_commitment_digest ~ '^[0-9a-f]{64}$'),
  CHECK (conflicting_commitment_digest > commitment_digest),
  signer_index integer NOT NULL CHECK (signer_index >= 0 AND signer_index <= 255),
  reporter_peer_id text NOT NULL CHECK (length(reporter_peer_id) > 0),
  record jsonb NOT NULL,
  created_at timestamptz NOT NULL DEFAULT NOW(),
  PRIMARY KEY (deployment_fingerprint, evidence_hash)
);

-- class: A; retention: one row per observed attestation output, pruned with its header
CREATE TABLE IF NOT EXISTS committee_da_attestation_candidates (
  header_hash text NOT NULL,
  out_ref text NOT NULL,
  record jsonb NOT NULL,
  created_at timestamptz NOT NULL DEFAULT NOW(),
  updated_at timestamptz NOT NULL DEFAULT NOW(),
  PRIMARY KEY (header_hash, out_ref)
);

-- class: B; retention: one row per signed L1 transaction, pruned with its header
CREATE TABLE IF NOT EXISTS committee_l1_submissions (
  header_hash text NOT NULL,
  tx_kind text NOT NULL,
  tx_hash text NOT NULL,
  record jsonb NOT NULL,
  created_at timestamptz NOT NULL DEFAULT NOW(),
  updated_at timestamptz NOT NULL DEFAULT NOW(),
  PRIMARY KEY (header_hash, tx_kind, tx_hash)
);

-- class: B; retention: one row per signature sent to a peer, pruned with its header
CREATE TABLE IF NOT EXISTS committee_peer_broadcasts (
  peer_id text NOT NULL,
  header_hash text NOT NULL,
  commitment_digest text NOT NULL CHECK (commitment_digest ~ '^[0-9a-f]{64}$'),
  signer_index integer NOT NULL CHECK (signer_index >= 0 AND signer_index <= 255),
  record jsonb NOT NULL,
  created_at timestamptz NOT NULL DEFAULT NOW(),
  updated_at timestamptz NOT NULL DEFAULT NOW(),
  PRIMARY KEY (peer_id, header_hash, commitment_digest, signer_index)
);

-- class: A; retention: one row per configured peer, overwritten on each health observation
CREATE TABLE IF NOT EXISTS committee_peer_health (
  peer_id text PRIMARY KEY,
  record jsonb NOT NULL,
  created_at timestamptz NOT NULL DEFAULT NOW(),
  updated_at timestamptz NOT NULL DEFAULT NOW()
);

-- class: B; retention: one row per accepted peer nonce, kept for replay protection
CREATE TABLE IF NOT EXISTS committee_peer_nonces (
  deployment_fingerprint text NOT NULL,
  signer_index integer NOT NULL CHECK (signer_index >= 0 AND signer_index <= 255),
  nonce text NOT NULL,
  record jsonb NOT NULL,
  created_at timestamptz NOT NULL DEFAULT NOW(),
  PRIMARY KEY (deployment_fingerprint, signer_index, nonce)
);

-- class: A; retention: one row per counter, forever; triggers keep it in the transaction that changes the rows it counts
CREATE TABLE IF NOT EXISTS committee_store_counts (
  name text PRIMARY KEY,
  value bigint NOT NULL CHECK (value >= 0)
);
`;

/**
 * Stores created before this schema still carry the conflict-evidence column
 * C4 made redundant: a stored record names one header (the codec refuses any
 * other), so `conflicting_header_hash` only ever equals `header_hash`. A
 * store from before C4 may still hold a relayed pair over two sibling headers
 * (docs/midgard/decisions/da-sibling-signatures-not-slashable.md); the codec
 * refuses it on read, so it would throw out of every conflict-evidence read.
 * Such rows are deleted while the column still tells them apart, then the
 * column goes with its ordering check, which is restated over the digests
 * alone as a new table has it, in the one transaction of the open.
 * They also lack the signed
 * decision's end time (C1 E6); it is backfilled from the member's own
 * signature records, whose canonical end time the write path checked. A
 * value that is not a bounded decimal stays NULL rather than failing the
 * open.
 * Their decision outbox records may carry the quarantine fields the L1
 * source no longer has (C1); the fields are stripped and the rest of each
 * record kept. `upgradeCommitteeL1Records` then upgrades the L1 source
 * state, which needs the record parser.
 */
const COMMITTEE_STORE_UPGRADE_SQL = `
DO $$
BEGIN
  IF EXISTS (
    SELECT 1 FROM information_schema.columns
    WHERE table_schema = current_schema()
      AND table_name = 'committee_da_conflict_evidence'
      AND column_name = 'conflicting_header_hash'
  ) THEN
    DELETE FROM committee_da_conflict_evidence
      WHERE header_hash <> conflicting_header_hash;
    ALTER TABLE committee_da_conflict_evidence
      DROP COLUMN conflicting_header_hash;
    ALTER TABLE committee_da_conflict_evidence
      ADD CHECK (conflicting_commitment_digest > commitment_digest);
  END IF;
END $$;
ALTER TABLE committee_da_signatures
  ADD COLUMN IF NOT EXISTS end_time_ms bigint CHECK (end_time_ms >= 0);
UPDATE committee_da_signatures
  SET end_time_ms = (record->'validation'->'l1Header'->>'endTime')::bigint
  WHERE end_time_ms IS NULL
    AND record->>'source' = 'local'
    AND record->'validation'->'l1Header'->>'endTime' ~ '^(0|[1-9][0-9]{0,17})$';
UPDATE committee_decision_outbox
  SET record = record - 'quarantineReason' - 'quarantinedAt'
  WHERE record ?| ARRAY['quarantineReason', 'quarantinedAt'];
`;

/**
 * Indexes and triggers. The open-header index serves the readiness probe's
 * per-header reads, which therefore cost O(headers not yet final), not
 * O(store); the unsettled-header index serves the tick's header read the
 * same way. The triggers keep `committee_store_counts`, so the probe reads
 * its totals without listing rows.
 */
const COMMITTEE_STORE_DERIVED_SQL = `
CREATE INDEX IF NOT EXISTS committee_state_queue_headers_open
  ON committee_state_queue_headers (header_hash)
  WHERE ${OPEN_HEADER_PREDICATE};

CREATE INDEX IF NOT EXISTS committee_state_queue_headers_unsettled
  ON committee_state_queue_headers (header_hash)
  WHERE ${UNSETTLED_HEADER_PREDICATE};

CREATE INDEX IF NOT EXISTS committee_da_signatures_signed_decisions
  ON committee_da_signatures (header_hash)
  WHERE end_time_ms IS NOT NULL;

-- The counters move once per statement, by the rows it changed (its
-- transition tables), never once per row: a statement that deletes a
-- thousand retired rows updates each counter once, not a thousand times
-- along one row's growing version chain.
CREATE OR REPLACE FUNCTION committee_store_count_rows() RETURNS trigger
LANGUAGE plpgsql AS $$
DECLARE
  delta bigint;
BEGIN
  IF TG_OP = 'INSERT' THEN
    SELECT count(*) INTO delta FROM new_rows;
  ELSE
    SELECT -count(*) INTO delta FROM old_rows;
  END IF;
  IF delta <> 0 THEN
    UPDATE committee_store_counts SET value = value + delta
     WHERE name = TG_ARGV[0];
  END IF;
  RETURN NULL;
END
$$;

CREATE OR REPLACE FUNCTION committee_store_count_verified_payloads() RETURNS trigger
LANGUAGE plpgsql AS $$
DECLARE
  delta bigint := 0;
  changed bigint;
BEGIN
  IF TG_OP <> 'INSERT' THEN
    SELECT count(*) INTO changed FROM old_rows
     WHERE record->>'validationStatus' = 'verified';
    delta := delta - changed;
  END IF;
  IF TG_OP <> 'DELETE' THEN
    SELECT count(*) INTO changed FROM new_rows
     WHERE record->>'validationStatus' = 'verified';
    delta := delta + changed;
  END IF;
  IF delta <> 0 THEN
    UPDATE committee_store_counts SET value = value + delta
     WHERE name = 'verified_payloads';
  END IF;
  RETURN NULL;
END
$$;

-- Distinct headers with a submitted or confirmed L1 submission. Only the
-- headers the statement changed are read, each through the primary key
-- prefix: a header is counted now if it has a submitted row, and was
-- counted before if it had one once the statement's changes are undone.
CREATE OR REPLACE FUNCTION committee_store_count_submitted_headers() RETURNS trigger
LANGUAGE plpgsql AS $$
DECLARE
  hashes text[] := '{}';
  undos integer[] := '{}';
  delta bigint;
BEGIN
  IF TG_OP <> 'INSERT' THEN
    SELECT hashes || coalesce(array_agg(header_hash), '{}'),
           undos || coalesce(array_agg((${SUBMITTED_PREDICATE})::integer), '{}')
      INTO hashes, undos FROM old_rows;
  END IF;
  IF TG_OP <> 'DELETE' THEN
    SELECT hashes || coalesce(array_agg(header_hash), '{}'),
           undos || coalesce(array_agg(-(${SUBMITTED_PREDICATE})::integer), '{}')
      INTO hashes, undos FROM new_rows;
  END IF;
  SELECT coalesce(sum((now_rows > 0)::integer - (now_rows + undo > 0)::integer), 0)
    INTO delta
    FROM (
      SELECT c.header_hash, sum(c.undo) AS undo,
             (SELECT count(*) FROM committee_l1_submissions s
               WHERE s.header_hash = c.header_hash
                 AND s.${SUBMITTED_PREDICATE}) AS now_rows
        FROM unnest(hashes, undos) AS c(header_hash, undo)
       GROUP BY c.header_hash
    ) headers;
  IF delta <> 0 THEN
    UPDATE committee_store_counts SET value = value + delta
     WHERE name = 'submitted_or_confirmed_headers';
  END IF;
  RETURN NULL;
END
$$;

CREATE OR REPLACE TRIGGER committee_state_queue_headers_count_insert
  AFTER INSERT ON committee_state_queue_headers REFERENCING NEW TABLE AS new_rows
  FOR EACH STATEMENT EXECUTE FUNCTION committee_store_count_rows('headers');
CREATE OR REPLACE TRIGGER committee_state_queue_headers_count_delete
  AFTER DELETE ON committee_state_queue_headers REFERENCING OLD TABLE AS old_rows
  FOR EACH STATEMENT EXECUTE FUNCTION committee_store_count_rows('headers');
CREATE OR REPLACE TRIGGER committee_da_signatures_count_insert
  AFTER INSERT ON committee_da_signatures REFERENCING NEW TABLE AS new_rows
  FOR EACH STATEMENT EXECUTE FUNCTION committee_store_count_rows('signatures');
CREATE OR REPLACE TRIGGER committee_da_signatures_count_delete
  AFTER DELETE ON committee_da_signatures REFERENCING OLD TABLE AS old_rows
  FOR EACH STATEMENT EXECUTE FUNCTION committee_store_count_rows('signatures');
CREATE OR REPLACE TRIGGER committee_l1_submissions_count_insert
  AFTER INSERT ON committee_l1_submissions REFERENCING NEW TABLE AS new_rows
  FOR EACH STATEMENT EXECUTE FUNCTION committee_store_count_rows('l1_submissions');
CREATE OR REPLACE TRIGGER committee_l1_submissions_count_delete
  AFTER DELETE ON committee_l1_submissions REFERENCING OLD TABLE AS old_rows
  FOR EACH STATEMENT EXECUTE FUNCTION committee_store_count_rows('l1_submissions');
CREATE OR REPLACE TRIGGER committee_da_payloads_verified_count_insert
  AFTER INSERT ON committee_da_payloads REFERENCING NEW TABLE AS new_rows
  FOR EACH STATEMENT EXECUTE FUNCTION committee_store_count_verified_payloads();
CREATE OR REPLACE TRIGGER committee_da_payloads_verified_count_update
  AFTER UPDATE ON committee_da_payloads REFERENCING OLD TABLE AS old_rows NEW TABLE AS new_rows
  FOR EACH STATEMENT EXECUTE FUNCTION committee_store_count_verified_payloads();
CREATE OR REPLACE TRIGGER committee_da_payloads_verified_count_delete
  AFTER DELETE ON committee_da_payloads REFERENCING OLD TABLE AS old_rows
  FOR EACH STATEMENT EXECUTE FUNCTION committee_store_count_verified_payloads();
CREATE OR REPLACE TRIGGER committee_l1_submissions_submitted_count_insert
  AFTER INSERT ON committee_l1_submissions REFERENCING NEW TABLE AS new_rows
  FOR EACH STATEMENT EXECUTE FUNCTION committee_store_count_submitted_headers();
CREATE OR REPLACE TRIGGER committee_l1_submissions_submitted_count_update
  AFTER UPDATE ON committee_l1_submissions REFERENCING OLD TABLE AS old_rows NEW TABLE AS new_rows
  FOR EACH STATEMENT EXECUTE FUNCTION committee_store_count_submitted_headers();
CREATE OR REPLACE TRIGGER committee_l1_submissions_submitted_count_delete
  AFTER DELETE ON committee_l1_submissions REFERENCING OLD TABLE AS old_rows
  FOR EACH STATEMENT EXECUTE FUNCTION committee_store_count_submitted_headers();

-- Seeded once from the rows already stored; the triggers keep it after.
DO $$
BEGIN
  IF NOT EXISTS (SELECT 1 FROM committee_store_counts) THEN
    INSERT INTO committee_store_counts (name, value)
    SELECT 'headers', count(*) FROM committee_state_queue_headers
    UNION ALL
    SELECT 'verified_payloads', count(*) FROM committee_da_payloads
     WHERE record->>'validationStatus' = 'verified'
    UNION ALL
    SELECT 'signatures', count(*) FROM committee_da_signatures
    UNION ALL
    SELECT 'l1_submissions', count(*) FROM committee_l1_submissions
    UNION ALL
    SELECT 'submitted_or_confirmed_headers', count(DISTINCT header_hash)
      FROM committee_l1_submissions WHERE ${SUBMITTED_PREDICATE};
  END IF;
END
$$;
`;

/**
 * The committee store schema as a migration set, for the schema lint
 * (`lintSchema`, plan F2). The store does not run it through the follower
 * runner; see `COMMITTEE_STORE_TABLES_SQL`.
 */
export const committeeStoreMigrations: MigrationSet = {
  namespace: "committee-store",
  migrations: [
    { id: "committee_store_tables", sql: COMMITTEE_STORE_TABLES_SQL },
  ],
};

/**
 * Creates or upgrades the committee store schema. One multi-statement query
 * runs as one implicit transaction: it applies whole or not at all. The L1
 * records are upgraded after it (`upgradeCommitteeL1Records`); that step
 * may refuse the open with a named readiness reason, never exit.
 */
export const initializeCommitteeSchema = async (pool: Pool): Promise<void> => {
  await pool.query(
    [
      COMMITTEE_STORE_TABLES_SQL,
      COMMITTEE_STORE_UPGRADE_SQL,
      COMMITTEE_STORE_DERIVED_SQL,
    ].join("\n"),
  );
  await upgradeCommitteeL1Records(pool);
};

/**
 * The readiness probe's reads: counters, and the headers not yet final. Each
 * open header's payload and submissions are read by key, through correlated
 * subqueries the planner cannot turn into a scan of the payload table,
 * whatever it estimates the open headers to number.
 */
export const COMMITTEE_READINESS_COUNTS_SQL = `
SELECT
  (SELECT jsonb_object_agg(name, value) FROM committee_store_counts) AS totals,
  count(*) FILTER (
    WHERE payload_status IS DISTINCT FROM 'verified'
  ) AS missing_payloads,
  count(*) FILTER (
    WHERE payload_status = 'verified' AND NOT submitted
  ) AS verified_missing_l1_attestation
FROM (
  SELECT
    (SELECT p.record->>'validationStatus' FROM committee_da_payloads p
      WHERE p.header_hash = h.header_hash) AS payload_status,
    EXISTS (
      SELECT 1 FROM committee_l1_submissions s
       WHERE s.header_hash = h.header_hash AND s.${SUBMITTED_PREDICATE}
    ) AS submitted
  FROM committee_state_queue_headers h
  WHERE h.${OPEN_HEADER_PREDICATE}
) open_headers`;
