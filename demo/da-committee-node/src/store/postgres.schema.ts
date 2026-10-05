import { MIDGARD_DEPLOYMENT_MARKER_SCHEMA_VERSION } from "@al-ft/midgard-core/deployment-manifest-identity";
import type { Pool } from "pg";
export const initializeCommitteeSchema = async (pool: Pool): Promise<void> => {
  await pool.query(`
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

      CREATE TABLE IF NOT EXISTS committee_state_queue_headers (
        header_hash text PRIMARY KEY,
        record jsonb NOT NULL,
        created_at timestamptz NOT NULL DEFAULT NOW(),
        updated_at timestamptz NOT NULL DEFAULT NOW()
      );

      CREATE TABLE IF NOT EXISTS committee_l1_source_state (
        id integer PRIMARY KEY CHECK (id = 1),
        record jsonb NOT NULL,
        created_at timestamptz NOT NULL DEFAULT NOW(),
        updated_at timestamptz NOT NULL DEFAULT NOW()
      );

      CREATE TABLE IF NOT EXISTS committee_decision_outbox (
        effect_id text PRIMARY KEY,
        header_hash text NOT NULL,
        record jsonb NOT NULL,
        created_at timestamptz NOT NULL DEFAULT NOW(),
        updated_at timestamptz NOT NULL DEFAULT NOW()
      );

      CREATE TABLE IF NOT EXISTS committee_da_payloads (
        header_hash text PRIMARY KEY,
        record jsonb NOT NULL,
        created_at timestamptz NOT NULL DEFAULT NOW(),
        updated_at timestamptz NOT NULL DEFAULT NOW()
      );

      CREATE TABLE IF NOT EXISTS committee_promise_capacity_evidence (
        evidence_key text PRIMARY KEY CHECK (evidence_key ~ '^[0-9a-f]{64}$'),
        record jsonb NOT NULL
      );

      CREATE TABLE IF NOT EXISTS committee_da_signatures (
        header_hash text NOT NULL,
        commitment_digest text NOT NULL CHECK (commitment_digest ~ '^[0-9a-f]{64}$'),
        signer_index integer NOT NULL CHECK (signer_index >= 0 AND signer_index <= 255),
        record jsonb NOT NULL,
        created_at timestamptz NOT NULL DEFAULT NOW(),
        updated_at timestamptz NOT NULL DEFAULT NOW(),
        PRIMARY KEY (header_hash, commitment_digest, signer_index)
      );

      CREATE TABLE IF NOT EXISTS committee_da_conflict_evidence (
        deployment_fingerprint text NOT NULL CHECK (deployment_fingerprint ~ '^[0-9a-f]{64}$'),
        evidence_hash text NOT NULL CHECK (evidence_hash ~ '^[0-9a-f]{64}$'),
        header_hash text NOT NULL CHECK (header_hash ~ '^[0-9a-f]{56}$'),
        commitment_digest text NOT NULL CHECK (commitment_digest ~ '^[0-9a-f]{64}$'),
        conflicting_header_hash text NOT NULL CHECK (conflicting_header_hash ~ '^[0-9a-f]{56}$'),
        conflicting_commitment_digest text NOT NULL CHECK (conflicting_commitment_digest ~ '^[0-9a-f]{64}$'),
        CHECK ((conflicting_header_hash || conflicting_commitment_digest) > (header_hash || commitment_digest)),
        signer_index integer NOT NULL CHECK (signer_index >= 0 AND signer_index <= 255),
        reporter_peer_id text NOT NULL CHECK (length(reporter_peer_id) > 0),
        record jsonb NOT NULL,
        created_at timestamptz NOT NULL DEFAULT NOW(),
        PRIMARY KEY (deployment_fingerprint, evidence_hash)
      );

      CREATE TABLE IF NOT EXISTS committee_da_attestation_candidates (
        header_hash text NOT NULL,
        out_ref text NOT NULL,
        record jsonb NOT NULL,
        created_at timestamptz NOT NULL DEFAULT NOW(),
        updated_at timestamptz NOT NULL DEFAULT NOW(),
        PRIMARY KEY (header_hash, out_ref)
      );

      CREATE TABLE IF NOT EXISTS committee_l1_submissions (
        header_hash text NOT NULL,
        tx_kind text NOT NULL,
        tx_hash text NOT NULL,
        record jsonb NOT NULL,
        created_at timestamptz NOT NULL DEFAULT NOW(),
        updated_at timestamptz NOT NULL DEFAULT NOW(),
        PRIMARY KEY (header_hash, tx_kind, tx_hash)
      );

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

      CREATE TABLE IF NOT EXISTS committee_peer_health (
        peer_id text PRIMARY KEY,
        record jsonb NOT NULL,
        created_at timestamptz NOT NULL DEFAULT NOW(),
        updated_at timestamptz NOT NULL DEFAULT NOW()
      );

      CREATE TABLE IF NOT EXISTS committee_peer_nonces (
        deployment_fingerprint text NOT NULL,
        signer_index integer NOT NULL CHECK (signer_index >= 0 AND signer_index <= 255),
        nonce text NOT NULL,
        record jsonb NOT NULL,
        created_at timestamptz NOT NULL DEFAULT NOW(),
        PRIMARY KEY (deployment_fingerprint, signer_index, nonce)
      );
    `);
};
