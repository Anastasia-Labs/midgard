import { decodeSingleCbor, encodeCbor } from "@al-ft/midgard-core/codec/cbor";
import {
  computeDaSha256Hash,
  decodeDaConflictEvidenceCbor,
} from "@al-ft/midgard-core/da-transport";
import { Client } from "pg";
import { describe, expect, it } from "vitest";

import { loadDaSigner, signDaAttestation } from "../src/signer.js";
import {
  availabilityCommitment,
  conflictFixture,
  SIBLING_HEADER_HASH,
} from "./conflict-evidence.conflict-fixture.js";
import {
  closeTestCommitteeStore,
  openTestCommitteeStore,
  testStoreDatabase,
} from "./helpers/committee-store.js";

// Owner ruling 2026-10-07: a pair over sibling headers is not equivocation
// (docs/midgard/decisions/da-sibling-signatures-not-slashable.md). A store
// from before the ruling may hold one; the codec refuses it on read, so the
// schema upgrade deletes it while dropping the column that told it apart.

/** The record an older build stored for a relayed sibling-header pair. */
const records = async () => {
  const fixture = await conflictFixture();
  const signer = await loadDaSigner(`hex:${"00".repeat(31)}01`);
  const sibling = availabilityCommitment(SIBLING_HEADER_HASH, "99".repeat(28));
  const compact = decodeDaConflictEvidenceCbor(
    fixture.encoded,
  ).compactEvidence!;
  const tuple = decodeSingleCbor(compact) as unknown[];
  const siblingCompact = encodeCbor([
    ...tuple.slice(0, 5),
    Buffer.from(SIBLING_HEADER_HASH, "hex"),
    Buffer.from(sibling.cbor, "hex"),
    Buffer.from(
      signDaAttestation({
        signer,
        signerIndex: 0,
        availabilityCommitment: sibling.commitment,
      }),
      "hex",
    ),
  ]);
  return {
    sameHeader: fixture.record,
    crossHeader: {
      ...fixture.record,
      conflictingHeaderHash: SIBLING_HEADER_HASH,
      conflictingCommitmentDigest: sibling.digest,
      evidenceHash: computeDaSha256Hash(siblingCompact).toString("hex"),
      compactEvidenceCborHex: siblingCompact.toString("hex"),
    },
  };
};

/** The conflict-evidence table as stores before this schema created it. */
const PRE_UPGRADE_TABLE_SQL = `
DROP TABLE committee_da_conflict_evidence;
CREATE TABLE committee_da_conflict_evidence (
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
);`;

type ConflictRecord = Awaited<ReturnType<typeof records>>["crossHeader"];

const insertPreUpgrade = (client: Client, record: ConflictRecord) =>
  client.query(
    `INSERT INTO committee_da_conflict_evidence (
       deployment_fingerprint, evidence_hash, header_hash, commitment_digest,
       conflicting_header_hash, conflicting_commitment_digest, signer_index,
       reporter_peer_id, record)
     VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9::jsonb)`,
    [
      record.deploymentFingerprint,
      record.evidenceHash,
      record.headerHash,
      record.commitmentDigest,
      record.conflictingHeaderHash,
      record.conflictingCommitmentDigest,
      record.signerIndex,
      record.reporterPeerId,
      JSON.stringify(record),
    ],
  );

describe("conflict evidence in a store from before the sibling ruling", () => {
  it("drops a cross-header record and the column on open, keeps the same-header record, and readers succeed", async () => {
    const { sameHeader, crossHeader } = await records();
    const database = await testStoreDatabase();
    await closeTestCommitteeStore(await openTestCommitteeStore(database));
    const client = new Client({ connectionString: database.url });
    await client.connect();
    try {
      await client.query(PRE_UPGRADE_TABLE_SQL);
      await insertPreUpgrade(client, {
        ...sameHeader,
        conflictingHeaderHash: sameHeader.headerHash,
      });
      await insertPreUpgrade(client, crossHeader);

      const store = await openTestCommitteeStore(database);
      await expect(store.listDaConflictEvidence()).resolves.toEqual([
        sameHeader,
      ]);
      const snapshot = await store.readRetirementSnapshot();
      expect(Object.values(snapshot.data.daConflictEvidence)).toEqual([
        sameHeader,
      ]);
      await closeTestCommitteeStore(store);

      const columns = await client.query<{ readonly column_name: string }>(
        `SELECT column_name FROM information_schema.columns
         WHERE table_name = 'committee_da_conflict_evidence'`,
      );
      expect(columns.rows.map((row) => row.column_name)).not.toContain(
        "conflicting_header_hash",
      );
      // The digest ordering a new table checks holds on the upgraded one.
      await expect(
        client.query(
          `UPDATE committee_da_conflict_evidence
           SET conflicting_commitment_digest = commitment_digest`,
        ),
      ).rejects.toThrow(/check constraint/u);
    } finally {
      await client.end();
    }
    // A second open of the upgraded store is a no-op.
    const again = await openTestCommitteeStore(database);
    await expect(again.listDaConflictEvidence()).resolves.toEqual([sameHeader]);
  });
});
