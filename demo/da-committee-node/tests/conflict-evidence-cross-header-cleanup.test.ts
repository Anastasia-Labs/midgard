import { readFile, writeFile } from "node:fs/promises";
import { join } from "node:path";

import { decodeSingleCbor, encodeCbor } from "@al-ft/midgard-core/codec/cbor";
import {
  computeDaSha256Hash,
  decodeDaConflictEvidenceCbor,
} from "@al-ft/midgard-core/da-transport";
import { Client } from "pg";
import { afterAll, describe, expect, it } from "vitest";

import { loadDaSigner, signDaAttestation } from "../src/signer.js";
import { conflictEvidenceKey } from "../src/store.committee-store.js";
import { JsonFileCommitteeStore } from "../src/store.js";
import { PostgresCommitteeStore } from "../src/store/postgres.js";
import {
  availabilityCommitment,
  conflictFixture,
  SIBLING_HEADER_HASH,
} from "./conflict-evidence.conflict-fixture.js";
import { tempDir } from "./helpers.js";
import { postgresTestDatabases } from "./helpers/postgres-database.js";

// Owner ruling 2026-10-07: a pair over sibling headers is not equivocation.
// A row such a pair left behind by an older build would throw out of every
// conflict-evidence read, so both stores drop it on open.

const databases = postgresTestDatabases("midgard_test_cross_header_cleanup");
afterAll(async () => databases.dropAll());

/** The record an older build stored for a relayed sibling-header pair. */
const crossHeaderRecord = async () => {
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

describe("cross-header conflict evidence left by older builds", () => {
  it("is dropped and persisted away when a JSON store opens", async () => {
    const { sameHeader, crossHeader } = await crossHeaderRecord();
    const directory = await tempDir();
    const store = await JsonFileCommitteeStore.open(directory);
    await store.saveDaConflictEvidence(sameHeader);
    await store.close();
    const file = join(directory, "committee.json");
    const raw = JSON.parse(await readFile(file, "utf8")) as {
      daConflictEvidence: Record<string, unknown>;
    };
    raw.daConflictEvidence[conflictEvidenceKey(crossHeader)] = crossHeader;
    await writeFile(file, JSON.stringify(raw));

    const reopened = await JsonFileCommitteeStore.open(directory);
    try {
      await expect(reopened.listDaConflictEvidence()).resolves.toEqual([
        sameHeader,
      ]);
      const snapshot = await reopened.readRetirementSnapshot();
      expect(Object.values(snapshot.data.daConflictEvidence)).toEqual([
        sameHeader,
      ]);
    } finally {
      await reopened.close();
    }
    expect(await readFile(file, "utf8")).not.toContain(SIBLING_HEADER_HASH);
  });

  it("is deleted when a Postgres store opens", async () => {
    const { sameHeader, crossHeader } = await crossHeaderRecord();
    const database = await databases.create();
    const store = await PostgresCommitteeStore.open(database.url);
    await store.saveDaConflictEvidence(sameHeader);
    await store.close();
    const client = new Client({ connectionString: database.url });
    await client.connect();
    try {
      await client.query(
        `INSERT INTO committee_da_conflict_evidence (
           deployment_fingerprint, evidence_hash, header_hash,
           commitment_digest, conflicting_header_hash,
           conflicting_commitment_digest, signer_index, reporter_peer_id,
           record)
         VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9::jsonb)`,
        [
          crossHeader.deploymentFingerprint,
          crossHeader.evidenceHash,
          crossHeader.headerHash,
          crossHeader.commitmentDigest,
          crossHeader.conflictingHeaderHash,
          crossHeader.conflictingCommitmentDigest,
          crossHeader.signerIndex,
          crossHeader.reporterPeerId,
          JSON.stringify(crossHeader),
        ],
      );
    } finally {
      await client.end();
    }

    const reopened = await PostgresCommitteeStore.open(database.url);
    try {
      await expect(reopened.listDaConflictEvidence()).resolves.toEqual([
        sameHeader,
      ]);
      const snapshot = await reopened.readRetirementSnapshot();
      expect(Object.values(snapshot.data.daConflictEvidence)).toEqual([
        sameHeader,
      ]);
    } finally {
      await reopened.close();
    }
  });
});
