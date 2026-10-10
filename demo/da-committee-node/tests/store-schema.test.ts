import {
  createTemporalRegistry,
  followerMigrations,
} from "@al-ft/midgard-l1-follower";
import { declaredTables, lintSchema } from "@al-ft/midgard-l1-follower/lint";
import { Client, Pool } from "pg";
import { describe, expect, it } from "vitest";

import {
  COMMITTEE_QUEUE_TABLE,
  COMMITTEE_QUEUE_TABLE_SPEC,
  committeeMigrations,
} from "../src/l1/follower/queue-table.js";
import type { CommitteeStoreReadinessCounts } from "../src/store.js";
import {
  committeeStoreMigrations,
  initializeCommitteeSchema,
} from "../src/store/postgres.schema.js";
import {
  closeTestCommitteeStore,
  openTestCommitteeStore,
  testStoreDatabase,
} from "./helpers/committee-store.js";

const withClient = async <T>(
  url: string,
  run: (client: Client) => Promise<T>,
): Promise<T> => {
  const client = new Client({ connectionString: url });
  await client.connect();
  try {
    return await run(client);
  } finally {
    await client.end();
  }
};

/** The readiness counts, computed by scanning every row. */
const scannedCounts = async (
  client: Client,
): Promise<CommitteeStoreReadinessCounts> => {
  const row = (
    await client.query<Record<string, string>>(`
      WITH open_headers AS (
        SELECT h.header_hash, p.record AS payload
        FROM committee_state_queue_headers h
        LEFT JOIN committee_da_payloads p USING (header_hash)
        WHERE h.record->>'status' IN ('unattested', 'attesting')
      ), submitted AS (
        SELECT DISTINCT header_hash FROM committee_l1_submissions
        WHERE record->>'resultStatus' IN ('submitted', 'confirmed')
      )
      SELECT
        (SELECT count(*) FROM committee_state_queue_headers) AS headers,
        (SELECT count(*) FROM open_headers
          WHERE payload IS NULL OR payload->>'validationStatus' <> 'verified') AS missing,
        (SELECT count(*) FROM committee_da_payloads
          WHERE record->>'validationStatus' = 'verified') AS verified,
        (SELECT count(*) FROM open_headers o
          WHERE o.payload->>'validationStatus' = 'verified'
            AND o.header_hash NOT IN (SELECT header_hash FROM submitted)) AS verified_missing,
        (SELECT count(*) FROM committee_da_signatures) AS signatures,
        (SELECT count(*) FROM committee_l1_submissions) AS submissions,
        (SELECT count(*) FROM submitted) AS submitted
    `)
  ).rows[0]!;
  return {
    discoveredHeaders: Number(row.headers),
    missingPayloads: Number(row.missing),
    verifiedPayloads: Number(row.verified),
    verifiedPayloadsMissingL1Attestation: Number(row.verified_missing),
    signatures: Number(row.signatures),
    l1AttestationSubmissions: Number(row.submissions),
    submittedOrConfirmedL1Attestations: Number(row.submitted),
  };
};

const hash = (index: number): string =>
  index.toString(16).padStart(2, "0").repeat(28);
const digest = (index: number): string =>
  index.toString(16).padStart(2, "0").repeat(32);

describe("committee store schema", () => {
  it("passes the F2 schema lint alongside the follower's tables, every committee table classed", () => {
    expect(
      lintSchema(
        [
          followerMigrations("postgres"),
          committeeMigrations("postgres"),
          committeeStoreMigrations,
        ],
        createTemporalRegistry([COMMITTEE_QUEUE_TABLE_SPEC]),
      ),
    ).toEqual([]);
    const declared = declaredTables([committeeStoreMigrations]);
    expect(
      declared.find((table) => table.table === "committee_da_signatures")
        ?.tableClass,
    ).toBe("B");
  });

  it("creates exactly the tables its lint migration declares", async () => {
    const database = await testStoreDatabase();
    const store = await openTestCommitteeStore(database);
    await closeTestCommitteeStore(store);
    const created = await withClient(database.url, async (client) =>
      (
        await client.query<{ readonly table_name: string }>(
          "SELECT table_name FROM information_schema.tables WHERE table_schema = 'public' ORDER BY table_name",
        )
      ).rows.map((row) => row.table_name),
    );
    expect(created).not.toContain(COMMITTEE_QUEUE_TABLE);
    expect(created).toEqual(
      declaredTables([committeeStoreMigrations])
        .map((table) => table.table)
        .sort(),
    );
  });

  it("keeps the readiness counters equal to a full scan through single- and many-row inserts, updates and deletes, and reseeds them on open", async () => {
    const database = await testStoreDatabase();
    const store = await openTestCommitteeStore(database);
    await withClient(database.url, async (client) => {
      const statuses = [
        "unattested",
        "attesting",
        "attesting",
        "finalized",
        "attesting",
      ];
      for (const [index, status] of statuses.entries())
        await client.query(
          "INSERT INTO committee_state_queue_headers (header_hash, record) VALUES ($1, $2)",
          [hash(index + 1), { status }],
        );
      for (const [index, validationStatus] of [
        "verified",
        "fetched",
        "verified",
        "verified",
        "verified",
      ].entries())
        await client.query(
          "INSERT INTO committee_da_payloads (header_hash, record) VALUES ($1, $2)",
          [hash(index + 1), { validationStatus }],
        );
      await client.query(
        "UPDATE committee_da_payloads SET record = $2 WHERE header_hash = $1",
        [hash(2), { validationStatus: "verified" }],
      );
      await client.query(
        "UPDATE committee_da_payloads SET record = $2 WHERE header_hash = $1",
        [hash(3), { validationStatus: "rejected" }],
      );
      await client.query(
        "UPDATE committee_da_payloads SET record = $2 WHERE header_hash = $1",
        [hash(4), { validationStatus: "rejected" }],
      );
      await client.query(
        "DELETE FROM committee_da_payloads WHERE header_hash = $1",
        [hash(1)],
      );
      for (const signer of [0, 1, 2])
        await client.query(
          "INSERT INTO committee_da_signatures (header_hash, commitment_digest, signer_index, record) VALUES ($1, $2, $3, '{}')",
          [hash(1), digest(9), signer],
        );
      await client.query(
        "DELETE FROM committee_da_signatures WHERE signer_index = 1",
      );
      const submission = (
        header: number,
        txHash: number,
        resultStatus: string,
      ) =>
        client.query(
          "INSERT INTO committee_l1_submissions (header_hash, tx_kind, tx_hash, record) VALUES ($1, 'attest', $2, $3)",
          [hash(header), digest(txHash), { resultStatus }],
        );
      await submission(1, 1, "submitted");
      await submission(1, 2, "submitted");
      await submission(1, 5, "submitted");
      await submission(2, 3, "pending");
      await submission(3, 4, "confirmed");
      await client.query(
        "UPDATE committee_l1_submissions SET record = $2 WHERE tx_hash = $1",
        [digest(3), { resultStatus: "confirmed" }],
      );
      await client.query(
        "UPDATE committee_l1_submissions SET record = $2 WHERE tx_hash = $1",
        [digest(4), { resultStatus: "failed" }],
      );
      // Header 1 keeps two submitted rows: still counted once.
      await client.query(
        "DELETE FROM committee_l1_submissions WHERE tx_hash = $1",
        [digest(1)],
      );
      const scanned = await scannedCounts(client);
      expect(scanned).toEqual({
        discoveredHeaders: 5,
        missingPayloads: 2,
        verifiedPayloads: 2,
        verifiedPayloadsMissingL1Attestation: 1,
        signatures: 2,
        l1AttestationSubmissions: 4,
        submittedOrConfirmedL1Attestations: 2,
      });
      await expect(store.readinessCounts()).resolves.toEqual(scanned);

      // Statements that change many rows at once, as retirement and
      // upserts do: each counter moves by every row the statement changed.
      await client.query(
        `INSERT INTO committee_l1_submissions (header_hash, tx_kind, tx_hash, record)
         VALUES ($1, 'attest', $2, $5), ($1, 'attest', $3, $5),
                ($4, 'attest', $6, $7)`,
        [
          hash(5),
          digest(6),
          digest(7),
          hash(4),
          { resultStatus: "submitted" },
          digest(8),
          { resultStatus: "pending" },
        ],
      );
      // Both of header 1's submitted rows fail in one statement.
      await client.query(
        "UPDATE committee_l1_submissions SET record = $2 WHERE header_hash = $1",
        [hash(1), { resultStatus: "failed" }],
      );
      // One insert and one update in a single upsert.
      await client.query(
        `INSERT INTO committee_da_payloads (header_hash, record)
         VALUES ($1, $3), ($2, $3)
         ON CONFLICT (header_hash) DO UPDATE SET record = EXCLUDED.record`,
        [hash(1), hash(3), { validationStatus: "verified" }],
      );
      await client.query("DELETE FROM committee_da_signatures");
      await client.query(
        `INSERT INTO committee_da_signatures (header_hash, commitment_digest, signer_index, record)
         SELECT $1, $2, signer, '{}' FROM generate_series(0, 2) signer`,
        [hash(2), digest(9)],
      );
      await client.query(
        "DELETE FROM committee_state_queue_headers WHERE header_hash = ANY($1)",
        [[hash(4), hash(5)]],
      );
      const afterStatements = await scannedCounts(client);
      expect(afterStatements).toEqual({
        discoveredHeaders: 3,
        missingPayloads: 0,
        verifiedPayloads: 4,
        verifiedPayloadsMissingL1Attestation: 2,
        signatures: 3,
        l1AttestationSubmissions: 7,
        submittedOrConfirmedL1Attestations: 2,
      });
      await expect(store.readinessCounts()).resolves.toEqual(afterStatements);
      // A store created before the counters: they are seeded from the rows.
      await client.query("DELETE FROM committee_store_counts");
    });
    await closeTestCommitteeStore(store);
    const reopened = await openTestCommitteeStore(database);
    const scanned = await withClient(database.url, scannedCounts);
    await expect(reopened.readinessCounts()).resolves.toEqual(scanned);
  });

  it("runs the schema statements only on a connection that still holds the instance lock", async () => {
    const database = await testStoreDatabase();
    await closeTestCommitteeStore(await openTestCommitteeStore(database));
    // Not pending, so the L1 record upgrade does not parse it.
    const quarantined = {
      status: "published",
      quarantineReason: "stale",
      quarantinedAt: "2026-10-01T00:00:00.000Z",
    };
    await withClient(database.url, (client) =>
      client.query(
        "INSERT INTO committee_decision_outbox (effect_id, header_hash, record) VALUES ($1, $2, $3)",
        ["effect-1", hash(1), quarantined],
      ),
    );
    const outboxRecord = () =>
      withClient(
        database.url,
        async (client) =>
          (
            await client.query<{ readonly record: unknown }>(
              "SELECT record FROM committee_decision_outbox WHERE effect_id = 'effect-1'",
            )
          ).rows[0]?.record,
      );
    const pool = new Pool({ connectionString: database.url });
    try {
      const lost = new Error("the instance lock is no longer held");
      await expect(
        initializeCommitteeSchema(
          pool,
          {
            assertHeldAtServer: async () => {
              throw lost;
            },
          },
          {},
          () => undefined,
        ),
      ).rejects.toBe(lost);
      // The upgrade UPDATE that strips the quarantine fields did not run.
      expect(await outboxRecord()).toEqual(quarantined);

      await initializeCommitteeSchema(
        pool,
        { assertHeldAtServer: async () => undefined },
        {},
        () => undefined,
      );
      expect(await outboxRecord()).toEqual({ status: "published" });
    } finally {
      await pool.end();
    }
  });
});
