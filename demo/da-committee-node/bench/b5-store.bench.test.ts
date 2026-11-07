import { createHash } from "node:crypto";

import * as SDK from "@al-ft/midgard-sdk";
import { Client } from "pg";
import { afterAll, describe, expect, it } from "vitest";

import type {
  DaPayloadRecord,
  DaStoredPayloadRootSet,
  L1SubmissionRecord,
  StateQueueHeaderRecord,
} from "../src/domain.js";
import { loadDaSigner, signDaAttestation } from "../src/signer.js";
import { PostgresCommitteeStore } from "../src/store/postgres.js";
import { healthyL1SourceState } from "../tests/helpers/committee-store.js";
import { postgresTestDatabases } from "../tests/helpers/postgres-database.js";
import {
  commitmentFor,
  signatureRecord,
} from "../tests/peer-coordinator.signature-record.js";

/**
 * B5 (plan §16.2), the mutation and readiness halves (C2, CC4): one store
 * mutation and one readiness probe at 10^3 and 10^5 records. Mutation cost
 * and readiness cost must not depend on store size: each ratio (median at
 * 10^5 over median at 10^3) ≤ 1.2.
 *
 * "Records" are decided headers, each with its header, verified payload,
 * the member's signature and a confirmed submission, as a long-running
 * member accumulates them; the same ten open headers sit on top at both
 * sizes. The two stores are measured in alternation, so drift in the host
 * load lands on both sides of the ratio.
 */
const SMALL = Number(process.env.COMMITTEE_B5_SMALL ?? "1000");
const LARGE = Number(process.env.COMMITTEE_B5_LARGE ?? "100000");
const ITERATIONS = Number(process.env.COMMITTEE_B5_ITERATIONS ?? "400");
const WARMUP = 50;
const OPEN_HEADERS = 10;
const TARGET_RATIO = 1.2;

const DEPLOYMENT = "b5".repeat(32);

const now = (): number => Number(process.hrtime.bigint()) / 1e6;
const median = (values: readonly number[]): number => {
  const sorted = [...values].sort((a, b) => a - b);
  return sorted[Math.floor(sorted.length / 2)]!;
};

/** Header hashes the preload never uses: the mutations write fresh rows. */
const freshHash = (store: number, iteration: number): string =>
  `ff${store.toString(16).padStart(2, "0")}${iteration.toString(16).padStart(52, "0")}`;

/** Preloads `records` decided headers and the open headers, in SQL. */
const preload = async (url: string, records: number): Promise<void> => {
  const client = new Client({ connectionString: url });
  await client.connect();
  try {
    const hash = `lpad(to_hex(i), 56, '0')`;
    await client.query(
      `INSERT INTO committee_state_queue_headers (header_hash, record)
       SELECT ${hash}, jsonb_build_object('headerHash', ${hash},
         'status', CASE WHEN i <= $2 THEN 'attesting' ELSE 'finalized' END)
       FROM generate_series(1, $1::int + $2::int) i`,
      [records, OPEN_HEADERS],
    );
    await client.query(
      `INSERT INTO committee_da_payloads (header_hash, record)
       SELECT ${hash}, jsonb_build_object('headerHash', ${hash},
         'validationStatus', 'verified')
       FROM generate_series(1, $1::int + $2::int) i`,
      [records, OPEN_HEADERS],
    );
    await client.query(
      `INSERT INTO committee_da_signatures
         (header_hash, commitment_digest, signer_index, record, end_time_ms)
       SELECT ${hash}, lpad(to_hex(i), 64, '0'), 0,
         jsonb_build_object('headerHash', ${hash}, 'source', 'local'), i
       FROM generate_series($2::int + 1, $1::int + $2::int) i`,
      [records, OPEN_HEADERS],
    );
    await client.query(
      `INSERT INTO committee_l1_submissions (header_hash, tx_kind, tx_hash, record)
       SELECT ${hash}, 'apply', lpad(to_hex(i), 64, '0'),
         jsonb_build_object('headerHash', ${hash}, 'resultStatus', 'confirmed')
       FROM generate_series($2::int + 1, $1::int + $2::int) i`,
      [records, OPEN_HEADERS],
    );
    await client.query("ANALYZE");
  } finally {
    await client.end();
  }
};

const headerRecord = (headerHash: string): StateQueueHeaderRecord => ({
  deploymentFingerprint: DEPLOYMENT,
  headerHash,
  stateQueueOutRef: "b5#0",
  blockAssetName: `block-${headerHash}`,
  header: {} as StateQueueHeaderRecord["header"],
  computedHeaderHash: headerHash,
  daAttestation: SDK.NO_DA_ATTESTATION,
  observedChainPoint: {
    slot: 1,
    blockHash: "aa".repeat(32),
    depth: 10,
    providerSource: "b5",
  },
  finalized: false,
  status: "attesting",
  validationErrors: [],
  updatedAt: new Date().toISOString(),
});

/** The roots a stored payload carries; the store checks all eight. */
const rootSummary: DaStoredPayloadRootSet = {
  utxosRoot: "44".repeat(32),
  transactionsRoot: "55".repeat(32),
  depositsRoot: "66".repeat(32),
  withdrawalsRoot: "77".repeat(32),
  forcedTransactionsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
  transitionTraceRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
  eventToStepRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
  validationTracesRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
};

const payloadRecord = (headerHash: string): DaPayloadRecord => ({
  deploymentFingerprint: DEPLOYMENT,
  headerHash,
  payloadSchemaVersion: 1,
  payloadCborHex: "aabb",
  payloadSha256: createHash("sha256")
    .update(Buffer.from("aabb", "hex"))
    .digest("hex"),
  sourcePeerId: "b5",
  fetchedAt: new Date().toISOString(),
  verifiedAt: new Date().toISOString(),
  rootSummary,

  validationStatus: "verified",
  conflictStatus: "none",
});

const submissionRecord = (headerHash: string): L1SubmissionRecord => ({
  deploymentFingerprint: DEPLOYMENT,
  headerHash,
  txKind: "init",
  txHash: headerHash.padEnd(64, "0"),
  inputsUsed: [],
  submittedAt: new Date().toISOString(),
  resultStatus: "submitted",
});

type Operation = "header" | "payload" | "signature" | "submission";
const OPERATIONS: readonly Operation[] = [
  "header",
  "payload",
  "signature",
  "submission",
];

const databases = postgresTestDatabases("midgard_test_committee_b5_store");
afterAll(async () => {
  await databases.dropAll();
});

describe("B5: committee store mutation and readiness cost at 10^3 and 10^5 records", () => {
  it("Postgres", async () => {
    const signer = await loadDaSigner(`hex:${"00".repeat(31)}b5`);
    const sizes = [SMALL, LARGE] as const;
    const stores: PostgresCommitteeStore[] = [];
    try {
      for (const records of sizes) {
        const database = await databases.create();
        const store = await PostgresCommitteeStore.open(database.url);
        stores.push(store);
        await store.saveL1SourceState(healthyL1SourceState);
        const start = now();
        await preload(database.url, records);
        console.log(
          `B5 preload ${records} records in ${Math.round(now() - start)} ms`,
        );
      }
      const signatureFor = (headerHash: string) => {
        const commitment = commitmentFor(headerHash);
        return signatureRecord({
          deploymentFingerprint: DEPLOYMENT,
          headerHash,
          signerIndex: 0,
          committeeSignersHash: "77".repeat(32),
          commitment,
          signatureWitness: signDaAttestation({
            signer,
            signerIndex: 0,
            availabilityCommitment: commitment.commitment,
          }),
        });
      };
      // Each operation builds its record outside the timed span: only the
      // store call is measured.
      const writes: Record<
        Operation,
        (
          store: PostgresCommitteeStore,
          headerHash: string,
        ) => () => Promise<unknown>
      > = {
        header: (store, headerHash) => {
          const record = headerRecord(headerHash);
          return () => store.upsertStateQueueHeader(record);
        },
        payload: (store, headerHash) => {
          const record = payloadRecord(headerHash);
          return () => store.saveDaPayload(record);
        },
        signature: (store, headerHash) => {
          const record = signatureFor(headerHash);
          return () => store.saveDaSignature(record);
        },
        submission: (store, headerHash) => {
          const record = submissionRecord(headerHash);
          return () => store.saveL1Submission(record);
        },
      };
      const mutate = async (
        store: PostgresCommitteeStore,
        operation: Operation,
        headerHash: string,
      ): Promise<number> => {
        const write = writes[operation](store, headerHash);
        const start = now();
        await write();
        return now() - start;
      };
      const probe = async (store: PostgresCommitteeStore): Promise<number> => {
        const start = now();
        await store.readinessCounts();
        return now() - start;
      };
      const samples = sizes.map(() => ({
        header: [] as number[],
        payload: [] as number[],
        signature: [] as number[],
        submission: [] as number[],
        readiness: [] as number[],
      }));
      for (let iteration = 0; iteration < WARMUP + ITERATIONS; iteration += 1)
        for (const [index, store] of stores.entries()) {
          // The probe is read before this iteration's writes, so both
          // stores see the same open headers: the ten preloaded ones plus
          // one per earlier iteration.
          const readiness = await probe(store);
          const headerHash = freshHash(index, iteration);
          const costs: Partial<Record<Operation, number>> = {};
          for (const operation of OPERATIONS)
            costs[operation] = await mutate(store, operation, headerHash);
          if (iteration < WARMUP) continue;
          samples[index]!.readiness.push(readiness);
          for (const operation of OPERATIONS)
            samples[index]![operation].push(costs[operation]!);
        }
      const report = Object.fromEntries(
        [...OPERATIONS, "readiness" as const].map((name) => {
          const small = median(samples[0]![name]);
          const large = median(samples[1]![name]);
          return [
            name,
            {
              smallMs: Number(small.toFixed(3)),
              largeMs: Number(large.toFixed(3)),
              ratio: Number((large / small).toFixed(3)),
            },
          ];
        }),
      );
      // The probe's totals cover every record, so a wrong count is a wrong
      // store, not a fast one.
      const counts = await Promise.all(
        stores.map((store) => store.readinessCounts()),
      );
      for (const [index, records] of sizes.entries())
        expect(counts[index]!.discoveredHeaders).toBe(
          records + OPEN_HEADERS + WARMUP + ITERATIONS,
        );
      console.log(
        `B5 ${JSON.stringify({ small: SMALL, large: LARGE, iterations: ITERATIONS, report })}`,
      );
      for (const [name, { ratio }] of Object.entries(report))
        expect(ratio, name).toBeLessThanOrEqual(TARGET_RATIO);
    } finally {
      await Promise.all(stores.map((store) => store.close()));
    }
  });
});
