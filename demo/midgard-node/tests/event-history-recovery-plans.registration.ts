import { createHash, randomUUID } from "node:crypto";

import * as SDK from "@al-ft/midgard-sdk";
import { SqlClient } from "@effect/sql";
import { CML, Data, toUnit } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { afterAll, beforeAll, beforeEach, expect } from "vitest";

import * as Authority from "../src/database/eventHistoryAuthority.js";
import * as Journal from "../src/database/eventHistoryJournal.js";
import { type HistoryRecoveryIntent } from "../src/database/eventHistoryRecoveryPlans.js";
import { formatDatabaseError } from "../src/database/utils/common.js";
import {
  decodeBoundEventHistoryLedgerSnapshot,
  eventHistoryCanonicalJson,
  type EventHistorySourceBinding,
  HISTORY_GENESIS_DIGEST_ALGORITHM,
} from "../src/l1-event-history-source.js";
import type { LedgerSnapshotOutput } from "../src/l1-ledger-snapshot.js";
import { NodeConfig } from "../src/services/config.js";
import { Database } from "../src/services/database.js";
import { makeCardanoSignedMapOutputTxBytes } from "./helpers/cardano-native-fixtures.js";
import { retainEverything } from "./helpers/history-journal-retention.js";
import { loadRealMidgardContractsForTest } from "./helpers/real-midgard-contracts.js";
import { applyMidgardNodeTestEnv } from "./test-env.js";
import { resetApplicationTables } from "./utils.js";

// Component verification: actual PostgreSQL authority, journal and atomic repair.
// The strict decoded L1 snapshot and empty block ancestry are explicitly modeled;
// neither these receipts nor the signed body establish source/native authority.
applyMidgardNodeTestEnv();

export const hash = (n: number) => n.toString(16).padStart(64, "0");

export const sha = (value: string | Buffer) =>
  createHash("sha256").update(value).digest("hex");

const modelOriginReceipt =
  "Explicit model source replay evidence; not ledger admission";

export const run = <A, E>(
  program: Effect.Effect<A, E, SqlClient.SqlClient | NodeConfig>,
) =>
  Effect.runPromise(
    program.pipe(
      Effect.mapError((error) => new Error(formatDatabaseError(error))),
      Effect.provide(Database.layer),
      Effect.provide(NodeConfig.layer),
    ),
  );

export const refusal = async <A, E>(
  program: Effect.Effect<A, E, SqlClient.SqlClient | NodeConfig>,
  message: string,
) => {
  const result = await run(program.pipe(Effect.either));
  expect(result._tag).toBe("Left");
  if (result._tag !== "Left") throw new Error("Expected refusal");
  expect(formatDatabaseError(result.left)).toContain(message);
};

export let binding: EventHistorySourceBinding;

let initial: Journal.Checkpoint["capture"];

const rootDatum = () =>
  Data.to(
    {
      position: "Root",
      next: null,
      protected_until: 0n,
      payload: "RootContent",
    },
    SDK.EventHistoryNode,
  );

export const read = async () => {
  const checkpoint = await run(Journal.load(binding));
  if (checkpoint === null) throw new Error("Missing checkpoint");
  return checkpoint;
};

export const start = async (initialGeneration = false) => {
  const claimed = await run(
    Authority.acquire({
      deploymentIdentity: binding.manifestId,
      ownerToken: randomUUID(),
      leaseDurationMs: 60_000,
    }),
  );
  const token = initialGeneration
    ? claimed
    : await run(Authority.beginRecovery(claimed, "Modeled component replay"));
  await run(
    Authority.withRecovery(
      token,
      Journal.seed({
        binding,
        capture: initial,
        height: 1,
        originReceipt: modelOriginReceipt,
        originReceiptDigest: sha(modelOriginReceipt),
        incarnations: [],
      }),
    ),
  );
  return { token, checkpoint: await read() };
};

export const intent = (): HistoryRecoveryIntent => {
  const signed = makeCardanoSignedMapOutputTxBytes();
  return {
    bindingDigest: binding.digest,
    manifestId: binding.manifestId,
    headerHash: "ab".repeat(28),
    signedTransactionHash: CML.hash_transaction(
      CML.Transaction.from_cbor_bytes(signed).body(),
    ).to_hex(),
    signedTransactionCborSha256: sha(signed),
    expectedRoot: hash(20),
    targetRoot: hash(21),
    journalDigest: hash(22),
  };
};

export const document = (value: HistoryRecoveryIntent) =>
  eventHistoryCanonicalJson({
    domain: "midgard-history-recovery-intent-v1",
    ...value,
  });

export const rows = () =>
  run(
    Effect.gen(function* () {
      const sql = yield* SqlClient.SqlClient;
      return yield* sql<{
        row: Record<string, unknown>;
      }>`SELECT to_jsonb(p) AS row FROM event_history_recovery_plans p ORDER BY recovery_id`;
    }),
  );

export const probes = () =>
  run(
    Effect.gen(function* () {
      const sql = yield* SqlClient.SqlClient;
      return yield* sql<{
        label: string;
      }>`SELECT label FROM history_recovery_plan_probe ORDER BY label`;
    }),
  );

export const repair = (label = "repaired") =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    yield* sql`INSERT INTO history_recovery_plan_probe(label) VALUES (${label})`;
  });

export const append = async (
  token: Authority.Token,
  checkpoint: Journal.Checkpoint,
) => {
  const point = {
    id: hash(1000 + checkpoint.head.height),
    slot: checkpoint.head.slot + 1,
    height: checkpoint.head.height + 1,
  };
  const capture = await Effect.runPromise(
    decodeBoundEventHistoryLedgerSnapshot(
      {
        ...checkpoint.capture.history.ledger,
        point: { id: point.id, slot: point.slot },
      },
      binding,
    ),
  );
  const prepared = Journal.prepareAppend(
    checkpoint,
    { point, parent: checkpoint.head.id, transactions: [] },
    { capture, transitions: [] },
  );
  await run(
    Authority.withRecovery(
      token,
      Journal.append(binding, prepared, () => Effect.void, retainEverything),
    ),
  );
  return read();
};

beforeAll(async () => {
  const contracts = await loadRealMidgardContractsForTest({
    txHash: hash(900),
    outputIndex: 0,
  });
  const pair = SDK.requireEventHistoryContracts(contracts);
  binding = {
    digest: hash(902),
    manifestId: hash(903),
    network: "Preprod",
    genesisAlgorithm: HISTORY_GENESIS_DIGEST_ALGORITHM,
    genesisSha256: hash(905),
    hubAddress: contracts.hubOracle.spendingScriptAddress,
    hubUnit: toUnit(contracts.hubOracle.policyId, SDK.HUB_ORACLE_ASSET_NAME),
    hubDatumCbor: Data.to(
      await Effect.runPromise(SDK.makeHubOracleDatum(contracts)),
      SDK.HubOracleDatum,
    ),
    deployments: {
      deposit: SDK.eventHistoryDeploymentFromContracts(pair.deposit),
      withdrawal: SDK.eventHistoryDeploymentFromContracts(pair.withdrawal),
    },
  };
  const outputs: LedgerSnapshotOutput[] = Object.values(
    binding.deployments,
  ).map((entry, outputIndex) => ({
    txHash: hash(910),
    outputIndex,
    address: entry.address,
    assets: { lovelace: 3_000_000n, [entry.policyId]: 1n },
    datum: rootDatum(),
    hasReferenceScript: false,
  }));
  outputs.push({
    txHash: hash(911),
    outputIndex: 0,
    address: binding.hubAddress,
    assets: { lovelace: 3_000_000n, [binding.hubUnit]: 1n },
    datum: binding.hubDatumCbor,
    hasReferenceScript: false,
  });
  initial = await Effect.runPromise(
    decodeBoundEventHistoryLedgerSnapshot(
      {
        point: { id: hash(1), slot: 100 },
        addresses: [
          binding.hubAddress,
          ...Object.values(binding.deployments).flatMap((entry) => [
            entry.address,
            entry.retentionAddress,
          ]),
        ],
        outputs,
      },
      binding,
    ),
  );
}, 120_000);

const cleanup = Effect.gen(function* () {
  const sql = yield* SqlClient.SqlClient;
  yield* sql`CREATE TABLE IF NOT EXISTS history_recovery_plan_probe(label text PRIMARY KEY)`;
  yield* resetApplicationTables;
});

beforeEach(async () => {
  await run(cleanup);
});

afterAll(async () => {
  await run(cleanup);
  await run(
    Effect.gen(function* () {
      const sql = yield* SqlClient.SqlClient;
      yield* sql`DROP TABLE history_recovery_plan_probe`;
    }),
  );
});
