import { mkdir, writeFile } from "node:fs/promises";
import { dirname } from "node:path";

import { eventKeyOfId } from "@al-ft/midgard-l1-follower/events";
import { SqlClient } from "@effect/sql";
import { Effect } from "effect";
import * as MigrationRunner from "midgard-node/database/migrations/runner";
import {
  provideDatabaseLayers,
  resetApplicationTables,
} from "midgard-node/tests/utils";
import { afterEach, describe, expect, it } from "vitest";

import { withdrawalOnL1 } from "../src/commands/e2e-finalize-summary.stack-database.js";
import { settlementEvidence } from "../src/full-stack/payout.js";
import {
  payoutConclusion,
  payoutOutRef,
} from "../src/full-stack/payout-body.js";
import { stackManifestPath } from "../src/full-stack/process.js";
import {
  RecordingProcesses,
  removeStackFixtures,
  stackEnvironment,
  stackFixture,
} from "./full-stack-fixtures.js";
import {
  exactPayout,
  FUND_TX,
  INITIALIZE_TX,
  payoutAssets,
  recipient,
} from "./full-stack-payout.fixtures.js";

const dbEnabled = process.env.MIDGARD_SKIP_DB_TESTS !== "1";
const MANIFEST_ID = "e".repeat(64);
const PAYOUT_POLICY = "5a".repeat(28);
/** The withdrawal-order policy, whose token carries the same event key. */
const ORDER_POLICY = "0d".repeat(28);
const EVENT_ID = `d8799f5820${"d4".repeat(32)}00ff`;
const EVENT_KEY = eventKeyOfId(Buffer.from(EVENT_ID, "hex")).toString("hex");

afterEach(removeStackFixtures);

/** The withdrawal's L1 facts and settlement rows as op1 held them on twin 2. */
const seedTwinWithdrawal = (sql: SqlClient.SqlClient) =>
  sql.unsafe(`
    INSERT INTO l1_blocks (slot, hash, height, parent_hash, qualifying_tx_count) VALUES
      (100, decode('${"b1".repeat(32)}', 'hex'), 1, NULL, 1),
      (101, decode('${"b2".repeat(32)}', 'hex'), 2, NULL, 1),
      (102, decode('${"b3".repeat(32)}', 'hex'), 3, NULL, 1);
    INSERT INTO l1_txs (tx_hash, block_slot, block_tx_index, is_valid, inputs, reference_inputs, collaterals, output_count, has_collateral_return, mint, withdrawals, redeemers, body_cbor, witness_cbor) VALUES
      (decode('${FUND_TX}', 'hex'), 101, 0, true, '{}', '{}', '{}', 1, false, '{}', '{}', '[]', decode('a0', 'hex'), decode('a0', 'hex')),
      (decode('${exactPayout.txHash}', 'hex'), 102, 0, true, '{}', '{}', '{}', 2, false, '{}', '{}', '[]', decode('${exactPayout.bodyCbor}', 'hex'), decode('a0', 'hex'));
    INSERT INTO l1_outputs (tx_hash, output_index, address, lovelace, assets, created_slot, created_tx_index, spent_slot, spent_tx) VALUES
      (decode('${INITIALIZE_TX}', 'hex'), 1, decode('70', 'hex'), 2000000, '{}', 100, 0, 101, decode('${FUND_TX}', 'hex')),
      (decode('${FUND_TX}', 'hex'), 0, decode('70', 'hex'), 12000000, '{}', 101, 0, 102, decode('${exactPayout.txHash}', 'hex')),
      (decode('${"0e".repeat(32)}', 'hex'), 0, decode('71', 'hex'), 2000000, '{}', 100, 1, NULL, NULL);
    INSERT INTO l1_output_assets (tx_hash, output_index, policy_id, asset_name, quantity) VALUES
      (decode('${INITIALIZE_TX}', 'hex'), 1, decode('${PAYOUT_POLICY}', 'hex'), decode('${EVENT_KEY}', 'hex'), 1),
      (decode('${FUND_TX}', 'hex'), 0, decode('${PAYOUT_POLICY}', 'hex'), decode('${EVENT_KEY}', 'hex'), 1),
      (decode('${"0e".repeat(32)}', 'hex'), 0, decode('${ORDER_POLICY}', 'hex'), decode('${EVENT_KEY}', 'hex'), 1);
    INSERT INTO settlement_jobs (deployment_id, kind, event_id, phase) VALUES
      ('${MANIFEST_ID}', 'withdrawal', '${EVENT_ID}', 'complete');
    INSERT INTO settlement_attempts (deployment_id, tx_hash, kind, event_id, phase, signed_cbor, required_outputs, fee_inputs, status) VALUES
      ('${MANIFEST_ID}', '${"47".repeat(32)}', 'withdrawal', '${EVENT_ID}', 'initialize', '', '{}', '{fee#0}', 'expired'),
      ('${MANIFEST_ID}', '${FUND_TX}', 'withdrawal', '${EVENT_ID}', 'fund', '', '{}', '{fee#1}', 'pending');
  `);

/**
 * Runs `body` with the stack's settlement read answered by a freshly migrated
 * database holding the twin's rows, as psql in the stack's Postgres would.
 */
const onTwinDatabase = (
  body: (
    sql: SqlClient.SqlClient,
    processes: RecordingProcesses,
  ) => Effect.Effect<void, unknown>,
) =>
  Effect.runPromise(
    provideDatabaseLayers(
      Effect.gen(function* () {
        const sql = yield* SqlClient.SqlClient;
        yield* MigrationRunner.migrate({
          appVersion: "test",
          actor: "full-stack-payout-postgres.test",
        });
        yield* resetApplicationTables;
        yield* seedTwinWithdrawal(sql);
        const { config } = yield* Effect.promise(() => stackFixture());
        const manifest = stackManifestPath(config);
        yield* Effect.promise(async () => {
          await mkdir(dirname(manifest), { recursive: true });
          await writeFile(
            manifest,
            JSON.stringify({
              manifestId: MANIFEST_ID,
              contracts: { payoutMint: { scriptHash: PAYOUT_POLICY } },
            }),
          );
        });
        const processes = new RecordingProcesses(
          config,
          stackEnvironment(config),
        );
        processes.responses["settlement-withdrawal-observation"] = (call: {
          args: readonly string[];
        }) =>
          Effect.runPromise(
            sql
              .unsafe<{ value: unknown }>(`${call.args.at(-1)} AS value`)
              .pipe(Effect.map(([row]) => row!.value)),
          );
        yield* body(sql, processes).pipe(
          Effect.ensuring(Effect.orDie(resetApplicationTables)),
        );
      }),
    ) as Effect.Effect<void, unknown, never>,
  );

describe.skipIf(!dbEnabled)("withdrawal payout read from L1 facts", () => {
  it("finds the payout another operator concluded, with no local conclusion", () =>
    onTwinDatabase((_sql, processes) =>
      Effect.promise(async () => {
        const evidence = await settlementEvidence(
          processes,
          "withdrawal",
          EVENT_ID,
        );
        const conclusion = payoutConclusion(evidence);
        expect(conclusion).toEqual(exactPayout);
        expect(payoutOutRef(conclusion!, recipient, payoutAssets)).toEqual({
          txHash: exactPayout.txHash,
          outputIndex: 0,
        });
      }),
    ));
  it("waits while the funded payout output is unspent", () =>
    onTwinDatabase((sql, processes) =>
      Effect.gen(function* () {
        yield* sql.unsafe(
          `UPDATE l1_outputs SET spent_slot = NULL, spent_tx = NULL WHERE tx_hash = decode('${FUND_TX}', 'hex')`,
        );
        const evidence = yield* Effect.promise(() =>
          settlementEvidence(processes, "withdrawal", EVENT_ID),
        );
        expect(payoutConclusion(evidence)).toBeUndefined();
      }),
    ));
  it("gives the finalizer the same L1 facts the journey judged", () =>
    onTwinDatabase((sql) =>
      withdrawalOnL1(MANIFEST_ID, PAYOUT_POLICY, EVENT_ID).pipe(
        Effect.provideService(SqlClient.SqlClient, sql),
        Effect.map((facts) =>
          expect(payoutConclusion(facts!)).toEqual(exactPayout),
        ),
      ),
    ));
});
