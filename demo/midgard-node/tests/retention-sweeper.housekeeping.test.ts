import "./utils.js";

import { SqlClient } from "@effect/sql";
import { Effect, Ref } from "effect";
import { describe, expect, it } from "vitest";

import {
  computeHousekeepingCutoff,
  computeRetentionCutoff,
} from "../src/database/retention-policy.js";
import { retentionSweepAction } from "../src/fibers/retention-sweeper.js";
import {
  ContractDeploymentIdentity,
  Globals,
  NodeConfig,
} from "../src/services/index.js";
import { insertQueueTerminal } from "./helpers/queue-terminal-rows.js";
import {
  DAY_MS,
  DEPLOYMENT,
  insertLease,
  journals,
  leaseTokens,
  remainingLabels,
} from "./history-retention-prune.fixtures.js";
import { header } from "./local-mutation-job-abandonment.journal-fixture.js";
import { provideDatabaseLayers, resetApplicationTables } from "./utils.js";

/** A manifest window other than the compiled profile's 15 days, so a sweep
 * pruning by it shows the window came from the manifest. */
const MANIFEST_DAYS = 21;

const VIEW = {
  confirmedHeadHash: header("not-journaled"),
  liveQueueHeaderHashes: [],
};

const ago = (sql: SqlClient.SqlClient, days: number) =>
  sql`NOW() - make_interval(secs => ${(days * DAY_MS) / 1000})`;

/** Rows on each side of the manifest window, and records the window must not
 * reach whatever their age. */
const seed = Effect.gen(function* () {
  const sql = yield* SqlClient.SqlClient;
  const tx = (label: string) => header(`tx:${label}`);
  for (const [label, days] of [
    ["rejected-inside", MANIFEST_DAYS - 1],
    ["rejected-outside", MANIFEST_DAYS + 1],
  ] as const)
    yield* sql`INSERT INTO tx_rejections (tx_id, reject_code, created_at)
      VALUES (${tx(label)}, 'test', ${ago(sql, days)})`;
  for (const [label, days] of [
    ["inside", MANIFEST_DAYS - 1],
    ["outside", MANIFEST_DAYS + 1],
    ["queued", MANIFEST_DAYS + 1],
    ["processing", MANIFEST_DAYS + 1],
  ] as const)
    yield* sql`INSERT INTO address_history (tx_id, address, created_at)
      VALUES (${tx(label)}, ${`addr_${label}`}, ${ago(sql, days)})`;
  // Not yet in a block: their entries are how they are found by address.
  yield* sql`INSERT INTO mempool (tx_id, tx) VALUES (${tx("queued")}, '\\x00')`;
  yield* sql`INSERT INTO processed_mempool (tx_id, tx)
    VALUES (${tx("processing")}, '\\x00')`;
  for (const [token, days] of [
    ["ended-inside", MANIFEST_DAYS - 1],
    ["ended-outside", MANIFEST_DAYS + 1],
  ] as const)
    yield* insertLease({
      token,
      status: "released",
      acquiredAgoMs: 60 * DAY_MS,
      releasedAgoMs: days * DAY_MS,
    });
  // The newest inspectable rows, never pruned.
  for (let index = 0; index < 100; index++)
    yield* insertLease({
      token: `newer-${index.toString().padStart(3, "0")}`,
      status: "released",
      acquiredAgoMs: 50 * DAY_MS - index * 1_000,
      releasedAgoMs: 50 * DAY_MS,
    });
  // Finalized locally is not paid out on L1. A completed sibling remains
  // part of the same header's root while another event waits for settlement.
  for (const [label, status] of [
    ["deposit-awaiting", "awaiting"],
    ["deposit-projected", "projected"],
    ["deposit-unsettled", "consumed"],
    ["deposit-complete", "consumed"],
  ]) {
    yield* sql`INSERT INTO deposits_utxos
      (event_id, event_info, inclusion_time, deposit_l1_tx_hash, ledger_tx_id,
        ledger_output, ledger_address, projected_header_hash, status)
      VALUES (${tx(label!)}, ${Buffer.from("00", "hex")}, ${ago(sql, 60)},
        ${Buffer.alloc(32, 1)}, ${Buffer.alloc(32, 2)}, ${Buffer.from("00", "hex")},
        'addr_test', ${status === "awaiting" ? null : header("events-header")}, ${status})`;
  }
  for (const [label, status, validity] of [
    ["withdrawal-awaiting", "awaiting", null],
    ["withdrawal-projected", "projected", "WithdrawalIsValid"],
    ["withdrawal-invalid", "finalized", "SpentWithdrawalUtxo"],
    ["withdrawal-unsettled", "finalized", "WithdrawalIsValid"],
    ["withdrawal-complete", "finalized", "WithdrawalIsValid"],
  ] as const) {
    const bytes = Buffer.from("00", "hex");
    yield* sql`INSERT INTO withdrawal_utxos
      (event_id, raw_event_info, settlement_event_info, inclusion_time,
        withdrawal_l1_tx_hash, withdrawal_l1_output_index, asset_name, l2_outref,
        l2_owner, l2_value, l1_address, l1_datum, refund_address, refund_datum,
        validity, projected_header_hash, status)
      VALUES (${tx(label)}, ${bytes}, ${status === "awaiting" ? null : bytes},
        ${ago(sql, 60)}, ${Buffer.concat([tx(label), Buffer.alloc(4)])}, 0, ${bytes}, ${bytes},
        ${Buffer.alloc(28, 4)}, ${bytes}, ${bytes}, ${bytes}, ${bytes}, ${bytes},
        ${validity}, ${status === "awaiting" ? null : header("events-header")}, ${status})`;
  }
  for (const kind of ["deposit", "withdrawal"] as const)
    for (const completion of ["unsettled", "complete"] as const)
      yield* sql`INSERT INTO settlement_jobs (deployment_id, kind, event_id, phase)
        VALUES (${DEPLOYMENT.toString("hex")}, ${kind},
          ${tx(`${kind}-${completion}`).toString("hex")},
          ${completion === "complete" ? "complete" : kind === "deposit" ? "absorb" : "initialize"})`;
  yield* journals(
    JOURNALS.map((label, index) => ({
      label,
      status: "locally_applied" as const,
      endedAgoMs: (40 - index) * DAY_MS,
    })),
  );
  // A landed merge of the oldest journal the follower holds no final height
  // for: the finality hold keeps it.
  yield* insertQueueTerminal({
    headerHash: header("terminal-named"),
    outcome: "merged",
    height: 1,
  });
});

const JOURNALS = ["terminal-named", "plain", "newest"];

const sweep = (setup: { readonly driverRecomputing?: boolean } = {}) =>
  Effect.runPromise(
    provideDatabaseLayers(
      Effect.gen(function* () {
        const sql = yield* SqlClient.SqlClient;
        const clear = resetApplicationTables;
        yield* clear;
        return yield* Effect.gen(function* () {
          yield* seed;
          if (setup.driverRecomputing === true) {
            // The driver's recompute refuses the prune its permit.
            const globals = yield* Globals;
            yield* Ref.update(globals.FOLLOWER_WRITE_GATE, (local) => ({
              ...local,
              epoch: "1",
              recomputing: true,
            }));
          }
          const nodeConfig = yield* NodeConfig;
          yield* retentionSweepAction(VIEW, new Date()).pipe(
            Effect.provideService(NodeConfig, {
              ...nodeConfig,
              RETENTION_DAYS: undefined,
            }),
          );
          const rows = (table: string) =>
            sql.unsafe<{ id: Buffer | string }>(
              table === "state_queue_mutation_leases"
                ? `SELECT token AS id FROM ${table}`
                : `SELECT tx_id AS id FROM ${table}`,
            );
          const ids = (rowsOf: readonly { id: Buffer | string }[]) =>
            rowsOf.map((row) =>
              typeof row.id === "string" ? row.id : row.id.toString("hex"),
            );
          return {
            rejections: ids(yield* rows("tx_rejections")),
            addresses: ids(yield* rows("address_history")),
            leases: yield* leaseTokens,
            journals: yield* remainingLabels(JOURNALS),
            deposits: (yield* sql<{
              event_id: Buffer;
            }>`SELECT event_id FROM deposits_utxos`).map((row) =>
              row.event_id.toString("hex"),
            ),
            withdrawals: (yield* sql<{
              event_id: Buffer;
            }>`SELECT event_id FROM withdrawal_utxos`).map((row) =>
              row.event_id.toString("hex"),
            ),
          };
        }).pipe(Effect.ensuring(Effect.orDie(clear)));
      }).pipe(
        Effect.provideService(ContractDeploymentIdentity, {
          manifestId: DEPLOYMENT.toString("hex"),
          manifest: {
            da: { transportProfile: { retentionDays: MANIFEST_DAYS } },
          },
        } as never),
        Effect.provide(Globals.Default),
      ),
    ),
  );

const hex = (label: string) => header(`tx:${label}`).toString("hex");

describe("a retention sweep with RETENTION_DAYS unset (B5)", () => {
  it("prunes housekeeping by the verified manifest's window and keeps what is still needed", async () => {
    const now = new Date();
    // Precondition: the DA challenge horizon is shorter than the window, so
    // the manifest window alone decides these rows.
    expect(computeHousekeepingCutoff(now, MANIFEST_DAYS)).toEqual(
      computeRetentionCutoff(now, MANIFEST_DAYS),
    );
    const result = await sweep();
    expect(result.rejections).toEqual([hex("rejected-inside")]);
    expect(result.addresses.sort()).toEqual(
      [hex("inside"), hex("queued"), hex("processing")].sort(),
    );
    // The window is the manifest's, not the former 7-day placeholder.
    expect(result.leases).toContain("ended-inside");
    expect(result.leases).not.toContain("ended-outside");
    expect(result.leases).toHaveLength(101);
    // Under the database fixture capability; the terminal-named journal and
    // the newest are kept.
    expect(result.journals).toEqual(["terminal-named", "newest"]);
  });

  it("keeps every journal and still prunes the rest while the follower-change driver recomputes", async () => {
    const result = await sweep({ driverRecomputing: true });
    expect(result.journals).toEqual(JOURNALS);
    expect(result.rejections).toEqual([hex("rejected-inside")]);
    expect(result.leases).not.toContain("ended-outside");
  });
  it("keeps old awaiting, invalid and unpaid events and their completed same-header proof siblings", async () => {
    const result = await sweep();
    expect(result.deposits.sort()).toEqual(
      [
        "deposit-awaiting",
        "deposit-projected",
        "deposit-unsettled",
        "deposit-complete",
      ]
        .map(hex)
        .sort(),
    );
    expect(result.withdrawals.sort()).toEqual(
      [
        "withdrawal-awaiting",
        "withdrawal-projected",
        "withdrawal-invalid",
        "withdrawal-unsettled",
        "withdrawal-complete",
      ]
        .map(hex)
        .sort(),
    );
  });
});
