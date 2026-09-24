import { createHash, randomUUID } from "node:crypto";

import * as SDK from "@al-ft/midgard-sdk";
import { SqlClient } from "@effect/sql";
import { Data, toUnit } from "@lucid-evolution/lucid";
import {
  Cause,
  Deferred,
  Effect,
  Exit,
  Fiber,
  Option,
  Ref,
  type Scope,
} from "effect";
import { afterAll, beforeAll, beforeEach, describe, expect, it } from "vitest";

import * as Authority from "../src/database/eventHistoryAuthority.js";
import * as Journal from "../src/database/eventHistoryJournal.js";
import { prepareHistoryRecoveryPlan } from "../src/database/eventHistoryRecoveryPlans.js";
import { MempoolLedgerDB } from "../src/database/index.js";
import { formatDatabaseError } from "../src/database/utils/common.js";
import {
  decodeBoundEventHistoryLedgerSnapshot,
  type EventHistorySourceBinding,
  HISTORY_GENESIS_DIGEST_ALGORITHM,
} from "../src/l1-event-history-source.js";
import type { LedgerSnapshotOutput } from "../src/l1-ledger-snapshot.js";
import { NodeConfig } from "../src/services/config.js";
import { Database } from "../src/services/database.js";
import { makeEventHistoryRecovery } from "../src/services/event-history-recovery.js";
import { Globals } from "../src/services/globals.js";
import { executeHistoryDependentRecovery } from "../src/services/history-dependent-recovery.js";
import { makeMempoolLedgerCacheService } from "../src/services/mempool-ledger-cache.js";
import type { NativeMpfOwnerService } from "../src/services/mpf-native-owner/protocol.js";
import { loadRealMidgardContractsForTest } from "./helpers/real-midgard-contracts.js";
import { applyMidgardNodeTestEnv, testDatabaseName } from "./test-env.js";

// Ordering component test: actual SQL, recovery ownership, cache and Globals.
// L1 evidence is explicitly modeled as in recovery-plans.test; this test does
// not authorize a real rollback. Only the native RPC boundary is replaced.
applyMidgardNodeTestEnv();
const hash = (n: number) => n.toString(16).padStart(64, "0");
const sha = (value: string) => createHash("sha256").update(value).digest("hex");
const run = <A, E>(
  program: Effect.Effect<
    A,
    E,
    SqlClient.SqlClient | NodeConfig | Globals | Scope.Scope
  >,
) =>
  Effect.runPromise(
    program.pipe(
      Effect.scoped,
      Effect.provide(Globals.Default),
      Effect.provide(Database.layer),
      Effect.provide(NodeConfig.layer),
    ),
  );
let binding: EventHistorySourceBinding;
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
    endpointIdentitySha256: hash(904),
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
});

const cleanup = Effect.gen(function* () {
  const sql = yield* SqlClient.SqlClient;
  const [database] = yield* sql<{
    name: string;
  }>`SELECT current_database() AS name`;
  expect(database?.name).toBe(testDatabaseName());
  yield* sql`CREATE TABLE IF NOT EXISTS history_dependent_ordering_probe(label text PRIMARY KEY)`;
  yield* sql`TRUNCATE history_dependent_ordering_probe, event_history_recovery_plans, event_history_l2_ledger_receipts, mempool_ledger, deposits_utxos, withdrawal_utxos, pending_block_finalization_deposits, pending_block_finalization_withdrawals, event_history_cursor, event_history_block_applications, event_history_live_outputs, event_history_incarnations, event_history_authority, event_history_replay_receipts`;
});
beforeEach(async () => {
  await run(cleanup);
});
afterAll(async () => {
  await run(cleanup);
  await run(
    Effect.gen(function* () {
      const sql = yield* SqlClient.SqlClient;
      yield* sql`DROP TABLE history_dependent_ordering_probe`;
    }),
  );
});

const fixture = Effect.gen(function* () {
  const sql = yield* SqlClient.SqlClient;
  const globals = yield* Globals;
  const cache = yield* makeMempoolLedgerCacheService(
    globals,
    MempoolLedgerDB.retrieveSpendable.pipe(
      Effect.provideService(SqlClient.SqlClient, sql),
    ),
  );
  const recovery = yield* makeEventHistoryRecovery({
    deploymentIdentity: binding.manifestId,
    ownerToken: randomUUID(),
    leaseDurationMs: 60_000,
    cache,
  });
  const receipt =
    "Explicit modeled ordering-test source receipt; not ledger authority";
  yield* recovery.startup.persist(
    Journal.seed({
      binding,
      capture: initial,
      height: 1,
      originReceipt: receipt,
      originReceiptDigest: sha(receipt),
      incarnations: [],
    }),
  );
  const checkpoint = yield* Journal.load(binding);
  if (checkpoint === null) throw new Error("Missing ordering checkpoint");
  yield* Ref.set(globals.BLOCKS_IN_QUEUE, 1);
  yield* Ref.set(globals.LATEST_LOCAL_BLOCK_END_TIME_MS, 900);
  const callbackOutsideTransaction = yield* Ref.make(false);
  const afterSqlCommit = Effect.gen(function* () {
    const tx = yield* Effect.serviceOption(SqlClient.TransactionConnection);
    yield* Ref.set(callbackOutsideTransaction, Option.isNone(tx));
    yield* Ref.set(globals.LATEST_LOCAL_BLOCK_END_TIME_MS, 100);
    yield* Ref.set(globals.BLOCKS_IN_QUEUE, 0);
  });
  const prepare = (token: Authority.Token) =>
    Authority.withRecovery(
      token,
      prepareHistoryRecoveryPlan(
        checkpoint,
        {
          bindingDigest: binding.digest,
          manifestId: binding.manifestId,
          headerHash: "ab".repeat(28),
          signedTransactionHash: hash(20),
          signedTransactionCborSha256: hash(21),
          expectedRoot: hash(22),
          targetRoot: hash(23),
          journalDigest: hash(24),
        },
        hash(25),
      ),
    );
  const inspect = Effect.gen(function* () {
    const plans = yield* sql<{
      state: string;
    }>`SELECT state FROM event_history_recovery_plans`;
    const probes = yield* sql<{
      label: string;
    }>`SELECT label FROM history_dependent_ordering_probe ORDER BY label`;
    return { plans, probes };
  });
  const assertFenced = Effect.gen(function* () {
    const authority = yield* Authority.retrieve;
    expect(Option.isSome(authority)).toBe(true);
    if (Option.isSome(authority))
      expect(authority.value.state).toBe("recovering");
    expect(
      (yield* Effect.either(recovery.runProducer(() => Effect.void)))._tag,
    ).toBe("Left");
    expect((yield* Effect.either(cache.withClaimLock(Effect.void)))._tag).toBe(
      "Left",
    );
  });
  return {
    sql,
    globals,
    recovery,
    checkpoint,
    callbackOutsideTransaction,
    afterSqlCommit,
    prepare,
    inspect,
    assertFenced,
  };
});
const native = (
  restoreCanonicalRoot: NativeMpfOwnerService["restoreCanonicalRoot"],
): NativeMpfOwnerService => ({ restoreCanonicalRoot }) as NativeMpfOwnerService;

describe("dependent recovery SQL and postcommit publication ordering", () => {
  it("commits repair and applied receipt and refreshes Globals despite cancellation pending across COMMIT", async () => {
    await run(
      Effect.gen(function* () {
        const h = yield* fixture;
        yield* h.recovery.startup.prepare((preparation) =>
          Effect.gen(function* () {
            const plan = yield* h.prepare(preparation.token);
            const entered = yield* Deferred.make<void>();
            const release = yield* Deferred.make<void>();
            const nativeCalls: unknown[] = [];
            const executing = yield* Effect.fork(
              executeHistoryDependentRecovery({
                checkpoint: h.checkpoint,
                preparation,
                plan,
                owner: native(async (operation) => {
                  nativeCalls.push(operation);
                }),
                repair: Effect.gen(function* () {
                  yield* h.sql`INSERT INTO history_dependent_ordering_probe(label) VALUES ('repaired')`;
                  yield* Deferred.succeed(entered, undefined);
                  yield* Deferred.await(release);
                }),
                afterSqlCommit: h.afterSqlCommit,
              }),
            );
            yield* Deferred.await(entered);
            expect(nativeCalls).toEqual([plan.native]);
            // This parent fiber has no TransactionConnection: PostgreSQL must keep
            // both effects invisible until the actual recovery transaction commits.
            expect(yield* h.inspect).toEqual({
              plans: [{ state: "prepared" }],
              probes: [],
            });
            expect(yield* Ref.get(h.globals.BLOCKS_IN_QUEUE)).toBe(1);
            yield* Fiber.interruptFork(executing);
            yield* Deferred.succeed(release, undefined);
            const result = yield* Fiber.await(executing);
            expect(Exit.isFailure(result)).toBe(true);
            if (Exit.isFailure(result))
              expect(Cause.isInterrupted(result.cause)).toBe(true);
            expect(yield* h.inspect).toEqual({
              plans: [{ state: "applied" }],
              probes: [{ label: "repaired" }],
            });
            expect(yield* Ref.get(h.callbackOutsideTransaction)).toBe(true);
            expect(yield* Ref.get(h.globals.BLOCKS_IN_QUEUE)).toBe(0);
            expect(
              yield* Ref.get(h.globals.LATEST_LOCAL_BLOCK_END_TIME_MS),
            ).toBe(100);
            yield* h.assertFenced;
          }),
        );
      }),
    );
  });

  it("rolls back an actual SQL constraint failure without publishing Globals or applying the plan", async () => {
    await run(
      Effect.gen(function* () {
        const h = yield* fixture;
        yield* h.recovery.startup.prepare((preparation) =>
          Effect.gen(function* () {
            const plan = yield* h.prepare(preparation.token);
            const nativeCalls: unknown[] = [];
            const result = yield* Effect.either(
              executeHistoryDependentRecovery({
                checkpoint: h.checkpoint,
                preparation,
                plan,
                owner: native(async (operation) => {
                  nativeCalls.push(operation);
                }),
                repair: Effect.gen(function* () {
                  yield* h.sql`INSERT INTO history_dependent_ordering_probe(label) VALUES ('duplicate')`;
                  yield* h.sql`INSERT INTO history_dependent_ordering_probe(label) VALUES ('duplicate')`;
                }),
                afterSqlCommit: h.afterSqlCommit,
              }),
            );
            expect(result._tag).toBe("Left");
            if (result._tag === "Left")
              expect(formatDatabaseError(result.left)).toContain(
                "duplicate key",
              );
            expect(nativeCalls).toEqual([plan.native]);
            expect(yield* h.inspect).toEqual({
              plans: [{ state: "prepared" }],
              probes: [],
            });
            expect(yield* Ref.get(h.callbackOutsideTransaction)).toBe(false);
            expect(yield* Ref.get(h.globals.BLOCKS_IN_QUEUE)).toBe(1);
            expect(
              yield* Ref.get(h.globals.LATEST_LOCAL_BLOCK_END_TIME_MS),
            ).toBe(900);
            yield* h.assertFenced;
          }),
        );
      }),
    );
  });

  it("retains prepared evidence and untouched SQL/Globals when native restoration fails", async () => {
    await run(
      Effect.gen(function* () {
        const h = yield* fixture;
        yield* h.recovery.startup.prepare((preparation) =>
          Effect.gen(function* () {
            const plan = yield* h.prepare(preparation.token);
            const result = yield* Effect.either(
              executeHistoryDependentRecovery({
                checkpoint: h.checkpoint,
                preparation,
                plan,
                owner: native(async () => {
                  throw new Error("Explicit native RPC failure");
                }),
                repair:
                  h.sql`INSERT INTO history_dependent_ordering_probe(label) VALUES ('must-not-run')`.pipe(
                    Effect.asVoid,
                  ),
                afterSqlCommit: h.afterSqlCommit,
              }),
            );
            expect(result._tag).toBe("Left");
            if (result._tag === "Left")
              expect(formatDatabaseError(result.left)).toContain(
                "Explicit native RPC failure",
              );
            expect(yield* h.inspect).toEqual({
              plans: [{ state: "prepared" }],
              probes: [],
            });
            expect(yield* Ref.get(h.callbackOutsideTransaction)).toBe(false);
            expect(yield* Ref.get(h.globals.BLOCKS_IN_QUEUE)).toBe(1);
            expect(
              yield* Ref.get(h.globals.LATEST_LOCAL_BLOCK_END_TIME_MS),
            ).toBe(900);
            yield* h.assertFenced;
          }),
        );
      }),
    );
  });
});
