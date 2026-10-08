/**
 * The node services for `commit-replacement-state-queue-emulator.test.ts`:
 * the follower run's landed-block processing (at the driver's applied view,
 * under its write capability, with its recompute as the rebase), the
 * own-commit disposition (the rebase it makes due) and the S6 pass over the
 * follower stand-in.
 */
import {
  appendIntentEventIn,
  createIntentReconciler,
  decodeTransaction,
  encodeOutRef,
  type FactStore,
  postgresDialect,
} from "@al-ft/midgard-l1-follower";
import { eventProjectionConfigFromContracts } from "@al-ft/midgard-l1-follower/events";
import { SqlClient } from "@effect/sql";
import type { PgClient } from "@effect/sql-pg/PgClient";
import { Effect, Layer, ManagedRuntime } from "effect";

import { followerSqlTx } from "../../src/database/follower-schema.js";
import { forcedOrderConfigFromContracts } from "../../src/forced-orders/index.js";
import { readLandedStateQueueFrom } from "../../src/l1-state-queue/hook.js";
import { stateQueueProjectionConfig } from "../../src/l1-state-queue/index.js";
import { nodeLandedBlockPorts } from "../../src/landed-blocks/node-ports.js";
import { processLandedQueue } from "../../src/landed-blocks/process.js";
import { bytea } from "../../src/landed-blocks/store.js";
import { NodeConfig } from "../../src/services/config.js";
import { Database } from "../../src/services/database.js";
import { withDriverView } from "../../src/services/follower-write-gate.driver.js";
import {
  followerViewOf,
  readFollowerWriteGate,
} from "../../src/services/follower-write-gate.js";
import { Globals } from "../../src/services/globals.js";
import {
  makeIntentJournal,
  readIntentStatus,
} from "../../src/services/intent-journal.js";
import { nodeFamilyPredicate } from "../../src/services/l1-follower.intent-predicates.js";
import { RESUBMIT_REJECTIONS_TO_ABANDON } from "../../src/services/l1-follower.intents.js";
import { Lucid } from "../../src/services/lucid.js";
import { ContractDeploymentIdentity } from "../../src/services/midgard-contracts.js";
import { selectNodeWallet } from "../../src/transactions/utils.wallet-view.js";
import { SDK } from "../deposit-flow-emulator-shared.js";
import type { Lifecycle } from "./correction-admission-scenario.js";
import { nodeFactStore } from "./emulator-operator-set.js";
import {
  mirrorEmulatorStateQueue,
  writeAddressFacts,
} from "./landed-state-queue.js";

/** The node services the follower run uses, over the lifecycle's globals. */
export const openNodeServices = async (life: Lifecycle) => {
  const { fixture, lucidService } = life;
  const contracts = fixture.contracts;
  // As the node's Lucid service does: under a follower the node signs over
  // its wallet view, so its wallets are selected with `selectNodeWallet`.
  const seed = fixture.operatorAccount.seedPhrase;
  const selectOperator = Effect.sync(() =>
    selectNodeWallet(fixture.operatorLucid, seed),
  );
  const nodeLucid: typeof lucidService = {
    ...lucidService,
    switchToOperatorsMainWallet: selectOperator,
    switchToOperatorsMergingWallet: selectOperator,
  };
  Effect.runSync(selectOperator);
  const operatorAddress = await fixture.operatorLucid.wallet().address();
  const runtime = ManagedRuntime.make(
    Layer.mergeAll(
      Layer.succeed(NodeConfig, life.production.nodeConfig),
      Layer.succeed(Globals, life.globals),
      Layer.succeed(
        Lucid,
        Lucid.make({
          ...nodeLucid,
          operatorMainAddress: operatorAddress,
          operatorMergeAddress: operatorAddress,
          referenceScriptsWalletAddress: await fixture.referenceScriptsLucid
            .wallet()
            .address(),
        }),
      ),
      Layer.succeed(
        ContractDeploymentIdentity,
        ContractDeploymentIdentity.make(
          fixture.runtimeOverrides!.deploymentIdentity,
        ),
      ),
      Database.layer,
    ),
  );
  const plan = {
    projection: eventProjectionConfigFromContracts(
      SDK.requireEventHistoryContracts(contracts),
      0,
    ),
    forcedOrders: forcedOrderConfigFromContracts(contracts),
    stateQueue: stateQueueProjectionConfig(contracts.stateQueue),
  };
  /**
   * What the commit spends or references, as follower facts (§8.2 journals
   * only over tracked facts): the operator wallet (its wallet view), the
   * reference-script wallet, and the protocol nodes the commit reads.
   */
  const trackedAddresses = [
    operatorAddress,
    await fixture.referenceScriptsLucid.wallet().address(),
    contracts.activeOperators.spendingScriptAddress,
    contracts.hubOracle.spendingScriptAddress,
    contracts.correctionLock.spendingScriptAddress,
    contracts.scheduler.spendingScriptAddress,
  ];
  const mirrorTracked = async () => {
    const lucid = fixture.operatorLucid;
    for (const address of trackedAddresses)
      await runtime.runPromise(
        writeAddressFacts(
          address,
          await lucid.utxosAt(address),
          lucid.currentSlot(),
        ),
      );
  };
  const query = <A>(
    program: (sql: SqlClient.SqlClient) => Effect.Effect<A, unknown>,
  ) => runtime.runPromise(Effect.flatMap(SqlClient.SqlClient, program));
  return {
    nodeLucid,
    /** The production journal (`IntentJournalLive`) over the node database. */
    journal: await runtime.runPromise(makeIntentJournal),
    /** The follower run's landed-block hook at the driver's applied view. */
    processLanded: () =>
      runtime.runPromise(
        Effect.gen(function* () {
          yield* mirrorEmulatorStateQueue(
            fixture.operatorLucid,
            contracts.stateQueue,
          );
          const store = yield* nodeFactStore;
          const read = yield* Effect.promise(() =>
            readLandedStateQueueFrom(store, plan.stateQueue),
          );
          if (read.kind !== "ok" || !read.queue.healthy)
            throw new Error(`The landed queue is unreadable: ${read.kind}`);
          const gate = yield* readFollowerWriteGate;
          if (gate.applied === undefined)
            throw new Error("The driver has applied no view");
          return yield* withDriverView(followerViewOf(gate.applied))(
            processLandedQueue(
              nodeLandedBlockPorts(store as FactStore, plan, (reason) =>
                Effect.promise(() => life.rebaseIfDue(reason)),
              ),
              read.queue,
            ),
          );
        }),
      ),
    mirrorTracked,
    /** The follower run's own-commit disposition after S6: the driver's
     * recompute when the rebase it makes due can run. */
    disposeDead: () =>
      life.rebaseIfDue("S6 derived the status of this node's own commits"),
    intentStatus: (txHash: string) =>
      runtime.runPromise(readIntentStatus(txHash)),
    /** S6's abandon event for `txHash`, at the follower's cursor. */
    abandon: (txHash: string) =>
      runtime.runPromise(
        Effect.gen(function* () {
          const sql = yield* SqlClient.SqlClient;
          const tx = yield* followerSqlTx;
          const [cursor] = yield* sql<{
            slot: string;
          }>`SELECT slot::text AS slot FROM l1_follower_cursor`;
          yield* Effect.promise(() =>
            appendIntentEventIn(
              tx,
              postgresDialect,
              Buffer.from(txHash, "hex"),
              "abandoned",
              {
                detail: { reason: "family_predicate_false" },
                tipSlot: Number(cursor!.slot),
              },
            ),
          );
        }),
      ),
    /**
     * The follower's landing fact for `signed` (an `l1_txs` row at the
     * cursor's block), which the stand-in does not write: S6 derives a
     * landed own commit `landed`, not expired, and its change live.
     */
    recordLanding: async (signed: Buffer) => {
      await mirrorTracked();
      const tx = decodeTransaction(signed);
      const refs = (outRefs: typeof tx.inputs) =>
        bytea(outRefs.map((outRef) => encodeOutRef(outRef)));
      await query((sql) => {
        const pg = sql as PgClient;
        return sql`INSERT INTO l1_txs (tx_hash, block_slot, block_tx_index,
            is_valid, inputs, reference_inputs, collaterals, output_count,
            has_collateral_return, mint, withdrawals, redeemers,
            invalid_before, invalid_after, body_cbor, witness_cbor)
          SELECT ${tx.hash}, c.slot,
            (SELECT count(*) FROM l1_txs t WHERE t.block_slot = c.slot), true,
            ${pg.array(refs(tx.inputs))}::bytea[],
            ${pg.array(refs(tx.referenceInputs))}::bytea[],
            ${pg.array(refs(tx.collaterals))}::bytea[],
            ${tx.outputs.length}, false, '{}'::jsonb, '{}'::jsonb, '[]'::jsonb,
            NULL, NULL, ${tx.bodyCbor}, ${Buffer.alloc(0)}
          FROM l1_follower_cursor c`;
      });
    },
    landedRow: async (headerHash: string) =>
      (
        await query(
          (sql) => sql<{
            kind: string;
            state: string;
            applied: boolean;
          }>`SELECT kind, state, applied FROM node_landed_blocks
            WHERE header_hash = ${Buffer.from(headerHash, "hex")}`,
        )
      )[0],
    /**
     * One S6 reconcile pass, as the node's intent stage runs it
     * (`createNodeIntentStage`) with the §8.4 family predicates, over a
     * transport that sees the emulator's mempool and records sends.
     */
    reconcileIntents: async () => {
      const sent: string[] = [];
      const report = await runtime.runPromise(
        Effect.gen(function* () {
          const store = yield* nodeFactStore;
          const reconciler = createIntentReconciler({
            dialect: store.dialect,
            transaction: (mode, run) => store.transaction(mode, run),
            // Only splits landed intents into followed and final; no
            // decision this journey asserts depends on it.
            securityParameter: 2160,
            abandonAfterRejections: RESUBMIT_REJECTIONS_TO_ABANDON,
            inMempool: (intent) =>
              Promise.resolve(
                intent.txHash.toString("hex") in fixture.emulator.mempool,
              ),
            wanted: nodeFamilyPredicate({
              store,
              stateQueue: plan.stateQueue,
              operatorSet: null,
              slotToPosixMs: (slot) =>
                fixture.operatorLucid.slotToUnixTime(slot),
              horizonLagBlocks: 0,
            }),
            submit: (intent) => {
              sent.push(intent.txHash.toString("hex"));
              return Promise.resolve({ kind: "accepted" as const });
            },
          });
          return yield* Effect.promise(() => reconciler.reconcile());
        }),
      );
      return { report, sent };
    },
    close: () => runtime.dispose(),
  };
};
