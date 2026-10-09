/**
 * The node services for `commit-replacement-state-queue-emulator.test.ts`:
 * the follower run's landed-block processing (at the follower's view, as
 * the production hook, under the driver's write capability, with its
 * recompute as the rebase), the own-commit disposition (the rebase it makes
 * due) and the S6 pass over the follower host's facts
 * (`emulator-l1-follower.ts`).
 */
import {
  appendIntentEventIn,
  createIntentReconciler,
  currentViewIn,
  decodeTransaction,
  type FactStore,
  postgresDialect,
} from "@al-ft/midgard-l1-follower";
import { eventProjectionConfigFromContracts } from "@al-ft/midgard-l1-follower/events";
import { SqlClient } from "@effect/sql";
import { Effect, Layer, ManagedRuntime } from "effect";

import { followerSqlTx } from "../../src/database/follower-schema.js";
import { forcedOrderConfigFromContracts } from "../../src/forced-orders/index.js";
import { readLandedStateQueueFrom } from "../../src/l1-state-queue/hook.js";
import { stateQueueProjectionConfig } from "../../src/l1-state-queue/index.js";
import { nodeLandedBlockPorts } from "../../src/landed-blocks/node-ports.js";
import { processLandedQueue } from "../../src/landed-blocks/process.js";
import { NodeConfig } from "../../src/services/config.js";
import { Database } from "../../src/services/database.js";
import { withDriverView } from "../../src/services/follower-write-gate.driver.js";
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
import { syncEmulatorChain } from "./emulator-l1-follower.js";
import { nodeFactStore } from "./emulator-operator-set.js";

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
   * The follower store at the emulator's tip: what the commit spends or
   * references are its facts (§8.2 journals only over tracked facts).
   */
  const followTip = () =>
    runtime.runPromise(syncEmulatorChain(fixture.operatorLucid));
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
          yield* syncEmulatorChain(fixture.operatorLucid);
          const store = yield* nodeFactStore;
          // The production hook runs at the change's view, also when the
          // driver's sink left that view held (orphans of a rewind).
          const view = yield* Effect.promise(() =>
            store.transaction("read", (tx) => currentViewIn(tx, store.dialect)),
          );
          if (view === null) throw new Error("The follower has no view");
          const read = yield* Effect.promise(() =>
            readLandedStateQueueFrom(store, plan.stateQueue, view),
          );
          if (read.kind !== "ok" || !read.queue.healthy)
            throw new Error(`The landed queue is unreadable: ${read.kind}`);
          return yield* withDriverView(view)(
            processLandedQueue(
              nodeLandedBlockPorts(store as FactStore, plan, (reason) =>
                Effect.promise(() => life.rebaseIfDue(reason)),
              ),
              read.queue,
            ),
          );
        }),
      ),
    followTip,
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
     * The follower at the emulator's tip, holding `signed`'s landing (its
     * `l1_txs` row): S6 derives a landed own commit `landed`, not expired,
     * and its change live.
     */
    recordLanding: async (signed: Buffer) => {
      await followTip();
      const [landed] = await query(
        (sql) => sql<{ n: string }>`SELECT count(*)::text AS n FROM l1_txs
          WHERE tx_hash = ${decodeTransaction(signed).hash}`,
      );
      if (landed?.n !== "1")
        throw new Error("The follower holds no landing for the commit");
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
