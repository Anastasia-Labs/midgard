/**
 * The node services and the rollback handle for
 * `commit-replacement-state-queue-emulator.test.ts`: the follower run's
 * landed-block processing, own-commit disposition and S6 pass over the
 * follower stand-in, and a lifecycle handle that follows a fork.
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
import { expect, vi } from "vitest";

import { followerSqlTx } from "../../src/database/follower-schema.js";
import { forcedOrderConfigFromContracts } from "../../src/forced-orders/index.js";
import { readLandedStateQueueFrom } from "../../src/l1-state-queue/hook.js";
import { stateQueueProjectionConfig } from "../../src/l1-state-queue/index.js";
import {
  disposeDeadOwnCommits,
  nodeLandedBlockPorts,
} from "../../src/landed-blocks/node-ports.js";
import { processLandedQueue } from "../../src/landed-blocks/process.js";
import { bytea } from "../../src/landed-blocks/store.js";
import { NodeConfig } from "../../src/services/config.js";
import { Database } from "../../src/services/database.js";
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
import type { Lifecycle } from "./correction-rewind-scenario.js";
import { nodeFactStore } from "./emulator-operator-set.js";
import type { emulatorState } from "./emulator-snapshot.js";
import {
  captureConfirmedHistoryObservations,
  historyOutputObservation,
} from "./history-projection-observations.js";
import type { makeRollbackHistoryTransport } from "./history-rollback-transport.js";
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
    /** The follower run's landed-block hook at the follower's current view. */
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
          return yield* processLandedQueue(
            nodeLandedBlockPorts(store as FactStore, plan),
            read.queue,
          );
        }),
      ),
    mirrorTracked,
    /** The follower run's own-commit disposition, after S6. */
    disposeDead: () => runtime.runPromise(disposeDeadOwnCommits),
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

/**
 * (c)'s rollback: the emulator back at `ancestor`, the history source
 * rolled back to its point, and a handle whose synchronize follows the
 * fork (the sealed recording cannot be extended past a rollback).
 */
export const followFork = async ({
  h,
  source: fork,
  ancestor,
}: {
  h: Lifecycle;
  source: ReturnType<typeof makeRollbackHistoryTransport>;
  ancestor: {
    id: string;
    height: number;
    state: ReturnType<typeof emulatorState>;
  };
}): Promise<{
  fork: Lifecycle;
  observer: Pick<Lifecycle["observer"], "forgetDropped">;
}> => {
  const { fixture, binding } = h;
  const emulator = fixture.emulator;
  h.observer.restore();
  Object.assign(emulator, structuredClone(ancestor.state));
  fixture.operatorLucid.clearUTxOOverride();
  vi.setSystemTime(emulator.now());
  fork.rollbackTo(ancestor.id);
  // Until the owner has read the rollback, its head is the old branch's,
  // whose slot is past the fork's first point: waiting there would fail as
  // superseded rather than wait.
  for (let polls = 0; ; polls += 1) {
    const frontier = await Effect.runPromise(h.production.owner.frontier);
    if (!frontier.ready || frontier.headHeight === ancestor.height) break;
    if (polls > 1_500) throw new Error("The owner never read the rollback");
    await new Promise((resolve) => setTimeout(resolve, 10));
  }
  const addresses = [
    ...new Set([
      binding.hubAddress,
      ...Object.values(binding.deployments).flatMap(
        ({ address, retentionAddress }) => [address, retentionAddress],
      ),
      fixture.contracts.stateQueue.spendingScriptAddress,
    ]),
  ];
  type Batch = Parameters<typeof fork.appendFork>[0];
  const batch = async (
    observations: Batch["observations"],
  ): Promise<Batch> => ({
    observations,
    observedSlot: emulator.slot,
    observedHeight: emulator.blockHeight,
    outputs: (
      await Promise.all(
        addresses.map((address) => fixture.operatorLucid.utxosAt(address)),
      )
    )
      .flat()
      .map(historyOutputObservation),
  });
  const batches: Batch[] = [];
  const forkObserver = captureConfirmedHistoryObservations(
    fixture.operatorLucid,
    emulator,
    async (observations) => {
      batches.push(await batch(observations));
    },
  );
  const synchronize = async () => {
    await forkObserver.flush();
    expect(forkObserver.pendingCount()).toBe(0);
    expect(Object.keys(emulator.mempool)).toHaveLength(0);
    for (const next of batches.splice(0)) fork.appendFork(next);
    const tip = fork.points.at(-1)!.point;
    if (emulator.slot > tip.slot || tip.id === ancestor.id) {
      if (emulator.slot <= tip.slot || emulator.blockHeight <= tip.height)
        emulator.awaitBlock(1);
      fork.appendFork(await batch([]));
    }
    vi.setSystemTime(emulator.now());
    const point = fork.points.at(-1)!.point;
    expect((await h.readyAt(point)).point.id).toBe(point.id);
  };
  const command: Lifecycle["command"] = async (effect) => {
    const result = await h.runWithoutSynchronizing(effect);
    await synchronize();
    return result;
  };
  return {
    fork: {
      ...h,
      synchronize,
      command,
      production: { ...h.production, synchronize, onCommitAttempt: () => {} },
    },
    observer: forkObserver,
  };
};
