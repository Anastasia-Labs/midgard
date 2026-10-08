/**
 * The node's landed-block ports: the history producer gate for writes, the
 * follower store for the view check, the foreign replay, the node's block
 * journals and merge finalization, and the history owner for the rebase.
 */
import {
  type FactStore,
  postgresDialect,
  type View,
  viewValidQuery,
} from "@al-ft/midgard-l1-follower";
import { SqlClient } from "@effect/sql";
import { Effect, Ref } from "effect";

import { numbered } from "../database/follower-schema.js";
import { MutationJobsDB } from "../database/index.js";
import type { ForcedOrderConfig } from "../forced-orders/index.js";
import type { EventProjectionConfig } from "../l1-events/config.js";
import type { StateQueueProjectionConfig } from "../l1-state-queue/index.js";
import { utxoToLedgerInsertMaterial } from "../mpf/ledger-hydration.js";
import { NodeConfig } from "../services/config.js";
import {
  runHistoryProducer,
  withHistoryWrite,
} from "../services/event-history-producer.js";
import { Globals } from "../services/globals.globals.js";
import { finalizeConfirmedMergeProgram } from "../transactions/state-queue/merge-to-confirmed-state.finalize-confirmed-merge-program.js";
import { readQueueHistory } from "./history.js";
import { ownJournal } from "./journal.js";
import { ledgerRows } from "./ledger.js";
import type { LandedBlockPorts } from "./ports.js";
import { rebasePlan } from "./rebase-target.js";
import { replayForeignBlock } from "./replay-foreign.js";

const confirmView = (view: View) =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const check = viewValidQuery(postgresDialect, view);
    const valid = yield* sql.unsafe<{ valid: boolean }>(
      numbered(check.sql),
      check.params as never,
    );
    return valid[0]?.valid === true;
  });

const genesis = Effect.gen(function* () {
  const config = yield* NodeConfig;
  const entries = [];
  for (const utxo of config.GENESIS_UTXOS) {
    const material = yield* utxoToLedgerInsertMaterial(utxo);
    entries.push({
      outref: material.ledgerOp.key,
      output: material.outputCbor,
    });
  }
  return yield* ledgerRows(entries, new Map());
});

const requestRebase = (reason: string) =>
  Effect.gen(function* () {
    const plan = yield* rebasePlan;
    if (plan.kind === "blocked") return plan.detail;
    if (plan.kind === "none") return undefined;
    const globals = yield* Globals;
    const owner = yield* Ref.get(globals.EVENT_HISTORY_OWNER);
    if (owner === undefined) return "the history owner is not running yet";
    // A recovery already running reaches the rebase through its reconcile;
    // asking again would restart it.
    if ((yield* owner.frontier).ready)
      yield* owner.requestReconciliation(reason);
    return undefined;
  });

/** The node's landed-block ports over `store`, with the follower plan's configs. */
export const nodeLandedBlockPorts = (
  store: FactStore,
  plan: {
    readonly projection: EventProjectionConfig;
    readonly forcedOrders: ForcedOrderConfig;
    readonly stateQueue: StateQueueProjectionConfig;
  },
) =>
  ({
    confirmView,
    queueHistory: (view) => readQueueHistory(store, plan.stateQueue, view),
    write: (work) => runHistoryProducer(withHistoryWrite(work)),
    replay: replayForeignBlock({
      store,
      events: plan.projection,
      forcedOrders: plan.forcedOrders,
    }),
    ownJournal,
    ownMergeCompleted: (headerHash) =>
      MutationJobsDB.retrieveByJobId(
        MutationJobsDB.confirmedMergeFinalizationJobId(headerHash),
      ).pipe(
        Effect.map(
          (job) =>
            job?.[MutationJobsDB.Columns.STATUS] ===
            MutationJobsDB.Status.Completed,
        ),
      ),
    finalizeOwnMerge: (input) =>
      runHistoryProducer(finalizeConfirmedMergeProgram(input)),
    genesis,
    requestRebase,
    rebaseFailure: Effect.flatMap(Globals, (globals) =>
      Ref.get(globals.LANDED_BLOCK_REBASE_FAILURE),
    ),
  }) satisfies LandedBlockPorts<NodeLandedBlockContext>;

export type NodeLandedBlockContext = Effect.Effect.Context<
  ReturnType<ReturnType<typeof replayForeignBlock>>
>;
