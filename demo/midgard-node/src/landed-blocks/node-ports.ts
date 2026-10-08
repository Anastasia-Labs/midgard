/**
 * The node's landed-block ports: the history producer gate for writes, the
 * follower store for the view check, the foreign replay, the node's block
 * journals, and the history owner for the rebase.
 */
import type { FactStore } from "@al-ft/midgard-l1-follower";
import type { EventProjectionConfig } from "@al-ft/midgard-l1-follower/events";
import { Effect, Ref } from "effect";

import { followerViewValid } from "../database/follower-schema.js";
import type { ForcedOrderConfig } from "../forced-orders/index.js";
import type { DriverHold } from "../l1-events/driver.js";
import type { StateQueueProjectionConfig } from "../l1-state-queue/index.js";
import { utxoToLedgerInsertMaterial } from "../mpf/ledger-hydration.js";
import { NodeConfig } from "../services/config.js";
import {
  runHistoryProducer,
  withHistoryWrite,
} from "../services/event-history-producer.js";
import { Globals } from "../services/globals.globals.js";
import { readQueueHistory } from "./history.js";
import { LANDED_BLOCK_REBASE_PENDING } from "./holds.js";
import { ownJournal } from "./journal.js";
import { ledgerRows } from "./ledger.js";
import type { LandedBlockPorts } from "./ports.js";
import { rebasePlan } from "./rebase-target.js";
import { replayForeignBlock } from "./replay-foreign.js";

const confirmView = followerViewValid;

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

/**
 * Asks the history owner for the landed-block rebase when one is due: a
 * processed landed row the working ledger lags, or an own journal it
 * disposes of or revives. Returns why it cannot run yet, if it cannot.
 */
export const requestRebase = (reason: string) =>
  Effect.gen(function* () {
    const plan = yield* rebasePlan;
    if (plan.kind === "blocked")
      return {
        reason: plan.reason ?? LANDED_BLOCK_REBASE_PENDING,
        detail: plan.detail,
      } satisfies DriverHold;
    if (plan.kind === "none") return undefined;
    const globals = yield* Globals;
    const owner = yield* Ref.get(globals.EVENT_HISTORY_OWNER);
    if (owner === undefined)
      return {
        reason: LANDED_BLOCK_REBASE_PENDING,
        detail: "the history owner is not running yet",
      } satisfies DriverHold;
    // A recovery already running reaches the rebase through its reconcile;
    // asking again would restart it.
    if ((yield* owner.frontier).ready)
      yield* owner.requestReconciliation(reason);
    return undefined;
  });

/**
 * The follower run's own-commit disposition, after S6: an own commit it
 * derives dead, or one holding an event whose admission left the chain, is
 * disposed of by the rebase asked for here, so the commit path builds its
 * replacement on the next tick (whichever lands wins). Never fails.
 */
export const disposeDeadOwnCommits = requestRebase(
  "S6 derived the status of this node's own commits",
).pipe(
  Effect.flatMap((held) =>
    held === undefined || held.reason === LANDED_BLOCK_REBASE_PENDING
      ? Effect.void
      : Effect.logWarning(
          `The own-commit disposition is held (${held.reason}): ${held.detail}`,
        ),
  ),
  Effect.catchAllCause((cause) =>
    Effect.logWarning(
      "The own-commit disposition could not be read; the next follower run reads it again",
      cause,
    ),
  ),
);

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
    genesis,
    requestRebase,
    rebaseFailure: Effect.flatMap(Globals, (globals) =>
      Ref.get(globals.LANDED_BLOCK_REBASE_FAILURE),
    ),
  }) satisfies LandedBlockPorts<NodeLandedBlockContext>;

export type NodeLandedBlockContext = Effect.Effect.Context<
  ReturnType<ReturnType<typeof replayForeignBlock>>
>;
