/**
 * The node's landed-block ports: the follower-change driver's write
 * capability (the hook's run provides it) for writes, the follower store
 * for the view check, the foreign replay, the node's block journals, and
 * the driver's recompute for the rebase.
 */
import type { FactStore } from "@al-ft/midgard-l1-follower";
import type { EventProjectionConfig } from "@al-ft/midgard-l1-follower/events";
import { Effect } from "effect";

import { followerViewValid } from "../database/follower-schema.js";
import type { ForcedOrderConfig } from "../forced-orders/index.js";
import type { DriverHold } from "../l1-events/driver.js";
import type { StateQueueProjectionConfig } from "../l1-state-queue/index.js";
import { utxoToLedgerInsertMaterial } from "../mpf/ledger-hydration.js";
import { NodeConfig } from "../services/config.js";
import { withFollowerWrite } from "../services/follower-write-gate.js";
import { readQueueHistory } from "./history.js";
import { ownJournal } from "./journal.js";
import { ledgerRows } from "./ledger.js";
import type { LandedBlockPorts } from "./ports.js";
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
 * The node's landed-block ports over `store`, with the follower plan's
 * configs and the driver's recompute as the rebase.
 */
export const nodeLandedBlockPorts = (
  store: FactStore,
  plan: {
    readonly projection: EventProjectionConfig;
    readonly forcedOrders: ForcedOrderConfig;
    readonly stateQueue: StateQueueProjectionConfig;
  },
  rebase: (reason: string) => Effect.Effect<DriverHold | undefined>,
) =>
  ({
    confirmView,
    queueHistory: (view) => readQueueHistory(store, plan.stateQueue, view),
    write: (work) => withFollowerWrite(work),
    replay: replayForeignBlock({
      store,
      events: plan.projection,
      forcedOrders: plan.forcedOrders,
    }),
    ownJournal,
    genesis,
    rebase,
  }) satisfies LandedBlockPorts<NodeLandedBlockContext>;

export type NodeLandedBlockContext = Effect.Effect.Context<
  ReturnType<ReturnType<typeof replayForeignBlock>>
>;
