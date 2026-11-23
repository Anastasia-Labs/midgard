/**
 * The node's landed-block ports: the follower-change driver's write
 * capability (the hook's run provides it) for writes, the follower store
 * for the view check, the foreign replay, the node's block journals, and
 * the driver's recompute for the rebase, the advance of the node's
 * confirmed-ledger verification over each committed fold, and the native
 * MPF's ledger retention.
 */
import type { FactStore, OriginConfig, View } from "@al-ft/midgard-l1-follower";
import type { EventProjectionConfig } from "@al-ft/midgard-l1-follower/events";
import type * as SDK from "@al-ft/midgard-sdk/da";
import { Effect, Ref } from "effect";

import { followerViewValid } from "../database/follower-schema.js";
import type { ForcedOrderConfig } from "../forced-orders/index.js";
import type { DriverHold } from "../l1-events/driver.js";
import type { StateQueueProjectionConfig } from "../l1-state-queue/index.js";
import { advanceVerifiedConfirmedLedger } from "../mpf/confirmed-ledger-advance.js";
import { utxoToLedgerInsertMaterial } from "../mpf/ledger-hydration.js";
import { NodeConfig } from "../services/config.js";
import { withFollowerWrite } from "../services/follower-write-gate.js";
import type { Globals } from "../services/globals.js";
import { readQueueHistory } from "./history.js";
import { ownJournal } from "./journal.js";
import { ledgerRows } from "./ledger.js";
import { retainLandedLedgers } from "./ledger-retention.js";
import type { CommittedFold, LandedBlockPorts } from "./ports.js";
import {
  foreignBlockReplayer,
  type replayForeignBlock,
} from "./replay-foreign.js";
import { heldReplays } from "./replay-held.js";

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
 * configs (with its origin's `hubOracleOneShot`, which the protocol-init tx
 * spends), the driver's recompute as the rebase, and the deployment's
 * availability commitment authority; each fold processing commits advances
 * `globals`' verified confirmed ledger (`advanceVerifiedConfirmedLedger`),
 * and each run has `globals`' native owner keep the ledgers the node needs.
 */
export const nodeLandedBlockPorts = (
  store: FactStore,
  plan: {
    readonly projection: EventProjectionConfig;
    readonly forcedOrders: ForcedOrderConfig;
    readonly stateQueue: StateQueueProjectionConfig;
    readonly origin?: Pick<OriginConfig, "hubOracleOneShot">;
  },
  rebase: (reason: string) => Effect.Effect<DriverHold | undefined>,
  availability: () => SDK.DaPayloadAvailabilityAuthority,
  globals: Pick<
    Globals,
    | "VERIFIED_CONFIRMED_LEDGER"
    | "CONFIRMED_LEDGER_FOLD_LOG"
    | "NATIVE_MPF_OWNER"
  >,
) => {
  const replayer = foreignBlockReplayer({
    store,
    events: plan.projection,
    forcedOrders: plan.forcedOrders,
    availability,
    hubOracleOneShot: plan.origin?.hubOracleOneShot,
  });
  const queueHistory = (view: View) =>
    readQueueHistory(store, plan.stateQueue, view);
  return {
    confirmView,
    queueHistory,
    write: (work) => withFollowerWrite(work),
    replay: heldReplays(replayer.replay),
    bindRetained: replayer.bind,
    retainLedgers: retainLandedLedgers(
      queueHistory,
      Ref.get(globals.NATIVE_MPF_OWNER),
    ),
    ownJournal,
    genesis,
    rebase,
    folded: (fold: CommittedFold) =>
      advanceVerifiedConfirmedLedger(globals, fold),
  } satisfies LandedBlockPorts<NodeLandedBlockContext>;
};

export type NodeLandedBlockContext = Effect.Effect.Context<
  ReturnType<ReturnType<typeof replayForeignBlock>>
>;
