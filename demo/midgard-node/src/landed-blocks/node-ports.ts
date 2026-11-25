/**
 * The node's landed-block ports: the follower-change driver's write
 * capability (the hook's run provides it) for writes, the follower store
 * for the view check, the foreign replay, the node's block journals, the
 * driver's recompute for the rebase, and the native MPF's ledger store (the
 * ledgers it retains, and its retention).
 */
import type { FactStore, OriginConfig, View } from "@al-ft/midgard-l1-follower";
import type { EventProjectionConfig } from "@al-ft/midgard-l1-follower/events";
import type * as SDK from "@al-ft/midgard-sdk/da";
import { Effect, Ref } from "effect";

import { followerViewValid } from "../database/follower-schema.js";
import type { ForcedOrderConfig } from "../forced-orders/index.js";
import type { DriverHold } from "../l1-events/driver.js";
import type { StateQueueProjectionConfig } from "../l1-state-queue/index.js";
import { withFollowerWrite } from "../services/follower-write-gate.js";
import type { Globals } from "../services/globals.js";
import { readQueueHistory } from "./history.js";
import { ownJournal } from "./journal.js";
import { retainLandedLedgers } from "./ledger-retention.js";
import type { LandedBlockPorts } from "./ports.js";
import {
  foreignBlockReplayer,
  type replayForeignBlock,
} from "./replay-foreign.js";
import { heldReplays } from "./replay-held.js";

const confirmView = followerViewValid;

/** The started native MPF owner, which holds the ledger store; fails while
 * the owner is not started. */
const startedOwner = (owner: Globals["NATIVE_MPF_OWNER"]) =>
  Effect.flatMap(Ref.get(owner), (started) =>
    started === undefined
      ? Effect.fail(
          new Error(
            "The native MPF owner, which holds the ledger store, is not started",
          ),
        )
      : Effect.succeed(started),
  );

/** The ledger with `root` in `owner`'s store; fails while the owner is not
 * started and when its store does not retain the root. */
export const ownerRetainedLedger =
  (owner: Globals["NATIVE_MPF_OWNER"]) => (root: string) =>
    Effect.flatMap(startedOwner(owner), (started) =>
      Effect.tryPromise({
        try: () => started.ledger(root),
        catch: (cause) => cause,
      }),
    );

/**
 * The node's landed-block ports over `store`, with the follower plan's
 * configs (with its origin's `hubOracleOneShot`, which the protocol-init tx
 * spends), the driver's recompute as the rebase, and the deployment's
 * availability commitment authority; each run reads and persists ledgers in
 * `globals`' native owner's store and has the owner keep the ledgers the
 * node needs.
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
  globals: Pick<Globals, "NATIVE_MPF_OWNER">,
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
  const ledgerOwner = startedOwner(globals.NATIVE_MPF_OWNER);
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
    retainedLedger: ownerRetainedLedger(globals.NATIVE_MPF_OWNER),
    persistLedger: (ledger) =>
      Effect.flatMap(ledgerOwner, (owner) =>
        Effect.tryPromise({
          try: () => owner.persistLedger(ledger),
          catch: (cause) => cause,
        }),
      ),
    ownJournal,
    rebase,
  } satisfies LandedBlockPorts<NodeLandedBlockContext>;
};

export type NodeLandedBlockContext = Effect.Effect.Context<
  ReturnType<ReturnType<typeof replayForeignBlock>>
>;
