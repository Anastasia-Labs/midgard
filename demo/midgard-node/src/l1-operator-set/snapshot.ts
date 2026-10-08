/**
 * The operator directory the watchdog plans from (NC14): the operator set's
 * lists, scheduler and hub oracle, the landed state queue's tail, and only
 * the retired nodes a planned tx needs, each read by its asset name. The
 * retired list is never read whole.
 */
import {
  type Dialect,
  type FactStore,
  liveUnitBeforeIn,
  liveUtxosIn,
  type SqlTx,
  type StoredOutput,
  type View,
} from "@al-ft/midgard-l1-follower";
import { toLucidUtxo } from "@al-ft/midgard-l1-follower/provider";
import * as SDK from "@al-ft/midgard-sdk";
import { Effect, Either } from "effect";

import type {
  LandedStateQueue,
  LandedStateQueueElement,
} from "../l1-state-queue/index.js";
import { landedTail } from "../l1-state-queue/index.js";
import type { OperatorListContract } from "./config.js";
import type { OperatorMembership } from "./membership.js";
import type { OperatorSet } from "./set.js";

/**
 * What the operator-set hook publishes for the node's fibers
 * (`Globals.OPERATOR_SET`): the set and membership of the last driver run,
 * the landed queue's tail of the same run, and the retired-anchor read.
 */
export type PublishedOperatorSet = Readonly<{
  set: OperatorSet;
  membership: OperatorMembership;
  stateQueueTail: SDK.StateQueueTail | null;
  /** The retired insertion anchor of a key, read at the follower's tip. */
  retiredInsertionAnchor: (
    operatorKey: string,
  ) => Promise<SDK.RetiredOperatorNode | null>;
}>;

/**
 * What one hook run publishes: its set and membership, the landed queue's
 * tail, and the retired-anchor read over `store`.
 */
export const publishedOperatorSetOf = (input: {
  readonly set: OperatorSet;
  readonly membership: OperatorMembership;
  readonly stateQueueTail: SDK.StateQueueTail | null;
  readonly store: Pick<FactStore, "dialect" | "transaction">;
  readonly retired: OperatorListContract;
}): PublishedOperatorSet => ({
  set: input.set,
  membership: input.membership,
  stateQueueTail: input.stateQueueTail,
  retiredInsertionAnchor: (operatorKey) =>
    input.store.transaction("read", (tx) =>
      retiredInsertionAnchorIn(
        tx,
        input.store.dialect,
        input.retired,
        operatorKey,
      ),
    ),
});

/** The state-queue tail as the SDK planners take it, from the landed queue. */
export const stateQueueTailOf = (
  queue: LandedStateQueue,
): SDK.StateQueueTail | null => {
  if (!queue.healthy) return null;
  const tail: LandedStateQueueElement | null = landedTail(queue);
  if (tail === null) return null;
  return {
    utxo: tail.element.utxo,
    datum: tail.element.datum,
    endTime: tail.endTimeMs,
    isRoot: tail === queue.root,
  };
};

const retiredNodeOf = (
  contract: OperatorListContract,
  stored: StoredOutput,
): SDK.RetiredOperatorNode | null => {
  const names = stored.output.assets.get(contract.policyId);
  const name = names === undefined ? undefined : [...names.keys()][0];
  if (name === undefined || names?.size !== 1) return null;
  const decoded = Effect.runSync(
    Effect.either(
      SDK.retiredOperatorNodeFromUTxO(
        toLucidUtxo(stored.outRef, stored.output),
        name,
      ),
    ),
  );
  return Either.isRight(decoded) ? decoded.right : null;
};

/**
 * The live retired node a retirement of `operatorKey` inserts after: the
 * node with the greatest key below it, or the root. Two index reads by asset
 * name, at the follower's tip; null when neither is live.
 */
export const retiredInsertionAnchorIn = async (
  tx: SqlTx,
  dialect: Dialect,
  contract: OperatorListContract,
  operatorKey: string,
): Promise<SDK.RetiredOperatorNode | null> => {
  const policyId = Buffer.from(contract.policyId, "hex");
  const before = await liveUnitBeforeIn(tx, dialect, {
    policyId,
    from: Buffer.from(contract.nodePrefix, "hex"),
    below: Buffer.from(`${contract.nodePrefix}${operatorKey}`, "hex"),
  });
  if (before !== null) return retiredNodeOf(contract, before);
  const root = await liveUtxosIn(tx, dialect, {
    by: "unit",
    policyId,
    assetName: Buffer.from(contract.rootAssetName, "hex"),
  });
  if (root.kind !== "ok" || root.utxos.length !== 1) return null;
  return retiredNodeOf(contract, root.utxos[0]!);
};

/**
 * The directory snapshot the watchdog plans from: the published set's lists,
 * scheduler and hub oracle and the landed queue's tail, with no retired
 * nodes (a planned retirement reads its anchor by asset name), and the
 * set's follower view (what a plan from it records under, S5), or why there
 * is none.
 */
export const publishedDirectoryOf = (
  published: PublishedOperatorSet | undefined,
):
  | Readonly<{
      kind: "ok";
      snapshot: SDK.OperatorDirectorySnapshot;
      view: View;
    }>
  | Readonly<{ kind: "unavailable"; reason: string; detail: string }> => {
  if (published === undefined)
    return {
      kind: "unavailable",
      reason: "operator_set_unavailable",
      detail: "the follower-change driver has not published it yet",
    };
  const { set, stateQueueTail } = published;
  if (set.unhealthy !== null)
    return {
      kind: "unavailable",
      reason: "operator_set_unhealthy",
      detail: set.unhealthy,
    };
  if (set.scheduler === null || set.hubOracle === null)
    return {
      kind: "unavailable",
      reason: "operator_set_unhealthy",
      detail: "no scheduler or hub oracle",
    };
  if (stateQueueTail === null)
    return {
      kind: "unavailable",
      reason: "state_queue_unavailable",
      detail: "the landed state queue has no healthy tail",
    };
  return {
    kind: "ok",
    snapshot: {
      registered: set.registered,
      active: set.active,
      retired: [],
      scheduler: set.scheduler,
      hubOracle: set.hubOracle,
      stateQueueTail,
    },
    view: set.view,
  };
};

/**
 * The retired insertion anchor of `operatorKey` from the published set,
 * failing when there is no set or no live anchor.
 */
export const publishedRetiredAnchorProgram = (
  published: PublishedOperatorSet | undefined,
  operatorKey: string,
): Effect.Effect<SDK.RetiredOperatorNode, SDK.StateQueueError> =>
  Effect.tryPromise({
    try: async () => {
      const anchor =
        published === undefined
          ? null
          : await published.retiredInsertionAnchor(operatorKey);
      if (anchor === null)
        throw new Error(`no live retired node to insert ${operatorKey} after`);
      return anchor;
    },
    catch: (cause) =>
      new SDK.StateQueueError({
        message: `could not read the retired insertion anchor: ${cause instanceof Error ? cause.message : String(cause)}`,
        cause,
      }),
  });
