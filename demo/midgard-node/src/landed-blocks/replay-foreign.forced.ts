/**
 * The forced orders of a foreign block's replay (`replay-foreign.ts`): the
 * ones admitted in its window at the view, from the facts, and the node
 * rows of the ones it includes, built as ingestion builds them
 * (`forcedOrderEntry`) from the field preimages the replay checked against
 * each order's commitments.
 */
import { MIDGARD_CONSENSUS_PROFILE } from "@al-ft/midgard-core/consensus-profile";
import type { FactStore, View } from "@al-ft/midgard-l1-follower";
import type * as SDK from "@al-ft/midgard-sdk";
import { Effect } from "effect";

import {
  type ForcedOrderConfig,
  forcedOrderEntry,
  forcedOrdersAdmittedAt,
  outRefLabel,
} from "../forced-orders/index.js";
import { missing, Verdict } from "./replay-foreign.verdict.js";

/**
 * The forced orders admitted in the block's window at the view: order id
 * hex to raw datum and to the order, from the facts, and the in-window
 * unreadable ones, id hex to why. An in-window order whose output cannot be
 * read back there is a wait.
 */
export const forcedInWindow = (
  store: FactStore,
  config: ForcedOrderConfig,
  view: View,
  inWindow: (time: bigint) => boolean,
) =>
  Effect.gen(function* () {
    const read = yield* Effect.promise(() =>
      forcedOrdersAdmittedAt(store, config, view.point),
    );
    if (read.kind !== "ok")
      return yield* Effect.fail(
        missing(`the forced orders are unreadable: ${read.kind}`),
      );
    const forced = new Map<string, Buffer>();
    const orders = new Map<string, SDK.TxOrderUTxOV1>();
    const seen = new Set<string>();
    const unreadable = new Map<string, string>();
    for (const admitted of read.orders) {
      const order = admitted.read;
      if (order !== null) seen.add(order.idCbor.toString("hex"));
      if (!inWindow(admitted.inclusionTime)) continue;
      if (order === null)
        return yield* Effect.fail(
          new Verdict(
            "forced_order_pending",
            `forced order ${outRefLabel(admitted.outRef)} is admitted in the window but its output cannot be read at the view`,
          ),
        );
      const id = order.idCbor.toString("hex");
      if (order.kind === "unreadable") unreadable.set(id, order.detail);
      else {
        forced.set(id, order.datum);
        orders.set(id, order.order);
      }
    }
    return { forced, orders, seen, unreadable };
  });

/**
 * The node rows of the forced orders `ids` the replay checked: each from
 * its order and the preimages its check derived. A missing one, or a row
 * that does not build, is this node's fault: the replay accepted the
 * order's transaction.
 */
export const forcedEntries = (
  ids: readonly string[],
  orders: ReadonlyMap<string, SDK.TxOrderUTxOV1>,
  preimages: ReadonlyMap<string, readonly Uint8Array[]>,
) =>
  Effect.forEach(ids, (id) => {
    const order = orders.get(id);
    const fieldPreimages = preimages.get(id);
    const fault = (detail: string) =>
      new Verdict("local_fault", `forced order ${id}: ${detail}`);
    if (order === undefined || fieldPreimages === undefined)
      return Effect.fail(fault("its checked order is not in hand"));
    return forcedOrderEntry({
      order,
      fieldPreimages,
      consensusProfile: MIDGARD_CONSENSUS_PROFILE,
    }).pipe(Effect.mapError((cause) => fault(cause.message)));
  });
