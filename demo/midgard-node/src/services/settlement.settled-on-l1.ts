/**
 * Whether an event's settlement has landed on L1, whoever sent it, from the
 * follower's facts: the depth of the settling tx under the tip, or null.
 * Every operator queues a job for every event, so another operator often
 * settles it first.
 *
 * - A deposit is settled once its event is retired (absorbed).
 * - A withdrawal is settled once its payout is concluded: its event is
 *   retired (the payout initialized) and no output holds the payout token
 *   any more; the conclusion is the spend of the last output that held it.
 *
 * Once the settling transaction is more than k deep the follower prunes its
 * facts (#869): the event's rows go once its retirement is, and a payout
 * output once its spend is. A node that was down that long reads such an
 * event as `pruned` (`eventOrderOrPrunedByIdIn`), and it is settled for
 * good: `"pruned"`, never `null`, so the job completes instead of building
 * again for an event that is gone.
 */
import {
  blockAtOrBeforeSlotIn,
  changedUtxosIn,
  currentViewIn,
  depth,
  postgresDialect,
  type SqlTx,
} from "@al-ft/midgard-l1-follower";
import { eventKeyOfId } from "@al-ft/midgard-l1-follower/events";
import * as SDK from "@al-ft/midgard-sdk";
import type { SqlClient } from "@effect/sql";
import { Effect } from "effect";

import { parseEventId } from "../commands/command-utils.js";
import { inFollowerSnapshot } from "../database/follower-schema.js";
import type { SettlementKind } from "../database/settlement.js";
import { eventOrderOrPrunedByIdIn } from "../l1-events/event-by-id.js";
import { MidgardContracts } from "./midgard-contracts.js";

/** Settled more than k deep: the follower has pruned the facts that show it. */
export type SettledBeyondK = "pruned";

/**
 * The depth of the payout's conclusion, or null while an output still holds
 * the payout token. With the event `retired`, no such output at all means
 * nothing to read (null); with it `pruned`, the initialize that minted the
 * token is more than k deep, so every output that held it was spent more
 * than k deep and pruned with its spend.
 */
const concludedDepth = async (
  tx: SqlTx,
  unit: Readonly<{ policyId: Buffer; assetName: Buffer }>,
  event: "retired" | "pruned",
): Promise<number | SettledBeyondK | null> => {
  const payouts = await changedUtxosIn(
    tx,
    postgresDialect,
    { by: "unit", ...unit },
    null,
  );
  if (payouts.kind !== "ok") return null;
  let last: number | null = null;
  for (const { spent } of payouts.utxos) {
    if (spent === null) return null;
    last = Math.max(last ?? spent.slot, spent.slot);
  }
  if (last === null) return event === "pruned" ? "pruned" : null;
  const block = await blockAtOrBeforeSlotIn(tx, last);
  const view = await currentViewIn(tx, postgresDialect);
  if (block?.slot !== last || view === null) return null;
  return depth(view.height, block.height);
};

/** Null while something is still to do, and when the facts do not say: the
 * job's own path then reads them again and reports what it finds. */
export const settledDepth = (
  kind: SettlementKind,
  eventIdHex: string,
): Effect.Effect<
  number | SettledBeyondK | null,
  unknown,
  MidgardContracts | SqlClient.SqlClient
> =>
  Effect.gen(function* () {
    const eventId = parseEventId(eventIdHex, `--${kind}-event-id`);
    const contracts = yield* MidgardContracts;
    const policyId =
      SDK.requireEventHistoryContracts(contracts)[kind].list.policyId;
    return yield* inFollowerSnapshot(async (tx) => {
      const read = await eventOrderOrPrunedByIdIn(
        tx,
        { kind, policyId },
        eventId,
      );
      const payout = {
        policyId: Buffer.from(contracts.payout.policyId, "hex"),
        assetName: eventKeyOfId(eventId),
      };
      switch (read.kind) {
        case "ok":
        case "absent":
        case "unavailable":
          return null;
        case "retired":
          return kind === "deposit"
            ? read.retirement.depth
            : concludedDepth(tx, payout, "retired");
        case "pruned":
          return kind === "deposit"
            ? "pruned"
            : concludedDepth(tx, payout, "pruned");
      }
    });
  });
