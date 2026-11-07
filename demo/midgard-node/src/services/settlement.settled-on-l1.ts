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
 */
import {
  blockAtOrBeforeSlotIn,
  changedUtxosIn,
  currentViewIn,
  depth,
  postgresDialect,
  type SqlTx,
} from "@al-ft/midgard-l1-follower";
import {
  eventKeyOfId,
  eventOrderByIdIn,
} from "@al-ft/midgard-l1-follower/events";
import * as SDK from "@al-ft/midgard-sdk";
import type { SqlClient } from "@effect/sql";
import { Effect } from "effect";

import { parseEventId } from "../commands/command-utils.js";
import { inFollowerSnapshot } from "../database/follower-schema.js";
import type { SettlementKind } from "../database/settlement.js";
import { MidgardContracts } from "./midgard-contracts.js";

const concludedDepth = async (
  tx: SqlTx,
  unit: Readonly<{ policyId: Buffer; assetName: Buffer }>,
): Promise<number | null> => {
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
  if (last === null) return null;
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
  number | null,
  unknown,
  MidgardContracts | SqlClient.SqlClient
> =>
  Effect.gen(function* () {
    const eventId = parseEventId(eventIdHex, `--${kind}-event-id`);
    const contracts = yield* MidgardContracts;
    const policyId =
      SDK.requireEventHistoryContracts(contracts)[kind].list.policyId;
    return yield* inFollowerSnapshot(async (tx) => {
      const read = await eventOrderByIdIn(
        tx,
        postgresDialect,
        { kind, policyId },
        eventId,
      );
      switch (read.kind) {
        case "ok":
        case "absent":
        case "unavailable":
          return null;
        case "retired":
          return kind === "deposit"
            ? read.retirement.depth
            : concludedDepth(tx, {
                policyId: Buffer.from(contracts.payout.policyId, "hex"),
                assetName: eventKeyOfId(eventId),
              });
      }
    });
  });
