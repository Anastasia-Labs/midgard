import type { DerivationHook } from "@al-ft/midgard-l1-follower";
import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import { COMMITTEE_QUEUE_TABLE } from "./queue-table.js";

/** The state-queue node a node output's datum carries, or null if it is not one. */
export const stateQueueNodeOf = (
  datumHex: string,
): SDK.StateQueueNode | null => {
  try {
    const datum = Data.from(
      Buffer.from(datumHex, "hex").toString("hex"),
      SDK.LinkedListDatum,
    );
    return "Node" in datum.data
      ? Effect.runSync(
          SDK.getStateQueueNodeFromStateQueueDatum(datum.data.Node),
        )
      : null;
  } catch {
    return null;
  }
};

export type CommitteeQueueParameters = Readonly<{
  /** The state-queue address, as raw address bytes. */
  stateQueueAddress: Buffer;
  /** The state-queue NFT policy id, lowercase hex. */
  stateQueuePolicyId: string;
}>;

/**
 * The S3 derivation that maintains `committee_queue_outputs`: closes the row
 * of every queue output a qualifying tx spends, and inserts a row for every
 * queue output it creates, in block order. It reads nothing but the block.
 */
export const committeeQueueDerivation = (
  parameters: CommitteeQueueParameters,
): DerivationHook => ({
  name: "committee-queue",
  writes: [COMMITTEE_QUEUE_TABLE],
  apply: async ({ tx, block, qualified }) => {
    const slot = block.point.slot;
    for (const entry of qualified) {
      for (const outRef of entry.spent)
        await tx.query(
          `UPDATE ${COMMITTEE_QUEUE_TABLE} SET spent_slot = ? WHERE tx_hash = ? AND output_index = ? AND spent_slot IS NULL`,
          [slot, outRef.txHash, outRef.index],
        );
      for (const { outRef, output } of entry.created) {
        if (!output.address.equals(parameters.stateQueueAddress)) continue;
        const decoded = SDK.decodeStateQueueOutput(
          output,
          parameters.stateQueuePolicyId,
        );
        if (decoded === null) continue;
        await tx.query(
          `INSERT INTO ${COMMITTEE_QUEUE_TABLE} (tx_hash, output_index, kind, asset_name, node_key, next_key, header_hash, da_status, end_time_ms, block_time_ms, problems, datum, created_slot, created_height, created_tx_index, spent_slot) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, NULL)`,
          [
            outRef.txHash,
            outRef.index,
            decoded.kind,
            decoded.assetName,
            decoded.nodeKey,
            decoded.nextKey,
            decoded.headerHash,
            decoded.daStatus,
            decoded.endTimeMs,
            decoded.blockTimeMs,
            decoded.problems.join(","),
            output.datum,
            slot,
            block.height,
            entry.tx.index,
          ],
        );
      }
    }
  },
});
