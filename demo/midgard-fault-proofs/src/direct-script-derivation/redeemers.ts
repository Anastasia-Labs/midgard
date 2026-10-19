import {
  buildMidgardBoundedItem,
  decodeMidgardRedeemerWitnessItem,
} from "@al-ft/midgard-core";
import {
  readCborBytes,
  readCborUnsigned,
} from "@al-ft/midgard-core/codec/cbor";
import type * as SDK from "@al-ft/midgard-sdk";

/** Preserves the actual Data span and declared units without copying a large
 * redeemer into the continuation datum. Data validation remains its own rule. */
export const directRedeemerAt = (
  items: readonly Buffer[],
  coordinate: SDK.DirectPurposeCoordinate,
): SDK.DirectRedeemer | null => {
  const tags = ["Spend", "Mint", "Reward", "Receive"] as const;
  if (coordinate.kind < 0n || coordinate.kind > 3n || coordinate.index < 0n)
    throw new Error("direct script derivation: redeemer coordinate differs");
  let selected: SDK.DirectRedeemer | null = null;
  for (const [index, item] of items.entries()) {
    const witness = decodeMidgardRedeemerWitnessItem(item);
    if (
      witness.purpose !== tags[Number(coordinate.kind)] ||
      witness.index !== coordinate.index
    )
      continue;
    if (selected !== null)
      throw new Error("direct script derivation: duplicate redeemer pointer");
    const purpose = readCborUnsigned(item, 1);
    const pointer = readCborUnsigned(item, purpose.nextOffset);
    const data = readCborBytes(item, pointer.nextOffset);
    selected = {
      item_index: BigInt(index),
      total_length: BigInt(item.length),
      item_commitment: buildMidgardBoundedItem({
        fieldIndex: 8,
        itemIndex: index,
        bytes: item,
      }).commitment.toString("hex"),
      data_offset: BigInt(data.nextOffset - data.value.length),
      data_length: BigInt(data.value.length),
      execution_memory: witness.executionUnits.memory,
      execution_steps: witness.executionUnits.steps,
    };
  }
  return selected;
};
