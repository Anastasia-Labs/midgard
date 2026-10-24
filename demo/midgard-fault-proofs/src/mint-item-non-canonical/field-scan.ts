import { decodeMidgardFieldPreimage } from "@al-ft/midgard-core";
import { Data } from "@lucid-evolution/lucid";

import {
  type MintFieldCursor,
  MintFieldCursorSchema,
  type MintItemEvidence,
} from "./mint-item-non-canonical.js";

/** Reconstruct the bounded envelope checkpoints from the committed field. */
export const nextMintFieldCursor = (
  evidence: MintItemEvidence,
  checkpoint: MintFieldCursor | null,
) => {
  const items = decodeMidgardFieldPreimage(
    Buffer.from(evidence.fieldPreimageHex, "hex"),
  );
  let offset = items.length < 24 ? 1 : items.length < 256 ? 2 : 3;
  let selectedOffset = 0;
  const checkpoints: MintFieldCursor[] = [];
  for (const [index, item] of items.entries()) {
    const payloadOffset =
      offset + (item.length < 24 ? 1 : item.length < 256 ? 2 : 3);
    if (index === evidence.itemIndex) selectedOffset = payloadOffset;
    offset = payloadOffset + item.length;
    if ((index + 1) % 32 === 0 || index === items.length - 1) {
      checkpoints.push({
        next_index: BigInt(index + 1),
        next_offset: BigInt(offset),
        item_offset: BigInt(selectedOffset),
        item_length: selectedOffset === 0 ? 0n : BigInt(evidence.itemLength),
        previous_policy:
          evidence.itemIndex > 0 && index >= evidence.itemIndex - 1
            ? evidence.scanControls[0]!.previousPolicy
            : "",
      });
    }
  }
  const encoded = (cursor: MintFieldCursor) =>
    Data.to(cursor as never, MintFieldCursorSchema as never);
  const ordinal =
    checkpoint === null
      ? -1
      : checkpoints.findIndex(
          (cursor) => encoded(cursor) === encoded(checkpoint),
        );
  if (checkpoint !== null && ordinal < 0)
    throw new Error(
      "mint-item-non-canonical: field checkpoint differs from authenticated trace",
    );
  const next = checkpoints[ordinal + 1];
  if (next === undefined)
    throw new Error(
      "mint-item-non-canonical: envelope walk is already complete",
    );
  return { cursor: next, terminal: ordinal + 1 === checkpoints.length - 1 };
};
