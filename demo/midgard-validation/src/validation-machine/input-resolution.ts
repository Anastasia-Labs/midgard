/**
 * The input-resolution schedule hashes.
 */

import { encodeCbor } from "@al-ft/midgard-core";
import { blake2b } from "@noble/hashes/blake2.js";

export const ZERO_32 = Buffer.alloc(32);

const INPUT_RESOLUTION_SCHEDULE_DOMAIN = Buffer.from(
  "MidgardInputResolutionScheduleV1",
  "ascii",
);

export const hash32 = (bytes: Uint8Array): Buffer =>
  Buffer.from(blake2b(Buffer.from(bytes), { dkLen: 32 }));

export const emptyMidgardInputResolutionSchedule = (): Buffer =>
  hash32(INPUT_RESOLUTION_SCHEDULE_DOMAIN);

/**
 * One schedule node: the input's source field, its position in that field
 * (`itemIndex`), its key, and the rest of the schedule.
 */
export const prependMidgardInputResolutionSchedule = (input: {
  readonly sourceKind: "spend" | "reference";
  readonly itemIndex: number;
  readonly key: Uint8Array;
  readonly nextHash: Uint8Array;
}): Buffer => {
  if (input.nextHash.length !== 32) {
    throw new Error("input-resolution schedule hash must contain 32 bytes");
  }
  if (!Number.isSafeInteger(input.itemIndex) || input.itemIndex < 0) {
    throw new Error("input-resolution schedule item index must be a natural");
  }
  return hash32(
    Buffer.concat([
      INPUT_RESOLUTION_SCHEDULE_DOMAIN,
      encodeCbor(input.sourceKind === "spend" ? 0n : 1n),
      encodeCbor(BigInt(input.itemIndex)),
      encodeCbor(Buffer.from(input.key)),
      Buffer.from(input.nextHash),
    ]),
  );
};

export type MidgardInputResolutionSourceKind = "spend" | "reference";

/**
 * The input-resolution schedule order: spend and reference inputs merged and
 * sorted by their canonical out-ref item bytes. A node's position in this
 * order is the `replay_cursor` the ValueAndMint input fold advances, so the
 * trace builder and the Phase B value walk both take it from here. Each entry
 * keeps `itemIndex`, its position in its own field, which the schedule node
 * binds.
 */
export const orderMidgardInputResolutionSchedule = <Item>(input: {
  readonly spend: readonly Item[];
  readonly reference: readonly Item[];
  readonly keyOf: (item: Item) => Uint8Array;
}): readonly {
  readonly sourceKind: MidgardInputResolutionSourceKind;
  readonly itemIndex: number;
  readonly item: Item;
}[] =>
  [
    ...input.spend.map((item, itemIndex) => ({
      sourceKind: "spend" as const,
      itemIndex,
      item,
    })),
    ...input.reference.map((item, itemIndex) => ({
      sourceKind: "reference" as const,
      itemIndex,
      item,
    })),
  ].sort((left, right) =>
    Buffer.compare(input.keyOf(left.item), input.keyOf(right.item)),
  );
