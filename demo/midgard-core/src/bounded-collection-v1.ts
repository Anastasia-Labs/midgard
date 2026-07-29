import { blake2b } from "@noble/hashes/blake2.js";

import {
  buildMidgardBoundedItemV1,
  type MidgardBoundedItemV1,
} from "./bounded-item-v1.js";
import { encodeCbor } from "./codec/cbor.js";
import { ensureHash32, type Hash32 } from "./codec/hash.js";
import {
  buildMidgardValidationMerkleFrontierV1,
  buildMidgardValidationMerkleMembershipV1,
  commitMidgardValidationMerkleFrontierV1,
  type MidgardValidationMerkleFrontierV1,
  type MidgardValidationMerkleMembershipV1,
  verifyMidgardValidationMerkleMembershipV1,
} from "./validation-merkle.js";

export const MIDGARD_BOUNDED_COLLECTION_V1_VERSION = 1 as const;
export const MIDGARD_BOUNDED_COLLECTION_FIELD_COUNT_V1 = 9 as const;

const ITEM_DOMAIN = Buffer.from("MidgardBoundedCollectionItemV1", "ascii");
const COMMITMENT_DOMAIN = Buffer.from(
  "MidgardBoundedCollectionCommitmentV1",
  "ascii",
);

export type MidgardBoundedCollectionV1 = {
  readonly fieldIndex: number;
  readonly items: readonly MidgardBoundedItemV1[];
  readonly leafHashes: readonly Hash32[];
  readonly frontier: MidgardValidationMerkleFrontierV1;
  readonly commitment: Hash32;
};

export type MidgardBoundedCollectionItemProofV1 = {
  readonly version: typeof MIDGARD_BOUNDED_COLLECTION_V1_VERSION;
  readonly fieldIndex: number;
  readonly itemCount: number;
  readonly itemIndex: number;
  readonly itemLength: number;
  readonly itemCommitment: Hash32;
  readonly frontier: MidgardValidationMerkleFrontierV1;
  readonly siblings: readonly Hash32[];
};

const hash32 = (value: Uint8Array): Hash32 =>
  ensureHash32(
    blake2b(value, { dkLen: 32 }),
    "bounded_collection_v1.commitment",
  );

const fieldIndexV1 = (fieldIndex: number): number => {
  if (
    !Number.isSafeInteger(fieldIndex) ||
    fieldIndex < 0 ||
    fieldIndex >= MIDGARD_BOUNDED_COLLECTION_FIELD_COUNT_V1
  ) {
    throw new Error(`unknown V1 bounded-collection field index ${fieldIndex}`);
  }
  return fieldIndex;
};

const itemIndexV1 = (itemIndex: number): number => {
  if (!Number.isSafeInteger(itemIndex) || itemIndex < 0) {
    throw new Error(
      "V1 bounded-collection item index must be non-negative",
    );
  }
  return itemIndex;
};

export const hashMidgardBoundedCollectionItemV1 = ({
  fieldIndex,
  itemIndex,
  itemLength,
  itemCommitment,
}: {
  readonly fieldIndex: number;
  readonly itemIndex: number;
  readonly itemLength: number;
  readonly itemCommitment: Uint8Array;
}): Hash32 =>
  hash32(
    Buffer.concat([
      ITEM_DOMAIN,
      encodeCbor([
        BigInt(MIDGARD_BOUNDED_COLLECTION_V1_VERSION),
        BigInt(fieldIndexV1(fieldIndex)),
        BigInt(itemIndexV1(itemIndex)),
        BigInt(itemIndexV1(itemLength)),
        ensureHash32(
          itemCommitment,
          "bounded_collection_v1.item_commitment",
        ),
      ]),
    ]),
  );

export const commitMidgardBoundedCollectionV1 = ({
  fieldIndex,
  frontier,
}: {
  readonly fieldIndex: number;
  readonly frontier: MidgardValidationMerkleFrontierV1;
}): Hash32 =>
  hash32(
    Buffer.concat([
      COMMITMENT_DOMAIN,
      encodeCbor([
        BigInt(MIDGARD_BOUNDED_COLLECTION_V1_VERSION),
        BigInt(fieldIndexV1(fieldIndex)),
        BigInt(frontier.count),
        commitMidgardValidationMerkleFrontierV1(frontier),
      ]),
    ]),
  );

export const buildMidgardBoundedCollectionV1 = ({
  fieldIndex,
  items,
}: {
  readonly fieldIndex: number;
  readonly items: readonly Uint8Array[];
}): MidgardBoundedCollectionV1 => {
  const exactFieldIndex = fieldIndexV1(fieldIndex);
  const exactItems = items.map((bytes, itemIndex) =>
    buildMidgardBoundedItemV1({
      fieldIndex: exactFieldIndex,
      itemIndex,
      bytes,
    }),
  );
  const leafHashes = exactItems.map((item, itemIndex) =>
    hashMidgardBoundedCollectionItemV1({
      fieldIndex: exactFieldIndex,
      itemIndex,
      itemLength: item.bytes.length,
      itemCommitment: item.commitment,
    }),
  );
  const frontier = buildMidgardValidationMerkleFrontierV1(leafHashes);
  return {
    fieldIndex: exactFieldIndex,
    items: exactItems,
    leafHashes,
    frontier,
    commitment: commitMidgardBoundedCollectionV1({
      fieldIndex: exactFieldIndex,
      frontier,
    }),
  };
};

export const buildMidgardBoundedCollectionItemProofV1 = (
  collection: MidgardBoundedCollectionV1,
  itemIndex: number,
): MidgardBoundedCollectionItemProofV1 => {
  const exactItemIndex = itemIndexV1(itemIndex);
  if (exactItemIndex >= collection.items.length) {
    throw new Error("V1 bounded-collection item index is out of range");
  }
  const membership: MidgardValidationMerkleMembershipV1 =
    buildMidgardValidationMerkleMembershipV1(
      collection.leafHashes,
      exactItemIndex,
    );
  return {
    version: MIDGARD_BOUNDED_COLLECTION_V1_VERSION,
    fieldIndex: collection.fieldIndex,
    itemCount: collection.items.length,
    itemIndex: exactItemIndex,
    itemLength: collection.items[exactItemIndex]!.bytes.length,
    itemCommitment: collection.items[exactItemIndex]!.commitment,
    frontier: membership.frontier,
    siblings: membership.siblings,
  };
};

export const verifyMidgardBoundedCollectionItemProofV1 = ({
  expectedCommitment,
  proof,
}: {
  readonly expectedCommitment: Uint8Array;
  readonly proof: MidgardBoundedCollectionItemProofV1;
}): boolean => {
  try {
    if (
      proof.version !== MIDGARD_BOUNDED_COLLECTION_V1_VERSION ||
      proof.itemCount <= 0 ||
      proof.itemLength < 0 ||
      proof.itemIndex < 0 ||
      proof.itemIndex >= proof.itemCount ||
      proof.frontier.count !== proof.itemCount
    ) {
      return false;
    }
    const leafHash = hashMidgardBoundedCollectionItemV1({
      fieldIndex: proof.fieldIndex,
      itemIndex: proof.itemIndex,
      itemLength: proof.itemLength,
      itemCommitment: proof.itemCommitment,
    });
    return (
      verifyMidgardValidationMerkleMembershipV1({
        frontier: proof.frontier,
        leafIndex: proof.itemIndex,
        leafHash,
        siblings: proof.siblings,
      }) &&
      commitMidgardBoundedCollectionV1({
        fieldIndex: proof.fieldIndex,
        frontier: proof.frontier,
      }).equals(
        ensureHash32(
          expectedCommitment,
          "bounded_collection_v1.expected",
        ),
      )
    );
  } catch {
    return false;
  }
};
