import { blake2b } from "@noble/hashes/blake2.js";

import {
  buildMidgardBoundedItem,
  type MidgardBoundedItem,
} from "./bounded-item.js";
import { encodeCbor } from "./codec/cbor.js";
import { ensureHash32, type Hash32 } from "./codec/hash.js";
import {
  buildMidgardValidationMerkleFrontier,
  buildMidgardValidationMerkleMembership,
  commitMidgardValidationMerkleFrontier,
  type MidgardValidationMerkleFrontier,
  type MidgardValidationMerkleMembership,
  verifyMidgardValidationMerkleMembership,
} from "./validation-merkle.js";

export const MIDGARD_BOUNDED_COLLECTION_VERSION = 1 as const;
export const MIDGARD_BOUNDED_COLLECTION_FIELD_COUNT = 9 as const;

const ITEM_DOMAIN = Buffer.from("MidgardBoundedCollectionItemV1", "ascii");
const COMMITMENT_DOMAIN = Buffer.from(
  "MidgardBoundedCollectionCommitmentV1",
  "ascii",
);

export type MidgardBoundedCollection = {
  readonly fieldIndex: number;
  readonly items: readonly MidgardBoundedItem[];
  readonly leafHashes: readonly Hash32[];
  readonly frontier: MidgardValidationMerkleFrontier;
  readonly commitment: Hash32;
};

export type MidgardBoundedCollectionItemProof = {
  readonly version: typeof MIDGARD_BOUNDED_COLLECTION_VERSION;
  readonly fieldIndex: number;
  readonly itemCount: number;
  readonly itemIndex: number;
  readonly itemLength: number;
  readonly itemCommitment: Hash32;
  readonly frontier: MidgardValidationMerkleFrontier;
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
    fieldIndex >= MIDGARD_BOUNDED_COLLECTION_FIELD_COUNT
  ) {
    throw new Error(`unknown V1 bounded-collection field index ${fieldIndex}`);
  }
  return fieldIndex;
};

const itemIndexV1 = (itemIndex: number): number => {
  if (!Number.isSafeInteger(itemIndex) || itemIndex < 0) {
    throw new Error("V1 bounded-collection item index must be non-negative");
  }
  return itemIndex;
};

export const hashMidgardBoundedCollectionItem = ({
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
        BigInt(MIDGARD_BOUNDED_COLLECTION_VERSION),
        BigInt(fieldIndexV1(fieldIndex)),
        BigInt(itemIndexV1(itemIndex)),
        BigInt(itemIndexV1(itemLength)),
        ensureHash32(itemCommitment, "bounded_collection_v1.item_commitment"),
      ]),
    ]),
  );

export const commitMidgardBoundedCollection = ({
  fieldIndex,
  frontier,
}: {
  readonly fieldIndex: number;
  readonly frontier: MidgardValidationMerkleFrontier;
}): Hash32 =>
  hash32(
    Buffer.concat([
      COMMITMENT_DOMAIN,
      encodeCbor([
        BigInt(MIDGARD_BOUNDED_COLLECTION_VERSION),
        BigInt(fieldIndexV1(fieldIndex)),
        BigInt(frontier.count),
        commitMidgardValidationMerkleFrontier(frontier),
      ]),
    ]),
  );

export const buildMidgardBoundedCollection = ({
  fieldIndex,
  items,
}: {
  readonly fieldIndex: number;
  readonly items: readonly Uint8Array[];
}): MidgardBoundedCollection => {
  const exactFieldIndex = fieldIndexV1(fieldIndex);
  const exactItems = items.map((bytes, itemIndex) =>
    buildMidgardBoundedItem({
      fieldIndex: exactFieldIndex,
      itemIndex,
      bytes,
    }),
  );
  const leafHashes = exactItems.map((item, itemIndex) =>
    hashMidgardBoundedCollectionItem({
      fieldIndex: exactFieldIndex,
      itemIndex,
      itemLength: item.bytes.length,
      itemCommitment: item.commitment,
    }),
  );
  const frontier = buildMidgardValidationMerkleFrontier(leafHashes);
  return {
    fieldIndex: exactFieldIndex,
    items: exactItems,
    leafHashes,
    frontier,
    commitment: commitMidgardBoundedCollection({
      fieldIndex: exactFieldIndex,
      frontier,
    }),
  };
};

export const buildMidgardBoundedCollectionItemProof = (
  collection: MidgardBoundedCollection,
  itemIndex: number,
): MidgardBoundedCollectionItemProof => {
  const exactItemIndex = itemIndexV1(itemIndex);
  if (exactItemIndex >= collection.items.length) {
    throw new Error("V1 bounded-collection item index is out of range");
  }
  const membership: MidgardValidationMerkleMembership =
    buildMidgardValidationMerkleMembership(
      collection.leafHashes,
      exactItemIndex,
    );
  return {
    version: MIDGARD_BOUNDED_COLLECTION_VERSION,
    fieldIndex: collection.fieldIndex,
    itemCount: collection.items.length,
    itemIndex: exactItemIndex,
    itemLength: collection.items[exactItemIndex]!.bytes.length,
    itemCommitment: collection.items[exactItemIndex]!.commitment,
    frontier: membership.frontier,
    siblings: membership.siblings,
  };
};

export const verifyMidgardBoundedCollectionItemProof = ({
  expectedCommitment,
  proof,
}: {
  readonly expectedCommitment: Uint8Array;
  readonly proof: MidgardBoundedCollectionItemProof;
}): boolean => {
  try {
    if (
      proof.version !== MIDGARD_BOUNDED_COLLECTION_VERSION ||
      proof.itemCount <= 0 ||
      proof.itemLength < 0 ||
      proof.itemIndex < 0 ||
      proof.itemIndex >= proof.itemCount ||
      proof.frontier.count !== proof.itemCount
    ) {
      return false;
    }
    const leafHash = hashMidgardBoundedCollectionItem({
      fieldIndex: proof.fieldIndex,
      itemIndex: proof.itemIndex,
      itemLength: proof.itemLength,
      itemCommitment: proof.itemCommitment,
    });
    return (
      verifyMidgardValidationMerkleMembership({
        frontier: proof.frontier,
        leafIndex: proof.itemIndex,
        leafHash,
        siblings: proof.siblings,
      }) &&
      commitMidgardBoundedCollection({
        fieldIndex: proof.fieldIndex,
        frontier: proof.frontier,
      }).equals(
        ensureHash32(expectedCommitment, "bounded_collection_v1.expected"),
      )
    );
  } catch {
    return false;
  }
};
