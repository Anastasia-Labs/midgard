/** The address-witness signer set the evaluation reads: each field-7 item's
 * key hash, in field order, as the `signers` role appends it to its compact
 * frontier, and the answer to one signature leaf against that frontier. */
import {
  appendMidgardValidationMerkleLeaf,
  buildMidgardValidationMerkleMembership,
  computeHash28,
  computeHash32,
  emptyMidgardValidationMerkleFrontier,
  type MidgardValidationMerkleFrontier,
} from "@al-ft/midgard-core";
import type * as SDK from "@al-ft/midgard-sdk";

export type NativeSignerSet = {
  /** Each address witness's key hash, in field order. */
  readonly keyHashes: readonly Buffer[];
  /** `signers.signer_leaf_v1` of each key hash. */
  readonly leaves: readonly Buffer[];
};

/** `signers.signer_of_item_v1` over every field-7 item `[vkey, signature]`:
 * the vkey is the 32 bytes after the array and bytes heads. */
export const nativeSignerSet = (
  witnesses: readonly Buffer[],
): NativeSignerSet => {
  const keyHashes = witnesses.map((item) => {
    if (item.length < 35)
      throw new Error(
        "execution-native-script-invalid: address witness is too short",
      );
    return computeHash28(item.subarray(3, 35));
  });
  return { keyHashes, leaves: keyHashes.map((key) => computeHash32(key)) };
};

const witnessAt = (
  set: NativeSignerSet,
  position: number,
): SDK.ExecutionNativeScriptInvalidSignerWitness => ({
  key_hash: set.keyHashes[position]!.toString("hex"),
  siblings: buildMidgardValidationMerkleMembership(
    set.leaves,
    position,
  ).siblings.map((hash) => Buffer.from(hash).toString("hex")),
});

/** The answer to a signature leaf over `keyHash`: its position when it signs,
 * else the position it would take between its two neighbours, which the
 * field's ascending order makes adjacent. */
export const nativeSignerQuery = (
  set: NativeSignerSet,
  keyHash: Buffer,
): {
  readonly query: SDK.ExecutionNativeScriptInvalidSignerQuery;
  readonly present: boolean;
} => {
  const found = set.keyHashes.findIndex((key) => key.equals(keyHash));
  if (found >= 0)
    return {
      present: true,
      query: {
        SignerPresent: {
          position: BigInt(found),
          siblings: witnessAt(set, found).siblings,
        },
      },
    };
  const position = set.keyHashes.filter(
    (key) => Buffer.compare(key, keyHash) < 0,
  ).length;
  const below = position === 0 ? undefined : set.keyHashes[position - 1];
  const above = set.keyHashes[position];
  if (
    (below !== undefined && Buffer.compare(below, keyHash) >= 0) ||
    (above !== undefined && Buffer.compare(keyHash, above) >= 0)
  )
    throw new Error(
      "execution-native-script-invalid: address witnesses are not in ascending signer order",
    );
  return {
    present: false,
    query: {
      SignerAbsent: {
        position: BigInt(position),
        below: below === undefined ? null : witnessAt(set, position - 1),
        above: above === undefined ? null : witnessAt(set, position),
      },
    },
  };
};

/** The compact frontier over the first `count` signer leaves. */
export const nativeSignerFrontier = (
  set: NativeSignerSet,
  count: number,
): Pick<
  SDK.ExecutionNativeScriptInvalidEvalState,
  "signer_count" | "signer_peaks"
> => {
  let frontier: MidgardValidationMerkleFrontier =
    emptyMidgardValidationMerkleFrontier();
  for (const leaf of set.leaves.slice(0, count))
    frontier = appendMidgardValidationMerkleLeaf(frontier, leaf);
  return {
    signer_count: BigInt(frontier.count),
    signer_peaks: frontier.peaks.map(({ height, hash }) => ({
      height: BigInt(height),
      hash: Buffer.from(hash).toString("hex"),
    })),
  };
};
