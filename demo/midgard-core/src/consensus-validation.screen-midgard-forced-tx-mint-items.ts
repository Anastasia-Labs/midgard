import { compareMidgardAssetKeyBytes } from "./codec/value.js";
import {
  canonicalBytesAt,
  canonicalHeadAt,
  canonicalIntAt,
  definiteArrayHeaderAt,
  eachVariableItem,
  Malformed,
} from "./consensus-validation.forced-tx-admission-reads.js";

const MINT_POLICY_ID_BYTES = 28;
const MINT_MAX_ASSET_NAME_BYTES = 32;

/**
 * Rule 6 on the mint field, the twin of the mint's `mint_items_are_valid`: the
 * checks the validation machine's stage-6 mint fold makes, so an admitted mint
 * field never stalls it. Each item is `[policy_id, {asset_name => quantity}]`:
 * a 28-byte policy id strictly after the previous item's, at least one asset,
 * each name at most 32 bytes and strictly after the previous name, each
 * quantity non-zero, the last asset ending exactly at the end of the item.
 * Keys are ordered by `compareMidgardAssetKeyBytes`. Returns the reason the
 * field is refused, or null.
 */
export const mintItemsRefusal = (preimage: Uint8Array): string | null => {
  let previousPolicy: Uint8Array | null = null;
  return eachVariableItem(preimage, (start, end) => {
    const [policyOffset, itemCount] = definiteArrayHeaderAt(preimage, start);
    if (itemCount !== 2n) {
      throw new Malformed(`policy entry of ${itemCount} items`);
    }
    const [assetsHeaderOffset, policyId] = canonicalBytesAt(
      preimage,
      policyOffset,
    );
    const [assetsOffset, assetCount] = canonicalHeadAt(
      preimage,
      assetsHeaderOffset,
      5,
    );
    if (policyId.length !== MINT_POLICY_ID_BYTES) {
      return `policy id of ${policyId.length} bytes`;
    }
    if (assetCount === 0n) return "policy with no asset";
    if (
      previousPolicy !== null &&
      compareMidgardAssetKeyBytes(previousPolicy, policyId) >= 0
    ) {
      return "policy ids out of order";
    }
    previousPolicy = policyId;
    let cursor = assetsOffset;
    let previousAsset: Uint8Array | null = null;
    for (let remaining = assetCount; remaining > 0n; remaining -= 1n) {
      const [quantityOffset, assetName] = canonicalBytesAt(preimage, cursor);
      const [next, quantity] = canonicalIntAt(preimage, quantityOffset);
      if (assetName.length > MINT_MAX_ASSET_NAME_BYTES) {
        return `asset name of ${assetName.length} bytes`;
      }
      if (quantity === 0n) return "zero quantity";
      if (
        previousAsset !== null &&
        compareMidgardAssetKeyBytes(previousAsset, assetName) >= 0
      ) {
        return "asset names out of order";
      }
      if (remaining === 1n ? next !== end : next >= end) {
        return "assets do not end with the item";
      }
      previousAsset = assetName;
      cursor = next;
    }
    return null;
  });
};
