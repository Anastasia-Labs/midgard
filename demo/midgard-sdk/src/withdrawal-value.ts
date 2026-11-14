/** The chain's rule for whether a withdrawal's requested `l2_value` matches
 * the ledger output it withdraws: `output_value_matches_v1` and the
 * asset-count bound in
 * `onchain/aiken/lib/midgard/fraud-proofs/withdrawal-mistag/step-03.ak`, which
 * the withdrawal-mistag proof judges a verdict by.
 *
 * - The ADA policy holds exactly the empty asset name.
 * - The requested lovelace equals the output's lovelace.
 * - The requested non-ADA assets are exactly the output's, with the same
 *   quantities, in any order: admission's `from_asset_list` accepts policies
 *   in any order, and the proof compares the sorted asset leaves.
 * - The output holds at most `WITHDRAWAL_MISTAG_MAXIMUM_ASSET_COUNT` non-ADA
 *   assets; lovelace is not counted.
 *
 * Step-03 also requires 28-byte policies, asset names of at most 32 bytes and
 * positive quantities. For non-ADA assets, equality with a decoded ledger
 * output implies them: the Midgard codec refuses any other output value.
 * Admission refuses a zero or negative lovelace (`from_asset_list` and
 * `withdrawal_is_funded`).
 */
import type { MidgardValue } from "@al-ft/midgard-core/codec";

import type { Value } from "./common.js";

/** The number of non-ADA assets a ledger output holds, as its ledger
 * descriptor commits it (`asset_count`). */
export const withdrawalLedgerAssetCount = (output: MidgardValue): number =>
  [...output.assets.values()].reduce((total, assets) => total + assets.size, 0);

/** Whether a decoded withdrawal `l2_value` requests exactly the ledger
 * output's value, by the chain's rule above (the asset-count bound is
 * separate). */
export const withdrawalValueMatchesLedgerOutput = (
  requested: Value,
  output: MidgardValue,
): boolean => {
  let lovelace = 0n;
  let assetCount = 0;
  for (const [policy, assets] of requested.entries()) {
    if (policy === "") {
      const entries = [...assets.entries()];
      if (entries.length !== 1 || entries[0]![0] !== "") return false;
      lovelace = entries[0]![1];
      continue;
    }
    const actual = output.assets.get(policy);
    for (const [name, quantity] of assets.entries()) {
      if (actual?.get(name) !== quantity) return false;
      assetCount += 1;
    }
  }
  return (
    lovelace === output.lovelace &&
    assetCount === withdrawalLedgerAssetCount(output)
  );
};
