/**
 * Output selectors shared by the operator-lifecycle transaction builders.
 *
 * Every builder resolves its redeemer indices from the *final* transaction
 * outputs, so each output it produces needs a selector precise enough to match
 * exactly one of them: address, inline datum CBOR, and the list NFT together.
 */
import type { Assets, TxOutput } from "@lucid-evolution/lucid";

import { outputDatumCborMatches } from "../tx-output-utils.js";

/**
 * Whether an output is the linked-list element carrying `unit` at `address`
 * with the inline datum `datum`.
 */
export const outputMatchesElement = ({
  output,
  address,
  datum,
  unit,
}: {
  readonly output: TxOutput;
  readonly address: string;
  readonly datum: string;
  readonly unit: string;
}): boolean =>
  output.address === address &&
  outputDatumCborMatches(output, datum) &&
  (output.assets[unit] ?? 0n) === 1n;

/**
 * Returns the single NFT unit of `policyId` held in `assets`, failing loudly
 * when the element does not hold exactly one.
 */
export const requirePolicyNftUnit = (
  assets: Assets,
  policyId: string,
  label: string,
): string => {
  const units = Object.entries(assets)
    .filter(
      ([unit, quantity]) =>
        unit !== "lovelace" && unit.startsWith(policyId) && quantity === 1n,
    )
    .map(([unit]) => unit);
  if (units.length !== 1) {
    throw new Error(
      `${label} expected exactly one ${policyId} NFT unit, got ${units.length.toString()}`,
    );
  }
  return units[0]!;
};
