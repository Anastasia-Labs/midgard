import { type Assets, normalizeAssets } from "@al-ft/midgard-core/assets";
import {
  assetsToValue as lucidAssetsToValue,
  CML,
  valueToAssets as lucidValueToAssets,
} from "@lucid-evolution/lucid";

import { InsufficientFundsError } from "./errors.js";

export type { Assets, AssetUnit } from "@al-ft/midgard-core/assets";
export {
  addAssets,
  assetQuantity,
  isZeroAssets,
  normalizeAssets,
} from "@al-ft/midgard-core/assets";
export type CmlValue = CML.Value;
export type ValueLike = Assets | CmlValue | bigint;

const isCmlValue = (value: ValueLike): value is CmlValue =>
  value instanceof CML.Value ||
  (typeof value === "object" &&
    value !== null &&
    typeof (value as { readonly coin?: unknown }).coin === "function" &&
    typeof (value as { readonly multi_asset?: unknown }).multi_asset ===
      "function" &&
    typeof (value as { readonly to_cbor_bytes?: unknown }).to_cbor_bytes ===
      "function");

export const assertNonNegativeAssets = (
  assets: Assets,
  fieldName = "assets",
): Assets => {
  const normalized = normalizeAssets(assets);
  for (const [unit, amount] of Object.entries(normalized)) {
    if (amount < 0n) {
      throw new InsufficientFundsError({
        unit: `${fieldName}.${unit}`,
        required: 0n,
        available: amount,
      });
    }
  }
  return normalized;
};

export const subtractAssets = (left: Assets, right: Assets): Assets => {
  const result: Record<string, bigint> = {
    ...assertNonNegativeAssets(left, "left"),
  };
  for (const [unit, amount] of Object.entries(
    assertNonNegativeAssets(right, "right"),
  )) {
    const available = result[unit] ?? 0n;
    if (available < amount) {
      throw new InsufficientFundsError({
        unit,
        required: amount,
        available,
      });
    }
    const next = available - amount;
    if (next === 0n) {
      delete result[unit];
    } else {
      result[unit] = next;
    }
  }
  return result;
};

export const cmlValueToAssets = (value: CmlValue): Assets =>
  normalizeAssets(lucidValueToAssets(value) as Assets);

export const assetsToCmlValue = (assets: Assets): CmlValue =>
  lucidAssetsToValue(assertNonNegativeAssets(assets)) as CmlValue;

export const normalizeValueLike = (value: ValueLike): Assets => {
  if (typeof value === "bigint") {
    return normalizeAssets({ lovelace: value });
  }
  if (isCmlValue(value)) {
    return cmlValueToAssets(value);
  }
  return normalizeAssets(value);
};

export const valueLikeToCmlValue = (value: ValueLike): CmlValue =>
  assetsToCmlValue(normalizeValueLike(value));
