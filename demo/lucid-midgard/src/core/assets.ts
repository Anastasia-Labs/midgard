import {
  CML,
  assetsToValue as lucidAssetsToValue,
  valueToAssets as lucidValueToAssets,
  type Assets as LucidAssets,
} from "@lucid-evolution/lucid";
import { InsufficientFundsError } from "./errors.js";

export type AssetUnit = "lovelace" | string;
export type Assets = Readonly<Record<AssetUnit, bigint>>;
export type CmlValue = InstanceType<typeof CML.Value>;
export type ValueLike = Assets | CmlValue | bigint;

const isCmlValue = (value: ValueLike): value is CmlValue =>
  typeof value === "object" && value instanceof CML.Value;

export const normalizeAssets = (assets: Assets): Assets => {
  const normalized: Record<string, bigint> = {};
  for (const [unit, amount] of Object.entries(assets)) {
    const quantity = BigInt(amount);
    if (quantity !== 0n) {
      normalized[unit] = quantity;
    }
  }
  return normalized;
};

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

export const addAssets = (left: Assets, right: Assets): Assets => {
  const result: Record<string, bigint> = { ...normalizeAssets(left) };
  for (const [unit, amount] of Object.entries(right)) {
    const quantity = BigInt(amount);
    if (quantity === 0n) {
      continue;
    }
    result[unit] = (result[unit] ?? 0n) + quantity;
    if (result[unit] === 0n) {
      delete result[unit];
    }
  }
  return result;
};

export const subtractAssets = (left: Assets, right: Assets): Assets => {
  const result: Record<string, bigint> = {
    ...assertNonNegativeAssets(left, "left"),
  };
  for (const [unit, amount] of Object.entries(
    assertNonNegativeAssets(right, "right"),
  )) {
    const quantity = BigInt(amount);
    if (quantity === 0n) {
      continue;
    }
    const available = result[unit] ?? 0n;
    if (available < quantity) {
      throw new InsufficientFundsError({
        unit,
        required: quantity,
        available,
      });
    }
    const next = available - quantity;
    if (next === 0n) {
      delete result[unit];
    } else {
      result[unit] = next;
    }
  }
  return result;
};

export const assetQuantity = (assets: Assets, unit: AssetUnit): bigint =>
  BigInt(assets[unit] ?? 0n);

export const isZeroAssets = (assets: Assets): boolean =>
  Object.keys(normalizeAssets(assets)).length === 0;

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

export const assetsToLucidAssets = (assets: Assets): LucidAssets =>
  assertNonNegativeAssets(assets) as LucidAssets;
