import {
  addAssets,
  assetsEqual,
  assetUnitFromParts,
  assetUnitParts,
  LOVELACE_UNIT,
  normalizeAssets,
  subtractAssetsDelta as subtractAssets,
} from "@al-ft/midgard-core/assets";
import type { Assets, UTxO } from "@lucid-evolution/lucid";

import * as SDK from "@/reserve-payout/primitives.js";

export {
  addAssets,
  assetsEqual,
  LOVELACE_UNIT,
  normalizeAssets,
  subtractAssets,
};

export const hasNonZeroAssetQuantity = (assets: Assets): boolean =>
  Object.values(assets).some((quantity) => quantity !== 0n);

export const removeAssetUnit = (
  assets: Assets,
  unit: string,
  expectedQuantity: bigint,
): Assets => {
  const actual = assets[unit] ?? 0n;
  if (actual !== expectedQuantity) {
    throw new Error(
      `Expected unit ${unit} quantity ${expectedQuantity.toString()}, got ${actual.toString()}`,
    );
  }
  return subtractAssets(assets, { [unit]: expectedQuantity });
};

export const assetsToValue = (assets: Assets): SDK.Value => {
  const outer = new Map<string, Map<string, bigint>>();
  for (const [unit, quantity] of Object.entries(normalizeAssets(assets))) {
    const { policyId, assetName } = assetUnitParts(unit);
    const inner = outer.get(policyId) ?? new Map<string, bigint>();
    inner.set(assetName, (inner.get(assetName) ?? 0n) + quantity);
    outer.set(policyId, inner);
  }
  return outer;
};

export const valueToAssets = (value: SDK.Value): Assets => {
  const assets: Record<string, bigint> = {};
  for (const [policyId, inner] of value.entries()) {
    for (const [assetName, quantity] of inner.entries()) {
      const unit = assetUnitFromParts(policyId, assetName);
      assets[unit] = (assets[unit] ?? 0n) + quantity;
    }
  }
  return normalizeAssets(assets as Assets);
};

export const minPositiveAssets = (left: Assets, right: Assets): Assets => {
  const result: Record<string, bigint> = {};
  for (const [unit, leftQuantity] of Object.entries(left)) {
    const rightQuantity = right[unit] ?? 0n;
    const taken =
      leftQuantity <= 0n || rightQuantity <= 0n
        ? 0n
        : leftQuantity < rightQuantity
          ? leftQuantity
          : rightQuantity;
    if (taken > 0n) {
      result[unit] = taken;
    }
  }
  return result as Assets;
};

export const assertAssetsNonNegative = (
  assets: Assets,
  context: string,
): void => {
  const negative = Object.entries(assets).filter(
    ([, quantity]) => quantity < 0n,
  );
  if (negative.length > 0) {
    throw new Error(
      `${context} contains negative quantities: ${negative
        .map(([unit, quantity]) => `${unit}=${quantity.toString()}`)
        .join(",")}`,
    );
  }
};

export const assertNoAssetExceeds = (
  actual: Assets,
  target: Assets,
  context: string,
): void => {
  for (const [unit, quantity] of Object.entries(actual)) {
    if (quantity > (target[unit] ?? 0n)) {
      throw new Error(
        `${context} exceeds target for ${unit}: actual=${quantity.toString()},target=${(
          target[unit] ?? 0n
        ).toString()}`,
      );
    }
  }
};

export const isPureAdaUtxo = (utxo: UTxO): boolean =>
  Object.entries(utxo.assets).every(
    ([unit, quantity]) => unit === LOVELACE_UNIT || quantity === 0n,
  );
