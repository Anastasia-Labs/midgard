import * as SDK from "@al-ft/midgard-sdk";
import type { Assets, UTxO } from "@lucid-evolution/lucid";

const ADA_POLICY_ID = "";
const ADA_ASSET_NAME = "";
export const LOVELACE_UNIT = "lovelace";

export const assetsEqual = (left: Assets, right: Assets): boolean => {
  const normalizedLeft = normalizeAssets(left);
  const normalizedRight = normalizeAssets(right);
  const leftEntries = Object.entries(normalizedLeft).sort(([a], [b]) =>
    a.localeCompare(b),
  );
  const rightEntries = Object.entries(normalizedRight).sort(([a], [b]) =>
    a.localeCompare(b),
  );
  if (leftEntries.length !== rightEntries.length) {
    return false;
  }
  return leftEntries.every(
    ([unit, quantity], index) =>
      rightEntries[index]?.[0] === unit &&
      rightEntries[index]?.[1] === quantity,
  );
};

export const normalizeAssets = (assets: Assets): Assets =>
  Object.fromEntries(
    Object.entries(assets).filter(([, quantity]) => quantity !== 0n),
  ) as Assets;

export const addAssets = (left: Assets, right: Assets): Assets => {
  const result: Record<string, bigint> = { ...left };
  for (const [unit, quantity] of Object.entries(right)) {
    result[unit] = (result[unit] ?? 0n) + quantity;
  }
  return normalizeAssets(result as Assets);
};

const negateAssets = (assets: Assets): Assets =>
  Object.fromEntries(
    Object.entries(assets).map(([unit, quantity]) => [unit, -quantity]),
  ) as Assets;

export const subtractAssets = (left: Assets, right: Assets): Assets =>
  addAssets(left, negateAssets(right));

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
    if (quantity === 0n) {
      continue;
    }
    const policyId =
      unit === LOVELACE_UNIT ? ADA_POLICY_ID : unit.slice(0, 56).toLowerCase();
    const assetName =
      unit === LOVELACE_UNIT ? ADA_ASSET_NAME : unit.slice(56).toLowerCase();
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
      const unit =
        policyId === ADA_POLICY_ID && assetName === ADA_ASSET_NAME
          ? LOVELACE_UNIT
          : `${policyId}${assetName}`;
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

export const isPlainPureAdaUtxo = (utxo: UTxO): boolean =>
  utxo.scriptRef === undefined &&
  utxo.datum === undefined &&
  utxo.datumHash === undefined &&
  isPureAdaUtxo(utxo);
