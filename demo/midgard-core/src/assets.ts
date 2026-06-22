export type AssetUnit = "lovelace" | string;
export type Assets = Readonly<Record<AssetUnit, bigint>>;

export const LOVELACE_UNIT = "lovelace";
export const ADA_POLICY_ID = "";
export const ADA_ASSET_NAME = "";

/**
 * Removes zero-quantity units and coerces asset values to bigint.
 */
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

/**
 * Adds two asset maps together, dropping units whose net quantity is zero.
 */
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

export const negateAssets = (assets: Assets): Assets =>
  Object.fromEntries(
    Object.entries(assets).map(([unit, quantity]) => [unit, -BigInt(quantity)]),
  );

export const subtractAssetsDelta = (left: Assets, right: Assets): Assets =>
  addAssets(left, negateAssets(right));

export const assetsEqual = (left: Assets, right: Assets): boolean => {
  const normalizedLeft = normalizeAssets(left);
  const normalizedRight = normalizeAssets(right);
  return (
    Object.keys(normalizedLeft).length ===
      Object.keys(normalizedRight).length &&
    Object.entries(normalizedLeft).every(
      ([unit, quantity]) => normalizedRight[unit] === quantity,
    )
  );
};

export const assetQuantity = (assets: Assets, unit: AssetUnit): bigint =>
  BigInt(assets[unit] ?? 0n);

/**
 * Returns whether every required asset has been fully covered.
 */
export const isZeroAssets = (assets: Assets): boolean =>
  Object.keys(normalizeAssets(assets)).length === 0;

export const assetUnitFromParts = (
  policyId: string,
  assetName: string,
): AssetUnit =>
  policyId === ADA_POLICY_ID && assetName === ADA_ASSET_NAME
    ? LOVELACE_UNIT
    : `${policyId}${assetName}`;

export const assetUnitParts = (
  unit: AssetUnit,
): {
  readonly policyId: string;
  readonly assetName: string;
} =>
  unit === LOVELACE_UNIT
    ? { policyId: ADA_POLICY_ID, assetName: ADA_ASSET_NAME }
    : {
        policyId: unit.slice(0, 56).toLowerCase(),
        assetName: unit.slice(56).toLowerCase(),
      };
