import { type CborInput, CborTag, encodeCbor } from "@al-ft/l1-node-transport";

/**
 * A node's raw `protocol_params` local-state-query answer: the 31-field
 * Conway `PParams` array (cardano-ledger `conway.cddl` order). Fields the
 * watcher does not read are zero; the rest default to the deployment test
 * snapshot (`WATCHER_TEST_CARDANO_PROTOCOL_PARAMETERS`).
 */
export type LedgerProtocolParameterOverrides = Partial<{
  minFeeA: bigint;
  minFeeB: bigint;
  maxTxSize: bigint;
  coinsPerUtxoByte: bigint;
  priceMemory: readonly [bigint, bigint];
  priceSteps: readonly [bigint, bigint];
  maxTxExUnits: readonly [bigint, bigint];
  maxValueSize: bigint;
  collateralPercentage: bigint;
  maxCollateralInputs: bigint;
  minFeeRefScriptCostPerByte: readonly [bigint, bigint];
}>;

const rational = ([numerator, denominator]: readonly [bigint, bigint]) =>
  new CborTag(30n, [numerator, denominator]);

export const ledgerProtocolParameters = (
  overrides: LedgerProtocolParameterOverrides = {},
): Uint8Array => {
  const value = {
    minFeeA: 44n,
    minFeeB: 155_381n,
    maxTxSize: 16_384n,
    coinsPerUtxoByte: 4_310n,
    priceMemory: [577n, 10_000n] as const,
    priceSteps: [721n, 10_000_000n] as const,
    maxTxExUnits: [16_500_000n, 10_000_000_000n] as const,
    maxValueSize: 5_000n,
    collateralPercentage: 150n,
    maxCollateralInputs: 3n,
    minFeeRefScriptCostPerByte: [15n, 1n] as const,
    ...overrides,
  };
  const fields: CborInput[] = Array.from({ length: 31 }, () => 0n);
  fields[0] = value.minFeeA;
  fields[1] = value.minFeeB;
  fields[3] = value.maxTxSize;
  fields[14] = value.coinsPerUtxoByte;
  fields[16] = [rational(value.priceMemory), rational(value.priceSteps)];
  fields[17] = [...value.maxTxExUnits];
  fields[19] = value.maxValueSize;
  fields[20] = value.collateralPercentage;
  fields[21] = value.maxCollateralInputs;
  fields[30] = rational(value.minFeeRefScriptCostPerByte);
  return encodeCbor(fields);
};

/** A `query` that answers the node's parameters. */
export const ledgerParameterQuery =
  (overrides: LedgerProtocolParameterOverrides = {}) =>
  async (): Promise<Uint8Array> =>
    ledgerProtocolParameters(overrides);
