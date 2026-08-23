import type { MidgardValue } from "@al-ft/midgard-core/codec";
import { MIDGARD_CONSENSUS_LIMITS_V1 } from "@al-ft/midgard-core/consensus-profile-v1";

import type { MidgardLedgerMint } from "./ledger-tx/types.js";
import type { ScriptMintValue } from "./script-context.js";
import type { MidgardValueDelta } from "./types.js";

type MutableAssets = Map<string, Map<string, bigint>>;

const addAssetDelta = (
  assets: MutableAssets,
  policyId: string,
  assetName: string,
  quantity: bigint,
): void => {
  if (quantity === 0n) {
    return;
  }
  const policy = assets.get(policyId) ?? new Map<string, bigint>();
  const nextQuantity = (policy.get(assetName) ?? 0n) + quantity;
  if (nextQuantity === 0n) {
    policy.delete(assetName);
  } else {
    policy.set(assetName, nextQuantity);
  }
  if (policy.size === 0) {
    assets.delete(policyId);
  } else {
    assets.set(policyId, policy);
  }
};

const orderedAssets = (
  assets: MutableAssets,
): ReadonlyMap<string, ReadonlyMap<string, bigint>> =>
  new Map(
    [...assets.entries()]
      .sort(([left], [right]) => (left < right ? -1 : left > right ? 1 : 0))
      .map(([policyId, policyAssets]) => [
        policyId,
        new Map(
          [...policyAssets.entries()].sort(([left], [right]) =>
            left < right ? -1 : left > right ? 1 : 0,
          ),
        ),
      ]),
  );

const addValueInto = (
  assets: MutableAssets,
  value: MidgardValue,
  multiplier: bigint,
): bigint => {
  for (const [policyId, policyAssets] of value.assets.entries()) {
    for (const [assetName, quantity] of policyAssets.entries()) {
      addAssetDelta(assets, policyId, assetName, quantity * multiplier);
    }
  }
  return value.lovelace * multiplier;
};

const addDeltaInto = (
  assets: MutableAssets,
  delta: MidgardValueDelta,
  multiplier: bigint,
): bigint => {
  for (const [policyId, policyAssets] of delta.assets.entries()) {
    for (const [assetName, quantity] of policyAssets.entries()) {
      addAssetDelta(assets, policyId, assetName, quantity * multiplier);
    }
  }
  return delta.lovelace * multiplier;
};

export const zeroMidgardValue = (): MidgardValue => ({
  lovelace: 0n,
  assets: new Map(),
});

export const sumMidgardValues = (
  values: readonly MidgardValue[],
): MidgardValue => {
  const assets: MutableAssets = new Map();
  let lovelace = 0n;
  for (const value of values) {
    lovelace += addValueInto(assets, value, 1n);
  }
  return {
    lovelace,
    assets: orderedAssets(assets),
  };
};

export const mintToValueDelta = (
  mint: MidgardLedgerMint,
): MidgardValueDelta => {
  const assets: MutableAssets = new Map();
  for (const entry of mint.assets) {
    addAssetDelta(
      assets,
      entry.policyId.toString("hex"),
      entry.assetName.toString("hex"),
      entry.quantity,
    );
  }
  return {
    lovelace: 0n,
    assets: orderedAssets(assets),
  };
};

export const valuePreservationDelta = (
  inputSum: MidgardValue,
  fee: bigint,
  mintDelta: MidgardValueDelta,
  outputSum: MidgardValue,
): MidgardValueDelta => {
  const assets: MutableAssets = new Map();
  const lovelace =
    addValueInto(assets, inputSum, 1n) -
    fee +
    addDeltaInto(assets, mintDelta, 1n) +
    addValueInto(assets, outputSum, -1n);
  return {
    lovelace,
    assets: orderedAssets(assets),
  };
};

export const isZeroValueDelta = (delta: MidgardValueDelta): boolean =>
  delta.lovelace === 0n &&
  [...delta.assets.values()].every((assets) =>
    [...assets.values()].every((quantity) => quantity === 0n),
  );

export const describeValueDelta = (delta: MidgardValueDelta): string => {
  const assets = [...delta.assets.entries()]
    .flatMap(([policyId, policyAssets]) =>
      [...policyAssets.entries()].map(
        ([assetName, quantity]) => `${policyId}.${assetName}=${quantity}`,
      ),
    )
    .join(",");
  return `lovelace=${delta.lovelace} assets=${assets === "" ? "none" : assets}`;
};

export const mintDeltaToScriptMintValue = (
  mintDelta: MidgardValueDelta,
): ScriptMintValue => {
  const result = new Map<string, Map<string, bigint>>();
  for (const [policyId, policyAssets] of mintDelta.assets.entries()) {
    result.set(policyId, new Map(policyAssets));
  }
  return result;
};

// C49 (#540). Mirrors `min_ada_output_overhead_bytes`/`min_ada_lovelace_v1`/
// `output_meets_min_ada_v1` in
// onchain/aiken/lib/midgard/validation-machine-v1.ak:2168-2216 verbatim: the
// Cardano minimum-Ada floor for one ledger output is linear in its serialized
// size, with a fixed 160-byte UTxO entry overhead and no additional Midgard
// margin at any size.
//
// This is a HAND MIRROR of the Aiken twin, not a call into a ledger library.
// `CML.min_ada_required` does appear in
// `demo/midgard-sdk/src/fraud-proof/validation-proof-item-v1.ts`, but it funds a
// _proof publication output_ on L1 and is not this consensus quantity; do not
// read the two as the same computation (decision 0003, Q27).
//
// D-S4 (#618): because both copies are hand-written, they are bound to each
// other on pinned absolute vectors -- including the exactly-at-the-floor accept
// and one-lovelace-below reject legs -- by
// `tests/min-ada-twin-cross-check-v1.test.ts`. Changing the slope, the
// intercept, or the comparison on either side fails that suite.
export const MIN_ADA_OUTPUT_OVERHEAD_BYTES_V1 = 160n;

export const minAdaLovelaceV1 = (
  coinsPerUtxoByte: bigint,
  serializedOutputBytes: bigint,
): bigint =>
  coinsPerUtxoByte * (MIN_ADA_OUTPUT_OVERHEAD_BYTES_V1 + serializedOutputBytes);

export const outputMeetsMinAdaV1 = (
  coinsPerUtxoByte: bigint,
  serializedOutputBytes: bigint,
  lovelace: bigint,
): boolean =>
  lovelace >= minAdaLovelaceV1(coinsPerUtxoByte, serializedOutputBytes);

/**
 * The deployment rate the two twins above are instantiated at.
 *
 * The floor itself stays parameterized -- both twins take
 * `coinsPerUtxoByte` as an argument and remain linear in the serialized size,
 * which is what decision 0003 protects when it forbids replacing the floor
 * with a flat constant. What is compiled is only the *rate's source*: per the
 * owner ruling on #627 (option B), `coins_per_utxo_byte` is a deployment
 * constant rather than a block-header field or a validation-context element,
 * because the rate that convicts a block is not the block producer's to
 * declare. Its Aiken counterpart is `env.coins_per_utxo_byte`
 * (onchain/aiken/env/default.ak, onchain/aiken/env/testnet.ak), and
 * `tests/min-ada-twin-cross-check-v1.test.ts` holds the two production pins
 * equal to each other and to the C70 snapshot.
 */
export const MIDGARD_COINS_PER_UTXO_BYTE_V1 = BigInt(
  MIDGARD_CONSENSUS_LIMITS_V1.coinsPerUtxoByte,
);

/**
 * `E_MIN_ADA` (MIN-ADA-TX) over one canonical serialized transaction output.
 *
 * The off-chain twin of the ValueAndMint stage-3 output-descriptor conjunct in
 * onchain/aiken/lib/midgard/validation-machine-v1.ak: the on-chain step reads
 * `descriptor.total_length` and `descriptor.lovelace` off the authenticated
 * output descriptor, and `total_length` is by construction the length of these
 * very bytes (`buildMidgardLedgerOutputMaterialV1` sets it from the canonical
 * output CBOR).
 */
export const outputCborMeetsMinAdaV1 = (
  outputCbor: Uint8Array,
  lovelace: bigint,
): boolean =>
  outputMeetsMinAdaV1(
    MIDGARD_COINS_PER_UTXO_BYTE_V1,
    BigInt(outputCbor.length),
    lovelace,
  );

/** The absolute floor, in lovelace, that `outputCborMeetsMinAdaV1` compares against. */
export const outputCborMinAdaLovelaceV1 = (outputCbor: Uint8Array): bigint =>
  minAdaLovelaceV1(MIDGARD_COINS_PER_UTXO_BYTE_V1, BigInt(outputCbor.length));
