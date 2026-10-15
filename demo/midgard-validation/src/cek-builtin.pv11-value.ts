import { DataB, DataI, DataMap, DataPair } from "@harmoniclabs/plutus-data";

import {
  MIDGARD_CEK_MAX_VALUE_KEY_BYTES,
  MIDGARD_CEK_MAX_VALUE_QUANTITY,
  MIDGARD_CEK_MIN_VALUE_QUANTITY,
} from "./cek-constant.value.js";

export type CardanoValue = DataMap<DataB, DataMap<DataB, DataI>>;
type Coins = Map<string, Map<string, bigint>>;

const key = (bytes: Uint8Array): string => Buffer.from(bytes).toString("hex");
const withinQuantityBounds = (quantity: bigint): boolean =>
  quantity >= MIDGARD_CEK_MIN_VALUE_QUANTITY &&
  quantity <= MIDGARD_CEK_MAX_VALUE_QUANTITY;
const coins = (value: CardanoValue): Coins =>
  new Map(
    value.map.map(({ fst, snd }) => [
      key(fst.bytes),
      new Map(
        snd.map.map(({ fst: token, snd: quantity }) => [
          key(token.bytes),
          quantity.int,
        ]),
      ),
    ]),
  );

// Hex order is byte order, including proper prefixes. Runtime results retain
// Cardano's strictly ascending keys, nonempty maps and nonzero quantities.
const canonicalValue = (value: Coins): CardanoValue =>
  new DataMap(
    [...value.entries()]
      .sort(([a], [b]) => (a < b ? -1 : a > b ? 1 : 0))
      .flatMap(([policy, tokens]) => {
        const entries = [...tokens.entries()]
          .filter(([, quantity]) => quantity !== 0n)
          .sort(([a], [b]) => (a < b ? -1 : a > b ? 1 : 0))
          .map(
            ([token, quantity]) =>
              new DataPair(
                new DataB(Buffer.from(token, "hex")),
                new DataI(quantity),
              ),
          );
        return entries.length === 0
          ? []
          : [
              new DataPair(
                new DataB(Buffer.from(policy, "hex")),
                new DataMap(entries),
              ),
            ];
      }),
  );

export const cardanoInsertCoin = (
  policy: Uint8Array,
  token: Uint8Array,
  quantity: bigint,
  value: CardanoValue,
): CardanoValue | "failure" => {
  if (
    quantity !== 0n &&
    (policy.length > MIDGARD_CEK_MAX_VALUE_KEY_BYTES ||
      token.length > MIDGARD_CEK_MAX_VALUE_KEY_BYTES ||
      !withinQuantityBounds(quantity))
  )
    return "failure";
  const result = coins(value);
  const policyKey = key(policy);
  const tokenKey = key(token);
  const tokens = result.get(policyKey) ?? new Map<string, bigint>();
  if (quantity === 0n) tokens.delete(tokenKey);
  else tokens.set(tokenKey, quantity);
  result.set(policyKey, tokens);
  return canonicalValue(result);
};

export const cardanoLookupCoin = (
  policy: Uint8Array,
  token: Uint8Array,
  value: CardanoValue,
): bigint => coins(value).get(key(policy))?.get(key(token)) ?? 0n;

export const cardanoUnionValue = (
  left: CardanoValue,
  right: CardanoValue,
): CardanoValue | "failure" => {
  const result = coins(left);
  for (const [policy, tokens] of coins(right)) {
    const destination = result.get(policy) ?? new Map<string, bigint>();
    for (const [token, quantity] of tokens) {
      const combined = (destination.get(token) ?? 0n) + quantity;
      if (!withinQuantityBounds(combined)) return "failure";
      destination.set(token, combined);
    }
    result.set(policy, destination);
  }
  return canonicalValue(result);
};

export const cardanoValueContains = (
  left: CardanoValue,
  right: CardanoValue,
): boolean | "failure" => {
  const available = coins(left);
  const required = coins(right);
  for (const value of [available, required]) {
    for (const tokens of value.values()) {
      for (const quantity of tokens.values())
        if (quantity < 0n) return "failure";
    }
  }
  for (const [policy, tokens] of required) {
    for (const [token, quantity] of tokens) {
      if ((available.get(policy)?.get(token) ?? 0n) < quantity) return false;
    }
  }
  return true;
};

export const cardanoScaleValue = (
  scalar: bigint,
  value: CardanoValue,
): CardanoValue | "failure" => {
  const result = coins(value);
  for (const tokens of result.values()) {
    for (const [token, quantity] of tokens) {
      const product = scalar * quantity;
      if (!withinQuantityBounds(product)) return "failure";
      tokens.set(token, product);
    }
  }
  return canonicalValue(result);
};

/** floor(log2(outer size))+1 plus floor(log2(max inner size))+1; empty is 0. */
export const cardanoValueMaxDepth = (value: CardanoValue): bigint => {
  let maxInner = 0;
  for (const { snd: tokens } of value.map)
    maxInner = Math.max(maxInner, tokens.map.length);
  const depth = (size: number): bigint =>
    size === 0 ? 0n : BigInt(BigInt(size).toString(2).length);
  return depth(value.map.length) + depth(maxInner);
};
