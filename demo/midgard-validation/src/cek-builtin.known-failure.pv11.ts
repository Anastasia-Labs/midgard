/**
 * The protocol version 11 part (tags 87 to 100) of the builtin failures the
 * direct failure arm proves: the TypeScript twin of `pv11_builtin_failure_v1`
 * (cek-builtin-v1.ak), reading its arguments in the Aiken order.
 */
import { type MidgardCekDirectValueWitness } from "./cek-builtin.argument-kinds.js";
import {
  bytes,
  constantPayload,
  constantType,
  dataPayload,
  exactly,
  integer,
  isType,
  listPayload,
  refuse,
} from "./cek-builtin.known-failure.readers.js";
import {
  isMidgardCekCanonicalValueData,
  MIDGARD_CEK_MAX_VALUE_KEY_BYTES,
  MIDGARD_CEK_MAX_VALUE_QUANTITY,
  MIDGARD_CEK_MIN_VALUE_QUANTITY,
} from "./cek-constant.value.js";
import { type Data, DataB, DataI, DataList, DataMap } from "./uplc-data.js";

/** expModInteger's operands lie in [-2^8191, 2^8191 - 1]. */
const EXP_MOD_BOUND = 2n ** 8191n;
/** Multi-scalar multiplication scalars lie in [-2^4095, 2^4095 - 1]. */
const MSM_SCALAR_BOUND = 2n ** 4095n;

type ValueEntry = {
  readonly currency: Uint8Array;
  readonly token: Uint8Array;
  readonly quantity: bigint;
};

/** `array_payload_v1`: an array's elements' payloads. */
const arrayItems = (value: MidgardCekDirectValueWitness): readonly Data[] => {
  if (constantType(value).kind !== "array") refuse("expected array");
  const payload = constantPayload(value);
  return payload instanceof DataList ? payload.list : refuse("array payload");
};

/** `value_payload_v1`, read entry by entry as `any_value_entry_v1` reads it. */
const valueEntries = (
  value: MidgardCekDirectValueWitness,
): readonly ValueEntry[] => {
  if (!isType(value, "value")) refuse("expected value");
  const payload = constantPayload(value);
  if (!(payload instanceof DataMap)) return refuse("value payload");
  return (payload as DataMap<Data, Data>).map.flatMap(({ fst, snd }) => {
    if (!(fst instanceof DataB) || !(snd instanceof DataMap)) {
      return refuse("value currency");
    }
    return (snd as DataMap<Data, Data>).map.map(
      ({ fst: token, snd: amount }) =>
        token instanceof DataB && amount instanceof DataI
          ? { currency: fst.bytes, token: token.bytes, quantity: amount.int }
          : refuse("value token"),
    );
  });
};

const outOfBound = (value: bigint, bound: bigint): boolean =>
  value < -bound || value >= bound;

const quantityOutOfBound = (quantity: bigint): boolean =>
  quantity < MIDGARD_CEK_MIN_VALUE_QUANTITY ||
  quantity > MIDGARD_CEK_MAX_VALUE_QUANTITY;

const greatestCommonDivisor = (left: bigint, right: bigint): bigint => {
  let [a, b] = [left, right];
  while (b !== 0n) [a, b] = [b, a % b];
  return a;
};

const sameKey = (left: Uint8Array, right: Uint8Array): boolean =>
  Buffer.compare(Buffer.from(left), Buffer.from(right)) === 0;

/** `builtin.lookup_value` over a Value's entries. */
const lookup = (
  entries: readonly ValueEntry[],
  currency: Uint8Array,
  token: Uint8Array,
): bigint =>
  entries.find(
    (entry) => sameKey(entry.currency, currency) && sameKey(entry.token, token),
  )?.quantity ?? 0n;

const expModFails = (
  arguments_: readonly MidgardCekDirectValueWitness[],
): boolean => {
  const [baseValue, exponentValue, modulusValue] = exactly(arguments_, 3);
  const base = integer(baseValue!);
  const exponent = integer(exponentValue!);
  const modulus = integer(modulusValue!);
  if (modulus <= 0n || outOfBound(modulus, EXP_MOD_BOUND)) return true;
  if (modulus === 1n) return false;
  // A base of zero under a negative exponent has no inverse either.
  return (
    outOfBound(base, EXP_MOD_BOUND) ||
    outOfBound(exponent, EXP_MOD_BOUND) ||
    (exponent < 0n &&
      greatestCommonDivisor(modulus, ((base % modulus) + modulus) % modulus) !==
        1n)
  );
};

const multiScalarMulFails = (
  tag: number,
  arguments_: readonly MidgardCekDirectValueWitness[],
): boolean => {
  const [scalars, points] = exactly(arguments_, 2);
  const [scalarType, scalarItems] = listPayload(scalars!);
  const [pointType] = listPayload(points!);
  return (
    scalarType.kind === "integer" &&
    pointType.kind === (tag === 92 ? "blsG1" : "blsG2") &&
    scalarItems.some((scalar) =>
      scalar instanceof DataI
        ? outOfBound(scalar.int, MSM_SCALAR_BOUND)
        : refuse("scalar payload"),
    )
  );
};

const insertCoinFails = (
  arguments_: readonly MidgardCekDirectValueWitness[],
): boolean => {
  const [currencyValue, tokenValue, quantityValue, value] = exactly(
    arguments_,
    4,
  );
  const currency = bytes(currencyValue!);
  const token = bytes(tokenValue!);
  const quantity = integer(quantityValue!);
  valueEntries(value!);
  // Deleting a coin (a zero quantity) never fails.
  return (
    quantity !== 0n &&
    (currency.length > MIDGARD_CEK_MAX_VALUE_KEY_BYTES ||
      token.length > MIDGARD_CEK_MAX_VALUE_KEY_BYTES ||
      quantityOutOfBound(quantity))
  );
};

/**
 * Whether protocol version 11 builtin `tag` over `arguments_` is a failure
 * the direct failure arm proves. dropList, lengthOfArray, listToArray and
 * lookupCoin never fail; valueData fails only past 40,000 entries, which no
 * direct payload holds. Throws where `pv11_builtin_failure_v1` fails the
 * script.
 */
export const midgardCekKnownPv11BuiltinFailure = (
  tag: bigint,
  arguments_: readonly MidgardCekDirectValueWitness[],
): boolean => {
  const number = Number(tag);
  if (number === 87) return expModFails(arguments_);
  if (number === 91) {
    const [source, indexValue] = exactly(arguments_, 2);
    const items = arrayItems(source!);
    const index = integer(indexValue!);
    return index < 0n || index >= BigInt(items.length);
  }
  if (number === 92 || number === 93) {
    return multiScalarMulFails(number, arguments_);
  }
  if (number === 94) return insertCoinFails(arguments_);
  if (number === 96) {
    // Only a coin both Values hold can sum out of range.
    const [left, right] = exactly(arguments_, 2);
    const rightEntries = valueEntries(right!);
    return valueEntries(left!).some(({ currency, token, quantity }) =>
      quantityOutOfBound(quantity + lookup(rightEntries, currency, token)),
    );
  }
  if (number === 97) {
    const [left, right] = exactly(arguments_, 2);
    const leftEntries = valueEntries(left!);
    const rightEntries = valueEntries(right!);
    return [...leftEntries, ...rightEntries].some(
      ({ quantity }) => quantity < 0n,
    );
  }
  if (number === 99) {
    const [source] = exactly(arguments_, 1);
    return !isMidgardCekCanonicalValueData(dataPayload(source!));
  }
  if (number === 100) {
    const [scalarValue, value] = exactly(arguments_, 2);
    const scalar = integer(scalarValue!);
    return valueEntries(value!).some(({ quantity }) =>
      quantityOutOfBound(scalar * quantity),
    );
  }
  return false;
};
