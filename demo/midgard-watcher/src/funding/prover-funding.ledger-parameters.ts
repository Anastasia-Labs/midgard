import { CborTag, type CborValue, decodeCbor } from "@al-ft/l1-node-transport";
import {
  type DeploymentManifestCardanoProtocolParameters,
  parseDeploymentManifestCardanoProtocolParameters,
} from "@al-ft/midgard-core/deployment-manifest-identity";

/**
 * The funding parameters of the node's current Conway `PParams`, read from
 * the raw `protocol_params` local-state-query answer (the 31 fields in
 * cardano-ledger `conway.cddl` order). Rationals stay exact.
 *
 * The reference-script fee tiers past the base price are ledger constants,
 * not protocol parameters: a 25,600-byte tier, a 6/5 multiplier per tier and
 * a 204,800-byte per-transaction maximum (cardano-ledger Conway).
 */
export const CONWAY_REFERENCE_SCRIPT_FEE_TIER_BYTES = 25_600n;
export const CONWAY_REFERENCE_SCRIPT_FEE_MULTIPLIER = Object.freeze({
  numerator: 6n,
  denominator: 5n,
});
export const CONWAY_MAXIMUM_REFERENCE_SCRIPTS_SIZE_BYTES = 204_800n;

const CONWAY_FIELDS = 31;

const fail = (message: string): never => {
  throw new Error(`node protocol parameters: ${message}`);
};

const integer = (value: CborValue, subject: string): bigint => {
  if (typeof value === "number" && Number.isSafeInteger(value))
    return BigInt(value);
  if (typeof value === "bigint") return value;
  if (
    value instanceof CborTag &&
    Number(value.tag) === 2 &&
    value.value instanceof Uint8Array
  )
    return value.value.length === 0
      ? 0n
      : BigInt(`0x${Buffer.from(value.value).toString("hex")}`);
  return fail(`${subject} is not an integer`);
};

const natural = (value: CborValue, subject: string): string => {
  const result = integer(value, subject);
  return result < 0n ? fail(`${subject} is negative`) : result.toString(10);
};

const array = (
  value: CborValue,
  subject: string,
  length: number,
): readonly CborValue[] =>
  Array.isArray(value) && value.length >= length
    ? value
    : fail(`${subject} is not an array of at least ${length.toString()} items`);

const gcd = (left: bigint, right: bigint): bigint => {
  let a = left;
  let b = right;
  while (b !== 0n) [a, b] = [b, a % b];
  return a === 0n ? 1n : a;
};

const reduced = (numerator: bigint, denominator: bigint) => {
  const divisor = gcd(numerator, denominator);
  return Object.freeze({
    numerator: (numerator / divisor).toString(10),
    denominator: (denominator / divisor).toString(10),
  });
};

/** A tag-30 rational, exact and reduced. */
const rational = (value: CborValue, subject: string) => {
  const pair = array(
    value instanceof CborTag && Number(value.tag) === 30 ? value.value : value,
    subject,
    2,
  );
  const numerator = integer(pair[0]!, `${subject} numerator`);
  const denominator = integer(pair[1]!, `${subject} denominator`);
  if (numerator < 0n || denominator <= 0n)
    fail(`${subject} is not a nonnegative rational`);
  return reduced(numerator, denominator);
};

export const deriveDeploymentManifestCardanoProtocolParametersFromLedger = (
  bytes: Uint8Array,
): DeploymentManifestCardanoProtocolParameters => {
  const fields = array(decodeCbor(bytes), "PParams", CONWAY_FIELDS);
  const prices = array(fields[16]!, "execution prices", 2);
  const maxTxExUnits = array(fields[17]!, "max tx execution units", 2);
  return parseDeploymentManifestCardanoProtocolParameters({
    minFeeA: natural(fields[0]!, "minFeeA"),
    minFeeB: natural(fields[1]!, "minFeeB"),
    priceMemory: rational(prices[0]!, "priceMemory"),
    priceSteps: rational(prices[1]!, "priceSteps"),
    coinsPerUtxoByte: natural(fields[14]!, "coinsPerUtxoByte"),
    collateralPercentage: natural(fields[20]!, "collateralPercentage"),
    maxCollateralInputs: natural(fields[21]!, "maxCollateralInputs"),
    maxTxSize: natural(fields[3]!, "maxTxSize"),
    maxValueSize: natural(fields[19]!, "maxValueSize"),
    maxTxExUnits: {
      memory: natural(maxTxExUnits[0]!, "maxTxExUnits.memory"),
      steps: natural(maxTxExUnits[1]!, "maxTxExUnits.steps"),
    },
    referenceScriptFee: {
      base: rational(fields[30]!, "minFeeRefScriptCostPerByte"),
      range: CONWAY_REFERENCE_SCRIPT_FEE_TIER_BYTES.toString(10),
      multiplier: reduced(
        CONWAY_REFERENCE_SCRIPT_FEE_MULTIPLIER.numerator,
        CONWAY_REFERENCE_SCRIPT_FEE_MULTIPLIER.denominator,
      ),
      maximumSizeBytes:
        CONWAY_MAXIMUM_REFERENCE_SCRIPTS_SIZE_BYTES.toString(10),
    },
  });
};
