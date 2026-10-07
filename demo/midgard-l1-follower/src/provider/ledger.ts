import {
  CborMap,
  CborTag,
  type CborValue,
  decodeCbor,
} from "@al-ft/l1-node-transport";
import type {
  CostModels,
  ProtocolParameters,
  SlotConfig,
} from "@lucid-evolution/lucid";

/** A local-state-query answer the provider cannot read. */
export class LedgerAnswerError extends Error {
  override readonly name = "LedgerAnswerError";
}

const fail = (message: string): never => {
  throw new LedgerAnswerError(message);
};

/** A CBOR integer, including a tag-2 bignum. */
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

const natural = (value: CborValue, subject: string): bigint => {
  const result = integer(value, subject);
  return result < 0n ? fail(`${subject} is negative`) : result;
};

const safeNumber = (value: CborValue, subject: string): number => {
  const result = natural(value, subject);
  return result > BigInt(Number.MAX_SAFE_INTEGER)
    ? fail(`${subject} exceeds a safe integer`)
    : Number(result);
};

const array = (
  value: CborValue,
  subject: string,
  minimum = 0,
): readonly CborValue[] =>
  Array.isArray(value) && value.length >= minimum
    ? value
    : fail(`${subject} is not an array of at least ${minimum} items`);

/** A tag-30 rational (or a bare `[numerator, denominator]`) as a number. */
const rational = (value: CborValue, subject: string): number => {
  const pair = array(
    value instanceof CborTag && Number(value.tag) === 30 ? value.value : value,
    subject,
    2,
  );
  const denominator = natural(pair[1]!, `${subject} denominator`);
  if (denominator === 0n) fail(`${subject} has a zero denominator`);
  return (
    Number(integer(pair[0]!, `${subject} numerator`)) / Number(denominator)
  );
};

const PLUTUS_LANGUAGES = [
  [0, "PlutusV1"],
  [1, "PlutusV2"],
  [2, "PlutusV3"],
] as const;

const costModels = (value: CborValue): CostModels => {
  if (!(value instanceof CborMap)) return fail("cost models are not a map");
  const byLanguage = new Map<number, readonly CborValue[]>();
  for (const [key, model] of value.entries)
    byLanguage.set(
      safeNumber(key, "cost model language"),
      array(model, "cost model"),
    );
  const models: Partial<Record<keyof CostModels, number[]>> = {};
  for (const [language, name] of PLUTUS_LANGUAGES) {
    const model = byLanguage.get(language);
    if (model === undefined)
      return fail(`the protocol parameters carry no ${name} cost model`);
    models[name] = model.map((cost) =>
      Number(integer(cost, `${name} cost model entry`)),
    );
  }
  return models as CostModels;
};

/** Conway `PParams` has 31 fields (cardano-ledger `conway.cddl` protocol_param_update order). */
const CONWAY_FIELDS = 31;

/**
 * Decodes the `protocol_params` answer (the current era's `PParams`, Conway
 * field order) into Lucid's protocol parameters.
 */
export const decodeProtocolParameters = (
  bytes: Uint8Array,
): ProtocolParameters => {
  const fields = array(decodeCbor(bytes), "protocol parameters", CONWAY_FIELDS);
  const at = (index: number): CborValue => fields[index]!;
  const version = array(at(12), "protocol version", 2);
  const prices = array(at(16), "execution prices", 2);
  const maxTxExUnits = array(at(17), "max tx execution units", 2);
  return {
    minFeeA: safeNumber(at(0), "minFeeA"),
    minFeeB: safeNumber(at(1), "minFeeB"),
    maxTxSize: safeNumber(at(3), "maxTxSize"),
    keyDeposit: natural(at(5), "keyDeposit"),
    poolDeposit: natural(at(6), "poolDeposit"),
    protocolMajorVersion: safeNumber(version[0]!, "protocol major version"),
    protocolMinorVersion: safeNumber(version[1]!, "protocol minor version"),
    coinsPerUtxoByte: natural(at(14), "coinsPerUtxoByte"),
    costModels: costModels(at(15)),
    priceMem: rational(prices[0]!, "priceMem"),
    priceStep: rational(prices[1]!, "priceStep"),
    maxTxExMem: natural(maxTxExUnits[0]!, "maxTxExMem"),
    maxTxExSteps: natural(maxTxExUnits[1]!, "maxTxExSteps"),
    maxValSize: safeNumber(at(19), "maxValSize"),
    collateralPercentage: safeNumber(at(20), "collateralPercentage"),
    maxCollateralInputs: safeNumber(at(21), "maxCollateralInputs"),
    govActionDeposit: natural(at(27), "govActionDeposit"),
    drepDeposit: natural(at(28), "drepDeposit"),
    minFeeRefScriptCostPerByte: rational(at(30), "minFeeRefScriptCostPerByte"),
  };
};

/** The `system_start` answer, a UTCTime `[year, dayOfYear, picosecondsOfDay]`, in Unix ms. */
export const decodeSystemStart = (bytes: Uint8Array): number => {
  const [year, day, picoseconds] = array(decodeCbor(bytes), "system start", 3);
  const yearStart = Date.UTC(safeNumber(year!, "system start year"), 0, 1);
  const dayOfYear = safeNumber(day!, "system start day");
  if (dayOfYear < 1 || dayOfYear > 366)
    fail("system start day is out of range");
  return (
    yearStart +
    (dayOfYear - 1) * 86_400_000 +
    Number(natural(picoseconds!, "system start time of day") / 1_000_000_000n)
  );
};

type EraSummary = Readonly<{
  startPicoseconds: bigint;
  startSlot: number;
  slotLengthMs: number;
}>;

/**
 * Decodes the `era_history` answer, the hard-fork combinator's summary: a
 * list of `[start, end | null, params]`, each bound `[relativeTime
 * (picoseconds), slot, epoch]`, params `[epochSize, slotLength (ms), ...]`.
 */
export const decodeEraHistory = (bytes: Uint8Array): readonly EraSummary[] => {
  const eras = array(decodeCbor(bytes), "era history", 1).map((era, index) => {
    const [start, , params] = array(era, `era ${index}`, 3);
    const bound = array(start!, `era ${index} start`, 3);
    const slotLengthMs = safeNumber(
      array(params!, `era ${index} parameters`, 2)[1]!,
      `era ${index} slot length`,
    );
    if (slotLengthMs === 0) fail(`era ${index} has a zero slot length`);
    return {
      startPicoseconds: natural(bound[0]!, `era ${index} start time`),
      startSlot: safeNumber(bound[1]!, `era ${index} start slot`),
      slotLengthMs,
    };
  });
  return eras;
};

/**
 * Lucid's slot configuration from the ledger: the start of the earliest era
 * of the contiguous run, ending at the current era, whose slots share the
 * current era's length. Slot-to-time conversion is linear from there.
 */
export const slotConfigFrom = (
  systemStartMs: number,
  eras: readonly EraSummary[],
): SlotConfig => {
  const last = eras[eras.length - 1];
  if (last === undefined) return fail("the era history is empty");
  let first = last;
  for (let index = eras.length - 2; index >= 0; index -= 1) {
    const era = eras[index]!;
    if (era.slotLengthMs !== last.slotLengthMs) break;
    first = era;
  }
  return {
    zeroTime: systemStartMs + Number(first.startPicoseconds / 1_000_000_000n),
    zeroSlot: first.startSlot,
    slotLength: last.slotLengthMs,
  };
};
