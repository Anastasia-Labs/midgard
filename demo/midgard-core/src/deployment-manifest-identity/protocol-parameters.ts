import {
  greatestCommonDivisor,
  requireCanonicalNatural,
  requireCanonicalRational,
  requireExactKeys,
  requireRecord,
} from "./primitives.js";
import {
  type DeploymentManifestCanonicalRational,
  type DeploymentManifestCardanoProtocolParameters,
} from "./types.js";

export const parseDeploymentManifestCardanoProtocolParameters = (
  value: unknown,
): DeploymentManifestCardanoProtocolParameters => {
  const candidate = requireRecord(
    value,
    "Deployment manifest cardanoProtocolParameters.snapshot",
  );
  requireExactKeys(
    candidate,
    [
      "minFeeA",
      "minFeeB",
      "priceMemory",
      "priceSteps",
      "coinsPerUtxoByte",
      "collateralPercentage",
      "maxCollateralInputs",
      "maxTxSize",
      "maxValueSize",
      "maxTxExUnits",
      "referenceScriptFee",
    ],
    [],
    "cardanoProtocolParameters.snapshot",
  );
  const maxTxExUnits = requireRecord(
    candidate.maxTxExUnits,
    "Deployment manifest cardanoProtocolParameters.snapshot.maxTxExUnits",
  );
  requireExactKeys(
    maxTxExUnits,
    ["memory", "steps"],
    [],
    "cardanoProtocolParameters.snapshot.maxTxExUnits",
  );
  const referenceScriptFee = requireRecord(
    candidate.referenceScriptFee,
    "Deployment manifest cardanoProtocolParameters.snapshot.referenceScriptFee",
  );
  requireExactKeys(
    referenceScriptFee,
    ["base", "range", "multiplier", "maximumSizeBytes"],
    [],
    "cardanoProtocolParameters.snapshot.referenceScriptFee",
  );
  const parsed = {
    minFeeA: requireCanonicalNatural(
      candidate.minFeeA,
      "cardanoProtocolParameters.snapshot.minFeeA",
    ),
    minFeeB: requireCanonicalNatural(
      candidate.minFeeB,
      "cardanoProtocolParameters.snapshot.minFeeB",
    ),
    priceMemory: requireCanonicalRational(
      candidate.priceMemory,
      "cardanoProtocolParameters.snapshot.priceMemory",
    ),
    priceSteps: requireCanonicalRational(
      candidate.priceSteps,
      "cardanoProtocolParameters.snapshot.priceSteps",
    ),
    coinsPerUtxoByte: requireCanonicalNatural(
      candidate.coinsPerUtxoByte,
      "cardanoProtocolParameters.snapshot.coinsPerUtxoByte",
    ),
    collateralPercentage: requireCanonicalNatural(
      candidate.collateralPercentage,
      "cardanoProtocolParameters.snapshot.collateralPercentage",
    ),
    maxCollateralInputs: requireCanonicalNatural(
      candidate.maxCollateralInputs,
      "cardanoProtocolParameters.snapshot.maxCollateralInputs",
    ),
    maxTxSize: requireCanonicalNatural(
      candidate.maxTxSize,
      "cardanoProtocolParameters.snapshot.maxTxSize",
    ),
    maxValueSize: requireCanonicalNatural(
      candidate.maxValueSize,
      "cardanoProtocolParameters.snapshot.maxValueSize",
    ),
    maxTxExUnits: Object.freeze({
      memory: requireCanonicalNatural(
        maxTxExUnits.memory,
        "cardanoProtocolParameters.snapshot.maxTxExUnits.memory",
      ),
      steps: requireCanonicalNatural(
        maxTxExUnits.steps,
        "cardanoProtocolParameters.snapshot.maxTxExUnits.steps",
      ),
    }),
    referenceScriptFee: Object.freeze({
      base: requireCanonicalRational(
        referenceScriptFee.base,
        "cardanoProtocolParameters.snapshot.referenceScriptFee.base",
      ),
      range: requireCanonicalNatural(
        referenceScriptFee.range,
        "cardanoProtocolParameters.snapshot.referenceScriptFee.range",
      ),
      multiplier: requireCanonicalRational(
        referenceScriptFee.multiplier,
        "cardanoProtocolParameters.snapshot.referenceScriptFee.multiplier",
      ),
      maximumSizeBytes: requireCanonicalNatural(
        referenceScriptFee.maximumSizeBytes,
        "cardanoProtocolParameters.snapshot.referenceScriptFee.maximumSizeBytes",
      ),
    }),
  } satisfies DeploymentManifestCardanoProtocolParameters;
  if (
    BigInt(parsed.maxTxSize) === 0n ||
    BigInt(parsed.maxValueSize) === 0n ||
    BigInt(parsed.maxTxExUnits.memory) === 0n ||
    BigInt(parsed.maxTxExUnits.steps) === 0n ||
    BigInt(parsed.coinsPerUtxoByte) === 0n ||
    BigInt(parsed.maxCollateralInputs) === 0n ||
    BigInt(parsed.referenceScriptFee.range) === 0n ||
    BigInt(parsed.referenceScriptFee.maximumSizeBytes) === 0n ||
    BigInt(parsed.referenceScriptFee.base.numerator) === 0n ||
    BigInt(parsed.referenceScriptFee.multiplier.numerator) === 0n
  ) {
    throw new Error(
      "Deployment manifest funding protocol-parameter bounds must be positive",
    );
  }
  return Object.freeze(parsed);
};

const protocolParameterNatural = (value: unknown, field: string): string => {
  if (typeof value === "bigint" && value >= 0n) return value.toString(10);
  if (typeof value === "number" && Number.isSafeInteger(value) && value >= 0) {
    return value.toString(10);
  }
  if (typeof value === "string" && /^(?:0|[1-9][0-9]*)$/u.test(value)) {
    return value;
  }
  throw new Error(`Ogmios protocol parameter ${field} must be a natural`);
};

const protocolParameterRational = (
  value: unknown,
  field: string,
): DeploymentManifestCanonicalRational => {
  let numerator: bigint;
  let denominator: bigint;
  if (typeof value === "string" && /^[0-9]+\/[1-9][0-9]*$/u.test(value)) {
    const [left, right] = value.split("/") as [string, string];
    numerator = BigInt(left);
    denominator = BigInt(right);
  } else if (
    (typeof value === "number" && Number.isFinite(value) && value >= 0) ||
    (typeof value === "string" &&
      /^(?:0|[1-9][0-9]*)(?:\.[0-9]+)?$/u.test(value))
  ) {
    const decimal = typeof value === "number" ? value.toString() : value;
    if (/e/iu.test(decimal)) {
      throw new Error(
        `Ogmios protocol parameter ${field} must not use exponent notation`,
      );
    }
    const [whole, fractional = ""] = decimal.split(".") as [string, string?];
    denominator = 10n ** BigInt(fractional.length);
    numerator = BigInt(`${whole}${fractional}`);
  } else {
    throw new Error(
      `Ogmios protocol parameter ${field} must be an exact nonnegative rational`,
    );
  }
  const divisor = greatestCommonDivisor(numerator, denominator);
  return Object.freeze({
    numerator: (numerator / divisor).toString(10),
    denominator: (denominator / divisor).toString(10),
  });
};

/**
 * Derives the release identity directly from Ogmios' raw Conway protocol-
 * parameter response.  Consumers compare this canonical value with the
 * signed deployment snapshot; a provider-normalized projection is never an
 * authority at runtime.
 */
export const deriveDeploymentManifestCardanoProtocolParametersFromOgmios = (
  value: unknown,
): DeploymentManifestCardanoProtocolParameters => {
  const envelope = requireRecord(value, "Ogmios protocol parameters response");
  const raw = requireRecord(
    Object.prototype.hasOwnProperty.call(envelope, "result")
      ? envelope.result
      : envelope,
    "Ogmios protocol parameters result",
  );
  const minFeeConstant = requireRecord(
    raw.minFeeConstant,
    "Ogmios minFeeConstant",
  );
  const minFeeAda = requireRecord(
    minFeeConstant.ada,
    "Ogmios minFeeConstant.ada",
  );
  const maxTransactionSize = requireRecord(
    raw.maxTransactionSize,
    "Ogmios maxTransactionSize",
  );
  const maxValueSize = requireRecord(raw.maxValueSize, "Ogmios maxValueSize");
  const maxExecutionUnits = requireRecord(
    raw.maxExecutionUnitsPerTransaction,
    "Ogmios maxExecutionUnitsPerTransaction",
  );
  const prices = requireRecord(
    raw.scriptExecutionPrices,
    "Ogmios scriptExecutionPrices",
  );
  const referenceFee = requireRecord(
    raw.minFeeReferenceScripts,
    "Ogmios minFeeReferenceScripts",
  );
  const canonicalMaximum = raw.maxReferenceScriptsSizePerTransaction;
  const legacyMaximum = raw.maxReferenceScriptsSize;
  if (canonicalMaximum === undefined && legacyMaximum === undefined) {
    throw new Error(
      "Ogmios protocol parameters omit maxReferenceScriptsSizePerTransaction",
    );
  }
  const maximum = requireRecord(
    canonicalMaximum ?? legacyMaximum,
    "Ogmios maxReferenceScriptsSizePerTransaction",
  );
  if (
    canonicalMaximum !== undefined &&
    legacyMaximum !== undefined &&
    protocolParameterNatural(
      requireRecord(
        canonicalMaximum,
        "Ogmios maxReferenceScriptsSizePerTransaction",
      ).bytes,
      "maxReferenceScriptsSizePerTransaction.bytes",
    ) !==
      protocolParameterNatural(
        requireRecord(legacyMaximum, "Ogmios maxReferenceScriptsSize").bytes,
        "maxReferenceScriptsSize.bytes",
      )
  ) {
    throw new Error("Ogmios reference-script maximum aliases disagree");
  }
  return parseDeploymentManifestCardanoProtocolParameters({
    minFeeA: protocolParameterNatural(
      raw.minFeeCoefficient,
      "minFeeCoefficient",
    ),
    minFeeB: protocolParameterNatural(
      minFeeAda.lovelace,
      "minFeeConstant.ada.lovelace",
    ),
    priceMemory: protocolParameterRational(
      prices.memory,
      "scriptExecutionPrices.memory",
    ),
    priceSteps: protocolParameterRational(
      prices.cpu,
      "scriptExecutionPrices.cpu",
    ),
    coinsPerUtxoByte: protocolParameterNatural(
      raw.minUtxoDepositCoefficient,
      "minUtxoDepositCoefficient",
    ),
    collateralPercentage: protocolParameterNatural(
      raw.collateralPercentage,
      "collateralPercentage",
    ),
    maxCollateralInputs: protocolParameterNatural(
      raw.maxCollateralInputs,
      "maxCollateralInputs",
    ),
    maxTxSize: protocolParameterNatural(
      maxTransactionSize.bytes,
      "maxTransactionSize.bytes",
    ),
    maxValueSize: protocolParameterNatural(
      maxValueSize.bytes,
      "maxValueSize.bytes",
    ),
    maxTxExUnits: {
      memory: protocolParameterNatural(
        maxExecutionUnits.memory,
        "maxExecutionUnitsPerTransaction.memory",
      ),
      steps: protocolParameterNatural(
        maxExecutionUnits.cpu,
        "maxExecutionUnitsPerTransaction.cpu",
      ),
    },
    referenceScriptFee: {
      base: protocolParameterRational(
        referenceFee.base,
        "minFeeReferenceScripts.base",
      ),
      range: protocolParameterNatural(
        referenceFee.range,
        "minFeeReferenceScripts.range",
      ),
      multiplier: protocolParameterRational(
        referenceFee.multiplier,
        "minFeeReferenceScripts.multiplier",
      ),
      maximumSizeBytes: protocolParameterNatural(
        maximum.bytes,
        "maxReferenceScriptsSizePerTransaction.bytes",
      ),
    },
  });
};
