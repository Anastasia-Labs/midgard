import {
  requireCanonicalNatural,
  requireCanonicalRational,
  requireExactKeys,
  requireRecord,
} from "./primitives.js";
import { type DeploymentManifestCardanoProtocolParameters } from "./types.js";

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
