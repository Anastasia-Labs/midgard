/** The emulator's live protocol parameters in the deployment manifest's
 * shape, with the ledger's reference-script fee tiers (Conway). */
import type { DeploymentManifestCardanoProtocolParameters } from "@al-ft/midgard-core/deployment-manifest-identity";
import type { Emulator } from "@lucid-evolution/lucid";

/** `value` as the exact decimal rational a node's ledger answer carries. */
const rational = (value: number) => {
  for (let digits = 0; digits <= 15; digits += 1) {
    const denominator = 10 ** digits;
    const numerator = Math.round(value * denominator);
    if (numerator / denominator === value)
      return Object.freeze({
        numerator: numerator.toString(),
        denominator: denominator.toString(),
      });
  }
  throw new Error(`protocol parameter ${value.toString()} is not decimal`);
};

export const emulatorManifestProtocolParameters = (
  emulator: Emulator,
): DeploymentManifestCardanoProtocolParameters => {
  const parameters = emulator.protocolParameters;
  return Object.freeze({
    minFeeA: parameters.minFeeA.toString(),
    minFeeB: parameters.minFeeB.toString(),
    priceMemory: rational(parameters.priceMem),
    priceSteps: rational(parameters.priceStep),
    coinsPerUtxoByte: parameters.coinsPerUtxoByte.toString(),
    collateralPercentage: parameters.collateralPercentage.toString(),
    maxCollateralInputs: parameters.maxCollateralInputs.toString(),
    maxTxSize: parameters.maxTxSize.toString(),
    maxValueSize: parameters.maxValSize.toString(),
    maxTxExUnits: Object.freeze({
      memory: parameters.maxTxExMem.toString(),
      steps: parameters.maxTxExSteps.toString(),
    }),
    referenceScriptFee: Object.freeze({
      base: rational(parameters.minFeeRefScriptCostPerByte),
      range: "25600",
      multiplier: Object.freeze({ numerator: "6", denominator: "5" }),
      maximumSizeBytes: "204800",
    }),
  });
};
