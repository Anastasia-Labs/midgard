import type { DeploymentManifestCardanoProtocolParameters } from "@al-ft/midgard-core/deployment-manifest-identity-v1";

export const TEST_CARDANO_PROTOCOL_PARAMETERS = Object.freeze({
  minFeeA: "44",
  minFeeB: "155381",
  priceMemory: Object.freeze({ numerator: "577", denominator: "10000" }),
  priceSteps: Object.freeze({ numerator: "721", denominator: "10000000" }),
  coinsPerUtxoByte: "4310",
  collateralPercentage: "150",
  maxCollateralInputs: "3",
  maxTxSize: "16384",
  maxValueSize: "5000",
  maxTxExUnits: Object.freeze({
    memory: "16500000",
    steps: "10000000000",
  }),
  referenceScriptFee: Object.freeze({
    base: Object.freeze({ numerator: "15", denominator: "1" }),
    range: "25600",
    multiplier: Object.freeze({ numerator: "6", denominator: "5" }),
    maximumSizeBytes: "204800",
  }),
}) satisfies DeploymentManifestCardanoProtocolParameters;
