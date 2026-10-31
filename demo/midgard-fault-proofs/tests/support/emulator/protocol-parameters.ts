import { readFileSync } from "node:fs";

import { PLUTUS_V3_CANONICAL_COST_MODEL_VIEW } from "@al-ft/midgard-core";
import {
  type CostModels,
  PROTOCOL_PARAMETERS_DEFAULT,
  type ProtocolParameters,
} from "@lucid-evolution/lucid";

import {
  VAN_ROSSEM_MAX_CPU_UNITS,
  VAN_ROSSEM_MAX_MEMORY_UNITS,
  VAN_ROSSEM_MAX_SIGNED_TX_BYTES,
} from "../../../src/proof-fit/van-rossem.js";

export type DiagnosticCardanoParameterOverrides = Pick<
  ProtocolParameters,
  | "minFeeA"
  | "minFeeB"
  | "maxValSize"
  | "maxTxExMem"
  | "maxTxExSteps"
  | "priceMem"
  | "priceStep"
  | "coinsPerUtxoByte"
  | "collateralPercentage"
  | "maxCollateralInputs"
  | "minFeeRefScriptCostPerByte"
  | "costModels"
>;

/**
 * Van Rossem (protocol version 11) transaction limits used by every
 * fault-proof Lucid Evolution emulator lifecycle.
 */
export const VAN_ROSSEM_TRANSACTION_LIMITS = Object.freeze({
  maxTxSize: VAN_ROSSEM_MAX_SIGNED_TX_BYTES,
  maxTxExMem: VAN_ROSSEM_MAX_MEMORY_UNITS,
  maxTxExSteps: VAN_ROSSEM_MAX_CPU_UNITS,
});

export const requireJsonRecord = (
  value: unknown,
  label: string,
): Record<string, unknown> => {
  if (typeof value !== "object" || value === null || Array.isArray(value)) {
    throw new Error(`${label} must be a JSON object`);
  }
  return value as Record<string, unknown>;
};

export const requireFiniteNumber = (
  record: Record<string, unknown>,
  key: string,
): number => {
  const value = record[key];
  if (typeof value !== "number" || !Number.isFinite(value)) {
    throw new Error(`Diagnostic Cardano parameter ${key} must be finite`);
  }
  return value;
};

export const requireNonNegativeInteger = (
  record: Record<string, unknown>,
  key: string,
): number => {
  const value = requireFiniteNumber(record, key);
  if (!Number.isSafeInteger(value) || value < 0) {
    throw new Error(
      `Diagnostic Cardano parameter ${key} must be a non-negative safe integer`,
    );
  }
  return value;
};

export const requireBigIntParameter = (
  record: Record<string, unknown>,
  key: string,
): bigint => {
  const value = record[key];
  if (
    (typeof value !== "string" || !/^(?:0|[1-9][0-9]*)$/u.test(value)) &&
    (typeof value !== "number" || !Number.isSafeInteger(value) || value < 0)
  ) {
    throw new Error(
      `Diagnostic Cardano parameter ${key} must be a non-negative integer`,
    );
  }
  return BigInt(value);
};

export const requireCostModel = (
  costModels: Record<string, unknown>,
  version: keyof CostModels,
): number[] => {
  const value = costModels[version];
  if (
    !Array.isArray(value) ||
    value.some((entry) => typeof entry !== "number" || !Number.isFinite(entry))
  ) {
    throw new Error(
      `Diagnostic Cardano parameter cost_models.${version} must be a finite-number array`,
    );
  }
  return [...value];
};

export const loadDiagnosticCardanoParameterOverrides =
  (): Partial<DiagnosticCardanoParameterOverrides> => {
    const parameterPath =
      process.env.MIDGARD_DIAGNOSTIC_CARDANO_PARAMETERS?.trim();
    if (parameterPath == null || parameterPath.length === 0) {
      return {};
    }
    const parsed = JSON.parse(readFileSync(parameterPath, "utf8")) as unknown;
    if (!Array.isArray(parsed) || parsed.length !== 1) {
      throw new Error(
        "Diagnostic Cardano parameter snapshot must contain exactly one epoch",
      );
    }
    const parameters = requireJsonRecord(
      parsed[0],
      "Diagnostic Cardano parameter snapshot entry",
    );
    const maxTxSize = requireNonNegativeInteger(parameters, "max_tx_size");
    if (maxTxSize !== VAN_ROSSEM_TRANSACTION_LIMITS.maxTxSize) {
      throw new Error(
        `Diagnostic target max_tx_size must be ${VAN_ROSSEM_TRANSACTION_LIMITS.maxTxSize.toString()}, found ${maxTxSize.toString()}`,
      );
    }
    const costModelsJson = requireJsonRecord(
      parameters.cost_models,
      "Diagnostic Cardano parameter cost_models",
    );
    const costModels: CostModels = {
      PlutusV1: requireCostModel(costModelsJson, "PlutusV1"),
      PlutusV2: requireCostModel(costModelsJson, "PlutusV2"),
      PlutusV3: requireCostModel(costModelsJson, "PlutusV3"),
    };
    const maxTxExMem = requireBigIntParameter(parameters, "max_tx_ex_mem");
    const maxTxExSteps = requireBigIntParameter(parameters, "max_tx_ex_steps");
    if (
      maxTxExMem !== VAN_ROSSEM_TRANSACTION_LIMITS.maxTxExMem ||
      maxTxExSteps !== VAN_ROSSEM_TRANSACTION_LIMITS.maxTxExSteps
    ) {
      throw new Error(
        "Diagnostic target transaction ExUnit limits must match Van Rossem",
      );
    }
    return {
      minFeeA: requireNonNegativeInteger(parameters, "min_fee_a"),
      minFeeB: requireNonNegativeInteger(parameters, "min_fee_b"),
      maxValSize: requireNonNegativeInteger(parameters, "max_val_size"),
      maxTxExMem,
      maxTxExSteps,
      priceMem: requireFiniteNumber(parameters, "price_mem"),
      priceStep: requireFiniteNumber(parameters, "price_step"),
      coinsPerUtxoByte: requireBigIntParameter(
        parameters,
        "coins_per_utxo_size",
      ),
      collateralPercentage: requireNonNegativeInteger(
        parameters,
        "collateral_percent",
      ),
      maxCollateralInputs: requireNonNegativeInteger(
        parameters,
        "max_collateral_inputs",
      ),
      minFeeRefScriptCostPerByte: requireFiniteNumber(
        parameters,
        "min_fee_ref_script_cost_per_byte",
      ),
      costModels,
    };
  };

/**
 * `costModels` with the Plutus V3 entries it lacks taken from the canonical
 * view. Lucid's default Plutus V3 model stops before the protocol version 11
 * builtins (entries 297 to 349), which would leave a validator that calls
 * one unaffordable; with them it is costed as on mainnet.
 */
const withCanonicalPlutusV3Tail = (costModels: CostModels): CostModels => ({
  ...costModels,
  PlutusV3: [
    ...costModels.PlutusV3,
    ...PLUTUS_V3_CANONICAL_COST_MODEL_VIEW.slice(costModels.PlutusV3.length),
  ],
});

const diagnosticOverrides = loadDiagnosticCardanoParameterOverrides();

/**
 * The protocol version our wallet's evaluator runs scripts at: mainnet's,
 * whose Plutus V3 builtins include the protocol version 11 ones, as the
 * emulator's ledger already does. Left unset, the evaluator refuses a
 * validator that calls one.
 */
const PROTOCOL_MAJOR_VERSION = 11;

export const EMULATOR_PROTOCOL_PARAMETERS = {
  ...PROTOCOL_PARAMETERS_DEFAULT,
  protocolMajorVersion: PROTOCOL_MAJOR_VERSION,
  ...diagnosticOverrides,
  costModels: withCanonicalPlutusV3Tail(
    diagnosticOverrides.costModels ?? PROTOCOL_PARAMETERS_DEFAULT.costModels,
  ),
  ...VAN_ROSSEM_TRANSACTION_LIMITS,
  maxCollateralInputs: 3,
} as const;
