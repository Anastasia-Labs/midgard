import {
  LucidEvolution,
  MintingPolicy,
  SpendingValidator,
  validatorToAddress,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";
import {
  hubOracleScript,
  stateQueueScript,
  schedulerScript,
  registeredOperatorsScript,
  activeOperatorsScript,
  retiredOperatorsScript,
  escapeHatchScript,
  fraudProofCatalogueScript,
  settlementQueueScript,
} from "./constants.js";
import { ValidatorScript, MultiValidator } from "./types.js";

export const getMultiValidator = (
  lucid: LucidEvolution,
  scripts: ValidatorScript,
): MultiValidator => {
  const mintValidator: MintingPolicy = {
    type: "PlutusV3",
    script: scripts.minting,
  };

  const network = lucid.config().network;
  if (!network) {
    throw Error("Invalid Network option");
  }
  const mintAddress = validatorToAddress(network, mintValidator);

  const spendValidator: SpendingValidator = {
    type: "PlutusV3",
    script: scripts.spending,
  };
  const spendAddress = validatorToAddress(network, spendValidator);

  return {
    spendValidator,
    spendAddress,
    mintValidator,
    mintAddress,
  };
};

export type MidgardValidators = {
  hubOracle: ReturnType<typeof getMultiValidator>;
  stateQueue: ReturnType<typeof getMultiValidator>;
  settlementQueue: ReturnType<typeof getMultiValidator>;
  scheduler: ReturnType<typeof getMultiValidator>;
  registeredOperators: ReturnType<typeof getMultiValidator>;
  activeOperators: ReturnType<typeof getMultiValidator>;
  retiredOperators: ReturnType<typeof getMultiValidator>;
  escapeHatch: ReturnType<typeof getMultiValidator>;
  fraudProofCatalogue: ReturnType<typeof getMultiValidator>;
  // TODO: Add remaining when they're uncommented in constants.ts
};

/**
 * Get ALL Midgard validators with addresses for testing
 */
export const getMidgardValidators = (
  lucid: LucidEvolution,
): Effect.Effect<MidgardValidators, never, never> =>
  Effect.gen(function* () {
    return {
      hubOracle: getMultiValidator(lucid, hubOracleScript),
      stateQueue: getMultiValidator(lucid, stateQueueScript),
      settlementQueue: getMultiValidator(lucid, settlementQueueScript),
      scheduler: getMultiValidator(lucid, schedulerScript),
      registeredOperators: getMultiValidator(lucid, registeredOperatorsScript),
      activeOperators: getMultiValidator(lucid, activeOperatorsScript),
      retiredOperators: getMultiValidator(lucid, retiredOperatorsScript),
      escapeHatch: getMultiValidator(lucid, escapeHatchScript),
      fraudProofCatalogue: getMultiValidator(lucid, fraudProofCatalogueScript),
    };
  });
