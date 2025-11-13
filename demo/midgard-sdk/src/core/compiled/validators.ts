import {
  applyDoubleCborEncoding,
  applyParamsToScript,
  MintingPolicy,
  SpendingValidator,
  Script,
} from "@lucid-evolution/lucid";

/**
 * Midgard validator set
 */
export type BluePrintValidators = {
  // Hub Oracle
  hubOracleSpend: SpendingValidator;
  hubOracleMint: MintingPolicy;

  // State Queue (LinkedList)
  stateQueueSpend: SpendingValidator;
  stateQueueMint: MintingPolicy;

  // Settlement Queue (LinkedList)
  settlementQueueSpend: SpendingValidator;
  settlementQueueMint: MintingPolicy;

  // Registered Operators (LinkedList)
  registeredOperatorsMint: MintingPolicy;

  // Active Operators (LinkedList)
  activeOperatorsSpend: SpendingValidator;
  activeOperatorsMint: MintingPolicy;

  // Retired Operators (LinkedList)
  retiredOperatorsMint: MintingPolicy;

  // Scheduler
  schedulerSpend: SpendingValidator;
  schedulerMint: MintingPolicy;

  // Escape Hatch
  escapeHatchSpend: SpendingValidator;
  escapeHatchMint: MintingPolicy;

  // Fraud Proof Catalogue
  fraudProofCatalogueSpend: SpendingValidator;
  fraudProofCatalogueMint: MintingPolicy;

  // Fraud Proof
  //   fraudProofSpend: SpendingValidator;
  //   fraudProofMint: MintingPolicy;
};

/**
 * Read Midgard validators from blueprint
 *
 * @param blueprint - Plutus/Aiken blueprint JSON
 * @param params - Whether to apply parameters
 * @param policyIds - Policy IDs to apply as parameters
 */
export function readMidgardValidators(
  blueprint: any,
  params: boolean,
  policyIds: string[],
): BluePrintValidators {
  const getValidator = (title: string): Script => {
    console.log("Validator title: ", title);
    const validator = blueprint.validators.find(
      (v: { title: string }) => v.title === title,
    );
    if (!validator) throw new Error(`Validator not found: ${title}`);

    let script = applyDoubleCborEncoding(validator.compiledCode);

    if (params && policyIds.length > 0) {
      script = applyParamsToScript(script, policyIds);
    }

    return {
      type: "PlutusV3",
      script: script,
    };
  };

  return {
    // Hub Oracle
    hubOracleSpend: getValidator("always_succeeds.hub_oracle_spend.else"),
    hubOracleMint: getValidator("always_succeeds.hub_oracle_mint.else"),

    // State Queue
    stateQueueSpend: getValidator("always_succeeds.state_queue_spend.else"),
    stateQueueMint: getValidator("always_succeeds.state_queue_mint.else"),

    // Settlement Queue
    settlementQueueSpend: getValidator(
      "always_succeeds.settlement_queue_spend.else",
    ),
    settlementQueueMint: getValidator(
      "always_succeeds.settlement_queue_mint.else",
    ),

    // Registered Operators
    registeredOperatorsMint: getValidator(
      "always_succeeds.registered_operators_mint.else",
    ),

    // Active Operators
    activeOperatorsSpend: getValidator(
      "always_succeeds.active_operators_spend.else",
    ),
    activeOperatorsMint: getValidator(
      "always_succeeds.active_operators_mint.else",
    ),

    // Retired Operators
    retiredOperatorsMint: getValidator(
      "always_succeeds.retired_operators_mint.else",
    ),

    // Scheduler
    schedulerSpend: getValidator("always_succeeds.scheduler_spend.else"),
    schedulerMint: getValidator("always_succeeds.scheduler_mint.else"),

    // Escape Hatch
    escapeHatchSpend: getValidator("always_succeeds.escape_hatch_spend.else"),
    escapeHatchMint: getValidator("always_succeeds.escape_hatch_mint.else"),

    // Fraud Proof Catalogue
    fraudProofCatalogueSpend: getValidator(
      "always_succeeds.fraud_proof_catalogue_spend.else",
    ),
    fraudProofCatalogueMint: getValidator(
      "always_succeeds.fraud_proof_catalogue_mint.else",
    ),

    // Fraud Proof
    // fraudProofSpend: getValidator("fraud_proof.spend"),
    // fraudProofMint: getValidator("fraud_proof.mint"),
  };
}
