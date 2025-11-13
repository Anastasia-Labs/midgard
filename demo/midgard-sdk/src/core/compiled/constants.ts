import { mintingPolicyToId } from "@lucid-evolution/lucid";
import { readMidgardValidators } from "./validators.js";
import blueprint from "./plutus.json" with { type: "json" };

// ============================================================================
// Hub Oracle
// ============================================================================

const hubOracleValidator = readMidgardValidators(blueprint, false, []);
const hubOraclePolicyId = mintingPolicyToId(hubOracleValidator.hubOracleMint);

const hubOracleScript = {
  spending: hubOracleValidator.hubOracleSpend.script,
  minting: hubOracleValidator.hubOracleMint.script,
  staking: "",
};

// ============================================================================
// State Queue
// ============================================================================

const stateQueueValidator = readMidgardValidators(blueprint, true, [
  hubOraclePolicyId,
]);
const stateQueuePolicyId = mintingPolicyToId(
  stateQueueValidator.stateQueueMint,
);

const stateQueueScript = {
  spending: stateQueueValidator.stateQueueSpend.script,
  minting: stateQueueValidator.stateQueueMint.script,
  staking: "",
};

// ============================================================================
// Settlement Queue
// ============================================================================

const settlementQueueValidator = readMidgardValidators(blueprint, true, [
  hubOraclePolicyId,
]);
const settlementQueuePolicyId = mintingPolicyToId(
  settlementQueueValidator.settlementQueueMint,
);

const settlementQueueScript = {
  spending: settlementQueueValidator.settlementQueueSpend.script,
  minting: settlementQueueValidator.settlementQueueMint.script,
  staking: "",
};

// ============================================================================
// Registered Operators
// ============================================================================

const registeredOperatorsValidator = readMidgardValidators(blueprint, true, [
  hubOraclePolicyId,
]);
const registeredOperatorsPolicyId = mintingPolicyToId(
  registeredOperatorsValidator.registeredOperatorsMint,
);

const registeredOperatorsScript = {
  spending: "",
  minting: registeredOperatorsValidator.registeredOperatorsMint.script,
  staking: "",
};

// ============================================================================
// Active Operators
// ============================================================================

const activeOperatorsValidator = readMidgardValidators(blueprint, true, [
  hubOraclePolicyId,
]);
const activeOperatorsPolicyId = mintingPolicyToId(
  activeOperatorsValidator.activeOperatorsMint,
);

const activeOperatorsScript = {
  spending: activeOperatorsValidator.activeOperatorsSpend.script,
  minting: activeOperatorsValidator.activeOperatorsMint.script,
  staking: "",
};

// ============================================================================
// Retired Operators
// ============================================================================

const retiredOperatorsValidator = readMidgardValidators(blueprint, true, [
  hubOraclePolicyId,
]);
const retiredOperatorsPolicyId = mintingPolicyToId(
  retiredOperatorsValidator.retiredOperatorsMint,
);

const retiredOperatorsScript = {
  spending: "",
  minting: retiredOperatorsValidator.retiredOperatorsMint.script,
  staking: "",
};

// ============================================================================
// Scheduler
// ============================================================================

const schedulerValidator = readMidgardValidators(blueprint, true, [
  hubOraclePolicyId,
]);
const schedulerPolicyId = mintingPolicyToId(schedulerValidator.schedulerMint);

const schedulerScript = {
  spending: schedulerValidator.schedulerSpend.script,
  minting: schedulerValidator.schedulerMint.script,
  staking: "",
};

// ============================================================================
// Escape Hatch
// ============================================================================

const escapeHatchValidator = readMidgardValidators(blueprint, true, [
  hubOraclePolicyId,
]);
const escapeHatchPolicyId = mintingPolicyToId(
  escapeHatchValidator.escapeHatchMint,
);

const escapeHatchScript = {
  spending: escapeHatchValidator.escapeHatchSpend.script,
  minting: escapeHatchValidator.escapeHatchMint.script,
  staking: "",
};

// ============================================================================
// Fraud Proof Catalogue
// ============================================================================

const fraudProofCatalogueValidator = readMidgardValidators(blueprint, true, [
  hubOraclePolicyId,
]);
const fraudProofCataloguePolicyId = mintingPolicyToId(
  fraudProofCatalogueValidator.fraudProofCatalogueMint,
);

const fraudProofCatalogueScript = {
  spending: fraudProofCatalogueValidator.fraudProofCatalogueSpend.script,
  minting: fraudProofCatalogueValidator.fraudProofCatalogueMint.script,
  staking: "",
};

// ============================================================================
// Computation Thread
// ============================================================================

const computationThreadValidator = readMidgardValidators(blueprint, true, [
  hubOraclePolicyId,
]);

// ============================================================================
// Exports
// ============================================================================

export {
  // Hub Oracle
  hubOraclePolicyId,
  hubOracleScript,
  hubOracleValidator,

  // State Queue
  stateQueuePolicyId,
  stateQueueScript,
  stateQueueValidator,

  // Settlement Queue
  settlementQueuePolicyId,
  settlementQueueScript,
  settlementQueueValidator,

  // Registered Operators
  registeredOperatorsPolicyId,
  registeredOperatorsScript,
  registeredOperatorsValidator,

  // Active Operators
  activeOperatorsPolicyId,
  activeOperatorsScript,
  activeOperatorsValidator,

  // Retired Operators
  retiredOperatorsPolicyId,
  retiredOperatorsScript,
  retiredOperatorsValidator,

  // Scheduler
  schedulerPolicyId,
  schedulerScript,
  schedulerValidator,

  // Escape Hatch
  escapeHatchPolicyId,
  escapeHatchScript,
  escapeHatchValidator,

  // Fraud Proof Catalogue
  fraudProofCataloguePolicyId,
  fraudProofCatalogueScript,
  fraudProofCatalogueValidator,

  // Fraud Proof
  // fraudProofPolicyId,
  // fraudProofScript,
  // fraudProofValidator,

  // Computation Thread
  // computationThreadPolicyId,
  // computationThreadScript,
  computationThreadValidator,
};
