import { spend } from "./recipes.types.js";

export const scriptIntegrityHashMissingRecipes = {
  fraudProofScriptIntegrityHashMissing: spend(
    (c) => c.fraudProofContracts.scriptIntegrityHashMissing.firstStep,
  ),
  fraudProofScriptIntegrityHashMissingStep02: spend(
    (c) => c.fraudProofContracts.scriptIntegrityHashMissing.steps[1],
  ),
  fraudProofScriptIntegrityHashMissingStep03: spend(
    (c) => c.fraudProofContracts.scriptIntegrityHashMissing.steps[2],
  ),
  fraudProofScriptIntegrityHashMissingScriptGrammar: spend(
    (c) => c.fraudProofContracts.scriptIntegrityHashMissing.scriptGrammar,
  ),
  fraudProofScriptIntegrityHashMissingScriptScan: spend(
    (c) => c.fraudProofContracts.scriptIntegrityHashMissing.scriptScan,
  ),
  fraudProofScriptIntegrityHashMissingRedeemerGrammar: spend(
    (c) => c.fraudProofContracts.scriptIntegrityHashMissing.redeemerGrammar,
  ),
  fraudProofScriptIntegrityHashMissingStep04: spend(
    (c) => c.fraudProofContracts.scriptIntegrityHashMissing.steps[6],
  ),
} as const;
