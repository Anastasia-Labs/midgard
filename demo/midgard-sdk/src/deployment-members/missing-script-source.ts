import { spend } from "./recipes.types.js";

export const missingScriptSourceRecipes = {
  fraudProofMissingScriptSource: spend(
    (c) => c.fraudProofContracts.missingScriptSource.firstStep,
  ),
  fraudProofMissingScriptSourceStep02: spend(
    (c) => c.fraudProofContracts.missingScriptSource.steps[1],
  ),
  fraudProofMissingScriptSourceStep03: spend(
    (c) => c.fraudProofContracts.missingScriptSource.steps[2],
  ),
  fraudProofMissingScriptSourceStep04: spend(
    (c) => c.fraudProofContracts.missingScriptSource.steps[3],
  ),
  fraudProofMissingScriptSourceStep05: spend(
    (c) => c.fraudProofContracts.missingScriptSource.steps[4],
  ),
  fraudProofMissingScriptSourceStep06: spend(
    (c) => c.fraudProofContracts.missingScriptSource.steps[5],
  ),
} as const;
