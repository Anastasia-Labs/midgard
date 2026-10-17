import { spend } from "./recipes.types.js";

export const unusedScriptWitnessRecipes = {
  fraudProofUnusedScriptWitness: spend(
    (c) => c.fraudProofContracts.unusedScriptWitness.firstStep,
  ),
  fraudProofUnusedScriptWitnessStep02: spend(
    (c) => c.fraudProofContracts.unusedScriptWitness.steps[1],
  ),
  fraudProofUnusedScriptWitnessStep03: spend(
    (c) => c.fraudProofContracts.unusedScriptWitness.steps[2],
  ),
  fraudProofUnusedScriptWitnessStep04: spend(
    (c) => c.fraudProofContracts.unusedScriptWitness.steps[3],
  ),
  fraudProofUnusedScriptWitnessStep05: spend(
    (c) => c.fraudProofContracts.unusedScriptWitness.steps[4],
  ),
  fraudProofUnusedScriptWitnessStep06: spend(
    (c) => c.fraudProofContracts.unusedScriptWitness.steps[5],
  ),
} as const;
