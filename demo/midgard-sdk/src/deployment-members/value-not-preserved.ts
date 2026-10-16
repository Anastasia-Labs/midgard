import { spend } from "./recipes.types.js";

export const valueNotPreservedRecipes = {
  fraudProofValueNotPreserved: spend(
    (c) => c.fraudProofContracts.valueNotPreserved.firstStep,
  ),
  fraudProofValueNotPreservedStep02: spend(
    (c) => c.fraudProofContracts.valueNotPreserved.steps[1],
  ),
  fraudProofValueNotPreservedStep03: spend(
    (c) => c.fraudProofContracts.valueNotPreserved.steps[2],
  ),
  fraudProofValueNotPreservedStep04: spend(
    (c) => c.fraudProofContracts.valueNotPreserved.steps[3],
  ),
  fraudProofValueNotPreservedUnionAcceptedSource: spend(
    (c) => c.fraudProofContracts.valueNotPreserved.unionAcceptedSource,
  ),
  fraudProofValueNotPreservedUnionForcedSource: spend(
    (c) => c.fraudProofContracts.valueNotPreserved.unionForcedSource,
  ),
  fraudProofValueNotPreservedUnionEvent: spend(
    (c) => c.fraudProofContracts.valueNotPreserved.unionEvent,
  ),
  fraudProofValueNotPreservedUnionPreState: spend(
    (c) => c.fraudProofContracts.valueNotPreserved.unionPreState,
  ),
  fraudProofValueNotPreservedUnionInputs: spend(
    (c) => c.fraudProofContracts.valueNotPreserved.unionInputs,
  ),
  fraudProofValueNotPreservedUnionInputValue: spend(
    (c) => c.fraudProofContracts.valueNotPreserved.unionInputValue,
  ),
  fraudProofValueNotPreservedUnionAssets: spend(
    (c) => c.fraudProofContracts.valueNotPreserved.unionAssets,
  ),
  fraudProofValueNotPreservedUnionFieldGrammar: spend(
    (c) => c.fraudProofContracts.valueNotPreserved.unionFieldGrammar,
  ),
  fraudProofValueNotPreservedUnionOutputs: spend(
    (c) => c.fraudProofContracts.valueNotPreserved.unionOutputs,
  ),
  fraudProofValueNotPreservedUnionOutputScan: spend(
    (c) => c.fraudProofContracts.valueNotPreserved.unionOutputScan,
  ),
  fraudProofValueNotPreservedUnionMint: spend(
    (c) => c.fraudProofContracts.valueNotPreserved.unionMint,
  ),
  fraudProofValueNotPreservedUnionUpdate: spend(
    (c) => c.fraudProofContracts.valueNotPreserved.unionUpdate,
  ),
  fraudProofValueNotPreservedUnionTerminal: spend(
    (c) => c.fraudProofContracts.valueNotPreserved.unionTerminal,
  ),
} as const;
