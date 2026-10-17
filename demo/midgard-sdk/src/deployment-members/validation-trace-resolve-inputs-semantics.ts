import { spend } from "./recipes.types.js";

export const validationTraceResolveInputsSemanticsRecipes = {
  validationTraceDisputeResolveInputsInitialSemantic: spend(
    (c) => c.fraudProofContracts.validationTraceDispute.semanticResolvers[26],
  ),
  validationTraceDisputeResolveInputsFinishSemantic: spend(
    (c) => c.fraudProofContracts.validationTraceDispute.semanticResolvers[27],
  ),
  validationTraceDisputeResolveInputsMembershipBeginSemantic: spend(
    (c) => c.fraudProofContracts.validationTraceDispute.semanticResolvers[28],
  ),
  validationTraceDisputeResolveInputsMembershipStepSemantic: spend(
    (c) => c.fraudProofContracts.validationTraceDispute.semanticResolvers[29],
  ),
  validationTraceDisputeResolveInputsMembershipFinalizeSemantic: spend(
    (c) => c.fraudProofContracts.validationTraceDispute.semanticResolvers[30],
  ),
  validationTraceDisputeResolveInputsNonMembershipSemantic: spend(
    (c) => c.fraudProofContracts.validationTraceDispute.semanticResolvers[31],
  ),
} as const;
