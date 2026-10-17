import { spend } from "./recipes.types.js";

export const executionNativeScriptInvalidRecipes = {
  fraudProofExecutionNativeScriptInvalid: spend(
    (c) => c.fraudProofContracts.executionNativeScriptInvalid.firstStep,
  ),
  fraudProofExecutionNativeScriptInvalidStep02: spend(
    (c) => c.fraudProofContracts.executionNativeScriptInvalid.steps[1],
  ),
  fraudProofExecutionNativeScriptInvalidStep03: spend(
    (c) => c.fraudProofContracts.executionNativeScriptInvalid.steps[2],
  ),
  fraudProofExecutionNativeScriptInvalidStep04: spend(
    (c) => c.fraudProofContracts.executionNativeScriptInvalid.steps[3],
  ),
  fraudProofExecutionNativeScriptInvalidStep05: spend(
    (c) => c.fraudProofContracts.executionNativeScriptInvalid.steps[4],
  ),
  fraudProofExecutionNativeScriptInvalidStep06: spend(
    (c) => c.fraudProofContracts.executionNativeScriptInvalid.steps[5],
  ),
  fraudProofExecutionNativeScriptInvalidAcceptedReconstructionInit: spend(
    (c) => c.fraudProofContracts.executionNativeScriptInvalid.steps[6],
  ),
  fraudProofExecutionNativeScriptInvalidAcceptedSpendPrefix: spend(
    (c) => c.fraudProofContracts.executionNativeScriptInvalid.steps[7],
  ),
  fraudProofExecutionNativeScriptInvalidAcceptedMintPrefix: spend(
    (c) => c.fraudProofContracts.executionNativeScriptInvalid.steps[8],
  ),
  fraudProofExecutionNativeScriptInvalidAcceptedObserverPrefix: spend(
    (c) => c.fraudProofContracts.executionNativeScriptInvalid.steps[9],
  ),
  fraudProofExecutionNativeScriptInvalidAcceptedReceivePrefix: spend(
    (c) => c.fraudProofContracts.executionNativeScriptInvalid.steps[10],
  ),
  fraudProofExecutionNativeScriptInvalidAcceptedInlineSource: spend(
    (c) => c.fraudProofContracts.executionNativeScriptInvalid.steps[11],
  ),
  fraudProofExecutionNativeScriptInvalidAcceptedReferenceSource: spend(
    (c) => c.fraudProofContracts.executionNativeScriptInvalid.steps[12],
  ),
} as const;
