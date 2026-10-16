import { spend, withdraw } from "./recipes.types.js";

export const validationTraceMachineYieldsRecipes = {
  validationTraceDisputeCekMaterialTraversal: spend(
    (c) => c.fraudProofContracts.validationTraceDispute.cekMaterialTraversal,
  ),
  validationTraceDisputeCekMaterialProgramTaskWithdraw: withdraw(
    (c) =>
      c.fraudProofContracts.validationTraceDispute.yields
        .cekMaterialProgramTask,
  ),
  validationTraceDisputeCekMaterialDataTaskWithdraw: withdraw(
    (c) =>
      c.fraudProofContracts.validationTraceDispute.yields.cekMaterialDataTask,
  ),
  validationTraceDisputeCekSelectionAuthenticateWithdraw: withdraw(
    (c) =>
      c.fraudProofContracts.validationTraceDispute.yields
        .cekSelectionAuthenticate,
  ),
  validationTraceDisputeCekSelectionSuccessorWithdraw: withdraw(
    (c) =>
      c.fraudProofContracts.validationTraceDispute.yields.cekSelectionSuccessor,
  ),
  validationTraceDisputeCekSelectionMaterialProgramWithdraw: withdraw(
    (c) =>
      c.fraudProofContracts.validationTraceDispute.yields
        .cekSelectionMaterialProgram,
  ),
  validationTraceDisputeCekSelectionMaterialDataWithdraw: withdraw(
    (c) =>
      c.fraudProofContracts.validationTraceDispute.yields
        .cekSelectionMaterialData,
  ),
  validationTraceDisputePhaseANativeItemNativeWithdraw: withdraw(
    (c) =>
      c.fraudProofContracts.validationTraceDispute.yields
        .phaseANativeItemNative,
  ),
  validationTraceDisputePhaseANativeItemForeignWithdraw: withdraw(
    (c) =>
      c.fraudProofContracts.validationTraceDispute.yields
        .phaseANativeItemForeign,
  ),
  validationTraceDisputeValueAndMintAssetFoldWithdraw: withdraw(
    (c) =>
      c.fraudProofContracts.validationTraceDispute.yields.valueAndMintAssetFold,
  ),
} as const;
