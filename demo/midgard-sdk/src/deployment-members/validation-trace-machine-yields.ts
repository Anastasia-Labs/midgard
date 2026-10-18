import { withdraw } from "./recipes.types.js";

export const validationTraceMachineYieldsRecipes = {
  validationTraceDisputeCekSelectionAuthenticateWithdraw: withdraw(
    (c) =>
      c.fraudProofContracts.validationTraceDispute.yields
        .cekSelectionAuthenticate,
  ),
  validationTraceDisputeCekSelectionSuccessorWithdraw: withdraw(
    (c) =>
      c.fraudProofContracts.validationTraceDispute.yields.cekSelectionSuccessor,
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
