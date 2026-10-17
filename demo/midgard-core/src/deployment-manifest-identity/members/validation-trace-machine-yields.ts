import { defineDeploymentFamily } from "../members.types.js";

export const validationTraceMachineYieldsMembers = defineDeploymentFamily(
  "validationTraceDispute",
  {
    validationTraceDisputeCekSelectionAuthenticateWithdraw: {
      purpose: "withdraw",
      referenceRole: "V1 validation-trace CEK selection authenticate yield",
      manifestPosition: 493,
      publicationPosition: 494,
      referenceRolePosition: 366,
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/cek_execution_selection_yields.authenticate.withdraw",
      familyPaths: [["yields", "cekSelectionAuthenticate"]],
    },
    validationTraceDisputeCekSelectionSuccessorWithdraw: {
      purpose: "withdraw",
      referenceRole: "V1 validation-trace CEK selection successor yield",
      manifestPosition: 494,
      publicationPosition: 495,
      referenceRolePosition: 367,
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/cek_execution_selection_yields.successor.withdraw",
      familyPaths: [["yields", "cekSelectionSuccessor"]],
    },
    validationTraceDisputePhaseANativeItemNativeWithdraw: {
      purpose: "withdraw",
      referenceRole: "V1 validation-trace phase-A native item native yield",
      manifestPosition: 495,
      publicationPosition: 496,
      referenceRolePosition: 296,
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/phase_a_native_scripts_item_yields_v1.native.withdraw",
      familyPaths: [["yields", "phaseANativeItemNative"]],
    },
    validationTraceDisputePhaseANativeItemForeignWithdraw: {
      purpose: "withdraw",
      referenceRole: "V1 validation-trace phase-A native item foreign yield",
      manifestPosition: 496,
      publicationPosition: 497,
      referenceRolePosition: 297,
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/phase_a_native_scripts_item_yields_v1.foreign.withdraw",
      familyPaths: [["yields", "phaseANativeItemForeign"]],
    },
    validationTraceDisputeValueAndMintAssetFoldWithdraw: {
      purpose: "withdraw",
      referenceRole: "V1 validation-trace value-and-mint asset-fold yield",
      manifestPosition: 497,
      publicationPosition: 498,
      referenceRolePosition: 295,
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/value_and_mint_asset_fold_yield.main.withdraw",
      familyPaths: [["yields", "valueAndMintAssetFold"]],
    },
  },
);
