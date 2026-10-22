import { defineDeploymentFamily } from "../members.types.js";

export const validationTraceMachineYieldsMembers = defineDeploymentFamily(
  "validationTraceDispute",
  {
    validationTraceDisputeCekSelectionAuthenticateWithdraw: {
      purpose: "withdraw",
      referenceRole: "V1 validation-trace CEK selection authenticate yield",
      manifestPosition: 532,
      publicationPosition: 533,
      referenceRolePosition: 373,
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/cek_execution_selection_yields.authenticate.withdraw",
      familyPaths: [["yields", "cekSelectionAuthenticate"]],
    },
    validationTraceDisputeCekSelectionSuccessorWithdraw: {
      purpose: "withdraw",
      referenceRole: "V1 validation-trace CEK selection successor yield",
      manifestPosition: 533,
      publicationPosition: 534,
      referenceRolePosition: 374,
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/cek_execution_selection_yields.successor.withdraw",
      familyPaths: [["yields", "cekSelectionSuccessor"]],
    },
    validationTraceDisputePhaseANativeItemNativeWithdraw: {
      purpose: "withdraw",
      referenceRole: "V1 validation-trace phase-A native item native yield",
      manifestPosition: 534,
      publicationPosition: 535,
      referenceRolePosition: 302,
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/phase_a_native_scripts_item_yields_v1.native.withdraw",
      familyPaths: [["yields", "phaseANativeItemNative"]],
    },
    validationTraceDisputePhaseANativeItemForeignWithdraw: {
      purpose: "withdraw",
      referenceRole: "V1 validation-trace phase-A native item foreign yield",
      manifestPosition: 535,
      publicationPosition: 536,
      referenceRolePosition: 303,
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/phase_a_native_scripts_item_yields_v1.foreign.withdraw",
      familyPaths: [["yields", "phaseANativeItemForeign"]],
    },
    validationTraceDisputeValueAndMintAssetFoldWithdraw: {
      purpose: "withdraw",
      referenceRole: "V1 validation-trace value-and-mint asset-fold yield",
      manifestPosition: 536,
      publicationPosition: 537,
      referenceRolePosition: 301,
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/value_and_mint_asset_fold_yield.main.withdraw",
      familyPaths: [["yields", "valueAndMintAssetFold"]],
    },
  },
);
