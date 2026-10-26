import { defineDeploymentFamily } from "../members.types.js";

export const validationTraceMachineYieldsMembers = defineDeploymentFamily(
  "validationTraceDispute",
  {
    validationTraceDisputeCekSelectionAuthenticateWithdraw: {
      purpose: "withdraw",
      referenceRole: "V1 validation-trace CEK selection authenticate yield",
      manifestPosition: 563,
      publicationPosition: 564,
      referenceRolePosition: 373,
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/cek_execution_selection_yields.authenticate.withdraw",
      familyPaths: [["yields", "cekSelectionAuthenticate"]],
    },
    validationTraceDisputeCekSelectionSuccessorWithdraw: {
      purpose: "withdraw",
      referenceRole: "V1 validation-trace CEK selection successor yield",
      manifestPosition: 564,
      publicationPosition: 565,
      referenceRolePosition: 374,
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/cek_execution_selection_yields.successor.withdraw",
      familyPaths: [["yields", "cekSelectionSuccessor"]],
    },
    validationTraceDisputePhaseANativeItemNativeWithdraw: {
      purpose: "withdraw",
      referenceRole: "V1 validation-trace phase-A native item native yield",
      manifestPosition: 565,
      publicationPosition: 566,
      referenceRolePosition: 302,
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/phase_a_native_scripts_item_yields_v1.native.withdraw",
      familyPaths: [["yields", "phaseANativeItemNative"]],
    },
    validationTraceDisputePhaseANativeItemForeignWithdraw: {
      purpose: "withdraw",
      referenceRole: "V1 validation-trace phase-A native item foreign yield",
      manifestPosition: 566,
      publicationPosition: 567,
      referenceRolePosition: 303,
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/phase_a_native_scripts_item_yields_v1.foreign.withdraw",
      familyPaths: [["yields", "phaseANativeItemForeign"]],
    },
    validationTraceDisputeValueAndMintAssetFoldWithdraw: {
      purpose: "withdraw",
      referenceRole: "V1 validation-trace value-and-mint asset-fold yield",
      manifestPosition: 567,
      publicationPosition: 568,
      referenceRolePosition: 301,
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/value_and_mint_asset_fold_yield.main.withdraw",
      familyPaths: [["yields", "valueAndMintAssetFold"]],
    },
  },
);
