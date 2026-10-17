import { defineDeploymentFamily } from "../members.types.js";

export const validationTraceMachineYieldsMembers = defineDeploymentFamily(
  "validationTraceDispute",
  {
    validationTraceDisputeCekMaterialTraversal: {
      purpose: "spend",
      referenceRole: "V1 validation-trace CEK material traversal",
      manifestPosition: 492,
      publicationPosition: 493,
      referenceRolePosition: 297,
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/cek_material_traversal_v1.main.spend",
      familyPaths: [["cekMaterialTraversal"]],
    },
    validationTraceDisputeCekMaterialProgramTaskWithdraw: {
      purpose: "withdraw",
      referenceRole: "V1 validation-trace CEK material program task yield",
      manifestPosition: 493,
      publicationPosition: 494,
      referenceRolePosition: 366,
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/cek_material_traversal_yields.program.withdraw",
      familyPaths: [["yields", "cekMaterialProgramTask"]],
    },
    validationTraceDisputeCekMaterialDataTaskWithdraw: {
      purpose: "withdraw",
      referenceRole: "V1 validation-trace CEK material Data task yield",
      manifestPosition: 494,
      publicationPosition: 495,
      referenceRolePosition: 367,
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/cek_material_traversal_yields.data.withdraw",
      familyPaths: [["yields", "cekMaterialDataTask"]],
    },
    validationTraceDisputeCekSelectionAuthenticateWithdraw: {
      purpose: "withdraw",
      referenceRole: "V1 validation-trace CEK selection authenticate yield",
      manifestPosition: 495,
      publicationPosition: 496,
      referenceRolePosition: 368,
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/cek_execution_selection_yields.authenticate.withdraw",
      familyPaths: [["yields", "cekSelectionAuthenticate"]],
    },
    validationTraceDisputeCekSelectionSuccessorWithdraw: {
      purpose: "withdraw",
      referenceRole: "V1 validation-trace CEK selection successor yield",
      manifestPosition: 496,
      publicationPosition: 497,
      referenceRolePosition: 369,
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/cek_execution_selection_yields.successor.withdraw",
      familyPaths: [["yields", "cekSelectionSuccessor"]],
    },
    validationTraceDisputeCekSelectionMaterialProgramWithdraw: {
      purpose: "withdraw",
      referenceRole: "V1 validation-trace CEK selection material program yield",
      manifestPosition: 497,
      publicationPosition: 498,
      referenceRolePosition: 370,
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/cek_execution_selection_yields.material_program.withdraw",
      familyPaths: [["yields", "cekSelectionMaterialProgram"]],
    },
    validationTraceDisputeCekSelectionMaterialDataWithdraw: {
      purpose: "withdraw",
      referenceRole: "V1 validation-trace CEK selection material data yield",
      manifestPosition: 498,
      publicationPosition: 499,
      referenceRolePosition: 371,
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/cek_execution_selection_yields.material_data.withdraw",
      familyPaths: [["yields", "cekSelectionMaterialData"]],
    },
    validationTraceDisputePhaseANativeItemNativeWithdraw: {
      purpose: "withdraw",
      referenceRole: "V1 validation-trace phase-A native item native yield",
      manifestPosition: 499,
      publicationPosition: 500,
      referenceRolePosition: 295,
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/phase_a_native_scripts_item_yields_v1.native.withdraw",
      familyPaths: [["yields", "phaseANativeItemNative"]],
    },
    validationTraceDisputePhaseANativeItemForeignWithdraw: {
      purpose: "withdraw",
      referenceRole: "V1 validation-trace phase-A native item foreign yield",
      manifestPosition: 500,
      publicationPosition: 501,
      referenceRolePosition: 296,
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/phase_a_native_scripts_item_yields_v1.foreign.withdraw",
      familyPaths: [["yields", "phaseANativeItemForeign"]],
    },
    validationTraceDisputeValueAndMintAssetFoldWithdraw: {
      purpose: "withdraw",
      referenceRole: "V1 validation-trace value-and-mint asset-fold yield",
      manifestPosition: 501,
      publicationPosition: 502,
      referenceRolePosition: 294,
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/value_and_mint_asset_fold_yield.main.withdraw",
      familyPaths: [["yields", "valueAndMintAssetFold"]],
    },
  },
);
