import { defineDeploymentFamily } from "../members.types.js";

export const validationTraceMachineYieldsMembers = defineDeploymentFamily(
  "validationTraceDispute",
  {
    validationTraceDisputeCekMaterialTraversal: {
      purpose: "spend",
      referenceRole: "V1 validation-trace CEK material traversal",
      manifestPosition: 498,
      publicationPosition: 499,
      referenceRolePosition: 303,
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/cek_material_traversal_v1.main.spend",
      familyPaths: [["cekMaterialTraversal"]],
    },
    validationTraceDisputeCekMaterialProgramTaskWithdraw: {
      purpose: "withdraw",
      referenceRole: "V1 validation-trace CEK material program task yield",
      manifestPosition: 499,
      publicationPosition: 500,
      referenceRolePosition: 372,
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/cek_material_traversal_yields.program.withdraw",
      familyPaths: [["yields", "cekMaterialProgramTask"]],
    },
    validationTraceDisputeCekMaterialDataTaskWithdraw: {
      purpose: "withdraw",
      referenceRole: "V1 validation-trace CEK material Data task yield",
      manifestPosition: 500,
      publicationPosition: 501,
      referenceRolePosition: 373,
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/cek_material_traversal_yields.data.withdraw",
      familyPaths: [["yields", "cekMaterialDataTask"]],
    },
    validationTraceDisputeCekSelectionAuthenticateWithdraw: {
      purpose: "withdraw",
      referenceRole: "V1 validation-trace CEK selection authenticate yield",
      manifestPosition: 501,
      publicationPosition: 502,
      referenceRolePosition: 374,
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/cek_execution_selection_yields.authenticate.withdraw",
      familyPaths: [["yields", "cekSelectionAuthenticate"]],
    },
    validationTraceDisputeCekSelectionSuccessorWithdraw: {
      purpose: "withdraw",
      referenceRole: "V1 validation-trace CEK selection successor yield",
      manifestPosition: 502,
      publicationPosition: 503,
      referenceRolePosition: 375,
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/cek_execution_selection_yields.successor.withdraw",
      familyPaths: [["yields", "cekSelectionSuccessor"]],
    },
    validationTraceDisputeCekSelectionMaterialProgramWithdraw: {
      purpose: "withdraw",
      referenceRole: "V1 validation-trace CEK selection material program yield",
      manifestPosition: 503,
      publicationPosition: 504,
      referenceRolePosition: 376,
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/cek_execution_selection_yields.material_program.withdraw",
      familyPaths: [["yields", "cekSelectionMaterialProgram"]],
    },
    validationTraceDisputeCekSelectionMaterialDataWithdraw: {
      purpose: "withdraw",
      referenceRole: "V1 validation-trace CEK selection material data yield",
      manifestPosition: 504,
      publicationPosition: 505,
      referenceRolePosition: 377,
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/cek_execution_selection_yields.material_data.withdraw",
      familyPaths: [["yields", "cekSelectionMaterialData"]],
    },
    validationTraceDisputePhaseANativeItemNativeWithdraw: {
      purpose: "withdraw",
      referenceRole: "V1 validation-trace phase-A native item native yield",
      manifestPosition: 505,
      publicationPosition: 506,
      referenceRolePosition: 301,
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/phase_a_native_scripts_item_yields_v1.native.withdraw",
      familyPaths: [["yields", "phaseANativeItemNative"]],
    },
    validationTraceDisputePhaseANativeItemForeignWithdraw: {
      purpose: "withdraw",
      referenceRole: "V1 validation-trace phase-A native item foreign yield",
      manifestPosition: 506,
      publicationPosition: 507,
      referenceRolePosition: 302,
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/phase_a_native_scripts_item_yields_v1.foreign.withdraw",
      familyPaths: [["yields", "phaseANativeItemForeign"]],
    },
    validationTraceDisputeValueAndMintAssetFoldWithdraw: {
      purpose: "withdraw",
      referenceRole: "V1 validation-trace value-and-mint asset-fold yield",
      manifestPosition: 507,
      publicationPosition: 508,
      referenceRolePosition: 300,
      commands: [],
      validatorTitle:
        "fraud_proofs/validation_trace/value_and_mint_asset_fold_yield.main.withdraw",
      familyPaths: [["yields", "valueAndMintAssetFold"]],
    },
  },
);
