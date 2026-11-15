import { defineDeploymentFamily } from "../members.types.js";

export const nativeScriptInvalidMembers = defineDeploymentFamily(
  "nativeScriptInvalid",
  {
    fraudProofNativeScriptInvalid: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof native-script-invalid step-01",
      commands: [],
      validatorTitle: "fraud_proofs/native_script_invalid/step_01.main.spend",
      familyPaths: [["firstStep"], ["steps", "0"]],
    },
    fraudProofNativeScriptInvalidStep02: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof native-script-invalid step-02",
      commands: [],
      validatorTitle: "fraud_proofs/native_script_invalid/step_02.main.spend",
      familyPaths: [["steps", "1"]],
    },
    fraudProofNativeScriptInvalidStep03: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof native-script-invalid step-03",
      commands: [],
      validatorTitle: "fraud_proofs/native_script_invalid/step_03.main.spend",
      familyPaths: [["steps", "2"]],
    },
    fraudProofNativeScriptInvalidStep04: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof native-script-invalid step-04",
      commands: [],
      validatorTitle: "fraud_proofs/native_script_invalid/step_04.main.spend",
      familyPaths: [["steps", "3"]],
    },
    fraudProofNativeScriptInvalidStep05: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof native-script-invalid step-05",
      commands: [],
      validatorTitle: "fraud_proofs/native_script_invalid/step_05.main.spend",
      familyPaths: [["steps", "4"]],
    },
  },
);
