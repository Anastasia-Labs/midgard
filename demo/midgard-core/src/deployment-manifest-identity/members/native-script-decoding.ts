import { defineDeploymentFamily } from "../members.types.js";

export const nativeScriptDecodingMembers = defineDeploymentFamily(
  "nativeScriptDecoding",
  {
    fraudProofNativeScriptDecoding: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof native-script-decoding step-01",
      manifestPosition: 195,
      publicationPosition: 101,
      referenceRolePosition: 63,
      commands: [],
      validatorTitle: "fraud_proofs/native_script_decoding/step_01.main.spend",
      familyPaths: [["firstStep"], ["steps", "0"]],
    },
    fraudProofNativeScriptDecodingStep02: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof native-script-decoding step-02",
      manifestPosition: 196,
      publicationPosition: 102,
      referenceRolePosition: 64,
      commands: [],
      validatorTitle: "fraud_proofs/native_script_decoding/step_02.main.spend",
      familyPaths: [["steps", "1"]],
    },
    fraudProofNativeScriptDecodingStep03OpenSubject: {
      purpose: "spend",
      referenceRole:
        "V1 fraud-proof native-script-decoding step-03 open-subject",
      manifestPosition: 197,
      publicationPosition: 103,
      referenceRolePosition: 65,
      commands: [],
      validatorTitle:
        "fraud_proofs/native_script_decoding/step_03_open_subject.main.spend",
      familyPaths: [["steps", "2"]],
    },
    fraudProofNativeScriptDecodingStep03BindDescriptor: {
      purpose: "spend",
      referenceRole:
        "V1 fraud-proof native-script-decoding step-03 bind-descriptor",
      manifestPosition: 198,
      publicationPosition: 104,
      referenceRolePosition: 66,
      commands: [],
      validatorTitle:
        "fraud_proofs/native_script_decoding/step_03_bind_descriptor.main.spend",
      familyPaths: [["steps", "3"]],
    },
    fraudProofNativeScriptDecodingStep03AdvanceOrClose: {
      purpose: "spend",
      referenceRole:
        "V1 fraud-proof native-script-decoding step-03 advance-or-close",
      manifestPosition: 199,
      publicationPosition: 105,
      referenceRolePosition: 67,
      commands: [],
      validatorTitle:
        "fraud_proofs/native_script_decoding/step_03_advance_or_close.main.spend",
      familyPaths: [["steps", "4"]],
    },
    fraudProofNativeScriptDecodingStep04: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof native-script-decoding step-04",
      manifestPosition: 200,
      publicationPosition: 106,
      referenceRolePosition: 68,
      commands: [],
      validatorTitle: "fraud_proofs/native_script_decoding/step_04.main.spend",
      familyPaths: [["steps", "5"]],
    },
  },
);
