import { defineDeploymentFamily } from "../members.types.js";

export const nativeScriptDecodingMembers = defineDeploymentFamily(
  "nativeScriptDecoding",
  {
    fraudProofNativeScriptDecoding: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof native-script-decoding step-01",
      manifestPosition: 201,
      publicationPosition: 107,
      referenceRolePosition: 69,
      commands: [],
      validatorTitle: "fraud_proofs/native_script_decoding/step_01.main.spend",
      familyPaths: [["firstStep"], ["steps", "0"]],
    },
    fraudProofNativeScriptDecodingStep02: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof native-script-decoding step-02",
      manifestPosition: 202,
      publicationPosition: 108,
      referenceRolePosition: 70,
      commands: [],
      validatorTitle: "fraud_proofs/native_script_decoding/step_02.main.spend",
      familyPaths: [["steps", "1"]],
    },
    fraudProofNativeScriptDecodingStep03OpenSubject: {
      purpose: "spend",
      referenceRole:
        "V1 fraud-proof native-script-decoding step-03 open-subject",
      manifestPosition: 203,
      publicationPosition: 109,
      referenceRolePosition: 71,
      commands: [],
      validatorTitle:
        "fraud_proofs/native_script_decoding/step_03_open_subject.main.spend",
      familyPaths: [["steps", "2"]],
    },
    fraudProofNativeScriptDecodingStep03BindDescriptor: {
      purpose: "spend",
      referenceRole:
        "V1 fraud-proof native-script-decoding step-03 bind-descriptor",
      manifestPosition: 204,
      publicationPosition: 110,
      referenceRolePosition: 72,
      commands: [],
      validatorTitle:
        "fraud_proofs/native_script_decoding/step_03_bind_descriptor.main.spend",
      familyPaths: [["steps", "3"]],
    },
    fraudProofNativeScriptDecodingStep03AdvanceOrClose: {
      purpose: "spend",
      referenceRole:
        "V1 fraud-proof native-script-decoding step-03 advance-or-close",
      manifestPosition: 205,
      publicationPosition: 111,
      referenceRolePosition: 73,
      commands: [],
      validatorTitle:
        "fraud_proofs/native_script_decoding/step_03_advance_or_close.main.spend",
      familyPaths: [["steps", "4"]],
    },
    fraudProofNativeScriptDecodingStep04: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof native-script-decoding step-04",
      manifestPosition: 206,
      publicationPosition: 112,
      referenceRolePosition: 74,
      commands: [],
      validatorTitle: "fraud_proofs/native_script_decoding/step_04.main.spend",
      familyPaths: [["steps", "5"]],
    },
  },
);
