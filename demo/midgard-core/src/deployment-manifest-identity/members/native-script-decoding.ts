import { defineDeploymentFamily } from "../members.types.js";

export const nativeScriptDecodingMembers = defineDeploymentFamily(
  "nativeScriptDecoding",
  {
    fraudProofNativeScriptDecoding: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof native-script-decoding step-01",
        token: "V1FpNativeScriptDecodingS01",
      },
      commands: [],
      validatorTitle: "fraud_proofs/native_script_decoding/step_01.main.spend",
      familyPaths: [["firstStep"], ["steps", "0"]],
    },
    fraudProofNativeScriptDecodingStep02: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof native-script-decoding step-02",
        token: "V1FpNativeScriptDecodingS02",
      },
      commands: [],
      validatorTitle: "fraud_proofs/native_script_decoding/step_02.main.spend",
      familyPaths: [["steps", "1"]],
    },
    fraudProofNativeScriptDecodingStep03OpenSubject: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof native-script-decoding step-03 open-subject",
        token: "V1FpNativeScriptDecodingS03Open",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/native_script_decoding/step_03_open_subject.main.spend",
      familyPaths: [["steps", "2"]],
    },
    fraudProofNativeScriptDecodingStep03BindDescriptor: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof native-script-decoding step-03 bind-descriptor",
        token: "V1FpNativeScriptDecodingS03Bind",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/native_script_decoding/step_03_bind_descriptor.main.spend",
      familyPaths: [["steps", "3"]],
    },
    fraudProofNativeScriptDecodingStep03AdvanceOrClose: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof native-script-decoding step-03 advance-or-close",
        token: "V1FpNativeScriptDecodingS03Scan",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/native_script_decoding/step_03_advance_or_close.main.spend",
      familyPaths: [["steps", "4"]],
    },
    fraudProofNativeScriptDecodingStep04: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof native-script-decoding step-04",
        token: "V1FpNativeScriptDecodingS04",
      },
      commands: [],
      validatorTitle: "fraud_proofs/native_script_decoding/step_04.main.spend",
      familyPaths: [["steps", "5"]],
    },
  },
);
