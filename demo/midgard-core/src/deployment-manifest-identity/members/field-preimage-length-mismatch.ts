import { defineDeploymentFamily } from "../members.types.js";

export const fieldPreimageLengthMismatchMembers = defineDeploymentFamily(
  "fieldPreimageLengthMismatch",
  {
    fraudProofFieldPreimageLengthMismatch: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof field-preimage-length-mismatch step-01",
        token: "V1FpFieldLenS01",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/field_preimage_length_mismatch/step_01.main.spend",
      familyPaths: [["firstStep"], ["steps", "0"]],
    },
    fraudProofFieldPreimageLengthMismatchStep02Accepted: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof field-preimage-length-mismatch step-02 accepted",
        token: "V1FpFieldLenS02Accepted",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/field_preimage_length_mismatch/step_02_accepted.main.spend",
      familyPaths: [["acceptedStep02"], ["steps", "1"]],
    },
    fraudProofFieldPreimageLengthMismatchStep02Forced: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof field-preimage-length-mismatch step-02 forced",
        token: "V1FpFieldLenS02Forced",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/field_preimage_length_mismatch/step_02_forced.main.spend",
      familyPaths: [["forcedStep02"], ["steps", "2"]],
    },
    fraudProofFieldPreimageLengthMismatchStep03: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof field-preimage-length-mismatch step-03",
        token: "V1FpFieldLenS03",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/field_preimage_length_mismatch/step_03.main.spend",
      familyPaths: [["steps", "3"]],
    },
  },
);
