import { defineDeploymentFamily } from "../members.types.js";

export const fieldPreimageLengthMismatchMembers = defineDeploymentFamily(
  "fieldPreimageLengthMismatch",
  {
    fraudProofFieldPreimageLengthMismatch: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof field-preimage-length-mismatch step-01",
      commands: [],
      validatorTitle:
        "fraud_proofs/field_preimage_length_mismatch/step_01.main.spend",
      familyPaths: [["firstStep"], ["steps", "0"]],
    },
    fraudProofFieldPreimageLengthMismatchStep02Accepted: {
      purpose: "spend",
      referenceRole:
        "V1 fraud-proof field-preimage-length-mismatch step-02 accepted",
      commands: [],
      validatorTitle:
        "fraud_proofs/field_preimage_length_mismatch/step_02_accepted.main.spend",
      familyPaths: [["acceptedStep02"], ["steps", "1"]],
    },
    fraudProofFieldPreimageLengthMismatchStep02Forced: {
      purpose: "spend",
      referenceRole:
        "V1 fraud-proof field-preimage-length-mismatch step-02 forced",
      commands: [],
      validatorTitle:
        "fraud_proofs/field_preimage_length_mismatch/step_02_forced.main.spend",
      familyPaths: [["forcedStep02"], ["steps", "2"]],
    },
    fraudProofFieldPreimageLengthMismatchStep03: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof field-preimage-length-mismatch step-03",
      commands: [],
      validatorTitle:
        "fraud_proofs/field_preimage_length_mismatch/step_03.main.spend",
      familyPaths: [["steps", "3"]],
    },
  },
);
