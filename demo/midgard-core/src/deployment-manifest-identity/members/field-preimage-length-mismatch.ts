import { defineDeploymentFamily } from "../members.types.js";

export const fieldPreimageLengthMismatchMembers = defineDeploymentFamily(
  "fieldPreimageLengthMismatch",
  {
    fraudProofFieldPreimageLengthMismatch: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof field-preimage-length-mismatch step-01",
      manifestPosition: 258,
      publicationPosition: 164,
      referenceRolePosition: 401,
      commands: [],
      validatorTitle:
        "fraud_proofs/field_preimage_length_mismatch/step_01.main.spend",
      familyPaths: [["firstStep"], ["steps", "0"]],
    },
    fraudProofFieldPreimageLengthMismatchStep02Accepted: {
      purpose: "spend",
      referenceRole:
        "V1 fraud-proof field-preimage-length-mismatch step-02 accepted",
      manifestPosition: 259,
      publicationPosition: 165,
      referenceRolePosition: 402,
      commands: [],
      validatorTitle:
        "fraud_proofs/field_preimage_length_mismatch/step_02_accepted.main.spend",
      familyPaths: [["acceptedStep02"], ["steps", "1"]],
    },
    fraudProofFieldPreimageLengthMismatchStep02Forced: {
      purpose: "spend",
      referenceRole:
        "V1 fraud-proof field-preimage-length-mismatch step-02 forced",
      manifestPosition: 260,
      publicationPosition: 166,
      referenceRolePosition: 403,
      commands: [],
      validatorTitle:
        "fraud_proofs/field_preimage_length_mismatch/step_02_forced.main.spend",
      familyPaths: [["forcedStep02"], ["steps", "2"]],
    },
    fraudProofFieldPreimageLengthMismatchStep03: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof field-preimage-length-mismatch step-03",
      manifestPosition: 261,
      publicationPosition: 167,
      referenceRolePosition: 404,
      commands: [],
      validatorTitle:
        "fraud_proofs/field_preimage_length_mismatch/step_03.main.spend",
      familyPaths: [["steps", "3"]],
    },
  },
);
