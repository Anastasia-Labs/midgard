import { defineDeploymentFamily } from "../members.types.js";

export const withdrawnReferenceInputMembers = defineDeploymentFamily(
  "withdrawnReferenceInput",
  {
    fraudProofWithdrawnReferenceInput: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof withdrawn-reference-input step-01",
      manifestPosition: 205,
      publicationPosition: 111,
      referenceRolePosition: 76,
      commands: [],
      validatorTitle:
        "fraud_proofs/withdrawn_reference_input/step_01.main.spend",
      familyPaths: [["firstStep"], ["steps", "0"]],
    },
    fraudProofWithdrawnReferenceInputStep02: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof withdrawn-reference-input step-02",
      manifestPosition: 206,
      publicationPosition: 112,
      referenceRolePosition: 77,
      commands: [],
      validatorTitle:
        "fraud_proofs/withdrawn_reference_input/step_02.main.spend",
      familyPaths: [["steps", "1"]],
    },
    fraudProofWithdrawnReferenceInputStep03: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof withdrawn-reference-input step-03",
      manifestPosition: 207,
      publicationPosition: 113,
      referenceRolePosition: 78,
      commands: [],
      validatorTitle:
        "fraud_proofs/withdrawn_reference_input/step_03.main.spend",
      familyPaths: [["steps", "2"]],
    },
  },
);
