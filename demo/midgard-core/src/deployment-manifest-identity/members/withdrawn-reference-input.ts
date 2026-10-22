import { defineDeploymentFamily } from "../members.types.js";

export const withdrawnReferenceInputMembers = defineDeploymentFamily(
  "withdrawnReferenceInput",
  {
    fraudProofWithdrawnReferenceInput: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof withdrawn-reference-input step-01",
      manifestPosition: 212,
      publicationPosition: 118,
      referenceRolePosition: 83,
      commands: [],
      validatorTitle:
        "fraud_proofs/withdrawn_reference_input/step_01.main.spend",
      familyPaths: [["firstStep"], ["steps", "0"]],
    },
    fraudProofWithdrawnReferenceInputStep02: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof withdrawn-reference-input step-02",
      manifestPosition: 213,
      publicationPosition: 119,
      referenceRolePosition: 84,
      commands: [],
      validatorTitle:
        "fraud_proofs/withdrawn_reference_input/step_02.main.spend",
      familyPaths: [["steps", "1"]],
    },
    fraudProofWithdrawnReferenceInputStep03: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof withdrawn-reference-input step-03",
      manifestPosition: 214,
      publicationPosition: 120,
      referenceRolePosition: 85,
      commands: [],
      validatorTitle:
        "fraud_proofs/withdrawn_reference_input/step_03.main.spend",
      familyPaths: [["steps", "2"]],
    },
  },
);
