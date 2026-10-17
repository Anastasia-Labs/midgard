import { defineDeploymentFamily } from "../members.types.js";

export const noReferenceInputMembers = defineDeploymentFamily(
  "noReferenceInput",
  {
    fraudProofNoReferenceInput: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof no-reference-input step-01",
      manifestPosition: 54,
      publicationPosition: 84,
      referenceRolePosition: 161,
      commands: [],
      validatorTitle: "fraud_proofs/no_reference_input/step_01.main.spend",
      familyPaths: [["firstStep"], ["steps", "0"]],
    },
    fraudProofNoReferenceInputStep02: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof no-reference-input step-02",
      manifestPosition: 418,
      publicationPosition: 85,
      referenceRolePosition: 162,
      commands: [],
      validatorTitle: "fraud_proofs/no_reference_input/step_02.main.spend",
      familyPaths: [["steps", "1"]],
    },
    fraudProofNoReferenceInputStep03: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof no-reference-input step-03",
      manifestPosition: 419,
      publicationPosition: 86,
      referenceRolePosition: 163,
      commands: [],
      validatorTitle: "fraud_proofs/no_reference_input/step_03.main.spend",
      familyPaths: [["steps", "2"]],
    },
    fraudProofNoReferenceInputStep04: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof no-reference-input step-04",
      manifestPosition: 420,
      publicationPosition: 87,
      referenceRolePosition: 164,
      commands: [],
      validatorTitle: "fraud_proofs/no_reference_input/step_04.main.spend",
      familyPaths: [["steps", "3"]],
    },
  },
);
