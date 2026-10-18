import { defineDeploymentFamily } from "../members.types.js";

export const noReferenceInputMembers = defineDeploymentFamily(
  "noReferenceInput",
  {
    fraudProofNoReferenceInput: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof no-reference-input step-01",
      manifestPosition: 53,
      publicationPosition: 89,
      referenceRolePosition: 166,
      commands: [],
      validatorTitle: "fraud_proofs/no_reference_input/step_01.main.spend",
      familyPaths: [["firstStep"], ["steps", "0"]],
    },
    fraudProofNoReferenceInputStep02: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof no-reference-input step-02",
      manifestPosition: 455,
      publicationPosition: 90,
      referenceRolePosition: 167,
      commands: [],
      validatorTitle: "fraud_proofs/no_reference_input/step_02.main.spend",
      familyPaths: [["steps", "1"]],
    },
    fraudProofNoReferenceInputStep03: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof no-reference-input step-03",
      manifestPosition: 456,
      publicationPosition: 91,
      referenceRolePosition: 168,
      commands: [],
      validatorTitle: "fraud_proofs/no_reference_input/step_03.main.spend",
      familyPaths: [["steps", "2"]],
    },
    fraudProofNoReferenceInputStep04: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof no-reference-input step-04",
      manifestPosition: 457,
      publicationPosition: 92,
      referenceRolePosition: 169,
      commands: [],
      validatorTitle: "fraud_proofs/no_reference_input/step_04.main.spend",
      familyPaths: [["steps", "3"]],
    },
  },
);
