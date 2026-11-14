import { defineDeploymentFamily } from "../members.types.js";

export const committedFieldShapeMembers = defineDeploymentFamily(
  "committedFieldShape",
  {
    fraudProofCommittedFieldShape: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof committed-field-shape step-01",
      manifestPosition: 217,
      publicationPosition: 123,
      referenceRolePosition: 88,
      commands: [],
      validatorTitle: "fraud_proofs/committed_field_shape/step_01.main.spend",
      familyPaths: [["firstStep"], ["steps", "0"]],
    },
    fraudProofCommittedFieldShapeStep02: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof committed-field-shape step-02",
      manifestPosition: 218,
      publicationPosition: 124,
      referenceRolePosition: 89,
      commands: [],
      validatorTitle: "fraud_proofs/committed_field_shape/step_02.main.spend",
      familyPaths: [["steps", "1"]],
    },
  },
);
