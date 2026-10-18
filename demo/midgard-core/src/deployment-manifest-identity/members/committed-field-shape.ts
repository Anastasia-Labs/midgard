import { defineDeploymentFamily } from "../members.types.js";

export const committedFieldShapeMembers = defineDeploymentFamily(
  "committedFieldShape",
  {
    fraudProofCommittedFieldShape: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof committed-field-shape step-01",
      manifestPosition: 216,
      publicationPosition: 122,
      referenceRolePosition: 87,
      commands: [],
      validatorTitle: "fraud_proofs/committed_field_shape/step_01.main.spend",
      familyPaths: [["firstStep"], ["steps", "0"]],
    },
    fraudProofCommittedFieldShapeStep02: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof committed-field-shape step-02",
      manifestPosition: 217,
      publicationPosition: 123,
      referenceRolePosition: 88,
      commands: [],
      validatorTitle: "fraud_proofs/committed_field_shape/step_02.main.spend",
      familyPaths: [["steps", "1"]],
    },
  },
);
