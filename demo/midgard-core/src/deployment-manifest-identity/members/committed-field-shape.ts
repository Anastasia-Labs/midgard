import { defineDeploymentFamily } from "../members.types.js";

export const committedFieldShapeMembers = defineDeploymentFamily(
  "committedFieldShape",
  {
    fraudProofCommittedFieldShape: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof committed-field-shape step-01",
        token: "V1FpCommittedFieldShapeS01",
      },
      commands: [],
      validatorTitle: "fraud_proofs/committed_field_shape/step_01.main.spend",
      familyPaths: [["firstStep"], ["steps", "0"]],
    },
    fraudProofCommittedFieldShapeStep02: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof committed-field-shape step-02",
        token: "V1FpCommittedFieldShapeS02",
      },
      commands: [],
      validatorTitle: "fraud_proofs/committed_field_shape/step_02.main.spend",
      familyPaths: [["steps", "1"]],
    },
  },
);
