import { defineDeploymentFamily } from "../members.types.js";

export const fieldItemWidthIllegalMembers = defineDeploymentFamily(
  "fieldItemWidthIllegal",
  {
    fraudProofFieldItemWidthIllegal: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof field-item-width-illegal step-01",
      manifestPosition: 256,
      publicationPosition: 162,
      referenceRolePosition: 397,
      commands: [],
      validatorTitle:
        "fraud_proofs/field_item_width_illegal/step_01.main.spend",
      familyPaths: [["firstStep"], ["steps", "0"]],
    },
    fraudProofFieldItemWidthIllegalStep02: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof field-item-width-illegal step-02",
      manifestPosition: 257,
      publicationPosition: 163,
      referenceRolePosition: 398,
      commands: [],
      validatorTitle:
        "fraud_proofs/field_item_width_illegal/step_02.main.spend",
      familyPaths: [["steps", "1"]],
    },
    fraudProofFieldItemWidthIllegalStep03: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof field-item-width-illegal step-03",
      manifestPosition: 258,
      publicationPosition: 164,
      referenceRolePosition: 399,
      commands: [],
      validatorTitle:
        "fraud_proofs/field_item_width_illegal/step_03.main.spend",
      familyPaths: [["steps", "2"]],
    },
  },
);
