import { defineDeploymentFamily } from "../members.types.js";

export const fieldItemWidthIllegalMembers = defineDeploymentFamily(
  "fieldItemWidthIllegal",
  {
    fraudProofFieldItemWidthIllegal: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof field-item-width-illegal step-01",
      manifestPosition: 262,
      publicationPosition: 168,
      referenceRolePosition: 405,
      commands: [],
      validatorTitle:
        "fraud_proofs/field_item_width_illegal/step_01.main.spend",
      familyPaths: [["firstStep"], ["steps", "0"]],
    },
    fraudProofFieldItemWidthIllegalStep02: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof field-item-width-illegal step-02",
      manifestPosition: 263,
      publicationPosition: 169,
      referenceRolePosition: 406,
      commands: [],
      validatorTitle:
        "fraud_proofs/field_item_width_illegal/step_02.main.spend",
      familyPaths: [["steps", "1"]],
    },
    fraudProofFieldItemWidthIllegalStep03: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof field-item-width-illegal step-03",
      manifestPosition: 264,
      publicationPosition: 170,
      referenceRolePosition: 407,
      commands: [],
      validatorTitle:
        "fraud_proofs/field_item_width_illegal/step_03.main.spend",
      familyPaths: [["steps", "2"]],
    },
  },
);
