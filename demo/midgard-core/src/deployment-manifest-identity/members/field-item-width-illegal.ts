import { defineDeploymentFamily } from "../members.types.js";

export const fieldItemWidthIllegalMembers = defineDeploymentFamily(
  "fieldItemWidthIllegal",
  {
    fraudProofFieldItemWidthIllegal: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof field-item-width-illegal step-01",
      commands: [],
      validatorTitle:
        "fraud_proofs/field_item_width_illegal/step_01.main.spend",
      familyPaths: [["firstStep"], ["steps", "0"]],
    },
    fraudProofFieldItemWidthIllegalStep02: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof field-item-width-illegal step-02",
      commands: [],
      validatorTitle:
        "fraud_proofs/field_item_width_illegal/step_02.main.spend",
      familyPaths: [["steps", "1"]],
    },
    fraudProofFieldItemWidthIllegalStep03: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof field-item-width-illegal step-03",
      commands: [],
      validatorTitle:
        "fraud_proofs/field_item_width_illegal/step_03.main.spend",
      familyPaths: [["steps", "2"]],
    },
  },
);
