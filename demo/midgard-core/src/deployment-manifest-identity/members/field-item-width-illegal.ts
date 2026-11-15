import { defineDeploymentFamily } from "../members.types.js";

export const fieldItemWidthIllegalMembers = defineDeploymentFamily(
  "fieldItemWidthIllegal",
  {
    fraudProofFieldItemWidthIllegal: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof field-item-width-illegal step-01",
        token: "V1FpItemWidthS01",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/field_item_width_illegal/step_01.main.spend",
      familyPaths: [["firstStep"], ["steps", "0"]],
    },
    fraudProofFieldItemWidthIllegalStep02: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof field-item-width-illegal step-02",
        token: "V1FpItemWidthS02",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/field_item_width_illegal/step_02.main.spend",
      familyPaths: [["steps", "1"]],
    },
    fraudProofFieldItemWidthIllegalStep03: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof field-item-width-illegal step-03",
        token: "V1FpItemWidthS03",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/field_item_width_illegal/step_03.main.spend",
      familyPaths: [["steps", "2"]],
    },
  },
);
