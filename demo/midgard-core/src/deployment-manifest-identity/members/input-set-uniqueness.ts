import { defineDeploymentFamily } from "../members.types.js";

export const inputSetUniquenessMembers = defineDeploymentFamily(
  "inputSetUniqueness",
  {
    fraudProofInputSetUniqueness: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof input-set-uniqueness step-01",
      manifestPosition: 231,
      publicationPosition: 137,
      referenceRolePosition: 115,
      commands: [],
      validatorTitle: "fraud_proofs/input_set_uniqueness/step_01.main.spend",
      familyPaths: [["firstStep"], ["steps", "0"]],
    },
    fraudProofInputSetUniquenessStep02: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof input-set-uniqueness step-02",
      manifestPosition: 232,
      publicationPosition: 138,
      referenceRolePosition: 116,
      commands: [],
      validatorTitle: "fraud_proofs/input_set_uniqueness/step_02.main.spend",
      familyPaths: [["steps", "1"]],
    },
    fraudProofInputSetUniquenessStep03: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof input-set-uniqueness step-03",
      manifestPosition: 233,
      publicationPosition: 139,
      referenceRolePosition: 117,
      commands: [],
      validatorTitle: "fraud_proofs/input_set_uniqueness/step_03.main.spend",
      familyPaths: [["steps", "2"]],
    },
    fraudProofInputSetUniquenessStep04: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof input-set-uniqueness step-04",
      manifestPosition: 234,
      publicationPosition: 140,
      referenceRolePosition: 118,
      commands: [],
      validatorTitle: "fraud_proofs/input_set_uniqueness/step_04.main.spend",
      familyPaths: [["steps", "3"]],
    },
  },
);
