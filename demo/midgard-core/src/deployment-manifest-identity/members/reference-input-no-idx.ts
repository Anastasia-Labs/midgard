import { defineDeploymentFamily } from "../members.types.js";

export const referenceInputNoIdxMembers = defineDeploymentFamily(
  "referenceInputNoIdx",
  {
    fraudProofReferenceInputNoIdx: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof reference-input-no-idx step-01",
      manifestPosition: 55,
      publicationPosition: 88,
      referenceRolePosition: 165,
      commands: [],
      validatorTitle: "fraud_proofs/reference_input_no_idx/step_01.main.spend",
      familyPaths: [["firstStep"], ["steps", "0"]],
    },
    fraudProofReferenceInputNoIdxStep02: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof reference-input-no-idx step-02",
      manifestPosition: 421,
      publicationPosition: 89,
      referenceRolePosition: 166,
      commands: [],
      validatorTitle: "fraud_proofs/reference_input_no_idx/step_02.main.spend",
      familyPaths: [["steps", "1"]],
    },
    fraudProofReferenceInputNoIdxStep03: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof reference-input-no-idx step-03",
      manifestPosition: 422,
      publicationPosition: 90,
      referenceRolePosition: 167,
      commands: [],
      validatorTitle: "fraud_proofs/reference_input_no_idx/step_03.main.spend",
      familyPaths: [["steps", "2"]],
    },
    fraudProofReferenceInputNoIdxStep04: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof reference-input-no-idx step-04",
      manifestPosition: 423,
      publicationPosition: 91,
      referenceRolePosition: 168,
      commands: [],
      validatorTitle: "fraud_proofs/reference_input_no_idx/step_04.main.spend",
      familyPaths: [["steps", "3"]],
    },
  },
);
