import { defineDeploymentFamily } from "../members.types.js";

export const referenceInputNoIdxMembers = defineDeploymentFamily(
  "referenceInputNoIdx",
  {
    fraudProofReferenceInputNoIdx: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof reference-input-no-idx step-01",
      manifestPosition: 55,
      publicationPosition: 94,
      referenceRolePosition: 171,
      commands: [],
      validatorTitle: "fraud_proofs/reference_input_no_idx/step_01.main.spend",
      familyPaths: [["firstStep"], ["steps", "0"]],
    },
    fraudProofReferenceInputNoIdxStep02: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof reference-input-no-idx step-02",
      manifestPosition: 475,
      publicationPosition: 95,
      referenceRolePosition: 172,
      commands: [],
      validatorTitle: "fraud_proofs/reference_input_no_idx/step_02.main.spend",
      familyPaths: [["steps", "1"]],
    },
    fraudProofReferenceInputNoIdxStep03: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof reference-input-no-idx step-03",
      manifestPosition: 476,
      publicationPosition: 96,
      referenceRolePosition: 173,
      commands: [],
      validatorTitle: "fraud_proofs/reference_input_no_idx/step_03.main.spend",
      familyPaths: [["steps", "2"]],
    },
    fraudProofReferenceInputNoIdxStep04: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof reference-input-no-idx step-04",
      manifestPosition: 477,
      publicationPosition: 97,
      referenceRolePosition: 174,
      commands: [],
      validatorTitle: "fraud_proofs/reference_input_no_idx/step_04.main.spend",
      familyPaths: [["steps", "3"]],
    },
  },
);
