import { defineDeploymentFamily } from "../members.types.js";

export const referenceInputNoIdxMembers = defineDeploymentFamily(
  "referenceInputNoIdx",
  {
    fraudProofReferenceInputNoIdx: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof reference-input-no-idx step-01",
        token: "V1FpReferenceInputNoIdxS01",
      },
      commands: [],
      validatorTitle: "fraud_proofs/reference_input_no_idx/step_01.main.spend",
      familyPaths: [["firstStep"], ["steps", "0"]],
    },
    fraudProofReferenceInputNoIdxStep02: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof reference-input-no-idx step-02",
        token: "V1FpReferenceInputNoIdxS02",
      },
      commands: [],
      validatorTitle: "fraud_proofs/reference_input_no_idx/step_02.main.spend",
      familyPaths: [["steps", "1"]],
    },
    fraudProofReferenceInputNoIdxStep03: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof reference-input-no-idx step-03",
        token: "V1FpReferenceInputNoIdxS03",
      },
      commands: [],
      validatorTitle: "fraud_proofs/reference_input_no_idx/step_03.main.spend",
      familyPaths: [["steps", "2"]],
    },
    fraudProofReferenceInputNoIdxStep04: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof reference-input-no-idx step-04",
        token: "V1FpReferenceInputNoIdxS04",
      },
      commands: [],
      validatorTitle: "fraud_proofs/reference_input_no_idx/step_04.main.spend",
      familyPaths: [["steps", "3"]],
    },
  },
);
