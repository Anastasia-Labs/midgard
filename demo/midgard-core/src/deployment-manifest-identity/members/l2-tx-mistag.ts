import { defineDeploymentFamily } from "../members.types.js";

export const l2TxMistagMembers = defineDeploymentFamily("l2TxMistag", {
  fraudProofL2TxMistag: {
    purpose: "spend",
    referenceRole: "V1 fraud-proof l2-tx-mistag step-01",
    manifestPosition: 228,
    publicationPosition: 134,
    referenceRolePosition: 99,
    commands: [],
    validatorTitle: "fraud_proofs/l2_tx_mistag/step_01.main.spend",
    familyPaths: [["firstStep"], ["steps", "0"]],
  },
  fraudProofL2TxMistagStep02: {
    purpose: "spend",
    referenceRole: "V1 fraud-proof l2-tx-mistag step-02",
    manifestPosition: 229,
    publicationPosition: 135,
    referenceRolePosition: 100,
    commands: [],
    validatorTitle: "fraud_proofs/l2_tx_mistag/step_02.main.spend",
    familyPaths: [["steps", "1"]],
  },
});
