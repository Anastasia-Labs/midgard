import { defineDeploymentFamily } from "../members.types.js";

export const l2TxMistagMembers = defineDeploymentFamily("l2TxMistag", {
  fraudProofL2TxMistag: {
    purpose: "spend",
    referenceRole: "V1 fraud-proof l2-tx-mistag step-01",
    manifestPosition: 221,
    publicationPosition: 127,
    referenceRolePosition: 92,
    commands: [],
    validatorTitle: "fraud_proofs/l2_tx_mistag/step_01.main.spend",
    familyPaths: [["firstStep"], ["steps", "0"]],
  },
  fraudProofL2TxMistagStep02: {
    purpose: "spend",
    referenceRole: "V1 fraud-proof l2-tx-mistag step-02",
    manifestPosition: 222,
    publicationPosition: 128,
    referenceRolePosition: 93,
    commands: [],
    validatorTitle: "fraud_proofs/l2_tx_mistag/step_02.main.spend",
    familyPaths: [["steps", "1"]],
  },
});
