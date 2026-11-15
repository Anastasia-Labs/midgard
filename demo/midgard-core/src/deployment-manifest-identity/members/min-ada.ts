import { defineDeploymentFamily } from "../members.types.js";

export const minAdaMembers = defineDeploymentFamily("minAda", {
  fraudProofMinAda: {
    purpose: "spend",
    reference: {
      role: "V1 fraud-proof min-ada step-01",
      token: "V1FpMinAdaS01",
    },
    commands: [],
    validatorTitle: "fraud_proofs/min_ada/step_01.main.spend",
    familyPaths: [["firstStep"], ["steps", "0"]],
  },
  fraudProofMinAdaStep02: {
    purpose: "spend",
    reference: {
      role: "V1 fraud-proof min-ada step-02",
      token: "V1FpMinAdaS02",
    },
    commands: [],
    validatorTitle: "fraud_proofs/min_ada/step_02.main.spend",
    familyPaths: [["steps", "1"]],
  },
  fraudProofMinAdaStep03: {
    purpose: "spend",
    reference: {
      role: "V1 fraud-proof min-ada step-03",
      token: "V1FpMinAdaS03",
    },
    commands: [],
    validatorTitle: "fraud_proofs/min_ada/step_03.main.spend",
    familyPaths: [["steps", "2"]],
  },
  fraudProofMinAdaStep04: {
    purpose: "spend",
    reference: {
      role: "V1 fraud-proof min-ada step-04",
      token: "V1FpMinAdaS04",
    },
    commands: [],
    validatorTitle: "fraud_proofs/min_ada/step_04.main.spend",
    familyPaths: [["steps", "3"]],
  },
  fraudProofMinAdaStep02TxWithdraw: {
    purpose: "withdraw",
    reference: {
      role: "V1 fraud-proof min-ada step-02 tx yield",
      token: "V1FpMinAdaS02TxYield",
    },
    commands: [],
    validatorTitle: "fraud_proofs/min_ada/step_02_yields.tx.withdraw",
    familyPaths: [["yields", "tx"]],
  },
});
