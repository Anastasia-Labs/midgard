import { defineDeploymentFamily } from "../members.types.js";

export const observerOrderInvalidMembers = defineDeploymentFamily(
  "observerOrderInvalid",
  {
    fraudProofObserverOrderInvalid: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof observer-order-invalid step-01",
        token: "V1FpObserverOrderS01",
      },
      commands: [],
      validatorTitle: "fraud_proofs/observer_order_invalid/step_01.main.spend",
      familyPaths: [["firstStep"], ["steps", "0"]],
    },
    fraudProofObserverOrderInvalidStep02: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof observer-order-invalid step-02",
        token: "V1FpObserverOrderS02",
      },
      commands: [],
      validatorTitle: "fraud_proofs/observer_order_invalid/step_02.main.spend",
      familyPaths: [["steps", "1"]],
    },
    fraudProofObserverOrderInvalidStep03: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof observer-order-invalid step-03",
        token: "V1FpObserverOrderS03",
      },
      commands: [],
      validatorTitle: "fraud_proofs/observer_order_invalid/step_03.main.spend",
      familyPaths: [["steps", "2"]],
    },
    fraudProofObserverOrderInvalidStep04: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof observer-order-invalid step-04",
        token: "V1FpObserverOrderS04",
      },
      commands: [],
      validatorTitle: "fraud_proofs/observer_order_invalid/step_04.main.spend",
      familyPaths: [["steps", "3"]],
    },
  },
);
