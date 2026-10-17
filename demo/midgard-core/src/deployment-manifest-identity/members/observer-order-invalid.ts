import { defineDeploymentFamily } from "../members.types.js";

export const observerOrderInvalidMembers = defineDeploymentFamily(
  "observerOrderInvalid",
  {
    fraudProofObserverOrderInvalid: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof observer-order-invalid step-01",
      manifestPosition: 302,
      publicationPosition: 208,
      referenceRolePosition: 449,
      commands: [],
      validatorTitle: "fraud_proofs/observer_order_invalid/step_01.main.spend",
      familyPaths: [["firstStep"], ["steps", "0"]],
    },
    fraudProofObserverOrderInvalidStep02: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof observer-order-invalid step-02",
      manifestPosition: 303,
      publicationPosition: 209,
      referenceRolePosition: 450,
      commands: [],
      validatorTitle: "fraud_proofs/observer_order_invalid/step_02.main.spend",
      familyPaths: [["steps", "1"]],
    },
    fraudProofObserverOrderInvalidStep03: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof observer-order-invalid step-03",
      manifestPosition: 304,
      publicationPosition: 210,
      referenceRolePosition: 451,
      commands: [],
      validatorTitle: "fraud_proofs/observer_order_invalid/step_03.main.spend",
      familyPaths: [["steps", "2"]],
    },
    fraudProofObserverOrderInvalidStep04: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof observer-order-invalid step-04",
      manifestPosition: 305,
      publicationPosition: 211,
      referenceRolePosition: 452,
      commands: [],
      validatorTitle: "fraud_proofs/observer_order_invalid/step_04.main.spend",
      familyPaths: [["steps", "3"]],
    },
  },
);
