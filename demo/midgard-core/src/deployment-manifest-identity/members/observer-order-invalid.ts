import { defineDeploymentFamily } from "../members.types.js";

export const observerOrderInvalidMembers = defineDeploymentFamily(
  "observerOrderInvalid",
  {
    fraudProofObserverOrderInvalid: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof observer-order-invalid step-01",
      manifestPosition: 308,
      publicationPosition: 214,
      referenceRolePosition: 457,
      commands: [],
      validatorTitle: "fraud_proofs/observer_order_invalid/step_01.main.spend",
      familyPaths: [["firstStep"], ["steps", "0"]],
    },
    fraudProofObserverOrderInvalidStep02: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof observer-order-invalid step-02",
      manifestPosition: 309,
      publicationPosition: 215,
      referenceRolePosition: 458,
      commands: [],
      validatorTitle: "fraud_proofs/observer_order_invalid/step_02.main.spend",
      familyPaths: [["steps", "1"]],
    },
    fraudProofObserverOrderInvalidStep03: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof observer-order-invalid step-03",
      manifestPosition: 310,
      publicationPosition: 216,
      referenceRolePosition: 459,
      commands: [],
      validatorTitle: "fraud_proofs/observer_order_invalid/step_03.main.spend",
      familyPaths: [["steps", "2"]],
    },
    fraudProofObserverOrderInvalidStep04: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof observer-order-invalid step-04",
      manifestPosition: 311,
      publicationPosition: 217,
      referenceRolePosition: 460,
      commands: [],
      validatorTitle: "fraud_proofs/observer_order_invalid/step_04.main.spend",
      familyPaths: [["steps", "3"]],
    },
  },
);
