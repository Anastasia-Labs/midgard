import { defineDeploymentFamily } from "../members.types.js";

export const networkIdMembers = defineDeploymentFamily("networkId", {
  fraudProofNetworkId: {
    purpose: "spend",
    referenceRole: "V1 fraud-proof network-id step-01",
    manifestPosition: 241,
    publicationPosition: 147,
    referenceRolePosition: 134,
    commands: [],
    validatorTitle: "fraud_proofs/network_id/step_01.main.spend",
    familyPaths: [["firstStep"], ["steps", "0"]],
  },
  fraudProofNetworkIdStep02: {
    purpose: "spend",
    referenceRole: "V1 fraud-proof network-id step-02",
    manifestPosition: 242,
    publicationPosition: 148,
    referenceRolePosition: 135,
    commands: [],
    validatorTitle: "fraud_proofs/network_id/step_02.main.spend",
    familyPaths: [["steps", "1"]],
  },
  fraudProofNetworkIdForcedStep: {
    purpose: "spend",
    referenceRole: "V1 fraud-proof network-id forced step",
    manifestPosition: 400,
    publicationPosition: 306,
    referenceRolePosition: 136,
    commands: [],
    validatorTitle: "fraud_proofs/network_id/forced_step.main.spend",
    familyPaths: [["forcedStep"]],
  },
  fraudProofNetworkIdForcedScan: {
    purpose: "spend",
    referenceRole: "V1 fraud-proof network-id forced scan",
    manifestPosition: 401,
    publicationPosition: 307,
    referenceRolePosition: 137,
    commands: [],
    validatorTitle: "fraud_proofs/network_id/forced_scan.main.spend",
    familyPaths: [["forcedScan"]],
  },
});
