import { defineDeploymentFamily } from "../members.types.js";

export const networkIdMembers = defineDeploymentFamily("networkId", {
  fraudProofNetworkId: {
    purpose: "spend",
    referenceRole: "V1 fraud-proof network-id step-01",
    manifestPosition: 242,
    publicationPosition: 148,
    referenceRolePosition: 135,
    commands: [],
    validatorTitle: "fraud_proofs/network_id/step_01.main.spend",
    familyPaths: [["firstStep"], ["steps", "0"]],
  },
  fraudProofNetworkIdStep02: {
    purpose: "spend",
    referenceRole: "V1 fraud-proof network-id step-02",
    manifestPosition: 243,
    publicationPosition: 149,
    referenceRolePosition: 136,
    commands: [],
    validatorTitle: "fraud_proofs/network_id/step_02.main.spend",
    familyPaths: [["steps", "1"]],
  },
  fraudProofNetworkIdForcedStep: {
    purpose: "spend",
    referenceRole: "V1 fraud-proof network-id forced step",
    manifestPosition: 401,
    publicationPosition: 307,
    referenceRolePosition: 137,
    commands: [],
    validatorTitle: "fraud_proofs/network_id/forced_step.main.spend",
    familyPaths: [["forcedStep"]],
  },
  fraudProofNetworkIdForcedScan: {
    purpose: "spend",
    referenceRole: "V1 fraud-proof network-id forced scan",
    manifestPosition: 402,
    publicationPosition: 308,
    referenceRolePosition: 138,
    commands: [],
    validatorTitle: "fraud_proofs/network_id/forced_scan.main.spend",
    familyPaths: [["forcedScan"]],
  },
});
