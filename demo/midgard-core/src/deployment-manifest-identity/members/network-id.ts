import { defineDeploymentFamily } from "../members.types.js";

export const networkIdMembers = defineDeploymentFamily("networkId", {
  fraudProofNetworkId: {
    purpose: "spend",
    referenceRole: "V1 fraud-proof network-id step-01",
    manifestPosition: 247,
    publicationPosition: 153,
    referenceRolePosition: 140,
    commands: [],
    validatorTitle: "fraud_proofs/network_id/step_01.main.spend",
    familyPaths: [["firstStep"], ["steps", "0"]],
  },
  fraudProofNetworkIdStep02: {
    purpose: "spend",
    referenceRole: "V1 fraud-proof network-id step-02",
    manifestPosition: 248,
    publicationPosition: 154,
    referenceRolePosition: 141,
    commands: [],
    validatorTitle: "fraud_proofs/network_id/step_02.main.spend",
    familyPaths: [["steps", "1"]],
  },
  fraudProofNetworkIdForcedStep: {
    purpose: "spend",
    referenceRole: "V1 fraud-proof network-id forced step",
    manifestPosition: 436,
    publicationPosition: 342,
    referenceRolePosition: 142,
    commands: [],
    validatorTitle: "fraud_proofs/network_id/forced_step.main.spend",
    familyPaths: [["forcedStep"]],
  },
  fraudProofNetworkIdForcedScan: {
    purpose: "spend",
    referenceRole: "V1 fraud-proof network-id forced scan",
    manifestPosition: 437,
    publicationPosition: 343,
    referenceRolePosition: 143,
    commands: [],
    validatorTitle: "fraud_proofs/network_id/forced_scan.main.spend",
    familyPaths: [["forcedScan"]],
  },
});
