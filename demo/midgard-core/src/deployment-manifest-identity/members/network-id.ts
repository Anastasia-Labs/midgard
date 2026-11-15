import { defineDeploymentFamily } from "../members.types.js";

export const networkIdMembers = defineDeploymentFamily("networkId", {
  fraudProofNetworkId: {
    purpose: "spend",
    reference: {
      role: "V1 fraud-proof network-id step-01",
      token: "V1FpNetworkIdS01",
    },
    commands: [],
    validatorTitle: "fraud_proofs/network_id/step_01.main.spend",
    familyPaths: [["firstStep"], ["steps", "0"]],
  },
  fraudProofNetworkIdStep02: {
    purpose: "spend",
    reference: {
      role: "V1 fraud-proof network-id step-02",
      token: "V1FpNetworkIdS02",
    },
    commands: [],
    validatorTitle: "fraud_proofs/network_id/step_02.main.spend",
    familyPaths: [["steps", "1"]],
  },
  fraudProofNetworkIdForcedStep: {
    purpose: "spend",
    reference: {
      role: "V1 fraud-proof network-id forced step",
      token: "V1FpNetworkIdForced",
    },
    commands: [],
    validatorTitle: "fraud_proofs/network_id/forced_step.main.spend",
    familyPaths: [["forcedStep"]],
  },
  fraudProofNetworkIdForcedScan: {
    purpose: "spend",
    reference: {
      role: "V1 fraud-proof network-id forced scan",
      token: "V1FpNetworkIdForcedScan",
    },
    commands: [],
    validatorTitle: "fraud_proofs/network_id/forced_scan.main.spend",
    familyPaths: [["forcedScan"]],
  },
});
