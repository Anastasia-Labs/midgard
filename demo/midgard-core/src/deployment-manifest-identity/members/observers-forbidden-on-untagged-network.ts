import { defineDeploymentFamily } from "../members.types.js";

export const observersForbiddenOnUntaggedNetworkMembers =
  defineDeploymentFamily("observersForbiddenOnUntaggedNetwork", {
    fraudProofObserversForbiddenOnUntaggedNetwork: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof observers-forbidden-on-untagged-network step-01",
        token: "V1FpObserversForbiddenS01",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/observers_forbidden_on_untagged_network/step_01.main.spend",
      familyPaths: [["firstStep"], ["steps", "0"]],
    },
    fraudProofObserversForbiddenOnUntaggedNetworkStep02: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof observers-forbidden-on-untagged-network step-02",
        token: "V1FpObserversForbiddenS02",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/observers_forbidden_on_untagged_network/step_02.main.spend",
      familyPaths: [["steps", "1"]],
    },
  });
