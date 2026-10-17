import { defineDeploymentFamily } from "../members.types.js";

export const observersForbiddenOnUntaggedNetworkMembers =
  defineDeploymentFamily("observersForbiddenOnUntaggedNetwork", {
    fraudProofObserversForbiddenOnUntaggedNetwork: {
      purpose: "spend",
      referenceRole:
        "V1 fraud-proof observers-forbidden-on-untagged-network step-01",
      manifestPosition: 305,
      publicationPosition: 211,
      referenceRolePosition: 448,
      commands: [],
      validatorTitle:
        "fraud_proofs/observers_forbidden_on_untagged_network/step_01.main.spend",
      familyPaths: [["firstStep"], ["steps", "0"]],
    },
    fraudProofObserversForbiddenOnUntaggedNetworkStep02: {
      purpose: "spend",
      referenceRole:
        "V1 fraud-proof observers-forbidden-on-untagged-network step-02",
      manifestPosition: 306,
      publicationPosition: 212,
      referenceRolePosition: 449,
      commands: [],
      validatorTitle:
        "fraud_proofs/observers_forbidden_on_untagged_network/step_02.main.spend",
      familyPaths: [["steps", "1"]],
    },
  });
