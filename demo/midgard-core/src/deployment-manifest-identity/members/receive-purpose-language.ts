import { defineDeploymentFamily } from "../members.types.js";

export const receivePurposeLanguageMembers = defineDeploymentFamily(
  "receivePurposeLanguage",
  {
    fraudProofReceivePurposeLanguage: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof receive-purpose-language step-01",
      manifestPosition: 320,
      publicationPosition: 226,
      referenceRolePosition: 461,
      commands: [],
      validatorTitle:
        "fraud_proofs/receive_purpose_language/step_01.main.spend",
      familyPaths: [["firstStep"], ["steps", "0"]],
    },
    fraudProofReceivePurposeLanguageStep02: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof receive-purpose-language step-02",
      manifestPosition: 321,
      publicationPosition: 227,
      referenceRolePosition: 462,
      commands: [],
      validatorTitle:
        "fraud_proofs/receive_purpose_language/step_02.main.spend",
      familyPaths: [["steps", "1"]],
    },
    fraudProofReceivePurposeLanguageStep03: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof receive-purpose-language step-03",
      manifestPosition: 322,
      publicationPosition: 228,
      referenceRolePosition: 463,
      commands: [],
      validatorTitle:
        "fraud_proofs/receive_purpose_language/step_03.main.spend",
      familyPaths: [["steps", "2"]],
    },
  },
);
