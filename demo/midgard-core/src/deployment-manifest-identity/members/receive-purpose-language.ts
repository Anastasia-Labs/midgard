import { defineDeploymentFamily } from "../members.types.js";

export const receivePurposeLanguageMembers = defineDeploymentFamily(
  "receivePurposeLanguage",
  {
    fraudProofReceivePurposeLanguage: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof receive-purpose-language step-01",
      manifestPosition: 326,
      publicationPosition: 232,
      referenceRolePosition: 469,
      commands: [],
      validatorTitle:
        "fraud_proofs/receive_purpose_language/step_01.main.spend",
      familyPaths: [["firstStep"], ["steps", "0"]],
    },
    fraudProofReceivePurposeLanguageStep02: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof receive-purpose-language step-02",
      manifestPosition: 327,
      publicationPosition: 233,
      referenceRolePosition: 470,
      commands: [],
      validatorTitle:
        "fraud_proofs/receive_purpose_language/step_02.main.spend",
      familyPaths: [["steps", "1"]],
    },
    fraudProofReceivePurposeLanguageStep03: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof receive-purpose-language step-03",
      manifestPosition: 328,
      publicationPosition: 234,
      referenceRolePosition: 471,
      commands: [],
      validatorTitle:
        "fraud_proofs/receive_purpose_language/step_03.main.spend",
      familyPaths: [["steps", "2"]],
    },
  },
);
