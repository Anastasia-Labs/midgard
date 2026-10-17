import { defineDeploymentFamily } from "../members.types.js";

export const resolvedOutputNonCanonicalMembers = defineDeploymentFamily(
  "resolvedOutputNonCanonical",
  {
    fraudProofResolvedOutputNonCanonical: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof resolved-output-non-canonical step-01",
      manifestPosition: 285,
      publicationPosition: 191,
      referenceRolePosition: 428,
      commands: [],
      validatorTitle:
        "fraud_proofs/resolved_output_non_canonical/step_01.main.spend",
      familyPaths: [["firstStep"], ["steps", "0"]],
    },
    fraudProofResolvedOutputNonCanonicalStep02: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof resolved-output-non-canonical step-02",
      manifestPosition: 286,
      publicationPosition: 192,
      referenceRolePosition: 429,
      commands: [],
      validatorTitle:
        "fraud_proofs/resolved_output_non_canonical/step_02.main.spend",
      familyPaths: [["steps", "1"]],
    },
    fraudProofResolvedOutputNonCanonicalStep03: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof resolved-output-non-canonical step-03",
      manifestPosition: 287,
      publicationPosition: 193,
      referenceRolePosition: 430,
      commands: [],
      validatorTitle:
        "fraud_proofs/resolved_output_non_canonical/step_03.main.spend",
      familyPaths: [["steps", "2"]],
    },
    fraudProofResolvedOutputNonCanonicalStep04: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof resolved-output-non-canonical step-04",
      manifestPosition: 288,
      publicationPosition: 194,
      referenceRolePosition: 431,
      commands: [],
      validatorTitle:
        "fraud_proofs/resolved_output_non_canonical/step_04.main.spend",
      familyPaths: [["steps", "3"]],
    },
    fraudProofResolvedOutputNonCanonicalStep04Datum: {
      purpose: "spend",
      referenceRole:
        "V1 fraud-proof resolved-output-non-canonical step-04 datum",
      manifestPosition: 289,
      publicationPosition: 195,
      referenceRolePosition: 432,
      commands: [],
      validatorTitle:
        "fraud_proofs/resolved_output_non_canonical/step_04_datum.main.spend",
      familyPaths: [["steps", "4"]],
    },
    fraudProofResolvedOutputNonCanonicalStep05: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof resolved-output-non-canonical step-05",
      manifestPosition: 290,
      publicationPosition: 196,
      referenceRolePosition: 433,
      commands: [],
      validatorTitle:
        "fraud_proofs/resolved_output_non_canonical/step_05.main.spend",
      familyPaths: [["steps", "5"]],
    },
  },
);
