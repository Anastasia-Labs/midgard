import { defineDeploymentFamily } from "../members.types.js";

export const resolvedOutputNonCanonicalMembers = defineDeploymentFamily(
  "resolvedOutputNonCanonical",
  {
    fraudProofResolvedOutputNonCanonical: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof resolved-output-non-canonical step-01",
      commands: [],
      validatorTitle:
        "fraud_proofs/resolved_output_non_canonical/step_01.main.spend",
      familyPaths: [["firstStep"], ["steps", "0"]],
    },
    fraudProofResolvedOutputNonCanonicalStep02: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof resolved-output-non-canonical step-02",
      commands: [],
      validatorTitle:
        "fraud_proofs/resolved_output_non_canonical/step_02.main.spend",
      familyPaths: [["steps", "1"]],
    },
    fraudProofResolvedOutputNonCanonicalStep03: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof resolved-output-non-canonical step-03",
      commands: [],
      validatorTitle:
        "fraud_proofs/resolved_output_non_canonical/step_03.main.spend",
      familyPaths: [["steps", "2"]],
    },
    fraudProofResolvedOutputNonCanonicalStep04: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof resolved-output-non-canonical step-04",
      commands: [],
      validatorTitle:
        "fraud_proofs/resolved_output_non_canonical/step_04.main.spend",
      familyPaths: [["steps", "3"]],
    },
    fraudProofResolvedOutputNonCanonicalStep04Datum: {
      purpose: "spend",
      referenceRole:
        "V1 fraud-proof resolved-output-non-canonical step-04 datum",
      commands: [],
      validatorTitle:
        "fraud_proofs/resolved_output_non_canonical/step_04_datum.main.spend",
      familyPaths: [["steps", "4"]],
    },
    fraudProofResolvedOutputNonCanonicalStep05: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof resolved-output-non-canonical step-05",
      commands: [],
      validatorTitle:
        "fraud_proofs/resolved_output_non_canonical/step_05.main.spend",
      familyPaths: [["steps", "5"]],
    },
  },
);
