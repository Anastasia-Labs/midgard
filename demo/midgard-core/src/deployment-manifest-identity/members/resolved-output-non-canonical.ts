import { defineDeploymentFamily } from "../members.types.js";

export const resolvedOutputNonCanonicalMembers = defineDeploymentFamily(
  "resolvedOutputNonCanonical",
  {
    fraudProofResolvedOutputNonCanonical: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof resolved-output-non-canonical step-01",
        token: "V1FpResolvedOutputS01",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/resolved_output_non_canonical/step_01.main.spend",
      familyPaths: [["firstStep"], ["steps", "0"]],
    },
    fraudProofResolvedOutputNonCanonicalStep02: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof resolved-output-non-canonical step-02",
        token: "V1FpResolvedOutputS02",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/resolved_output_non_canonical/step_02.main.spend",
      familyPaths: [["steps", "1"]],
    },
    fraudProofResolvedOutputNonCanonicalStep03: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof resolved-output-non-canonical step-03",
        token: "V1FpResolvedOutputS03",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/resolved_output_non_canonical/step_03.main.spend",
      familyPaths: [["steps", "2"]],
    },
    fraudProofResolvedOutputNonCanonicalStep04: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof resolved-output-non-canonical step-04",
        token: "V1FpResolvedOutputS04",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/resolved_output_non_canonical/step_04.main.spend",
      familyPaths: [["steps", "3"]],
    },
    fraudProofResolvedOutputNonCanonicalStep04Datum: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof resolved-output-non-canonical step-04 datum",
        token: "V1FpResolvedOutputS04Datum",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/resolved_output_non_canonical/step_04_datum.main.spend",
      familyPaths: [["steps", "4"]],
    },
    fraudProofResolvedOutputNonCanonicalStep05: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof resolved-output-non-canonical step-05",
        token: "V1FpResolvedOutputS05",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/resolved_output_non_canonical/step_05.main.spend",
      familyPaths: [["steps", "5"]],
    },
  },
);
