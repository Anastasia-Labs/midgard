import { defineDeploymentFamily } from "../members.types.js";

export const scriptIntegrityHashMismatchMembers = defineDeploymentFamily(
  "scriptIntegrityHashMismatch",
  {
    fraudProofScriptIntegrityHashMismatch: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof script-integrity-hash-mismatch step-01",
      manifestPosition: 352,
      publicationPosition: 271,
      referenceRolePosition: 501,
      commands: [],
      validatorTitle:
        "fraud_proofs/script_integrity_hash_mismatch/step_01.main.spend",
      familyPaths: [["firstStep"], ["steps", "0"]],
    },
    fraudProofScriptIntegrityHashMismatchStep02: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof script-integrity-hash-mismatch step-02",
      manifestPosition: 353,
      publicationPosition: 272,
      referenceRolePosition: 502,
      commands: [],
      validatorTitle:
        "fraud_proofs/script_integrity_hash_mismatch/step_02.main.spend",
      familyPaths: [["steps", "1"]],
    },
    fraudProofScriptIntegrityHashMismatchStep03: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof script-integrity-hash-mismatch step-03",
      manifestPosition: 354,
      publicationPosition: 273,
      referenceRolePosition: 503,
      commands: [],
      validatorTitle:
        "fraud_proofs/script_integrity_hash_mismatch/step_03.main.spend",
      familyPaths: [["steps", "2"]],
    },
    fraudProofScriptIntegrityHashMismatchStep04: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof script-integrity-hash-mismatch step-04",
      manifestPosition: 355,
      publicationPosition: 274,
      referenceRolePosition: 504,
      commands: [],
      validatorTitle:
        "fraud_proofs/script_integrity_hash_mismatch/step_04.main.spend",
      familyPaths: [["steps", "3"]],
    },
    fraudProofScriptIntegrityHashMismatchStep05: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof script-integrity-hash-mismatch step-05",
      manifestPosition: 356,
      publicationPosition: 275,
      referenceRolePosition: 505,
      commands: [],
      validatorTitle:
        "fraud_proofs/script_integrity_hash_mismatch/step_05.main.spend",
      familyPaths: [["steps", "4"]],
    },
  },
);
