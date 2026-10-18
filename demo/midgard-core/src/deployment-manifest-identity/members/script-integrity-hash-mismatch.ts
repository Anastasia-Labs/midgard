import { defineDeploymentFamily } from "../members.types.js";

export const scriptIntegrityHashMismatchMembers = defineDeploymentFamily(
  "scriptIntegrityHashMismatch",
  {
    fraudProofScriptIntegrityHashMismatch: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof script-integrity-hash-mismatch step-01",
      manifestPosition: 357,
      publicationPosition: 308,
      referenceRolePosition: 545,
      commands: [],
      validatorTitle:
        "fraud_proofs/script_integrity_hash_mismatch/step_01.main.spend",
      familyPaths: [["firstStep"], ["steps", "0"]],
    },
    fraudProofScriptIntegrityHashMismatchStep02: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof script-integrity-hash-mismatch step-02",
      manifestPosition: 358,
      publicationPosition: 309,
      referenceRolePosition: 546,
      commands: [],
      validatorTitle:
        "fraud_proofs/script_integrity_hash_mismatch/step_02.main.spend",
      familyPaths: [["steps", "1"]],
    },
    fraudProofScriptIntegrityHashMismatchStep03: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof script-integrity-hash-mismatch step-03",
      manifestPosition: 359,
      publicationPosition: 310,
      referenceRolePosition: 547,
      commands: [],
      validatorTitle:
        "fraud_proofs/script_integrity_hash_mismatch/step_03.main.spend",
      familyPaths: [["steps", "2"]],
    },
    fraudProofScriptIntegrityHashMismatchStep04: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof script-integrity-hash-mismatch step-04",
      manifestPosition: 360,
      publicationPosition: 311,
      referenceRolePosition: 548,
      commands: [],
      validatorTitle:
        "fraud_proofs/script_integrity_hash_mismatch/step_04.main.spend",
      familyPaths: [["steps", "3"]],
    },
    fraudProofScriptIntegrityHashMismatchStep05: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof script-integrity-hash-mismatch step-05",
      manifestPosition: 361,
      publicationPosition: 312,
      referenceRolePosition: 549,
      commands: [],
      validatorTitle:
        "fraud_proofs/script_integrity_hash_mismatch/step_05.main.spend",
      familyPaths: [["steps", "4"]],
    },
  },
);
