import { defineDeploymentFamily } from "../members.types.js";

export const scriptIntegrityHashMismatchMembers = defineDeploymentFamily(
  "scriptIntegrityHashMismatch",
  {
    fraudProofScriptIntegrityHashMismatch: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof script-integrity-hash-mismatch step-01",
      manifestPosition: 374,
      publicationPosition: 356,
      referenceRolePosition: 589,
      commands: [],
      validatorTitle:
        "fraud_proofs/script_integrity_hash_mismatch/step_01.main.spend",
      familyPaths: [["firstStep"], ["steps", "0"]],
    },
    fraudProofScriptIntegrityHashMismatchStep02: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof script-integrity-hash-mismatch step-02",
      manifestPosition: 375,
      publicationPosition: 357,
      referenceRolePosition: 590,
      commands: [],
      validatorTitle:
        "fraud_proofs/script_integrity_hash_mismatch/step_02.main.spend",
      familyPaths: [["steps", "1"]],
    },
    fraudProofScriptIntegrityHashMismatchStep03: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof script-integrity-hash-mismatch step-03",
      manifestPosition: 376,
      publicationPosition: 358,
      referenceRolePosition: 591,
      commands: [],
      validatorTitle:
        "fraud_proofs/script_integrity_hash_mismatch/step_03.main.spend",
      familyPaths: [["steps", "2"]],
    },
    fraudProofScriptIntegrityHashMismatchStep04: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof script-integrity-hash-mismatch step-04",
      manifestPosition: 377,
      publicationPosition: 359,
      referenceRolePosition: 592,
      commands: [],
      validatorTitle:
        "fraud_proofs/script_integrity_hash_mismatch/step_04.main.spend",
      familyPaths: [["steps", "3"]],
    },
    fraudProofScriptIntegrityHashMismatchStep05: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof script-integrity-hash-mismatch step-05",
      manifestPosition: 378,
      publicationPosition: 360,
      referenceRolePosition: 593,
      commands: [],
      validatorTitle:
        "fraud_proofs/script_integrity_hash_mismatch/step_05.main.spend",
      familyPaths: [["steps", "4"]],
    },
  },
);
