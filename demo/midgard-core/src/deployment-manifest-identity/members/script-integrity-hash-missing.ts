import { defineDeploymentFamily } from "../members.types.js";

export const scriptIntegrityHashMissingMembers = defineDeploymentFamily(
  "scriptIntegrityHashMissing",
  {
    fraudProofScriptIntegrityHashMissing: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof script-integrity-hash-missing step-01",
      manifestPosition: 270,
      publicationPosition: 176,
      referenceRolePosition: 408,
      commands: [],
      validatorTitle:
        "fraud_proofs/script_integrity_hash_missing/step_01.main.spend",
      familyPaths: [["firstStep"], ["steps", "0"]],
    },
    fraudProofScriptIntegrityHashMissingStep02: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof script-integrity-hash-missing step-02",
      manifestPosition: 271,
      publicationPosition: 177,
      referenceRolePosition: 409,
      commands: [],
      validatorTitle:
        "fraud_proofs/script_integrity_hash_missing/step_02.main.spend",
      familyPaths: [["steps", "1"]],
    },
    fraudProofScriptIntegrityHashMissingStep03: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof script-integrity-hash-missing step-03",
      manifestPosition: 272,
      publicationPosition: 178,
      referenceRolePosition: 410,
      commands: [],
      validatorTitle:
        "fraud_proofs/script_integrity_hash_missing/step_03.main.spend",
      familyPaths: [["steps", "2"]],
    },
    fraudProofScriptIntegrityHashMissingScriptGrammar: {
      purpose: "spend",
      referenceRole:
        "V1 fraud-proof script-integrity-hash-missing script-grammar",
      manifestPosition: 273,
      publicationPosition: 179,
      referenceRolePosition: 411,
      commands: [],
      validatorTitle:
        "fraud_proofs/script_integrity_hash_missing/script_grammar.main.spend",
      familyPaths: [["scriptGrammar"], ["steps", "3"]],
    },
    fraudProofScriptIntegrityHashMissingScriptScan: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof script-integrity-hash-missing script-scan",
      manifestPosition: 274,
      publicationPosition: 180,
      referenceRolePosition: 412,
      commands: [],
      validatorTitle:
        "fraud_proofs/script_integrity_hash_missing/script_scan.main.spend",
      familyPaths: [["scriptScan"], ["steps", "4"]],
    },
    fraudProofScriptIntegrityHashMissingRedeemerGrammar: {
      purpose: "spend",
      referenceRole:
        "V1 fraud-proof script-integrity-hash-missing redeemer-grammar",
      manifestPosition: 275,
      publicationPosition: 181,
      referenceRolePosition: 413,
      commands: [],
      validatorTitle:
        "fraud_proofs/script_integrity_hash_missing/redeemer_grammar.main.spend",
      familyPaths: [["redeemerGrammar"], ["steps", "5"]],
    },
    fraudProofScriptIntegrityHashMissingStep04: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof script-integrity-hash-missing step-04",
      manifestPosition: 276,
      publicationPosition: 182,
      referenceRolePosition: 414,
      commands: [],
      validatorTitle:
        "fraud_proofs/script_integrity_hash_missing/step_04.main.spend",
      familyPaths: [["steps", "6"]],
    },
  },
);
