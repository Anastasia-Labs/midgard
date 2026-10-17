import { defineDeploymentFamily } from "../members.types.js";

export const scriptIntegrityHashMissingMembers = defineDeploymentFamily(
  "scriptIntegrityHashMissing",
  {
    fraudProofScriptIntegrityHashMissing: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof script-integrity-hash-missing step-01",
      manifestPosition: 264,
      publicationPosition: 170,
      referenceRolePosition: 400,
      commands: [],
      validatorTitle:
        "fraud_proofs/script_integrity_hash_missing/step_01.main.spend",
      familyPaths: [["firstStep"], ["steps", "0"]],
    },
    fraudProofScriptIntegrityHashMissingStep02: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof script-integrity-hash-missing step-02",
      manifestPosition: 265,
      publicationPosition: 171,
      referenceRolePosition: 401,
      commands: [],
      validatorTitle:
        "fraud_proofs/script_integrity_hash_missing/step_02.main.spend",
      familyPaths: [["steps", "1"]],
    },
    fraudProofScriptIntegrityHashMissingStep03: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof script-integrity-hash-missing step-03",
      manifestPosition: 266,
      publicationPosition: 172,
      referenceRolePosition: 402,
      commands: [],
      validatorTitle:
        "fraud_proofs/script_integrity_hash_missing/step_03.main.spend",
      familyPaths: [["steps", "2"]],
    },
    fraudProofScriptIntegrityHashMissingScriptGrammar: {
      purpose: "spend",
      referenceRole:
        "V1 fraud-proof script-integrity-hash-missing script-grammar",
      manifestPosition: 267,
      publicationPosition: 173,
      referenceRolePosition: 403,
      commands: [],
      validatorTitle:
        "fraud_proofs/script_integrity_hash_missing/script_grammar.main.spend",
      familyPaths: [["scriptGrammar"], ["steps", "3"]],
    },
    fraudProofScriptIntegrityHashMissingScriptScan: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof script-integrity-hash-missing script-scan",
      manifestPosition: 268,
      publicationPosition: 174,
      referenceRolePosition: 404,
      commands: [],
      validatorTitle:
        "fraud_proofs/script_integrity_hash_missing/script_scan.main.spend",
      familyPaths: [["scriptScan"], ["steps", "4"]],
    },
    fraudProofScriptIntegrityHashMissingRedeemerGrammar: {
      purpose: "spend",
      referenceRole:
        "V1 fraud-proof script-integrity-hash-missing redeemer-grammar",
      manifestPosition: 269,
      publicationPosition: 175,
      referenceRolePosition: 405,
      commands: [],
      validatorTitle:
        "fraud_proofs/script_integrity_hash_missing/redeemer_grammar.main.spend",
      familyPaths: [["redeemerGrammar"], ["steps", "5"]],
    },
    fraudProofScriptIntegrityHashMissingStep04: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof script-integrity-hash-missing step-04",
      manifestPosition: 270,
      publicationPosition: 176,
      referenceRolePosition: 406,
      commands: [],
      validatorTitle:
        "fraud_proofs/script_integrity_hash_missing/step_04.main.spend",
      familyPaths: [["steps", "6"]],
    },
  },
);
