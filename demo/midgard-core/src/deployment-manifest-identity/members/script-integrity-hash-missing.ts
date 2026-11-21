import { defineDeploymentFamily } from "../members.types.js";

export const scriptIntegrityHashMissingMembers = defineDeploymentFamily(
  "scriptIntegrityHashMissing",
  {
    fraudProofScriptIntegrityHashMissing: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof script-integrity-hash-missing step-01",
        token: "V1FpIntegrityMissingS01",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/script_integrity_hash_missing/step_01.main.spend",
      familyPaths: [["firstStep"], ["steps", "0"]],
    },
    fraudProofScriptIntegrityHashMissingStep02: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof script-integrity-hash-missing step-02",
        token: "V1FpIntegrityMissingS02",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/script_integrity_hash_missing/step_02.main.spend",
      familyPaths: [["steps", "1"]],
    },
    fraudProofScriptIntegrityHashMissingStep03: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof script-integrity-hash-missing step-03",
        token: "V1FpIntegrityMissingS03",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/script_integrity_hash_missing/step_03.main.spend",
      familyPaths: [["steps", "2"]],
    },
    fraudProofScriptIntegrityHashMissingScriptGrammar: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof script-integrity-hash-missing script-grammar",
        token: "V1FpIntegrityMissingGrammar",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/script_integrity_hash_missing/script_grammar.main.spend",
      familyPaths: [["scriptGrammar"], ["steps", "3"]],
    },
    fraudProofScriptIntegrityHashMissingScriptScan: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof script-integrity-hash-missing script-scan",
        token: "V1FpIntegrityMissingScan",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/script_integrity_hash_missing/script_scan.main.spend",
      familyPaths: [["scriptScan"], ["steps", "4"]],
    },
    fraudProofScriptIntegrityHashMissingRedeemerGrammar: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof script-integrity-hash-missing redeemer-grammar",
        token: "V1FpIntegrityMissingRedeemer",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/script_integrity_hash_missing/redeemer_grammar.main.spend",
      familyPaths: [["redeemerGrammar"], ["steps", "5"]],
    },
    fraudProofScriptIntegrityHashMissingStep04: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof script-integrity-hash-missing step-04",
        token: "V1FpIntegrityMissingS04",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/script_integrity_hash_missing/step_04.main.spend",
      familyPaths: [["steps", "6"]],
    },
  },
);
