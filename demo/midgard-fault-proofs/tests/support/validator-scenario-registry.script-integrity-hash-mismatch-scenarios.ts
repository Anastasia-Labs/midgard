import type { ValidatorScenarioPair } from "./validator-scenario-registry.validator-scenarios.js";

const SIHM_FORGED_SUCCESSOR =
  "demo/midgard-fault-proofs/tests/submit-init-emulator-script-integrity-hash-mismatch-forged-successor.test.ts";
const sihmForgedSuccessor: ValidatorScenarioPair = {
  passing: [
    {
      file: SIHM_FORGED_SUCCESSOR,
      test: "refuses a forged successor at every role of an accepted transaction",
    },
  ],
  failing: [
    {
      file: SIHM_FORGED_SUCCESSOR,
      test: "refuses a forged successor at every role of an accepted transaction",
    },
  ],
};
const SIHM_VERTICALS =
  "demo/midgard-fault-proofs/tests/submit-init-emulator-script-integrity-hash-mismatch-verticals.test.ts";
/**
 * The script-integrity-hash-mismatch validators' scenarios: the shared
 * all-purposes walk roles and the family's entry, which a forged successor
 * row refuses at every role, and its terminal. `VALIDATOR_SCENARIOS` spreads
 * them in.
 */
export const SCRIPT_INTEGRITY_HASH_MISMATCH_SCENARIOS: Readonly<
  Record<string, ValidatorScenarioPair>
> = {
  "fraud_proofs/direct_script_derivation/all_purposes_spend.main":
    sihmForgedSuccessor,
  "fraud_proofs/direct_script_derivation/all_purposes_spend_resolve.main":
    sihmForgedSuccessor,
  "fraud_proofs/direct_script_derivation/all_purposes_mint_grammar.main":
    sihmForgedSuccessor,
  "fraud_proofs/direct_script_derivation/all_purposes_mint.main":
    sihmForgedSuccessor,
  "fraud_proofs/direct_script_derivation/all_purposes_observe_open.main":
    sihmForgedSuccessor,
  "fraud_proofs/direct_script_derivation/all_purposes_observe.main":
    sihmForgedSuccessor,
  "fraud_proofs/direct_script_derivation/all_purposes_receive_grammar.main":
    sihmForgedSuccessor,
  "fraud_proofs/direct_script_derivation/all_purposes_receive.main":
    sihmForgedSuccessor,
  "fraud_proofs/script_integrity_hash_mismatch/family_entry.main":
    sihmForgedSuccessor,
  "fraud_proofs/script_integrity_hash_mismatch/terminal.main": {
    passing: [
      {
        file: SIHM_VERTICALS,
        test: "A-N: convicts an accepted transaction whose hash omits a walked language and removes its block",
      },
    ],
    failing: [
      {
        file: SIHM_VERTICALS,
        test: "refuses $name: the terminal returns false",
      },
    ],
  },
};

/** The family's scenario pair: an A-N conviction and the terminal refusals. */
export const SCRIPT_INTEGRITY_HASH_MISMATCH_FAMILY_SCENARIOS: ValidatorScenarioPair =
  SCRIPT_INTEGRITY_HASH_MISMATCH_SCENARIOS[
    "fraud_proofs/script_integrity_hash_mismatch/terminal.main"
  ]!;
