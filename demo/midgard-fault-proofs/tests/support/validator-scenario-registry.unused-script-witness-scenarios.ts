import type { ValidatorScenarioPair } from "./validator-scenario-registry.validator-scenarios.js";

const USW_FORGED_SUCCESSOR =
  "demo/midgard-fault-proofs/tests/submit-init-emulator-unused-script-witness-forged-successor.test.ts";
const uswForgedSuccessor: ValidatorScenarioPair = {
  passing: [
    {
      file: USW_FORGED_SUCCESSOR,
      test: "refuses a forged successor at every role of an accepted transaction",
    },
  ],
  failing: [
    {
      file: USW_FORGED_SUCCESSOR,
      test: "refuses a forged successor at every role of an accepted transaction",
    },
  ],
};
const USW_VERTICALS =
  "demo/midgard-fault-proofs/tests/submit-init-emulator-unused-script-witness-verticals.test.ts";
/**
 * The unused-script-witness validators' scenarios: the family's entry and
 * field-6 witness walk roles, which a forged successor row refuses at every
 * role, and its terminal. `VALIDATOR_SCENARIOS` spreads them in.
 */
export const UNUSED_SCRIPT_WITNESS_SCENARIOS: Readonly<
  Record<string, ValidatorScenarioPair>
> = {
  "fraud_proofs/unused_script_witness/family_entry.main": uswForgedSuccessor,
  "fraud_proofs/unused_script_witness/witness_entry.main": uswForgedSuccessor,
  "fraud_proofs/unused_script_witness/witness_grammar.main": uswForgedSuccessor,
  "fraud_proofs/unused_script_witness/witness_scan.main": uswForgedSuccessor,
  "fraud_proofs/unused_script_witness/terminal.main": {
    passing: [
      {
        file: USW_VERTICALS,
        test: "A-N: convicts an accepted transaction carrying a witness no purpose executes and removes its block",
      },
    ],
    failing: [
      {
        file: USW_VERTICALS,
        test: "refuses $name: the terminal returns false",
      },
    ],
  },
};

/** The family's scenario pair: an A-N conviction and the terminal refusals. */
export const UNUSED_SCRIPT_WITNESS_FAMILY_SCENARIOS: ValidatorScenarioPair =
  UNUSED_SCRIPT_WITNESS_SCENARIOS[
    "fraud_proofs/unused_script_witness/terminal.main"
  ]!;
