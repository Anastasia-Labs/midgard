import type { ValidatorScenarioPair } from "./validator-scenario-registry.validator-scenarios.js";

const ENSI_FORGED_SUCCESSOR =
  "demo/midgard-fault-proofs/tests/submit-init-emulator-execution-native-script-invalid-forged-successor.test.ts";
const ensiForgedSuccessor = (test: string): ValidatorScenarioPair => {
  const scenario = [{ file: ENSI_FORGED_SUCCESSOR, test }];
  return { passing: scenario, failing: scenario };
};
const ENSI_OBSERVER =
  "refuses a forged successor at every role of a forced acceptance whose observer's native script is false";
const ENSI_KEY_SPEND =
  "refuses a forged successor at every role of a rejection recorded at a key-locked spend";
const ENSI_VERTICALS =
  "demo/midgard-fault-proofs/tests/submit-init-emulator-execution-native-script-invalid-verticals.test.ts";
const ESSD_VERTICALS =
  "demo/midgard-fault-proofs/tests/submit-init-emulator-execution-source-script-decoding-verticals.test.ts";

/**
 * The execution-native-script-invalid and execution-source-script-decoding
 * validators' scenarios: the native family's entry, source lookup, signer,
 * evaluation and spend roles, which forged successor rows refuse at every
 * role, and each family's terminal. `VALIDATOR_SCENARIOS` spreads them in.
 */
export const EXECUTION_NATIVE_SCRIPT_SCENARIOS: Readonly<
  Record<string, ValidatorScenarioPair>
> = {
  "fraud_proofs/execution_native_script_invalid/family_entry.main":
    ensiForgedSuccessor(ENSI_OBSERVER),
  "fraud_proofs/execution_native_script_invalid/lookup.main":
    ensiForgedSuccessor(ENSI_OBSERVER),
  "fraud_proofs/execution_native_script_invalid/signers.main":
    ensiForgedSuccessor(ENSI_OBSERVER),
  "fraud_proofs/execution_native_script_invalid/evaluate.main":
    ensiForgedSuccessor(ENSI_OBSERVER),
  "fraud_proofs/execution_native_script_invalid/spend_key.main":
    ensiForgedSuccessor(ENSI_KEY_SPEND),
  "fraud_proofs/execution_native_script_invalid/spend_rank.main":
    ensiForgedSuccessor(ENSI_KEY_SPEND),
  "fraud_proofs/execution_native_script_invalid/terminal.main": {
    passing: [
      {
        file: ENSI_VERTICALS,
        test: "A-N: convicts an accepted transaction whose observer's native script is not yet valid and removes its block",
      },
    ],
    failing: [
      {
        file: ENSI_VERTICALS,
        test: "refuses $name: the terminal returns false",
      },
    ],
  },
  "fraud_proofs/execution_source_script_decoding/terminal.main": {
    passing: [
      {
        file: ESSD_VERTICALS,
        test: "R-F: convicts a forced rejection recording %s",
      },
    ],
    failing: [
      {
        file: ESSD_VERTICALS,
        test: "refuses $name: the terminal returns false",
      },
    ],
  },
};

/** Each family's scenario pair: its convictions and terminal refusals. */
export const EXECUTION_NATIVE_SCRIPT_INVALID_FAMILY_SCENARIOS: ValidatorScenarioPair =
  EXECUTION_NATIVE_SCRIPT_SCENARIOS[
    "fraud_proofs/execution_native_script_invalid/terminal.main"
  ]!;
export const EXECUTION_SOURCE_SCRIPT_DECODING_FAMILY_SCENARIOS: ValidatorScenarioPair =
  EXECUTION_NATIVE_SCRIPT_SCENARIOS[
    "fraud_proofs/execution_source_script_decoding/terminal.main"
  ]!;
