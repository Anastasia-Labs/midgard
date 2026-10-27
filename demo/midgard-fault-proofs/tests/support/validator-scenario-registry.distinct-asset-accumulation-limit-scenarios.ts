import type { ValidatorScenarioPair } from "./validator-scenario-registry.validator-scenarios.js";

const DAAL_FORGED_SUCCESSOR =
  "demo/midgard-fault-proofs/tests/submit-init-emulator-distinct-asset-accumulation-limit-forged-successor.test.ts";
const daalForgedSuccessor: ValidatorScenarioPair = {
  passing: [
    {
      file: DAAL_FORGED_SUCCESSOR,
      test: "refuses a forged successor at every role of an accepted transaction",
    },
  ],
  failing: [
    {
      file: DAAL_FORGED_SUCCESSOR,
      test: "refuses a forged successor at every role of an accepted transaction",
    },
  ],
};
const DAAL_VERTICALS =
  "demo/midgard-fault-proofs/tests/submit-init-emulator-distinct-asset-accumulation-limit-verticals.test.ts";
/**
 * The distinct-asset-accumulation-limit validators' scenarios: the family's
 * entry and fold roles, which a forged successor row refuses at every role,
 * and its terminal. `VALIDATOR_SCENARIOS` spreads them in.
 */
export const DISTINCT_ASSET_ACCUMULATION_LIMIT_SCENARIOS: Readonly<
  Record<string, ValidatorScenarioPair>
> = {
  "fraud_proofs/distinct_asset_accumulation_limit/family_entry.main":
    daalForgedSuccessor,
  "fraud_proofs/distinct_asset_accumulation_limit/inputs.main":
    daalForgedSuccessor,
  "fraud_proofs/distinct_asset_accumulation_limit/inputs_resolve.main":
    daalForgedSuccessor,
  "fraud_proofs/distinct_asset_accumulation_limit/input_rank.main":
    daalForgedSuccessor,
  "fraud_proofs/distinct_asset_accumulation_limit/outputs_grammar.main":
    daalForgedSuccessor,
  "fraud_proofs/distinct_asset_accumulation_limit/outputs.main":
    daalForgedSuccessor,
  "fraud_proofs/distinct_asset_accumulation_limit/mint_grammar.main":
    daalForgedSuccessor,
  "fraud_proofs/distinct_asset_accumulation_limit/mint.main":
    daalForgedSuccessor,
  "fraud_proofs/distinct_asset_accumulation_limit/terminal.main": {
    passing: [
      {
        file: DAAL_VERTICALS,
        test: "A-N: convicts an accepted transaction crossing in its inputs and removes its block",
      },
    ],
    failing: [
      {
        file: DAAL_VERTICALS,
        test: "refuses $name: the terminal returns false",
      },
    ],
  },
};

/** The family's scenario pair: an A-N conviction and the terminal refusals. */
export const DISTINCT_ASSET_ACCUMULATION_LIMIT_FAMILY_SCENARIOS: ValidatorScenarioPair =
  DISTINCT_ASSET_ACCUMULATION_LIMIT_SCENARIOS[
    "fraud_proofs/distinct_asset_accumulation_limit/terminal.main"
  ]!;
