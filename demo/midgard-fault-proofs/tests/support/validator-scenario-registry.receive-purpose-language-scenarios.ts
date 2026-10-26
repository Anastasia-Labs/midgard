import type { ValidatorScenarioPair } from "./validator-scenario-registry.validator-scenarios.js";

const RPL_FORGED_SUCCESSOR =
  "demo/midgard-fault-proofs/tests/submit-init-emulator-receive-purpose-language-forged-successor.test.ts";
const RPL_ACCEPTED_ROUTE =
  "refuses a forged successor at every role of the accepted forbidden-execution route";
const rplForgedSuccessor: ValidatorScenarioPair = {
  passing: [{ file: RPL_FORGED_SUCCESSOR, test: RPL_ACCEPTED_ROUTE }],
  failing: [{ file: RPL_FORGED_SUCCESSOR, test: RPL_ACCEPTED_ROUTE }],
};
const RPL_VERTICALS =
  "demo/midgard-fault-proofs/tests/submit-init-emulator-receive-purpose-language-verticals.test.ts";
/**
 * The receive-purpose-language validators' scenarios: the family's entry and
 * lookup, which a forged successor row refuses at every role of the accepted
 * route, and its terminal. `VALIDATOR_SCENARIOS` spreads them in.
 */
export const RECEIVE_PURPOSE_LANGUAGE_SCENARIOS: Readonly<
  Record<string, ValidatorScenarioPair>
> = {
  "fraud_proofs/receive_purpose_language/family_entry.main": rplForgedSuccessor,
  "fraud_proofs/receive_purpose_language/lookup.main": rplForgedSuccessor,
  "fraud_proofs/receive_purpose_language/terminal.main": {
    passing: [
      {
        file: RPL_VERTICALS,
        test: "A-N: convicts an accepted transaction executing a PlutusV3 receive purpose with a redeemer and removes its block",
      },
    ],
    failing: [
      {
        file: RPL_VERTICALS,
        test: "refuses an accepted transaction's native receive purpose with a redeemer",
      },
    ],
  },
};

/** The family's scenario pair: an A-N conviction and a terminal refusal. */
export const RECEIVE_PURPOSE_LANGUAGE_FAMILY_SCENARIOS: ValidatorScenarioPair =
  RECEIVE_PURPOSE_LANGUAGE_SCENARIOS[
    "fraud_proofs/receive_purpose_language/terminal.main"
  ]!;
