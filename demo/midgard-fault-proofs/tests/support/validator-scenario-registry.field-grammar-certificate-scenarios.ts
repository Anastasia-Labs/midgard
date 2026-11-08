import type { ValidatorScenarioPair } from "./validator-scenario-registry.validator-scenarios.js";

const FIELD_GRAMMAR_CERTIFICATE_EMULATOR =
  "demo/midgard-fault-proofs/tests/field-grammar-certificate-emulator.test.ts";

/**
 * The field grammar certificate's scenarios: the builder opens, advances,
 * resumes and reclaims certificates through the deployed validator, which
 * refuses an open whose datum claims items its batch did not walk.
 * `VALIDATOR_SCENARIOS` spreads them in.
 */
export const FIELD_GRAMMAR_CERTIFICATE_SCENARIOS: Readonly<
  Record<string, ValidatorScenarioPair>
> = {
  "field_grammar_certificate.field_grammar_certificate": {
    passing: [
      {
        file: FIELD_GRAMMAR_CERTIFICATE_EMULATOR,
        test: "opens, advances and completes a certificate, then reuses it",
      },
      {
        file: FIELD_GRAMMAR_CERTIFICATE_EMULATOR,
        test: "resumes its own InProgress certificate from the item it reached",
      },
    ],
    failing: [
      {
        file: FIELD_GRAMMAR_CERTIFICATE_EMULATOR,
        test: "refuses an open whose certificate claims items its batch did not walk",
      },
    ],
  },
};
