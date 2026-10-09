export type ValidatorScenario = Readonly<{ file: string; test: string }>;

export type ValidatorScenarioPair = Readonly<{
  passing: readonly ValidatorScenario[];
  failing: readonly ValidatorScenario[];
}>;

export const VALIDATOR_SCENARIOS: Readonly<
  Record<string, ValidatorScenarioPair>
> = {
  "fraud_proofs/validation_trace/script_sources_stage_one_redeemer_envelope_v1.main":
    {
      passing: [
        {
          file: "demo/midgard-fault-proofs/tests/redeemer-data-refusal-deployed.test.ts",
          test: "settles authenticated Data refusals through the deployed ScriptSources item chain",
        },
      ],
      failing: [
        {
          file: "demo/midgard-fault-proofs/tests/redeemer-data-refusal-deployed.test.ts",
          test: "settles authenticated Data refusals through the deployed ScriptSources item chain",
        },
      ],
    },
  "fraud_proofs/validation_trace/script_sources_stage_one_redeemer_traversal_normalizer_v1.main":
    {
      passing: [
        {
          file: "demo/midgard-fault-proofs/tests/redeemer-data-refusal-deployed.test.ts",
          test: "settles authenticated Data refusals through the deployed ScriptSources item chain",
        },
      ],
      failing: [
        {
          file: "demo/midgard-fault-proofs/tests/redeemer-data-refusal-deployed.test.ts",
          test: "settles authenticated Data refusals through the deployed ScriptSources item chain",
        },
      ],
    },
  "fraud_proofs/validation_trace/script_sources_stage_one_redeemer_outer_normalizer_v1.main":
    {
      passing: [
        {
          file: "demo/midgard-fault-proofs/tests/redeemer-data-refusal-deployed.test.ts",
          test: "settles authenticated Data refusals through the deployed ScriptSources item chain",
        },
      ],
      failing: [
        {
          file: "demo/midgard-fault-proofs/tests/redeemer-data-refusal-deployed.test.ts",
          test: "settles authenticated Data refusals through the deployed ScriptSources item chain",
        },
      ],
    },
  "fraud_proofs/validation_trace/script_sources_stage_one_redeemer_source_authenticator.main":
    {
      passing: [
        {
          file: "demo/midgard-fault-proofs/tests/redeemer-data-refusal-deployed.test.ts",
          test: "settles authenticated Data refusals through the deployed ScriptSources item chain",
        },
      ],
      failing: [
        {
          file: "demo/midgard-fault-proofs/tests/redeemer-data-refusal-deployed.test.ts",
          test: "settles authenticated Data refusals through the deployed ScriptSources item chain",
        },
      ],
    },
  "fraud_proofs/validation_trace/script_sources_stage_one_redeemer_execution_settlement_v1.main":
    {
      passing: [
        {
          file: "demo/midgard-fault-proofs/tests/redeemer-data-refusal-deployed.test.ts",
          test: "settles authenticated Data refusals through the deployed ScriptSources item chain",
        },
      ],
      failing: [
        {
          file: "demo/midgard-fault-proofs/tests/redeemer-data-refusal-deployed.test.ts",
          test: "settles authenticated Data refusals through the deployed ScriptSources item chain",
        },
      ],
    },
  "fraud_proofs/validation_trace/script_sources_stage_one_redeemer_invalid_data_executor.main":
    {
      passing: [
        {
          file: "demo/midgard-fault-proofs/tests/redeemer-data-refusal-deployed.test.ts",
          test: "settles authenticated Data refusals through the deployed ScriptSources item chain",
        },
      ],
      failing: [
        {
          file: "demo/midgard-fault-proofs/tests/redeemer-data-refusal-deployed.test.ts",
          test: "refuses canonical Data at the deployed invalid executor",
        },
      ],
    },
  "fraud_proofs/validation_trace/boundary_v1.main": {
    passing: [
      {
        file: "demo/midgard-fault-proofs/tests/submit-init-emulator-validation-dispute-terminal-padding.test.ts",
        test: "awards an authenticated padded terminal run and refuses a forged terminal opening",
      },
    ],
    failing: [
      {
        file: "demo/midgard-fault-proofs/tests/submit-init-emulator-validation-dispute-terminal-padding.test.ts",
        test: "refuses terminal-padding awards against an honest operator trace",
      },
    ],
  },
  "operator_directory/active_operators.spend": {
    passing: [
      {
        file: "demo/midgard-node/tests/operator-exit-emulator.test.ts",
        test: "retires an unscheduled operator and returns the bond on recovery",
      },
      {
        file: "demo/midgard-node/tests/operator-exit-emulator.test.ts",
        test: "retires a struck operator without its signature, paying exactly the inactivity penalty",
      },
    ],
    failing: [
      {
        file: "demo/midgard-node/tests/operator-exit-emulator.test.ts",
        test: "refuses a retirement the operator did not sign, and a forced retirement below the strike limit",
      },
    ],
  },
  "operator_directory/retired_operators.spend": {
    passing: [
      {
        file: "demo/midgard-node/tests/operator-exit-emulator.test.ts",
        test: "retires an unscheduled operator and returns the bond on recovery",
      },
    ],
    failing: [
      {
        file: "demo/midgard-node/tests/operator-exit-emulator.test.ts",
        test: "refuses bond recovery that the retired operator did not sign",
      },
    ],
  },
  "operator_directory/registered_operators.spend": {
    passing: [
      {
        file: "demo/midgard-node/tests/operator-exit-emulator.test.ts",
        test: "slashes duplicate registrations of an active and then a retired operator",
      },
    ],
    failing: [
      {
        file: "demo/midgard-node/tests/operator-exit-emulator.test.ts",
        test: "slashes a duplicate registration proved by another registration, and refuses a non-duplicate or a wrong fee",
      },
    ],
  },
  // Each lifecycle test runs the honest transaction and, beside it, one the
  // pool refuses; the test asserts the refusal came from the pool spend.
  "da_bond_pool.da_bond_pool": {
    passing: [
      {
        file: "demo/midgard-node/tests/da-bond-pool-lifecycle.test.ts",
        test: "runs the real InitPool, then a non-owner top-up at exactly the minimum; a top-up one lovelace below it is refused by the pool spend",
      },
      {
        file: "demo/midgard-node/tests/da-bond-pool-lifecycle.test.ts",
        test: "withdraws through Begin, Cancel, Begin and Complete at unlock_at; Complete one slot before unlock_at is refused by the pool spend",
      },
    ],
    failing: [
      {
        file: "demo/midgard-node/tests/da-bond-pool-lifecycle.test.ts",
        test: "runs the real InitPool, then a non-owner top-up at exactly the minimum; a top-up one lovelace below it is refused by the pool spend",
      },
      {
        file: "demo/midgard-node/tests/da-bond-pool-lifecycle.test.ts",
        test: "withdraws through Begin, Cancel, Begin and Complete at unlock_at; Complete one slot before unlock_at is refused by the pool spend",
      },
    ],
  },
  "state_queue_yields.remove_unattested": {
    passing: [
      {
        file: "demo/midgard-fault-proofs/tests/unattested-timeout-suffix-lifecycle.test.ts",
        test: "removes an expired tail while retaining its immature attested predecessor and root",
      },
    ],
    failing: [
      {
        file: "demo/midgard-fault-proofs/tests/unattested-timeout-suffix-lifecycle.test.ts",
        test: "refuses premature and already-attested targets in the applied validators",
      },
    ],
  },
  "fraud_proofs/invalid_signature/step_02.main": {
    passing: [
      {
        file: "demo/midgard-fault-proofs/tests/submit-init-emulator-invalid-signature-lifecycle.test.ts",
        test: "convicts an invalid address witness end to end, mints the permanent fraud-proof token, and removes the fraudulent commitment",
      },
    ],
    failing: [
      {
        file: "demo/midgard-fault-proofs/tests/submit-init-emulator-invalid-signature-lifecycle.test.ts",
        test: "refuses an attack on an honest commitment at step-02's on-chain Ed25519 check",
      },
    ],
  },
  "fraud_proofs/committed_field_shape/step_01.main": {
    passing: [
      {
        file: "demo/midgard-fault-proofs/tests/submit-init-emulator-committed-field-shape.test.ts",
        test: "proves a real wrong-stride commitment through mint and removes its block",
      },
    ],
    failing: [
      {
        file: "demo/midgard-fault-proofs/tests/submit-init-emulator-committed-field-shape-adversarial.test.ts",
        test: "refuses fabricated verdict and uncommitted bytes against an honest commitment at step-01",
      },
    ],
  },
  "fraud_proofs/committed_field_shape/step_02.main": {
    passing: [
      {
        file: "demo/midgard-fault-proofs/tests/submit-init-emulator-committed-field-shape.test.ts",
        test: "proves a real wrong-stride commitment through mint and removes its block",
      },
    ],
    failing: [
      {
        file: "demo/midgard-fault-proofs/tests/submit-init-emulator-committed-field-shape-adversarial.test.ts",
        test: "binds a committed non-envelope but refuses it at the exact step-02 predicate",
      },
    ],
  },
  "fraud_proofs/value_not_preserved/step_04.main": {
    passing: [
      {
        file: "demo/midgard-fault-proofs/tests/submit-init-emulator-value-not-preserved-token.test.ts",
        test: "proves an inflated token end to end, mints the permanent fraud-proof token, and removes the fraudulent commitment",
      },
    ],
    failing: [
      {
        file: "demo/midgard-fault-proofs/tests/submit-init-emulator-value-not-preserved-adversarial.test.ts",
        test: "never finalizes against a balanced honest commitment: step-04 refuses the zero delta locally and on-chain",
      },
    ],
  },
  "fraud_proofs/missing_signature/step_04.main": {
    passing: [
      {
        file: "demo/midgard-fault-proofs/tests/submit-init-emulator-missing-signature-lifecycle.test.ts",
        test: "proves through the core, refuses a duplicate proof, and removes/slashes the fraudulent block",
      },
    ],
    failing: [
      {
        file: "demo/midgard-fault-proofs/tests/submit-init-emulator-missing-signature-adversarial.test.ts",
        test: "refuses every honest-path local forgery and rejects the guard-bypassing conviction at step-04 on-chain",
      },
    ],
  },
  "fraud_proofs/transaction_output_non_canonical/step_04.main": {
    passing: [
      {
        file: "demo/midgard-fault-proofs/tests/transaction-output-non-canonical-lifecycle.test.ts",
        test: "convicts an accepted malformed output at the maximum shape, refuses every accepted seam, the honest twin and the adjacent width, cancels every step, then mints and removes",
      },
    ],
    failing: [
      {
        file: "demo/midgard-fault-proofs/tests/transaction-output-non-canonical-lifecycle.test.ts",
        test: "refuses to mint against an honest forced rejection: the malformed output reaches its non-canonical terminal and step 04 refuses on chain",
      },
    ],
  },
  "fraud_proofs/withdrawn_reference_input/step_03.main": {
    passing: [
      {
        file: "demo/midgard-fault-proofs/tests/submit-init-emulator-withdrawn-reference-input-lifecycle.test.ts",
        test: "proves the same-block conflict, mints permanent evidence, and removes the fraudulent block",
      },
    ],
    failing: [
      {
        file: "demo/midgard-fault-proofs/tests/submit-init-emulator-withdrawn-reference-input-adversarial.test.ts",
        test: "refuses both different-outref roads at the exact step-03 checks",
      },
    ],
  },
  "fraud_proofs/min_fee/step_02.main": {
    passing: [
      {
        file: "demo/midgard-fault-proofs/tests/submit-init-emulator-min-fee.test.ts",
        test: "cancels both steps, resumes the same thread, rejects malformed evidence, mints, and removes",
      },
    ],
    failing: [
      {
        file: "demo/midgard-fault-proofs/tests/submit-init-emulator-min-fee.test.ts",
        test: "reaches step-02 and lets the compiled validator refuse an honest exact fee",
      },
    ],
  },
  "fraud_proofs/field_item_width_illegal/step_02.main": {
    passing: [
      {
        file: "demo/midgard-fault-proofs/tests/field-item-width-illegal-lifecycle.test.ts",
        test: "convicts the widest accepted output a maximum field can carry: cancels every step, refuses every step-02 seam, the adjacent item coordinate and the honest bound, then mints and removes",
      },
    ],
    failing: [
      {
        file: "demo/midgard-fault-proofs/tests/field-item-width-illegal-lifecycle.test.ts",
        test: "convicts the widest accepted output a maximum field can carry: cancels every step, refuses every step-02 seam, the adjacent item coordinate and the honest bound, then mints and removes",
      },
    ],
  },
  "fraud_proofs/receive_purpose_language/step_02.main": {
    passing: [
      {
        file: "demo/midgard-fault-proofs/tests/receive-purpose-language-lifecycle.test.ts",
        test: "convicts an accepted PlutusV3 receive at the maximum shape: cancels every step, refuses every step-02 seam and the adjacent index, then mints and removes through the actuator",
      },
    ],
    failing: [
      {
        file: "demo/midgard-fault-proofs/tests/receive-purpose-language-lifecycle.test.ts",
        test: "convicts an accepted PlutusV3 receive at the maximum shape: cancels every step, refuses every step-02 seam and the adjacent index, then mints and removes through the actuator",
      },
    ],
  },
  "fraud_proofs/transition_trace/source_v1.main": {
    passing: [
      {
        file: "demo/midgard-fault-proofs/tests/submit-init-emulator-transition-trace-validation-runs.test.ts",
        test: "proves missing validation run and removes the block",
      },
      {
        file: "demo/midgard-fault-proofs/tests/submit-init-emulator-transition-trace-validation-runs.test.ts",
        test: "proves foreign validation run and removes the block",
      },
      {
        file: "demo/midgard-fault-proofs/tests/submit-init-emulator-transition-trace-validation-runs.test.ts",
        test: "proves malformed validation run and removes the block",
      },
    ],
    failing: [
      {
        file: "demo/midgard-fault-proofs/tests/submit-init-emulator-transition-trace-validation-runs.test.ts",
        test: "refuses missing validation-run claim against the honest block",
      },
      {
        file: "demo/midgard-fault-proofs/tests/submit-init-emulator-transition-trace-validation-runs.test.ts",
        test: "refuses foreign validation-run claim against the honest block",
      },
      {
        file: "demo/midgard-fault-proofs/tests/submit-init-emulator-transition-trace-validation-runs.test.ts",
        test: "refuses malformed validation-run claim against the honest block",
      },
    ],
  },
  "fraud_proofs/transition_trace/withdrawal_v1.main": {
    passing: [
      {
        file: "demo/midgard-fault-proofs/tests/submit-init-emulator-transition-trace-withdrawal-branch.test.ts",
        test: "convicts a wrong withdrawal step with an honest delete that opens a lone neighbour group",
      },
      {
        file: "demo/midgard-fault-proofs/tests/submit-init-emulator-transition-trace-withdrawal-branch.test.ts",
        test: "convicts a step that commits the all-zero root for the empty ledger",
      },
    ],
    failing: [
      {
        file: "demo/midgard-fault-proofs/tests/submit-init-emulator-transition-trace-withdrawal-branch.test.ts",
        test: "defends an honest withdrawal step against a Branch standing in for the spent output's lone neighbour",
      },
      {
        file: "demo/midgard-fault-proofs/tests/submit-init-emulator-transition-trace-withdrawal-branch.test.ts",
        test: "defends an honest withdrawal step against a terminal Leaf whose skipped nibble is rewritten",
      },
      {
        file: "demo/midgard-fault-proofs/tests/submit-init-emulator-transition-trace-withdrawal-branch.test.ts",
        test: "defends an honest step that commits the empty ledger root",
      },
    ],
  },
};
