import { VALIDATION_TRACE_SCENARIOS } from "./validator-scenario-registry.validation-trace-scenarios.js";

export type ValidatorScenario = Readonly<{ file: string; test: string }>;

export type ValidatorScenarioPair = Readonly<{
  passing: readonly ValidatorScenario[];
  failing: readonly ValidatorScenario[];
}>;

/** One emulator test both executes the honest transition and refuses an
 * altered one at the same validator. */
const directScriptDerivationScenario = (
  test: string,
): ValidatorScenarioPair => {
  const scenario = [
    {
      file: "demo/midgard-fault-proofs/tests/direct-script-derivation-emulator.test.ts",
      test,
    },
  ];
  return { passing: scenario, failing: scenario };
};
const DIRECT_BINDING =
  "executes separate direct event and pre-state custody steps for %s";
const DIRECT_SOURCE =
  "executes source field-authentication-map custody in ordinary eight-item batches";
const DIRECT_RECEIVE =
  "executes bounded receive batches and sorted ordinal custody without trace states";
const MR_FORGED_SUCCESSOR =
  "demo/midgard-fault-proofs/tests/submit-init-emulator-missing-redeemer-forged-successor.test.ts";
const missingRedeemerForgedSuccessor = (
  test: string,
): ValidatorScenarioPair => {
  const scenario = [{ file: MR_FORGED_SUCCESSOR, test }];
  return { passing: scenario, failing: scenario };
};
const MR_OBSERVER =
  "refuses a forged successor at every role of the accepted observer route";
const MR_CLAIMED =
  "refuses a forged successor at every role of the forced claimed-redeemer route";
const MSS_FORGED_SUCCESSOR =
  "demo/midgard-fault-proofs/tests/submit-init-emulator-missing-script-source-forged-successor.test.ts";
const MSS_OBSERVER =
  "refuses a forged successor at every role of the accepted observer route";
const missingScriptSourceForgedSuccessor: ValidatorScenarioPair = {
  passing: [{ file: MSS_FORGED_SUCCESSOR, test: MSS_OBSERVER }],
  failing: [{ file: MSS_FORGED_SUCCESSOR, test: MSS_OBSERVER }],
};
const MR_VERTICALS =
  "demo/midgard-fault-proofs/tests/submit-init-emulator-missing-redeemer-verticals.test.ts";
const streamedReplayStep = (failing: string): ValidatorScenarioPair => ({
  passing: [
    {
      file: STREAMED_REPLAY_FILE,
      test: "convicts a wrong transition root with every streamed step honest",
    },
  ],
  failing: [{ file: STREAMED_REPLAY_FILE, test: failing }],
});

const CLASS_STAKERS_FILE =
  "demo/midgard-fault-proofs/tests/script-decode-class-stakers.test.ts";

/** Each class staker's rows step its class honestly and refuse a forged segment. */
const classStaker: ValidatorScenarioPair = {
  passing: [
    {
      file: CLASS_STAKERS_FILE,
      test: "the %s staker steps its class honestly and refuses a segment claiming a machine it does not reach",
    },
  ],
  failing: [
    {
      file: CLASS_STAKERS_FILE,
      test: "the %s staker steps its class honestly and refuses a segment claiming a machine it does not reach",
    },
  ],
};

const STREAMED_REPLAY_FILE =
  "demo/midgard-fault-proofs/tests/submit-init-emulator-transition-trace-final-forged-steps.test.ts";

export const VALIDATOR_SCENARIOS: Readonly<
  Record<string, ValidatorScenarioPair>
> = {
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
  "state_queue.spend": {
    passing: [
      {
        file: "demo/midgard-fault-proofs/tests/submit-init-emulator-record-completed-fraud-by-reference.test.ts",
        test: "lets any wallet record, leaving the cited token and the node value in place",
      },
    ],
    failing: [
      {
        file: "demo/midgard-fault-proofs/tests/submit-init-emulator-record-completed-fraud-by-reference.test.ts",
        test: "refuses a cited token that names another header",
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
  "fraud_proofs/redeemer_canonicity/step_02_data.main": {
    passing: [
      {
        file: "demo/midgard-fault-proofs/tests/redeemer-canonicity-streamed-data.test.ts",
        test: "convicts the acceptance of redeemer data whose only flaw lies past its first 30,000 bytes",
      },
    ],
    failing: [
      {
        file: "demo/midgard-fault-proofs/tests/redeemer-canonicity-streamed-data.test.ts",
        test: "refuses a false acceptance claim against canonical redeemer data nested 4,000 lists deep, and every forged data step on the way",
      },
    ],
  },
  "fraud_proofs/resolved_output_non_canonical/step_04_datum.main": {
    passing: [
      {
        file: "demo/midgard-fault-proofs/tests/resolved-output-non-canonical-datum-lifecycle.test.ts",
        test: "convicts a forced rejection of maximum outputs whose inline datums are the densest canonical Data, reading each to its verdict, and measures the costliest datum step",
      },
    ],
    failing: [
      {
        file: "demo/midgard-fault-proofs/tests/resolved-output-non-canonical-datum-lifecycle.test.ts",
        test: "convicts the acceptance of an output whose inline datum is not canonical under a canonical envelope, refusing forged datum steps, and cancels a thread parked on the datum step",
      },
    ],
  },
  "fraud_proofs/transaction_output_non_canonical/step_03_datum.main": {
    passing: [
      {
        file: "demo/midgard-fault-proofs/tests/transaction-output-non-canonical-datum-lifecycle.test.ts",
        test: "convicts a forced rejection of a canonical inline datum that ends at the output's last byte on a chunk boundary, then mints and removes",
      },
    ],
    failing: [
      {
        file: "demo/midgard-fault-proofs/tests/transaction-output-non-canonical-datum-lifecycle.test.ts",
        test: "upholds an honest forced rejection of a non-canonical inline datum read across chunk boundaries and transactions, refusing every forged datum checkpoint, the early verdicts and the proof mint",
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
  "fraud_proofs/transition_trace/source_v1.main": {
    passing: [
      {
        file: "demo/midgard-fault-proofs/tests/submit-init-emulator-transition-trace-source-phase.test.ts",
        test: "proves an L2 transaction stepped under the forced phase and removes the block",
      },
    ],
    failing: [
      {
        file: "demo/midgard-fault-proofs/tests/submit-init-emulator-transition-trace-source-phase.test.ts",
        test: "refuses the same source-phase witness against an honest step",
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
  "fraud_proofs/direct_script_derivation/bind_event.main":
    directScriptDerivationScenario(DIRECT_BINDING),
  "fraud_proofs/direct_script_derivation/bind_pre_state.main":
    directScriptDerivationScenario(DIRECT_BINDING),
  "fraud_proofs/direct_script_derivation/source_start.main":
    directScriptDerivationScenario(DIRECT_SOURCE),
  "fraud_proofs/direct_script_derivation/source_resolved.main":
    directScriptDerivationScenario(DIRECT_SOURCE),
  "fraud_proofs/direct_script_derivation/source_authenticate.main":
    directScriptDerivationScenario(DIRECT_SOURCE),
  "fraud_proofs/direct_script_derivation/source_map.main":
    directScriptDerivationScenario(DIRECT_SOURCE),
  "fraud_proofs/direct_script_derivation/source_input_finish.main":
    directScriptDerivationScenario(DIRECT_SOURCE),
  "fraud_proofs/direct_script_derivation/source_grammar.main":
    directScriptDerivationScenario(DIRECT_SOURCE),
  "fraud_proofs/direct_script_derivation/source_witnesses.main":
    directScriptDerivationScenario(DIRECT_SOURCE),
  "fraud_proofs/direct_script_derivation/source_finish.main":
    directScriptDerivationScenario(DIRECT_SOURCE),
  "fraud_proofs/direct_script_derivation/receive_start.main":
    directScriptDerivationScenario(DIRECT_RECEIVE),
  "fraud_proofs/direct_script_derivation/receive_grammar.main":
    directScriptDerivationScenario(DIRECT_RECEIVE),
  "fraud_proofs/direct_script_derivation/receive_outputs.main":
    directScriptDerivationScenario(DIRECT_RECEIVE),
  "fraud_proofs/direct_script_derivation/receive_finish.main":
    directScriptDerivationScenario(DIRECT_RECEIVE),
  "fraud_proofs/direct_script_derivation/receive_declaration.main":
    directScriptDerivationScenario(DIRECT_RECEIVE),
  "fraud_proofs/direct_script_derivation/purpose_select.main":
    missingRedeemerForgedSuccessor(MR_OBSERVER),
  "fraud_proofs/direct_script_derivation/source_lookup.main":
    missingRedeemerForgedSuccessor(MR_OBSERVER),
  "fraud_proofs/missing_redeemer/family_entry.main":
    missingRedeemerForgedSuccessor(MR_OBSERVER),
  "fraud_proofs/missing_script_source/family_entry.main":
    missingScriptSourceForgedSuccessor,
  "fraud_proofs/missing_script_source/observed_decision.main":
    missingScriptSourceForgedSuccessor,
  "fraud_proofs/missing_redeemer/step_02.main":
    missingRedeemerForgedSuccessor(MR_OBSERVER),
  "fraud_proofs/missing_redeemer/step_03.main":
    missingRedeemerForgedSuccessor(MR_OBSERVER),
  "fraud_proofs/missing_redeemer/step_04.main":
    missingRedeemerForgedSuccessor(MR_OBSERVER),
  "fraud_proofs/direct_script_derivation/claim_start.main":
    missingRedeemerForgedSuccessor(MR_OBSERVER),
  "fraud_proofs/direct_script_derivation/claim_adjudicate.main":
    missingRedeemerForgedSuccessor(MR_OBSERVER),
  "fraud_proofs/direct_script_derivation/claimed_redeemer_grammar.main":
    missingRedeemerForgedSuccessor(MR_CLAIMED),
  "fraud_proofs/direct_script_derivation/claimed_redeemer_scan.main":
    missingRedeemerForgedSuccessor(MR_CLAIMED),
  "fraud_proofs/direct_script_derivation/claim_terminal.main": {
    passing: [
      {
        file: MR_VERTICALS,
        test: "A-N: convicts an accepted transaction whose required observer has no redeemer and removes its block",
      },
    ],
    failing: [
      {
        file: MR_VERTICALS,
        test: "R-F: refuses a forced rejection whose claimed observer redeemer is absent",
      },
    ],
  },
  "script_decode_v1.script_decode_v1": {
    passing: [
      {
        file: "demo/midgard-fault-proofs/tests/script-decode-record.test.ts",
        test: "records a decodable script once and finds it afterwards",
      },
      {
        file: "demo/midgard-fault-proofs/tests/script-decode-record.test.ts",
        test: "refuses a forged run, Walk, or record and accepts the honest one",
      },
      {
        file: "demo/midgard-fault-proofs/tests/output-reference-script-decoding-plutus-max-shape.test.ts",
        test: "records the densest Plutus reference script of a 16,384-byte output after Init and convicts its acceptance, the producer's phase-8 first fault, through removal",
      },
    ],
    failing: [
      {
        file: "demo/midgard-fault-proofs/tests/script-decode-record.test.ts",
        test: "records a decodable script once and finds it afterwards",
      },
      {
        file: "demo/midgard-fault-proofs/tests/script-decode-record.test.ts",
        test: "refuses a forged run, Walk, or record and accepts the honest one",
      },
    ],
  },
  "script_decode_v1.script_decode_bytes_v1": classStaker,
  "script_decode_v1.script_decode_data_v1": classStaker,
  "script_decode_v1.script_decode_fold_v1": classStaker,
  "script_decode_v1.script_decode_list_v1": classStaker,
  "script_decode_v1.script_decode_term_v1": classStaker,
  "script_decode_v1.script_decode_value_v1": classStaker,
  ...VALIDATION_TRACE_SCENARIOS,
  "fraud_proofs/transition_trace/accepted_transaction_yields.l2_delete":
    streamedReplayStep(
      "refuses an input delete step claiming a post-root its witness does not give",
    ),
  "fraud_proofs/transition_trace/accepted_transaction_yields.l2_outputs":
    streamedReplayStep(
      "refuses an outputs certification step claiming one output more than the field holds",
    ),
  "fraud_proofs/transition_trace/accepted_transaction_yields.l2_output_step":
    streamedReplayStep(
      "refuses an output step claiming a post-root its insert does not give",
    ),
  "fraud_proofs/transition_trace/output_step.descriptor": streamedReplayStep(
    "refuses an output step inserting a descriptor its output does not derive",
  ),
  "fraud_proofs/transition_trace/output_step.summary": streamedReplayStep(
    "refuses an output step whose summaries and descriptor are forged together",
  ),
};
