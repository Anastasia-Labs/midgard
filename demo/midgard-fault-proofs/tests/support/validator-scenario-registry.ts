/**
 * Which lucid-evolution emulator scenarios prove each validator in both
 * polarities: a passing scenario where the deployed, parameterized validator
 * accepts, and a failing one where it must refuse
 * (docs/agents/contracts.md#scenario-coverage).
 *
 * Two key spaces: every validator in the freshly built blueprint
 * (`<module>.<validator>`, the handler suffix dropped) and every family in
 * `FAMILY_APPLICATION_REGISTRY`. `validator-scenario-registry.test.ts` fails
 * when either has a key that is neither mapped here nor listed as unmapped
 * with a reason, when a mapping names a test that does not exist, and when
 * the unmapped lists change size without the pinned counts changing with
 * them.
 *
 * A scenario is a test file (repository-relative) and the test's title
 * exactly as written in the source, including `it.each` placeholders. A
 * table test whose rows cover both polarities may be named on both sides.
 * The mappings were seeded from test titles and bodies; whether the named
 * failing scenario really reaches this validator's refusal is a review
 * judgement the test cannot make.
 */
import type { FraudProofCatalogueCategoryName } from "@al-ft/midgard-sdk";

export type ValidatorScenario = Readonly<{ file: string; test: string }>;

export type ValidatorScenarioPair = Readonly<{
  passing: readonly ValidatorScenario[];
  failing: readonly ValidatorScenario[];
}>;

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
};

export const FAMILY_SCENARIOS: Readonly<
  Partial<Record<FraudProofCatalogueCategoryName, ValidatorScenarioPair>>
> = {
  nonExistentInput: {
    passing: [
      {
        file: "demo/midgard-fault-proofs/tests/submit-init-emulator-ledger-rules.test.ts",
        test: "proves and removes a tail non-existent-input block end to end",
      },
    ],
    failing: [
      {
        file: "demo/midgard-fault-proofs/tests/non-existent-input-wrongful-rejection-lifecycle.test.ts",
        test: "proves $count inputs at $index deep=$deep",
      },
    ],
  },
  transitionTrace: {
    passing: [
      {
        file: "demo/midgard-fault-proofs/tests/submit-init-emulator-transition-trace.test.ts",
        test: "submits and removes a tail transition-trace fraud proof end to end",
      },
    ],
    failing: [
      {
        file: "demo/midgard-fault-proofs/tests/submit-init-emulator-transition-trace-subvariants.test.ts",
        test: "rejects an honest late withdrawal accused as omitted at final 6",
      },
    ],
  },
  validationTraceDispute: {
    passing: [
      {
        file: "demo/midgard-fault-proofs/tests/validation-trace-dispute-installed-lifecycle.test.ts",
        test: "plays the full honest game to award and removal, refusing forged and caller-authored material at the exact checks",
      },
    ],
    failing: [
      {
        file: "demo/midgard-fault-proofs/tests/submit-init-emulator-validation-dispute-phase-a-item.test.ts",
        test: "refuses a forged %s successor against an honest trace",
      },
    ],
  },
  daHashPreimage: {
    passing: [
      {
        file: "demo/midgard-fault-proofs/tests/submit-init-emulator-da-hash-preimage.test.ts",
        test: "proves and removes a tail miskeyed-leaf block end to end",
      },
    ],
    failing: [
      {
        file: "demo/midgard-fault-proofs/tests/submit-init-emulator-da-hash-preimage.test.ts",
        test: "cannot advance a da-hash-preimage thread against a valid block",
      },
    ],
  },
  noReferenceInput: {
    passing: [
      {
        file: "demo/midgard-fault-proofs/tests/submit-init-emulator-no-reference-input-lifecycle.test.ts",
        test: "convicts a reference input that never existed, mints the permanent fraud-proof token, and removes the fraudulent commitment",
      },
    ],
    failing: [
      {
        file: "demo/midgard-fault-proofs/tests/submit-init-emulator-no-reference-input-lifecycle.test.ts",
        test: "refuses to convict an honest commitment whose reference input was produced in-block",
      },
    ],
  },
  referenceInputNoIdx: {
    passing: [
      {
        file: "demo/midgard-fault-proofs/tests/submit-init-emulator-reference-input-no-idx-lifecycle.test.ts",
        test: "proves and removes an out-of-range reference-input block end to end",
      },
    ],
    failing: [
      {
        file: "demo/midgard-fault-proofs/tests/submit-init-emulator-reference-input-no-idx-lifecycle.test.ts",
        test: "refuses every attack on an honest commitment at the validator's own check",
      },
    ],
  },
  invalidSignature: {
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
  fabricatedDeposit: {
    passing: [
      {
        file: "demo/midgard-fault-proofs/tests/submit-init-emulator-fabricated-deposit.test.ts",
        test: "proves $scenario deposit with $mode history, mints permanent evidence, and removes the fraudulent commitment",
      },
    ],
    failing: [
      {
        file: "demo/midgard-fault-proofs/tests/submit-init-emulator-fabricated-deposit.test.ts",
        test: "cannot advance a fabricated-deposit thread against a valid %s block",
      },
    ],
  },
  fabricatedWithdrawal: {
    passing: [
      {
        file: "demo/midgard-fault-proofs/tests/submit-init-emulator-fabricated-withdrawal.test.ts",
        test: "proves $scenario withdrawal with $mode history, mints permanent evidence, and removes the fraudulent commitment",
      },
    ],
    failing: [
      {
        file: "demo/midgard-fault-proofs/tests/submit-init-emulator-fabricated-withdrawal.test.ts",
        test: "cannot advance a fabricated-withdrawal thread against a valid %s block",
      },
    ],
  },
  nativeScriptDecoding: {
    passing: [
      {
        file: "demo/midgard-fault-proofs/tests/submit-init-emulator-native-script-decoding-direction-a.test.ts",
        test: "proves a wrongful acceptance through the proving core, mints the permanent fraud-proof token, and removes the fraudulent commitment",
      },
    ],
    failing: [
      {
        file: "demo/midgard-fault-proofs/tests/submit-init-emulator-native-script-decoding-adversarial.test.ts",
        test: "refuses a direction-A conviction over a well-formed payload",
      },
    ],
  },
  missingSignature: {
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
  missingNativeScriptTx: {
    passing: [
      {
        file: "demo/midgard-fault-proofs/tests/submit-init-emulator-missing-native-script-tx-lifecycle.test.ts",
        test: "proves the absent script through six reference-script steps, cancels explicitly, and removes the fraudulent commitment",
      },
    ],
    failing: [
      {
        file: "demo/midgard-fault-proofs/tests/submit-init-emulator-missing-native-script-tx-adversarial.test.ts",
        test: "refuses an honest present script on-chain and pins every earlier negative/cancel gate",
      },
    ],
  },
  withdrawnReferenceInput: {
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
  canonicalDecodability: {
    passing: [
      {
        file: "demo/midgard-fault-proofs/tests/submit-init-emulator-canonical-decodability.test.ts",
        test: "mints permanent evidence and removes the fraudulent commitment",
      },
    ],
    failing: [
      {
        file: "demo/midgard-fault-proofs/tests/submit-init-emulator-canonical-decodability-adversarial.test.ts",
        test: "binds verdict 0 but cannot fabricate or finalize a conviction",
      },
    ],
  },
  committedFieldShape: {
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
  minFee: {
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
  doubleWithdraw: {
    passing: [
      {
        file: "demo/midgard-fault-proofs/tests/submit-init-emulator-double-withdraw.test.ts",
        test: "proves the payable duplicate, resumes from step-02, and removes the fraudulent block",
      },
    ],
    failing: [
      {
        file: "demo/midgard-fault-proofs/tests/submit-init-emulator-double-withdraw.test.ts",
        test: "refuses an honest non-payable duplicate and same-leaf pairing on chain, and enforces cancel ownership",
      },
    ],
  },
  l2TxMistag: {
    passing: [
      {
        file: "demo/midgard-fault-proofs/tests/submit-init-emulator-l2-tx-mistag.test.ts",
        test: "mints permanent evidence for a committed code-1 normal leaf and removes the fraudulent block",
      },
    ],
    failing: [
      {
        file: "demo/midgard-fault-proofs/tests/submit-init-emulator-l2-tx-mistag-adversarial.test.ts",
        test: "refuses an honest code-0 leaf at the exact on-chain check and a scalar flip at membership",
      },
    ],
  },
  withdrawnInput: {
    passing: [
      {
        file: "demo/midgard-fault-proofs/tests/submit-init-emulator-withdrawn-input-lifecycle.test.ts",
        test: "mints the permanent fault token and removes the fraudulent block",
      },
    ],
    failing: [
      {
        file: "demo/midgard-fault-proofs/tests/submit-init-emulator-withdrawn-input-honest.test.ts",
        test: "refuses on-chain when the withdrawals root commits a different out-ref",
      },
    ],
  },
  valueNotPreserved: {
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
  inputSetUniqueness: {
    passing: [
      {
        file: "demo/midgard-fault-proofs/tests/submit-init-emulator-input-set-uniqueness-lifecycle.test.ts",
        test: "proves a duplicate spend input end to end, mints the permanent fraud-proof token, and removes the fraudulent commitment",
      },
    ],
    failing: [
      {
        file: "demo/midgard-fault-proofs/tests/submit-init-emulator-input-set-uniqueness-adversarial.test.ts",
        test: "refuses every fabricated claim against an honest all-unique commitment, at the exact on-chain check",
      },
    ],
  },
  mintAuthorization: {
    passing: [
      {
        file: "demo/midgard-fault-proofs/tests/submit-init-emulator-mint-authorization-direction-a-lifecycle.test.ts",
        test: "proves an absent mint policy end to end, mints the permanent fraud-proof token, and removes the fraudulent commitment",
      },
    ],
    failing: [
      {
        file: "demo/midgard-fault-proofs/tests/submit-init-emulator-mint-authorization-adversarial.test.ts",
        test: "refuses a false absence claim when the committed field 6 consulted the policy's script",
      },
    ],
  },
  networkId: {
    passing: [
      {
        file: "demo/midgard-fault-proofs/tests/network-id-wrongful-rejection-lifecycle.test.ts",
        test: "runs Init through the forced door to a permanent mint and removal, cancels every nonterminal step, and restarts by out-ref",
      },
    ],
    failing: [
      {
        file: "demo/midgard-fault-proofs/tests/network-id-wrongful-rejection-lifecycle.test.ts",
        test: "refuses every scan mutation at the exact check that owns it",
      },
    ],
  },
  nativeScriptInvalid: {
    passing: [
      {
        file: "demo/midgard-fault-proofs/tests/native-script-invalid-wrongful-rejection-lifecycle.test.ts",
        test: "reopens durable evidence and convicts a true script through block removal",
      },
    ],
    failing: [
      {
        file: "demo/midgard-fault-proofs/tests/native-script-invalid-wrongful-rejection-lifecycle.test.ts",
        test: "refuses an honest false native script with %s signers on chain",
      },
    ],
  },
  minAda: {
    passing: [
      {
        file: "demo/midgard-fault-proofs/tests/min-ada-wrongful-rejection-lifecycle.test.ts",
        test: "authenticates exact output through registered mint and removal assets=$assetCount prefix=$prefixCount output=$outputBytes depth=$depth",
      },
    ],
    failing: [
      {
        file: "demo/midgard-fault-proofs/tests/min-ada-wrongful-rejection-lifecycle.test.ts",
        test: "refuses an honest underfunded output on chain",
      },
    ],
  },
  fieldPreimageLengthMismatch: {
    passing: [
      {
        file: "demo/midgard-fault-proofs/tests/field-preimage-length-mismatch-lifecycle.test.ts",
        test: "starts at generic Init, refuses every mutated accepted seam on chain, convicts the accepted source, mints proof, and removes the descendant chain",
      },
    ],
    failing: [
      {
        file: "demo/midgard-fault-proofs/tests/field-preimage-length-mismatch-lifecycle.test.ts",
        test: "refuses an honest accepted block at the terminal after authenticating its field on chain",
      },
    ],
  },
  fieldItemWidthIllegal: {
    passing: [
      {
        file: "demo/midgard-fault-proofs/tests/field-item-width-illegal-lifecycle.test.ts",
        test: "contradicts a wrongful forced rejection of a non-empty mint-policy item",
      },
    ],
    failing: [
      {
        file: "demo/midgard-fault-proofs/tests/field-item-width-illegal-lifecycle.test.ts",
        test: "refuses to contradict an honest forced rejection of an output one byte over the bound",
      },
    ],
  },
  witnessScriptDecoding: {
    passing: [
      {
        file: "demo/midgard-fault-proofs/tests/witness-script-decoding-lifecycle.test.ts",
        test: "contradicts a wrongful node-limit rejection of the widest canonical script the field bound admits",
      },
    ],
    failing: [
      {
        file: "demo/midgard-fault-proofs/tests/witness-script-decoding-lifecycle.test.ts",
        test: "refuses to contradict honest forced rejections: the undecodable wrapper and the empty payload",
      },
    ],
  },
  scriptIntegrityHashMissing: {
    passing: [
      {
        file: "demo/midgard-fault-proofs/tests/script-integrity-hash-missing-lifecycle.test.ts",
        test: "publishes, proves accepted absent integrity hash, mints, and removes",
      },
    ],
    failing: [
      {
        file: "demo/midgard-fault-proofs/tests/script-integrity-hash-missing-lifecycle.test.ts",
        test: "refuses an honest accepted block and every substituted accepted seam on chain",
      },
    ],
  },
  transactionOutputNonCanonical: {
    passing: [
      {
        file: "demo/midgard-fault-proofs/tests/transaction-output-non-canonical-lifecycle.test.ts",
        test: "convicts a forced rejection of the maximum canonical output through real checkpoints, refuses every forced seam, then mints and removes",
      },
    ],
    failing: [
      {
        file: "demo/midgard-fault-proofs/tests/transaction-output-non-canonical-lifecycle.test.ts",
        test: "refuses to mint against an honest forced rejection: the malformed output reaches its non-canonical terminal and step 04 refuses on chain",
      },
    ],
  },
  resolvedOutputNonCanonical: {
    passing: [
      {
        file: "demo/midgard-fault-proofs/tests/resolved-output-non-canonical-lifecycle.test.ts",
        test: "contradicts a wrongful acceptance of a non-canonical spend input at the maximum shape: refuses the accepted seams, resumes the scan, mints and removes",
      },
    ],
    failing: [
      {
        file: "demo/midgard-fault-proofs/tests/resolved-output-non-canonical-lifecycle.test.ts",
        test: "refuses to convict an honest accepted block: the finishable control cannot be advanced and a canonical verdict cannot mint under an accepted subject",
      },
    ],
  },
  mintDeclaredAssetLimit: {
    passing: [
      {
        file: "demo/midgard-fault-proofs/tests/mint-declared-asset-limit-lifecycle.test.ts",
        test: "proves the maximum accepted crossing, refuses every honest and substituted accepted shape, and removes the block",
      },
    ],
    failing: [
      {
        file: "demo/midgard-fault-proofs/tests/mint-declared-asset-limit-lifecycle.test.ts",
        test: "proves the exact forced wrongful rejection across a policy item, refuses an honest rejection and every mutated leaf, and cancels from every step",
      },
    ],
  },
  spendInputSignerMissing: {
    passing: [
      {
        file: "demo/midgard-fault-proofs/tests/spend-input-signer-missing-lifecycle.test.ts",
        test: "runs a forced wrongful rejection with a valid matching signature through removal",
      },
    ],
    failing: [
      {
        file: "demo/midgard-fault-proofs/tests/spend-input-signer-missing-lifecycle.test.ts",
        test: "refuses an honest accepted block, every substituted accepted seam, and a mutated spend coordinate on chain",
      },
    ],
  },
  protectedOutputSignerMissing: {
    passing: [
      {
        file: "demo/midgard-fault-proofs/tests/protected-output-signer-missing-lifecycle.test.ts",
        test: "runs maximum-carriage evidence through cancel, restartable scan, mint and leased removal",
      },
    ],
    failing: [
      {
        file: "demo/midgard-fault-proofs/tests/protected-output-signer-missing-lifecycle.test.ts",
        test: "refuses an honest forced rejection whose signer really is missing",
      },
    ],
  },
  observersForbiddenOnUntaggedNetwork: {
    passing: [
      {
        file: "demo/midgard-fault-proofs/tests/observers-forbidden-on-untagged-network-lifecycle.test.ts",
        test: "contradicts a wrongful forced rejection of the maximum observer field on a tagged scalar",
      },
    ],
    failing: [
      {
        file: "demo/midgard-fault-proofs/tests/observers-forbidden-on-untagged-network-lifecycle.test.ts",
        test: "refuses to contradict an honest forced rejection of observers on scalar 255 under a present integrity hash",
      },
    ],
  },
  observerOrderInvalid: {
    passing: [
      {
        file: "demo/midgard-fault-proofs/tests/observer-order-invalid-lifecycle.test.ts",
        test: "convicts an accepted field whose first adjacent pair descends",
      },
    ],
    failing: [
      {
        file: "demo/midgard-fault-proofs/tests/observer-order-invalid-lifecycle.test.ts",
        test: "refuses to contradict an honest forced rejection of a duplicate observer",
      },
    ],
  },
  outputReferenceScriptDecoding: {
    passing: [
      {
        file: "demo/midgard-fault-proofs/tests/output-reference-script-decoding-lifecycle.test.ts",
        test: "contradicts a wrongful forced DepthLimit rejection of nested containers through the frame stack",
      },
    ],
    failing: [
      {
        file: "demo/midgard-fault-proofs/tests/output-reference-script-decoding-lifecycle.test.ts",
        test: "contradicts a wrongful acceptance of an empty native payload at the bind, and refuses to convict an honest accepted signature script",
      },
    ],
  },
  executionSourceScriptDecoding: {
    passing: [
      {
        file: "demo/midgard-fault-proofs/tests/execution-source-script-decoding-lifecycle.test.ts",
        test: "contradicts a wrongful forced DepthLimit rejection of nested containers through the frame stack",
      },
    ],
    failing: [
      {
        file: "demo/midgard-fault-proofs/tests/execution-source-script-decoding-lifecycle.test.ts",
        test: "refuses to convict an honest accepted block whose source item decodes to the exact terminal",
      },
    ],
  },
  receivePurposeLanguage: {
    passing: [
      {
        file: "demo/midgard-fault-proofs/tests/receive-purpose-language-lifecycle.test.ts",
        test: "contradicts a wrongful forced rejection of a native receive at the maximum shape: refuses every forced-door seam, then mints and removes",
      },
    ],
    failing: [
      {
        file: "demo/midgard-fault-proofs/tests/receive-purpose-language-lifecycle.test.ts",
        test: "refuses to convict an honest accepted native receive at the terminal step",
      },
    ],
  },
  unusedScriptWitness: {
    passing: [
      {
        file: "demo/midgard-fault-proofs/tests/unused-script-witness-lifecycle.test.ts",
        test: "contradicts a wrongful forced rejection of a used inline script at the maximum shape: refuses every forced-door seam, then mints and removes",
      },
    ],
    failing: [
      {
        file: "demo/midgard-fault-proofs/tests/unused-script-witness-lifecycle.test.ts",
        test: "refuses to convict an honest accepted block whose accused inline script is used, at the terminal step",
      },
    ],
  },
  missingScriptSource: {
    passing: [
      {
        file: "demo/midgard-fault-proofs/tests/missing-script-source-lifecycle.test.ts",
        test: "corrects purpose kind $purposeKind with the required source $presentAt",
      },
    ],
    failing: [
      {
        file: "demo/midgard-fault-proofs/tests/missing-script-source-lifecycle.test.ts",
        test: "refuses the honest forced rejection off chain and at the terminal contradiction",
      },
    ],
  },
  missingRedeemer: {
    passing: [
      {
        file: "demo/midgard-fault-proofs/tests/missing-redeemer-lifecycle.test.ts",
        test: "convicts the $direction direction for purpose kind $purposeKind from Init through the permanent mint and removal",
      },
    ],
    failing: [
      {
        file: "demo/midgard-fault-proofs/tests/missing-redeemer-lifecycle.test.ts",
        test: "refuses honest blocks, mutated coordinates, and every substituted authentication seam, and cancels every other physical step",
      },
    ],
  },
  executionNativeScriptInvalid: {
    passing: [
      {
        file: "demo/midgard-fault-proofs/tests/execution-native-script-invalid-lifecycle.test.ts",
        test: "runs $direction/$sourceOrigin/$acceptedPurpose lifecycle (cancel=$cancelAt, maximum=$maximum)",
      },
    ],
    failing: [
      {
        file: "demo/midgard-fault-proofs/tests/execution-native-script-invalid-lifecycle.test.ts",
        test: "runs $direction/$sourceOrigin/$acceptedPurpose lifecycle (cancel=$cancelAt, maximum=$maximum)",
      },
    ],
  },
  scriptIntegrityHashMismatch: {
    passing: [
      {
        file: "demo/midgard-fault-proofs/tests/script-integrity-hash-mismatch-lifecycle.test.ts",
        test: "$direction bitmap $bitmap honest=$honest: authenticated lifecycle and terminal polarity",
      },
    ],
    failing: [
      {
        file: "demo/midgard-fault-proofs/tests/script-integrity-hash-mismatch-lifecycle.test.ts",
        test: "$direction bitmap $bitmap honest=$honest: authenticated lifecycle and terminal polarity",
      },
    ],
  },
  distinctAssetAccumulationLimit: {
    passing: [
      {
        file: "demo/midgard-fault-proofs/tests/distinct-asset-accumulation-limit-lifecycle.test.ts",
        test: "proves $kind forced=$forced maximum=$maximum honest=$honest crossing/boundary",
      },
    ],
    failing: [
      {
        file: "demo/midgard-fault-proofs/tests/distinct-asset-accumulation-limit-lifecycle.test.ts",
        test: "proves $kind forced=$forced maximum=$maximum honest=$honest crossing/boundary",
      },
    ],
  },
  mintItemNonCanonical: {
    passing: [
      {
        file: "demo/midgard-fault-proofs/tests/mint-item-non-canonical-lifecycle.test.ts",
        test: "proves grammar and ordering faults, refuses honest mint/burn and forged evidence, cancels, resumes and removes",
      },
    ],
    failing: [
      {
        file: "demo/midgard-fault-proofs/tests/mint-item-non-canonical-lifecycle.test.ts",
        test: "proves grammar and ordering faults, refuses honest mint/burn and forged evidence, cancels, resumes and removes",
      },
    ],
  },
};

/**
 * Existing gaps, grouped by reason. Map a validator and delete it here; the
 * pinned count below then has to come down with it. Adding a validator here
 * instead of mapping it means raising that count, which a reviewer sees.
 */
export const UNMAPPED_VALIDATORS: readonly Readonly<{
  reason: string;
  validators: readonly string[];
}>[] = [
  {
    reason:
      "validation-trace dispute stage validators. The dispute game is mapped at family level (validationTraceDispute); no scenario is mapped where one stage validator itself refuses.",
    validators: [
      "fraud_proofs/validation_trace/award_v1.main",
      "fraud_proofs/validation_trace/boundary_v1.main",
      "fraud_proofs/validation_trace/canonical_decode_empty_semantic_v1.main",
      "fraud_proofs/validation_trace/canonical_decode_item_observe_v1.main",
      "fraud_proofs/validation_trace/canonical_decode_item_proof_v1.main",
      "fraud_proofs/validation_trace/canonical_decode_item_semantic_v1.main",
      "fraud_proofs/validation_trace/canonical_decode_item_settlement_v1.main",
      "fraud_proofs/validation_trace/canonical_decode_item_source_v1.main",
      "fraud_proofs/validation_trace/canonical_decode_v1.main",
      "fraud_proofs/validation_trace/cek_context_assemble.main",
      "fraud_proofs/validation_trace/cek_context_control.main",
      "fraud_proofs/validation_trace/cek_context_finalize_authenticate.main",
      "fraud_proofs/validation_trace/cek_context_finalize_midgard.main",
      "fraud_proofs/validation_trace/cek_context_finalize_mint.main",
      "fraud_proofs/validation_trace/cek_context_finalize_observe.main",
      "fraud_proofs/validation_trace/cek_context_finalize_spend.main",
      "fraud_proofs/validation_trace/cek_context_finalize_withdraw.main",
      "fraud_proofs/validation_trace/cek_context_item_bind.main",
      "fraud_proofs/validation_trace/cek_context_item_data_continue.main",
      "fraud_proofs/validation_trace/cek_context_item_data_finish_descriptor.main",
      "fraud_proofs/validation_trace/cek_context_item_data_finish_value.main",
      "fraud_proofs/validation_trace/cek_context_item_finalize.main",
      "fraud_proofs/validation_trace/cek_context_item_hash.main",
      "fraud_proofs/validation_trace/cek_context_item_return.main",
      "fraud_proofs/validation_trace/cek_context_item_selection_continue.main",
      "fraud_proofs/validation_trace/cek_context_item_selection_finish.main",
      "fraud_proofs/validation_trace/cek_context_mint_init.main",
      "fraud_proofs/validation_trace/cek_context_mint_item.main",
      "fraud_proofs/validation_trace/cek_context_observer_authenticate.main",
      "fraud_proofs/validation_trace/cek_context_observer_fold.main",
      "fraud_proofs/validation_trace/cek_context_output.main",
      "fraud_proofs/validation_trace/cek_context_redeemer_begin.main",
      "fraud_proofs/validation_trace/cek_context_redeemer_select_authenticate.main",
      "fraud_proofs/validation_trace/cek_context_redeemer_select_finish.main",
      "fraud_proofs/validation_trace/cek_context_redeemer_select_hash.main",
      "fraud_proofs/validation_trace/cek_context_redeemer_select_initialize.main",
      "fraud_proofs/validation_trace/cek_context_reference.main",
      "fraud_proofs/validation_trace/cek_context_seed.main",
      "fraud_proofs/validation_trace/cek_context_settle.main",
      "fraud_proofs/validation_trace/cek_context_signer.main",
      "fraud_proofs/validation_trace/cek_context_spend.main",
      "fraud_proofs/validation_trace/cek_context_step_semantic_v1.main",
      "fraud_proofs/validation_trace/cek_context_tx_info.main",
      "fraud_proofs/validation_trace/cek_core_arm_compute.main",
      "fraud_proofs/validation_trace/cek_core_arm_machine.main",
      "fraud_proofs/validation_trace/cek_core_arm_map_conversion.main",
      "fraud_proofs/validation_trace/cek_core_bls_budget.main",
      "fraud_proofs/validation_trace/cek_core_bls_final.main",
      "fraud_proofs/validation_trace/cek_core_bls_roots.main",
      "fraud_proofs/validation_trace/cek_core_builtin_budget.main",
      "fraud_proofs/validation_trace/cek_core_builtin_roots.main",
      "fraud_proofs/validation_trace/cek_core_direct_scalar.main",
      "fraud_proofs/validation_trace/cek_core_direct_structured.main",
      "fraud_proofs/validation_trace/cek_core_failure_budget.main",
      "fraud_proofs/validation_trace/cek_core_failure_known.main",
      "fraud_proofs/validation_trace/cek_core_map_start_budget.main",
      "fraud_proofs/validation_trace/cek_core_map_start_nodes.main",
      "fraud_proofs/validation_trace/cek_core_map_start_roots.main",
      "fraud_proofs/validation_trace/cek_core_semantic_choose.main",
      "fraud_proofs/validation_trace/cek_core_semantic_data_construct.main",
      "fraud_proofs/validation_trace/cek_core_semantic_data_misc.main",
      "fraud_proofs/validation_trace/cek_core_semantic_data_scalar.main",
      "fraud_proofs/validation_trace/cek_core_semantic_failure_material.main",
      "fraud_proofs/validation_trace/cek_core_semantic_failure_roots.main",
      "fraud_proofs/validation_trace/cek_core_semantic_list_construct.main",
      "fraud_proofs/validation_trace/cek_core_semantic_list_select.main",
      "fraud_proofs/validation_trace/cek_core_semantic_pair.main",
      "fraud_proofs/validation_trace/cek_core_semantic_result.main",
      "fraud_proofs/validation_trace/cek_core_settle.main",
      "fraud_proofs/validation_trace/cek_core_step_semantic_v1.main",
      "fraud_proofs/validation_trace/cek_core_type_failure_kinds.main",
      "fraud_proofs/validation_trace/cek_core_type_failure_roots.main",
      "fraud_proofs/validation_trace/cek_execution_selection_semantic_v1.main",
      "fraud_proofs/validation_trace/cek_execution_selection_yields.authenticate",
      "fraud_proofs/validation_trace/cek_execution_selection_yields.material_data",
      "fraud_proofs/validation_trace/cek_execution_selection_yields.material_program",
      "fraud_proofs/validation_trace/cek_execution_selection_yields.successor",
      "fraud_proofs/validation_trace/cek_finish_semantic_v1.main",
      "fraud_proofs/validation_trace/cek_material_traversal_v1.main",
      "fraud_proofs/validation_trace/cek_material_traversal_yields.data",
      "fraud_proofs/validation_trace/cek_material_traversal_yields.program",
      "fraud_proofs/validation_trace/cek_v1.main",
      "fraud_proofs/validation_trace/compact_binding_semantic_v1.main",
      "fraud_proofs/validation_trace/compact_binding_v1.main",
      "fraud_proofs/validation_trace/dispute_v1.main",
      "fraud_proofs/validation_trace/game_v1.main",
      "fraud_proofs/validation_trace/input_sets_empty_semantic_v1.main",
      "fraud_proofs/validation_trace/input_sets_item_semantic_v1.main",
      "fraud_proofs/validation_trace/input_sets_v1.main",
      "fraud_proofs/validation_trace/ledger_delta_finalize_semantic_v1.main",
      "fraud_proofs/validation_trace/ledger_delta_operation_semantic_v1.main",
      "fraud_proofs/validation_trace/ledger_delta_output_finish_semantic_v1.main",
      "fraud_proofs/validation_trace/ledger_delta_output_semantic_v1.main",
      "fraud_proofs/validation_trace/ledger_delta_proof_frame_semantic_v1.main",
      "fraud_proofs/validation_trace/ledger_delta_replay_finish_semantic_v1.main",
      "fraud_proofs/validation_trace/ledger_delta_replay_semantic_v1.main",
      "fraud_proofs/validation_trace/ledger_delta_terminal_semantic_v1.main",
      "fraud_proofs/validation_trace/ledger_delta_v1.main",
      "fraud_proofs/validation_trace/ledger_output_descriptor_datum_summary_yield.main",
      "fraud_proofs/validation_trace/ledger_output_descriptor_reference_script_yield.main",
      "fraud_proofs/validation_trace/ledger_output_descriptor_scan_facts_yield.main",
      "fraud_proofs/validation_trace/ledger_output_descriptor_value_summary_yield.main",
      "fraud_proofs/validation_trace/ledger_output_proof_datum_advance_bytes_yield.main",
      "fraud_proofs/validation_trace/ledger_output_proof_datum_advance_integer_yield.main",
      "fraud_proofs/validation_trace/ledger_output_proof_datum_attach_bytes_yield.main",
      "fraud_proofs/validation_trace/ledger_output_proof_datum_attach_integer_yield.main",
      "fraud_proofs/validation_trace/ledger_output_proof_datum_close_yield.main",
      "fraud_proofs/validation_trace/ledger_output_proof_datum_finalize_frame_yield.main",
      "fraud_proofs/validation_trace/ledger_output_proof_datum_finish_yield.main",
      "fraud_proofs/validation_trace/ledger_output_proof_datum_fold_list_yield.main",
      "fraud_proofs/validation_trace/ledger_output_proof_datum_fold_map_yield.main",
      "fraud_proofs/validation_trace/ledger_output_proof_datum_head_large_constructor_yield.main",
      "fraud_proofs/validation_trace/ledger_output_proof_datum_head_map_yield.main",
      "fraud_proofs/validation_trace/ledger_output_proof_datum_head_scalar_yield.main",
      "fraud_proofs/validation_trace/ledger_output_proof_datum_head_sequence_yield.main",
      "fraud_proofs/validation_trace/ledger_output_proof_datum_large_constructor_yield.main",
      "fraud_proofs/validation_trace/ledger_output_proof_datum_large_fields_yield.main",
      "fraud_proofs/validation_trace/ledger_output_proof_native_script_yield.main",
      "fraud_proofs/validation_trace/ledger_output_proof_reference_script_yield.main",
      "fraud_proofs/validation_trace/ledger_output_proof_scalar_bytes_yield.main",
      "fraud_proofs/validation_trace/ledger_output_proof_scalar_integer_yield.main",
      "fraud_proofs/validation_trace/ledger_output_proof_script_hash_yield.main",
      "fraud_proofs/validation_trace/ledger_output_proof_span_yield.main",
      "fraud_proofs/validation_trace/ledger_output_proof_structure_assets_yield.main",
      "fraud_proofs/validation_trace/ledger_output_proof_structure_finish_yield.main",
      "fraud_proofs/validation_trace/ledger_output_proof_structure_optional_yield.main",
      "fraud_proofs/validation_trace/ledger_output_proof_structure_yield.main",
      "fraud_proofs/validation_trace/ledger_output_proof_value_yield.main",
      "fraud_proofs/validation_trace/native_scripts_effectful_semantic_v1.main",
      "fraud_proofs/validation_trace/native_scripts_native_semantic_v1.main",
      "fraud_proofs/validation_trace/native_scripts_terminal_semantic_v1.main",
      "fraud_proofs/validation_trace/native_scripts_v1.main",
      "fraud_proofs/validation_trace/phase_a_native_scripts_advance_semantic_v1.main",
      "fraud_proofs/validation_trace/phase_a_native_scripts_all_or_any_container_frame_payload_semantic_v1.main",
      "fraud_proofs/validation_trace/phase_a_native_scripts_all_or_any_empty_container_payload_semantic_v1.main",
      "fraud_proofs/validation_trace/phase_a_native_scripts_at_least_container_frame_payload_semantic_v1.main",
      "fraud_proofs/validation_trace/phase_a_native_scripts_at_least_empty_container_payload_semantic_v1.main",
      "fraud_proofs/validation_trace/phase_a_native_scripts_frame_semantic_v1.main",
      "fraud_proofs/validation_trace/phase_a_native_scripts_item_semantic_v1.main",
      "fraud_proofs/validation_trace/phase_a_native_scripts_item_yields_v1.foreign",
      "fraud_proofs/validation_trace/phase_a_native_scripts_item_yields_v1.native",
      "fraud_proofs/validation_trace/phase_a_native_scripts_signature_above_last_payload_semantic_v1.main",
      "fraud_proofs/validation_trace/phase_a_native_scripts_signature_below_first_payload_semantic_v1.main",
      "fraud_proofs/validation_trace/phase_a_native_scripts_signature_between_payload_semantic_v1.main",
      "fraud_proofs/validation_trace/phase_a_native_scripts_signature_empty_payload_semantic_v1.main",
      "fraud_proofs/validation_trace/phase_a_native_scripts_signature_membership_payload_semantic_v1.main",
      "fraud_proofs/validation_trace/phase_a_native_scripts_timelock_payload_semantic_v1.main",
      "fraud_proofs/validation_trace/phase_a_native_scripts_token_head_semantic_v1.main",
      "fraud_proofs/validation_trace/phase_a_native_scripts_v1.main",
      "fraud_proofs/validation_trace/phase_a_script_preconditions_item_semantic_v1.main",
      "fraud_proofs/validation_trace/phase_a_script_preconditions_semantic_v1.main",
      "fraud_proofs/validation_trace/phase_a_script_preconditions_v1.main",
      "fraud_proofs/validation_trace/proof_item_v1.main",
      "fraud_proofs/validation_trace/resolve_inputs_finish_semantic_v1.main",
      "fraud_proofs/validation_trace/resolve_inputs_initial_semantic_v1.main",
      "fraud_proofs/validation_trace/resolve_inputs_membership_begin_semantic_v1.main",
      "fraud_proofs/validation_trace/resolve_inputs_membership_finalize_semantic_v1.main",
      "fraud_proofs/validation_trace/resolve_inputs_membership_step_semantic_v1.main",
      "fraud_proofs/validation_trace/resolve_inputs_non_membership_semantic_v1.main",
      "fraud_proofs/validation_trace/resolve_inputs_v1.main",
      "fraud_proofs/validation_trace/script_integrity_authentication_semantic_v1.main",
      "fraud_proofs/validation_trace/script_integrity_compact_semantic_v1.main",
      "fraud_proofs/validation_trace/script_integrity_finalize_semantic_v1.main",
      "fraud_proofs/validation_trace/script_integrity_v1.main",
      "fraud_proofs/validation_trace/script_integrity_witness_set_semantic_v1.main",
      "fraud_proofs/validation_trace/script_sources_middle_yields_v1.stage_four_begin",
      "fraud_proofs/validation_trace/script_sources_middle_yields_v1.stage_four_finish",
      "fraud_proofs/validation_trace/script_sources_middle_yields_v1.stage_six_begin_policy",
      "fraud_proofs/validation_trace/script_sources_middle_yields_v1.stage_six_finish",
      "fraud_proofs/validation_trace/script_sources_middle_yields_v1.stage_six_fold_asset",
      "fraud_proofs/validation_trace/script_sources_middle_yields_v1.stage_three_finish",
      "fraud_proofs/validation_trace/script_sources_middle_yields_v1.stage_three_replay",
      "fraud_proofs/validation_trace/script_sources_middle_yields_v1.stage_two_advance",
      "fraud_proofs/validation_trace/script_sources_non_output_semantic_v1.main",
      "fraud_proofs/validation_trace/script_sources_output_proof_begin_semantic_v1.main",
      "fraud_proofs/validation_trace/script_sources_output_proof_finalize_semantic_v1.main",
      "fraud_proofs/validation_trace/script_sources_output_proof_finish_semantic_v1.main",
      "fraud_proofs/validation_trace/script_sources_output_proof_step_semantic_v1.main",
      "fraud_proofs/validation_trace/script_sources_redeemer_item_step_yield_v1.main",
      "fraud_proofs/validation_trace/script_sources_stage_eight_finish_semantic_v1.main",
      "fraud_proofs/validation_trace/script_sources_stage_eight_purpose_semantic_v1.main",
      "fraud_proofs/validation_trace/script_sources_stage_eleven_finish_semantic_v1.main",
      "fraud_proofs/validation_trace/script_sources_stage_eleven_source_semantic_v1.main",
      "fraud_proofs/validation_trace/script_sources_stage_nine_effectful_match_semantic_v1.main",
      "fraud_proofs/validation_trace/script_sources_stage_nine_mismatch_semantic_v1.main",
      "fraud_proofs/validation_trace/script_sources_stage_nine_missing_semantic_v1.main",
      "fraud_proofs/validation_trace/script_sources_stage_nine_native_match_semantic_v1.main",
      "fraud_proofs/validation_trace/script_sources_stage_one_finish_semantic_v1.main",
      "fraud_proofs/validation_trace/script_sources_stage_one_redeemer_advance_bytes_executor.main",
      "fraud_proofs/validation_trace/script_sources_stage_one_redeemer_advance_integer_executor.main",
      "fraud_proofs/validation_trace/script_sources_stage_one_redeemer_advance_large_constructor_executor.main",
      "fraud_proofs/validation_trace/script_sources_stage_one_redeemer_advance_large_fields_executor.main",
      "fraud_proofs/validation_trace/script_sources_stage_one_redeemer_attach_bytes_executor.main",
      "fraud_proofs/validation_trace/script_sources_stage_one_redeemer_attach_integer_executor.main",
      "fraud_proofs/validation_trace/script_sources_stage_one_redeemer_cek_envelope.main",
      "fraud_proofs/validation_trace/script_sources_stage_one_redeemer_cek_settlement.main",
      "fraud_proofs/validation_trace/script_sources_stage_one_redeemer_close_executor.main",
      "fraud_proofs/validation_trace/script_sources_stage_one_redeemer_envelope_v1.main",
      "fraud_proofs/validation_trace/script_sources_stage_one_redeemer_execution_settlement_v1.main",
      "fraud_proofs/validation_trace/script_sources_stage_one_redeemer_finalize_frame_executor_v1.main",
      "fraud_proofs/validation_trace/script_sources_stage_one_redeemer_finish_data_executor.main",
      "fraud_proofs/validation_trace/script_sources_stage_one_redeemer_fold_list_executor.main",
      "fraud_proofs/validation_trace/script_sources_stage_one_redeemer_fold_map_executor_v1.main",
      "fraud_proofs/validation_trace/script_sources_stage_one_redeemer_head_large_constructor_executor.main",
      "fraud_proofs/validation_trace/script_sources_stage_one_redeemer_head_map_executor.main",
      "fraud_proofs/validation_trace/script_sources_stage_one_redeemer_head_scalar_executor.main",
      "fraud_proofs/validation_trace/script_sources_stage_one_redeemer_head_sequence_executor.main",
      "fraud_proofs/validation_trace/script_sources_stage_one_redeemer_invalid_header_executor.main",
      "fraud_proofs/validation_trace/script_sources_stage_one_redeemer_invalid_tail_executor.main",
      "fraud_proofs/validation_trace/script_sources_stage_one_redeemer_open_header_executor.main",
      "fraud_proofs/validation_trace/script_sources_stage_one_redeemer_open_tail_executor.main",
      "fraud_proofs/validation_trace/script_sources_stage_one_redeemer_outer_normalizer_v1.main",
      "fraud_proofs/validation_trace/script_sources_stage_one_redeemer_semantic_v1.main",
      "fraud_proofs/validation_trace/script_sources_stage_one_redeemer_source_authenticator.main",
      "fraud_proofs/validation_trace/script_sources_stage_one_redeemer_traversal_normalizer_v1.main",
      "fraud_proofs/validation_trace/script_sources_stage_seven_finish_semantic_v1.main",
      "fraud_proofs/validation_trace/script_sources_stage_seven_observer_bound_yield_v1.main",
      "fraud_proofs/validation_trace/script_sources_stage_seven_observer_item_yield_v1.main",
      "fraud_proofs/validation_trace/script_sources_stage_seven_observer_semantic_v1.main",
      "fraud_proofs/validation_trace/script_sources_stage_seven_receive_semantic_v1.main",
      "fraud_proofs/validation_trace/script_sources_stage_ten_match_semantic_v1.main",
      "fraud_proofs/validation_trace/script_sources_stage_ten_mismatch_semantic_v1.main",
      "fraud_proofs/validation_trace/script_sources_stage_ten_missing_semantic_v1.main",
      "fraud_proofs/validation_trace/script_sources_stage_twelve_finish_semantic_v1.main",
      "fraud_proofs/validation_trace/script_sources_stage_twelve_redeemer_semantic_v1.main",
      "fraud_proofs/validation_trace/script_sources_stage_zero_begin_semantic_v1.main",
      "fraud_proofs/validation_trace/script_sources_stage_zero_finish_semantic_v1.main",
      "fraud_proofs/validation_trace/script_sources_stage_zero_hash_advance_semantic_v1.main",
      "fraud_proofs/validation_trace/script_sources_stage_zero_hash_block_semantic_v1.main",
      "fraud_proofs/validation_trace/script_sources_stage_zero_hash_terminal_semantic_v1.main",
      "fraud_proofs/validation_trace/script_sources_v1.main",
      "fraud_proofs/validation_trace/signatures_address_item_semantic_v1.main",
      "fraud_proofs/validation_trace/signatures_advance_semantic_v1.main",
      "fraud_proofs/validation_trace/signatures_handoff_semantic_v1.main",
      "fraud_proofs/validation_trace/signatures_required_item_semantic_v1.main",
      "fraud_proofs/validation_trace/signatures_v1.main",
      "fraud_proofs/validation_trace/source_v1.main",
      "fraud_proofs/validation_trace/static_ledger_rules_semantic_v1.main",
      "fraud_proofs/validation_trace/static_ledger_rules_v1.main",
      "fraud_proofs/validation_trace/timeout_v1.main",
      "fraud_proofs/validation_trace/value_and_mint_asset_fold_yield.main",
      "fraud_proofs/validation_trace/value_and_mint_begin_semantic_v1.main",
      "fraud_proofs/validation_trace/value_and_mint_finalize_semantic_v1.main",
      "fraud_proofs/validation_trace/value_and_mint_mint_asset_semantic_v1.main",
      "fraud_proofs/validation_trace/value_and_mint_mint_finish_semantic_v1.main",
      "fraud_proofs/validation_trace/value_and_mint_output_asset_semantic_v1.main",
      "fraud_proofs/validation_trace/value_and_mint_output_descriptor_semantic_v1.main",
      "fraud_proofs/validation_trace/value_and_mint_output_finish_semantic_v1.main",
      "fraud_proofs/validation_trace/value_and_mint_replay_asset_semantic_v1.main",
      "fraud_proofs/validation_trace/value_and_mint_replay_begin_semantic_v1.main",
      "fraud_proofs/validation_trace/value_and_mint_replay_finish_semantic_v1.main",
      "fraud_proofs/validation_trace/value_and_mint_replay_input_semantic_v1.main",
      "fraud_proofs/validation_trace/value_and_mint_v1.main",
    ],
  },
  {
    reason:
      "fault-proof step, yield or witness validator. Its family is mapped at family level, or listed as unmapped there; no scenario is mapped where this validator itself refuses.",
    validators: [
      "fraud_proofs/canonical_decodability/step_01.main",
      "fraud_proofs/canonical_decodability/step_02.main",
      "fraud_proofs/cross_block_duplicate_event/step_01.main",
      "fraud_proofs/cross_block_duplicate_event/step_02.main",
      "fraud_proofs/da_hash_preimage/step_01.main",
      "fraud_proofs/da_hash_preimage/step_02.main",
      "fraud_proofs/distinct_asset_accumulation_limit/step_01.main",
      "fraud_proofs/distinct_asset_accumulation_limit/step_02.main",
      "fraud_proofs/distinct_asset_accumulation_limit/step_03.main",
      "fraud_proofs/distinct_asset_accumulation_limit/step_04.main",
      "fraud_proofs/distinct_asset_accumulation_limit/step_05.main",
      "fraud_proofs/distinct_asset_accumulation_limit/step_06.main",
      "fraud_proofs/double_spend/step_01.main",
      "fraud_proofs/double_spend/step_02.main",
      "fraud_proofs/double_spend/step_03.main",
      "fraud_proofs/double_spend/step_04.main",
      "fraud_proofs/double_withdraw/step_01.main",
      "fraud_proofs/double_withdraw/step_02.main",
      "fraud_proofs/execution_native_script_invalid/accepted_inline_source.main",
      "fraud_proofs/execution_native_script_invalid/accepted_mint_prefix.main",
      "fraud_proofs/execution_native_script_invalid/accepted_observer_prefix.main",
      "fraud_proofs/execution_native_script_invalid/accepted_receive_prefix.main",
      "fraud_proofs/execution_native_script_invalid/accepted_reconstruction_init.main",
      "fraud_proofs/execution_native_script_invalid/accepted_reference_source.main",
      "fraud_proofs/execution_native_script_invalid/accepted_spend_prefix.main",
      "fraud_proofs/execution_native_script_invalid/step_01.main",
      "fraud_proofs/execution_native_script_invalid/step_02.main",
      "fraud_proofs/execution_native_script_invalid/step_03.main",
      "fraud_proofs/execution_native_script_invalid/step_04.main",
      "fraud_proofs/execution_native_script_invalid/step_05.main",
      "fraud_proofs/execution_native_script_invalid/step_06.main",
      "fraud_proofs/execution_source_script_decoding/step_01.main",
      "fraud_proofs/execution_source_script_decoding/step_02.main",
      "fraud_proofs/execution_source_script_decoding/step_03.main",
      "fraud_proofs/execution_source_script_decoding/step_04.main",
      "fraud_proofs/execution_source_script_decoding/step_05.main",
      "fraud_proofs/fabricated_deposit/step_01.main",
      "fraud_proofs/fabricated_deposit/step_02.main",
      "fraud_proofs/fabricated_deposit/step_03.main",
      "fraud_proofs/fabricated_deposit/step_04.main",
      "fraud_proofs/fabricated_withdrawal/step_01.main",
      "fraud_proofs/fabricated_withdrawal/step_02.main",
      "fraud_proofs/fabricated_withdrawal/step_03.main",
      "fraud_proofs/fabricated_withdrawal/step_04.main",
      "fraud_proofs/field_item_width_illegal/step_01.main",
      "fraud_proofs/field_item_width_illegal/step_03.main",
      "fraud_proofs/field_preimage_length_mismatch/step_01.main",
      "fraud_proofs/field_preimage_length_mismatch/step_02_accepted.main",
      "fraud_proofs/field_preimage_length_mismatch/step_02_forced.main",
      "fraud_proofs/field_preimage_length_mismatch/step_03.main",
      "fraud_proofs/input_no_idx/step_01.main",
      "fraud_proofs/input_no_idx/step_02.main",
      "fraud_proofs/input_no_idx/step_03.main",
      "fraud_proofs/input_no_idx/step_04.main",
      "fraud_proofs/input_set_uniqueness/step_01.main",
      "fraud_proofs/input_set_uniqueness/step_02.main",
      "fraud_proofs/input_set_uniqueness/step_03.main",
      "fraud_proofs/input_set_uniqueness/step_04.main",
      "fraud_proofs/invalid_range/step_01.main",
      "fraud_proofs/invalid_range/step_02.main",
      "fraud_proofs/invalid_signature/step_01.main",
      "fraud_proofs/l2_tx_mistag/step_01.main",
      "fraud_proofs/l2_tx_mistag/step_02.main",
      "fraud_proofs/min_ada/step_01.main",
      "fraud_proofs/min_ada/step_02.main",
      "fraud_proofs/min_ada/step_02_yields.tx",
      "fraud_proofs/min_ada/step_02_yields.utxo",
      "fraud_proofs/min_ada/step_03.main",
      "fraud_proofs/min_ada/step_04.main",
      "fraud_proofs/min_ada/step_05.main",
      "fraud_proofs/min_fee/step_01.main",
      "fraud_proofs/mint_authorization/evaluate.main",
      "fraud_proofs/mint_authorization/step_01.main",
      "fraud_proofs/mint_authorization/step_02.main",
      "fraud_proofs/mint_authorization/step_03.main",
      "fraud_proofs/mint_authorization/step_04.main",
      "fraud_proofs/mint_authorization/step_05.main",
      "fraud_proofs/mint_authorization/witness_scan.main",
      "fraud_proofs/mint_declared_asset_limit/step_01.main",
      "fraud_proofs/mint_declared_asset_limit/step_02.main",
      "fraud_proofs/mint_declared_asset_limit/step_03.main",
      "fraud_proofs/mint_declared_asset_limit/step_04.main",
      "fraud_proofs/mint_item_non_canonical/step_01.main",
      "fraud_proofs/mint_item_non_canonical/step_02.main",
      "fraud_proofs/mint_item_non_canonical/step_03.main",
      "fraud_proofs/mint_item_non_canonical/step_04.main",
      "fraud_proofs/missing_native_script_tx/step_01.main",
      "fraud_proofs/missing_native_script_tx/step_02.main",
      "fraud_proofs/missing_native_script_tx/step_03.main",
      "fraud_proofs/missing_native_script_tx/step_04.main",
      "fraud_proofs/missing_native_script_tx/step_05.main",
      "fraud_proofs/missing_native_script_tx/step_06.main",
      "fraud_proofs/missing_native_script_tx/step_07.main",
      "fraud_proofs/missing_native_script_tx/step_08.main",
      "fraud_proofs/missing_native_script_utxo/step_01.main",
      "fraud_proofs/missing_native_script_utxo/step_02.main",
      "fraud_proofs/missing_native_script_utxo/step_03.main",
      "fraud_proofs/missing_native_script_utxo/step_04.main",
      "fraud_proofs/missing_native_script_utxo/step_05.main",
      "fraud_proofs/missing_native_script_utxo/step_06.main",
      "fraud_proofs/missing_native_script_utxo/step_07.main",
      "fraud_proofs/missing_redeemer/step_01.main",
      "fraud_proofs/missing_redeemer/step_02.main",
      "fraud_proofs/missing_redeemer/step_02a.main",
      "fraud_proofs/missing_redeemer/step_02b.main",
      "fraud_proofs/missing_redeemer/step_03.main",
      "fraud_proofs/missing_redeemer/step_04.main",
      "fraud_proofs/missing_redeemer/step_05.main",
      "fraud_proofs/missing_script_source/step_01.main",
      "fraud_proofs/missing_script_source/step_02.main",
      "fraud_proofs/missing_script_source/step_03.main",
      "fraud_proofs/missing_script_source/step_04.main",
      "fraud_proofs/missing_script_source/step_05.main",
      "fraud_proofs/missing_script_source/step_06.main",
      "fraud_proofs/missing_signature/forced_signer.main",
      "fraud_proofs/missing_signature/forced_step.main",
      "fraud_proofs/missing_signature/forced_witness.main",
      "fraud_proofs/missing_signature/step_01.main",
      "fraud_proofs/missing_signature/step_02.main",
      "fraud_proofs/missing_signature/step_03.main",
      "fraud_proofs/mpf_chunked_proof/challenge.main",
      "fraud_proofs/native_script_decoding/step_01.main",
      "fraud_proofs/native_script_decoding/step_02.main",
      "fraud_proofs/native_script_decoding/step_03_advance_or_close.main",
      "fraud_proofs/native_script_decoding/step_03_bind_descriptor.main",
      "fraud_proofs/native_script_decoding/step_03_open_subject.main",
      "fraud_proofs/native_script_decoding/step_04.main",
      "fraud_proofs/native_script_invalid/step_01.main",
      "fraud_proofs/native_script_invalid/step_02.main",
      "fraud_proofs/native_script_invalid/step_03.main",
      "fraud_proofs/native_script_invalid/step_04.main",
      "fraud_proofs/native_script_invalid/step_05.main",
      "fraud_proofs/network_id/forced_scan.main",
      "fraud_proofs/network_id/forced_step.main",
      "fraud_proofs/network_id/step_01.main",
      "fraud_proofs/network_id/step_02.main",
      "fraud_proofs/no_input/step_01.main",
      "fraud_proofs/no_input/step_02.main",
      "fraud_proofs/no_input/step_03.main",
      "fraud_proofs/no_input/step_04.main",
      "fraud_proofs/no_reference_input/step_01.main",
      "fraud_proofs/no_reference_input/step_02.main",
      "fraud_proofs/no_reference_input/step_03.main",
      "fraud_proofs/no_reference_input/step_04.main",
      "fraud_proofs/observer_order_invalid/step_01.main",
      "fraud_proofs/observer_order_invalid/step_02.main",
      "fraud_proofs/observer_order_invalid/step_03.main",
      "fraud_proofs/observer_order_invalid/step_04.main",
      "fraud_proofs/observers_forbidden_on_untagged_network/step_01.main",
      "fraud_proofs/observers_forbidden_on_untagged_network/step_02.main",
      "fraud_proofs/output_reference_script_decoding/step_01.main",
      "fraud_proofs/output_reference_script_decoding/step_02.main",
      "fraud_proofs/output_reference_script_decoding/step_03.main",
      "fraud_proofs/output_reference_script_decoding/step_04.main",
      "fraud_proofs/output_reference_script_decoding/step_05.main",
      "fraud_proofs/output_reference_script_decoding/step_06.main",
      "fraud_proofs/protected_output_signer_missing/step_01.main",
      "fraud_proofs/protected_output_signer_missing/step_02.main",
      "fraud_proofs/protected_output_signer_missing/step_03.main",
      "fraud_proofs/protected_output_signer_missing/step_04.main",
      "fraud_proofs/protected_output_signer_missing/step_05.main",
      "fraud_proofs/receive_purpose_language/step_01.main",
      "fraud_proofs/receive_purpose_language/step_03.main",
      "fraud_proofs/redeemer_canonicity/step_01.main",
      "fraud_proofs/redeemer_canonicity/step_02.main",
      "fraud_proofs/redeemer_canonicity/step_03.main",
      "fraud_proofs/reference_input_no_idx/step_01.main",
      "fraud_proofs/reference_input_no_idx/step_02.main",
      "fraud_proofs/reference_input_no_idx/step_03.main",
      "fraud_proofs/reference_input_no_idx/step_04.main",
      "fraud_proofs/resolved_output_non_canonical/step_01.main",
      "fraud_proofs/resolved_output_non_canonical/step_02.main",
      "fraud_proofs/resolved_output_non_canonical/step_03.main",
      "fraud_proofs/resolved_output_non_canonical/step_04.main",
      "fraud_proofs/resolved_output_non_canonical/step_05.main",
      "fraud_proofs/script_integrity_hash_mismatch/step_01.main",
      "fraud_proofs/script_integrity_hash_mismatch/step_02.main",
      "fraud_proofs/script_integrity_hash_mismatch/step_03.main",
      "fraud_proofs/script_integrity_hash_mismatch/step_04.main",
      "fraud_proofs/script_integrity_hash_mismatch/step_05.main",
      "fraud_proofs/script_integrity_hash_missing/redeemer_grammar.main",
      "fraud_proofs/script_integrity_hash_missing/script_grammar.main",
      "fraud_proofs/script_integrity_hash_missing/script_scan.main",
      "fraud_proofs/script_integrity_hash_missing/step_01.main",
      "fraud_proofs/script_integrity_hash_missing/step_02.main",
      "fraud_proofs/script_integrity_hash_missing/step_03.main",
      "fraud_proofs/script_integrity_hash_missing/step_04.main",
      "fraud_proofs/spend_input_signer_missing/step_01.main",
      "fraud_proofs/spend_input_signer_missing/step_02.main",
      "fraud_proofs/spend_input_signer_missing/step_03.main",
      "fraud_proofs/spend_input_signer_missing/step_04.main",
      "fraud_proofs/spend_input_signer_missing/step_05.main",
      "fraud_proofs/transaction_output_non_canonical/step_01.main",
      "fraud_proofs/transaction_output_non_canonical/step_02.main",
      "fraud_proofs/transaction_output_non_canonical/step_03.main",
      "fraud_proofs/transition_trace/accepted_transaction_v1.main",
      "fraud_proofs/transition_trace/accepted_transaction_yields.claim_endpoints",
      "fraud_proofs/transition_trace/accepted_transaction_yields.claim_source",
      "fraud_proofs/transition_trace/accepted_transaction_yields.claim_structure",
      "fraud_proofs/transition_trace/accepted_transaction_yields.l2_open",
      "fraud_proofs/transition_trace/accepted_transaction_yields.l2_replay",
      "fraud_proofs/transition_trace/control_v1.main",
      "fraud_proofs/transition_trace/deposit_summaries.summaries",
      "fraud_proofs/transition_trace/deposit_v1.main",
      "fraud_proofs/transition_trace/deposit_value.value_output",
      "fraud_proofs/transition_trace/deposit_yields.projection",
      "fraud_proofs/transition_trace/duplicate_v1.main",
      "fraud_proofs/transition_trace/forced_v1.main",
      "fraud_proofs/transition_trace/l1_event_v1.main",
      "fraud_proofs/transition_trace/l1_event_yield.forced_timing",
      "fraud_proofs/transition_trace/l1_event_yield.timing",
      "fraud_proofs/transition_trace/output_assembly.assembly",
      "fraud_proofs/transition_trace/output_scan.scan_output",
      "fraud_proofs/transition_trace/output_summaries.summaries",
      "fraud_proofs/transition_trace/output_value.value_output",
      "fraud_proofs/transition_trace/route_v1.main",
      "fraud_proofs/transition_trace/source_v1.main",
      "fraud_proofs/transition_trace/withdrawal_v1.main",
      "fraud_proofs/unused_redeemer/step_01.main",
      "fraud_proofs/unused_redeemer/step_02.main",
      "fraud_proofs/unused_redeemer/step_02a.main",
      "fraud_proofs/unused_redeemer/step_02b.main",
      "fraud_proofs/unused_redeemer/step_02c.main",
      "fraud_proofs/unused_redeemer/step_03.main",
      "fraud_proofs/unused_redeemer/step_04.main",
      "fraud_proofs/unused_redeemer/step_05.main",
      "fraud_proofs/unused_redeemer/step_06.main",
      "fraud_proofs/unused_script_witness/step_01.main",
      "fraud_proofs/unused_script_witness/step_02.main",
      "fraud_proofs/unused_script_witness/step_03.main",
      "fraud_proofs/unused_script_witness/step_04.main",
      "fraud_proofs/unused_script_witness/step_05.main",
      "fraud_proofs/unused_script_witness/step_06.main",
      "fraud_proofs/value_not_preserved/step_01.main",
      "fraud_proofs/value_not_preserved/step_02.main",
      "fraud_proofs/value_not_preserved/step_03.main",
      "fraud_proofs/value_not_preserved/union_accepted_source.main",
      "fraud_proofs/value_not_preserved/union_assets.main",
      "fraud_proofs/value_not_preserved/union_event.main",
      "fraud_proofs/value_not_preserved/union_field_grammar.main",
      "fraud_proofs/value_not_preserved/union_forced_source.main",
      "fraud_proofs/value_not_preserved/union_input_value.main",
      "fraud_proofs/value_not_preserved/union_inputs.main",
      "fraud_proofs/value_not_preserved/union_mint.main",
      "fraud_proofs/value_not_preserved/union_output_scan.main",
      "fraud_proofs/value_not_preserved/union_outputs.main",
      "fraud_proofs/value_not_preserved/union_pre_state.main",
      "fraud_proofs/value_not_preserved/union_terminal.main",
      "fraud_proofs/value_not_preserved/union_update.main",
      "fraud_proofs/withdrawal_mistag/step_01.main",
      "fraud_proofs/withdrawal_mistag/step_02.main",
      "fraud_proofs/withdrawal_mistag/step_03.main",
      "fraud_proofs/withdrawal_mistag/step_04.main",
      "fraud_proofs/withdrawal_mistag/step_05.main",
      "fraud_proofs/withdrawn_input/step_01.main",
      "fraud_proofs/withdrawn_input/step_02.main",
      "fraud_proofs/withdrawn_input/step_03.main",
      "fraud_proofs/withdrawn_reference_input/step_01.main",
      "fraud_proofs/withdrawn_reference_input/step_02.main",
      "fraud_proofs/witness_script_decoding/step_01.main",
      "fraud_proofs/witness_script_decoding/step_02.main",
      "fraud_proofs/witness_script_decoding/step_03.main",
      "fraud_proofs/witness_script_decoding/step_04.main",
      "fraud_proofs/zero_input/step_01.main",
      "fraud_proofs/zero_input/step_02.main",
    ],
  },
  {
    reason:
      "protocol validator with no emulator scenario pair mapped yet. Happy paths for several exist (deposit, payout, scheduler and state-queue flows in midgard-node), but none was mapped to a refusal this validator performs.",
    validators: [
      "availability_challenge.availability_challenge",
      "availability_challenge_yields.bond",
      "availability_challenge_yields.close",
      "availability_challenge_yields.open",
      "availability_challenge_yields.settle",
      "availability_challenge_yields.timeout",
      "computation_thread.mint",
      "correction_lock.spend",
      "da_attestation.da_attestation",
      "da_params_governor.da_params_governor",
      "field_preimage_certificate.field_preimage_certificate",
      "fraud_proof.mint",
      "fraud_proof.spend",
      "fraud_proof_catalogue.mint",
      "fraud_proof_catalogue.spend",
      "hub_oracle.mint",
      "mpf_chunked_verify.verify",
      "operator_directory/active_operators.mint",
      "operator_directory/registered_operators.mint",
      "operator_directory/retired_operators.mint",
      "payout.mint",
      "payout.spend",
      "pexcludes.exclusion",
      "phas.membership",
      "reserve.spend",
      "reserve.withdraw",
      "scheduler.mint",
      "scheduler.spend",
      "settlement.mint",
      "settlement.spend",
      "state_queue.mint",
      "state_queue.spend",
      "state_queue_yields.commit",
      "state_queue_yields.merge",
      "state_queue_yields.remove_fraudulent",
      "state_queue_yields.remove_unavailable",
      "user_events/cek_program_material_v1.spend",
      "user_events/deposit.mint",
      "user_events/deposit.spend",
      "user_events/history.history",
      "user_events/history_data.retention",
      "user_events/history_retirement.retirement_observer",
      "user_events/tx_order_v1.mint",
      "user_events/tx_order_v1.spend",
      "user_events/withdrawal.mint",
      "user_events/withdrawal.spend",
      "user_events/witness.main",
    ],
  },
];

export const UNMAPPED_FAMILIES: readonly Readonly<{
  reason: string;
  families: readonly FraudProofCatalogueCategoryName[];
}>[] = [
  {
    reason:
      "No emulator scenario found in which the family's validators refuse an honest block; its negatives are off-chain (planner, submitter or classifier refusals) or absent.",
    families: [
      "doubleSpend",
      "nonExistentInputNoIndex",
      "invalidRange",
      "zeroInput",
      "crossBlockDuplicateEvent",
      "missingNativeScriptUtxo",
      "redeemerCanonicity",
      "unusedRedeemer",
      "withdrawalMistag",
    ],
  },
];

/** Only ever lowered. */
export const UNMAPPED_VALIDATOR_COUNT = 565;
/** Only ever lowered. */
export const UNMAPPED_FAMILY_COUNT = 9;
