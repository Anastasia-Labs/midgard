import type { FraudProofCatalogueCategoryName } from "@al-ft/midgard-sdk";

import { type ValidatorScenarioPair } from "./validator-scenario-registry.validator-scenarios.js";

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
        test: "refuses to convict a retained commitment whose selected reference input exists at its event",
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
