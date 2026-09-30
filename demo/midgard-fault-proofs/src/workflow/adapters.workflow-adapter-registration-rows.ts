import {
  manual,
  type WorkflowAdapterRegistration,
} from "./adapters.freeze-registration.js";

/**
 * Exact fail-closed registration audit for the current canonical catalogue.
 *
 * A directory full of submitters is not called a production workflow adapter.
 * Registration requires all Q51 properties at once: canonical prepared input,
 * per-transaction durable intent before network submission, local UPLC
 * evaluation, reference-script identity, authenticated reconciliation, and
 * chain-state resume. This list names the concrete gap for every family rather
 * than installing a permissive generic adapter.
 */
export const workflowAdapterRegistrationRows = [
  {
    category: "doubleSpend",
    status: "missing",
    reason: "constrained_adapter_is_not_launch_scope_complete",
    existingSurface: [
      "workflow/double-spend-adapter.ts",
      "prepare-double-spend.ts",
      "submit-init.ts",
      "double-spend/submit-step-01.ts..double-spend/submit-step-04.ts",
      "remove-fraudulent-block.ts",
      "workflow/local-kupmios-http-ogmios-source.ts",
      "workflow/raw-l1-family-derivation.ts",
      "workflow/runtime.ts#WORKFLOW_RUNNER_FACTORIES.doubleSpend",
    ],
    requiredClosure:
      "install the manifest-bound runner in a compiled application with a concrete public retained-DA libp2p transport/runtime-config loader; the fault-proofs package has no libp2p runtime dependency and cannot honestly self-register it",
  },
  {
    category: "nonExistentInput",
    status: "missing",
    reason: "constrained_adapter_is_not_launch_scope_complete",
    existingSurface: [
      "workflow/ledger-absence-artifact.ts",
      "workflow/non-existent-input.ts",
      "workflow/manifest-bound-family-assembly.ts",
      "workflow/runtime.ts#WORKFLOW_RUNNER_FACTORIES.nonExistentInput",
      "workflow/linear-family-definitions.ts#LINEAR_FAMILY_DEFINITIONS.nonExistentInput",
      "non-existent-input/submit-step-01.ts..non-existent-input/submit-step-04.ts",
      "remove-fraudulent-block.ts",
    ],
    requiredClosure:
      "install and exercise the manifest-bound non-existent-input runner in the compiled application with authenticated predecessor replay, its exact reference roster, and public retained-DA runtime",
  },
  {
    category: "nonExistentInputNoIndex",
    status: "missing",
    reason: "constrained_adapter_is_not_launch_scope_complete",
    existingSurface: [
      "workflow/input-no-idx.ts",
      "workflow/manifest-bound-family-assembly.ts",
      "workflow/runtime.ts#WORKFLOW_RUNNER_FACTORIES.nonExistentInputNoIndex",
      "workflow/linear-family-definitions.ts#LINEAR_FAMILY_DEFINITIONS.nonExistentInputNoIndex",
      "submit-input-no-idx-step-01.ts..submit-input-no-idx-step-04.ts",
      "remove-fraudulent-block.ts",
    ],
    requiredClosure:
      "install and exercise the manifest-bound input-no-idx runner in the compiled application with its exact reference roster and public retained-DA runtime",
  },
  {
    category: "invalidRange",
    status: "missing",
    reason: "constrained_adapter_is_not_launch_scope_complete",
    existingSurface: [
      "workflow/native-inclusion-two-step.ts",
      "workflow/manifest-bound-family-assembly.ts",
      "workflow/proof-chunk-prerequisite.ts",
      "workflow/runtime.ts#WORKFLOW_RUNNER_FACTORIES.invalidRange",
      "workflow/linear-family-definitions.ts#LINEAR_FAMILY_DEFINITIONS.invalidRange",
      "submit-init.ts",
      "submit-invalid-range-step-01.ts..submit-invalid-range-step-02.ts",
      "remove-fraudulent-block.ts",
    ],
    requiredClosure:
      "install and exercise the exact manifest-bound runner in a compiled application with the concrete public retained-DA runtime loader",
  },
  manual("transitionTrace", [
    "transition-trace/detect.ts",
    "prepare-transition-trace.ts",
    "submit-transition-trace-proof.ts",
  ]),
  {
    category: "zeroInput",
    status: "missing",
    reason: "constrained_adapter_is_not_launch_scope_complete",
    existingSurface: [
      "workflow/native-inclusion-two-step.ts",
      "workflow/manifest-bound-family-assembly.ts",
      "workflow/proof-chunk-prerequisite.ts",
      "workflow/runtime.ts#WORKFLOW_RUNNER_FACTORIES.zeroInput",
      "workflow/linear-family-definitions.ts#LINEAR_FAMILY_DEFINITIONS.zeroInput",
      "submit-init.ts",
      "submit-zero-input-step-01.ts..submit-zero-input-step-02.ts",
      "remove-fraudulent-block.ts",
    ],
    requiredClosure:
      "install and exercise the exact manifest-bound runner in a compiled application with the concrete public retained-DA runtime loader",
  },
  {
    category: "validationTraceDispute",
    status: "missing",
    reason: "constrained_adapter_is_not_launch_scope_complete",
    existingSurface: [
      "workflow/family-application-registry.ts#VALIDATION_TRACE_DISPUTE_FAMILY_APPLICATION_RECORD",
    ],
    requiredClosure:
      "install the manifest-bound interactive-dispute runner in the compiled watcher application with authenticated retained DA",
  },
  {
    category: "daHashPreimage",
    status: "missing",
    reason: "constrained_adapter_is_not_launch_scope_complete",
    existingSurface: [
      "evidence/fraud-proof-evidence.ts",
      "workflow/da-hash-preimage.ts",
      "workflow/manifest-bound-family-assembly.ts",
      "workflow/runtime.ts#WORKFLOW_RUNNER_FACTORIES.daHashPreimage",
      "workflow/linear-family-definitions.ts#LINEAR_FAMILY_DEFINITIONS.daHashPreimage",
      "submit-init.ts",
      "submit-da-hash-preimage-step-01.ts..submit-da-hash-preimage-step-02.ts",
      "remove-fraudulent-block.ts",
    ],
    requiredClosure:
      "install and exercise the manifest-bound Q44 runner in a compiled application with the concrete public retained-DA libp2p runtime loader",
  },
  {
    category: "noReferenceInput",
    status: "missing",
    reason: "constrained_adapter_is_not_launch_scope_complete",
    existingSurface: [
      "workflow/no-reference-input.ts",
      "workflow/manifest-bound-family-assembly.ts",
      "workflow/ledger-absence-artifact.ts",
      "workflow/runtime.ts#WORKFLOW_RUNNER_FACTORIES.noReferenceInput",
      "workflow/linear-family-definitions.ts#LINEAR_FAMILY_DEFINITIONS.noReferenceInput",
      "submit-no-reference-input-step-01.ts..submit-no-reference-input-step-04.ts",
      "remove-fraudulent-block.ts",
    ],
    requiredClosure:
      "install and exercise the manifest-bound no-reference-input runner in the compiled application with authenticated predecessor replay, its exact reference roster, and public retained-DA runtime",
  },
  {
    category: "referenceInputNoIdx",
    status: "missing",
    reason: "constrained_adapter_is_not_launch_scope_complete",
    existingSurface: [
      "workflow/reference-input-no-idx.ts",
      "workflow/manifest-bound-family-assembly.ts",
      "workflow/runtime.ts#WORKFLOW_RUNNER_FACTORIES.referenceInputNoIdx",
      "workflow/linear-family-definitions.ts#LINEAR_FAMILY_DEFINITIONS.referenceInputNoIdx",
      "submit-reference-input-no-idx-step-01.ts..submit-reference-input-no-idx-step-04.ts",
      "remove-fraudulent-block.ts",
    ],
    requiredClosure:
      "install and exercise the manifest-bound reference-input-no-idx runner in a compiled application with the concrete public retained-DA libp2p runtime loader",
  },
  {
    category: "invalidSignature",
    status: "missing",
    reason: "constrained_adapter_is_not_launch_scope_complete",
    existingSurface: [
      "workflow/invalid-signature.ts",
      "workflow/manifest-bound-family-assembly.ts",
      "workflow/runtime.ts#WORKFLOW_RUNNER_FACTORIES.invalidSignature",
      "workflow/linear-family-definitions.ts#LINEAR_FAMILY_DEFINITIONS.invalidSignature",
      "submit-invalid-signature-step-01.ts..submit-invalid-signature-step-02.ts",
      "remove-fraudulent-block.ts",
    ],
    requiredClosure:
      "install and exercise the manifest-bound invalid-signature runner in the compiled application with its exact reference roster and public retained-DA runtime",
  },
  {
    category: "fabricatedDeposit",
    status: "missing",
    reason: "constrained_adapter_is_not_launch_scope_complete",
    existingSurface: [
      "workflow/fabricated-deposit-evidence.ts",
      "workflow/fabricated-deposit.ts",
      "workflow/manifest-bound-family-assembly.ts",
      "workflow/runtime.ts#WORKFLOW_RUNNER_FACTORIES.fabricatedDeposit",
      "workflow/linear-family-definitions.ts#LINEAR_FAMILY_DEFINITIONS.fabricatedDeposit",
      "submit-fabricated-deposit-step-01.ts..submit-fabricated-deposit-step-04.ts",
      "remove-fraudulent-block.ts",
    ],
    requiredClosure:
      "install and exercise the manifest-bound fabricated-deposit runner in a compiled application with its public L1 event authority and retained-DA runtime",
  },
  {
    category: "fabricatedWithdrawal",
    status: "missing",
    reason: "constrained_adapter_is_not_launch_scope_complete",
    existingSurface: [
      "workflow/fabricated-withdrawal-evidence.ts",
      "workflow/fabricated-withdrawal.ts",
      "workflow/manifest-bound-family-assembly.ts",
      "workflow/runtime.ts#WORKFLOW_RUNNER_FACTORIES.fabricatedWithdrawal",
      "workflow/linear-family-definitions.ts#LINEAR_FAMILY_DEFINITIONS.fabricatedWithdrawal",
      "submit-fabricated-withdrawal-step-01.ts..submit-fabricated-withdrawal-step-04.ts",
      "remove-fraudulent-block.ts",
    ],
    requiredClosure:
      "install and exercise the manifest-bound fabricated-withdrawal runner in a compiled application with its public L1 event authority and retained-DA runtime",
  },
  manual("nativeScriptDecoding", [
    "native-script-decoding/replay.ts",
    "native-script-decoding/artifact.ts",
    "native-script-decoding/workflow.ts",
    "workflow/family-application-registry.ts#NATIVE_SCRIPT_DECODING_FAMILY_APPLICATION_RECORD",
  ]),
  {
    category: "missingSignature",
    status: "missing",
    reason: "constrained_adapter_is_not_launch_scope_complete",
    existingSurface: [
      "workflow/missing-signature-state.ts",
      "workflow/missing-signature-adapter.ts",
      "workflow/missing-signature.ts",
      "workflow/family-application-registry.ts#MISSING_SIGNATURE_FAMILY_APPLICATION_RECORD",
      "missing-signature/submit-missing-signature-init.ts",
      "missing-signature/submit-missing-signature-step-01.ts..step-04.ts",
      "remove-fraudulent-block.ts",
    ],
    requiredClosure:
      "install and exercise the manifest-bound missing-signature runner in a compiled application with the concrete public retained-DA libp2p runtime loader",
  },
  manual("missingNativeScriptTx", [
    "missing-native-script-tx/prepare.ts",
    "missing-native-script-tx/submit-missing-native-script-tx-step-01.ts..step-06.ts",
  ]),
  {
    category: "withdrawnReferenceInput",
    status: "missing",
    reason: "constrained_adapter_is_not_launch_scope_complete",
    existingSurface: [
      "workflow/withdrawn-reference-input.ts",
      "workflow/manifest-bound-family-assembly.ts",
      "workflow/runtime.ts#WORKFLOW_RUNNER_FACTORIES.withdrawnReferenceInput",
      "workflow/linear-family-definitions.ts#LINEAR_FAMILY_DEFINITIONS.withdrawnReferenceInput",
      "withdrawn-reference-input/prepare-withdrawn-reference-input.ts",
      "withdrawn-reference-input/submit-withdrawn-reference-input-init.ts",
      "withdrawn-reference-input/submit-withdrawn-reference-input-step-01.ts..step-03.ts",
      "remove-fraudulent-block.ts",
    ],
    requiredClosure:
      "install and exercise the manifest-bound withdrawn-reference-input runner in a compiled application with authenticated field carriage and public retained-DA runtime",
  },
  {
    category: "canonicalDecodability",
    status: "missing",
    reason: "constrained_adapter_is_not_launch_scope_complete",
    existingSurface: [
      "evidence/canonical-decodability-raw-evidence.ts",
      "workflow/canonical-decodability.ts",
      "workflow/manifest-bound-family-assembly.ts",
      "workflow/field-carriage-prerequisite.ts",
      "workflow/runtime.ts#WORKFLOW_RUNNER_FACTORIES.canonicalDecodability",
      "workflow/linear-family-definitions.ts#LINEAR_FAMILY_DEFINITIONS.canonicalDecodability",
      "canonical-decodability/submit-canonical-decodability-init.ts",
      "canonical-decodability/submit-canonical-decodability-step-01.ts..step-02.ts",
      "remove-fraudulent-block.ts",
    ],
    requiredClosure:
      "install and exercise the manifest-bound canonical-decodability runner in a compiled application with the concrete public retained-DA runtime loader",
  },
  {
    category: "committedFieldShape",
    status: "missing",
    reason: "constrained_adapter_is_not_launch_scope_complete",
    existingSurface: [
      "workflow/committed-field-shape.ts",
      "workflow/manifest-bound-family-assembly.ts",
      "workflow/runtime.ts#WORKFLOW_RUNNER_FACTORIES.committedFieldShape",
      "workflow/linear-family-definitions.ts#LINEAR_FAMILY_DEFINITIONS.committedFieldShape",
      "committed-field-shape/submit-committed-field-shape-init.ts",
      "committed-field-shape/submit-committed-field-shape-step-01.ts..step-02.ts",
      "remove-fraudulent-block.ts",
    ],
    requiredClosure:
      "install and exercise the manifest-bound committed-field-shape runner in a compiled application with the concrete public retained-DA libp2p runtime loader",
  },
  {
    category: "minFee",
    status: "missing",
    reason: "constrained_adapter_is_not_launch_scope_complete",
    existingSurface: [
      "workflow/min-fee.ts",
      "workflow/manifest-bound-family-assembly.ts",
      "workflow/field-carriage-prerequisite.ts",
      "workflow/runtime.ts#WORKFLOW_RUNNER_FACTORIES.minFee",
      "workflow/linear-family-definitions.ts#LINEAR_FAMILY_DEFINITIONS.minFee",
      "prepare-min-fee.ts",
      "submit-min-fee-init.ts",
      "submit-min-fee-step-01.ts..submit-min-fee-step-02.ts",
      "remove-fraudulent-block.ts",
    ],
    requiredClosure:
      "install and exercise the manifest-bound min-fee runner in a compiled application with the concrete public retained-DA runtime loader",
  },
  {
    category: "withdrawalMistag",
    status: "missing",
    reason: "constrained_adapter_is_not_launch_scope_complete",
    existingSurface: [
      "withdrawal-mistag/workflow.ts",
      "withdrawal-mistag/replay.ts",
    ],
    requiredClosure:
      "install and exercise the manifest-bound withdrawal-mistag runner with retained history",
  },
  {
    category: "doubleWithdraw",
    status: "missing",
    reason: "constrained_adapter_is_not_launch_scope_complete",
    existingSurface: [
      "workflow/double-withdraw.ts",
      "workflow/manifest-bound-family-assembly.ts",
      "workflow/runtime.ts#WORKFLOW_RUNNER_FACTORIES.doubleWithdraw",
      "workflow/linear-family-definitions.ts#LINEAR_FAMILY_DEFINITIONS.doubleWithdraw",
      "double-withdraw/submit-double-withdraw-init.ts",
      "double-withdraw/submit-double-withdraw-step-01.ts..step-02.ts",
      "remove-fraudulent-block.ts",
    ],
    requiredClosure:
      "install and exercise the manifest-bound double-withdraw runner in a compiled application with the concrete public retained-DA libp2p runtime loader",
  },
  {
    category: "crossBlockDuplicateEvent",
    status: "missing",
    reason: "constrained_adapter_is_not_launch_scope_complete",
    existingSurface: [
      "cross-block-duplicate-event/workflow.ts",
      "cross-block-duplicate-event/settlement-authority.ts",
      "remove-fraudulent-block.ts",
    ],
    requiredClosure:
      "install and exercise the manifest-bound cross-block runner with authenticated live settlement NFT history and retained public DA",
  },
  {
    category: "l2TxMistag",
    status: "missing",
    reason: "constrained_adapter_is_not_launch_scope_complete",
    existingSurface: [
      "workflow/l2-tx-mistag.ts",
      "workflow/manifest-bound-family-assembly.ts",
      "workflow/proof-chunk-prerequisite.ts",
      "workflow/runtime.ts#WORKFLOW_RUNNER_FACTORIES.l2TxMistag",
      "workflow/linear-family-definitions.ts#LINEAR_FAMILY_DEFINITIONS.l2TxMistag",
      "l2-tx-mistag/prepare-l2-tx-mistag.ts",
      "l2-tx-mistag/submit-l2-tx-mistag-init.ts",
      "l2-tx-mistag/submit-l2-tx-mistag-step-01.ts..step-02.ts",
      "remove-fraudulent-block.ts",
    ],
    requiredClosure:
      "install and exercise the manifest-bound l2-tx-mistag runner in a compiled application with the concrete public retained-DA libp2p runtime loader",
  },
  {
    category: "withdrawnInput",
    status: "missing",
    reason: "constrained_adapter_is_not_launch_scope_complete",
    existingSurface: [
      "workflow/withdrawn-input.ts",
      "workflow/manifest-bound-family-assembly.ts",
      "workflow/runtime.ts#WORKFLOW_RUNNER_FACTORIES.withdrawnInput",
      "workflow/linear-family-definitions.ts#LINEAR_FAMILY_DEFINITIONS.withdrawnInput",
      "withdrawn-input/evidence.ts",
      "withdrawn-input/submit-withdrawn-input-init.ts",
      "withdrawn-input/submit-withdrawn-input-step-01.ts..step-03.ts",
      "remove-fraudulent-block.ts",
    ],
    requiredClosure:
      "install and exercise the manifest-bound withdrawn-input runner in a compiled application with its exact proof/field publication roster and public retained-DA runtime",
  },
  {
    category: "valueNotPreserved",
    status: "missing",
    reason: "constrained_adapter_is_not_launch_scope_complete",
    existingSurface: [
      "value-not-preserved/workflow.ts",
      "workflow/family-application-registry.ts#VALUE_NOT_PRESERVED_FAMILY_APPLICATION_RECORD",
      "value-not-preserved/artifact.ts",
      "value-not-preserved/field-prerequisite.ts",
      "value-not-preserved/submit-union.ts",
      "remove-fraudulent-block.ts",
    ],
    requiredClosure:
      "install the manifest-bound value-conservation runner with its complete reference roster and public retained-DA runtime in the compiled application",
  },
  {
    category: "inputSetUniqueness",
    status: "missing",
    reason: "constrained_adapter_is_not_launch_scope_complete",
    existingSurface: [
      "workflow/input-set-uniqueness.ts",
      "workflow/manifest-bound-family-assembly.ts",
      "workflow/runtime.ts#WORKFLOW_RUNNER_FACTORIES.inputSetUniqueness",
      "workflow/linear-family-definitions.ts#LINEAR_FAMILY_DEFINITIONS.inputSetUniqueness",
      "input-set-uniqueness/scan.ts",
      "input-set-uniqueness/submit-input-set-uniqueness-init.ts",
      "input-set-uniqueness/submit-input-set-uniqueness-step-01.ts..step-02.ts",
      "remove-fraudulent-block.ts",
    ],
    requiredClosure:
      "install and exercise the manifest-bound input-set-uniqueness runner in a compiled application with its exact proof/field publication roster and public retained-DA runtime",
  },
  {
    category: "mintAuthorization",
    status: "missing",
    reason: "detector_or_scanner_only",
    existingSurface: [
      "mint-authorization/prover.ts",
      "mint-authorization/submit-mint-authorization-step-01.ts..step-05.ts",
    ],
    requiredClosure:
      "add a complete chain-state driver over the scan finding and all five submit steps",
  },
  {
    category: "networkId",
    status: "missing",
    reason: "constrained_adapter_is_not_launch_scope_complete",
    existingSurface: [
      "network-id/workflow-adapter.ts",
      "network-id/prepare.ts",
      "network-id/submit-network-id-init.ts",
      "network-id/submit-network-id-step-01.ts..submit-network-id-step-02.ts",
      "network-id/submit-network-id-forced-step-01.ts",
      "network-id/submit-network-id-forced-bind.ts",
      "network-id/wrongful-rejection.ts",
      "workflow/local-kupmios-http-ogmios-source.ts",
      "workflow/raw-l1-family-derivation.ts",
      "workflow/family-application-registry.ts#NETWORK_ID_FAMILY_APPLICATION_RECORD",
    ],
    requiredClosure:
      "install the manifest-bound runner in a compiled application with a concrete public retained-DA libp2p transport/runtime-config loader; the fault-proofs package has no libp2p runtime dependency and cannot honestly self-register it",
  },
  manual("missingNativeScriptUtxo", [
    "missing-native-script-utxo/prepare.ts",
    "missing-native-script-utxo/submit-missing-native-script-utxo-step-01.ts..step-05.ts",
  ]),
  manual("nativeScriptInvalid", [
    "native-script-invalid/prepare.ts",
    "native-script-invalid/submit-native-script-invalid-step-01.ts..step-03.ts",
  ]),
  manual("minAda", [
    "min-ada-v1 SDK wire schema",
    "min-ada deployed step-01/step-02 validators",
  ]),
  {
    category: "fieldPreimageLengthMismatch",
    status: "missing",
    reason: "partial_resume_surface_has_no_complete_driver",
    existingSurface: [
      "field-preimage-length-mismatch/config.ts",
      "field-preimage-length-mismatch/workflow.ts",
    ],
    requiredClosure:
      "install and lifecycle-prove both manifest-bound direction branches through canonical removal",
  },
  {
    category: "fieldItemWidthIllegal",
    status: "missing",
    reason: "partial_resume_surface_has_no_complete_driver",
    existingSurface: [
      "field-item-width-illegal/contracts.ts",
      "field-item-width-illegal/field-item-width-illegal.ts",
    ],
    requiredClosure:
      "install and lifecycle-prove the manifest-bound three-step driver through canonical removal",
  },
  {
    category: "witnessScriptDecoding",
    status: "missing",
    reason: "partial_resume_surface_has_no_complete_driver",
    existingSurface: ["witness-script-decoding/workflow.ts"],
    requiredClosure:
      "install and lifecycle-prove the resumable manifest-bound structural scan through canonical removal",
  },
  {
    category: "scriptIntegrityHashMissing",
    status: "missing",
    reason: "partial_resume_surface_has_no_complete_driver",
    existingSurface: ["script-integrity-hash-missing/family.ts"],
    requiredClosure:
      "install and lifecycle-prove the seven-script staged driver through canonical removal",
  },
  {
    category: "transactionOutputNonCanonical",
    status: "missing",
    reason: "constrained_adapter_is_not_launch_scope_complete",
    existingSurface: [
      "transaction-output-non-canonical/workflow.ts",
      "workflow/runtime.ts#createTransactionOutputNonCanonicalProductionWorkflowRunnerV1",
    ],
    requiredClosure:
      "install and exercise the manifest-bound four-step runner in the compiled watcher application with public retained DA",
  },
  {
    category: "resolvedOutputNonCanonical",
    status: "missing",
    reason: "constrained_adapter_is_not_launch_scope_complete",
    existingSurface: [
      "resolved-output-non-canonical production runner surface is centrally installed",
    ],
    requiredClosure:
      "replace this static readiness row with the admitted central runner after the Wave 2 integration gate",
  },
  {
    category: "mintDeclaredAssetLimit",
    status: "missing",
    reason: "constrained_adapter_is_not_launch_scope_complete",
    existingSurface: [
      "mint-declared-asset-limit production runner surface is centrally installed",
    ],
    requiredClosure:
      "replace this static readiness row with the admitted central runner after the Wave 2 integration gate",
  },
  {
    category: "spendInputSignerMissing",
    status: "missing",
    reason: "constrained_adapter_is_not_launch_scope_complete",
    existingSurface: [
      "spend-input-signer-missing production runner surface is centrally installed",
    ],
    requiredClosure:
      "replace this static readiness row with the admitted central runner after the Wave 3 integration gate",
  },
  {
    category: "protectedOutputSignerMissing",
    status: "missing",
    reason: "constrained_adapter_is_not_launch_scope_complete",
    existingSurface: [
      "protected-output-signer-missing production runner surface is centrally installed",
    ],
    requiredClosure:
      "replace this static readiness row with the admitted central runner after the Wave 3 integration gate",
  },
  {
    category: "observersForbiddenOnUntaggedNetwork",
    status: "missing",
    reason: "constrained_adapter_is_not_launch_scope_complete",
    existingSurface: [
      "observers-forbidden-on-untagged-network production runner surface is centrally installed",
    ],
    requiredClosure:
      "replace this static readiness row with the admitted central runner after the Wave 3 integration gate",
  },
  {
    category: "observerOrderInvalid",
    status: "missing",
    reason: "constrained_adapter_is_not_launch_scope_complete",
    existingSurface: [
      "observer-order-invalid production runner surface is centrally installed",
    ],
    requiredClosure:
      "install the manifest-bound four-script runner in the compiled watcher application with authenticated retained DA",
  },
  {
    category: "redeemerCanonicity",
    status: "missing",
    reason: "constrained_adapter_is_not_launch_scope_complete",
    existingSurface: [
      "redeemer-canonicity production runner surface is centrally installed",
    ],
    requiredClosure:
      "install the manifest-bound three-script runner in the compiled watcher application with authenticated retained DA",
  },
  {
    category: "outputReferenceScriptDecoding",
    status: "missing",
    reason: "constrained_adapter_is_not_launch_scope_complete",
    existingSurface: [
      "output-reference-script-decoding production runner surface is centrally installed",
    ],
    requiredClosure:
      "install the manifest-bound six-script runner in the compiled watcher application with public retained DA",
  },
  {
    category: "executionSourceScriptDecoding",
    status: "missing",
    reason: "constrained_adapter_is_not_launch_scope_complete",
    existingSurface: [
      "execution-source-script-decoding production runner surface is centrally installed",
    ],
    requiredClosure:
      "install the manifest-bound five-script runner in the compiled watcher application with authenticated retained validation witnesses",
  },
  {
    category: "receivePurposeLanguage",
    status: "missing",
    reason: "constrained_adapter_is_not_launch_scope_complete",
    existingSurface: [
      "receive-purpose-language/manifest-workflow.ts",
      "workflow/manifest-bound-family-assembly.ts",
      "receive-purpose-language production runner surface is centrally installed",
    ],
    requiredClosure:
      "install the manifest-bound three-script runner in the compiled watcher application with authenticated retained DA",
  },
  {
    category: "unusedScriptWitness",
    status: "missing",
    reason: "constrained_adapter_is_not_launch_scope_complete",
    existingSurface: [
      "unused-script-witness production runner surface is being centrally installed",
    ],
    requiredClosure:
      "install the manifest-bound six-script runner in the compiled watcher application with authenticated retained DA",
  },
  {
    category: "missingScriptSource",
    status: "missing",
    reason: "constrained_adapter_is_not_launch_scope_complete",
    existingSurface: [
      "missing-script-source production runner surface is being centrally installed",
    ],
    requiredClosure:
      "install the manifest-bound six-script runner in the compiled watcher application with authenticated retained DA",
  },
  {
    category: "missingRedeemer",
    status: "missing",
    reason: "constrained_adapter_is_not_launch_scope_complete",
    existingSurface: [
      "missing-redeemer production runner surface is being centrally installed",
    ],
    requiredClosure:
      "install the manifest-bound seven-script runner in the compiled watcher application with authenticated retained DA",
  },
  {
    category: "unusedRedeemer",
    status: "missing",
    reason: "constrained_adapter_is_not_launch_scope_complete",
    existingSurface: [
      "unused-redeemer production runner surface is being centrally installed",
    ],
    requiredClosure:
      "install the manifest-bound nine-script runner in the compiled watcher application with authenticated retained DA",
  },
  {
    category: "executionNativeScriptInvalid",
    status: "missing",
    reason: "constrained_adapter_is_not_launch_scope_complete",
    existingSurface: [
      "execution-native-script-invalid transaction-driving 13-script production runner surface is centrally installed",
    ],
    requiredClosure:
      "retain the manifest-bound 13-script runner in the compiled watcher application with authenticated retained DA and historical L1 state",
  },
  {
    category: "scriptIntegrityHashMismatch",
    status: "missing",
    reason: "constrained_adapter_is_not_launch_scope_complete",
    existingSurface: [
      "script-integrity-hash-mismatch production runner surface is being centrally installed",
    ],
    requiredClosure:
      "install the manifest-bound five-script runner in the compiled watcher application with authenticated retained DA",
  },
  {
    category: "distinctAssetAccumulationLimit",
    status: "missing",
    reason: "constrained_adapter_is_not_launch_scope_complete",
    existingSurface: [
      "distinct-asset-accumulation-limit production runner surface is being centrally installed",
    ],
    requiredClosure:
      "install the manifest-bound six-script runner in the compiled watcher application with authenticated retained DA",
  },
  {
    category: "mintItemNonCanonical",
    status: "missing",
    reason: "constrained_adapter_is_not_launch_scope_complete",
    existingSurface: ["mint-item-non-canonical/workflow.ts"],
    requiredClosure:
      "install and exercise the manifest-bound four-step runner with public retained DA",
  },
] as const satisfies readonly WorkflowAdapterRegistration[];
