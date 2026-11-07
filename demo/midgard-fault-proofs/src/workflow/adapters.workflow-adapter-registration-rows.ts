import {
  FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER,
  type FraudProofCatalogueCategoryName,
} from "@al-ft/midgard-sdk";

import {
  manual,
  type MissingWorkflowAdapterRegistration,
  type WorkflowAdapterRegistration,
} from "./adapters.freeze-registration.js";

/** A family's missing-adapter row; the reason defaults to the common one. */
type MissingRow = Readonly<
  Partial<Pick<MissingWorkflowAdapterRegistration, "reason">> &
    Pick<
      MissingWorkflowAdapterRegistration,
      "existingSurface" | "requiredClosure"
    >
>;

/** Each family's row, keyed by category; the rows follow catalogue order. */
const missingRows: {
  readonly [Category in FraudProofCatalogueCategoryName]: MissingRow;
} = {
  doubleSpend: {
    existingSurface: [
      "workflow/double-spend-adapter.ts",
      "prepare-double-spend.ts",
      "submit-init.ts",
      "double-spend/submit-step-01.ts..double-spend/submit-step-04.ts",
      "remove-fraudulent-block.ts",
      "workflow/raw-l1-family-derivation.ts",
      "workflow/runtime.ts#WORKFLOW_RUNNER_FACTORIES.doubleSpend",
    ],
    requiredClosure:
      "install the manifest-bound runner in a compiled application with a concrete public retained-DA libp2p transport/runtime-config loader; the fault-proofs package has no libp2p runtime dependency and cannot honestly self-register it",
  },
  nonExistentInput: {
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
  nonExistentInputNoIndex: {
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
  invalidRange: {
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
  transitionTrace: manual([
    "transition-trace/detect.ts",
    "prepare-transition-trace.ts",
    "submit-transition-trace-proof.ts",
  ]),
  zeroInput: {
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
  validationTraceDispute: {
    existingSurface: [
      "validation-trace-dispute/{adapter,workflow,removal}.ts",
      "workflow/family-application-registry.ts#VALIDATION_TRACE_DISPUTE_FAMILY_APPLICATION_RECORD",
      "workflow/runtime.ts#WORKFLOW_RUNNER_FACTORIES.validationTraceDispute",
      "workflow/complete-replay.ts#createValidationTraceDisputeCompleteCanonicalReplay",
      "remove-fraudulent-block.ts",
    ],
    requiredClosure:
      "install and exercise the manifest-bound validation-trace dispute runner in a compiled application with the concrete public retained-DA libp2p runtime loader",
  },
  daHashPreimage: {
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
  noReferenceInput: {
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
  referenceInputNoIdx: {
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
  invalidSignature: {
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
  fabricatedDeposit: {
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
  fabricatedWithdrawal: {
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
  nativeScriptDecoding: manual([
    "native-script-decoding/replay.ts",
    "native-script-decoding/artifact.ts",
    "native-script-decoding/workflow.ts",
    "workflow/family-application-registry.ts#NATIVE_SCRIPT_DECODING_FAMILY_APPLICATION_RECORD",
  ]),
  missingSignature: {
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
  withdrawnReferenceInput: {
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
  canonicalDecodability: {
    existingSurface: [
      "evidence/canonical-decodability-raw-evidence.ts",
      "workflow/canonical-decodability.ts",
      "workflow/manifest-bound-family-assembly.ts",
      "workflow/field-carriage-prerequisite.ts",
      "workflow/runtime.ts#WORKFLOW_RUNNER_FACTORIES.canonicalDecodability",
      "workflow/canonical-decodability.canonical-decodability-family-definition.ts#CANONICAL_DECODABILITY_FAMILY_DEFINITION",
      "canonical-decodability/submit-canonical-decodability-init.ts",
      "canonical-decodability/submit-canonical-decodability-step-01.ts..step-02.ts",
      "remove-fraudulent-block.ts",
    ],
    requiredClosure:
      "install and exercise the manifest-bound canonical-decodability runner in a compiled application with the concrete public retained-DA runtime loader",
  },
  committedFieldShape: {
    existingSurface: [
      "workflow/committed-field-shape.ts",
      "workflow/manifest-bound-family-assembly.ts",
      "workflow/runtime.ts#WORKFLOW_RUNNER_FACTORIES.committedFieldShape",
      "workflow/committed-field-shape.contracts.ts#COMMITTED_FIELD_SHAPE_FAMILY_DEFINITION",
      "committed-field-shape/submit-committed-field-shape-init.ts",
      "committed-field-shape/submit-committed-field-shape-step-01.ts..step-02.ts",
      "remove-fraudulent-block.ts",
    ],
    requiredClosure:
      "install and exercise the manifest-bound committed-field-shape runner in a compiled application with the concrete public retained-DA libp2p runtime loader",
  },
  minFee: {
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
  withdrawalMistag: {
    existingSurface: [
      "withdrawal-mistag/workflow.ts",
      "withdrawal-mistag/replay.ts",
    ],
    requiredClosure:
      "install and exercise the manifest-bound withdrawal-mistag runner with retained history",
  },
  doubleWithdraw: {
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
  l2TxMistag: {
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
  withdrawnInput: {
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
  valueNotPreserved: {
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
  inputSetUniqueness: {
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
  mintAuthorization: {
    reason: "detector_or_scanner_only",
    existingSurface: [
      "mint-authorization/prover.ts",
      "mint-authorization/submit-mint-authorization-step-01.ts..step-05.ts",
    ],
    requiredClosure:
      "add a complete chain-state driver over the scan finding and all five submit steps",
  },
  networkId: {
    existingSurface: [
      "network-id/workflow-adapter.ts",
      "network-id/prepare.ts",
      "network-id/submit-network-id-init.ts",
      "network-id/submit-network-id-step-01.ts..submit-network-id-step-02.ts",
      "network-id/submit-network-id-forced-step-01.ts",
      "network-id/submit-network-id-forced-bind.ts",
      "network-id/wrongful-rejection.ts",
      "workflow/raw-l1-family-derivation.ts",
      "workflow/family-application-registry.ts#NETWORK_ID_FAMILY_APPLICATION_RECORD",
    ],
    requiredClosure:
      "install the manifest-bound runner in a compiled application with a concrete public retained-DA libp2p transport/runtime-config loader; the fault-proofs package has no libp2p runtime dependency and cannot honestly self-register it",
  },
  nativeScriptInvalid: manual([
    "native-script-invalid/prepare.ts",
    "native-script-invalid/submit-native-script-invalid-step-01.ts..step-03.ts",
  ]),
  minAda: manual([
    "min-ada-v1 SDK wire schema",
    "min-ada deployed step-01/step-02 validators",
  ]),
  fieldPreimageLengthMismatch: {
    reason: "partial_resume_surface_has_no_complete_driver",
    existingSurface: [
      "field-preimage-length-mismatch/config.ts",
      "field-preimage-length-mismatch/workflow.ts",
    ],
    requiredClosure:
      "install and lifecycle-prove both manifest-bound direction branches through canonical removal",
  },
  fieldItemWidthIllegal: {
    reason: "partial_resume_surface_has_no_complete_driver",
    existingSurface: [
      "field-item-width-illegal/contracts.ts",
      "field-item-width-illegal/field-item-width-illegal.ts",
    ],
    requiredClosure:
      "install and lifecycle-prove the manifest-bound three-step driver through canonical removal",
  },
  witnessScriptDecoding: {
    reason: "partial_resume_surface_has_no_complete_driver",
    existingSurface: ["witness-script-decoding/workflow.ts"],
    requiredClosure:
      "install and lifecycle-prove the resumable manifest-bound structural scan through canonical removal",
  },
  scriptIntegrityHashMissing: {
    reason: "partial_resume_surface_has_no_complete_driver",
    existingSurface: ["script-integrity-hash-missing/family.ts"],
    requiredClosure:
      "install and lifecycle-prove the seven-script staged driver through canonical removal",
  },
  transactionOutputNonCanonical: {
    existingSurface: [
      "transaction-output-non-canonical/workflow.ts",
      "workflow/runtime.ts#createTransactionOutputNonCanonicalProductionWorkflowRunnerV1",
    ],
    requiredClosure:
      "install and exercise the manifest-bound four-step runner in the compiled watcher application with public retained DA",
  },
  resolvedOutputNonCanonical: {
    existingSurface: [
      "resolved-output-non-canonical production runner surface is centrally installed",
    ],
    requiredClosure:
      "replace this static readiness row with the admitted central runner after the Wave 2 integration gate",
  },
  mintDeclaredAssetLimit: {
    existingSurface: [
      "mint-declared-asset-limit production runner surface is centrally installed",
    ],
    requiredClosure:
      "replace this static readiness row with the admitted central runner after the Wave 2 integration gate",
  },
  spendInputSignerMissing: {
    existingSurface: [
      "spend-input-signer-missing production runner surface is centrally installed",
    ],
    requiredClosure:
      "replace this static readiness row with the admitted central runner after the Wave 3 integration gate",
  },
  protectedOutputSignerMissing: {
    existingSurface: [
      "protected-output-signer-missing production runner surface is centrally installed",
    ],
    requiredClosure:
      "replace this static readiness row with the admitted central runner after the Wave 3 integration gate",
  },
  observersForbiddenOnUntaggedNetwork: {
    existingSurface: [
      "observers-forbidden-on-untagged-network production runner surface is centrally installed",
    ],
    requiredClosure:
      "replace this static readiness row with the admitted central runner after the Wave 3 integration gate",
  },
  observerOrderInvalid: {
    existingSurface: [
      "observer-order-invalid production runner surface is centrally installed",
    ],
    requiredClosure:
      "install the manifest-bound four-script runner in the compiled watcher application with authenticated retained DA",
  },
  redeemerCanonicity: {
    existingSurface: [
      "redeemer-canonicity production runner surface is centrally installed",
    ],
    requiredClosure:
      "install the manifest-bound three-script runner in the compiled watcher application with authenticated retained DA",
  },
  outputReferenceScriptDecoding: {
    existingSurface: [
      "output-reference-script-decoding production runner surface is centrally installed",
    ],
    requiredClosure:
      "install the manifest-bound six-script runner in the compiled watcher application with public retained DA",
  },
  executionSourceScriptDecoding: {
    existingSurface: [
      "execution-source-script-decoding/replay.ts",
      "execution-source-script-decoding/artifact.ts",
      "execution-source-script-decoding/workflow.ts",
      "workflow/family-application-registry.ts#EXECUTION_SOURCE_SCRIPT_DECODING_FAMILY_APPLICATION_RECORD",
    ],
    requiredClosure:
      "install and exercise the manifest-bound execution-source-script-decoding runner in a compiled application with its retained-DA runtime",
  },
  receivePurposeLanguage: {
    existingSurface: [
      "receive-purpose-language/replay.ts",
      "receive-purpose-language/artifact.ts",
      "receive-purpose-language/workflow.ts",
      "workflow/family-application-registry.ts#RECEIVE_PURPOSE_LANGUAGE_FAMILY_APPLICATION_RECORD",
    ],
    requiredClosure:
      "install and exercise the manifest-bound receive-purpose-language runner in a compiled application with its retained-DA runtime",
  },
  unusedScriptWitness: {
    existingSurface: [
      "unused-script-witness/replay.ts",
      "unused-script-witness/artifact.ts",
      "unused-script-witness/workflow.ts",
      "workflow/family-application-registry.ts#UNUSED_SCRIPT_WITNESS_FAMILY_APPLICATION_RECORD",
    ],
    requiredClosure:
      "install and exercise the manifest-bound unused-script-witness runner in a compiled application with its retained-DA runtime",
  },
  missingScriptSource: {
    existingSurface: [
      "missing-script-source/replay.ts",
      "missing-script-source/artifact.ts",
      "missing-script-source/workflow.ts",
      "workflow/family-application-registry.ts#MISSING_SCRIPT_SOURCE_FAMILY_APPLICATION_RECORD",
    ],
    requiredClosure:
      "install and exercise the manifest-bound missing-script-source runner in a compiled application with its retained-DA runtime",
  },
  missingRedeemer: {
    existingSurface: [
      "missing-redeemer/replay.ts",
      "missing-redeemer/artifact.ts",
      "missing-redeemer/workflow.ts",
      "workflow/family-application-registry.ts#MISSING_REDEEMER_FAMILY_APPLICATION_RECORD",
    ],
    requiredClosure:
      "install and exercise the manifest-bound missing-redeemer runner in a compiled application with its retained-DA runtime",
  },
  unusedRedeemer: {
    existingSurface: [
      "unused-redeemer/replay.ts",
      "unused-redeemer/artifact.ts",
      "unused-redeemer/workflow.ts",
      "workflow/family-application-registry.ts#UNUSED_REDEEMER_FAMILY_APPLICATION_RECORD",
    ],
    requiredClosure:
      "install and exercise the manifest-bound unused-redeemer runner in a compiled application with its retained-DA runtime",
  },
  executionNativeScriptInvalid: {
    existingSurface: [
      "execution-native-script-invalid/replay.ts",
      "execution-native-script-invalid/artifact.ts",
      "execution-native-script-invalid/workflow.ts",
      "workflow/family-application-registry.ts#EXECUTION_NATIVE_SCRIPT_INVALID_FAMILY_APPLICATION_RECORD",
    ],
    requiredClosure:
      "install and exercise the manifest-bound execution-native-script-invalid runner in a compiled application with its retained-DA runtime",
  },
  scriptIntegrityHashMismatch: {
    existingSurface: [
      "script-integrity-hash-mismatch/replay.ts",
      "script-integrity-hash-mismatch/artifact.ts",
      "script-integrity-hash-mismatch/workflow.ts",
      "workflow/family-application-registry.ts#SCRIPT_INTEGRITY_HASH_MISMATCH_FAMILY_APPLICATION_RECORD",
    ],
    requiredClosure:
      "install and exercise the manifest-bound script-integrity-hash-mismatch runner in a compiled application with its retained-DA runtime",
  },
  distinctAssetAccumulationLimit: {
    existingSurface: [
      "distinct-asset-accumulation-limit/replay.ts",
      "distinct-asset-accumulation-limit/artifact.ts",
      "distinct-asset-accumulation-limit/workflow.ts",
      "workflow/family-application-registry.ts#DISTINCT_ASSET_ACCUMULATION_LIMIT_FAMILY_APPLICATION_RECORD",
    ],
    requiredClosure:
      "install and exercise the manifest-bound distinct-asset-accumulation-limit runner in a compiled application with its retained-DA runtime",
  },
  mintItemNonCanonical: {
    existingSurface: ["mint-item-non-canonical/workflow.ts"],
    requiredClosure:
      "install and exercise the manifest-bound four-step runner with public retained DA",
  },
};

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
export const workflowAdapterRegistrationRows: readonly WorkflowAdapterRegistration[] =
  FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER.map((category) => {
    const row = missingRows[category];
    return {
      category,
      status: "missing",
      reason: row.reason ?? "constrained_adapter_is_not_launch_scope_complete",
      existingSurface: row.existingSurface,
      requiredClosure: row.requiredClosure,
    };
  });
