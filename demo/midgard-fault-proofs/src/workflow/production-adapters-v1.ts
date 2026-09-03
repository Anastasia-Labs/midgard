import { normalizeDaDeploymentFingerprintHex } from "@al-ft/midgard-core/da-transport";
import {
  FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER,
  type FraudProofCatalogueCategoryName,
} from "@al-ft/midgard-sdk";

import type { WorkflowActuationPermit } from "./production-actuation-permit-v1.js";
import type { WorkflowFundingReservationPermit } from "./production-funding-reservation-permit-v1.js";
import {
  isAdmittedWorkflowRunner,
  WORKFLOW_ADAPTER_RUNNER,
} from "./production-runner-admission-v1.js";

export { WORKFLOW_ADAPTER_RUNNER } from "./production-runner-admission-v1.js";

export const WORKFLOW_ADAPTER_REGISTRY_SCHEMA_VERSION =
  "midgard-production-fraud-proof-workflow-adapters-v1" as const;
export const WORKFLOW_APPLICATION_REGISTRY_SCHEMA_VERSION =
  "midgard-production-fraud-proof-application-registry-v1" as const;
/** Permit-free identity/configuration used only for startup readiness. */
export type WorkflowAdapterReadinessInput = {
  readonly mode: "run" | "resume";
  readonly category: FraudProofCatalogueCategoryName;
  readonly deploymentFingerprint: string;
  readonly headerHash: string;
  readonly journalDirectory: string;
  /** Versioned infrastructure configuration only; never proof evidence. */
  readonly runtimeConfigPath: string;
};

export type WorkflowAdapterRunnerInput = WorkflowAdapterReadinessInput & {
  /** Exact admitted classifier decision; forms part of the durable run key. */
  readonly decisionDigest: string;
  /** Live, revocable authority for this decision and rollback generation. */
  readonly actuationPermit: WorkflowActuationPermit;
  /** Durable, exact-input funding authority for this decision/generation. */
  readonly fundingReservationPermit: WorkflowFundingReservationPermit;
};

export type WorkflowAdapterRunner = {
  readonly runnerVersion: typeof WORKFLOW_ADAPTER_RUNNER;
  readonly runOrResume: (input: WorkflowAdapterRunnerInput) => Promise<unknown>;
};

export type MissingWorkflowAdapterReason =
  | "manual_step_chain_has_no_atomic_driver"
  | "one_shot_prover_has_no_pre_submit_journal_hook"
  | "prover_is_not_chain_state_resumable"
  | "partial_resume_surface_has_no_complete_driver"
  | "detector_or_scanner_only"
  | "constrained_adapter_is_not_launch_scope_complete";

export type MissingWorkflowAdapterRegistration = {
  readonly category: FraudProofCatalogueCategoryName;
  readonly status: "missing";
  readonly reason: MissingWorkflowAdapterReason;
  readonly existingSurface: readonly string[];
  readonly requiredClosure: string;
};

export type ReadyWorkflowAdapterRegistration = {
  readonly category: FraudProofCatalogueCategoryName;
  readonly status: "ready";
  readonly adapterVersion: string;
  readonly runner: WorkflowAdapterRunner;
  readonly existingSurface: readonly string[];
  readonly guarantees: readonly string[];
};

export type WorkflowAdapterRegistration =
  | ReadyWorkflowAdapterRegistration
  | MissingWorkflowAdapterRegistration;

export type WorkflowApplicationRunnerInstallation = Readonly<{
  readonly category: FraudProofCatalogueCategoryName;
  readonly deploymentFingerprint: string;
  readonly runner: WorkflowAdapterRunner;
}>;

export type WorkflowApplicationRegistry = Readonly<{
  readonly schemaVersion: typeof WORKFLOW_APPLICATION_REGISTRY_SCHEMA_VERSION;
  readonly deploymentFingerprint: string;
  readonly installedCategories: readonly FraudProofCatalogueCategoryName[];
  /** Exact full-catalogue overlay; static rows remain unchanged. */
  readonly registrations: readonly WorkflowAdapterRegistration[];
}>;

const manual = (
  category: FraudProofCatalogueCategoryName,
  existingSurface: readonly string[],
): WorkflowAdapterRegistration => ({
  category,
  status: "missing",
  reason: "manual_step_chain_has_no_atomic_driver",
  existingSurface,
  requiredClosure:
    "add a chain-state cursor plus build/local-evaluate, pre-submit-intent, submit, and authenticated reconcile hooks for every transaction",
});

const freezeRegistration = (
  registration: WorkflowAdapterRegistration,
): WorkflowAdapterRegistration =>
  registration.status === "ready"
    ? Object.freeze({
        ...registration,
        existingSurface: Object.freeze([...registration.existingSurface]),
        guarantees: Object.freeze([...registration.guarantees]),
      })
    : Object.freeze({
        ...registration,
        existingSurface: Object.freeze([...registration.existingSurface]),
      });

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
const workflowAdapterRegistrationRows = [
  {
    category: "doubleSpend",
    status: "missing",
    reason: "constrained_adapter_is_not_launch_scope_complete",
    existingSurface: [
      "workflow/double-spend-adapter-v1.ts",
      "prepare-double-spend.ts",
      "submit-init.ts",
      "submit-step-01.ts..submit-step-04.ts",
      "remove-fraudulent-block.ts",
      "workflow/local-kupmios-http-ogmios-source-v1.ts",
      "workflow/raw-l1-family-derivation-v1.ts",
      "workflow/production-runtime-v1.ts#createDoubleSpendProductionWorkflowRunnerV1",
    ],
    requiredClosure:
      "install the manifest-bound runner in a compiled application with a concrete public retained-DA libp2p transport/runtime-config loader; the fault-proofs package has no libp2p runtime dependency and cannot honestly self-register it",
  },
  {
    category: "nonExistentInput",
    status: "missing",
    reason: "constrained_adapter_is_not_launch_scope_complete",
    existingSurface: [
      "workflow/production-ledger-absence-artifact-v1.ts",
      "workflow/production-non-existent-input-v1.ts",
      "workflow/production-runtime-v1.ts#createNonExistentInputProductionWorkflowRunnerV1",
      "ne-submit-step-01.ts..ne-submit-step-04.ts",
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
      "workflow/production-input-no-idx-v1.ts",
      "workflow/production-runtime-v1.ts#createInputNoIdxProductionWorkflowRunnerV1",
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
      "workflow/production-native-inclusion-two-step-v1.ts",
      "workflow/production-proof-chunk-prerequisite-v1.ts",
      "workflow/production-runtime-v1.ts#createInvalidRangeProductionWorkflowRunnerV1",
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
      "workflow/production-native-inclusion-two-step-v1.ts",
      "workflow/production-proof-chunk-prerequisite-v1.ts",
      "workflow/production-runtime-v1.ts#createZeroInputProductionWorkflowRunnerV1",
      "submit-init.ts",
      "submit-zero-input-step-01.ts..submit-zero-input-step-02.ts",
      "remove-fraudulent-block.ts",
    ],
    requiredClosure:
      "install and exercise the exact manifest-bound runner in a compiled application with the concrete public retained-DA runtime loader",
  },
  manual("validationTraceDispute", [
    "validation-dispute/submit.ts",
    "validation-dispute/from-files.ts",
  ]),
  {
    category: "daHashPreimage",
    status: "missing",
    reason: "constrained_adapter_is_not_launch_scope_complete",
    existingSurface: [
      "evidence/production-fraud-proof-evidence-v1.ts",
      "workflow/production-da-hash-preimage-v1.ts",
      "workflow/production-runtime-v1.ts#createDaHashPreimageProductionWorkflowRunnerV1",
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
      "workflow/production-no-reference-input-v1.ts",
      "workflow/production-ledger-absence-artifact-v1.ts",
      "workflow/production-runtime-v1.ts#createNoReferenceInputProductionWorkflowRunnerV1",
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
      "workflow/production-reference-input-no-idx-v1.ts",
      "workflow/production-runtime-v1.ts#createReferenceInputNoIdxProductionWorkflowRunnerV1",
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
      "workflow/production-invalid-signature-v1.ts",
      "workflow/production-runtime-v1.ts#createInvalidSignatureProductionWorkflowRunnerV1",
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
      "workflow/production-fabricated-deposit-evidence-v1.ts",
      "workflow/production-fabricated-deposit-v1.ts",
      "workflow/production-runtime-v1.ts#createFabricatedDepositProductionWorkflowRunnerV1",
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
      "workflow/production-fabricated-withdrawal-evidence-v1.ts",
      "workflow/production-fabricated-withdrawal-v1.ts",
      "workflow/production-runtime-v1.ts#createFabricatedWithdrawalProductionWorkflowRunnerV1",
      "submit-fabricated-withdrawal-step-01.ts..submit-fabricated-withdrawal-step-04.ts",
      "remove-fraudulent-block.ts",
    ],
    requiredClosure:
      "install and exercise the manifest-bound fabricated-withdrawal runner in a compiled application with its public L1 event authority and retained-DA runtime",
  },
  {
    category: "nativeScriptDecoding",
    status: "missing",
    reason: "one_shot_prover_has_no_pre_submit_journal_hook",
    existingSurface: [
      "native-script-decoding/prover-v1.ts",
      "native-script-decoding/prover-adapters-v1.ts",
    ],
    requiredClosure:
      "emit durable shared-workflow intent before each internal prover transaction; then forward each submitted/confirmed hash into the shared journal",
  },
  {
    category: "missingSignature",
    status: "missing",
    reason: "constrained_adapter_is_not_launch_scope_complete",
    existingSurface: [
      "workflow/production-missing-signature-state-v1.ts",
      "workflow/production-missing-signature-adapter-v1.ts",
      "workflow/production-missing-signature-v1.ts",
      "workflow/production-runtime-v1.ts#createMissingSignatureProductionWorkflowRunnerV1",
      "missing-signature/submit-missing-signature-init.ts",
      "missing-signature/submit-missing-signature-step-01.ts..step-04.ts",
      "remove-fraudulent-block.ts",
    ],
    requiredClosure:
      "install and exercise the manifest-bound missing-signature runner in a compiled application with the concrete public retained-DA libp2p runtime loader",
  },
  manual("missingNativeScriptTx", [
    "missing-native-script-tx/prepare-v1.ts",
    "missing-native-script-tx/submit-missing-native-script-tx-init.ts",
    "missing-native-script-tx/submit-missing-native-script-tx-step-01.ts..step-06.ts",
  ]),
  {
    category: "withdrawnReferenceInput",
    status: "missing",
    reason: "constrained_adapter_is_not_launch_scope_complete",
    existingSurface: [
      "workflow/production-withdrawn-reference-input-v1.ts",
      "workflow/production-runtime-v1.ts#createWithdrawnReferenceInputProductionWorkflowRunnerV1",
      "withdrawn-reference-input/prepare-withdrawn-reference-input-v1.ts",
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
      "evidence/canonical-decodability-raw-evidence-v1.ts",
      "workflow/production-canonical-decodability-v1.ts",
      "workflow/production-field-carriage-prerequisite-v1.ts",
      "workflow/production-runtime-v1.ts#createCanonicalDecodabilityProductionWorkflowRunnerV1",
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
      "workflow/production-committed-field-shape-v1.ts",
      "workflow/production-runtime-v1.ts#createCommittedFieldShapeProductionWorkflowRunnerV1",
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
      "workflow/production-min-fee-v1.ts",
      "workflow/production-field-carriage-prerequisite-v1.ts",
      "workflow/production-runtime-v1.ts#createMinFeeProductionWorkflowRunnerV1",
      "prepare-min-fee.ts",
      "submit-min-fee-init.ts",
      "submit-min-fee-step-01.ts..submit-min-fee-step-02.ts",
      "remove-fraudulent-block.ts",
    ],
    requiredClosure:
      "install and exercise the manifest-bound min-fee runner in a compiled application with the concrete public retained-DA runtime loader",
  },
  manual("withdrawalMistag", [
    "withdrawal-mistag/prepare-withdrawal-mistag.ts",
    "withdrawal-mistag/submit-withdrawal-mistag-init.ts",
    "withdrawal-mistag/submit-withdrawal-mistag-step-01.ts..step-05.ts",
  ]),
  {
    category: "doubleWithdraw",
    status: "missing",
    reason: "constrained_adapter_is_not_launch_scope_complete",
    existingSurface: [
      "workflow/production-double-withdraw-v1.ts",
      "workflow/production-runtime-v1.ts#createDoubleWithdrawProductionWorkflowRunnerV1",
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
    reason: "partial_resume_surface_has_no_complete_driver",
    existingSurface: [
      "cross-block-duplicate-event/prepare-v1.ts",
      "cross-block-duplicate-event/resume-v1.ts",
      "cross-block-duplicate-event/submit-cross-block-duplicate-event-init.ts",
    ],
    requiredClosure:
      "wrap init and both resume positions in one per-transaction intent/evaluate/submit/reconcile state machine",
  },
  {
    category: "l2TxMistag",
    status: "missing",
    reason: "constrained_adapter_is_not_launch_scope_complete",
    existingSurface: [
      "workflow/production-l2-tx-mistag-v1.ts",
      "workflow/production-proof-chunk-prerequisite-v1.ts",
      "workflow/production-runtime-v1.ts#createL2TxMistagProductionWorkflowRunnerV1",
      "l2-tx-mistag/prepare-l2-tx-mistag-v1.ts",
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
      "workflow/production-withdrawn-input-v1.ts",
      "workflow/production-runtime-v1.ts#createWithdrawnInputProductionWorkflowRunnerV1",
      "withdrawn-input/evidence-v1.ts",
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
    reason: "prover_is_not_chain_state_resumable",
    existingSurface: [
      "value-not-preserved/prover-v1.ts",
      "value-not-preserved/evidence-v1.ts",
    ],
    requiredClosure:
      "derive the current fold cursor from the live thread and persist intent/confirmation around every fold transaction",
  },
  {
    category: "inputSetUniqueness",
    status: "missing",
    reason: "constrained_adapter_is_not_launch_scope_complete",
    existingSurface: [
      "workflow/production-input-set-uniqueness-v1.ts",
      "workflow/production-runtime-v1.ts#createInputSetUniquenessProductionWorkflowRunnerV1",
      "input-set-uniqueness/scan-v1.ts",
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
      "mint-authorization/prover-v1.ts",
      "mint-authorization/submit-mint-authorization-init.ts",
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
      "network-id/workflow-adapter-v1.ts",
      "network-id/prepare-v1.ts",
      "network-id/submit-network-id-init.ts",
      "network-id/submit-network-id-step-01.ts..submit-network-id-step-02.ts",
      "workflow/local-kupmios-http-ogmios-source-v1.ts",
      "workflow/raw-l1-family-derivation-v1.ts",
      "workflow/production-runtime-v1.ts#createNetworkIdProductionWorkflowRunnerV1",
    ],
    requiredClosure:
      "install the manifest-bound runner in a compiled application with a concrete public retained-DA libp2p transport/runtime-config loader; the fault-proofs package has no libp2p runtime dependency and cannot honestly self-register it",
  },
  manual("missingNativeScriptUtxo", [
    "missing-native-script-utxo/prepare-v1.ts",
    "missing-native-script-utxo/submit-missing-native-script-utxo-init.ts",
    "missing-native-script-utxo/submit-missing-native-script-utxo-step-01.ts..step-05.ts",
  ]),
  manual("nativeScriptInvalid", [
    "native-script-invalid/prepare-v1.ts",
    "native-script-invalid/submit-native-script-invalid-init.ts",
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
      "field-preimage-length-mismatch/production-config-v1.ts",
      "field-preimage-length-mismatch/workflow-v1.ts",
    ],
    requiredClosure:
      "install and lifecycle-prove both manifest-bound direction branches through canonical removal",
  },
  {
    category: "fieldItemWidthIllegal",
    status: "missing",
    reason: "partial_resume_surface_has_no_complete_driver",
    existingSurface: [
      "field-item-width-illegal/contracts-v1.ts",
      "field-item-width-illegal/field-item-width-illegal-v1.ts",
    ],
    requiredClosure:
      "install and lifecycle-prove the manifest-bound three-step driver through canonical removal",
  },
  {
    category: "witnessScriptDecoding",
    status: "missing",
    reason: "partial_resume_surface_has_no_complete_driver",
    existingSurface: ["witness-script-decoding/workflow-v1.ts"],
    requiredClosure:
      "install and lifecycle-prove the resumable manifest-bound structural scan through canonical removal",
  },
  {
    category: "scriptIntegrityHashMissing",
    status: "missing",
    reason: "partial_resume_surface_has_no_complete_driver",
    existingSurface: ["script-integrity-hash-missing/family-v1.ts"],
    requiredClosure:
      "install and lifecycle-prove the seven-script staged driver through canonical removal",
  },
  {
    category: "transactionOutputNonCanonical",
    status: "missing",
    reason: "constrained_adapter_is_not_launch_scope_complete",
    existingSurface: [
      "transaction-output-non-canonical/production-workflow-v1.ts",
      "workflow/production-runtime-v1.ts#createTransactionOutputNonCanonicalProductionWorkflowRunnerV1",
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
] as const satisfies readonly WorkflowAdapterRegistration[];

export const WORKFLOW_ADAPTER_REGISTRATIONS = Object.freeze(
  workflowAdapterRegistrationRows.map(freezeRegistration),
);

export const validateWorkflowAdapterCoverage = (
  registrations: readonly {
    readonly category: unknown;
    readonly status?: unknown;
    readonly runner?: {
      readonly runnerVersion?: unknown;
      readonly runOrResume?: unknown;
    };
  }[],
  catalogue: readonly FraudProofCatalogueCategoryName[] = FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER,
): void => {
  if (registrations.length !== catalogue.length) {
    throw new Error(
      `production workflow registration cardinality mismatch: expected=${catalogue.length.toString()} actual=${registrations.length.toString()}`,
    );
  }
  const seen = new Set<unknown>();
  for (const [index, registration] of registrations.entries()) {
    if (seen.has(registration.category)) {
      throw new Error(
        `production workflow registration duplicates ${String(registration.category)}`,
      );
    }
    seen.add(registration.category);
    const expected = catalogue[index];
    if (registration.category !== expected) {
      throw new Error(
        `production workflow registration order mismatch at ${index.toString()}: expected=${String(expected)} actual=${String(registration.category)}`,
      );
    }
    if (registration.status !== "missing" && registration.status !== "ready") {
      throw new Error(
        `production workflow registration ${String(registration.category)} has an unknown status`,
      );
    }
    if (
      registration.status === "ready" &&
      (registration.runner?.runnerVersion !== WORKFLOW_ADAPTER_RUNNER ||
        typeof registration.runner.runOrResume !== "function" ||
        !isAdmittedWorkflowRunner({
          category: expected!,
          runner: registration.runner,
        }))
    ) {
      throw new Error(
        `production workflow registration ${String(registration.category)} has no compiled executable runner admitted for its exact category`,
      );
    }
  }
};

validateWorkflowAdapterCoverage(WORKFLOW_ADAPTER_REGISTRATIONS);

const admittedApplicationRegistries = new WeakSet<object>();

const assertCanonicalLaunchScope = (
  launchScope: readonly FraudProofCatalogueCategoryName[],
  label: string,
): void => {
  if (new Set(launchScope).size !== launchScope.length) {
    throw new Error(`${label} contains a duplicate category`);
  }
  for (const [index, category] of launchScope.entries()) {
    if (!FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER.includes(category)) {
      throw new Error(`${label} contains unknown category ${String(category)}`);
    }
    if (
      index > 0 &&
      FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER.indexOf(launchScope[index - 1]!) >=
        FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER.indexOf(category)
    ) {
      throw new Error(`${label} is not in canonical catalogue order`);
    }
  }
};

/**
 * Installs executable runners into an immutable, deployment-bound application
 * overlay. The canonical registry above is never mutated. Applications must
 * call this only after their own opaque signed-deployment verifier has admitted
 * the identity; runner admission and exact category coverage are rechecked
 * here so metadata alone can never become ready.
 */
export const installWorkflowApplicationRegistry = ({
  deploymentFingerprint,
  requiredInstalledCategories,
  installations,
}: {
  readonly deploymentFingerprint: string;
  readonly requiredInstalledCategories: readonly FraudProofCatalogueCategoryName[];
  readonly installations: readonly WorkflowApplicationRunnerInstallation[];
}): WorkflowApplicationRegistry => {
  const normalizedDeploymentFingerprint = normalizeDaDeploymentFingerprintHex(
    deploymentFingerprint,
  );
  if (normalizedDeploymentFingerprint !== deploymentFingerprint) {
    throw new Error(
      "production workflow application deployment fingerprint is not canonical",
    );
  }
  assertCanonicalLaunchScope(
    requiredInstalledCategories,
    "production workflow application installation scope",
  );
  if (requiredInstalledCategories.length === 0) {
    throw new Error(
      "production workflow application installation scope must not be empty",
    );
  }
  if (installations.length !== requiredInstalledCategories.length) {
    throw new Error(
      `production workflow application installation cardinality mismatch: expected=${requiredInstalledCategories.length.toString()} actual=${installations.length.toString()}`,
    );
  }
  const installationByCategory = new Map<
    FraudProofCatalogueCategoryName,
    WorkflowApplicationRunnerInstallation
  >();
  for (const [index, installation] of installations.entries()) {
    const expected = requiredInstalledCategories[index];
    if (installationByCategory.has(installation.category)) {
      throw new Error(
        `production workflow application installation duplicates ${String(installation.category)}`,
      );
    }
    if (installation.category !== expected) {
      throw new Error(
        `production workflow application installation order mismatch at ${index.toString()}: expected=${String(expected)} actual=${String(installation.category)}`,
      );
    }
    if (installation.deploymentFingerprint !== deploymentFingerprint) {
      throw new Error(
        `production workflow application installation ${String(installation.category)} has an unrecognized deployment identity`,
      );
    }
    if (
      installation.runner.runnerVersion !== WORKFLOW_ADAPTER_RUNNER ||
      typeof installation.runner.runOrResume !== "function" ||
      !isAdmittedWorkflowRunner({
        category: installation.category,
        runner: installation.runner,
      })
    ) {
      throw new Error(
        `production workflow application installation ${String(installation.category)} has no module-admitted category-bound runner`,
      );
    }
    installationByCategory.set(installation.category, installation);
  }
  const registrations = Object.freeze(
    WORKFLOW_ADAPTER_REGISTRATIONS.map((registration) => {
      const installation = installationByCategory.get(registration.category);
      if (installation === undefined) return registration;
      return freezeRegistration({
        category: registration.category,
        status: "ready",
        adapterVersion: WORKFLOW_APPLICATION_REGISTRY_SCHEMA_VERSION,
        runner: installation.runner,
        existingSurface: registration.existingSurface,
        guarantees: [
          "runner is module-admitted for the exact catalogue category",
          "runner is installed for this exact verified deployment fingerprint",
          "runtime reconstructs proof evidence from authenticated L1 and public retained DA",
        ],
      });
    }),
  );
  validateWorkflowAdapterCoverage(registrations);
  const registry = Object.freeze({
    schemaVersion: WORKFLOW_APPLICATION_REGISTRY_SCHEMA_VERSION,
    deploymentFingerprint,
    installedCategories: Object.freeze([...requiredInstalledCategories]),
    registrations,
  });
  admittedApplicationRegistries.add(registry);
  return registry;
};

export const assertWorkflowApplicationRegistry = (
  registry: WorkflowApplicationRegistry,
): void => {
  if (
    !admittedApplicationRegistries.has(registry) ||
    registry.schemaVersion !== WORKFLOW_APPLICATION_REGISTRY_SCHEMA_VERSION
  ) {
    throw new Error(
      "production workflow application registry was not installed through the authenticated immutable boundary",
    );
  }
  validateWorkflowAdapterCoverage(registry.registrations);
};

const resolveWorkflowRegistrations = (
  applicationRegistry?: WorkflowApplicationRegistry,
): readonly WorkflowAdapterRegistration[] => {
  if (applicationRegistry === undefined) {
    return WORKFLOW_ADAPTER_REGISTRATIONS;
  }
  assertWorkflowApplicationRegistry(applicationRegistry);
  return applicationRegistry.registrations;
};

export const missingWorkflowAdapters = (
  launchScope: readonly FraudProofCatalogueCategoryName[] = FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER,
  applicationRegistry?: WorkflowApplicationRegistry,
): readonly MissingWorkflowAdapterRegistration[] => {
  const registrations = resolveWorkflowRegistrations(applicationRegistry);
  validateWorkflowAdapterCoverage(registrations);
  assertCanonicalLaunchScope(launchScope, "production workflow launch scope");
  const scope = new Set(launchScope);
  return registrations.filter(
    (registration): registration is MissingWorkflowAdapterRegistration =>
      registration.status === "missing" && scope.has(registration.category),
  );
};

export class MissingWorkflowAdaptersError extends Error {
  readonly missing: readonly MissingWorkflowAdapterRegistration[];

  constructor(missing: readonly MissingWorkflowAdapterRegistration[]) {
    super(
      `production fraud-proof workflow adapters are unavailable: ${missing
        .map((entry) => `${entry.category}(${entry.reason})`)
        .join(", ")}`,
    );
    this.name = "MissingProductionWorkflowAdaptersErrorV1";
    this.missing = missing;
  }
}

/** Refuses production startup until the requested launch scope is concrete. */
export const assertWorkflowAdaptersReady = (
  launchScope: readonly FraudProofCatalogueCategoryName[] = FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER,
  applicationRegistry?: WorkflowApplicationRegistry,
): void => {
  const missing = missingWorkflowAdapters(launchScope, applicationRegistry);
  if (missing.length > 0) {
    throw new MissingWorkflowAdaptersError(missing);
  }
};

export const workflowAdapterRunner = (
  category: FraudProofCatalogueCategoryName,
  applicationRegistry?: WorkflowApplicationRegistry,
): WorkflowAdapterRunner => {
  const registrations = resolveWorkflowRegistrations(applicationRegistry);
  validateWorkflowAdapterCoverage(registrations);
  const registration = registrations.find(
    (candidate) => candidate.category === category,
  );
  if (registration === undefined || registration.status !== "ready") {
    assertWorkflowAdaptersReady([category], applicationRegistry);
    throw new Error(
      `production workflow registry invariant: ${category} has no ready registration`,
    );
  }
  return registration.runner;
};
