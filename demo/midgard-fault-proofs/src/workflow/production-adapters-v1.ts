import { normalizeDaDeploymentFingerprintHex } from "@al-ft/midgard-core/da-transport";
import {
  FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER,
  type FraudProofCatalogueCategoryName,
} from "@al-ft/midgard-sdk";

import type { ProductionWorkflowActuationPermitV1 } from "./production-actuation-permit-v1.js";
import type { ProductionWorkflowFundingReservationPermitV1 } from "./production-funding-reservation-permit-v1.js";
import {
  isAdmittedProductionWorkflowRunnerV1,
  PRODUCTION_WORKFLOW_ADAPTER_RUNNER_V1,
} from "./production-runner-admission-v1.js";

export { PRODUCTION_WORKFLOW_ADAPTER_RUNNER_V1 } from "./production-runner-admission-v1.js";

export const PRODUCTION_WORKFLOW_ADAPTER_REGISTRY_V1_SCHEMA_VERSION =
  "midgard-production-fraud-proof-workflow-adapters-v1" as const;
export const PRODUCTION_WORKFLOW_APPLICATION_REGISTRY_V1_SCHEMA_VERSION =
  "midgard-production-fraud-proof-application-registry-v1" as const;
/** Permit-free identity/configuration used only for startup readiness. */
export type ProductionWorkflowAdapterReadinessInputV1 = {
  readonly mode: "run" | "resume";
  readonly category: FraudProofCatalogueCategoryName;
  readonly deploymentFingerprint: string;
  readonly headerHash: string;
  readonly journalDirectory: string;
  /** Versioned infrastructure configuration only; never proof evidence. */
  readonly runtimeConfigPath: string;
};

export type ProductionWorkflowAdapterRunnerInputV1 =
  ProductionWorkflowAdapterReadinessInputV1 & {
    /** Exact admitted classifier decision; forms part of the durable run key. */
    readonly decisionDigest: string;
    /** Live, revocable authority for this decision and rollback generation. */
    readonly actuationPermit: ProductionWorkflowActuationPermitV1;
    /** Durable, exact-input funding authority for this decision/generation. */
    readonly fundingReservationPermit: ProductionWorkflowFundingReservationPermitV1;
  };

export type ProductionWorkflowAdapterRunnerV1 = {
  readonly runnerVersion: typeof PRODUCTION_WORKFLOW_ADAPTER_RUNNER_V1;
  readonly runOrResume: (
    input: ProductionWorkflowAdapterRunnerInputV1,
  ) => Promise<unknown>;
};

export type MissingProductionWorkflowAdapterReasonV1 =
  | "manual_step_chain_has_no_atomic_driver"
  | "one_shot_prover_has_no_pre_submit_journal_hook"
  | "prover_is_not_chain_state_resumable"
  | "partial_resume_surface_has_no_complete_driver"
  | "detector_or_scanner_only"
  | "constrained_adapter_is_not_launch_scope_complete";

export type MissingProductionWorkflowAdapterRegistrationV1 = {
  readonly category: FraudProofCatalogueCategoryName;
  readonly status: "missing";
  readonly reason: MissingProductionWorkflowAdapterReasonV1;
  readonly existingSurface: readonly string[];
  readonly requiredClosure: string;
};

export type ReadyProductionWorkflowAdapterRegistrationV1 = {
  readonly category: FraudProofCatalogueCategoryName;
  readonly status: "ready";
  readonly adapterVersion: string;
  readonly runner: ProductionWorkflowAdapterRunnerV1;
  readonly existingSurface: readonly string[];
  readonly guarantees: readonly string[];
};

export type ProductionWorkflowAdapterRegistrationV1 =
  | ReadyProductionWorkflowAdapterRegistrationV1
  | MissingProductionWorkflowAdapterRegistrationV1;

export type ProductionWorkflowApplicationRunnerInstallationV1 = Readonly<{
  readonly category: FraudProofCatalogueCategoryName;
  readonly deploymentFingerprint: string;
  readonly runner: ProductionWorkflowAdapterRunnerV1;
}>;

export type ProductionWorkflowApplicationRegistryV1 = Readonly<{
  readonly schemaVersion: typeof PRODUCTION_WORKFLOW_APPLICATION_REGISTRY_V1_SCHEMA_VERSION;
  readonly deploymentFingerprint: string;
  readonly installedCategories: readonly FraudProofCatalogueCategoryName[];
  /** Exact full-catalogue overlay; static rows remain unchanged. */
  readonly registrations: readonly ProductionWorkflowAdapterRegistrationV1[];
}>;

const manual = (
  category: FraudProofCatalogueCategoryName,
  existingSurface: readonly string[],
): ProductionWorkflowAdapterRegistrationV1 => ({
  category,
  status: "missing",
  reason: "manual_step_chain_has_no_atomic_driver",
  existingSurface,
  requiredClosure:
    "add a chain-state cursor plus build/local-evaluate, pre-submit-intent, submit, and authenticated reconcile hooks for every transaction",
});

const freezeRegistrationV1 = (
  registration: ProductionWorkflowAdapterRegistrationV1,
): ProductionWorkflowAdapterRegistrationV1 =>
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
const productionWorkflowAdapterRegistrationRowsV1 = [
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
  manual("fabricatedDeposit", [
    "prepare-fabricated-deposit.ts",
    "submit-fabricated-deposit-step-01.ts..submit-fabricated-deposit-step-04.ts",
  ]),
  manual("fabricatedWithdrawal", [
    "prepare-fabricated-withdrawal.ts",
    "submit-fabricated-withdrawal-step-01.ts..submit-fabricated-withdrawal-step-04.ts",
  ]),
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
] as const satisfies readonly ProductionWorkflowAdapterRegistrationV1[];

export const PRODUCTION_WORKFLOW_ADAPTER_REGISTRATIONS_V1 = Object.freeze(
  productionWorkflowAdapterRegistrationRowsV1.map(freezeRegistrationV1),
);

export const validateProductionWorkflowAdapterCoverageV1 = (
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
      (registration.runner?.runnerVersion !==
        PRODUCTION_WORKFLOW_ADAPTER_RUNNER_V1 ||
        typeof registration.runner.runOrResume !== "function" ||
        !isAdmittedProductionWorkflowRunnerV1({
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

validateProductionWorkflowAdapterCoverageV1(
  PRODUCTION_WORKFLOW_ADAPTER_REGISTRATIONS_V1,
);

const admittedApplicationRegistriesV1 = new WeakSet<object>();

const assertCanonicalLaunchScopeV1 = (
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
export const installProductionWorkflowApplicationRegistryV1 = ({
  deploymentFingerprint,
  requiredInstalledCategories,
  installations,
}: {
  readonly deploymentFingerprint: string;
  readonly requiredInstalledCategories: readonly FraudProofCatalogueCategoryName[];
  readonly installations: readonly ProductionWorkflowApplicationRunnerInstallationV1[];
}): ProductionWorkflowApplicationRegistryV1 => {
  const normalizedDeploymentFingerprint = normalizeDaDeploymentFingerprintHex(
    deploymentFingerprint,
  );
  if (normalizedDeploymentFingerprint !== deploymentFingerprint) {
    throw new Error(
      "production workflow application deployment fingerprint is not canonical",
    );
  }
  assertCanonicalLaunchScopeV1(
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
    ProductionWorkflowApplicationRunnerInstallationV1
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
      installation.runner.runnerVersion !==
        PRODUCTION_WORKFLOW_ADAPTER_RUNNER_V1 ||
      typeof installation.runner.runOrResume !== "function" ||
      !isAdmittedProductionWorkflowRunnerV1({
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
    PRODUCTION_WORKFLOW_ADAPTER_REGISTRATIONS_V1.map((registration) => {
      const installation = installationByCategory.get(registration.category);
      if (installation === undefined) return registration;
      return freezeRegistrationV1({
        category: registration.category,
        status: "ready",
        adapterVersion:
          PRODUCTION_WORKFLOW_APPLICATION_REGISTRY_V1_SCHEMA_VERSION,
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
  validateProductionWorkflowAdapterCoverageV1(registrations);
  const registry = Object.freeze({
    schemaVersion: PRODUCTION_WORKFLOW_APPLICATION_REGISTRY_V1_SCHEMA_VERSION,
    deploymentFingerprint,
    installedCategories: Object.freeze([...requiredInstalledCategories]),
    registrations,
  });
  admittedApplicationRegistriesV1.add(registry);
  return registry;
};

export const assertProductionWorkflowApplicationRegistryV1 = (
  registry: ProductionWorkflowApplicationRegistryV1,
): void => {
  if (
    !admittedApplicationRegistriesV1.has(registry) ||
    registry.schemaVersion !==
      PRODUCTION_WORKFLOW_APPLICATION_REGISTRY_V1_SCHEMA_VERSION
  ) {
    throw new Error(
      "production workflow application registry was not installed through the authenticated immutable boundary",
    );
  }
  validateProductionWorkflowAdapterCoverageV1(registry.registrations);
};

const resolveProductionWorkflowRegistrationsV1 = (
  applicationRegistry?: ProductionWorkflowApplicationRegistryV1,
): readonly ProductionWorkflowAdapterRegistrationV1[] => {
  if (applicationRegistry === undefined) {
    return PRODUCTION_WORKFLOW_ADAPTER_REGISTRATIONS_V1;
  }
  assertProductionWorkflowApplicationRegistryV1(applicationRegistry);
  return applicationRegistry.registrations;
};

export const missingProductionWorkflowAdaptersV1 = (
  launchScope: readonly FraudProofCatalogueCategoryName[] = FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER,
  applicationRegistry?: ProductionWorkflowApplicationRegistryV1,
): readonly MissingProductionWorkflowAdapterRegistrationV1[] => {
  const registrations =
    resolveProductionWorkflowRegistrationsV1(applicationRegistry);
  validateProductionWorkflowAdapterCoverageV1(registrations);
  assertCanonicalLaunchScopeV1(launchScope, "production workflow launch scope");
  const scope = new Set(launchScope);
  return registrations.filter(
    (
      registration,
    ): registration is MissingProductionWorkflowAdapterRegistrationV1 =>
      registration.status === "missing" && scope.has(registration.category),
  );
};

export class MissingProductionWorkflowAdaptersErrorV1 extends Error {
  readonly missing: readonly MissingProductionWorkflowAdapterRegistrationV1[];

  constructor(
    missing: readonly MissingProductionWorkflowAdapterRegistrationV1[],
  ) {
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
export const assertProductionWorkflowAdaptersReadyV1 = (
  launchScope: readonly FraudProofCatalogueCategoryName[] = FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER,
  applicationRegistry?: ProductionWorkflowApplicationRegistryV1,
): void => {
  const missing = missingProductionWorkflowAdaptersV1(
    launchScope,
    applicationRegistry,
  );
  if (missing.length > 0) {
    throw new MissingProductionWorkflowAdaptersErrorV1(missing);
  }
};

export const productionWorkflowAdapterRunnerV1 = (
  category: FraudProofCatalogueCategoryName,
  applicationRegistry?: ProductionWorkflowApplicationRegistryV1,
): ProductionWorkflowAdapterRunnerV1 => {
  const registrations =
    resolveProductionWorkflowRegistrationsV1(applicationRegistry);
  validateProductionWorkflowAdapterCoverageV1(registrations);
  const registration = registrations.find(
    (candidate) => candidate.category === category,
  );
  if (registration === undefined || registration.status !== "ready") {
    assertProductionWorkflowAdaptersReadyV1([category], applicationRegistry);
    throw new Error(
      `production workflow registry invariant: ${category} has no ready registration`,
    );
  }
  return registration.runner;
};
