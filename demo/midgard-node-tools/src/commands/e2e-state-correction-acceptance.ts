import { workflowReadinessReport } from "@al-ft/midgard-fault-proofs";
import { FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER } from "@al-ft/midgard-sdk";

import type {
  DbEvidence,
  RawEvidenceRef,
  TransactionEvidence,
} from "../e2e/summary.js";

export const E2E_STATE_CORRECTION_ACCEPTANCE_SCHEMA_VERSION =
  "midgard-e2e-state-correction-acceptance-v1" as const;

export const REQUIRED_STATE_CORRECTION_RECOVERY_DRILL_IDS = [
  "crash-before-detect",
  "crash-after-detect",
  "crash-before-persist-evidence",
  "crash-after-persist-evidence",
  "crash-before-proof-init",
  "crash-after-proof-init",
  "crash-before-submit",
  "crash-after-submit",
  "crash-before-proof-token-confirm",
  "crash-after-proof-token-confirm",
  "crash-before-removal-slashing-confirm",
  "crash-after-removal-slashing-confirm",
  "crash-before-terminal-verification",
  "crash-after-terminal-verification",
  "l1-rollback-before-finality",
  "l1-rollback-after-finality-within-k",
  "configured-source-inconsistency",
  "external-provider-disagreement",
  "missing-da",
  "withholding",
  "stale-manifest",
  "recorded-live-chain-rewind",
] as const;

export const REQUIRED_STATE_CORRECTION_GATE_LABELS = [
  "state_correction_acceptance",
  "state_correction_exact_economics",
  "withdrawal_reserve_payout",
  "forced_classification_directions",
  "watcher_crash_rollback_matrix",
  "state_correction_final_reconciliation",
  "state_correction_local_workflow_readiness",
  "availability_challenge_readiness",
] as const;

export type StateCorrectionAvailabilityChallengeCapability =
  | "missing"
  | "authenticated_deployed";

type DeploymentBinding = {
  readonly manifestId: string;
  readonly blueprintSha256: string;
  readonly catalogueRoot: string;
  readonly parametersSha256: string;
  readonly releaseEvidenceSha256: string;
};

type ChainPoint = {
  readonly slot: string;
  readonly blockHash: string;
};

type StateCorrectionFamilyDrill = {
  readonly familyId: string;
  readonly violationId: string;
  readonly headerHash: string;
  readonly routeId: string;
  readonly detectionSource: "public-l1-da";
  readonly watcherDriven: true;
  readonly initTxHash: string;
  readonly proofStepTxHashes: readonly string[];
  readonly proofTokenTxHash: string;
  readonly removalTxHash: string;
  readonly correctionTxHash: string;
  readonly permanentProofTokenRetained: true;
  readonly stateQueueNodeRemoved: true;
  readonly correctedQueueObserved: true;
  readonly expectedSlashLovelace: string;
  readonly observedSlashLovelace: string;
  readonly expectedProverRewardLovelace: string;
  readonly observedProverRewardLovelace: string;
  readonly chainPoint: ChainPoint;
  readonly finalStateRoot: string;
};

type WithdrawalReservePayout = {
  readonly withdrawalOrderTxHash: string;
  readonly reserveTxHash: string;
  readonly payoutInitTxHash: string;
  readonly payoutAddTxHashes: readonly string[];
  readonly payoutConcludeTxHash: string;
  readonly expectedDestination: string;
  readonly observedDestination: string;
  readonly expectedPayoutValueSha256: string;
  readonly observedPayoutValueSha256: string;
  readonly expectedReserveValueSha256: string;
  readonly observedReserveValueSha256: string;
  readonly reserveAccountingExact: true;
  readonly finalStatus: "paid";
  readonly chainPoint: ChainPoint;
};

type ForcedClassificationDrill = {
  readonly direction: "valid-marked-invalid" | "invalid-marked-valid";
  readonly operatorClassification: "valid" | "invalid";
  readonly canonicalClassification: "valid" | "invalid";
  readonly finalClassification: "valid" | "invalid";
  readonly detectionSource: "public-l1-da";
  readonly watcherDriven: true;
  readonly routeId: string;
  readonly evidenceTxHash: string;
  readonly correctionTxHash: string;
  readonly corrected: true;
  readonly chainPoint: ChainPoint;
};

type RecoveryDrill = {
  readonly id: string;
  readonly status: "recovered";
  readonly failClosed: true;
  readonly duplicateSubmissions: 0;
  readonly lostEvidence: 0;
  readonly falseVerifiedStates: 0;
  readonly unrecoverableWorkflows: 0;
  readonly manualRepair: false;
  readonly watcherReadyAfterRecovery: true;
  readonly evidenceSha256: string;
};

export type E2EStateCorrectionAcceptance = {
  readonly schemaVersion: typeof E2E_STATE_CORRECTION_ACCEPTANCE_SCHEMA_VERSION;
  readonly runId: string;
  readonly network: "Preprod";
  readonly deployment: DeploymentBinding;
  readonly families: readonly StateCorrectionFamilyDrill[];
  readonly withdrawalReservePayout: WithdrawalReservePayout;
  readonly forcedClassifications: readonly ForcedClassificationDrill[];
  readonly recoveryDrills: readonly RecoveryDrill[];
  readonly finalState: {
    readonly stateQueueDepth: 0;
    readonly unfinishedMutationJobs: 0;
    readonly pendingFinalizations: 0;
    readonly watcherReady: true;
    readonly watcherVerificationResumed: true;
    readonly exactEconomicReconciliation: true;
    readonly finalStateSha256: string;
  };
};

/**
 * Finalizer-facing Q56/Q58 prerequisite gates. These are derived from compiled
 * runtime capability and the independently parsed release manifest, never
 * from the aggregate acceptance bundle.
 */
export const stateCorrectionLocalReadinessEvidence = ({
  availabilityChallengeCapability,
}: {
  readonly availabilityChallengeCapability: StateCorrectionAvailabilityChallengeCapability;
}): readonly DbEvidence[] => {
  const workflow = workflowReadinessReport();
  const missing = workflow.registrations.filter(
    (registration) => registration.status === "missing",
  );
  const registeredCategories = new Set(
    workflow.registrations.map((registration) => registration.category),
  );
  const unregisteredCategories = FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER.filter(
    (category) => !registeredCategories.has(category),
  );
  const duplicateRegistrationCount =
    workflow.registrations.length - registeredCategories.size;
  const exactRegistrationCoverage =
    workflow.registrations.length ===
      FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER.length &&
    unregisteredCategories.length === 0 &&
    duplicateRegistrationCount === 0;
  const workflowReady =
    exactRegistrationCoverage &&
    missing.length === 0 &&
    workflow.readyCategoryCount === FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER.length;
  return [
    {
      label: REQUIRED_STATE_CORRECTION_GATE_LABELS[6],
      status: workflowReady ? "satisfied" : "blocked",
      source: "compiled-production-workflow-registry-v1",
      details: {
        catalogueCategoryCount:
          FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER.length.toString(),
        actualRegistrationCount: workflow.registrations.length.toString(),
        uniqueRegistrationCount: registeredCategories.size.toString(),
        duplicateRegistrationCount: duplicateRegistrationCount.toString(),
        readyCategoryCount: workflow.readyCategoryCount.toString(),
        missingCategoryCount: workflow.missingCategoryCount.toString(),
        unregisteredCategories: unregisteredCategories.join(","),
        missingCategories: missing
          .map((registration) => registration.category)
          .join(","),
        missingReasons: missing
          .map(
            (registration) => `${registration.category}:${registration.reason}`,
          )
          .join(","),
      },
    },
    {
      label: REQUIRED_STATE_CORRECTION_GATE_LABELS[7],
      status:
        availabilityChallengeCapability === "authenticated_deployed"
          ? "satisfied"
          : "blocked",
      source: "finalized-deployment-manifest-v1",
      details: {
        capability: availabilityChallengeCapability,
        requirement:
          "authenticated Q58 challenge/respond/timeout/correct deployment and lifecycle",
      },
    },
  ];
};

const SHA256_PATTERN = /^[0-9a-f]{64}$/u;
const LOVELACE_PATTERN = /^(?:0|[1-9][0-9]*)$/u;

const record = (
  value: unknown,
  field: string,
): Readonly<Record<string, unknown>> => {
  if (typeof value !== "object" || value === null || Array.isArray(value)) {
    throw new Error(`${field} must be an object`);
  }
  return value as Readonly<Record<string, unknown>>;
};

const exactKeys = (
  value: Readonly<Record<string, unknown>>,
  keys: readonly string[],
  field: string,
): void => {
  const actual = Object.keys(value).sort();
  const expected = [...keys].sort();
  if (
    actual.length !== expected.length ||
    actual.some((key, index) => key !== expected[index])
  ) {
    throw new Error(
      `${field} must contain exactly: ${expected.join(", ")}; found: ${actual.join(", ")}`,
    );
  }
};

const string = (value: unknown, field: string): string => {
  if (
    typeof value !== "string" ||
    value.length === 0 ||
    value !== value.trim()
  ) {
    throw new Error(`${field} must be a non-empty canonical string`);
  }
  return value;
};

const literal = <T extends string | number | boolean>(
  value: unknown,
  expected: T,
  field: string,
): T => {
  if (value !== expected) {
    throw new Error(`${field} must be ${JSON.stringify(expected)}`);
  }
  return expected;
};

const sha256 = (value: unknown, field: string): string => {
  const parsed = string(value, field);
  if (!SHA256_PATTERN.test(parsed)) {
    throw new Error(`${field} must be lowercase SHA-256 hex`);
  }
  return parsed;
};

const positiveLovelace = (value: unknown, field: string): string => {
  const parsed = string(value, field);
  if (!LOVELACE_PATTERN.test(parsed) || BigInt(parsed) <= 0n) {
    throw new Error(`${field} must be a positive canonical lovelace string`);
  }
  return parsed;
};

const stringArray = (
  value: unknown,
  field: string,
  parse: (entry: unknown, entryField: string) => string = string,
): readonly string[] => {
  if (!Array.isArray(value) || value.length === 0) {
    throw new Error(`${field} must be a non-empty array`);
  }
  const parsed = value.map((entry, index) =>
    parse(entry, `${field}[${index.toString()}]`),
  );
  if (new Set(parsed).size !== parsed.length) {
    throw new Error(`${field} must not contain duplicates`);
  }
  return parsed;
};

const parseChainPoint = (value: unknown, field: string): ChainPoint => {
  const candidate = record(value, field);
  exactKeys(candidate, ["slot", "blockHash"], field);
  const slot = string(candidate.slot, `${field}.slot`);
  if (!LOVELACE_PATTERN.test(slot)) {
    throw new Error(`${field}.slot must be a canonical non-negative integer`);
  }
  return {
    slot,
    blockHash: sha256(candidate.blockHash, `${field}.blockHash`),
  };
};

const parseDeployment = (value: unknown): DeploymentBinding => {
  const candidate = record(value, "state-correction deployment");
  exactKeys(
    candidate,
    [
      "manifestId",
      "blueprintSha256",
      "catalogueRoot",
      "parametersSha256",
      "releaseEvidenceSha256",
    ],
    "state-correction deployment",
  );
  return {
    manifestId: sha256(candidate.manifestId, "deployment.manifestId"),
    blueprintSha256: sha256(
      candidate.blueprintSha256,
      "deployment.blueprintSha256",
    ),
    catalogueRoot: sha256(candidate.catalogueRoot, "deployment.catalogueRoot"),
    parametersSha256: sha256(
      candidate.parametersSha256,
      "deployment.parametersSha256",
    ),
    releaseEvidenceSha256: sha256(
      candidate.releaseEvidenceSha256,
      "deployment.releaseEvidenceSha256",
    ),
  };
};

const FAMILY_KEYS = [
  "familyId",
  "violationId",
  "headerHash",
  "routeId",
  "detectionSource",
  "watcherDriven",
  "initTxHash",
  "proofStepTxHashes",
  "proofTokenTxHash",
  "removalTxHash",
  "correctionTxHash",
  "permanentProofTokenRetained",
  "stateQueueNodeRemoved",
  "correctedQueueObserved",
  "expectedSlashLovelace",
  "observedSlashLovelace",
  "expectedProverRewardLovelace",
  "observedProverRewardLovelace",
  "chainPoint",
  "finalStateRoot",
] as const;

const parseFamily = (
  value: unknown,
  index: number,
): StateCorrectionFamilyDrill => {
  const field = `families[${index.toString()}]`;
  const candidate = record(value, field);
  exactKeys(candidate, FAMILY_KEYS, field);
  const expectedSlashLovelace = positiveLovelace(
    candidate.expectedSlashLovelace,
    `${field}.expectedSlashLovelace`,
  );
  const observedSlashLovelace = positiveLovelace(
    candidate.observedSlashLovelace,
    `${field}.observedSlashLovelace`,
  );
  const expectedProverRewardLovelace = positiveLovelace(
    candidate.expectedProverRewardLovelace,
    `${field}.expectedProverRewardLovelace`,
  );
  const observedProverRewardLovelace = positiveLovelace(
    candidate.observedProverRewardLovelace,
    `${field}.observedProverRewardLovelace`,
  );
  if (expectedSlashLovelace !== observedSlashLovelace) {
    throw new Error(`${field} observed slash does not equal expected slash`);
  }
  if (expectedProverRewardLovelace !== observedProverRewardLovelace) {
    throw new Error(
      `${field} observed prover reward does not equal expected prover reward`,
    );
  }
  return {
    familyId: string(candidate.familyId, `${field}.familyId`),
    violationId: string(candidate.violationId, `${field}.violationId`),
    headerHash: (() => {
      const parsed = string(candidate.headerHash, `${field}.headerHash`);
      if (!/^[0-9a-f]{56}$/u.test(parsed)) {
        throw new Error(`${field}.headerHash must be 28-byte lowercase hex`);
      }
      return parsed;
    })(),
    routeId: string(candidate.routeId, `${field}.routeId`),
    detectionSource: literal(
      candidate.detectionSource,
      "public-l1-da",
      `${field}.detectionSource`,
    ),
    watcherDriven: literal(
      candidate.watcherDriven,
      true,
      `${field}.watcherDriven`,
    ),
    initTxHash: sha256(candidate.initTxHash, `${field}.initTxHash`),
    proofStepTxHashes: stringArray(
      candidate.proofStepTxHashes,
      `${field}.proofStepTxHashes`,
      sha256,
    ),
    proofTokenTxHash: sha256(
      candidate.proofTokenTxHash,
      `${field}.proofTokenTxHash`,
    ),
    removalTxHash: sha256(candidate.removalTxHash, `${field}.removalTxHash`),
    correctionTxHash: sha256(
      candidate.correctionTxHash,
      `${field}.correctionTxHash`,
    ),
    permanentProofTokenRetained: literal(
      candidate.permanentProofTokenRetained,
      true,
      `${field}.permanentProofTokenRetained`,
    ),
    stateQueueNodeRemoved: literal(
      candidate.stateQueueNodeRemoved,
      true,
      `${field}.stateQueueNodeRemoved`,
    ),
    correctedQueueObserved: literal(
      candidate.correctedQueueObserved,
      true,
      `${field}.correctedQueueObserved`,
    ),
    expectedSlashLovelace,
    observedSlashLovelace,
    expectedProverRewardLovelace,
    observedProverRewardLovelace,
    chainPoint: parseChainPoint(candidate.chainPoint, `${field}.chainPoint`),
    finalStateRoot: sha256(candidate.finalStateRoot, `${field}.finalStateRoot`),
  };
};

const WITHDRAWAL_KEYS = [
  "withdrawalOrderTxHash",
  "reserveTxHash",
  "payoutInitTxHash",
  "payoutAddTxHashes",
  "payoutConcludeTxHash",
  "expectedDestination",
  "observedDestination",
  "expectedPayoutValueSha256",
  "observedPayoutValueSha256",
  "expectedReserveValueSha256",
  "observedReserveValueSha256",
  "reserveAccountingExact",
  "finalStatus",
  "chainPoint",
] as const;

const parseWithdrawal = (value: unknown): WithdrawalReservePayout => {
  const field = "withdrawalReservePayout";
  const candidate = record(value, field);
  exactKeys(candidate, WITHDRAWAL_KEYS, field);
  const expectedDestination = string(
    candidate.expectedDestination,
    `${field}.expectedDestination`,
  );
  const observedDestination = string(
    candidate.observedDestination,
    `${field}.observedDestination`,
  );
  const expectedPayoutValueSha256 = sha256(
    candidate.expectedPayoutValueSha256,
    `${field}.expectedPayoutValueSha256`,
  );
  const observedPayoutValueSha256 = sha256(
    candidate.observedPayoutValueSha256,
    `${field}.observedPayoutValueSha256`,
  );
  const expectedReserveValueSha256 = sha256(
    candidate.expectedReserveValueSha256,
    `${field}.expectedReserveValueSha256`,
  );
  const observedReserveValueSha256 = sha256(
    candidate.observedReserveValueSha256,
    `${field}.observedReserveValueSha256`,
  );
  if (expectedDestination !== observedDestination) {
    throw new Error(`${field} payout destination mismatch`);
  }
  if (expectedPayoutValueSha256 !== observedPayoutValueSha256) {
    throw new Error(`${field} payout value mismatch`);
  }
  if (expectedReserveValueSha256 !== observedReserveValueSha256) {
    throw new Error(`${field} reserve value mismatch`);
  }
  return {
    withdrawalOrderTxHash: sha256(
      candidate.withdrawalOrderTxHash,
      `${field}.withdrawalOrderTxHash`,
    ),
    reserveTxHash: sha256(candidate.reserveTxHash, `${field}.reserveTxHash`),
    payoutInitTxHash: sha256(
      candidate.payoutInitTxHash,
      `${field}.payoutInitTxHash`,
    ),
    payoutAddTxHashes: stringArray(
      candidate.payoutAddTxHashes,
      `${field}.payoutAddTxHashes`,
      sha256,
    ),
    payoutConcludeTxHash: sha256(
      candidate.payoutConcludeTxHash,
      `${field}.payoutConcludeTxHash`,
    ),
    expectedDestination,
    observedDestination,
    expectedPayoutValueSha256,
    observedPayoutValueSha256,
    expectedReserveValueSha256,
    observedReserveValueSha256,
    reserveAccountingExact: literal(
      candidate.reserveAccountingExact,
      true,
      `${field}.reserveAccountingExact`,
    ),
    finalStatus: literal(candidate.finalStatus, "paid", `${field}.finalStatus`),
    chainPoint: parseChainPoint(candidate.chainPoint, `${field}.chainPoint`),
  };
};

const FORCED_CLASSIFICATION_KEYS = [
  "direction",
  "operatorClassification",
  "canonicalClassification",
  "finalClassification",
  "detectionSource",
  "watcherDriven",
  "routeId",
  "evidenceTxHash",
  "correctionTxHash",
  "corrected",
  "chainPoint",
] as const;

const parseForcedClassification = (
  value: unknown,
  index: number,
): ForcedClassificationDrill => {
  const field = `forcedClassifications[${index.toString()}]`;
  const candidate = record(value, field);
  exactKeys(candidate, FORCED_CLASSIFICATION_KEYS, field);
  const direction = string(candidate.direction, `${field}.direction`);
  if (
    direction !== "valid-marked-invalid" &&
    direction !== "invalid-marked-valid"
  ) {
    throw new Error(`${field}.direction is not a required forced direction`);
  }
  const expected =
    direction === "valid-marked-invalid"
      ? {
          operator: "invalid" as const,
          canonical: "valid" as const,
          final: "valid" as const,
        }
      : {
          operator: "valid" as const,
          canonical: "invalid" as const,
          final: "invalid" as const,
        };
  return {
    direction,
    operatorClassification: literal(
      candidate.operatorClassification,
      expected.operator,
      `${field}.operatorClassification`,
    ),
    canonicalClassification: literal(
      candidate.canonicalClassification,
      expected.canonical,
      `${field}.canonicalClassification`,
    ),
    finalClassification: literal(
      candidate.finalClassification,
      expected.final,
      `${field}.finalClassification`,
    ),
    detectionSource: literal(
      candidate.detectionSource,
      "public-l1-da",
      `${field}.detectionSource`,
    ),
    watcherDriven: literal(
      candidate.watcherDriven,
      true,
      `${field}.watcherDriven`,
    ),
    routeId: string(candidate.routeId, `${field}.routeId`),
    evidenceTxHash: sha256(candidate.evidenceTxHash, `${field}.evidenceTxHash`),
    correctionTxHash: sha256(
      candidate.correctionTxHash,
      `${field}.correctionTxHash`,
    ),
    corrected: literal(candidate.corrected, true, `${field}.corrected`),
    chainPoint: parseChainPoint(candidate.chainPoint, `${field}.chainPoint`),
  };
};

const RECOVERY_KEYS = [
  "id",
  "status",
  "failClosed",
  "duplicateSubmissions",
  "lostEvidence",
  "falseVerifiedStates",
  "unrecoverableWorkflows",
  "manualRepair",
  "watcherReadyAfterRecovery",
  "evidenceSha256",
] as const;

const parseRecovery = (value: unknown, index: number): RecoveryDrill => {
  const field = `recoveryDrills[${index.toString()}]`;
  const candidate = record(value, field);
  exactKeys(candidate, RECOVERY_KEYS, field);
  return {
    id: string(candidate.id, `${field}.id`),
    status: literal(candidate.status, "recovered", `${field}.status`),
    failClosed: literal(candidate.failClosed, true, `${field}.failClosed`),
    duplicateSubmissions: literal(
      candidate.duplicateSubmissions,
      0,
      `${field}.duplicateSubmissions`,
    ),
    lostEvidence: literal(candidate.lostEvidence, 0, `${field}.lostEvidence`),
    falseVerifiedStates: literal(
      candidate.falseVerifiedStates,
      0,
      `${field}.falseVerifiedStates`,
    ),
    unrecoverableWorkflows: literal(
      candidate.unrecoverableWorkflows,
      0,
      `${field}.unrecoverableWorkflows`,
    ),
    manualRepair: literal(
      candidate.manualRepair,
      false,
      `${field}.manualRepair`,
    ),
    watcherReadyAfterRecovery: literal(
      candidate.watcherReadyAfterRecovery,
      true,
      `${field}.watcherReadyAfterRecovery`,
    ),
    evidenceSha256: sha256(candidate.evidenceSha256, `${field}.evidenceSha256`),
  };
};

const parseFinalState = (
  value: unknown,
): E2EStateCorrectionAcceptance["finalState"] => {
  const field = "finalState";
  const candidate = record(value, field);
  exactKeys(
    candidate,
    [
      "stateQueueDepth",
      "unfinishedMutationJobs",
      "pendingFinalizations",
      "watcherReady",
      "watcherVerificationResumed",
      "exactEconomicReconciliation",
      "finalStateSha256",
    ],
    field,
  );
  return {
    stateQueueDepth: literal(
      candidate.stateQueueDepth,
      0,
      `${field}.stateQueueDepth`,
    ),
    unfinishedMutationJobs: literal(
      candidate.unfinishedMutationJobs,
      0,
      `${field}.unfinishedMutationJobs`,
    ),
    pendingFinalizations: literal(
      candidate.pendingFinalizations,
      0,
      `${field}.pendingFinalizations`,
    ),
    watcherReady: literal(
      candidate.watcherReady,
      true,
      `${field}.watcherReady`,
    ),
    watcherVerificationResumed: literal(
      candidate.watcherVerificationResumed,
      true,
      `${field}.watcherVerificationResumed`,
    ),
    exactEconomicReconciliation: literal(
      candidate.exactEconomicReconciliation,
      true,
      `${field}.exactEconomicReconciliation`,
    ),
    finalStateSha256: sha256(
      candidate.finalStateSha256,
      `${field}.finalStateSha256`,
    ),
  };
};

export const parseE2EStateCorrectionAcceptance = (
  value: unknown,
): E2EStateCorrectionAcceptance => {
  const candidate = record(value, "state-correction acceptance evidence");
  exactKeys(
    candidate,
    [
      "schemaVersion",
      "runId",
      "network",
      "deployment",
      "families",
      "withdrawalReservePayout",
      "forcedClassifications",
      "recoveryDrills",
      "finalState",
    ],
    "state-correction acceptance evidence",
  );
  literal(
    candidate.schemaVersion,
    E2E_STATE_CORRECTION_ACCEPTANCE_SCHEMA_VERSION,
    "schemaVersion",
  );
  literal(candidate.network, "Preprod", "network");
  if (!Array.isArray(candidate.families)) {
    throw new Error("families must be an array");
  }
  const families = candidate.families.map(parseFamily);
  const expectedFamilies = [...FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER];
  const actualFamilies = families.map((family) => family.familyId);
  if (
    actualFamilies.length !== expectedFamilies.length ||
    actualFamilies.some((family, index) => family !== expectedFamilies[index])
  ) {
    throw new Error(
      `families must cover the launch-scope catalogue exactly in canonical order: ${expectedFamilies.join(",")}`,
    );
  }
  if (!Array.isArray(candidate.forcedClassifications)) {
    throw new Error("forcedClassifications must be an array");
  }
  const forcedClassifications = candidate.forcedClassifications.map(
    parseForcedClassification,
  );
  const requiredDirections = [
    "valid-marked-invalid",
    "invalid-marked-valid",
  ] as const;
  if (
    forcedClassifications.length !== requiredDirections.length ||
    forcedClassifications.some(
      (drill, index) => drill.direction !== requiredDirections[index],
    )
  ) {
    throw new Error(
      `forcedClassifications must contain exactly: ${requiredDirections.join(",")}`,
    );
  }
  if (!Array.isArray(candidate.recoveryDrills)) {
    throw new Error("recoveryDrills must be an array");
  }
  const recoveryDrills = candidate.recoveryDrills.map(parseRecovery);
  const actualRecoveryIds = recoveryDrills.map((drill) => drill.id);
  if (
    actualRecoveryIds.length !==
      REQUIRED_STATE_CORRECTION_RECOVERY_DRILL_IDS.length ||
    actualRecoveryIds.some(
      (id, index) => id !== REQUIRED_STATE_CORRECTION_RECOVERY_DRILL_IDS[index],
    )
  ) {
    throw new Error(
      `recoveryDrills must contain exactly: ${REQUIRED_STATE_CORRECTION_RECOVERY_DRILL_IDS.join(",")}`,
    );
  }
  return {
    schemaVersion: E2E_STATE_CORRECTION_ACCEPTANCE_SCHEMA_VERSION,
    runId: string(candidate.runId, "runId"),
    network: "Preprod",
    deployment: parseDeployment(candidate.deployment),
    families,
    withdrawalReservePayout: parseWithdrawal(candidate.withdrawalReservePayout),
    forcedClassifications,
    recoveryDrills,
    finalState: parseFinalState(candidate.finalState),
  };
};

export const stateCorrectionAcceptanceEvidence = ({
  expectedRunId,
  evidence,
  evidencePath,
}: {
  readonly expectedRunId: string;
  readonly evidence?: E2EStateCorrectionAcceptance;
  readonly evidencePath?: string;
}): {
  readonly db: readonly DbEvidence[];
  readonly transactions: readonly TransactionEvidence[];
  readonly rawEvidence: readonly RawEvidenceRef[];
  readonly notes: readonly string[];
} => {
  if (evidence === undefined || evidencePath === undefined) {
    return {
      db: [
        {
          label: REQUIRED_STATE_CORRECTION_GATE_LABELS[0],
          status: "failed",
          source: "e2e-finalize-summary",
          details: {
            missing: "--state-correction-evidence",
            requiredFamilies: FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER.join(","),
            requiredRecoveryDrills:
              REQUIRED_STATE_CORRECTION_RECOVERY_DRILL_IDS.join(","),
          },
        },
      ],
      transactions: [],
      rawEvidence: [],
      notes: [
        "State-correction acceptance is incomplete: no strict Q57/C83-C85/W45 evidence artifact was supplied.",
      ],
    };
  }
  const bindingMatches = evidence.runId === expectedRunId;
  // The summary must never promote claims from this aggregate artifact into
  // authenticated evidence. The family workflow journals, terminal L1
  // observations, recovery outputs, deployment manifest, blueprint,
  // parameters, release identity, economics, and final chain/queue state are
  // independent inputs. Until the finalizer has loaded and reconciled those
  // sources, a well-shaped aggregate is only an index and remains blocked.
  const status: DbEvidence["status"] = bindingMatches ? "blocked" : "failed";
  const details = {
    runId: evidence.runId,
    expectedRunId,
    manifestId: evidence.deployment.manifestId,
    blueprintSha256: evidence.deployment.blueprintSha256,
    catalogueRoot: evidence.deployment.catalogueRoot,
    parametersSha256: evidence.deployment.parametersSha256,
    releaseEvidenceSha256: evidence.deployment.releaseEvidenceSha256,
  };
  return {
    db: [
      {
        label: REQUIRED_STATE_CORRECTION_GATE_LABELS[0],
        status,
        source: "state-correction-acceptance-v1",
        details: {
          ...details,
          familyCount: evidence.families.length.toString(),
          detectionSource: "public-l1-da",
          watcherDriven: "true",
          correctedFamilyCount: evidence.families.length.toString(),
          independentEvidence:
            "workflow journals and authenticated terminal L1 observations not reconciled",
        },
      },
      {
        label: REQUIRED_STATE_CORRECTION_GATE_LABELS[1],
        status,
        source: "state-correction-acceptance-v1",
        details: {
          ...details,
          reconciledFamilies: evidence.families.length.toString(),
          exactFinalReconciliation:
            evidence.finalState.exactEconomicReconciliation.toString(),
        },
      },
      {
        label: REQUIRED_STATE_CORRECTION_GATE_LABELS[2],
        status,
        source: "state-correction-acceptance-v1",
        details: {
          ...details,
          destination: evidence.withdrawalReservePayout.observedDestination,
          payoutValueSha256:
            evidence.withdrawalReservePayout.observedPayoutValueSha256,
          reserveValueSha256:
            evidence.withdrawalReservePayout.observedReserveValueSha256,
          finalStatus: evidence.withdrawalReservePayout.finalStatus,
        },
      },
      {
        label: REQUIRED_STATE_CORRECTION_GATE_LABELS[3],
        status,
        source: "state-correction-acceptance-v1",
        details: {
          ...details,
          directions: evidence.forcedClassifications
            .map((drill) => drill.direction)
            .join(","),
        },
      },
      {
        label: REQUIRED_STATE_CORRECTION_GATE_LABELS[4],
        status,
        source: "state-correction-acceptance-v1",
        details: {
          ...details,
          recoveredCases: evidence.recoveryDrills
            .map((drill) => drill.id)
            .join(","),
        },
      },
      {
        label: REQUIRED_STATE_CORRECTION_GATE_LABELS[5],
        status,
        source: "state-correction-acceptance-v1",
        details: {
          ...details,
          stateQueueDepth: evidence.finalState.stateQueueDepth.toString(),
          unfinishedMutationJobs:
            evidence.finalState.unfinishedMutationJobs.toString(),
          pendingFinalizations:
            evidence.finalState.pendingFinalizations.toString(),
          watcherReady: evidence.finalState.watcherReady.toString(),
          watcherVerificationResumed:
            evidence.finalState.watcherVerificationResumed.toString(),
          finalStateSha256: evidence.finalState.finalStateSha256,
        },
      },
    ],
    transactions: [],
    rawEvidence: [{ label: "state-correction-acceptance", path: evidencePath }],
    notes: [
      `State-correction acceptance ${bindingMatches ? "blocked pending independent provenance" : "run-id mismatch"}: families=${evidence.families.length.toString()} recoveryDrills=${evidence.recoveryDrills.length.toString()} manifestId=${evidence.deployment.manifestId}`,
    ],
  };
};
