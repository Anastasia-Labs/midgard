import { FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER } from "@al-ft/midgard-sdk";

import {
  parseFamily,
  parseForcedClassification,
  parseWithdrawal,
  RECOVERY_KEYS,
} from "./e2e-state-correction-acceptance.parse-family.js";
import {
  E2E_STATE_CORRECTION_ACCEPTANCE_SCHEMA_VERSION,
  type E2EStateCorrectionAcceptance,
  exactKeys,
  literal,
  parseDeployment,
  record,
  type RecoveryDrill,
  REQUIRED_STATE_CORRECTION_RECOVERY_DRILL_IDS,
  sha256,
  string,
} from "./e2e-state-correction-acceptance.required-state-correction-recovery-drill-ids.js";

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
