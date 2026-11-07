import { FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER } from "@al-ft/midgard-sdk";

import type {
  DbEvidence,
  RawEvidenceRef,
  TransactionEvidence,
} from "../e2e/summary.js";
import {
  type E2EStateCorrectionAcceptance,
  REQUIRED_STATE_CORRECTION_GATE_LABELS,
  REQUIRED_STATE_CORRECTION_RECOVERY_DRILL_IDS,
} from "./e2e-state-correction-acceptance.required-state-correction-recovery-drill-ids.js";

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
    // No run produces this evidence yet (an e2e-stack run drives no fault
    // proof), so each gate is blocked as not run: never satisfied, and never
    // a functional failure of the run that was checked.
    return {
      db: REQUIRED_STATE_CORRECTION_GATE_LABELS.map((label) => ({
        label,
        status: "blocked",
        source: "e2e-finalize-summary",
        details: {
          reason: "not run",
          missing: "--state-correction-evidence",
          requiredFamilies: FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER.join(","),
          requiredRecoveryDrills:
            REQUIRED_STATE_CORRECTION_RECOVERY_DRILL_IDS.join(","),
        },
      })),
      transactions: [],
      rawEvidence: [],
      notes: [
        "State-correction acceptance was not run: no strict Q57/C83-C85/W45 evidence artifact was supplied, so release readiness stays blocked.",
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
