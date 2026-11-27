import {
  assertSecurityGradeEvidence,
  type EvidenceProvenance,
} from "@al-ft/midgard-sdk/fraud-proof";

import type {
  FraudProofWorkflowAction,
  FraudProofWorkflowObservation,
} from "./orchestrator.js";
import type { FraudProofRawL1FamilyStage } from "./raw-l1-family-derivation.js";

export const MISSING_SIGNATURE_ACTION =
  "midgard-production-missing-signature-action-v1" as const;

const CATEGORY = "missingSignature" as const;
const OUT_REF = /^[0-9a-f]{64}#(?:0|[1-9][0-9]*)$/u;

const canonicalOutRef = (value: string, label: string): string => {
  if (!OUT_REF.test(value)) {
    throw new Error(`${label} is not a canonical Cardano output reference`);
  }
  return value;
};

const admitStage = ({
  headerHash,
  provenance,
  stage,
}: {
  readonly headerHash: string;
  readonly provenance: EvidenceProvenance;
  readonly stage: FraudProofRawL1FamilyStage;
}): FraudProofRawL1FamilyStage => {
  const admitted = assertSecurityGradeEvidence(provenance);
  if (admitted.trustClass !== "authenticated_cardano_l1") {
    throw new Error(
      "missingSignature workflow observation is not authenticated Cardano L1",
    );
  }
  if (stage.kind === "removed") {
    if (
      stage.terminal.category !== CATEGORY ||
      stage.terminal.headerHash !== headerHash
    ) {
      throw new Error(
        "missingSignature terminal changed its category or target header",
      );
    }
    return stage;
  }
  if (stage.kind === "pending") return stage;
  canonicalOutRef(stage.stateQueueBlockOutRef, "state-queue block outRef");
  if (stage.kind === "step") {
    if (stage.step < 1 || stage.step > 7) {
      throw new Error(
        `missingSignature authenticated L1 reported step ${stage.step.toString()} outside its exact production chain`,
      );
    }
    canonicalOutRef(stage.threadOutRef, "computation-thread outRef");
  }
  if (stage.kind === "proof_token") {
    canonicalOutRef(stage.fraudProofOutRef, "fraud-proof token outRef");
    canonicalOutRef(stage.nextRemovalOutRef, "next removal outRef");
  }
  return stage;
};

const action = (
  actionId: string,
  input: FraudProofWorkflowAction["input"],
): FraudProofWorkflowAction => Object.freeze({ actionId, input });

/**
 * Authenticated chain-state dispatch for missing-signature.
 *
 * Step 04 is intentionally not a fixed terminal step. Each non-terminal scan
 * creates another step-04 UTxO whose out-ref gives the next batch a distinct,
 * durable action identity. The on-chain datum carries the authenticated field
 * cursor; the workflow never invents or journals a second cursor.
 */
export const missingSignatureObservation = ({
  headerHash,
  provenance,
  stage,
}: {
  readonly headerHash: string;
  readonly provenance: EvidenceProvenance;
  readonly stage: FraudProofRawL1FamilyStage;
}): FraudProofWorkflowObservation => {
  const admitted = admitStage({ headerHash, provenance, stage });
  if (admitted.kind === "removed") {
    return { kind: "completed", terminal: admitted.terminal };
  }
  if (admitted.kind === "pending") {
    return { kind: "pending", reason: admitted.reason };
  }
  if (admitted.kind === "not_started") {
    return {
      kind: "action_required",
      action: action(`init:${admitted.stateQueueBlockOutRef}`, {
        schemaVersion: MISSING_SIGNATURE_ACTION,
        category: CATEGORY,
        stage: "init",
        stateQueueBlockOutRef: admitted.stateQueueBlockOutRef,
      }),
    };
  }
  if (admitted.kind === "step") {
    const stageName = `step_0${admitted.step.toString()}`;
    return {
      kind: "action_required",
      action: action(
        `${stageName}:${admitted.threadOutRef}:${admitted.stateQueueBlockOutRef}`,
        {
          schemaVersion: MISSING_SIGNATURE_ACTION,
          category: CATEGORY,
          stage: stageName,
          ordinal: admitted.step,
          threadOutRef: admitted.threadOutRef,
          stateQueueBlockOutRef: admitted.stateQueueBlockOutRef,
        },
      ),
    };
  }
  return {
    kind: "action_required",
    action: action(
      `remove:${admitted.nextRemovalOutRef}:${admitted.fraudProofOutRef}:${admitted.stateQueueBlockOutRef}`,
      {
        schemaVersion: MISSING_SIGNATURE_ACTION,
        category: CATEGORY,
        stage: "remove",
        fraudProofOutRef: admitted.fraudProofOutRef,
        stateQueueBlockOutRef: admitted.stateQueueBlockOutRef,
        nextRemovalOutRef: admitted.nextRemovalOutRef,
        requiresMutationLease:
          admitted.nextRemovalOutRef !== admitted.stateQueueBlockOutRef,
      },
    ),
  };
};
