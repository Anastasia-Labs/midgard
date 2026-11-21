import {
  assertSecurityGradeEvidence,
  type EvidenceProvenance,
} from "@al-ft/midgard-sdk/fraud-proof";

import {
  LINEAR_FAMILY_SPEC,
  type LinearFamilyCategory,
  type LinearFamilySpec,
  linearFamilySpec,
  type LinearFamilyStep,
} from "./linear-family-spec.js";
import type {
  FraudProofWorkflowAction,
  FraudProofWorkflowObservation,
} from "./orchestrator.js";
import type { FraudProofRawL1FamilyStage } from "./raw-l1-family-derivation.js";

export const LINEAR_FAMILY_ACTION =
  "midgard-production-linear-family-action-v1" as const;

const OUT_REF = /^[0-9a-f]{64}#(?:0|[1-9][0-9]*)$/u;

const canonicalOutRef = (value: string, label: string): string => {
  if (!OUT_REF.test(value)) {
    throw new Error(`${label} is not a canonical Cardano output reference`);
  }
  return value;
};

const stageStep = (
  spec: LinearFamilySpec,
  ordinal: number,
): LinearFamilyStep => {
  const step = spec.steps[ordinal - 1];
  if (step === undefined || step.ordinal !== ordinal) {
    throw new Error(
      `${spec.category} authenticated L1 reported step ${ordinal.toString()} outside its exact production chain`,
    );
  }
  return step;
};

/** Strictly admits a raw-derived stage before it can control workflow dispatch. */
export const admitLinearFamilyStage = ({
  spec,
  headerHash,
  provenance,
  stage,
}: {
  readonly spec: LinearFamilySpec;
  readonly headerHash: string;
  readonly provenance: EvidenceProvenance;
  readonly stage: FraudProofRawL1FamilyStage;
}): FraudProofRawL1FamilyStage => {
  if (spec.schemaVersion !== LINEAR_FAMILY_SPEC) {
    throw new Error(`${spec.category} linear family spec version changed`);
  }
  const admitted = assertSecurityGradeEvidence(provenance);
  if (admitted.trustClass !== "authenticated_cardano_l1") {
    throw new Error(
      `${spec.category} linear workflow observation is not authenticated Cardano L1`,
    );
  }
  if (stage.kind === "removed") {
    if (
      stage.terminal.category !== spec.category ||
      stage.terminal.headerHash !== headerHash
    ) {
      throw new Error(
        `${spec.category} terminal changed its category or target header`,
      );
    }
    return stage;
  }
  if (stage.kind === "pending") return stage;
  canonicalOutRef(stage.stateQueueBlockOutRef, "state-queue block outRef");
  if (stage.kind === "step") {
    stageStep(spec, stage.step);
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
 * Maps one authenticated chain stage to exactly one content-addressed action.
 * Outrefs are part of the action id because rebuilding against a replaced
 * queue/thread input must create a new durable intent rather than mutate an
 * earlier action's meaning. Category and header are deliberately not repeated
 * in the id: the orchestrator's immutable workflow identity binds both before
 * an adapter is selected. The mutable state-queue outref is still included in
 * every step id because it is not fixed by that workflow identity.
 */
export const linearFamilyObservation = <Category extends LinearFamilyCategory>({
  category,
  headerHash,
  provenance,
  stage,
}: {
  readonly category: Category;
  readonly headerHash: string;
  readonly provenance: EvidenceProvenance;
  readonly stage: FraudProofRawL1FamilyStage;
}): FraudProofWorkflowObservation => {
  const spec = linearFamilySpec(category);
  const admitted = admitLinearFamilyStage({
    spec,
    headerHash,
    provenance,
    stage,
  });
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
        schemaVersion: LINEAR_FAMILY_ACTION,
        category,
        stage: "init",
        stateQueueBlockOutRef: admitted.stateQueueBlockOutRef,
      }),
    };
  }
  if (admitted.kind === "step") {
    const step = stageStep(spec, admitted.step);
    return {
      kind: "action_required",
      action: action(
        `${step.actionId}:${admitted.threadOutRef}:${admitted.stateQueueBlockOutRef}`,
        {
          schemaVersion: LINEAR_FAMILY_ACTION,
          category,
          stage: step.actionId,
          ordinal: step.ordinal,
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
        schemaVersion: LINEAR_FAMILY_ACTION,
        category,
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
