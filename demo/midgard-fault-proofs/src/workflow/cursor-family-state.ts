import { type FraudProofCatalogueCategoryName } from "@al-ft/midgard-sdk/core";
import {
  assertSecurityGradeEvidence,
  type EvidenceProvenance,
} from "@al-ft/midgard-sdk/fraud-proof";

import {
  computationThreadStage,
  type ComputationThreadStep,
  isComputationThreadStep,
} from "./computation-thread-steps.js";
import type {
  FraudProofWorkflowAction,
  FraudProofWorkflowObservation,
} from "./orchestrator.js";
import type { FraudProofRawL1FamilyStage } from "./raw-l1-family-derivation.js";

export const CURSOR_FAMILY_ACTION =
  "midgard-production-cursor-family-action-v1" as const;

export type CursorFamilyStep = ComputationThreadStep;
export type CursorFamilySuccessor = CursorFamilyStep | "proof_token";

export type CursorFamilySpec<
  Category extends
    FraudProofCatalogueCategoryName = FraudProofCatalogueCategoryName,
> = Readonly<{
  category: Category;
  stepCount: CursorFamilyStep;
  /** Exact legal immediate successors for each step, including self-loops. */
  successors: Readonly<
    Partial<Record<CursorFamilyStep, readonly CursorFamilySuccessor[]>>
  >;
}>;

const OUT_REF = /^[0-9a-f]{64}#(?:0|[1-9][0-9]*)$/u;

const canonicalOutRef = (value: string, label: string): string => {
  if (!OUT_REF.test(value)) {
    throw new Error(`${label} is not a canonical Cardano output reference`);
  }
  return value;
};

const action = (
  actionId: string,
  input: FraudProofWorkflowAction["input"],
): FraudProofWorkflowAction => Object.freeze({ actionId, input });

const validateSpec = <Category extends FraudProofCatalogueCategoryName>(
  spec: CursorFamilySpec<Category>,
): CursorFamilySpec<Category> => {
  if (!isComputationThreadStep(spec.stepCount)) {
    throw new Error(`${spec.category} cursor spec has an invalid step count`);
  }
  for (let ordinal = 1; ordinal <= spec.stepCount; ordinal += 1) {
    const step = ordinal as CursorFamilyStep;
    const successors = spec.successors[step];
    if (
      successors === undefined ||
      successors.length === 0 ||
      new Set(successors).size !== successors.length ||
      successors.some(
        (candidate) =>
          candidate !== "proof_token" &&
          (!Number.isSafeInteger(candidate) ||
            candidate < 1 ||
            candidate > spec.stepCount),
      )
    ) {
      throw new Error(
        `${spec.category} cursor spec omits an exact legal successor for step ${ordinal.toString()}`,
      );
    }
  }
  if (
    Object.keys(spec.successors).some(
      (key) => Number(key) < 1 || Number(key) > spec.stepCount,
    )
  ) {
    throw new Error(`${spec.category} cursor spec declares an unknown step`);
  }
  return spec;
};

const admitStage = <Category extends FraudProofCatalogueCategoryName>({
  spec: inputSpec,
  headerHash,
  provenance,
  stage,
}: {
  readonly spec: CursorFamilySpec<Category>;
  readonly headerHash: string;
  readonly provenance: EvidenceProvenance;
  readonly stage: FraudProofRawL1FamilyStage;
}): FraudProofRawL1FamilyStage => {
  const spec = validateSpec(inputSpec);
  const admitted = assertSecurityGradeEvidence(provenance);
  if (admitted.trustClass !== "authenticated_cardano_l1") {
    throw new Error(
      `${spec.category} cursor observation is not authenticated Cardano L1`,
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
  canonicalOutRef(stage.stateQueueBlockOutRef, "state-queue block outRef");
  if (stage.kind === "step") {
    if (stage.step < 1 || stage.step > spec.stepCount) {
      throw new Error(
        `${spec.category} authenticated L1 reported step ${stage.step.toString()} outside its exact production chain`,
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

export const cursorFamilyObservation = <
  Category extends FraudProofCatalogueCategoryName,
>({
  spec,
  headerHash,
  provenance,
  stage,
}: {
  readonly spec: CursorFamilySpec<Category>;
  readonly headerHash: string;
  readonly provenance: EvidenceProvenance;
  readonly stage: FraudProofRawL1FamilyStage;
}): FraudProofWorkflowObservation => {
  const admitted = admitStage({ spec, headerHash, provenance, stage });
  if (admitted.kind === "removed") {
    return { kind: "completed", terminal: admitted.terminal };
  }
  if (admitted.kind === "not_started") {
    return {
      kind: "action_required",
      action: action(`init:${admitted.stateQueueBlockOutRef}`, {
        schemaVersion: CURSOR_FAMILY_ACTION,
        category: spec.category,
        stage: "init",
        stateQueueBlockOutRef: admitted.stateQueueBlockOutRef,
      }),
    };
  }
  if (admitted.kind === "step") {
    const stageName = computationThreadStage(admitted.step);
    return {
      kind: "action_required",
      action: action(
        `${stageName}:${admitted.threadOutRef}:${admitted.stateQueueBlockOutRef}`,
        {
          schemaVersion: CURSOR_FAMILY_ACTION,
          category: spec.category,
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
        schemaVersion: CURSOR_FAMILY_ACTION,
        category: spec.category,
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
