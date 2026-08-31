import {
  assertSecurityGradeEvidenceV1,
  type EvidenceProvenanceV1,
} from "@al-ft/midgard-sdk";

import type { FraudProofWorkflowTerminalV1 } from "./journal-v1.js";
import type {
  FraudProofWorkflowActionV1,
  FraudProofWorkflowObservationV1,
  FraudProofWorkflowReconcileResultV1,
} from "./orchestrator-v1.js";
import {
  PRODUCTION_LINEAR_FAMILY_SPEC_V1,
  type ProductionLinearFamilyCategoryV1,
  type ProductionLinearFamilySpecV1,
  productionLinearFamilySpecV1,
  type ProductionLinearFamilyStepV1,
} from "./production-linear-family-spec-v1.js";
import type { FraudProofRawL1FamilyStageV1 } from "./raw-l1-family-derivation-v1.js";

export const PRODUCTION_LINEAR_FAMILY_ACTION_V1 =
  "midgard-production-linear-family-action-v1" as const;

const OUT_REF = /^[0-9a-f]{64}#(?:0|[1-9][0-9]*)$/u;
const TX_HASH = /^[0-9a-f]{64}$/u;

const canonicalOutRef = (value: string, label: string): string => {
  if (!OUT_REF.test(value)) {
    throw new Error(`${label} is not a canonical Cardano output reference`);
  }
  return value;
};

const transactionHashOf = (outRef: string): string => outRef.split("#")[0]!;

const stageStep = (
  spec: ProductionLinearFamilySpecV1,
  ordinal: number,
): ProductionLinearFamilyStepV1 => {
  const step = spec.steps[ordinal - 1];
  if (step === undefined || step.ordinal !== ordinal) {
    throw new Error(
      `${spec.category} authenticated L1 reported step ${ordinal.toString()} outside its exact production chain`,
    );
  }
  return step;
};

/** Strictly admits a raw-derived stage before it can control workflow dispatch. */
export const admitProductionLinearFamilyStageV1 = ({
  spec,
  headerHash,
  provenance,
  stage,
}: {
  readonly spec: ProductionLinearFamilySpecV1;
  readonly headerHash: string;
  readonly provenance: EvidenceProvenanceV1;
  readonly stage: FraudProofRawL1FamilyStageV1;
}): FraudProofRawL1FamilyStageV1 => {
  if (spec.schemaVersion !== PRODUCTION_LINEAR_FAMILY_SPEC_V1) {
    throw new Error(`${spec.category} linear family spec version changed`);
  }
  const admitted = assertSecurityGradeEvidenceV1(provenance);
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
  input: FraudProofWorkflowActionV1["input"],
): FraudProofWorkflowActionV1 => Object.freeze({ actionId, input });

/**
 * Maps one authenticated chain stage to exactly one content-addressed action.
 * Outrefs are part of the action id because rebuilding against a replaced
 * queue/thread input must create a new durable intent rather than mutate an
 * earlier action's meaning. Category and header are deliberately not repeated
 * in the id: the orchestrator's immutable workflow identity binds both before
 * an adapter is selected. The mutable state-queue outref is still included in
 * every step id because it is not fixed by that workflow identity.
 */
export const productionLinearFamilyObservationV1 = <
  Category extends ProductionLinearFamilyCategoryV1,
>({
  category,
  headerHash,
  provenance,
  stage,
}: {
  readonly category: Category;
  readonly headerHash: string;
  readonly provenance: EvidenceProvenanceV1;
  readonly stage: FraudProofRawL1FamilyStageV1;
}): FraudProofWorkflowObservationV1 => {
  const spec = productionLinearFamilySpecV1(category);
  const admitted = admitProductionLinearFamilyStageV1({
    spec,
    headerHash,
    provenance,
    stage,
  });
  if (admitted.kind === "removed") {
    return { kind: "completed", terminal: admitted.terminal };
  }
  if (admitted.kind === "not_started") {
    return {
      kind: "action_required",
      action: action(`init:${admitted.stateQueueBlockOutRef}`, {
        schemaVersion: PRODUCTION_LINEAR_FAMILY_ACTION_V1,
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
          schemaVersion: PRODUCTION_LINEAR_FAMILY_ACTION_V1,
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
        schemaVersion: PRODUCTION_LINEAR_FAMILY_ACTION_V1,
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

type LinearActionStageV1 = "init" | "remove" | `step_0${1 | 2 | 3 | 4}`;

const parsedAction = ({
  category,
  action,
}: {
  readonly category: ProductionLinearFamilyCategoryV1;
  readonly action: FraudProofWorkflowActionV1;
}): {
  readonly stage: LinearActionStageV1;
  readonly ordinal?: 1 | 2 | 3 | 4;
  readonly inputOutRef: string;
  readonly proofOutRef?: string;
} => {
  const input = action.input;
  if (
    input.schemaVersion !== PRODUCTION_LINEAR_FAMILY_ACTION_V1 ||
    input.category !== category ||
    typeof input.stage !== "string"
  ) {
    throw new Error(`${category} workflow action changed identity`);
  }
  if (input.stage === "init") {
    if (
      typeof input.stateQueueBlockOutRef !== "string" ||
      action.actionId !== `init:${input.stateQueueBlockOutRef}`
    ) {
      throw new Error(`${category} init action changed its queue input`);
    }
    return {
      stage: "init",
      inputOutRef: canonicalOutRef(
        input.stateQueueBlockOutRef,
        "init state-queue block outRef",
      ),
    };
  }
  if (input.stage === "remove") {
    if (
      typeof input.nextRemovalOutRef !== "string" ||
      typeof input.fraudProofOutRef !== "string" ||
      typeof input.stateQueueBlockOutRef !== "string" ||
      typeof input.requiresMutationLease !== "boolean" ||
      input.requiresMutationLease !==
        (input.nextRemovalOutRef !== input.stateQueueBlockOutRef) ||
      action.actionId !==
        `remove:${input.nextRemovalOutRef}:${input.fraudProofOutRef}:${input.stateQueueBlockOutRef}`
    ) {
      throw new Error(`${category} removal action changed its inputs`);
    }
    return {
      stage: "remove",
      inputOutRef: canonicalOutRef(
        input.nextRemovalOutRef,
        "removal queue input outRef",
      ),
      proofOutRef: canonicalOutRef(
        input.fraudProofOutRef,
        "removal proof-token outRef",
      ),
    };
  }
  if (!/^step_0[1-4]$/u.test(input.stage)) {
    throw new Error(`${category} workflow action names an unknown stage`);
  }
  const ordinal = Number(input.stage.slice(-1)) as 1 | 2 | 3 | 4;
  const step = stageStep(productionLinearFamilySpecV1(category), ordinal);
  if (
    input.stage !== step.actionId ||
    input.ordinal !== ordinal ||
    typeof input.threadOutRef !== "string" ||
    typeof input.stateQueueBlockOutRef !== "string" ||
    action.actionId !==
      `${step.actionId}:${input.threadOutRef}:${input.stateQueueBlockOutRef}`
  ) {
    throw new Error(`${category} step action changed order or thread input`);
  }
  return {
    stage: step.actionId,
    ordinal,
    inputOutRef: canonicalOutRef(
      input.threadOutRef,
      `${step.actionId} computation-thread outRef`,
    ),
  };
};

const outputBelongsToTransaction = (outRef: string, txHash: string): boolean =>
  transactionHashOf(outRef) === txHash;

const exactSuccessor = ({
  spec,
  parsed,
  stage,
  txHash,
}: {
  readonly spec: ProductionLinearFamilySpecV1;
  readonly parsed: ReturnType<typeof parsedAction>;
  readonly stage: FraudProofRawL1FamilyStageV1;
  readonly txHash: string;
}): boolean => {
  if (parsed.stage === "init") {
    return (
      stage.kind === "step" &&
      stage.step === 1 &&
      outputBelongsToTransaction(stage.threadOutRef, txHash)
    );
  }
  if (parsed.stage === "remove") {
    return (
      stage.kind === "removed" &&
      stage.terminal.correction.removalTxHash === txHash &&
      stage.terminal.correction.removedStateQueueOutRef ===
        parsed.inputOutRef &&
      stage.terminal.correction.referencedProofTokenOutRef ===
        parsed.proofOutRef
    );
  }
  const ordinal = parsed.ordinal!;
  if (ordinal === spec.steps.length) {
    return (
      stage.kind === "proof_token" &&
      outputBelongsToTransaction(stage.fraudProofOutRef, txHash)
    );
  }
  return (
    stage.kind === "step" &&
    stage.step === ordinal + 1 &&
    outputBelongsToTransaction(stage.threadOutRef, txHash)
  );
};

const sameRequiredAction = ({
  category,
  headerHash,
  provenance,
  stage,
  actionId,
}: {
  readonly category: ProductionLinearFamilyCategoryV1;
  readonly headerHash: string;
  readonly provenance: EvidenceProvenanceV1;
  readonly stage: FraudProofRawL1FamilyStageV1;
  readonly actionId: string;
}): boolean => {
  const observed = productionLinearFamilyObservationV1({
    category,
    headerHash,
    provenance,
    stage,
  });
  return (
    observed.kind === "action_required" && observed.action.actionId === actionId
  );
};

/**
 * Reconciles only against raw-derived L1 facts. A confirmed transaction must
 * produce the immediate content-addressed successor; skipped/reordered stages
 * are a conflict. A stale removal input that was never included may be rebuilt
 * under its new outref and therefore returns `not_found`.
 */
export const reconcileProductionLinearFamilyActionV1 = async ({
  category,
  headerHash,
  action,
  txHash,
  provenance,
  stage,
  transactionConfirmed,
}: {
  readonly category: ProductionLinearFamilyCategoryV1;
  readonly headerHash: string;
  readonly action: FraudProofWorkflowActionV1;
  readonly txHash?: string;
  readonly provenance: EvidenceProvenanceV1;
  readonly stage: FraudProofRawL1FamilyStageV1;
  readonly transactionConfirmed: (txHash: string) => Promise<boolean>;
}): Promise<FraudProofWorkflowReconcileResultV1> => {
  const spec = productionLinearFamilySpecV1(category);
  const admittedStage = admitProductionLinearFamilyStageV1({
    spec,
    headerHash,
    provenance,
    stage,
  });
  const parsed = parsedAction({ category, action });
  if (txHash === undefined) {
    return sameRequiredAction({
      category,
      headerHash,
      provenance,
      stage: admittedStage,
      actionId: action.actionId,
    }) ||
      (parsed.stage === "remove" && admittedStage.kind === "proof_token")
      ? { kind: "not_found" }
      : {
          kind: "conflict",
          reason: `${category} chain state advanced without the intended transaction hash`,
        };
  }
  if (!TX_HASH.test(txHash)) {
    throw new Error(`${category} reconciliation transaction hash is invalid`);
  }
  const included = await transactionConfirmed(txHash);
  if (
    included &&
    exactSuccessor({ spec, parsed, stage: admittedStage, txHash })
  ) {
    return { kind: "confirmed", txHash };
  }
  const unchanged = sameRequiredAction({
    category,
    headerHash,
    provenance,
    stage: admittedStage,
    actionId: action.actionId,
  });
  if (included && unchanged) {
    return { kind: "pending", txHash };
  }
  if (!included && unchanged) {
    return { kind: "not_found" };
  }
  if (
    !included &&
    parsed.stage === "remove" &&
    admittedStage.kind === "proof_token"
  ) {
    return { kind: "not_found" };
  }
  return {
    kind: "conflict",
    reason: included
      ? `${category} transaction did not produce its exact immediate successor`
      : `${category} chain state changed without the intended transaction`,
  };
};

export type ProductionLinearFamilyObservedTerminalV1 = Readonly<{
  kind: "completed";
  terminal: FraudProofWorkflowTerminalV1;
}>;
