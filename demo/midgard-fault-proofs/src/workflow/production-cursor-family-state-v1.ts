import {
  assertSecurityGradeEvidence,
  type EvidenceProvenance,
  type FraudProofCatalogueCategoryName,
} from "@al-ft/midgard-sdk";

import type {
  FraudProofWorkflowAction,
  FraudProofWorkflowObservation,
  FraudProofWorkflowReconcileResult,
} from "./orchestrator-v1.js";
import type { FraudProofRawL1FamilyStage } from "./raw-l1-family-derivation-v1.js";

export const CURSOR_FAMILY_ACTION =
  "midgard-production-cursor-family-action-v1" as const;

export type CursorFamilyStep =
  | 1
  | 2
  | 3
  | 4
  | 5
  | 6
  | 7
  | 8
  | 9
  | 10
  | 11
  | 12
  | 13;
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
const TX_HASH = /^[0-9a-f]{64}$/u;

const canonicalOutRef = (value: string, label: string): string => {
  if (!OUT_REF.test(value)) {
    throw new Error(`${label} is not a canonical Cardano output reference`);
  }
  return value;
};

const outputBelongsToTransaction = (outRef: string, txHash: string): boolean =>
  outRef.split("#")[0] === txHash;

const action = (
  actionId: string,
  input: FraudProofWorkflowAction["input"],
): FraudProofWorkflowAction => Object.freeze({ actionId, input });

const validateSpec = <Category extends FraudProofCatalogueCategoryName>(
  spec: CursorFamilySpec<Category>,
): CursorFamilySpec<Category> => {
  if (
    !Number.isSafeInteger(spec.stepCount) ||
    spec.stepCount < 1 ||
    spec.stepCount > 13
  ) {
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
    const stageName = `step_${admitted.step.toString().padStart(2, "0")}`;
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

type ParsedAction = Readonly<{
  stage: "init" | "remove" | "step";
  ordinal?: CursorFamilyStep;
  inputOutRef: string;
  proofOutRef?: string;
}>;

const parsedAction = <Category extends FraudProofCatalogueCategoryName>(
  spec: CursorFamilySpec<Category>,
  workflowAction: FraudProofWorkflowAction,
): ParsedAction => {
  const input = workflowAction.input;
  if (
    input.schemaVersion !== CURSOR_FAMILY_ACTION ||
    input.category !== spec.category ||
    typeof input.stage !== "string"
  ) {
    throw new Error(`${spec.category} cursor action changed identity`);
  }
  if (input.stage === "init") {
    if (
      typeof input.stateQueueBlockOutRef !== "string" ||
      workflowAction.actionId !== `init:${input.stateQueueBlockOutRef}`
    ) {
      throw new Error(`${spec.category} init action changed its queue input`);
    }
    return {
      stage: "init",
      inputOutRef: canonicalOutRef(
        input.stateQueueBlockOutRef,
        `${spec.category} init queue outRef`,
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
      workflowAction.actionId !==
        `remove:${input.nextRemovalOutRef}:${input.fraudProofOutRef}:${input.stateQueueBlockOutRef}`
    ) {
      throw new Error(`${spec.category} removal action changed its inputs`);
    }
    return {
      stage: "remove",
      inputOutRef: canonicalOutRef(
        input.nextRemovalOutRef,
        `${spec.category} removal queue outRef`,
      ),
      proofOutRef: canonicalOutRef(
        input.fraudProofOutRef,
        `${spec.category} removal proof-token outRef`,
      ),
    };
  }
  if (!/^step_(?:0[1-9]|1[0-3])$/u.test(input.stage)) {
    throw new Error(`${spec.category} cursor action names an unknown stage`);
  }
  const ordinal = Number(input.stage.slice("step_".length)) as CursorFamilyStep;
  if (
    ordinal > spec.stepCount ||
    input.ordinal !== ordinal ||
    typeof input.threadOutRef !== "string" ||
    typeof input.stateQueueBlockOutRef !== "string" ||
    workflowAction.actionId !==
      `${input.stage}:${input.threadOutRef}:${input.stateQueueBlockOutRef}`
  ) {
    throw new Error(`${spec.category} cursor step changed order or input`);
  }
  return {
    stage: "step",
    ordinal,
    inputOutRef: canonicalOutRef(
      input.threadOutRef,
      `${spec.category} step thread outRef`,
    ),
  };
};

const exactSuccessor = <Category extends FraudProofCatalogueCategoryName>({
  spec,
  parsed,
  stage,
  txHash,
}: {
  readonly spec: CursorFamilySpec<Category>;
  readonly parsed: ParsedAction;
  readonly stage: FraudProofRawL1FamilyStage;
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
  const successors = spec.successors[parsed.ordinal!]!;
  return (
    (stage.kind === "step" &&
      successors.includes(stage.step) &&
      outputBelongsToTransaction(stage.threadOutRef, txHash)) ||
    (stage.kind === "proof_token" &&
      successors.includes("proof_token") &&
      outputBelongsToTransaction(stage.fraudProofOutRef, txHash))
  );
};

const sameRequiredAction = <Category extends FraudProofCatalogueCategoryName>({
  spec,
  headerHash,
  provenance,
  stage,
  actionId,
}: {
  readonly spec: CursorFamilySpec<Category>;
  readonly headerHash: string;
  readonly provenance: EvidenceProvenance;
  readonly stage: FraudProofRawL1FamilyStage;
  readonly actionId: string;
}): boolean => {
  const observed = cursorFamilyObservation({
    spec,
    headerHash,
    provenance,
    stage,
  });
  return (
    observed.kind === "action_required" && observed.action.actionId === actionId
  );
};

export const reconcileCursorFamilyAction = async <
  Category extends FraudProofCatalogueCategoryName,
>({
  spec: inputSpec,
  headerHash,
  action: workflowAction,
  txHash,
  provenance,
  stage,
  transactionConfirmed,
}: {
  readonly spec: CursorFamilySpec<Category>;
  readonly headerHash: string;
  readonly action: FraudProofWorkflowAction;
  readonly txHash?: string;
  readonly provenance: EvidenceProvenance;
  readonly stage: FraudProofRawL1FamilyStage;
  readonly transactionConfirmed: (txHash: string) => Promise<boolean>;
}): Promise<FraudProofWorkflowReconcileResult> => {
  const spec = validateSpec(inputSpec);
  const admittedStage = admitStage({ spec, headerHash, provenance, stage });
  const parsed = parsedAction(spec, workflowAction);
  const unchanged = () =>
    sameRequiredAction({
      spec,
      headerHash,
      provenance,
      stage: admittedStage,
      actionId: workflowAction.actionId,
    });
  if (txHash === undefined) {
    return unchanged() ||
      (parsed.stage === "remove" && admittedStage.kind === "proof_token")
      ? { kind: "not_found" }
      : {
          kind: "conflict",
          reason: `${spec.category} chain state advanced without the intended transaction hash`,
        };
  }
  if (!TX_HASH.test(txHash)) {
    throw new Error(`${spec.category} reconciliation tx hash is invalid`);
  }
  const included = await transactionConfirmed(txHash);
  if (
    included &&
    exactSuccessor({ spec, parsed, stage: admittedStage, txHash })
  ) {
    return { kind: "confirmed", txHash };
  }
  if (included && unchanged()) return { kind: "pending", txHash };
  if (!included && unchanged()) return { kind: "not_found" };
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
      ? `${spec.category} transaction did not produce a legal exact successor`
      : `${spec.category} chain state changed without the intended transaction`,
  };
};
