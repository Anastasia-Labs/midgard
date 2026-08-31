import {
  assertSecurityGradeEvidenceV1,
  type EvidenceProvenanceV1,
} from "@al-ft/midgard-sdk";

import type {
  FraudProofWorkflowActionV1,
  FraudProofWorkflowObservationV1,
  FraudProofWorkflowReconcileResultV1,
} from "./orchestrator-v1.js";
import type { FraudProofRawL1FamilyStageV1 } from "./raw-l1-family-derivation-v1.js";

export const PRODUCTION_MISSING_SIGNATURE_ACTION_V1 =
  "midgard-production-missing-signature-action-v1" as const;

const CATEGORY = "missingSignature" as const;
const OUT_REF = /^[0-9a-f]{64}#(?:0|[1-9][0-9]*)$/u;
const TX_HASH = /^[0-9a-f]{64}$/u;

const canonicalOutRef = (value: string, label: string): string => {
  if (!OUT_REF.test(value)) {
    throw new Error(`${label} is not a canonical Cardano output reference`);
  }
  return value;
};

const transactionHashOf = (outRef: string): string => outRef.split("#")[0]!;

const outputBelongsToTransaction = (outRef: string, txHash: string): boolean =>
  transactionHashOf(outRef) === txHash;

const admitStage = ({
  headerHash,
  provenance,
  stage,
}: {
  readonly headerHash: string;
  readonly provenance: EvidenceProvenanceV1;
  readonly stage: FraudProofRawL1FamilyStageV1;
}): FraudProofRawL1FamilyStageV1 => {
  const admitted = assertSecurityGradeEvidenceV1(provenance);
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
  canonicalOutRef(stage.stateQueueBlockOutRef, "state-queue block outRef");
  if (stage.kind === "step") {
    if (stage.step < 1 || stage.step > 4) {
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
  input: FraudProofWorkflowActionV1["input"],
): FraudProofWorkflowActionV1 => Object.freeze({ actionId, input });

/**
 * Authenticated chain-state dispatch for missing-signature.
 *
 * Step 04 is intentionally not a fixed terminal step. Each non-terminal scan
 * creates another step-04 UTxO whose out-ref gives the next batch a distinct,
 * durable action identity. The on-chain datum carries the authenticated field
 * cursor; the workflow never invents or journals a second cursor.
 */
export const productionMissingSignatureObservationV1 = ({
  headerHash,
  provenance,
  stage,
}: {
  readonly headerHash: string;
  readonly provenance: EvidenceProvenanceV1;
  readonly stage: FraudProofRawL1FamilyStageV1;
}): FraudProofWorkflowObservationV1 => {
  const admitted = admitStage({ headerHash, provenance, stage });
  if (admitted.kind === "removed") {
    return { kind: "completed", terminal: admitted.terminal };
  }
  if (admitted.kind === "not_started") {
    return {
      kind: "action_required",
      action: action(`init:${admitted.stateQueueBlockOutRef}`, {
        schemaVersion: PRODUCTION_MISSING_SIGNATURE_ACTION_V1,
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
          schemaVersion: PRODUCTION_MISSING_SIGNATURE_ACTION_V1,
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
        schemaVersion: PRODUCTION_MISSING_SIGNATURE_ACTION_V1,
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

type ParsedActionV1 = Readonly<{
  stage: "init" | "remove" | `step_0${1 | 2 | 3 | 4}`;
  ordinal?: 1 | 2 | 3 | 4;
  inputOutRef: string;
  proofOutRef?: string;
}>;

const parsedAction = (action: FraudProofWorkflowActionV1): ParsedActionV1 => {
  const input = action.input;
  if (
    input.schemaVersion !== PRODUCTION_MISSING_SIGNATURE_ACTION_V1 ||
    input.category !== CATEGORY ||
    typeof input.stage !== "string"
  ) {
    throw new Error("missingSignature workflow action changed identity");
  }
  if (input.stage === "init") {
    if (
      typeof input.stateQueueBlockOutRef !== "string" ||
      action.actionId !== `init:${input.stateQueueBlockOutRef}`
    ) {
      throw new Error("missingSignature init action changed its queue input");
    }
    return {
      stage: "init",
      inputOutRef: canonicalOutRef(
        input.stateQueueBlockOutRef,
        "missingSignature init state-queue block outRef",
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
      throw new Error("missingSignature removal action changed its inputs");
    }
    return {
      stage: "remove",
      inputOutRef: canonicalOutRef(
        input.nextRemovalOutRef,
        "missingSignature removal queue input outRef",
      ),
      proofOutRef: canonicalOutRef(
        input.fraudProofOutRef,
        "missingSignature removal proof-token outRef",
      ),
    };
  }
  if (!/^step_0[1-4]$/u.test(input.stage)) {
    throw new Error("missingSignature workflow action names an unknown stage");
  }
  const ordinal = Number(input.stage.slice(-1)) as 1 | 2 | 3 | 4;
  if (
    input.ordinal !== ordinal ||
    typeof input.threadOutRef !== "string" ||
    typeof input.stateQueueBlockOutRef !== "string" ||
    action.actionId !==
      `${input.stage}:${input.threadOutRef}:${input.stateQueueBlockOutRef}`
  ) {
    throw new Error(
      "missingSignature step action changed order or thread input",
    );
  }
  return {
    stage: input.stage as ParsedActionV1["stage"],
    ordinal,
    inputOutRef: canonicalOutRef(
      input.threadOutRef,
      `${input.stage} computation-thread outRef`,
    ),
  };
};

const exactSuccessor = ({
  parsed,
  stage,
  txHash,
}: {
  readonly parsed: ParsedActionV1;
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
  if (ordinal < 4) {
    return (
      stage.kind === "step" &&
      stage.step === ordinal + 1 &&
      outputBelongsToTransaction(stage.threadOutRef, txHash)
    );
  }
  // Step 04 either advances the authenticated cursor at the same validator or
  // completes by minting the permanent fraud-proof token.
  return (
    (stage.kind === "step" &&
      stage.step === 4 &&
      outputBelongsToTransaction(stage.threadOutRef, txHash)) ||
    (stage.kind === "proof_token" &&
      outputBelongsToTransaction(stage.fraudProofOutRef, txHash))
  );
};

const sameRequiredAction = ({
  headerHash,
  provenance,
  stage,
  actionId,
}: {
  readonly headerHash: string;
  readonly provenance: EvidenceProvenanceV1;
  readonly stage: FraudProofRawL1FamilyStageV1;
  readonly actionId: string;
}): boolean => {
  const observed = productionMissingSignatureObservationV1({
    headerHash,
    provenance,
    stage,
  });
  return (
    observed.kind === "action_required" && observed.action.actionId === actionId
  );
};

/** Reconciles one durable action solely from authenticated Cardano L1 state. */
export const reconcileProductionMissingSignatureActionV1 = async ({
  headerHash,
  action,
  txHash,
  provenance,
  stage,
  transactionConfirmed,
}: {
  readonly headerHash: string;
  readonly action: FraudProofWorkflowActionV1;
  readonly txHash?: string;
  readonly provenance: EvidenceProvenanceV1;
  readonly stage: FraudProofRawL1FamilyStageV1;
  readonly transactionConfirmed: (txHash: string) => Promise<boolean>;
}): Promise<FraudProofWorkflowReconcileResultV1> => {
  const admittedStage = admitStage({ headerHash, provenance, stage });
  const parsed = parsedAction(action);
  if (txHash === undefined) {
    return sameRequiredAction({
      headerHash,
      provenance,
      stage: admittedStage,
      actionId: action.actionId,
    }) ||
      (parsed.stage === "remove" && admittedStage.kind === "proof_token")
      ? { kind: "not_found" }
      : {
          kind: "conflict",
          reason:
            "missingSignature chain state advanced without the intended transaction hash",
        };
  }
  if (!TX_HASH.test(txHash)) {
    throw new Error(
      "missingSignature reconciliation transaction hash is invalid",
    );
  }
  const included = await transactionConfirmed(txHash);
  if (included && exactSuccessor({ parsed, stage: admittedStage, txHash })) {
    return { kind: "confirmed", txHash };
  }
  const unchanged = sameRequiredAction({
    headerHash,
    provenance,
    stage: admittedStage,
    actionId: action.actionId,
  });
  if (included && unchanged) return { kind: "pending", txHash };
  if (!included && unchanged) return { kind: "not_found" };
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
      ? "missingSignature transaction did not produce its exact immediate successor"
      : "missingSignature chain state changed without the intended transaction",
  };
};
