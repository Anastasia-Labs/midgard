import { vi } from "vitest";

import {
  FIELD_CARRIAGE_PREREQUISITE,
  type FieldCarriagePrerequisitePort,
} from "../src/workflow/field-carriage-prerequisite.js";
import type { FraudProofWorkflowIdentity } from "../src/workflow/journal.js";
import {
  FRAUD_PROOF_WORKFLOW_ADAPTER,
  FRAUD_PROOF_WORKFLOW_SAFETY,
  type FraudProofFamilyWorkflowAdapter,
  type FraudProofWorkflowAction,
} from "../src/workflow/orchestrator.js";
import type { LocallyEvaluatedTransaction } from "../src/workflow/transaction-boundary.js";

export const txHash = "11".repeat(32);

export const headerHash = "22".repeat(28);

const requirementSha256 = "33".repeat(32);

export const baseAction: FraudProofWorkflowAction = Object.freeze({
  actionId: `step_02:${"44".repeat(32)}#0:${"55".repeat(32)}#0`,
  input: Object.freeze({
    schemaVersion: "midgard-production-linear-family-action-v1",
    category: "nonExistentInput",
    stage: "step_02",
    ordinal: 2,
    threadOutRef: `${"44".repeat(32)}#0`,
    stateQueueBlockOutRef: `${"55".repeat(32)}#0`,
  }),
});

export const publicationAction: FraudProofWorkflowAction = Object.freeze({
  actionId: `publish-field-carriage:${baseAction.actionId}:${requirementSha256}:0`,
  input: Object.freeze({
    schemaVersion: FIELD_CARRIAGE_PREREQUISITE,
    category: "nonExistentInput",
    stage: "publish_field_carriage",
    forAction: baseAction,
    requirementSha256,
    publicationIndex: 0,
    publicationEncoding: "nothing_but_bytes",
    publicationDigest: "66".repeat(32),
    datumCborSha256: "77".repeat(32),
  }),
});

export const certificateAction: FraudProofWorkflowAction = Object.freeze({
  actionId: `certify-field-carriage:${baseAction.actionId}:${requirementSha256}`,
  input: Object.freeze({
    schemaVersion: FIELD_CARRIAGE_PREREQUISITE,
    category: "nonExistentInput",
    stage: "certify_field_carriage",
    forAction: baseAction,
    requirementSha256,
    certificateDatumCborSha256: "88".repeat(32),
    certificateUnit: "99".repeat(28) + "aa",
  }),
});

export const identity: FraudProofWorkflowIdentity = {
  schemaVersion: "midgard-fraud-proof-workflow-identity-v1",
  deploymentFingerprint: "ab".repeat(32),
  category: "nonExistentInput",
  target: { kind: "state_queue_header", headerHash },
};

export const context = {
  identity,
  workflowId: "bc".repeat(32),
  artifact: { source: "public-da" },
  entries: [],
} as const;

const signed = (): LocallyEvaluatedTransaction["signed"] =>
  ({
    toHash: () => txHash,
    submit: async () => txHash,
    toTransaction: () => ({
      witness_set: () => ({
        native_scripts: () => undefined,
        plutus_v1_scripts: () => undefined,
        plutus_v2_scripts: () => undefined,
        plutus_v3_scripts: () => undefined,
      }),
    }),
  }) as unknown as LocallyEvaluatedTransaction["signed"];

const transaction = (): LocallyEvaluatedTransaction => ({
  txHash,
  signed: signed(),
  referenceScripts: [],
});

export const base = (): FraudProofFamilyWorkflowAdapter => ({
  adapterVersion: FRAUD_PROOF_WORKFLOW_ADAPTER,
  category: "nonExistentInput",
  safety: FRAUD_PROOF_WORKFLOW_SAFETY,
  prepare: vi.fn(async () => ({ source: "public-da" })),
  observe: vi.fn(async () => ({
    kind: "action_required" as const,
    action: baseAction,
  })),
  preflight: vi.fn(async () => ({
    actionId: baseAction.actionId,
    txHash: "cd".repeat(32),
    scriptExecution: "reference_scripts" as const,
    localUplcEvaluation: {
      status: "passed" as const,
      evaluator: "base-local-uplc",
    },
    referenceScripts: [
      {
        role: "non-existent-input step-02",
        outRef: `${"de".repeat(32)}#0`,
        scriptHash: "ef".repeat(28),
      },
    ],
  })),
  submit: vi.fn(async () => ({
    kind: "submitted" as const,
    txHash: "cd".repeat(32),
  })),
  reconcile: vi.fn(async () => ({
    kind: "confirmed" as const,
    txHash: "cd".repeat(32),
  })),
});

export const prerequisite = ({
  phase = "publication",
  reconcile = async () => ({ kind: "confirmed" as const, txHash }),
}: {
  readonly phase?: "publication" | "certificate" | "satisfied";
  readonly reconcile?: FieldCarriagePrerequisitePort<"nonExistentInput">["reconcile"];
} = {}): FieldCarriagePrerequisitePort<"nonExistentInput"> => ({
  portVersion: FIELD_CARRIAGE_PREREQUISITE,
  category: "nonExistentInput",
  resolveAuthenticated: vi.fn(async () => ({
    publications: [],
    requirement: null,
  })),
  inspect: vi.fn(async () =>
    phase === "satisfied"
      ? { kind: "satisfied" as const }
      : {
          kind: "required" as const,
          action:
            phase === "publication" ? publicationAction : certificateAction,
        },
  ),
  capture: vi.fn(async () => ({
    transaction: transaction(),
    durableRecovery: {
      fieldCarriage: {
        schemaVersion: "midgard-production-field-carriage-recovery-v1",
        kind: phase,
        requirementSha256,
        outRef: `${txHash}#0`,
        datumCbor: "d87980",
        unit: null,
      },
    },
  })),
  reconcile,
});
