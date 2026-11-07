import { vi } from "vitest";

import { type FraudProofWorkflowIdentity } from "../src/workflow/journal.js";
import {
  FRAUD_PROOF_WORKFLOW_ADAPTER,
  FRAUD_PROOF_WORKFLOW_SAFETY,
  type FraudProofFamilyWorkflowAdapter,
  type FraudProofWorkflowAction,
} from "../src/workflow/orchestrator.js";
import {
  PROOF_CHUNK_PREREQUISITE,
  type ProofChunkPrerequisitePort,
} from "../src/workflow/proof-chunk-prerequisite.js";
import type { LocallyEvaluatedTransaction } from "../src/workflow/transaction-boundary.js";

export const txHash = "11".repeat(32);

export const headerHash = "22".repeat(28);

export const baseAction: FraudProofWorkflowAction = Object.freeze({
  actionId: `step_01:${"33".repeat(32)}#0:${"44".repeat(32)}#0`,
  input: Object.freeze({
    schemaVersion: "midgard-production-linear-family-action-v1",
    category: "invalidRange",
    stage: "step_01",
    ordinal: 1,
    threadOutRef: `${"33".repeat(32)}#0`,
    stateQueueBlockOutRef: `${"44".repeat(32)}#0`,
  }),
});

export const publicationAction: FraudProofWorkflowAction = Object.freeze({
  actionId: `publish-proof-chunks:${baseAction.actionId}:${"55".repeat(32)}`,
  input: Object.freeze({
    schemaVersion: PROOF_CHUNK_PREREQUISITE,
    category: "invalidRange",
    stage: "direct_or_publish_proof",
    forAction: baseAction,
    proofCborSha256: "55".repeat(32),
    chunkDatumSha256s: ["66".repeat(32)],
  }),
});

export const identity: FraudProofWorkflowIdentity = {
  schemaVersion: "midgard-fraud-proof-workflow-identity-v1",
  deploymentFingerprint: "77".repeat(32),
  category: "invalidRange",
  target: { kind: "state_queue_header", headerHash },
};

export const context = {
  identity,
  workflowId: "88".repeat(32),
  artifact: { proofCbor: "proof-from-public-da" },
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

export const transaction = (): LocallyEvaluatedTransaction => ({
  txHash,
  signed: signed(),
  referenceScripts: [],
});

export const base = ({
  preflightFailure,
}: {
  readonly preflightFailure?: Error;
} = {}): FraudProofFamilyWorkflowAdapter => ({
  adapterVersion: FRAUD_PROOF_WORKFLOW_ADAPTER,
  category: "invalidRange",
  safety: FRAUD_PROOF_WORKFLOW_SAFETY,
  prepare: vi.fn(async () => ({ proofCbor: "proof-from-public-da" })),
  observe: vi.fn(async () => ({
    kind: "action_required" as const,
    action: baseAction,
  })),
  preflight: vi.fn(async () => {
    if (preflightFailure !== undefined) throw preflightFailure;
    return {
      actionId: baseAction.actionId,
      txHash: "99".repeat(32),
      scriptExecution: "reference_scripts" as const,
      localUplcEvaluation: {
        status: "passed" as const,
        evaluator: "base-local-uplc",
      },
      referenceScripts: [
        {
          role: "invalid-range step-01",
          outRef: `${"aa".repeat(32)}#0`,
          scriptHash: "bb".repeat(28),
        },
      ],
    };
  }),
  submit: vi.fn(async () => ({
    kind: "submitted" as const,
    txHash: "99".repeat(32),
  })),
  reconcile: vi.fn(async () => ({
    kind: "confirmed" as const,
    txHash: "99".repeat(32),
  })),
});

export const prerequisite = ({
  satisfied = false,
  reconcile = async () => ({ kind: "confirmed" as const, txHash }),
}: {
  readonly satisfied?: boolean;
  readonly reconcile?: ProofChunkPrerequisitePort<"invalidRange">["reconcile"];
} = {}): ProofChunkPrerequisitePort<"invalidRange"> => ({
  portVersion: PROOF_CHUNK_PREREQUISITE,
  category: "invalidRange",
  classifyDirectCapacityFailure: vi.fn((cause: unknown) => {
    if (
      !(cause instanceof Error) ||
      cause.message !== "Max transaction size of 16384 exceeded. Found: 16385"
    ) {
      throw cause;
    }
    return {
      kind: "max_tx_size" as const,
      maximumTransactionBytes: 16_384,
      actualTransactionBytes: 16_385,
      errorSha256: "77".repeat(32),
    };
  }),
  inspect: vi.fn(async () =>
    satisfied
      ? { kind: "satisfied" as const }
      : { kind: "required" as const, action: publicationAction },
  ),
  capture: vi.fn(async () => ({
    transaction: transaction(),
    durableRecovery: {
      proofChunkPublication: {
        schemaVersion: "midgard-production-proof-chunk-publication-recovery-v1",
        proofCborSha256: "55".repeat(32),
        outputs: [
          {
            outRef: `${txHash}#0`,
            datumCbor: "d87980",
          },
        ],
      },
    },
  })),
  reconcile,
});
