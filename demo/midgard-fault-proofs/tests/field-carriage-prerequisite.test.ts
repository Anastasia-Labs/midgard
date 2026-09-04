import { describe, expect, it, vi } from "vitest";

import {
  FIELD_CARRIAGE_PREREQUISITE,
  type FieldCarriagePrerequisitePort,
  withFieldCarriagePrerequisite,
} from "../src/workflow/field-carriage-prerequisite.js";
import type { FraudProofWorkflowIdentity } from "../src/workflow/journal.js";
import {
  FRAUD_PROOF_WORKFLOW_ADAPTER,
  FRAUD_PROOF_WORKFLOW_SAFETY,
  type FraudProofFamilyWorkflowAdapter,
  type FraudProofWorkflowAction,
} from "../src/workflow/orchestrator.js";
import type { LocallyEvaluatedTransaction } from "../src/workflow/transaction-boundary.js";

const txHash = "11".repeat(32);
const headerHash = "22".repeat(28);
const requirementSha256 = "33".repeat(32);
const baseAction: FraudProofWorkflowAction = Object.freeze({
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
const publicationAction: FraudProofWorkflowAction = Object.freeze({
  actionId: `publish-field-carriage:${baseAction.actionId}:${requirementSha256}:0`,
  input: Object.freeze({
    schemaVersion: FIELD_CARRIAGE_PREREQUISITE,
    category: "nonExistentInput",
    stage: "publish_field_carriage",
    forAction: baseAction,
    requirementSha256,
    publicationIndex: 0,
    publicationDigest: "66".repeat(32),
    datumCborSha256: "77".repeat(32),
  }),
});
const certificateAction: FraudProofWorkflowAction = Object.freeze({
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

const identity: FraudProofWorkflowIdentity = {
  schemaVersion: "midgard-fraud-proof-workflow-identity-v1",
  deploymentFingerprint: "ab".repeat(32),
  category: "nonExistentInput",
  target: { kind: "state_queue_header", headerHash },
};
const context = {
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

const base = (): FraudProofFamilyWorkflowAdapter => ({
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

const prerequisite = ({
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

describe("production field-carriage prerequisite V1", () => {
  it("journals the first raw publication and forbids direct step bypass", async () => {
    const underlying = base();
    const port = prerequisite();
    const adapter = withFieldCarriagePrerequisite({
      category: "nonExistentInput",
      base: underlying,
      prerequisite: port,
    });
    await expect(adapter.observe(context)).resolves.toEqual({
      kind: "action_required",
      action: publicationAction,
    });
    await expect(
      adapter.preflight({ ...context, action: baseAction }),
    ).rejects.toThrow("cannot bypass authenticated field carriage");
    expect(underlying.preflight).not.toHaveBeenCalled();
    const preflight = await adapter.preflight({
      ...context,
      action: publicationAction,
    });
    expect(preflight).toMatchObject({
      actionId: publicationAction.actionId,
      txHash,
      scriptExecution: "none",
      localUplcEvaluation: { status: "passed" },
    });
    await expect(
      adapter.submit({
        ...context,
        action: publicationAction,
        preflight,
      }),
    ).resolves.toEqual({ kind: "submitted", txHash });
  });

  it("makes tier-3 certification another distinct action", async () => {
    const adapter = withFieldCarriagePrerequisite({
      category: "nonExistentInput",
      base: base(),
      prerequisite: prerequisite({ phase: "certificate" }),
    });
    await expect(adapter.observe(context)).resolves.toEqual({
      kind: "action_required",
      action: certificateAction,
    });
    await expect(
      adapter.preflight({ ...context, action: certificateAction }),
    ).resolves.toMatchObject({ actionId: certificateAction.actionId, txHash });
  });

  it("delegates the proof step only after all field carriage is authenticated", async () => {
    const underlying = base();
    const adapter = withFieldCarriagePrerequisite({
      category: "nonExistentInput",
      base: underlying,
      prerequisite: prerequisite({ phase: "satisfied" }),
    });
    await expect(adapter.observe(context)).resolves.toEqual({
      kind: "action_required",
      action: baseAction,
    });
    await adapter.preflight({ ...context, action: baseAction });
    expect(underlying.preflight).toHaveBeenCalledOnce();
  });

  it("reconciles after restart and rejects duplicate or substituted captures", async () => {
    const reconcile = vi.fn(async () => ({
      kind: "confirmed" as const,
      txHash,
    }));
    const port = prerequisite({ reconcile });
    const adapter = withFieldCarriagePrerequisite({
      category: "nonExistentInput",
      base: base(),
      prerequisite: port,
    });
    await expect(
      adapter.reconcile({
        ...context,
        action: publicationAction,
        txHash,
        durableRecovery: { persisted: true },
      }),
    ).resolves.toEqual({ kind: "confirmed", txHash });
    expect(reconcile).toHaveBeenCalledOnce();
    await adapter.preflight({ ...context, action: publicationAction });
    await expect(
      adapter.preflight({ ...context, action: publicationAction }),
    ).rejects.toThrow("already captured this action");
    await expect(
      adapter.preflight({
        ...context,
        action: {
          ...publicationAction,
          input: {
            ...publicationAction.input,
            requirementSha256: "ff".repeat(32),
          },
        },
      }),
    ).rejects.toThrow("differs from current requirement");
    expect(port.capture).toHaveBeenCalledOnce();
  });
});
