import { type LucidEvolution, type UTxO } from "@lucid-evolution/lucid";
import { describe, expect, it, vi } from "vitest";

import {
  createAuthenticatedFieldCarriagePrerequisitePort,
  FIELD_CARRIAGE_PREREQUISITE,
  FIELD_CARRIAGE_RECOVERY,
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
import { createRawDatumPreimageRequirement } from "../src/workflow/raw-datum-preimage.js";
import { withRawDatumPreimagePrerequisite } from "../src/workflow/raw-datum-preimage-prerequisite.js";
import { FRAUD_PROOF_AUTHENTICATED_PUBLICATION_OBSERVER } from "../src/workflow/raw-l1-publication-observation.js";
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
  it("waits for publication finality while refusing publication and proof preflight", async () => {
    const underlying = base();
    const port = prerequisite();
    vi.mocked(port.inspect).mockResolvedValue({
      kind: "pending",
      reason: "field publication is not release-final",
    });
    const adapter = withFieldCarriagePrerequisite({
      category: "nonExistentInput",
      base: underlying,
      prerequisite: port,
    });
    await expect(adapter.observe(context)).resolves.toEqual({
      kind: "pending",
      reason: "field publication is not release-final",
    });
    await expect(
      adapter.preflight({ ...context, action: baseAction }),
    ).rejects.toThrow("cannot bypass authenticated field carriage");
    await expect(
      adapter.preflight({ ...context, action: publicationAction }),
    ).rejects.toThrow("differs from current requirement");
    expect(port.capture).not.toHaveBeenCalled();
    expect(underlying.preflight).not.toHaveBeenCalled();
    expect(underlying.submit).not.toHaveBeenCalled();
    vi.mocked(port.inspect).mockResolvedValue({ kind: "satisfied" });
    await expect(adapter.observe(context)).resolves.toEqual({
      kind: "action_required",
      action: baseAction,
    });
  });

  it("keeps an exact raw publication intent pending until its output is authenticated", async () => {
    const requirement = createRawDatumPreimageRequirement({
      preimage: Buffer.from("8101", "hex"),
    });
    const signer = {
      source: "test",
      address: "addr_test1_field_publication",
      paymentKeyHash: "12".repeat(28),
      selectWallet: () => undefined,
    };
    let candidates: UTxO[] = [];
    let outputConfirmed = false;
    let included = false;
    const port = createAuthenticatedFieldCarriagePrerequisitePort({
      category: "nonExistentInput",
      lucid: { utxosAt: async () => candidates } as unknown as LucidEvolution,
      network: "Preview",
      signer,
      publications: {
        observerVersion: FRAUD_PROOF_AUTHENTICATED_PUBLICATION_OBSERVER,
        observeExact: async ({ expectedOutRef }) =>
          outputConfirmed
            ? { kind: "confirmed", outRef: expectedOutRef }
            : { kind: "not_found" },
      },
      requirementForAction: () => requirement,
      transactionConfirmed: async () => included,
    });
    const required = await port.inspect({
      headerHash,
      baseAction,
      artifact: context.artifact,
      entries: [],
    });
    if (required.kind !== "required")
      throw new Error("missing publication requirement");
    const identitySha256 = required.action.input.requirementSha256;
    if (typeof identitySha256 !== "string")
      throw new Error("missing requirement identity");
    const datumCbor = requirement.publicationDatums[0]!;
    const input = {
      headerHash,
      action: required.action,
      artifact: context.artifact,
      txHash,
      durableRecovery: {
        fieldCarriage: {
          schemaVersion: FIELD_CARRIAGE_RECOVERY,
          kind: "publication",
          requirementSha256: identitySha256,
          outRef: `${txHash}#0`,
          datumCbor,
          unit: null,
        },
      },
    } as const;
    await expect(port.reconcile(input)).resolves.toEqual({
      kind: "pending",
      txHash,
    });
    candidates = [
      {
        txHash,
        outputIndex: 0,
        address: signer.address,
        assets: { lovelace: 2_000_000n },
        datum: datumCbor,
      },
    ];
    const wrapped = withRawDatumPreimagePrerequisite({
      category: "nonExistentInput",
      base: base(),
      prerequisite: port,
    });
    await expect(wrapped.observe(context)).resolves.toEqual({
      kind: "pending",
      reason: "nonExistentInput field publication 0 is not release-final",
    });
    await expect(
      wrapped.preflight({ ...context, action: baseAction }),
    ).rejects.toThrow("cannot bypass authenticated field carriage");
    included = true;
    await expect(port.reconcile(input)).resolves.toMatchObject({
      kind: "conflict",
      reason: expect.stringContaining("omitted its journaled output"),
    });
    outputConfirmed = true;
    await expect(port.reconcile(input)).resolves.toEqual({
      kind: "confirmed",
      txHash,
    });
    await expect(wrapped.observe(context)).resolves.toEqual({
      kind: "action_required",
      action: baseAction,
    });
    await expect(
      port.reconcile({
        ...input,
        durableRecovery: {
          fieldCarriage: {
            ...input.durableRecovery.fieldCarriage,
            datumCbor: "00",
          },
        },
      }),
    ).resolves.toMatchObject({ kind: "conflict" });
  });

  it("keeps nested raw and field publication namespaces distinct", async () => {
    const fieldPort = prerequisite();
    const rawPort = prerequisite({ phase: "satisfied" });
    const adapter = withRawDatumPreimagePrerequisite({
      category: "nonExistentInput",
      base: withFieldCarriagePrerequisite({
        category: "nonExistentInput",
        base: base(),
        prerequisite: fieldPort,
      }),
      prerequisite: rawPort,
    });
    await expect(adapter.observe(context)).resolves.toEqual({
      kind: "action_required",
      action: publicationAction,
    });
    const preflight = await adapter.preflight({
      ...context,
      action: publicationAction,
    });
    expect(fieldPort.capture).toHaveBeenCalledOnce();
    expect(rawPort.capture).not.toHaveBeenCalled();
    await expect(
      adapter.submit({ ...context, action: publicationAction, preflight }),
    ).resolves.toEqual({ kind: "submitted", txHash });
    const reconcile = vi.fn(async () => ({
      kind: "confirmed" as const,
      txHash,
    }));
    const restarted = withRawDatumPreimagePrerequisite({
      category: "nonExistentInput",
      base: withFieldCarriagePrerequisite({
        category: "nonExistentInput",
        base: base(),
        prerequisite: prerequisite({ reconcile }),
      }),
      prerequisite: prerequisite({ phase: "satisfied" }),
    });
    await expect(
      restarted.reconcile({
        ...context,
        action: publicationAction,
        txHash,
        durableRecovery: preflight.durableRecovery,
      }),
    ).resolves.toEqual({ kind: "confirmed", txHash });
    expect(reconcile).toHaveBeenCalledOnce();
  });

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
