import type { EvidenceProvenance } from "@al-ft/midgard-sdk";
import { describe, expect, it, vi } from "vitest";

import {
  FRAUD_PROOF_FAMILY_L1_OBSERVATION_PORT,
  type FraudProofFamilyL1ObservationPort,
} from "../src/workflow/family-l1-observation.js";
import type { FraudProofWorkflowIdentity } from "../src/workflow/journal.js";
import {
  createLinearFamilyWorkflowAdapter,
  LINEAR_FAMILY_TRANSACTION_PORT,
  type LinearFamilyTransactionPort,
} from "../src/workflow/linear-family-adapter.js";
import { linearFamilyObservation } from "../src/workflow/linear-family-state.js";
import type { FraudProofRawL1FamilyStage } from "../src/workflow/raw-l1-family-derivation.js";
import type { LocallyEvaluatedTransaction } from "../src/workflow/transaction-boundary.js";

const hash = (byte: string): string => byte.repeat(32);
const headerHash = "ab".repeat(28);
const outRef = (byte: string, index = 0): string => `${hash(byte)}#${index}`;
const txHash = hash("44");
const referenceOutRef = outRef("55");
const provenance: EvidenceProvenance = {
  trustClass: "authenticated_cardano_l1",
  sourceId: "local-kupmios/kupo+ogmios",
  grade: "security",
};

const identity: FraudProofWorkflowIdentity = {
  schemaVersion: "midgard-fraud-proof-workflow-identity-v1",
  deploymentFingerprint: hash("aa"),
  category: "daHashPreimage",
  target: { kind: "state_queue_header", headerHash },
};

const signed = ({
  submittedHash = txHash,
  includedReferenceOutRef = referenceOutRef,
  inlineScriptKind,
}: {
  readonly submittedHash?: string;
  readonly includedReferenceOutRef?: string;
  readonly inlineScriptKind?: "native" | "plutusV1" | "plutusV2" | "plutusV3";
} = {}): LocallyEvaluatedTransaction["signed"] => {
  const [referenceTxHash, referenceIndex] = includedReferenceOutRef.split("#");
  return {
    toHash: () => txHash,
    submit: async () => submittedHash,
    toTransaction: () => ({
      witness_set: () => ({
        native_scripts: () =>
          inlineScriptKind === "native" ? { len: () => 1 } : undefined,
        plutus_v1_scripts: () =>
          inlineScriptKind === "plutusV1" ? { len: () => 1 } : undefined,
        plutus_v2_scripts: () =>
          inlineScriptKind === "plutusV2" ? { len: () => 1 } : undefined,
        plutus_v3_scripts: () =>
          inlineScriptKind === "plutusV3" ? { len: () => 1 } : undefined,
      }),
      body: () => ({
        reference_inputs: () => ({
          len: () => 1,
          get: () => ({
            transaction_id: () => ({ to_hex: () => referenceTxHash! }),
            index: () => BigInt(referenceIndex!),
          }),
        }),
      }),
    }),
  } as unknown as LocallyEvaluatedTransaction["signed"];
};

const transaction = (
  overrides: Partial<LocallyEvaluatedTransaction> = {},
): LocallyEvaluatedTransaction => ({
  txHash,
  signed: signed(),
  referenceScripts: [
    {
      role: "V1 fraud-proof da-hash-preimage step-01",
      outRef: referenceOutRef,
      scriptHash: "66".repeat(28),
    },
  ],
  ...overrides,
});

const l1 = (
  stageRef: { value: FraudProofRawL1FamilyStage },
  confirmed = async (_txHash: string) => false,
): FraudProofFamilyL1ObservationPort<"daHashPreimage"> => ({
  portVersion: FRAUD_PROOF_FAMILY_L1_OBSERVATION_PORT,
  category: "daHashPreimage",
  publications: {} as never,
  observeHeader: async () => {
    throw new Error("unused in focused adapter test");
  },
  transactionConfirmed: async ({ txHash: requested }) =>
    await confirmed(requested),
  observe: async () => ({ provenance, stage: stageRef.value }),
});

const port = (
  capture: LinearFamilyTransactionPort<"daHashPreimage">["capture"],
): LinearFamilyTransactionPort<"daHashPreimage"> => ({
  portVersion: LINEAR_FAMILY_TRANSACTION_PORT,
  category: "daHashPreimage",
  prepare: async () => ({ prepared: true }),
  capture,
});

const leaseCoordinator = {
  acquire: async () => {
    throw new Error("no mutation lease expected in this focused step test");
  },
};

const context = {
  identity,
  workflowId: hash("bb"),
  artifact: { prepared: true },
  entries: [],
} as const;

describe("production linear family adapter V1", () => {
  it("captures local UPLC intent, binds signed reference inputs, and submits the exact body", async () => {
    const stage = {
      value: {
        kind: "step",
        step: 1,
        threadOutRef: outRef("11"),
        stateQueueBlockOutRef: outRef("10"),
      } as const,
    };
    const capture = vi.fn(async () => ({ transaction: transaction() }));
    const adapter = createLinearFamilyWorkflowAdapter({
      category: "daHashPreimage",
      l1: l1(stage),
      transactions: port(capture),
      stateQueueMutationLeaseCoordinator: leaseCoordinator,
    });
    const observation = await adapter.observe(context);
    if (observation.kind !== "action_required") {
      throw new Error("expected action");
    }
    const preflight = await adapter.preflight({
      ...context,
      action: observation.action,
    });
    expect(preflight).toMatchObject({
      actionId: observation.action.actionId,
      txHash,
      scriptExecution: "reference_scripts",
      localUplcEvaluation: { status: "passed" },
    });
    await expect(
      adapter.submit({
        ...context,
        action: observation.action,
        preflight,
      }),
    ).resolves.toEqual({ kind: "submitted", txHash });
    expect(capture).toHaveBeenCalledTimes(1);
  });

  it("rejects stale/substituted actions before invoking a builder", async () => {
    const stage = {
      value: {
        kind: "step",
        step: 1,
        threadOutRef: outRef("11"),
        stateQueueBlockOutRef: outRef("10"),
      } as const,
    };
    const capture = vi.fn(async () => ({ transaction: transaction() }));
    const adapter = createLinearFamilyWorkflowAdapter({
      category: "daHashPreimage",
      l1: l1(stage),
      transactions: port(capture),
      stateQueueMutationLeaseCoordinator: leaseCoordinator,
    });
    const required = linearFamilyObservation({
      category: "daHashPreimage",
      headerHash,
      provenance,
      stage: stage.value,
    });
    if (required.kind !== "action_required") throw new Error("missing action");
    await expect(
      adapter.preflight({
        ...context,
        action: {
          ...required.action,
          input: { ...required.action.input, threadOutRef: outRef("12") },
        },
      }),
    ).rejects.toThrow("differs from authenticated current L1 state");
    expect(capture).not.toHaveBeenCalled();
  });

  it("rejects inline-only and forged reference-script claims", async () => {
    const stage = {
      value: {
        kind: "step",
        step: 1,
        threadOutRef: outRef("11"),
        stateQueueBlockOutRef: outRef("10"),
      } as const,
    };
    const required = linearFamilyObservation({
      category: "daHashPreimage",
      headerHash,
      provenance,
      stage: stage.value,
    });
    if (required.kind !== "action_required") throw new Error("missing action");

    const inline = createLinearFamilyWorkflowAdapter({
      category: "daHashPreimage",
      l1: l1(stage),
      transactions: port(async () => ({
        transaction: transaction({ referenceScripts: [] }),
      })),
      stateQueueMutationLeaseCoordinator: leaseCoordinator,
    });
    await expect(
      inline.preflight({ ...context, action: required.action }),
    ).rejects.toThrow("did not use published reference scripts");

    const forged = createLinearFamilyWorkflowAdapter({
      category: "daHashPreimage",
      l1: l1(stage),
      transactions: port(async () => ({
        transaction: transaction({
          signed: signed({ includedReferenceOutRef: outRef("56") }),
        }),
      })),
      stateQueueMutationLeaseCoordinator: leaseCoordinator,
    });
    await expect(
      forged.preflight({ ...context, action: required.action }),
    ).rejects.toThrow("claimed a reference script absent from the signed body");

    for (const inlineScriptKind of [
      "native",
      "plutusV1",
      "plutusV2",
      "plutusV3",
    ] as const) {
      const embedded = createLinearFamilyWorkflowAdapter({
        category: "daHashPreimage",
        l1: l1(stage),
        transactions: port(async () => ({
          transaction: transaction({ signed: signed({ inlineScriptKind }) }),
        })),
        stateQueueMutationLeaseCoordinator: leaseCoordinator,
      });
      await expect(
        embedded.preflight({ ...context, action: required.action }),
      ).rejects.toThrow("embeds inline script witnesses");
    }
  });

  it("rejects duplicate outstanding preflight capture without replacing it", async () => {
    const stage = {
      value: {
        kind: "step",
        step: 1,
        threadOutRef: outRef("11"),
        stateQueueBlockOutRef: outRef("10"),
      } as const,
    };
    const required = linearFamilyObservation({
      category: "daHashPreimage",
      headerHash,
      provenance,
      stage: stage.value,
    });
    if (required.kind !== "action_required") throw new Error("missing action");
    const capture = vi.fn(async () => ({ transaction: transaction() }));
    const adapter = createLinearFamilyWorkflowAdapter({
      category: "daHashPreimage",
      l1: l1(stage),
      transactions: port(capture),
      stateQueueMutationLeaseCoordinator: leaseCoordinator,
    });
    await adapter.preflight({ ...context, action: required.action });
    await expect(
      adapter.preflight({ ...context, action: required.action }),
    ).rejects.toThrow("already has an outstanding captured body");
    expect(capture).toHaveBeenCalledTimes(1);
  });

  it("fails an acquired mutation lease when signed-body admission rejects", async () => {
    const stage = {
      value: {
        kind: "proof_token",
        fraudProofOutRef: outRef("22"),
        stateQueueBlockOutRef: outRef("11"),
        nextRemovalOutRef: outRef("33"),
      } as FraudProofRawL1FamilyStage,
    };
    const required = linearFamilyObservation({
      category: "daHashPreimage",
      headerHash,
      provenance,
      stage: stage.value,
    });
    if (required.kind !== "action_required") throw new Error("missing action");
    const lease = {
      token: "lease-before-admission",
      source: "state-queue-observer-v1",
      renew: vi.fn(async () => undefined),
      release: vi.fn(async () => undefined),
      fail: vi.fn(async () => undefined),
    };
    const adapter = createLinearFamilyWorkflowAdapter({
      category: "daHashPreimage",
      l1: l1(stage),
      transactions: port(async () => ({
        transaction: transaction({
          signed: signed({ inlineScriptKind: "plutusV3" }),
        }),
        mutationLease: lease,
      })),
      stateQueueMutationLeaseCoordinator: { acquire: async () => lease },
    });
    await expect(
      adapter.preflight({ ...context, action: required.action }),
    ).rejects.toThrow("embeds inline script witnesses");
    expect(lease.fail).toHaveBeenCalledTimes(1);
    expect(lease.release).not.toHaveBeenCalled();
  });

  it("reconciles through a fresh adapter without an in-memory captured body", async () => {
    const stage = {
      value: {
        kind: "step",
        step: 1,
        threadOutRef: outRef("11"),
        stateQueueBlockOutRef: outRef("10"),
      } as FraudProofRawL1FamilyStage,
    };
    const required = linearFamilyObservation({
      category: "daHashPreimage",
      headerHash,
      provenance,
      stage: stage.value,
    });
    if (required.kind !== "action_required") throw new Error("missing action");
    const fresh = createLinearFamilyWorkflowAdapter({
      category: "daHashPreimage",
      l1: l1(stage, async (requested) => requested === txHash),
      transactions: port(async () => ({ transaction: transaction() })),
      stateQueueMutationLeaseCoordinator: leaseCoordinator,
    });
    stage.value = {
      kind: "step",
      step: 2,
      threadOutRef: `${txHash}#0`,
      stateQueueBlockOutRef: outRef("10"),
    };
    await expect(
      fresh.reconcile({
        ...context,
        action: required.action,
        txHash,
      }),
    ).resolves.toEqual({ kind: "confirmed", txHash });
  });

  it("persists and resumes the exact mutation lease for descendant removal", async () => {
    const stage = {
      value: {
        kind: "proof_token",
        fraudProofOutRef: outRef("22"),
        stateQueueBlockOutRef: outRef("11"),
        nextRemovalOutRef: outRef("33"),
      } as FraudProofRawL1FamilyStage,
    };
    const required = linearFamilyObservation({
      category: "daHashPreimage",
      headerHash,
      provenance,
      stage: stage.value,
    });
    if (required.kind !== "action_required") throw new Error("missing action");
    const lease = {
      token: "lease-token-1",
      source: "state-queue-observer-v1",
      renew: vi.fn(async () => undefined),
      release: vi.fn(async () => undefined),
      fail: vi.fn(async () => undefined),
    };
    const adapter = createLinearFamilyWorkflowAdapter({
      category: "daHashPreimage",
      l1: l1(stage),
      transactions: port(async () => ({
        transaction: transaction(),
        mutationLease: lease,
      })),
      stateQueueMutationLeaseCoordinator: {
        acquire: async () => lease,
      },
    });
    const preflight = await adapter.preflight({
      ...context,
      action: required.action,
    });
    expect(preflight.durableRecovery).toEqual({
      stateQueueMutationLease: {
        token: lease.token,
        source: lease.source,
      },
    });

    const resumedLease = {
      ...lease,
      renew: vi.fn(async () => undefined),
    };
    const resume = vi.fn(async () => resumedLease);
    const fresh = createLinearFamilyWorkflowAdapter({
      category: "daHashPreimage",
      l1: l1(stage),
      transactions: port(async () => ({ transaction: transaction() })),
      stateQueueMutationLeaseCoordinator: {
        acquire: async () => resumedLease,
        resume,
      },
    });
    await expect(
      fresh.reconcile({
        ...context,
        action: required.action,
        txHash,
        durableRecovery: preflight.durableRecovery,
      }),
    ).resolves.toEqual({ kind: "not_found" });
    expect(resume).toHaveBeenCalledWith({
      token: lease.token,
      source: lease.source,
    });
    expect(resumedLease.renew).toHaveBeenCalledTimes(1);
  });

  it("rejects missing, surplus, or substituted mutation-lease authority", async () => {
    const stage = {
      value: {
        kind: "proof_token",
        fraudProofOutRef: outRef("22"),
        stateQueueBlockOutRef: outRef("11"),
        nextRemovalOutRef: outRef("33"),
      } as FraudProofRawL1FamilyStage,
    };
    const required = linearFamilyObservation({
      category: "daHashPreimage",
      headerHash,
      provenance,
      stage: stage.value,
    });
    if (required.kind !== "action_required") throw new Error("missing action");
    const missing = createLinearFamilyWorkflowAdapter({
      category: "daHashPreimage",
      l1: l1(stage),
      transactions: port(async () => ({ transaction: transaction() })),
      stateQueueMutationLeaseCoordinator: leaseCoordinator,
    });
    await expect(
      missing.preflight({ ...context, action: required.action }),
    ).rejects.toThrow("topology disagreed with its mutation lease");

    await expect(
      missing.reconcile({
        ...context,
        action: required.action,
        txHash,
        durableRecovery: {
          stateQueueMutationLease: {
            token: "substituted",
            source: "foreign-observer",
          },
        },
      }),
    ).resolves.toMatchObject({ kind: "conflict" });
  });

  it("rejects category-substituted L1 or transaction ports", () => {
    const stage = {
      value: {
        kind: "not_started",
        stateQueueBlockOutRef: outRef("10"),
      } as const,
    };
    expect(() =>
      createLinearFamilyWorkflowAdapter({
        category: "daHashPreimage",
        l1: { ...l1(stage), category: "minFee" } as never,
        transactions: port(async () => ({ transaction: transaction() })),
        stateQueueMutationLeaseCoordinator: leaseCoordinator,
      }),
    ).toThrow("ports changed identity");
    expect(() =>
      createLinearFamilyWorkflowAdapter({
        category: "daHashPreimage",
        l1: l1(stage),
        transactions: {
          ...port(async () => ({ transaction: transaction() })),
          category: "minFee",
        } as never,
        stateQueueMutationLeaseCoordinator: leaseCoordinator,
      }),
    ).toThrow("ports changed identity");
  });
});
