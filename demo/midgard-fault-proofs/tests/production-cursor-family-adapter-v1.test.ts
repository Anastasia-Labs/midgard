import type { EvidenceProvenanceV1 } from "@al-ft/midgard-sdk";
import { describe, expect, it, vi } from "vitest";

import {
  FRAUD_PROOF_FAMILY_L1_OBSERVATION_PORT_V1,
  type FraudProofFamilyL1ObservationPortV1,
} from "../src/workflow/family-l1-observation-v1.js";
import type {
  FraudProofWorkflowIdentityV1,
  FraudProofWorkflowTerminalV1,
} from "../src/workflow/journal-v1.js";
import {
  createProductionCursorFamilyWorkflowAdapterV1,
  PRODUCTION_CURSOR_FAMILY_TRANSACTION_PORT_V1,
  type ProductionCursorFamilyTransactionPortV1,
} from "../src/workflow/production-cursor-family-adapter-v1.js";
import { MISSING_NATIVE_SCRIPT_TX_CURSOR_SPEC_V1 } from "../src/workflow/production-cursor-family-spec-v1.js";
import { productionCursorFamilyObservationV1 } from "../src/workflow/production-cursor-family-state-v1.js";
import type { FraudProofRawL1FamilyStageV1 } from "../src/workflow/raw-l1-family-derivation-v1.js";
import type { LocallyEvaluatedTransactionV1 } from "../src/workflow/transaction-boundary-v1.js";

const hash = (byte: string): string => byte.repeat(32);
const headerHash = "ab".repeat(28);
const outRef = (byte: string, index = 0): string => `${hash(byte)}#${index}`;
const txHash = hash("44");
const referenceOutRef = outRef("55");
const provenance: EvidenceProvenanceV1 = {
  trustClass: "authenticated_cardano_l1",
  sourceId: "local-kupmios/kupo+ogmios",
  grade: "security",
};

const identity: FraudProofWorkflowIdentityV1 = {
  schemaVersion: "midgard-fraud-proof-workflow-identity-v1",
  deploymentFingerprint: hash("aa"),
  category: "missingNativeScriptTx",
  target: { kind: "state_queue_header", headerHash },
};

const signed = ({
  bodyHash = txHash,
  submittedHash = txHash,
  includedReferenceOutRef = referenceOutRef,
  inlineScript = false,
  submit,
}: {
  readonly bodyHash?: string;
  readonly submittedHash?: string;
  readonly includedReferenceOutRef?: string;
  readonly inlineScript?: boolean;
  readonly submit?: () => Promise<string>;
} = {}): LocallyEvaluatedTransactionV1["signed"] => {
  const [referenceTxHash, referenceIndex] = includedReferenceOutRef.split("#");
  return {
    toHash: () => bodyHash,
    submit: submit ?? (async () => submittedHash),
    toTransaction: () => ({
      witness_set: () => ({
        native_scripts: () => undefined,
        plutus_v1_scripts: () => undefined,
        plutus_v2_scripts: () => undefined,
        plutus_v3_scripts: () => (inlineScript ? { len: () => 1 } : undefined),
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
  } as unknown as LocallyEvaluatedTransactionV1["signed"];
};

const transaction = (
  overrides: Partial<LocallyEvaluatedTransactionV1> = {},
): LocallyEvaluatedTransactionV1 => ({
  txHash,
  signed: signed(),
  referenceScripts: [
    {
      role: "V1 missing-native-script-tx step",
      outRef: referenceOutRef,
      scriptHash: "66".repeat(28),
    },
  ],
  ...overrides,
});

const l1 = (
  stageRef: { value: FraudProofRawL1FamilyStageV1 },
  confirmed = async (_txHash: string) => false,
): FraudProofFamilyL1ObservationPortV1<"missingNativeScriptTx"> => ({
  portVersion: FRAUD_PROOF_FAMILY_L1_OBSERVATION_PORT_V1,
  category: "missingNativeScriptTx",
  publications: {} as never,
  observeHeader: async () => {
    throw new Error("unused in focused cursor adapter test");
  },
  transactionConfirmed: async ({ txHash: requested }) =>
    await confirmed(requested),
  observe: async () => ({ provenance, stage: stageRef.value }),
});

const port = (
  capture: ProductionCursorFamilyTransactionPortV1<"missingNativeScriptTx">["capture"],
): ProductionCursorFamilyTransactionPortV1<"missingNativeScriptTx"> => ({
  portVersion: PRODUCTION_CURSOR_FAMILY_TRANSACTION_PORT_V1,
  category: "missingNativeScriptTx",
  prepare: async () => ({ prepared: true }),
  capture,
});

const noLeaseCoordinator = {
  acquire: async () => {
    throw new Error("no mutation lease expected");
  },
};

const context = {
  identity,
  workflowId: hash("bb"),
  artifact: { prepared: true },
  entries: [],
} as const;

const required = (stage: FraudProofRawL1FamilyStageV1) => {
  const observation = productionCursorFamilyObservationV1({
    spec: MISSING_NATIVE_SCRIPT_TX_CURSOR_SPEC_V1,
    headerHash,
    provenance,
    stage,
  });
  if (observation.kind !== "action_required") {
    throw new Error("fixture has no required action");
  }
  return observation.action;
};

const terminal = ({
  removalTxHash = txHash,
  removedOutRef = outRef("33"),
  proofOutRef = outRef("22"),
}: {
  readonly removalTxHash?: string;
  readonly removedOutRef?: string;
  readonly proofOutRef?: string;
} = {}): FraudProofWorkflowTerminalV1 => ({
  schemaVersion: "midgard-fraud-proof-workflow-terminal-v1",
  category: "missingNativeScriptTx",
  headerHash,
  proofToken: {
    unit: "11".repeat(28) + "22".repeat(28),
    outRef: proofOutRef,
    createdByTxHash: hash("22"),
    retainedAtFinalState: true,
  },
  correction: {
    removalTxHash,
    removedStateQueueOutRef: removedOutRef,
    fraudulentHeaderAbsent: true,
    referencedProofTokenOutRef: proofOutRef,
  },
  economics: {
    operatorCredential: "66".repeat(28),
    proverCredential: "77".repeat(28),
    operatorBondInputOutRef: outRef("88"),
    operatorBondInputLovelace: "900000000",
    slashedLovelace: "500000000",
    proverRewardOutputOutRef: outRef("99"),
    proverRewardLovelace: "100000000",
    removalFeeLovelace: "500000000",
    duplicateRewardAbsent: true,
  },
  observedAt: {
    slot: "1000",
    blockHash: hash("aa"),
    confirmationDepth: 30,
  },
});

describe("production cursor family adapter V1", () => {
  it("captures one exact locally evaluated reference-only body and refuses overwrite", async () => {
    const stage = {
      value: {
        kind: "step",
        step: 7,
        threadOutRef: outRef("11"),
        stateQueueBlockOutRef: outRef("10"),
      } as FraudProofRawL1FamilyStageV1,
    };
    const capture = vi.fn(async () => ({ transaction: transaction() }));
    const adapter = createProductionCursorFamilyWorkflowAdapterV1({
      spec: MISSING_NATIVE_SCRIPT_TX_CURSOR_SPEC_V1,
      l1: l1(stage),
      transactions: port(capture),
      stateQueueMutationLeaseCoordinator: noLeaseCoordinator,
    });
    const action = required(stage.value);
    const preflight = await adapter.preflight({ ...context, action });
    expect(preflight).toMatchObject({
      actionId: action.actionId,
      txHash,
      scriptExecution: "reference_scripts",
      localUplcEvaluation: { status: "passed" },
    });
    await expect(adapter.preflight({ ...context, action })).rejects.toThrow(
      "already has an outstanding captured body",
    );
    await expect(
      adapter.submit({ ...context, action, preflight }),
    ).resolves.toEqual({ kind: "submitted", txHash });
    expect(capture).toHaveBeenCalledTimes(1);
  });

  it("rejects stale actions, artifact substitution, body-hash drift, and inline scripts", async () => {
    const stage = {
      value: {
        kind: "step",
        step: 6,
        threadOutRef: outRef("11"),
        stateQueueBlockOutRef: outRef("10"),
      } as FraudProofRawL1FamilyStageV1,
    };
    const action = required(stage.value);
    const capture = vi.fn(async ({ artifact }) => {
      if (artifact.prepared !== true || Object.keys(artifact).length !== 1) {
        throw new Error("family artifact changed after preparation");
      }
      return { transaction: transaction() };
    });
    const adapter = createProductionCursorFamilyWorkflowAdapterV1({
      spec: MISSING_NATIVE_SCRIPT_TX_CURSOR_SPEC_V1,
      l1: l1(stage),
      transactions: port(capture),
      stateQueueMutationLeaseCoordinator: noLeaseCoordinator,
    });
    await expect(
      adapter.preflight({
        ...context,
        action: {
          ...action,
          input: { ...action.input, threadOutRef: outRef("12") },
        },
      }),
    ).rejects.toThrow("differs from authenticated current L1 state");
    await expect(
      adapter.preflight({
        ...context,
        artifact: { prepared: true, injected: "operator-private" },
        action,
      }),
    ).rejects.toThrow("family artifact changed after preparation");

    for (const hostile of [
      transaction({ signed: signed({ bodyHash: hash("99") }) }),
      transaction({ signed: signed({ inlineScript: true }) }),
      transaction({ referenceScripts: [] }),
      transaction({
        signed: signed({ includedReferenceOutRef: outRef("56") }),
      }),
    ]) {
      const hostileAdapter = createProductionCursorFamilyWorkflowAdapterV1({
        spec: MISSING_NATIVE_SCRIPT_TX_CURSOR_SPEC_V1,
        l1: l1(stage),
        transactions: port(async () => ({ transaction: hostile })),
        stateQueueMutationLeaseCoordinator: noLeaseCoordinator,
      });
      await expect(
        hostileAdapter.preflight({ ...context, action }),
      ).rejects.toThrow();
    }
  });

  it("reconciles an ambiguous submitted body after a fresh-process restart", async () => {
    const stage = {
      value: {
        kind: "step",
        step: 7,
        threadOutRef: outRef("11"),
        stateQueueBlockOutRef: outRef("10"),
      } as FraudProofRawL1FamilyStageV1,
    };
    const action = required(stage.value);
    const first = createProductionCursorFamilyWorkflowAdapterV1({
      spec: MISSING_NATIVE_SCRIPT_TX_CURSOR_SPEC_V1,
      l1: l1(stage),
      transactions: port(async () => ({
        transaction: transaction({
          signed: signed({
            submit: async () => {
              stage.value = {
                kind: "step",
                step: 7,
                threadOutRef: `${txHash}#0`,
                stateQueueBlockOutRef: outRef("10"),
              };
              throw new Error("connection closed after submission");
            },
          }),
        }),
      })),
      stateQueueMutationLeaseCoordinator: noLeaseCoordinator,
    });
    const preflight = await first.preflight({ ...context, action });
    await expect(
      first.submit({ ...context, action, preflight }),
    ).rejects.toThrow("connection closed after submission");

    const fresh = createProductionCursorFamilyWorkflowAdapterV1({
      spec: MISSING_NATIVE_SCRIPT_TX_CURSOR_SPEC_V1,
      l1: l1(stage, async (candidate) => candidate === txHash),
      transactions: port(async () => ({ transaction: transaction() })),
      stateQueueMutationLeaseCoordinator: noLeaseCoordinator,
    });
    await expect(
      fresh.reconcile({ ...context, action, txHash }),
    ).resolves.toEqual({ kind: "confirmed", txHash });
  });

  it("resumes, renews, releases, and fails the durable descendant-removal lease", async () => {
    const proofOutRef = outRef("22");
    const removalOutRef = outRef("33");
    const stage = {
      value: {
        kind: "proof_token",
        fraudProofOutRef: proofOutRef,
        stateQueueBlockOutRef: outRef("10"),
        nextRemovalOutRef: removalOutRef,
      } as FraudProofRawL1FamilyStageV1,
    };
    const action = required(stage.value);
    const lease = () => ({
      token: "cursor-removal-lease",
      source: "state-queue-observer-v1",
      renew: vi.fn(async () => undefined),
      release: vi.fn(async () => undefined),
      fail: vi.fn(async () => undefined),
    });
    const acquired = lease();
    const first = createProductionCursorFamilyWorkflowAdapterV1({
      spec: MISSING_NATIVE_SCRIPT_TX_CURSOR_SPEC_V1,
      l1: l1(stage),
      transactions: port(async () => ({
        transaction: transaction(),
        mutationLease: acquired,
      })),
      stateQueueMutationLeaseCoordinator: { acquire: async () => acquired },
    });
    const preflight = await first.preflight({ ...context, action });
    expect(preflight.durableRecovery).toEqual({
      stateQueueMutationLease: {
        token: acquired.token,
        source: acquired.source,
      },
    });

    const pendingLease = lease();
    const pending = createProductionCursorFamilyWorkflowAdapterV1({
      spec: MISSING_NATIVE_SCRIPT_TX_CURSOR_SPEC_V1,
      l1: l1(stage, async () => true),
      transactions: port(async () => ({ transaction: transaction() })),
      stateQueueMutationLeaseCoordinator: {
        acquire: async () => pendingLease,
        resume: async () => pendingLease,
      },
    });
    await expect(
      pending.reconcile({
        ...context,
        action,
        txHash,
        durableRecovery: preflight.durableRecovery,
      }),
    ).resolves.toEqual({ kind: "pending", txHash });
    expect(pendingLease.renew).toHaveBeenCalledTimes(1);

    const releasedLease = lease();
    stage.value = { kind: "removed", terminal: terminal() };
    const confirmed = createProductionCursorFamilyWorkflowAdapterV1({
      spec: MISSING_NATIVE_SCRIPT_TX_CURSOR_SPEC_V1,
      l1: l1(stage, async () => true),
      transactions: port(async () => ({ transaction: transaction() })),
      stateQueueMutationLeaseCoordinator: {
        acquire: async () => releasedLease,
        resume: async () => releasedLease,
      },
    });
    await expect(
      confirmed.reconcile({
        ...context,
        action,
        txHash,
        durableRecovery: preflight.durableRecovery,
      }),
    ).resolves.toEqual({ kind: "confirmed", txHash });
    expect(releasedLease.release).toHaveBeenCalledTimes(1);

    const failedLease = lease();
    stage.value = {
      kind: "removed",
      terminal: terminal({ removedOutRef: outRef("34") }),
    };
    const conflicted = createProductionCursorFamilyWorkflowAdapterV1({
      spec: MISSING_NATIVE_SCRIPT_TX_CURSOR_SPEC_V1,
      l1: l1(stage, async () => true),
      transactions: port(async () => ({ transaction: transaction() })),
      stateQueueMutationLeaseCoordinator: {
        acquire: async () => failedLease,
        resume: async () => failedLease,
      },
    });
    await expect(
      conflicted.reconcile({
        ...context,
        action,
        txHash,
        durableRecovery: preflight.durableRecovery,
      }),
    ).resolves.toMatchObject({ kind: "conflict" });
    expect(failedLease.fail).toHaveBeenCalledTimes(1);
  });

  it("fails an acquired lease if signed-body admission fails before durable intent", async () => {
    const stage = {
      value: {
        kind: "proof_token",
        fraudProofOutRef: outRef("22"),
        stateQueueBlockOutRef: outRef("10"),
        nextRemovalOutRef: outRef("33"),
      } as FraudProofRawL1FamilyStageV1,
    };
    const action = required(stage.value);
    const lease = {
      token: "preflight-failure-lease",
      source: "state-queue-observer-v1",
      renew: vi.fn(async () => undefined),
      release: vi.fn(async () => undefined),
      fail: vi.fn(async () => undefined),
    };
    const adapter = createProductionCursorFamilyWorkflowAdapterV1({
      spec: MISSING_NATIVE_SCRIPT_TX_CURSOR_SPEC_V1,
      l1: l1(stage),
      transactions: port(async () => ({
        transaction: transaction({ signed: signed({ inlineScript: true }) }),
        mutationLease: lease,
      })),
      stateQueueMutationLeaseCoordinator: { acquire: async () => lease },
    });
    await expect(adapter.preflight({ ...context, action })).rejects.toThrow(
      "embeds inline script witnesses",
    );
    expect(lease.fail).toHaveBeenCalledTimes(1);
    expect(lease.release).not.toHaveBeenCalled();
  });
});
