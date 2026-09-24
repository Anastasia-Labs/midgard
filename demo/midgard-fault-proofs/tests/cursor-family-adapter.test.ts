import type { EvidenceProvenance } from "@al-ft/midgard-sdk";
import { describe, expect, it, vi } from "vitest";

import { TRANSITION_TRACE_CURSOR_SPEC } from "../src/transition-trace/workflow-spec.js";
import { WorkflowActionChangedError } from "../src/workflow/action-changed.js";
import {
  createCursorFamilyWorkflowAdapter,
  CURSOR_FAMILY_TRANSACTION_PORT,
  type CursorFamilyTransactionPort,
} from "../src/workflow/cursor-family-adapter.js";
import { MISSING_NATIVE_SCRIPT_TX_CURSOR_SPEC } from "../src/workflow/cursor-family-spec.js";
import { cursorFamilyObservation } from "../src/workflow/cursor-family-state.js";
import {
  FRAUD_PROOF_FAMILY_L1_OBSERVATION_PORT,
  type FraudProofFamilyL1ObservationPort,
} from "../src/workflow/family-l1-observation.js";
import type {
  FraudProofWorkflowIdentity,
  FraudProofWorkflowTerminal,
} from "../src/workflow/journal.js";
import {
  type JournalJsonObject,
  normalizeJournalJson,
} from "../src/workflow/journal.js";
import type { FraudProofRawL1FamilyStage } from "../src/workflow/raw-l1-family-derivation.js";
import {
  type LocallyEvaluatedTransaction,
  workflowPreflightTransaction,
} from "../src/workflow/transaction-boundary.js";

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
} = {}): LocallyEvaluatedTransaction["signed"] => {
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
  } as unknown as LocallyEvaluatedTransaction["signed"];
};

const transaction = (
  overrides: Partial<LocallyEvaluatedTransaction> = {},
): LocallyEvaluatedTransaction => ({
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
  stageRef: { value: FraudProofRawL1FamilyStage },
  confirmed = async (_txHash: string) => false,
): FraudProofFamilyL1ObservationPort<"missingNativeScriptTx"> => ({
  portVersion: FRAUD_PROOF_FAMILY_L1_OBSERVATION_PORT,
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
  capture: CursorFamilyTransactionPort<"missingNativeScriptTx">["capture"],
): CursorFamilyTransactionPort<"missingNativeScriptTx"> => ({
  portVersion: CURSOR_FAMILY_TRANSACTION_PORT,
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

const required = (stage: FraudProofRawL1FamilyStage) => {
  const observation = cursorFamilyObservation({
    spec: MISSING_NATIVE_SCRIPT_TX_CURSOR_SPEC,
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
} = {}): FraudProofWorkflowTerminal => ({
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
  it("distinguishes a changed authenticated action from a deterministic build failure", async () => {
    const stage: { value: FraudProofRawL1FamilyStage } = {
      value: { kind: "not_started", stateQueueBlockOutRef: outRef("10") },
    };
    const failure = new Error("local UPLC validator rejected the transaction");
    const capture = vi.fn(async () => {
      throw failure;
    });
    const adapter = createCursorFamilyWorkflowAdapter({
      spec: MISSING_NATIVE_SCRIPT_TX_CURSOR_SPEC,
      l1: l1(stage),
      transactions: port(capture),
      stateQueueMutationLeaseCoordinator: noLeaseCoordinator,
    });
    const original = await adapter.observe(context);
    if (original.kind !== "action_required") throw new Error("missing action");
    stage.value = { kind: "not_started", stateQueueBlockOutRef: outRef("12") };
    await expect(
      adapter.preflight({ ...context, action: original.action }),
    ).rejects.toBeInstanceOf(WorkflowActionChangedError);
    expect(capture).not.toHaveBeenCalled();
    const current = await adapter.observe(context);
    if (current.kind !== "action_required")
      throw new Error("missing replacement action");
    await expect(
      adapter.preflight({ ...context, action: current.action }),
    ).rejects.toBe(failure);
    expect(capture).toHaveBeenCalledExactlyOnceWith({
      action: current.action,
      artifact: context.artifact,
    });
  });

  it("admits canonically ordered journal actions and rejects a changed field", async () => {
    const stage = {
      value: {
        kind: "not_started",
        stateQueueBlockOutRef: outRef("10"),
      } as FraudProofRawL1FamilyStage,
    };
    const capture = vi.fn(async () => ({ transaction: transaction() }));
    const adapter = createCursorFamilyWorkflowAdapter({
      spec: MISSING_NATIVE_SCRIPT_TX_CURSOR_SPEC,
      l1: l1(stage),
      transactions: port(capture),
      stateQueueMutationLeaseCoordinator: noLeaseCoordinator,
    });
    const original = required(stage.value);
    const action = {
      ...original,
      input: normalizeJournalJson(original.input) as JournalJsonObject,
    };
    expect(JSON.stringify(action)).not.toBe(JSON.stringify(original));
    await expect(
      adapter.preflight({
        ...context,
        action: {
          ...action,
          input: { ...action.input, stateQueueBlockOutRef: outRef("99") },
        },
      }),
    ).rejects.toThrow("differs from authenticated current L1 state");
    await expect(
      adapter.preflight({ ...context, action }),
    ).resolves.toMatchObject({ actionId: action.actionId });
    expect(capture).toHaveBeenCalledOnce();
  });

  it("captures one exact locally evaluated reference-only body and refuses overwrite", async () => {
    const stage = {
      value: {
        kind: "step",
        step: 7,
        threadOutRef: outRef("11"),
        stateQueueBlockOutRef: outRef("10"),
      } as FraudProofRawL1FamilyStage,
    };
    const capture = vi.fn(async () => ({ transaction: transaction() }));
    const adapter = createCursorFamilyWorkflowAdapter({
      spec: MISSING_NATIVE_SCRIPT_TX_CURSOR_SPEC,
      l1: l1(stage),
      transactions: port(capture),
      stateQueueMutationLeaseCoordinator: noLeaseCoordinator,
    });
    const action = required(stage.value);
    const preflight = await adapter.preflight({ ...context, action });
    expect(workflowPreflightTransaction(preflight)).toBe(
      (await capture.mock.results[0]!.value).transaction.signed,
    );
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
      } as FraudProofRawL1FamilyStage,
    };
    const action = required(stage.value);
    const capture = vi.fn(async ({ artifact }) => {
      if (artifact.prepared !== true || Object.keys(artifact).length !== 1) {
        throw new Error("family artifact changed after preparation");
      }
      return { transaction: transaction() };
    });
    const adapter = createCursorFamilyWorkflowAdapter({
      spec: MISSING_NATIVE_SCRIPT_TX_CURSOR_SPEC,
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
      const hostileAdapter = createCursorFamilyWorkflowAdapter({
        spec: MISSING_NATIVE_SCRIPT_TX_CURSOR_SPEC,
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
      } as FraudProofRawL1FamilyStage,
    };
    const action = required(stage.value);
    const first = createCursorFamilyWorkflowAdapter({
      spec: MISSING_NATIVE_SCRIPT_TX_CURSOR_SPEC,
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

    const fresh = createCursorFamilyWorkflowAdapter({
      spec: MISSING_NATIVE_SCRIPT_TX_CURSOR_SPEC,
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
      } as FraudProofRawL1FamilyStage,
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
    const capturedRemoval = transaction();
    const first = createCursorFamilyWorkflowAdapter({
      spec: MISSING_NATIVE_SCRIPT_TX_CURSOR_SPEC,
      l1: l1(stage),
      transactions: port(async () => ({
        transaction: capturedRemoval,
        mutationLease: acquired,
      })),
      stateQueueMutationLeaseCoordinator: { acquire: async () => acquired },
    });
    const preflight = await first.preflight({ ...context, action });
    expect(workflowPreflightTransaction(preflight)).toBe(
      capturedRemoval.signed,
    );
    expect(preflight.durableRecovery).toEqual({
      stateQueueMutationLease: {
        token: acquired.token,
        source: acquired.source,
      },
    });

    const pendingLease = lease();
    const pending = createCursorFamilyWorkflowAdapter({
      spec: MISSING_NATIVE_SCRIPT_TX_CURSOR_SPEC,
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
    const confirmed = createCursorFamilyWorkflowAdapter({
      spec: MISSING_NATIVE_SCRIPT_TX_CURSOR_SPEC,
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

    const expiredResume = vi.fn(async () => {
      throw new Error("old mutation lease expired after confirmed removal");
    });
    const expired = createCursorFamilyWorkflowAdapter({
      spec: MISSING_NATIVE_SCRIPT_TX_CURSOR_SPEC,
      l1: l1(stage, async () => true),
      transactions: port(async () => {
        throw new Error("must not rebuild");
      }),
      stateQueueMutationLeaseCoordinator: {
        acquire: async () => {
          throw new Error("must not acquire");
        },
        resume: expiredResume,
      },
    });
    await expect(
      expired.reconcile({
        ...context,
        action: action,
        txHash,
        durableRecovery: preflight.durableRecovery,
        authorizeResubmission: async () => {
          throw new Error("must not submit");
        },
      }),
    ).resolves.toEqual({ kind: "confirmed", txHash });
    expect(expiredResume).toHaveBeenCalledTimes(1);

    const failedLease = lease();
    stage.value = {
      kind: "removed",
      terminal: terminal({ removedOutRef: outRef("34") }),
    };
    const conflicted = createCursorFamilyWorkflowAdapter({
      spec: MISSING_NATIVE_SCRIPT_TX_CURSOR_SPEC,
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
      } as FraudProofRawL1FamilyStage,
    };
    const action = required(stage.value);
    const lease = {
      token: "preflight-failure-lease",
      source: "state-queue-observer-v1",
      renew: vi.fn(async () => undefined),
      release: vi.fn(async () => undefined),
      fail: vi.fn(async () => undefined),
    };
    const adapter = createCursorFamilyWorkflowAdapter({
      spec: MISSING_NATIVE_SCRIPT_TX_CURSOR_SPEC,
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

describe("authenticated family action refinement", () => {
  const stage = {
    value: {
      kind: "step",
      step: 6,
      threadOutRef: outRef("11"),
      stateQueueBlockOutRef: outRef("10"),
    } as FraudProofRawL1FamilyStage,
  };
  it("binds extra grammar fields and rejects changes before capture", async () => {
    let grammar = "start";
    const capture = vi.fn(async () => ({ transaction: transaction() }));
    const refineAction = vi.fn(async () => ({ grammar }));
    const adapter = createCursorFamilyWorkflowAdapter({
      spec: MISSING_NATIVE_SCRIPT_TX_CURSOR_SPEC,
      l1: l1(stage),
      transactions: port(capture),
      stateQueueMutationLeaseCoordinator: noLeaseCoordinator,
      refineAction,
    });
    const observed = await adapter.observe(context);
    if (observed.kind !== "action_required") throw new Error("missing action");
    expect(observed.action.actionId).toBe(required(stage.value).actionId);
    expect(observed.action.input.grammar).toBe("start");
    grammar = "finish";
    await expect(
      adapter.preflight({ ...context, action: observed.action }),
    ).rejects.toThrow("differs from authenticated current L1 state");
    expect(capture).not.toHaveBeenCalled();
    grammar = "start";
    await adapter.preflight({ ...context, action: observed.action });
    expect(capture).toHaveBeenCalledOnce();
    expect(capture.mock.calls[0]).toBeDefined();
  });
  it("never permits canonical input overrides and does not refine read-only observation", async () => {
    const refineAction = vi.fn(async () => ({ stage: "remove" }));
    const adapter = createCursorFamilyWorkflowAdapter({
      spec: MISSING_NATIVE_SCRIPT_TX_CURSOR_SPEC,
      l1: l1(stage),
      transactions: port(async () => ({ transaction: transaction() })),
      stateQueueMutationLeaseCoordinator: noLeaseCoordinator,
      refineAction,
    });
    await expect(adapter.observe(context)).rejects.toThrow(
      "cannot override canonical input stage",
    );
    expect(
      await adapter.observe({ ...context, reconciliationOnly: true }),
    ).toEqual({ kind: "action_required", action: required(stage.value) });
    expect(refineAction).toHaveBeenCalledOnce();
  });
});

it("reobserves the original self-loop cursor after its successor rolls back", async () => {
  const original: FraudProofRawL1FamilyStage = {
    kind: "step",
    step: 7,
    threadOutRef: outRef("11"),
    stateQueueBlockOutRef: outRef("10"),
  };
  const stage = { value: original };
  let included = true;
  const capture = vi.fn(async () => ({ transaction: transaction() }));
  const adapter = createCursorFamilyWorkflowAdapter({
    spec: MISSING_NATIVE_SCRIPT_TX_CURSOR_SPEC,
    l1: l1(stage, async () => included),
    transactions: port(capture),
    stateQueueMutationLeaseCoordinator: noLeaseCoordinator,
  });
  const before = await adapter.observe(context);
  if (before.kind !== "action_required")
    throw new Error("missing original action");
  stage.value = {
    kind: "step",
    step: 7,
    threadOutRef: `${txHash}#0`,
    stateQueueBlockOutRef: outRef("10"),
  };
  await expect(
    adapter.reconcile({ ...context, action: before.action, txHash }),
  ).resolves.toEqual({ kind: "confirmed", txHash });
  const successor = await adapter.observe(context);
  if (successor.kind !== "action_required")
    throw new Error("missing successor action");
  stage.value = original;
  included = false;
  await expect(adapter.observe(context)).resolves.toEqual(before);
  await expect(
    adapter.preflight({ ...context, action: successor.action }),
  ).rejects.toThrow("differs from authenticated current L1 state");
  await expect(
    adapter.reconcile({ ...context, action: before.action, txHash }),
  ).resolves.toMatchObject({ kind: "unknown" });
  expect(capture).not.toHaveBeenCalled();
});

describe("transition terminal queue mutation lease", () => {
  const terminalStage = (): FraudProofRawL1FamilyStage => ({
    kind: "step",
    step: 7,
    threadOutRef: outRef("11"),
    stateQueueBlockOutRef: outRef("10"),
  });
  const transitionContext = {
    ...context,
    identity: { ...identity, category: "transitionTrace" as const },
  };
  const makeLease = () => ({
    token: "transition-terminal-lease",
    source: "state-queue-observer-v1",
    renew: vi.fn(async () => undefined),
    release: vi.fn(async () => undefined),
    fail: vi.fn(async () => undefined),
  });
  it("journals and resumes the terminal marker lease through pending and confirmed states", async () => {
    const stage = { value: terminalStage() };
    const firstLease = makeLease();
    const make = (lease: ReturnType<typeof makeLease>) =>
      createCursorFamilyWorkflowAdapter({
        spec: TRANSITION_TRACE_CURSOR_SPEC,
        l1: {
          ...l1(stage, async () => true),
          category: "transitionTrace" as const,
        },
        transactions: {
          portVersion: CURSOR_FAMILY_TRANSACTION_PORT,
          category: "transitionTrace" as const,
          prepare: async () => ({}),
          capture: async () => ({
            transaction: transaction(),
            mutationLease: lease,
          }),
        },
        refineAction: async () => ({ requiresMutationLease: true }),
        stateQueueMutationLeaseCoordinator: {
          acquire: async () => lease,
          resume: async () => lease,
        },
      });
    const first = make(firstLease);
    const observed = await first.observe(transitionContext);
    if (observed.kind !== "action_required")
      throw new Error("missing terminal action");
    const action = observed.action;
    const preflight = await first.preflight({ ...transitionContext, action });
    expect(preflight.durableRecovery).toEqual({
      stateQueueMutationLease: {
        token: firstLease.token,
        source: firstLease.source,
      },
    });
    const resumedLease = makeLease();
    const fresh = make(resumedLease);
    await expect(
      fresh.reconcile({
        ...transitionContext,
        action,
        txHash,
        durableRecovery: preflight.durableRecovery,
      }),
    ).resolves.toEqual({ kind: "pending", txHash });
    expect(resumedLease.renew).toHaveBeenCalledOnce();
    stage.value = {
      kind: "proof_token",
      fraudProofOutRef: `${txHash}#0`,
      stateQueueBlockOutRef: `${txHash}#1`,
      nextRemovalOutRef: `${txHash}#1`,
    };
    await expect(
      fresh.reconcile({
        ...transitionContext,
        action,
        txHash,
        durableRecovery: preflight.durableRecovery,
      }),
    ).resolves.toEqual({ kind: "confirmed", txHash });
    expect(resumedLease.release).toHaveBeenCalledOnce();
  });
  it("refuses a terminal capture or journal that omits its mutation lease", async () => {
    const stage = { value: terminalStage() };
    const capture = vi.fn(async () => ({ transaction: transaction() }));
    const adapter = createCursorFamilyWorkflowAdapter({
      spec: TRANSITION_TRACE_CURSOR_SPEC,
      l1: { ...l1(stage), category: "transitionTrace" as const },
      transactions: {
        portVersion: CURSOR_FAMILY_TRANSACTION_PORT,
        category: "transitionTrace" as const,
        prepare: async () => ({}),
        capture,
      },
      refineAction: async () => ({ requiresMutationLease: true }),
      stateQueueMutationLeaseCoordinator: noLeaseCoordinator,
    });
    const observed = await adapter.observe(transitionContext);
    if (observed.kind !== "action_required")
      throw new Error("missing terminal action");
    await expect(
      adapter.preflight({ ...transitionContext, action: observed.action }),
    ).rejects.toThrow(/lease/);
    await expect(
      adapter.reconcile({
        ...transitionContext,
        action: observed.action,
        txHash,
      }),
    ).resolves.toMatchObject({
      kind: "conflict",
      reason: expect.stringContaining("lease"),
    });
    const changed = {
      ...observed.action,
      input: { ...observed.action.input, requiresMutationLease: false },
    };
    await expect(
      adapter.preflight({ ...transitionContext, action: changed }),
    ).rejects.toThrow(/authenticated current L1 state/);
    expect(capture).toHaveBeenCalledOnce();
  });
});
