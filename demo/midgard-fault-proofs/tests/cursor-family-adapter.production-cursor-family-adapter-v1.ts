import { describe, expect, it, vi } from "vitest";

import { EXECUTION_NATIVE_SCRIPT_INVALID_CURSOR_SPEC } from "../src/execution-native-script-invalid/workflow-spec.js";
import { WorkflowActionChangedError } from "../src/workflow/action-changed.js";
import { createCursorFamilyWorkflowAdapter } from "../src/workflow/cursor-family-adapter.js";
import {
  type JournalJsonObject,
  normalizeJournalJson,
} from "../src/workflow/journal.js";
import type { FraudProofRawL1FamilyStage } from "../src/workflow/raw-l1-family-derivation.js";
import { workflowPreflightTransaction } from "../src/workflow/transaction-boundary.js";
import {
  context,
  hash,
  l1,
  noLeaseCoordinator,
  outRef,
  port,
  required,
  signed,
  terminal,
  transaction,
  txHash,
} from "./cursor-family-adapter.terminal.js";

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
      spec: EXECUTION_NATIVE_SCRIPT_INVALID_CURSOR_SPEC,
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
      spec: EXECUTION_NATIVE_SCRIPT_INVALID_CURSOR_SPEC,
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
        step: 5,
        threadOutRef: outRef("11"),
        stateQueueBlockOutRef: outRef("10"),
      } as FraudProofRawL1FamilyStage,
    };
    const capture = vi.fn(async () => ({ transaction: transaction() }));
    const adapter = createCursorFamilyWorkflowAdapter({
      spec: EXECUTION_NATIVE_SCRIPT_INVALID_CURSOR_SPEC,
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
        step: 4,
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
      spec: EXECUTION_NATIVE_SCRIPT_INVALID_CURSOR_SPEC,
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
        spec: EXECUTION_NATIVE_SCRIPT_INVALID_CURSOR_SPEC,
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
        step: 5,
        threadOutRef: outRef("11"),
        stateQueueBlockOutRef: outRef("10"),
      } as FraudProofRawL1FamilyStage,
    };
    const action = required(stage.value);
    const first = createCursorFamilyWorkflowAdapter({
      spec: EXECUTION_NATIVE_SCRIPT_INVALID_CURSOR_SPEC,
      l1: l1(stage),
      transactions: port(async () => ({
        transaction: transaction({
          signed: signed({
            submit: async () => {
              stage.value = {
                kind: "step",
                step: 5,
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
      spec: EXECUTION_NATIVE_SCRIPT_INVALID_CURSOR_SPEC,
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
      spec: EXECUTION_NATIVE_SCRIPT_INVALID_CURSOR_SPEC,
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
      spec: EXECUTION_NATIVE_SCRIPT_INVALID_CURSOR_SPEC,
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
      spec: EXECUTION_NATIVE_SCRIPT_INVALID_CURSOR_SPEC,
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
      spec: EXECUTION_NATIVE_SCRIPT_INVALID_CURSOR_SPEC,
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
      spec: EXECUTION_NATIVE_SCRIPT_INVALID_CURSOR_SPEC,
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
      spec: EXECUTION_NATIVE_SCRIPT_INVALID_CURSOR_SPEC,
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
