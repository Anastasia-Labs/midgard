import { describe, expect, it, vi } from "vitest";

import { WorkflowActionChangedError } from "../src/workflow/action-changed.js";
import { createCursorFamilyWorkflowAdapter } from "../src/workflow/cursor-family-adapter.create-cursor-family-workflow-adapter.js";
import {
  type JournalJsonObject,
  normalizeJournalJson,
} from "../src/workflow/journal.fraud-proof-workflow-terminal.js";
import type { FraudProofRawL1FamilyStage } from "../src/workflow/raw-l1-family-derivation.js";
import { workflowPreflightTransaction } from "../src/workflow/transaction-boundary.js";
import {
  context,
  hash,
  headerHash,
  identity,
  l1,
  outRef,
  port,
  required,
  signed,
  transaction,
  txHash,
} from "./cursor-family-adapter.terminal.js";
import { THIRTEEN_STEP_CURSOR_SPEC } from "./support/thirteen-step-cursor-spec.js";

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
      spec: THIRTEEN_STEP_CURSOR_SPEC,
      l1: l1(stage),
      transactions: port(capture),
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
      holder: {
        workflowId: context.workflowId,
        objective: {
          category: identity.category,
          headerHash,
        },
      },
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
      spec: THIRTEEN_STEP_CURSOR_SPEC,
      l1: l1(stage),
      transactions: port(capture),
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
      spec: THIRTEEN_STEP_CURSOR_SPEC,
      l1: l1(stage),
      transactions: port(capture),
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
      spec: THIRTEEN_STEP_CURSOR_SPEC,
      l1: l1(stage),
      transactions: port(capture),
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
        spec: THIRTEEN_STEP_CURSOR_SPEC,
        l1: l1(stage),
        transactions: port(async () => ({ transaction: hostile })),
      });
      await expect(
        hostileAdapter.preflight({ ...context, action }),
      ).rejects.toThrow();
    }
  });

  it("ends the descendant-removal lease with the removal's submission", async () => {
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
    const acquired = {
      token: "cursor-removal-lease",
      source: "state-queue-observer-v1",
      renew: vi.fn(async () => undefined),
      release: vi.fn(async () => undefined),
      fail: vi.fn(async () => undefined),
    };
    const capturedRemoval = transaction();
    const adapter = createCursorFamilyWorkflowAdapter({
      spec: THIRTEEN_STEP_CURSOR_SPEC,
      l1: l1(stage),
      transactions: port(async () => ({
        transaction: capturedRemoval,
        mutationLease: acquired,
      })),
    });
    const preflight = await adapter.preflight({ ...context, action });
    expect(workflowPreflightTransaction(preflight)).toBe(
      capturedRemoval.signed,
    );
    expect(acquired.release).not.toHaveBeenCalled();
    await expect(
      adapter.submit({ ...context, action, preflight }),
    ).resolves.toEqual({ kind: "submitted", txHash });
    expect(acquired.release).toHaveBeenCalledTimes(1);
    expect(acquired.fail).not.toHaveBeenCalled();
    expect(acquired.renew).not.toHaveBeenCalled();
  });

  it("fails an acquired lease if signed-body admission fails before submission", async () => {
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
      spec: THIRTEEN_STEP_CURSOR_SPEC,
      l1: l1(stage),
      transactions: port(async () => ({
        transaction: transaction({ signed: signed({ inlineScript: true }) }),
        mutationLease: lease,
      })),
    });
    await expect(adapter.preflight({ ...context, action })).rejects.toThrow(
      "embeds inline script witnesses",
    );
    expect(lease.fail).toHaveBeenCalledTimes(1);
    expect(lease.release).not.toHaveBeenCalled();
  });
});
