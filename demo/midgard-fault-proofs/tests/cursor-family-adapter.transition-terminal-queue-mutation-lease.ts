import "./cursor-family-adapter.production-cursor-family-adapter-v1.js";

import { describe, expect, it, vi } from "vitest";

import { TRANSITION_TRACE_CURSOR_SPEC } from "../src/transition-trace/workflow-spec.js";
import { CURSOR_FAMILY_TRANSACTION_PORT } from "../src/workflow/cursor-family-adapter.admit-reference-scripts.js";
import { createCursorFamilyWorkflowAdapter } from "../src/workflow/cursor-family-adapter.create-cursor-family-workflow-adapter.js";
import type { FraudProofRawL1FamilyStage } from "../src/workflow/raw-l1-family-derivation.js";
import {
  context,
  identity,
  l1,
  outRef,
  port,
  required,
  transaction,
  txHash,
} from "./cursor-family-adapter.terminal.js";
import { THIRTEEN_STEP_CURSOR_SPEC } from "./support/thirteen-step-cursor-spec.js";

describe("authenticated family action refinement", () => {
  const stage = {
    value: {
      kind: "step",
      step: 4,
      threadOutRef: outRef("11"),
      stateQueueBlockOutRef: outRef("10"),
    } as FraudProofRawL1FamilyStage,
  };
  it("binds extra grammar fields and rejects changes before capture", async () => {
    let grammar = "start";
    const capture = vi.fn(async () => ({ transaction: transaction() }));
    const refineAction = vi.fn(async () => ({ grammar }));
    const adapter = createCursorFamilyWorkflowAdapter({
      spec: THIRTEEN_STEP_CURSOR_SPEC,
      l1: l1(stage),
      transactions: port(capture),
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
  it("never permits canonical input overrides", async () => {
    const refineAction = vi.fn(async () => ({ stage: "remove" }));
    const adapter = createCursorFamilyWorkflowAdapter({
      spec: THIRTEEN_STEP_CURSOR_SPEC,
      l1: l1(stage),
      transactions: port(async () => ({ transaction: transaction() })),
      refineAction,
    });
    await expect(adapter.observe(context)).rejects.toThrow(
      "cannot override canonical input stage",
    );
    expect(refineAction).toHaveBeenCalledOnce();
  });
});

it("reobserves the original self-loop cursor after its successor rolls back", async () => {
  const original: FraudProofRawL1FamilyStage = {
    kind: "step",
    step: 5,
    threadOutRef: outRef("11"),
    stateQueueBlockOutRef: outRef("10"),
  };
  const stage = { value: original };
  const capture = vi.fn(async () => ({ transaction: transaction() }));
  const adapter = createCursorFamilyWorkflowAdapter({
    spec: THIRTEEN_STEP_CURSOR_SPEC,
    l1: l1(stage),
    transactions: port(capture),
  });
  const before = await adapter.observe(context);
  if (before.kind !== "action_required")
    throw new Error("missing original action");
  stage.value = {
    kind: "step",
    step: 5,
    threadOutRef: `${txHash}#0`,
    stateQueueBlockOutRef: outRef("10"),
  };
  const successor = await adapter.observe(context);
  if (successor.kind !== "action_required")
    throw new Error("missing successor action");
  expect(successor.action.actionId).not.toBe(before.action.actionId);
  stage.value = original;
  await expect(adapter.observe(context)).resolves.toEqual(before);
  await expect(
    adapter.preflight({ ...context, action: successor.action }),
  ).rejects.toThrow("differs from authenticated current L1 state");
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
  it("captures the terminal marker under its lease and ends the lease with the submission", async () => {
    const stage = { value: terminalStage() };
    const lease = makeLease();
    const adapter = createCursorFamilyWorkflowAdapter({
      spec: TRANSITION_TRACE_CURSOR_SPEC,
      l1: { ...l1(stage), category: "transitionTrace" as const },
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
    });
    const observed = await adapter.observe(transitionContext);
    if (observed.kind !== "action_required")
      throw new Error("missing terminal action");
    const action = observed.action;
    const preflight = await adapter.preflight({ ...transitionContext, action });
    expect(preflight).toMatchObject({ actionId: action.actionId, txHash });
    expect(lease.release).not.toHaveBeenCalled();
    await expect(
      adapter.submit({ ...transitionContext, action, preflight }),
    ).resolves.toEqual({ kind: "submitted", txHash });
    expect(lease.release).toHaveBeenCalledOnce();
    expect(lease.fail).not.toHaveBeenCalled();
  });
  it("refuses a terminal capture that omits its mutation lease", async () => {
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
    });
    const observed = await adapter.observe(transitionContext);
    if (observed.kind !== "action_required")
      throw new Error("missing terminal action");
    await expect(
      adapter.preflight({ ...transitionContext, action: observed.action }),
    ).rejects.toThrow(/lease/);
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
