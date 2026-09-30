import "./cursor-family-adapter.production-cursor-family-adapter-v1.js";

import { describe, expect, it, vi } from "vitest";

import { TRANSITION_TRACE_CURSOR_SPEC } from "../src/transition-trace/workflow-spec.js";
import {
  createCursorFamilyWorkflowAdapter,
  CURSOR_FAMILY_TRANSACTION_PORT,
} from "../src/workflow/cursor-family-adapter.js";
import { MISSING_NATIVE_SCRIPT_TX_CURSOR_SPEC } from "../src/workflow/cursor-family-spec.js";
import type { FraudProofRawL1FamilyStage } from "../src/workflow/raw-l1-family-derivation.js";
import {
  context,
  identity,
  l1,
  noLeaseCoordinator,
  outRef,
  port,
  required,
  transaction,
  txHash,
} from "./cursor-family-adapter.terminal.js";

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
