import "vitest";
import "../src/remove-fraudulent-block.remove-fraudulent-block-explicit-category.js";
import "../src/workflow/action-changed.js";
import "../src/workflow/family-l1-observation.js";
import "../src/workflow/journal.fraud-proof-workflow-terminal.js";
import "../src/workflow/linear-family-adapter.js";
import "../src/workflow/linear-family-state.js";
import "../src/workflow/transaction-boundary.js";
import "./linear-family-adapter.terminal.js";

import { describe, expect, it, vi } from "vitest";

import { WorkflowActionChangedError } from "../src/workflow/action-changed.js";
import {
  type JournalJsonObject,
  normalizeJournalJson,
} from "../src/workflow/journal.fraud-proof-workflow-terminal.js";
import { createLinearFamilyWorkflowAdapter } from "../src/workflow/linear-family-adapter.js";
import { linearFamilyObservation } from "../src/workflow/linear-family-state.js";
import type { FraudProofRawL1FamilyStage } from "../src/workflow/raw-l1-family-derivation.js";
import { workflowPreflightTransaction } from "../src/workflow/transaction-boundary.js";
import {
  context,
  headerHash,
  identity,
  l1,
  outRef,
  port,
  provenance,
  signed,
  terminal,
  transaction,
  txHash,
} from "./linear-family-adapter.terminal.js";

describe("production linear family adapter V1", () => {
  it.each(["fabricatedDeposit", "fabricatedWithdrawal"] as const)(
    "holds the terminal queue lease for %s until its submission",
    async (category) => {
      const stage: { value: FraudProofRawL1FamilyStage } = {
        value: {
          kind: "step",
          step: 4,
          threadOutRef: outRef("22"),
          stateQueueBlockOutRef: outRef("11"),
        },
      };
      const required = linearFamilyObservation({
        category,
        headerHash,
        provenance,
        stage: stage.value,
      });
      if (required.kind !== "action_required")
        throw new Error("missing terminal action");
      const lease = {
        token: "terminal-lease",
        source: "queue-observer",
        renew: vi.fn(async () => undefined),
        release: vi.fn(async () => undefined),
        fail: vi.fn(async () => undefined),
      };
      const observation = { ...l1(stage), category };
      const transactions = {
        ...port(async () => ({
          transaction: transaction(),
          mutationLease: lease,
        })),
        category,
      };
      const args = {
        ...context,
        identity: { ...identity, category },
        action: required.action,
      };
      const adapter = createLinearFamilyWorkflowAdapter({
        category,
        l1: observation,
        transactions,
      });
      const preflight = await adapter.preflight(args);
      expect(lease.release).not.toHaveBeenCalled();
      await expect(adapter.submit({ ...args, preflight })).resolves.toEqual({
        kind: "submitted",
        txHash,
      });
      expect(lease.release).toHaveBeenCalledTimes(1);
      expect(lease.fail).not.toHaveBeenCalled();
      const withoutLease = createLinearFamilyWorkflowAdapter({
        category,
        l1: observation,
        transactions: {
          ...transactions,
          capture: async () => ({ transaction: transaction() }),
        },
      });
      await expect(withoutLease.preflight(args)).rejects.toThrow(
        "topology disagreed with its mutation lease",
      );
    },
  );

  it("distinguishes a changed authenticated action from a deterministic build failure", async () => {
    const stage: { value: FraudProofRawL1FamilyStage } = {
      value: { kind: "not_started", stateQueueBlockOutRef: outRef("10") },
    };
    const failure = new Error("local UPLC validator rejected the transaction");
    const capture = vi.fn(async () => {
      throw failure;
    });
    const adapter = createLinearFamilyWorkflowAdapter({
      category: "daHashPreimage",
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
        objective: { category: "daHashPreimage", headerHash },
      },
    });
  });

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
    });
    const observation = await adapter.observe(context);
    if (observation.kind !== "action_required") {
      throw new Error("expected action");
    }
    const preflight = await adapter.preflight({
      ...context,
      action: observation.action,
    });
    expect(workflowPreflightTransaction(preflight)).toBe(
      (await capture.mock.results[0]!.value).transaction.signed,
    );
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

  it("accepts a JSON-normalized copy of the required action", async () => {
    const stage = {
      value: {
        kind: "not_started",
        stateQueueBlockOutRef: outRef("10"),
      } as const,
    };
    const capture = vi.fn(async () => ({ transaction: transaction() }));
    const adapter = createLinearFamilyWorkflowAdapter({
      category: "daHashPreimage",
      l1: l1(stage),
      transactions: port(capture),
    });
    const observation = await adapter.observe(context);
    if (observation.kind !== "action_required") {
      throw new Error("expected action");
    }
    const normalized = {
      actionId: observation.action.actionId,
      input: normalizeJournalJson(
        observation.action.input,
        "action",
      ) as JournalJsonObject,
    };
    expect(Object.keys(normalized.input)).not.toEqual(
      Object.keys(observation.action.input),
    );
    const preflight = await adapter.preflight({
      ...context,
      action: normalized,
    });
    expect(preflight.actionId).toBe(observation.action.actionId);
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
    });
    await expect(
      adapter.preflight({ ...context, action: required.action }),
    ).rejects.toThrow("embeds inline script witnesses");
    expect(lease.fail).toHaveBeenCalledTimes(1);
    expect(lease.release).not.toHaveBeenCalled();
  });

  it("holds the descendant-removal mutation lease until the removal is submitted, then completes without a rebuild", async () => {
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
    });
    const preflight = await adapter.preflight({
      ...context,
      action: required.action,
    });
    expect(lease.release).not.toHaveBeenCalled();
    await expect(
      adapter.submit({ ...context, action: required.action, preflight }),
    ).resolves.toEqual({ kind: "submitted", txHash });
    expect(lease.release).toHaveBeenCalledTimes(1);
    expect(lease.fail).not.toHaveBeenCalled();
    stage.value = { kind: "removed", terminal: terminal() };

    const fresh = createLinearFamilyWorkflowAdapter({
      category: "daHashPreimage",
      l1: l1(stage, async () => true),
      transactions: port(async () => {
        throw new Error("must not rebuild");
      }),
    });
    await expect(fresh.observe(context)).resolves.toEqual({
      kind: "completed",
      terminal: terminal(),
    });
  });

  it("rejects a removal capture without its mutation-lease authority", async () => {
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
    });
    await expect(
      missing.preflight({ ...context, action: required.action }),
    ).rejects.toThrow("topology disagreed with its mutation lease");
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
      }),
    ).toThrow("ports changed identity");
  });
});

it("reobserves the original linear step after its successor rolls back", async () => {
  const original: FraudProofRawL1FamilyStage = {
    kind: "step",
    step: 1,
    threadOutRef: outRef("11"),
    stateQueueBlockOutRef: outRef("10"),
  };
  const stage = { value: original };
  const capture = vi.fn(async () => ({ transaction: transaction() }));
  const adapter = createLinearFamilyWorkflowAdapter({
    category: "daHashPreimage",
    l1: l1(stage),
    transactions: port(capture),
  });
  const before = await adapter.observe(context);
  if (before.kind !== "action_required")
    throw new Error("missing original action");
  stage.value = {
    kind: "step",
    step: 2,
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
