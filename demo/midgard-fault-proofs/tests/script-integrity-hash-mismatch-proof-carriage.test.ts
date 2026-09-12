import { beforeEach, describe, expect, it, vi } from "vitest";

const mocks = vi.hoisted(() => ({
  accepted: vi.fn(),
  publish: vi.fn(),
  resolve: vi.fn(),
  directFirst: vi.fn(),
  transaction: { txHash: "ab".repeat(32) },
}));

vi.mock(
  "../src/script-integrity-hash-mismatch/submit.js",
  async (original) => ({
    ...(await original<object>()),
    submitScriptIntegrityHashMismatchStep01Accepted: mocks.accepted,
  }),
);
vi.mock("../src/publish-proof-chunks.js", async (original) => ({
  ...(await original<object>()),
  publishProofChunks: mocks.publish,
  resolvePublishedProofChunks: mocks.resolve,
}));
vi.mock("../src/workflow/transaction-boundary.js", async (original) => ({
  ...(await original<object>()),
  captureLocallyEvaluatedTransaction: async (
    submit: (boundary: unknown) => Promise<void>,
  ) => {
    await submit({});
    return mocks.transaction;
  },
}));

vi.mock("../src/workflow/proof-chunk-prerequisite.js", async (original) => ({
  ...(await original<object>()),
  resolveDirectFirstProofChunks: mocks.directFirst,
}));

import { createScriptIntegrityHashMismatchLucidActuator } from "../src/script-integrity-hash-mismatch/lucid-actuator.js";
import type { FraudProofWorkflowAction } from "../src/workflow/orchestrator.js";

const capture = (workflowAction?: FraudProofWorkflowAction) =>
  createScriptIntegrityHashMismatchLucidActuator({
    binding: {
      definition: { headerHash: "11".repeat(28) },
      network: "Custom",
      resolvedContracts: { category: { categoryId: "00000033" } },
    },
    lucid: {},
    signer: { paymentKeyHash: "22".repeat(28) },
    contracts: {},
    references: { steps: [{}], witnesses: {} },
  } as never).capture({
    workflowAction,
    action: {
      stage: "step_01",
      threadOutRef: `${"33".repeat(32)}#0`,
      stateQueueBlockOutRef: `${"44".repeat(32)}#0`,
    },
    artifact: {
      headerHash: "11".repeat(28),
      acceptedInclusion: { txMembershipProofCbor: "80" },
    } as never,
  });

describe("script integrity source proof publication recovery", () => {
  beforeEach(() => {
    vi.clearAllMocks();
    mocks.accepted.mockReset();
    mocks.resolve.mockResolvedValue(undefined);
    mocks.publish.mockResolvedValue(undefined);
    mocks.directFirst.mockResolvedValue([]);
  });

  it("leaves size-limit publication scheduling to the common journal adapter", async () => {
    mocks.accepted.mockRejectedValue(
      new Error("Max transaction size of 16384 exceeded. Found: 19752"),
    );
    await expect(capture()).rejects.toThrow("Max transaction size");
    expect(mocks.publish).not.toHaveBeenCalled();
    expect(mocks.resolve).not.toHaveBeenCalled();
    expect(mocks.directFirst).not.toHaveBeenCalled();
  });

  it("uses the exact common workflow action to reopen published chunks", async () => {
    const chunks = [{ outRef: `${"55".repeat(32)}#0` }];
    const workflowAction = {
      actionId: "step_01:thread",
      input: { category: "scriptIntegrityHashMismatch", stage: "step_01" },
    };
    mocks.directFirst.mockResolvedValue(chunks);
    mocks.accepted.mockResolvedValue(undefined);
    await expect(capture(workflowAction)).resolves.toEqual({
      transaction: mocks.transaction,
    });
    expect(mocks.directFirst).toHaveBeenCalledWith(
      expect.objectContaining({ action: workflowAction, proofCbor: "80" }),
    );
    expect(mocks.accepted).toHaveBeenCalledWith(
      expect.objectContaining({ publishedProofChunks: chunks }),
    );
    expect(mocks.publish).not.toHaveBeenCalled();
  });

  it.each([
    "validator refused",
    "Max transaction size of 32768 exceeded. Found: 33000",
  ])("does not publish after an unrelated failure: %s", async (message) => {
    mocks.accepted.mockRejectedValue(new Error(message));
    await expect(capture()).rejects.toThrow(message);
    expect(mocks.resolve).not.toHaveBeenCalled();
    expect(mocks.publish).not.toHaveBeenCalled();
  });
});
