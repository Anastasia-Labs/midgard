import { beforeEach, describe, expect, it, vi } from "vitest";

const mocks = vi.hoisted(() => ({
  accepted: vi.fn(),
  publish: vi.fn(),
  resolve: vi.fn(),
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

import { createScriptIntegrityHashMismatchLucidActuator } from "../src/script-integrity-hash-mismatch/lucid-actuator.js";

const capture = () =>
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
  });

  it("captures a publication for journaling only after the ordinary transaction exceeds the real byte limit", async () => {
    mocks.accepted.mockRejectedValue(
      new Error("Max transaction size of 16384 exceeded. Found: 19752"),
    );
    await expect(capture()).resolves.toEqual({
      transaction: mocks.transaction,
      prerequisite: "proof_chunks",
    });
    expect(mocks.publish).toHaveBeenCalledOnce();
    expect(mocks.publish.mock.calls[0]![0]).toMatchObject({
      awaitConfirmation: false,
      preSubmitBoundary: {},
    });
  });

  it("reopens exact published chunks on restart and captures the family transition", async () => {
    const chunks = [{ outRef: `${"55".repeat(32)}#0` }];
    mocks.accepted
      .mockRejectedValueOnce(
        new Error("Max transaction size of 16384 exceeded. Found: 19752"),
      )
      .mockResolvedValueOnce(undefined);
    mocks.resolve.mockResolvedValue(chunks);
    await expect(capture()).resolves.toEqual({
      transaction: mocks.transaction,
    });
    expect(mocks.accepted.mock.calls[1]![0]).toMatchObject({
      publishedProofChunks: chunks,
    });
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
