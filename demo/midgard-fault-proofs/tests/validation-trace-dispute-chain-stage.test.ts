import type { LucidEvolution, UTxO } from "@lucid-evolution/lucid";
import { describe, expect, it, vi } from "vitest";

import { deriveValidationTraceDisputeChainStage } from "../src/validation-dispute/workflow-chain-state.js";

const at = (name: string) => ({ spendingScriptAddress: `addr_test_${name}` });

const chain = {
  opener: at("opener"),
  source: at("source"),
  game: at("game"),
  boundary: at("boundary"),
  timeout: at("timeout"),
  award: at("award"),
  prepareResolvers: [],
  semanticResolvers: [],
} as unknown as Parameters<
  typeof deriveValidationTraceDisputeChainStage
>[0]["chain"];

const derive = (utxoByUnit: () => Promise<UTxO>) => {
  const utxosAt = vi.fn(async (): Promise<UTxO[]> => []);
  const lucid = { utxoByUnit, utxosAt } as unknown as LucidEvolution;
  const run = deriveValidationTraceDisputeChainStage({
    lucid,
    chain,
    computationThreadPolicyId: "11".repeat(28),
    fraudProofPolicyId: "22".repeat(28),
    fraudProofSpendingScriptAddress: "addr_test_proof",
    stateQueue: { policyId: "33".repeat(28), address: "addr_test_queue" },
    categoryId: "0000000a",
    headerHash: "44".repeat(28),
    currentTime: 0,
  });
  return { run, utxosAt };
};

describe("validationTraceDispute chain-stage thread lookup", () => {
  it("fails closed on a provider error instead of deriving not_started", async () => {
    const { run, utxosAt } = derive(() =>
      Promise.reject(
        new Error(
          "Location: getUtxoByUnit. Error: Couldn't perform query. Received status code: 502",
        ),
      ),
    );
    await expect(run).rejects.toThrow("Received status code: 502");
    expect(utxosAt).not.toHaveBeenCalled();
  });

  it("treats a provider's explicit unit-not-found as no live thread", async () => {
    const { run, utxosAt } = derive(() =>
      Promise.reject(new Error("Unit not found.")),
    );
    // With no thread, the derivation moves on to the state-queue lookup
    // (which this stub leaves empty, so it rejects further down); the
    // not-found error itself must not surface.
    await expect(run).rejects.not.toThrow("Unit not found");
    expect(utxosAt).toHaveBeenCalledWith("addr_test_queue");
  });
});
