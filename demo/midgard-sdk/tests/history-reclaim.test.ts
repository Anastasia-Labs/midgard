import { type LucidEvolution, type UTxO } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { describe, expect, it, vi } from "vitest";

import { type MidgardValidators } from "../src/common.js";
import { buildReclaimEventHistoryDataTxProgram } from "../src/user-events/history-reclaim.js";

// These provider-boundary cases complement the applied retention/retirement
// scenarios in the node reserve-payout suite. No transaction is constructed
// from an archive-only or mismatching retained output.
const retained: UTxO = {
  txHash: "11".repeat(32),
  outputIndex: 0,
  address: "retention-fixture",
  assets: { lovelace: 3_000_000n },
  datum: "d87980",
};
const contracts = {
  eventHistory: {
    deposit: { retention: { spendingScriptAddress: retained.address } },
  },
} as unknown as MidgardValidators;

describe("retained-data reclamation provider authority", () => {
  it.each([
    { name: "already spent retained data", outputs: [] },
    {
      name: "retained data at another address",
      outputs: [{ ...retained, address: "foreign-retention" }],
    },
    {
      name: "a reference-script-bearing output",
      outputs: [
        { ...retained, scriptRef: { type: "PlutusV3", script: "5900" } },
      ],
    },
    {
      name: "retained bytes different from the requested output",
      outputs: [{ ...retained, datum: "d8798101" }],
    },
  ])("refuses $name before collecting inputs", async ({ outputs }) => {
    const newTx = vi.fn();
    const lucid = {
      config: () => ({ network: "Custom" }),
      utxosByOutRef: vi.fn().mockResolvedValue(outputs),
      newTx,
    } as unknown as LucidEvolution;
    const result = await Effect.runPromise(
      Effect.either(
        buildReclaimEventHistoryDataTxProgram(lucid, contracts, {
          kind: "Deposit",
          retainedInput: retained,
          hubOracleRefInput: { ...retained, txHash: "22".repeat(32) },
        }),
      ),
    );
    expect(result._tag).toBe("Left");
    expect(newTx).not.toHaveBeenCalled();
    expect(lucid.utxosByOutRef).toHaveBeenCalledWith([retained]);
  });
});
