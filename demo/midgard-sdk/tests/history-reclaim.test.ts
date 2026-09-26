import { type LucidEvolution, type UTxO } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { describe, expect, it, vi } from "vitest";

import { type MidgardValidators } from "../src/common.js";
import { encodeEventHistoryData } from "../src/user-events/history-data.js";
import { buildReclaimEventHistoryDataTxProgram } from "../src/user-events/history-reclaim.js";

// These provider-boundary cases complement the applied retention/retirement
// scenarios in the node reserve-payout suite. No transaction is constructed
// from an archive-only or mismatching retained output.
const retainedDatum = (eventKey: string): string =>
  encodeEventHistoryData({
    event_key: eventKey,
    event_payload: 0n,
    reclaim_auth: { PublicKeyCredential: ["aa".repeat(28)] },
  });
// A decodable datum, so each refusal below can only come from its own guard.
const retained: UTxO = {
  txHash: "11".repeat(32),
  outputIndex: 0,
  address: "retention-fixture",
  assets: { lovelace: 3_000_000n },
  datum: retainedDatum("01".repeat(32)),
};
const unavailable = /unavailable at the deployed retention script/;
const contracts = {
  eventHistory: {
    deposit: { retention: { spendingScriptAddress: retained.address } },
  },
} as unknown as MidgardValidators;

describe("retained-data reclamation provider authority", () => {
  it.each([
    { name: "already spent retained data", outputs: [], refusal: unavailable },
    {
      name: "retained data at another address",
      outputs: [{ ...retained, address: "foreign-retention" }],
      refusal: unavailable,
    },
    {
      name: "a reference-script-bearing output",
      outputs: [
        { ...retained, scriptRef: { type: "PlutusV3", script: "5900" } },
      ],
      refusal: unavailable,
    },
    {
      name: "retained bytes different from the requested output",
      outputs: [{ ...retained, datum: retainedDatum("02".repeat(32)) }],
      refusal: /differs from the requested output/,
    },
  ])("refuses $name before collecting inputs", async ({ outputs, refusal }) => {
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
    if (result._tag === "Left")
      expect(String(result.left.cause)).toMatch(refusal);
    expect(newTx).not.toHaveBeenCalled();
    expect(lucid.utxosByOutRef).toHaveBeenCalledWith([retained]);
  });
});
