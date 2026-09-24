import {
  type LucidEvolution,
  type Script,
  type UTxO,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { expect, it, vi } from "vitest";

import { referenceScriptAuthTokenNameText } from "../src/reference-scripts.js";
import {
  mergeReferenceScripts,
  resolveReferenceScriptsProgram,
} from "../src/reserve-payout/references.js";

const reference = (outputIndex: number): UTxO => ({
  txHash: "11".repeat(32),
  outputIndex,
  address: "reference-fixture",
  assets: { lovelace: 2_000_000n },
});

it.each(["deposit", "withdrawal"] as const)(
  "resolves published %s spending and retirement roles without losing the retirement reference",
  (kind) => {
    const list = reference(0);
    const retirement = reference(1);
    const merged = mergeReferenceScripts(undefined, [
      { name: `${kind} spending`, utxo: list },
      { name: `${kind} history retirement`, utxo: retirement },
    ]);
    expect(referenceScriptAuthTokenNameText(`${kind} spending`)).toBe(
      kind === "deposit" ? "DepositSpend" : "WithdrawalSpend",
    );
    expect(referenceScriptAuthTokenNameText(`${kind} history retirement`)).toBe(
      kind === "deposit"
        ? "DepositHistoryRetirement"
        : "WithdrawalHistoryRetirement",
    );
    expect(
      kind === "deposit" ? merged.depositSpending : merged.withdrawalSpending,
    ).toBe(list);
    expect(merged.historyRetirement).toBe(retirement);
  },
);

it.each(["deposit", "withdrawal"] as const)(
  "keeps explicit history references authoritative for published %s role names",
  async (kind) => {
    const utxosAt = vi.fn();
    const lucid = { utxosAt } as unknown as LucidEvolution;
    const explicit = {
      historyList: reference(0),
      historyRetirement: reference(1),
    };
    const script: Script = { type: "PlutusV3", script: "5900" };
    const resolved = await Effect.runPromise(
      resolveReferenceScriptsProgram(
        lucid,
        "reference-fixture",
        [
          { name: `${kind} spending`, script },
          { name: `${kind} history retirement`, script },
        ],
        explicit,
      ),
    );
    expect(utxosAt).not.toHaveBeenCalled();
    expect(mergeReferenceScripts(explicit, resolved)).toMatchObject(explicit);
  },
);
