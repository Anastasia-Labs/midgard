import {
  applyDoubleCborEncoding,
  credentialToAddress,
  type LucidEvolution,
  type Script,
  type UTxO,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { expect, it, vi } from "vitest";

import {
  referenceScriptAuthTokenNameText,
  referenceScriptAuthUnit,
} from "../src/reference-scripts.js";
import {
  mergeReferenceScripts,
  resolveReferenceScriptsProgram,
} from "../src/reserve-payout/references.js";

const policyId = "ab".repeat(28);

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
    const utxosAtWithUnit = vi.fn();
    const lucid = { utxosAt, utxosAtWithUnit } as unknown as LucidEvolution;
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
        { policyId },
        explicit,
      ),
    );
    expect(utxosAt).not.toHaveBeenCalled();
    expect(utxosAtWithUnit).not.toHaveBeenCalled();
    expect(mergeReferenceScripts(explicit, resolved)).toMatchObject(explicit);
  },
);

it("resolves a payout reference only from the holder of its role token", async () => {
  const address = credentialToAddress("Preprod", {
    type: "Key",
    hash: "cd".repeat(28),
  });
  const script: Script = {
    type: "PlutusV3",
    script: applyDoubleCborEncoding(`5820${"01".repeat(32)}`),
  };
  const unit = referenceScriptAuthUnit(policyId, "payout spending");
  // The same script at the same address, with a lower outRef, but no role
  // token: anyone can publish that, so it must not be picked.
  const imposter: UTxO = {
    txHash: "00".repeat(32),
    outputIndex: 0,
    address,
    assets: { lovelace: 40_000_000n },
    scriptRef: script,
  };
  const genuine: UTxO = {
    txHash: "ff".repeat(32),
    outputIndex: 0,
    address,
    assets: { lovelace: 40_000_000n, [unit]: 1n },
    scriptRef: script,
  };
  const resolveFrom = (wallet: readonly UTxO[]) => {
    const utxosAtWithUnit = vi.fn(async (_: string, wanted: string) =>
      wallet.filter((utxo) => utxo.assets[wanted] !== undefined),
    );
    const lucid = {
      utxosAt: async () => [...wallet],
      utxosAtWithUnit,
    } as unknown as LucidEvolution;
    return Effect.runPromise(
      Effect.either(
        resolveReferenceScriptsProgram(
          lucid,
          address,
          [{ name: "payout spending", script }],
          { policyId },
        ),
      ),
    ).then((result) => ({ result, utxosAtWithUnit }));
  };

  const accepted = await resolveFrom([imposter, genuine]);
  expect(accepted.utxosAtWithUnit).toHaveBeenCalledWith(address, unit);
  expect(accepted.result._tag).toBe("Right");
  if (accepted.result._tag === "Right")
    expect(accepted.result.right).toEqual([
      { name: "payout spending", utxo: genuine },
    ]);

  const refused = await resolveFrom([imposter]);
  expect(refused.result._tag).toBe("Left");
  if (refused.result._tag === "Left")
    expect(refused.result.left.message).toBe("Missing reference script");
});
