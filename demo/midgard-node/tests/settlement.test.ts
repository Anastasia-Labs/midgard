import {
  CML,
  generateSeedPhrase,
  walletFromSeed,
} from "@lucid-evolution/lucid";
import { describe, expect, it, vi } from "vitest";

import type { SettlementAttempt } from "../src/database/settlement.js";
import type { NodeConfigDep } from "../src/services/config.js";
import {
  canExpireSettlementAttempt,
  inspectSettlementAttempt,
  settlementNextPhase,
  settlementWalletAddress,
} from "../src/services/settlement.js";
import {
  readSettlementOutputEvidence,
  settlementOutputsMatch,
} from "../src/services/settlement-output.js";

// These tests pin the new scheduler's replay boundary, which the synchronous
// reserve/payout builder tests do not exercise.
describe("automatic settlement recovery", () => {
  const expired = {
    status: "not_found",
    validToSlot: 120,
    beforeSlot: 121,
    afterSlot: 121,
    beforeHash: "aa",
    afterHash: "aa",
    allInputsUnspent: true,
  };
  it.each([
    { ...expired, status: "pending" },
    { ...expired, status: "failed" },
    { ...expired, status: "confirmed" },
    { ...expired, beforeSlot: 119, afterSlot: 119 },
    { ...expired, afterSlot: 122 },
    { ...expired, afterHash: "bb" },
    { ...expired, allInputsUnspent: false },
  ])("does not replace an ambiguous body: %j", (input) => {
    expect(canExpireSettlementAttempt(input)).toBe(false);
  });
  it("permits rebuilding only after expiry with a stable indexed tip and unspent fee inputs", () => {
    expect(canExpireSettlementAttempt(expired)).toBe(true);
    expect(
      canExpireSettlementAttempt({
        ...expired,
        beforeSlot: 120,
        afterSlot: 120,
      }),
    ).toBe(true);
  });
  it("does not treat initialization or reserve funding as a completed withdrawal", () => {
    expect(settlementNextPhase("initialize")).toBe("fund");
    expect(settlementNextPhase("fund")).toBe("fund");
    expect(settlementNextPhase("conclude")).toBe("complete");
    expect(settlementNextPhase("absorb")).toBe("complete");
  });
  it("isolates the fee wallet from all existing operational wallets", () => {
    const main = generateSeedPhrase(),
      merge = generateSeedPhrase(),
      refs = generateSeedPhrase(),
      settlement = generateSeedPhrase();
    const config = {
      NETWORK: "Preprod",
      L1_OPERATOR_SEED_PHRASE: main,
      L1_OPERATOR_SEED_PHRASE_FOR_MERGE_TX: merge,
      L1_REFERENCE_SCRIPT_SEED_PHRASE: refs,
      L1_SETTLEMENT_SEED_PHRASE: settlement,
    } as NodeConfigDep;
    expect(settlementWalletAddress(config)).toBe(
      walletFromSeed(settlement, { network: "Preprod" }).address,
    );
    for (const seed of [main, merge, refs])
      expect(() =>
        settlementWalletAddress({ ...config, L1_SETTLEMENT_SEED_PHRASE: seed }),
      ).toThrow("distinct");
    expect(() =>
      settlementWalletAddress({ ...config, L1_SETTLEMENT_SEED_PHRASE: "" }),
    ).toThrow("requires a funded");
  });
  it("checks body identity, finite validity and reserved inputs on every replay", async () => {
    const inputs = CML.TransactionInputList.new();
    inputs.add(
      CML.TransactionInput.new(
        CML.TransactionHash.from_hex("ab".repeat(32)),
        0n,
      ),
    );
    const outputs = CML.TransactionOutputList.new();
    const address = walletFromSeed(generateSeedPhrase(), {
      network: "Preprod",
    }).address;
    outputs.add(
      CML.TransactionOutput.new(
        CML.Address.from_bech32(address),
        CML.Value.from_coin(2_000_000n),
      ),
    );
    const body = CML.TransactionBody.new(inputs, outputs, 200_000n);
    body.set_ttl(120n);
    const tx = CML.Transaction.new(body, CML.TransactionWitnessSet.new(), true);
    const attempt: SettlementAttempt = {
      deployment_id: "cd".repeat(32),
      kind: "deposit",
      event_id: "01",
      phase: "absorb",
      signed_cbor: tx.to_cbor_hex(),
      tx_hash: CML.hash_transaction(body).to_hex(),
      required_outputs: [0],
      fee_inputs: [`${"ab".repeat(32)}#0`],
      status: "pending",
      recovery: false,
    };
    expect(inspectSettlementAttempt(attempt).validToSlot).toBe(120);
    const historical = [
      {
        transaction_id: attempt.tx_hash,
        output_index: 0,
        address,
        value: { coins: "2000000", assets: {} },
        datum_hash: null,
        script_hash: null,
        created_at: { slot_no: 110, header_hash: "ef".repeat(32) },
        spent_at: { slot_no: 115, header_hash: "fe".repeat(32) },
      },
    ];
    expect(settlementOutputsMatch(attempt, "ef".repeat(32), historical)).toBe(
      true,
    );
    expect(settlementOutputsMatch(attempt, "ee".repeat(32), historical)).toBe(
      false,
    );
    expect(
      settlementOutputsMatch(attempt, "ef".repeat(32), [
        { ...historical[0], value: { coins: "1999999", assets: {} } },
      ]),
    ).toBe(false);
    // A restart can observe an output already consumed by a later transaction.
    // Kupo's unspent query would lose this evidence and block the entire queue.
    const fetch = vi
      .spyOn(globalThis, "fetch")
      .mockImplementation(async (url) =>
        Response.json(String(url).includes("unspent") ? [] : historical),
      );
    try {
      expect(
        await readSettlementOutputEvidence(
          "http://kupo.invalid",
          attempt,
          "ef".repeat(32),
        ),
      ).toBe(true);
      expect(fetch).toHaveBeenCalledWith(
        `http://kupo.invalid/matches/*@${attempt.tx_hash}`,
        expect.anything(),
      );
    } finally {
      fetch.mockRestore();
    }

    expect(() =>
      inspectSettlementAttempt({ ...attempt, tx_hash: "00".repeat(32) }),
    ).toThrow("hash mismatch");
    expect(() =>
      inspectSettlementAttempt({ ...attempt, required_outputs: [1] }),
    ).toThrow("output index");
    expect(() =>
      inspectSettlementAttempt({ ...attempt, fee_inputs: [] }),
    ).toThrow("fee reservation");
    expect(() =>
      inspectSettlementAttempt({
        ...attempt,
        fee_inputs: [`${"ee".repeat(32)}#0`],
      }),
    ).toThrow("fee reservation");
  });
});
