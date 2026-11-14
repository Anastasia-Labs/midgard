/**
 * Step 03 judges an authenticated min-Ada descriptor at Midgard's L2 rate, the
 * deployment constant the step-03 validator compiles, never at the L1
 * `coinsPerUtxoByte` protocol parameter a deployment binding carries. The
 * descriptor here sits between the two floors, so the rate the local
 * pre-check reads decides whether a real fault is submitted or refused.
 */
import { MinAdaStep03DatumSchema } from "@al-ft/midgard-sdk";
import {
  MIDGARD_COINS_PER_UTXO_BYTE,
  minAdaLovelace,
  outputMeetsMinAda,
} from "@al-ft/midgard-validation";
import { Data, type UTxO } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  minAdaDescriptorIsFault,
  submitMinAdaStep03,
} from "../src/min-ada/submit-step-03.js";

/** An L1 rate unlike Midgard's 4,310, as a binding's protocol parameters may carry. */
const L1_COINS_PER_UTXO_BYTE = 1n;
const TOTAL_LENGTH = 60n;
const L2_FLOOR = minAdaLovelace(MIDGARD_COINS_PER_UTXO_BYTE, TOTAL_LENGTH);
/** Meets the L1 floor, one lovelace under the L2 floor. */
const UNDER_L2 = L2_FLOOR - 1n;

const descriptor = (direction: bigint, lovelace: bigint) => ({
  direction,
  total_length: TOTAL_LENGTH,
  lovelace,
});

describe("min-ada step 03 rate", () => {
  it("fixes the descriptor between the two floors", () => {
    expect(MIDGARD_COINS_PER_UTXO_BYTE).toBe(4_310n);
    expect(
      outputMeetsMinAda(L1_COINS_PER_UTXO_BYTE, TOTAL_LENGTH, UNDER_L2),
    ).toBe(true);
    expect(
      outputMeetsMinAda(MIDGARD_COINS_PER_UTXO_BYTE, TOTAL_LENGTH, UNDER_L2),
    ).toBe(false);
  });

  it("judges both directions at the L2 floor", () => {
    expect(minAdaDescriptorIsFault(descriptor(0n, UNDER_L2))).toBe(true);
    expect(minAdaDescriptorIsFault(descriptor(1n, UNDER_L2))).toBe(false);
    expect(minAdaDescriptorIsFault(descriptor(0n, L2_FLOOR))).toBe(false);
    expect(minAdaDescriptorIsFault(descriptor(1n, L2_FLOOR))).toBe(true);
  });

  describe("submitter pre-check", () => {
    const policyId = "ab".repeat(28);
    const categoryId = "cd".repeat(4);
    const headerHash = "ef".repeat(28);
    const prover = "12".repeat(28);
    const address =
      "addr_test1wrsexavz37208qda7mwwu4k7hcpg26cz0ce86f5e9kul3hqzlh22t";
    const contracts = {
      computationThread: { policyId },
      steps: [
        {},
        {},
        { spendingScriptAddress: address, spendingScriptHash: "34".repeat(28) },
      ],
    };
    const threadAt = (lovelace: bigint, direction = 0n): UTxO => ({
      txHash: "56".repeat(32),
      outputIndex: 0,
      address,
      assets: {
        lovelace: 5_000_000n,
        [`${policyId}${categoryId}${headerHash}`]: 1n,
      },
      datum: Data.to(
        {
          fraud_prover: prover,
          data: { MinAdaTxDescriptor: descriptor(direction, lovelace) },
        } as never,
        MinAdaStep03DatumSchema as never,
      ),
    });
    const submit = (thread: UTxO) =>
      submitMinAdaStep03({
        lucid: {
          utxosByOutRef: () => Promise.resolve([thread]),
        } as never,
        contracts: contracts as never,
        categoryId,
        signer: { paymentKeyHash: prover, selectWallet: () => {} } as never,
        threadOutRef: `${thread.txHash}#0`,
        // No script: a submission that clears the pre-check stops at the
        // reference-script check, before any transaction is built.
        referenceScriptUtxo: { ...thread, datum: null, scriptRef: null },
      });

    it("submits an accepted output that meets the L1 floor but not the L2 floor", async () => {
      await expect(submit(threadAt(UNDER_L2))).rejects.toThrow(
        /reference .* carries no script/u,
      );
    });

    it("refuses an accepted output that meets the L2 floor", async () => {
      await expect(submit(threadAt(L2_FLOOR))).rejects.toThrow(
        /does not violate min-Ada/u,
      );
    });
  });
});
