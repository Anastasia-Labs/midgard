import { CML, credentialToAddress, Data } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  assertDaAvailabilitySignedLimits,
  type DaAvailabilityOperationLimits,
} from "../src/availability-challenge-operation.js";

const PENALTY = 100_000_000n;
const CAP = 1_200_000n;
const CLOSE_CAP = 1_000_000n;
const ADDRESS = credentialToAddress("Preprod", {
  type: "Key",
  hash: "33".repeat(28),
});

const limits = (
  overrides: Partial<DaAvailabilityOperationLimits> = {},
): DaAvailabilityOperationLimits => ({
  maxTxSize: 16_384,
  maxTxExMem: 14_000_000n,
  maxTxExSteps: 10_000_000_000n,
  coinsPerUtxoByte: 4_310n,
  feeCeilings: { timeout: CAP, close: CLOSE_CAP },
  timeoutFeePartCeiling: PENALTY,
  ...overrides,
});

/** A minimal signed-shape transaction with one redeemer and the given fee. */
const intent = (action: string, fee: bigint) => {
  const inputs = CML.TransactionInputList.new();
  inputs.add(
    CML.TransactionInput.new(CML.TransactionHash.from_hex("ab".repeat(32)), 0n),
  );
  const outputs = CML.TransactionOutputList.new();
  outputs.add(
    CML.TransactionOutput.new(
      CML.Address.from_bech32(ADDRESS),
      CML.Value.from_coin(5_000_000n),
    ),
  );
  const redeemers = CML.LegacyRedeemerList.new();
  redeemers.add(
    CML.LegacyRedeemer.new(
      CML.RedeemerTag.Spend,
      0n,
      CML.PlutusData.from_cbor_hex(Data.void()),
      CML.ExUnits.new(1_000_000n, 1_000_000n),
    ),
  );
  const witnesses = CML.TransactionWitnessSet.new();
  witnesses.set_redeemers(CML.Redeemers.new_arr_legacy_redeemer(redeemers));
  const tx = CML.Transaction.new(
    CML.TransactionBody.new(inputs, outputs, fee),
    witnesses,
    true,
  );
  return {
    action,
    signedCbor: tx.to_cbor_hex(),
    txHash: CML.hash_transaction(tx.body()).to_hex(),
  } as Parameters<typeof assertDaAvailabilitySignedLimits>[0];
};

const EXCEEDS = /exceeds deployment fee, size or execution reserve/;

describe("signed timeout fee limits cap c = fee - feePart only", () => {
  it("admits a full penalty plus the whole cap", () => {
    expect(() =>
      assertDaAvailabilitySignedLimits(
        intent("timeout", PENALTY + CAP),
        limits(),
        PENALTY,
      ),
    ).not.toThrow();
  });

  it("refuses c one lovelace above max_timeout_fee", () => {
    expect(() =>
      assertDaAvailabilitySignedLimits(
        intent("timeout", PENALTY + CAP + 1n),
        limits(),
        PENALTY,
      ),
    ).toThrow(EXCEEDS);
    expect(() =>
      assertDaAvailabilitySignedLimits(
        intent("timeout", 30_000n + CAP + 1n),
        limits(),
        30_000n,
      ),
    ).toThrow(EXCEEDS);
  });

  it("refuses a claimed fee part above the fee or the penalty", () => {
    expect(() =>
      assertDaAvailabilitySignedLimits(
        intent("timeout", 500_000n),
        limits(),
        500_001n,
      ),
    ).toThrow(EXCEEDS);
    expect(() =>
      assertDaAvailabilitySignedLimits(
        intent("timeout", PENALTY + 1n),
        limits(),
        PENALTY + 1n,
      ),
    ).toThrow(EXCEEDS);
  });

  it("holds a journaled timeout without its fee part to penalty + cap", () => {
    expect(() =>
      assertDaAvailabilitySignedLimits(
        intent("timeout", PENALTY + CAP),
        limits(),
      ),
    ).not.toThrow();
    expect(() =>
      assertDaAvailabilitySignedLimits(
        intent("timeout", PENALTY + CAP + 1n),
        limits(),
      ),
    ).toThrow(EXCEEDS);
    expect(() =>
      assertDaAvailabilitySignedLimits(
        intent("timeout", CAP + 1n),
        limits({ timeoutFeePartCeiling: undefined }),
      ),
    ).toThrow(EXCEEDS);
  });

  it("keeps the whole-fee ceiling for every other action", () => {
    expect(() =>
      assertDaAvailabilitySignedLimits(intent("close", CLOSE_CAP), limits()),
    ).not.toThrow();
    expect(() =>
      assertDaAvailabilitySignedLimits(
        intent("close", CLOSE_CAP + 1n),
        limits(),
        0n,
      ),
    ).toThrow(EXCEEDS);
  });
});
