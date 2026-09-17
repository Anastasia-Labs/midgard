import { CML, credentialToAddress, type UTxO } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { afterEach, describe, expect, it, vi } from "vitest";

import { awaitSubmittedTransactionConfirmation } from "../src/transactions/utils.js";

const fixture = (datumMode: "none" | "inline" | "hash" = "none") => {
  const address = credentialToAddress("Preprod", {
    type: "Key",
    hash: "ab".repeat(28),
  });
  const outputs = CML.TransactionOutputList.new();
  const datum = CML.PlutusData.from_cbor_hex("00");
  const datumHash = CML.hash_plutus_data(datum).to_hex();
  for (let index = 0; index < 3; index += 1) {
    const output = CML.TransactionOutput.new(
      CML.Address.from_bech32(address),
      CML.Value.from_coin(2_000_000n + BigInt(index)),
      index === 0 && datumMode !== "none"
        ? datumMode === "inline"
          ? CML.DatumOption.new_datum(datum)
          : CML.DatumOption.new_hash(CML.DatumHash.from_hex(datumHash))
        : undefined,
    );
    outputs.add(output);
  }
  const tx = CML.Transaction.new(
    CML.TransactionBody.new(CML.TransactionInputList.new(), outputs, 0n),
    CML.TransactionWitnessSet.new(),
    true,
    undefined,
  );
  const txHash = CML.hash_transaction(tx.body()).to_hex();
  const visible: UTxO[] = [0, 1, 2].map((outputIndex) => ({
    txHash,
    outputIndex,
    address,
    assets: { lovelace: 2_000_000n + BigInt(outputIndex) },
  }));
  if (datumMode === "inline") visible[0]!.datum = "00";
  if (datumMode === "hash") visible[0]!.datumHash = datumHash;
  const utxosByOutRef = vi.fn(async () => visible.slice(0, 2));
  const awaitTxConfirmation = vi.fn(async () => ({ txHash }));
  const overrideUTxOs = vi.fn();
  const lucid = {
    config: () => ({ provider: undefined }),
    awaitTxConfirmation,
    utxosByOutRef,
    // Even an already populated wallet override cannot satisfy provider readiness.
    wallet: () => ({ getUtxos: async () => visible, overrideUTxOs }),
  };
  const run = (
    requiredOutputIndexes: readonly number[] = [0, 1],
    confirmedHash = txHash,
  ) =>
    Effect.runPromise(
      Effect.either(
        awaitSubmittedTransactionConfirmation(
          lucid as never,
          {
            txHash: confirmedHash,
            signedTxCbor: tx.to_cbor_hex(),
            walletAddress: address,
          },
          {
            requiredOutputIndexes,
            confirmationTimeoutMs: 1_000,
            confirmationRetries: 0,
            confirmationPollIntervalMs: 100,
          },
        ),
      ),
    );
  return {
    visible,
    utxosByOutRef,
    awaitTxConfirmation,
    overrideUTxOs,
    txHash,
    run,
  };
};

afterEach(() => vi.useRealTimers());

describe("required transaction output visibility", () => {
  it("returns immediately when required outputs are visible, without waiting for unrelated outputs", async () => {
    vi.useFakeTimers();
    const f = fixture();
    const result = f.run();
    await vi.advanceTimersByTimeAsync(0);
    expect(await result).toMatchObject({ _tag: "Right", right: f.txHash });
    expect(f.utxosByOutRef).toHaveBeenCalledExactlyOnceWith([
      { txHash: f.txHash, outputIndex: 0 },
      { txHash: f.txHash, outputIndex: 1 },
    ]);
    expect(f.overrideUTxOs).toHaveBeenCalledTimes(1);
    expect(vi.getTimerCount()).toBe(0);
  });

  it("waits through partial and wrong output visibility despite synthetic wallet outputs", async () => {
    vi.useFakeTimers();
    const f = fixture();
    f.utxosByOutRef
      .mockResolvedValueOnce([f.visible[0]!])
      .mockResolvedValueOnce([
        f.visible[0]!,
        { ...f.visible[1]!, txHash: "cd".repeat(32) },
      ])
      .mockResolvedValueOnce([
        f.visible[0]!,
        { ...f.visible[1]!, assets: { lovelace: 1n } },
      ])
      .mockResolvedValueOnce([
        f.visible[0]!,
        { ...f.visible[1]!, address: "wrong-address" },
      ])
      .mockResolvedValueOnce([
        f.visible[0]!,
        { ...f.visible[1]!, datum: "00" },
      ]);
    const result = f.run();
    await vi.advanceTimersByTimeAsync(0);
    expect(f.overrideUTxOs).not.toHaveBeenCalled();
    await vi.advanceTimersByTimeAsync(499);
    expect(f.utxosByOutRef).toHaveBeenCalledTimes(5);
    expect(f.overrideUTxOs).not.toHaveBeenCalled();
    await vi.advanceTimersByTimeAsync(1);
    expect(await result).toMatchObject({ _tag: "Right", right: f.txHash });
    expect(f.awaitTxConfirmation).toHaveBeenCalledTimes(1);
  });

  it.each(["inline", "hash"] as const)(
    "compares %s datums while allowing a provider-resolved hash datum",
    async (mode) => {
      vi.useFakeTimers();
      const f = fixture(mode);
      const correct = { ...f.visible[0]!, datum: "00" };
      f.utxosByOutRef
        .mockResolvedValueOnce([{ ...correct, datum: "01" }, f.visible[1]!])
        .mockResolvedValueOnce([
          { ...correct, datumHash: "ff".repeat(32) },
          f.visible[1]!,
        ])
        .mockResolvedValue([correct, f.visible[1]!]);
      const result = f.run();
      await vi.advanceTimersByTimeAsync(199);
      expect(f.overrideUTxOs).not.toHaveBeenCalled();
      await vi.advanceTimersByTimeAsync(1);
      expect(await result).toMatchObject({ _tag: "Right", right: f.txHash });
    },
  );

  it("fails within the bound when a required output remains unavailable", async () => {
    vi.useFakeTimers();
    const f = fixture();
    f.utxosByOutRef.mockResolvedValue([]);
    const result = f.run();
    await vi.advanceTimersByTimeAsync(1_000);
    const outcome = await result;
    expect(outcome._tag).toBe("Left");
    if (outcome._tag === "Left") {
      expect(outcome.left.txHash).toBe(f.txHash);
      expect(outcome.left.message).toContain(
        "Transaction confirmed but required outputs did not become visible",
      );
    }
    expect(f.overrideUTxOs).not.toHaveBeenCalled();
    expect(f.utxosByOutRef.mock.calls.length).toBeLessThanOrEqual(11);
  });

  it("bounds a stalled provider query and rejects invalid signed output indexes", async () => {
    vi.useFakeTimers();
    const f = fixture();
    f.utxosByOutRef.mockImplementation(() => new Promise<UTxO[]>(() => {}));
    const result = f.run();
    await vi.advanceTimersByTimeAsync(1_000);
    expect((await result)._tag).toBe("Left");
    const mismatched = await f.run([0], "ff".repeat(32));
    expect(mismatched._tag).toBe("Left");
    if (mismatched._tag === "Left")
      expect(String(mismatched.left.cause)).toContain(
        "do not belong to the confirmed signed transaction",
      );
    const invalid = await f.run([3]);
    expect(invalid._tag).toBe("Left");
    if (invalid._tag === "Left")
      expect(invalid.left.message).toBe(
        "Invalid required outputs for confirmed transaction",
      );
  });
});
