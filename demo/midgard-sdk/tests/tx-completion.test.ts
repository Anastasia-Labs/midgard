import {
  Emulator,
  generateEmulatorAccount,
  Lucid,
} from "@lucid-evolution/lucid";
import { Effect, Either } from "effect";
import { describe, expect, it, vi } from "vitest";

import {
  completeTxWithLocalUPLCEvalProgram,
  EMPTY_WALLET_INPUTS_CAUSE,
} from "../src/tx-completion.js";

const payment = async () => {
  const account = generateEmulatorAccount({ lovelace: 30_000_000n });
  const lucid = await Lucid(new Emulator([account]), "Custom");
  lucid.selectWallet.fromSeed(account.seedPhrase);
  const wallet = lucid.wallet();
  const inputs = await wallet.getUtxos();
  const tx = lucid.newTx().pay.ToAddress(account.address, {
    lovelace: 2_000_000n,
  });
  return { wallet, inputs, tx };
};

describe("shared transaction completion", () => {
  it("refuses an empty explicit pool before completion or wallet fallback", async () => {
    const { tx, wallet } = await payment();
    const complete = vi.spyOn(tx, "complete");
    const readWallet = vi.spyOn(wallet, "getUtxos");
    const mapped = { kind: "completion-refused" };
    const catchError = vi.fn((_cause: unknown) => mapped);

    const result = await Effect.runPromise(
      Effect.either(completeTxWithLocalUPLCEvalProgram(tx, catchError, [])),
    );

    expect(complete).not.toHaveBeenCalled();
    expect(readWallet).not.toHaveBeenCalled();
    expect(result).toEqual(Either.left(mapped));
    expect(catchError).toHaveBeenCalledExactlyOnceWith(
      expect.objectContaining({ cause: EMPTY_WALLET_INPUTS_CAUSE }),
    );
  });

  it("completes from the selected pool without reading the wallet", async () => {
    const { tx, wallet, inputs } = await payment();
    const readWallet = vi
      .spyOn(wallet, "getUtxos")
      .mockRejectedValue(new Error("unexpected wallet fallback"));

    const completed = await Effect.runPromise(
      completeTxWithLocalUPLCEvalProgram(tx, (cause) => cause, inputs),
    );

    expect(readWallet).not.toHaveBeenCalled();
    expect(completed.toTransaction().body().inputs().len()).toBe(1);
    expect(
      completed
        .toTransaction()
        .body()
        .inputs()
        .get(0)
        .transaction_id()
        .to_hex(),
    ).toBe(inputs[0]!.txHash);
  });

  it("keeps wallet discovery when the pool is omitted", async () => {
    const { tx, wallet } = await payment();
    const readWallet = vi.spyOn(wallet, "getUtxos");

    const completed = await Effect.runPromise(
      completeTxWithLocalUPLCEvalProgram(tx, (cause) => cause),
    );

    expect(readWallet).toHaveBeenCalled();
    expect(completed.toTransaction().body().inputs().len()).toBe(1);
  });
});
