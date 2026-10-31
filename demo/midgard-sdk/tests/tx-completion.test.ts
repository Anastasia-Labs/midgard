import { readFileSync } from "node:fs";

import {
  applyDoubleCborEncoding,
  Constr,
  Data,
  Emulator,
  generateEmulatorAccount,
  Lucid,
  type SpendingValidator,
  validatorToAddress,
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

  it("evaluates a script spend whose redeemer and inline datum hold a tag 102 constructor before a sibling", async () => {
    // Lucid writes Constr 128 as tag 102 over an indefinite [index, fields]
    // list. Before uplc 0.5.3 the evaluator's decoder left that list's break
    // unread, so the enclosing list ended there and the sibling `5` broke
    // the transaction decode.
    const value = Data.to(new Constr(0, [new Constr(128, []), 5n]));
    expect(value).toBe("d8799fd8669f188080ff05ff");
    const blueprint = JSON.parse(
      readFileSync(
        new URL(
          "../../midgard-node/blueprints/always-succeeds/plutus.json",
          import.meta.url,
        ),
        "utf8",
      ),
    ) as { validators: { compiledCode: string }[] };
    const validator: SpendingValidator = {
      type: "PlutusV3",
      script: applyDoubleCborEncoding(blueprint.validators[0]!.compiledCode),
    };
    const account = generateEmulatorAccount({ lovelace: 100_000_000n });
    const emulator = new Emulator([account]);
    const lucid = await Lucid(emulator, "Custom");
    lucid.selectWallet.fromSeed(account.seedPhrase);
    const scriptAddress = validatorToAddress("Custom", validator);
    const lock = await lucid
      .newTx()
      .pay.ToContract(
        scriptAddress,
        { kind: "inline", value },
        { lovelace: 5_000_000n },
      )
      .complete();
    await emulator.awaitTx(
      await (await lock.sign.withWallet().complete()).submit(),
    );
    const [locked] = await lucid.utxosAt(scriptAddress);
    expect(locked?.datum).toBe(value);

    const completed = await Effect.runPromise(
      completeTxWithLocalUPLCEvalProgram(
        lucid
          .newTx()
          .collectFrom([locked!], value)
          .attach.SpendingValidator(validator),
        (cause) => cause,
      ),
    );

    const redeemers = completed.toTransaction().witness_set().redeemers();
    expect(
      redeemers?.as_arr_legacy_redeemer()?.get(0).data().to_cbor_hex(),
    ).toBe(value);
    await emulator.awaitTx(
      await (await completed.sign.withWallet().complete()).submit(),
    );
    expect(await lucid.utxosAt(scriptAddress)).toEqual([]);
  });
});
