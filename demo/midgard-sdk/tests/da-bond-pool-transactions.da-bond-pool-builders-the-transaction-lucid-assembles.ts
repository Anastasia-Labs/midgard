import "./da-bond-pool-transactions.da-bond-pool-builders-refusals-before-assembly.js";

import { Constr, type UTxO } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import {
  decodeDaBondPoolDatum,
  encodeDaBondPoolDatum,
} from "../src/da-bond-pool.js";
import {
  appendDaBondPoolInitialization,
  buildBeginDaBondPoolWithdrawTxProgram,
  buildCancelDaBondPoolWithdrawTxProgram,
  buildCompleteDaBondPoolWithdrawTxProgram,
  buildTopUpDaBondPoolTxProgram,
  DaBondPoolBuildError,
} from "../src/da-bond-pool-transactions.js";
import { initPool } from "./da-bond-pool-transactions.init-pool.js";
import {
  ADA,
  assembled,
  FLOOR,
  MIN_TOP_UP,
  MINT,
  outRefKey,
  parameters,
  parkingAddress,
  poolAt,
  redeemerOf,
  setupScene,
  SPEND,
  submit,
  WITHDRAW_DELAY_MS,
} from "./da-bond-pool-transactions.setup-scene.js";

describe("DA bond pool builders: the transaction Lucid assembles", () => {
  it("InitPool, TopUp, Begin, Cancel, Begin, Complete name the real positions and bounds", async () => {
    const scene = await setupScene();
    const initial = FLOOR + 600n * ADA;
    let pool = await initPool(scene, initial);
    expect(pool.assets).toEqual({ lovelace: initial, [scene.unit]: 1n });
    expect(pool.datum).toBe(encodeDaBondPoolDatum("Bonded"));

    // TopUp, by reference: the datum bytes carry over, value grows by Δ.
    const topUp = await Effect.runPromise(
      buildTopUpDaBondPoolTxProgram(scene.lucid, {
        poolValidator: scene.pool,
        parameters,
        pool: { utxo: pool },
        amount: MIN_TOP_UP,
        referenceScripts: { daBondPoolSpending: scene.spendingReference },
      }),
    );
    let signed = await (await topUp.complete()).sign.withWallet().complete();
    let view = assembled(signed);
    expect(view.inlineScripts).toBe(0);
    expect(view.referenceInputs).toContain(outRefKey(scene.spendingReference));
    await submit(scene, signed);
    const toppedUp = await poolAt(scene);
    expect(redeemerOf(view, SPEND)).toEqual(
      new Constr(0, [BigInt(toppedUp.outputIndex)]),
    );
    expect(toppedUp.datum).toBe(pool.datum);
    expect(toppedUp.assets).toEqual({
      lovelace: initial + MIN_TOP_UP,
      [scene.unit]: 1n,
    });
    pool = toppedUp;

    // BeginWithdraw with a validTo that is not on a slot boundary: unlock_at
    // follows the upper bound the ledger carries, not the one asked for.
    const begin = async (utxo: UTxO) => {
      const now = BigInt(scene.emulator.now());
      const built = await Effect.runPromise(
        buildBeginDaBondPoolWithdrawTxProgram(scene.lucid, {
          poolValidator: scene.pool,
          parameters,
          pool: { utxo },
          daParamsUtxo: scene.daParamsUtxo,
          signerKeyHashes: [scene.walletKeyHash],
          withdrawDelayMs: WITHDRAW_DELAY_MS,
          validity: { validFrom: now, validTo: now + 300_537n },
        }),
      );
      const beginSigned = await (await built.complete()).sign
        .withWallet()
        .complete();
      const beginView = assembled(beginSigned);
      await submit(scene, beginSigned);
      const next = await poolAt(scene);
      const ledgerValidTo = BigInt(
        scene.lucid.slotToUnixTime(Number(beginView.ttlSlot!)),
      );
      expect(ledgerValidTo).toBeLessThan(now + 300_537n);
      const unlockAt = ledgerValidTo - 1n + WITHDRAW_DELAY_MS;
      expect(decodeDaBondPoolDatum(next.datum!)).toEqual({
        Withdrawing: { unlock_at: unlockAt },
      });
      expect(next.assets).toEqual(utxo.assets);
      expect(beginView.signers).toEqual([scene.walletKeyHash]);
      expect(redeemerOf(beginView, SPEND)).toEqual(
        new Constr(2, [
          BigInt(
            beginView.referenceInputs.indexOf(outRefKey(scene.daParamsUtxo)),
          ),
          BigInt(next.outputIndex),
        ]),
      );
      expect(beginView.inlineScripts).toBe(1);
      return { next, unlockAt };
    };
    const firstBegin = await begin(pool);
    pool = firstBegin.next;

    const cancel = await Effect.runPromise(
      buildCancelDaBondPoolWithdrawTxProgram(scene.lucid, {
        poolValidator: scene.pool,
        parameters,
        pool: { utxo: pool },
        daParamsUtxo: scene.daParamsUtxo,
        signerKeyHashes: [scene.walletKeyHash],
        referenceScripts: { daBondPoolSpending: scene.spendingReference },
      }),
    );
    signed = await (await cancel.complete()).sign.withWallet().complete();
    view = assembled(signed);
    await submit(scene, signed);
    const cancelled = await poolAt(scene);
    expect(cancelled.datum).toBe(encodeDaBondPoolDatum("Bonded"));
    expect(cancelled.assets).toEqual(pool.assets);
    // The params UTxO and the script reference share a transaction, so the
    // sorted order is fixed by output index: params second, although the
    // builder reads it first and the body lists it first.
    expect(view.referenceInputs).toEqual([
      outRefKey(scene.spendingReference),
      outRefKey(scene.daParamsUtxo),
    ]);
    expect(redeemerOf(view, SPEND)).toEqual(
      new Constr(3, [1n, BigInt(cancelled.outputIndex)]),
    );
    pool = cancelled;

    const secondBegin = await begin(pool);
    pool = secondBegin.next;
    const unlockAt = secondBegin.unlockAt;
    scene.emulator.awaitSlot(Number(WITHDRAW_DELAY_MS / 1_000n) + 400);

    const destination = parkingAddress();
    const amount = 250n * ADA;
    const complete = await Effect.runPromise(
      buildCompleteDaBondPoolWithdrawTxProgram(scene.lucid, {
        poolValidator: scene.pool,
        parameters,
        pool: { utxo: pool },
        daParamsUtxo: scene.daParamsUtxo,
        signerKeyHashes: [scene.walletKeyHash],
        amount,
        destination,
        validity: { validFrom: unlockAt },
      }),
    );
    signed = await (await complete.complete()).sign.withWallet().complete();
    view = assembled(signed);
    const completeHash = await submit(scene, signed);
    const ledgerValidFrom = BigInt(
      scene.lucid.slotToUnixTime(Number(view.validityStartSlot!)),
    );
    // unlock_at sits 1 ms before a slot boundary (a slot-aligned upper bound
    // minus one plus a whole-second delay), and Lucid floors validFrom to its
    // slot, so the builder must round the asked-for unlock_at up to the next
    // boundary: exactly unlock_at + 1.
    expect(ledgerValidFrom).toBe(unlockAt + 1n);
    const completed = await poolAt(scene);
    expect(completed.datum).toBe(encodeDaBondPoolDatum("Bonded"));
    expect(completed.assets).toEqual({
      lovelace: initial + MIN_TOP_UP - amount,
      [scene.unit]: 1n,
    });
    const [paid] = (await scene.lucid.utxosAt(destination)).filter(
      (utxo) => utxo.txHash === completeHash,
    );
    expect(paid?.assets).toEqual({ lovelace: amount });
    expect(redeemerOf(view, SPEND)).toEqual(
      new Constr(4, [
        amount,
        BigInt(view.referenceInputs.indexOf(outRefKey(scene.daParamsUtxo))),
        BigInt(completed.outputIndex),
      ]),
    );
  }, 300_000);

  it("resolves InitPool's output_index from the final position, and refuses a stated one it does not match", async () => {
    const scene = await setupScene();
    const [initUtxo] = await scene.lucid.wallet().getUtxos();
    const parking = parkingAddress();
    const withPriorOutputs = (outputIndex?: bigint) =>
      appendDaBondPoolInitialization(
        scene.lucid
          .newTx()
          .collectFrom([initUtxo!])
          .pay.ToAddress(parking, { lovelace: 2n * ADA })
          .pay.ToAddress(parking, { lovelace: 3n * ADA })
          .pay.ToAddress(parking, { lovelace: 4n * ADA }),
        {
          poolValidator: scene.pool,
          floorLovelace: FLOOR,
          lovelace: FLOOR,
          ...(outputIndex === undefined ? {} : { outputIndex }),
        },
      );
    await expect(withPriorOutputs(0n).complete()).rejects.toThrow(
      /landed at 3, expected 0/u,
    );
    const signed = await (await withPriorOutputs(3n).complete()).sign
      .withWallet()
      .complete();
    const view = assembled(signed);
    const txHash = await submit(scene, signed);
    const pool = await poolAt(scene);
    expect(pool.txHash).toBe(txHash);
    expect(pool.outputIndex).toBe(3);
    expect(redeemerOf(view, MINT)).toEqual(new Constr(0, [3n]));
    expect(() =>
      appendDaBondPoolInitialization(scene.lucid.newTx(), {
        poolValidator: scene.pool,
        floorLovelace: FLOOR,
        lovelace: FLOOR - 1n,
      }),
    ).toThrow(DaBondPoolBuildError);
  }, 300_000);
});
