import { Constr, type UTxO } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { expect } from "vitest";

import { buildInitDaBondPoolTxProgram } from "../src/da-bond-pool-transactions.js";
import {
  assembled,
  MINT,
  parameters,
  poolAt,
  redeemerOf,
  type Scene,
  submit,
} from "./da-bond-pool-transactions.setup-scene.js";

export const initPool = async (
  scene: Scene,
  lovelace: bigint,
): Promise<UTxO> => {
  const [initUtxo] = await scene.lucid.wallet().getUtxos();
  const built = await Effect.runPromise(
    buildInitDaBondPoolTxProgram(scene.lucid, {
      poolValidator: scene.pool,
      parameters,
      initUtxo: initUtxo!,
      lovelace,
    }),
  );
  const signed = await (await built.complete()).sign.withWallet().complete();
  const view = assembled(signed);
  const txHash = await submit(scene, signed);
  const pool = await poolAt(scene);
  expect(pool.txHash).toBe(txHash);
  expect(redeemerOf(view, MINT)).toEqual(
    new Constr(0, [BigInt(pool.outputIndex)]),
  );
  return pool;
};
