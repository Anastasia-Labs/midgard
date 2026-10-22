/** What the chain shows of our published copies of a field the observe
 * move of a canonical-decode item reads when the field is too long to carry
 * inline, read from every transaction the emulator accepted: the copies,
 * the transactions that read each, and the one that spends it. */
import { CML, Data } from "@lucid-evolution/lucid";
import { expect } from "vitest";

import type { Scene } from "./validation-trace-dispute-open-scene.js";

export type Landed = Readonly<{
  hash: string;
  cbor: string;
  tx: CML.Transaction;
}>;

/** Every transaction the emulator accepted, in order. */
export const landed = (scene: Scene): Landed[] =>
  [...scene.l1.signedCbors].map(([hash, cbor]) => ({
    hash,
    cbor,
    tx: CML.Transaction.from_cbor_hex(cbor),
  }));

export const outRefsOf = (inputs: CML.TransactionInputList | undefined) => {
  const found: string[] = [];
  for (let index = 0; index < (inputs?.len() ?? 0); index += 1) {
    const input = inputs!.get(index);
    found.push(
      `${input.transaction_id().to_hex()}#${input.index().toString()}`,
    );
  }
  return found;
};

/** The outputs the emulator accepted at our key address that carry only ADA
 * and, as their inline datum, `bytes` bytes and nothing else. */
export const copiesOf = (scene: Scene, bytes: number) => {
  const prover = scene.h.proverSigner.address;
  return landed(scene).flatMap(({ hash, tx }) => {
    const outputs = tx.body().outputs();
    const found: string[] = [];
    for (let index = 0; index < outputs.len(); index += 1) {
      const output = outputs.get(index);
      const datum = output.datum()?.as_datum();
      if (
        output.address().to_bech32(undefined) === prover &&
        output.amount().multi_asset().policy_count() === 0 &&
        output.script_ref() === undefined &&
        datum !== undefined &&
        Data.from<string>(datum.to_cbor_hex()).length === 2 * bytes
      )
        found.push(`${hash}#${index.toString()}`);
    }
    return found;
  });
};

/** The transactions that read `outRef` as a reference input. */
export const readersOf = (scene: Scene, outRef: string) =>
  landed(scene).filter(({ tx }) =>
    outRefsOf(tx.body().reference_inputs()).includes(outRef),
  );

/** The transaction that spends `outRef`, if one landed. */
export const spenderOf = (scene: Scene, outRef: string) =>
  landed(scene).find(({ tx }) =>
    outRefsOf(tx.body().inputs()).includes(outRef),
  );

/** The block the emulator included the transaction `hash` in. */
export const blockOf = async (scene: Scene, hash: string) => {
  const status = await scene.h.emulator.getTransactionStatus(hash);
  if (status.status !== "confirmed")
    throw new Error(`transaction ${hash} is not confirmed`);
  return status.confirmation.blockHeight!;
};

/** Spends `outRef` back to our key address by our key, and lands it. */
export const spendByKey = async (scene: Scene, outRef: string) => {
  const lucid = scene.h.proverLucid;
  const [hash, index] = outRef.split("#");
  const [utxo] = await lucid.utxosByOutRef([
    { txHash: hash!, outputIndex: Number(index) },
  ]);
  const spend = await lucid
    .newTx()
    .collectFrom([utxo!])
    .complete({ coinSelection: false });
  await (await spend.sign.withWallet().complete()).submit();
  scene.h.emulator.awaitBlock(1);
};

/** `spender` spends one copy back to our key address by our key alone: no
 * script runs and nothing else is spent. */
export const expectReclaimedByKey = (scene: Scene, spender: Landed) => {
  const body = spender.tx.body();
  expect(spender.tx.witness_set().redeemers()).toBeUndefined();
  expect(body.inputs().len()).toBe(1);
  const outputs = body.outputs();
  for (let index = 0; index < outputs.len(); index += 1)
    expect(outputs.get(index).address().to_bech32(undefined)).toBe(
      scene.h.proverSigner.address,
    );
};
