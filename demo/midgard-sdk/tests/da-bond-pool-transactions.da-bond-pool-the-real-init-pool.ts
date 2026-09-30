import "./da-bond-pool-transactions.da-bond-pool-builders-the-transaction-lucid-assembles.js";

import { readFileSync } from "node:fs";

import { Data, toUnit, type TxSigned } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import { daBondPoolUnit, encodeDaBondPoolDatum } from "../src/da-bond-pool.js";
import { appendDaBondPoolInitialization } from "../src/da-bond-pool-transactions.js";
import { parseFaultProofBlueprint } from "../src/fraud-proof/contracts/blueprint.js";
import { buildDaBondPoolValidator } from "../src/protocol-contracts.js";
import {
  ADA,
  assembled,
  FLOOR,
  NETWORK,
  parameters,
  parkingAddress,
  setupScene,
  standInPool,
  submit,
} from "./da-bond-pool-transactions.setup-scene.js";

describe("DA bond pool: the real InitPool", () => {
  const blueprint = parseFaultProofBlueprint(
    JSON.parse(
      readFileSync(
        new URL("../../../onchain/aiken/plutus.json", import.meta.url),
        "utf8",
      ),
    ) as unknown,
  );

  it("mints the pool from the local blueprint and measures what it adds to a transaction", async () => {
    const scene = await setupScene();
    const parking = parkingAddress();
    // Split off the nonce first, then publish the pool's minting script from
    // the other wallet UTxO so the publish cannot spend the nonce.
    const walletAddress = await scene.lucid.wallet().address();
    const split = await (
      await scene.lucid
        .newTx()
        .pay.ToAddress(walletAddress, { lovelace: 200n * ADA })
        .complete()
    ).sign
      .withWallet()
      .complete();
    const splitHash = await submit(scene, split);
    const walletUtxos = await scene.lucid.wallet().getUtxos();
    const nonce = walletUtxos.find(
      (utxo) => utxo.txHash === splitHash && utxo.outputIndex === 0,
    );
    const funding = walletUtxos.find((utxo) => utxo !== nonce);
    expect(nonce?.assets).toEqual({ lovelace: 200n * ADA });
    expect(funding).toBeDefined();
    const pool = buildDaBondPoolValidator(
      blueprint,
      NETWORK,
      nonce!,
      "11".repeat(28),
      "22".repeat(28),
      parameters,
    );
    const publish = await (
      await scene.lucid
        .newTx()
        .collectFrom([funding!])
        .pay.ToContract(
          parking,
          { kind: "inline", value: Data.void() },
          { lovelace: 60n * ADA },
          pool.mintingScript,
        )
        .complete({ coinSelection: false })
    ).sign
      .withWallet()
      .complete();
    const publishHash = await submit(scene, publish);
    const [mintingReference] = await scene.lucid.utxosByOutRef([
      { txHash: publishHash, outputIndex: 0 },
    ]);
    expect(await scene.lucid.utxosByOutRef([nonce!])).toHaveLength(1);

    // A baseline that already runs a script, as the atomic init does, so the
    // measured difference is the pool alone and not collateral.
    const standIn = standInPool();
    const baseUnit = toUnit(standIn.policyId, "42415345");
    const base = () =>
      scene.lucid
        .newTx()
        .collectFrom([nonce!])
        .mintAssets({ [baseUnit]: 1n }, Data.void())
        .attach.Script(standIn.mintingScript)
        .pay.ToAddress(parking, { lovelace: 2n * ADA, [baseUnit]: 1n });
    const signedSize = async (
      tx: ReturnType<typeof base>,
    ): Promise<{ bytes: number; signed: TxSigned }> => {
      const signed = await (await tx.complete()).sign.withWallet().complete();
      return { bytes: assembled(signed).bytes, signed };
    };
    const baseline = await signedSize(base());
    const inline = await signedSize(
      appendDaBondPoolInitialization(base(), {
        poolValidator: pool,
        floorLovelace: FLOOR,
        lovelace: FLOOR,
      }),
    );
    const referenced = await signedSize(
      appendDaBondPoolInitialization(base(), {
        poolValidator: pool,
        floorLovelace: FLOOR,
        lovelace: FLOOR,
        referenceScript: mintingReference,
      }),
    );
    const scriptBytes = pool.mintingScript.script.length / 2;
    const inlineDelta = inline.bytes - baseline.bytes;
    const referencedDelta = referenced.bytes - baseline.bytes;
    console.info(
      `DA bond pool init size: script ${scriptBytes.toString()} B; ` +
        `+${inlineDelta.toString()} B inline, +${referencedDelta.toString()} B by reference ` +
        `(baseline ${baseline.bytes.toString()} B)`,
    );
    expect(inlineDelta - referencedDelta).toBeGreaterThanOrEqual(
      scriptBytes - 64,
    );
    expect(referencedDelta).toBeLessThan(400);

    await submit(scene, referenced.signed);
    const minted = await scene.lucid.utxosAtWithUnit(
      pool.spendingScriptAddress,
      daBondPoolUnit(pool.policyId),
    );
    expect(minted).toHaveLength(1);
    expect(minted[0]!.datum).toBe(encodeDaBondPoolDatum("Bonded"));
    expect(minted[0]!.assets).toEqual({
      lovelace: FLOOR,
      [daBondPoolUnit(pool.policyId)]: 1n,
    });
  }, 300_000);
});
