/**
 * Which outputs at the prover's proof-materials address the transition-trace
 * carriage lookup takes as a chunk's carriage: only the outputs of ours its
 * claims name, never another workflow's output holding the same bytes.
 */
import {
  Data,
  Emulator,
  generateEmulatorAccount,
  getAddressDetails,
  Lucid,
  type LucidEvolution,
  type UTxO,
} from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  proofMaterialsAddress,
  spendProofMaterials,
} from "../src/funding/proof-materials-address.js";
import { resolveTransitionTraceDataCarriage } from "../src/transition-trace/proof-carriage.js";
import { createOwnOutputJournal } from "../src/workflow/own-outputs.js";
import { EMULATOR_PROTOCOL_PARAMETERS } from "./support/emulator/protocol-parameters.js";

const DATUMS = [Data.to("aa".repeat(4096)), Data.to("bb".repeat(100))];

const outRef = (utxo: UTxO) => `${utxo.txHash}#${utxo.outputIndex}`;

/** A prover wallet on a fresh emulator and its proof-materials address. */
const prover = async () => {
  const wallet = generateEmulatorAccount({ lovelace: 2_000_000_000n });
  const emulator = new Emulator([wallet], EMULATOR_PROTOCOL_PARAMETERS);
  const lucid = await Lucid(emulator, "Custom");
  lucid.selectWallet.fromSeed(wallet.seedPhrase);
  const keyHash = getAddressDetails(wallet.address).paymentCredential!.hash;
  return { lucid, keyHash, address: proofMaterialsAddress("Custom", keyHash) };
};

/** The out-refs held at `address`, sorted. */
const materials = async (lucid: LucidEvolution, address: string) =>
  (await lucid.utxosAt(address)).map(outRef).sort();

describe("transition-trace carriage lookup", () => {
  it("publishes each chunk once, claims it, and reads it back by its claim", async () => {
    const { lucid, address } = await prover();
    const ownOutputs = createOwnOutputJournal();

    const published = await resolveTransitionTraceDataCarriage({
      lucid,
      address,
      datums: DATUMS,
      ownOutputs,
      publish: true,
    });
    const before = await materials(lucid, address);
    const read = await resolveTransitionTraceDataCarriage({
      lucid,
      address,
      datums: DATUMS,
      ownOutputs,
    });

    expect(published.map((utxo) => utxo.datum)).toEqual(DATUMS);
    expect(read.map(outRef)).toEqual(published.map(outRef));
    expect(await materials(lucid, address)).toEqual(before);
    expect(before).toHaveLength(2);
  }, 120_000);

  it("never takes another workflow's chunk with the same bytes", async () => {
    const { lucid, address } = await prover();
    const ours = await resolveTransitionTraceDataCarriage({
      lucid,
      address,
      datums: DATUMS,
      ownOutputs: createOwnOutputJournal(),
      publish: true,
    });

    // A second workflow of the same prover holds no claim on them.
    const theirs = createOwnOutputJournal();
    await expect(
      resolveTransitionTraceDataCarriage({
        lucid,
        address,
        datums: DATUMS,
        ownOutputs: theirs,
      }),
    ).rejects.toThrow("requires a journaled chunk publication");
    const own = await resolveTransitionTraceDataCarriage({
      lucid,
      address,
      datums: DATUMS,
      ownOutputs: theirs,
      publish: true,
    });

    expect(own.map((utxo) => utxo.datum)).toEqual(DATUMS);
    for (const utxo of own)
      expect(ours.map(outRef)).not.toContain(outRef(utxo));
  }, 120_000);

  it("publishes a chunk again once the output its claim names is spent", async () => {
    const { lucid, keyHash, address } = await prover();
    const ownOutputs = createOwnOutputJournal();
    const [first, second] = await resolveTransitionTraceDataCarriage({
      lucid,
      address,
      datums: DATUMS,
      ownOutputs,
      publish: true,
    });
    const spend = await (
      await spendProofMaterials(
        lucid.newTx().collectFrom([first!]),
        keyHash,
      ).complete()
    ).sign
      .withWallet()
      .complete();
    await lucid.awaitTx(await spend.submit());

    await expect(
      resolveTransitionTraceDataCarriage({
        lucid,
        address,
        datums: DATUMS,
        ownOutputs,
      }),
    ).rejects.toThrow("requires a journaled chunk publication");
    const again = await resolveTransitionTraceDataCarriage({
      lucid,
      address,
      datums: DATUMS,
      ownOutputs,
      publish: true,
    });

    expect(again[0]!.datum).toBe(DATUMS[0]);
    expect(outRef(again[0]!)).not.toBe(outRef(first!));
    expect(outRef(again[1]!)).toBe(outRef(second!));
  }, 120_000);
});
