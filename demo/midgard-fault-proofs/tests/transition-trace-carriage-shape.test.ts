/**
 * Which outputs at the prover's proof-materials address the transition-trace
 * carriage lookup takes as a chunk's carriage: the plain output holding the
 * chunk's bytes, found by content.
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

import { isPrerequisiteTurn } from "../src/fault-proof-turn.js";
import {
  proofMaterialsAddress,
  spendProofMaterials,
} from "../src/funding/proof-materials-address.js";
import { resolveTransitionTraceDataCarriage } from "../src/transition-trace/proof-carriage.js";
import { EMULATOR_PROTOCOL_PARAMETERS } from "./support/emulator/protocol-parameters.js";
import { settleTurnsUnjournaled } from "./support/submit-unjournaled.js";

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

type CarriageArgs = Omit<
  Parameters<typeof resolveTransitionTraceDataCarriage>[0],
  "publish"
>;

/**
 * The lookup with publication: each chunk it returns as a publication turn is
 * submitted and confirmed before it is called again.
 */
const publishing = async (args: CarriageArgs) =>
  await settleTurnsUnjournaled(resolveTransitionTraceDataCarriage, {
    ...args,
    publish: true,
  });

/** The lookup without publication, which never returns a turn. */
const reading = async (args: CarriageArgs) => {
  const carriage = await resolveTransitionTraceDataCarriage(args);
  if (isPrerequisiteTurn(carriage))
    throw new Error(`carriage lookup returned ${carriage.label}`);
  return carriage;
};

/** The out-refs held at `address`, sorted. */
const materials = async (lucid: LucidEvolution, address: string) =>
  (await lucid.utxosAt(address)).map(outRef).sort();

describe("transition-trace carriage lookup", () => {
  it("returns a missing chunk as a publication turn and submits nothing", async () => {
    const { lucid, address } = await prover();

    const turn = await resolveTransitionTraceDataCarriage({
      lucid,
      address,
      datums: DATUMS,
      publish: true,
    });
    if (!isPrerequisiteTurn(turn))
      throw new Error("carriage lookup published nothing for the first chunk");

    expect(turn.kind).toBe("publication");
    expect(turn.plan.transactions).toBe(DATUMS.length);
    expect(await materials(lucid, address)).toEqual([]);
  }, 120_000);

  it("publishes each chunk once and reads it back by its content", async () => {
    const { lucid, address } = await prover();

    const published = await publishing({
      lucid,
      address,
      datums: DATUMS,
    });
    const before = await materials(lucid, address);
    const read = await reading({
      lucid,
      address,
      datums: DATUMS,
    });

    expect(published.map((utxo) => utxo.datum)).toEqual(DATUMS);
    expect(read.map(outRef)).toEqual(published.map(outRef));
    expect(await materials(lucid, address)).toEqual(before);
    expect(before).toHaveLength(2);
  }, 120_000);

  it("publishes a chunk again once its output is spent", async () => {
    const { lucid, keyHash, address } = await prover();
    const [first, second] = await publishing({
      lucid,
      address,
      datums: DATUMS,
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
      }),
    ).rejects.toThrow("requires a published chunk");
    const again = await publishing({
      lucid,
      address,
      datums: DATUMS,
    });

    expect(again[0]!.datum).toBe(DATUMS[0]);
    expect(outRef(again[0]!)).not.toBe(outRef(first!));
    expect(outRef(again[1]!)).toBe(outRef(second!));
  }, 120_000);
});
