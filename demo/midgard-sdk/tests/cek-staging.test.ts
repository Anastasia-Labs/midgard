import {
  bindMidgardCekStagedOperand,
  collectMidgardCekStagedRuns,
  compareMidgardCekStagedOutRefs,
  midgardCekFlatRoot,
  type MidgardCekStagedOutRef,
} from "@al-ft/midgard-core";
import {
  Constr,
  Data,
  Emulator,
  generateEmulatorAccount,
  Lucid,
  type LucidEvolution,
  type OutRef,
  type TxSignBuilder,
  type UTxO,
} from "@lucid-evolution/lucid";
import { Effect, Either } from "effect";
import { describe, expect, it } from "vitest";

import {
  buildUnsignedMidgardCekStagingProgram,
  buildUnsignedMidgardCekStagingReclaimProgram,
  midgardCekStagingOutRef,
  MidgardCekStagingPins,
  midgardCekStagingResult,
  midgardCekStagingRunChunks,
  planMidgardCekStaging,
} from "../src/fraud-proof/staging.js";

/**
 * A staged builtin step's operands are staged as outputs at the stager's own
 * address, read back to the operands' roots, and reclaimed after settle; a
 * staging output a thread still pins is never reclaimed.
 */

const operand = (length: number, salt: number): Buffer =>
  Buffer.from(
    Uint8Array.from({ length }, (_, index) => (index * 13 + salt) & 0xff),
  );

const setup = async () => {
  const account = generateEmulatorAccount({ lovelace: 2_000_000_000n });
  const emulator = new Emulator([account]);
  const lucid = await Lucid(emulator, "Custom");
  lucid.selectWallet.fromSeed(account.seedPhrase);
  return { account, lucid };
};

const submit = async (lucid: LucidEvolution, unsigned: TxSignBuilder) => {
  const signed = await unsigned.sign.withWallet().complete();
  await lucid.awaitTx(await signed.submit());
};

/** Stages `operands` one output a transaction; returns the staging result. */
const stage = async (
  lucid: LucidEvolution,
  stagerAddress: string,
  operands: readonly Buffer[],
) => {
  const plan = planMidgardCekStaging(operands);
  const refs: MidgardCekStagedOutRef[] = [];
  for (const run of plan) {
    const unsigned = await Effect.runPromise(
      buildUnsignedMidgardCekStagingProgram(lucid, { run, stagerAddress }),
    );
    refs.push(midgardCekStagingOutRef(unsigned, run));
    await submit(lucid, unsigned);
  }
  return { plan, refs, staging: midgardCekStagingResult(operands, plan, refs) };
};

const outRefOf = (ref: MidgardCekStagedOutRef): OutRef => ({
  txHash: Buffer.from(ref.transactionId).toString("hex"),
  outputIndex: ref.outputIndex,
});

const reclaim = (
  lucid: LucidEvolution,
  outputs: readonly UTxO[],
  pins: MidgardCekStagingPins,
) =>
  Effect.runPromise(
    Effect.either(
      buildUnsignedMidgardCekStagingReclaimProgram(lucid, { outputs, pins }),
    ),
  );

describe("CEK operand staging", () => {
  it("stages operands that read back to their roots, then reclaims them once released", async () => {
    const { account, lucid } = await setup();
    const large = operand(20_000, 1);
    const small = operand(5_000, 2);
    const operands = [large, small, large];
    const { plan, refs, staging } = await stage(
      lucid,
      account.address,
      operands,
    );
    // Equal operands share their outputs: 5 chunks (2 runs) + 2 chunks (1 run).
    expect(plan).toHaveLength(3);
    expect(staging[2]).toEqual(staging[0]);

    const outputs = await lucid.utxosByOutRef(refs.map(outRefOf));
    expect(outputs).toHaveLength(3);
    expect(outputs.every((output) => output.address === account.address)).toBe(
      true,
    );
    const ordered = [...refs].sort(compareMidgardCekStagedOutRefs);
    const referenceInputs = ordered.map((ref) => {
      const output = outputs.find(
        (utxo) =>
          utxo.txHash === outRefOf(ref).txHash &&
          utxo.outputIndex === ref.outputIndex,
      )!;
      return { outRef: ref, chunks: midgardCekStagingRunChunks(output.datum!) };
    });
    const runs = collectMidgardCekStagedRuns(referenceInputs, ordered);
    operands.forEach((bytes, index) => {
      const indices = staging[index]!.map((ref) =>
        ordered.findIndex(
          (candidate) => compareMidgardCekStagedOutRefs(candidate, ref) === 0,
        ),
      );
      const bound = bindMidgardCekStagedOperand(runs, indices);
      expect(Buffer.from(bound.root)).toEqual(
        Buffer.from(midgardCekFlatRoot(bytes)),
      );
      expect(bound.length).toBe(bytes.length);
    });

    const pins = new MidgardCekStagingPins();
    pins.pin("thread-a", staging[0]!);
    pins.pin("thread-b", staging[1]!);
    pins.release("thread-a");
    const refused = await reclaim(lucid, outputs, pins);
    expect(Either.isLeft(refused)).toBe(true);
    if (Either.isLeft(refused)) {
      expect(refused.left.message).toContain("pinned by thread thread-b");
    }
    expect(await lucid.utxosByOutRef(refs.map(outRefOf))).toHaveLength(3);

    pins.release("thread-b");
    const released = await reclaim(lucid, outputs, pins);
    expect(Either.isRight(released)).toBe(true);
    if (Either.isLeft(released)) return;
    await submit(lucid, released.right);
    expect(await lucid.utxosByOutRef(refs.map(outRefOf))).toEqual([]);
  });

  it("refuses to reclaim a staging output a thread pins, and spends nothing", async () => {
    const { account, lucid } = await setup();
    const { refs, staging } = await stage(lucid, account.address, [
      operand(9_000, 3),
    ]);
    const pins = new MidgardCekStagingPins([["thread-c", staging[0]!]]);
    const restored = new MidgardCekStagingPins(pins.entries());
    const outputs = await lucid.utxosByOutRef(refs.map(outRefOf));
    for (const pinSet of [pins, restored]) {
      const refused = await reclaim(lucid, outputs, pinSet);
      expect(Either.isLeft(refused)).toBe(true);
      if (Either.isLeft(refused)) {
        expect(refused.left.message).toContain("pinned by thread thread-c");
      }
    }
    expect(await lucid.utxosByOutRef(refs.map(outRefOf))).toHaveLength(1);
  });

  it("refuses to reclaim an output that is not a staging output", async () => {
    const { account, lucid } = await setup();
    const unsigned = await lucid
      .newTx()
      .pay.ToAddressWithData(
        account.address,
        { kind: "inline", value: Data.to(new Constr(0, [["ab", 1n]])) },
        { lovelace: 3_000_000n },
      )
      .complete();
    await submit(lucid, unsigned);
    const wallet = await lucid.wallet().getUtxos();
    const withDatum = wallet.filter((utxo) => utxo.datum != null);
    const plain = wallet.filter((utxo) => utxo.datum == null);
    expect(withDatum).toHaveLength(1);
    for (const outputs of [withDatum, plain]) {
      const refused = await reclaim(
        lucid,
        outputs,
        new MidgardCekStagingPins(),
      );
      expect(Either.isLeft(refused)).toBe(true);
      if (Either.isLeft(refused)) {
        expect(refused.left.message).toContain("is not a staging output");
      }
    }
  });
});
