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
  paymentCredentialOf,
  type TxSignBuilder,
  type UTxO,
} from "@lucid-evolution/lucid";
import { Effect, Either } from "effect";
import { describe, expect, it } from "vitest";

import {
  buildUnsignedMidgardCekStagingProgram,
  buildUnsignedMidgardCekStagingReclaimProgram,
  midgardCekStagingAddress,
  midgardCekStagingOutRef,
  MidgardCekStagingPins,
  midgardCekStagingResult,
  midgardCekStagingRunChunks,
  midgardCekStagingRunDatumCbor,
  planMidgardCekStaging,
} from "../src/fraud-proof/staging.js";

/**
 * A staged builtin step's operands are staged as outputs at the stager's
 * staging address, read back to the operands' roots, and reclaimed after
 * settle; a staging output a thread still pins is never reclaimed, and no
 * other wallet-funded transaction spends one.
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

/**
 * A stager wallet holding exactly `outputs`, each paid to it by a separate
 * funder account; the stager's wallet is selected.
 */
const stagerHolding = async (
  outputs: readonly { readonly lovelace: bigint; readonly datum?: string }[],
) => {
  const funder = generateEmulatorAccount({ lovelace: 2_000_000_000n });
  const stager = generateEmulatorAccount({ lovelace: 0n });
  const emulator = new Emulator([funder]);
  const lucid = await Lucid(emulator, "Custom");
  lucid.selectWallet.fromSeed(funder.seedPhrase);
  let tx = lucid.newTx();
  for (const { lovelace, datum } of outputs) {
    tx =
      datum === undefined
        ? tx.pay.ToAddress(stager.address, { lovelace })
        : tx.pay.ToAddressWithData(
            stager.address,
            { kind: "inline", value: datum },
            { lovelace },
          );
  }
  await submit(lucid, await tx.complete());
  lucid.selectWallet.fromSeed(stager.seedPhrase);
  return { funder, stager, lucid };
};

const lovelaceOf = (utxos: readonly UTxO[]): bigint =>
  utxos.reduce((sum, utxo) => sum + (utxo.assets["lovelace"] ?? 0n), 0n);

const stagingAddressOf = (walletAddress: string): string =>
  midgardCekStagingAddress("Custom", paymentCredentialOf(walletAddress).hash);

/** Stages `operands` one output a transaction; returns the staging result. */
const stage = async (lucid: LucidEvolution, operands: readonly Buffer[]) => {
  const plan = planMidgardCekStaging(operands);
  const refs: MidgardCekStagedOutRef[] = [];
  for (const run of plan) {
    const unsigned = await Effect.runPromise(
      buildUnsignedMidgardCekStagingProgram(lucid, { run }),
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
    const { plan, refs, staging } = await stage(lucid, operands);
    // Equal operands share their outputs: 5 chunks (2 runs) + 2 chunks (1 run).
    expect(plan).toHaveLength(3);
    expect(staging[2]).toEqual(staging[0]);

    const outputs = await lucid.utxosByOutRef(refs.map(outRefOf));
    expect(outputs).toHaveLength(3);
    const stagingAddress = stagingAddressOf(account.address);
    expect(stagingAddress).not.toBe(account.address);
    expect(outputs.every((output) => output.address === stagingAddress)).toBe(
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
    const before = lovelaceOf(await lucid.wallet().getUtxos());
    await submit(lucid, released.right);
    expect(await lucid.utxosByOutRef(refs.map(outRefOf))).toEqual([]);
    const returned = lovelaceOf(await lucid.wallet().getUtxos()) - before;
    expect(returned).toBeGreaterThan(lovelaceOf(outputs) - 1_000_000n);
  });

  it("refuses to reclaim a staging output a thread pins, and spends nothing", async () => {
    const { lucid } = await setup();
    const { refs, staging } = await stage(lucid, [operand(9_000, 3)]);
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
      .pay.ToAddressWithData(
        account.address,
        {
          kind: "inline",
          value: midgardCekStagingRunDatumCbor([operand(64, 4)]),
        },
        { lovelace: 3_000_000n },
      )
      .complete();
    await submit(lucid, unsigned);
    const wallet = await lucid.wallet().getUtxos();
    const withDatum = wallet.filter((utxo) => utxo.datum != null);
    const plain = wallet.filter((utxo) => utxo.datum == null);
    expect(withDatum).toHaveLength(2);
    for (const outputs of [...withDatum.map((utxo) => [utxo]), plain]) {
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

  it("keeps staging outputs out of the wallet, so low-balance transactions neither spend nor lose them", async () => {
    const { funder, stager, lucid } = await stagerHolding([
      { lovelace: 300_000_000n },
    ]);
    const { refs } = await stage(lucid, [operand(20_000, 5)]);
    const staged = await lucid.utxosByOutRef(refs.map(outRefOf));
    expect(staged).toHaveLength(2);
    const wallet = await lucid.wallet().getUtxos();
    const stagedKeys = new Set(
      staged.map((utxo) => `${utxo.txHash}#${utxo.outputIndex.toString()}`),
    );
    expect(
      wallet.some((utxo) =>
        stagedKeys.has(`${utxo.txHash}#${utxo.outputIndex.toString()}`),
      ),
    ).toBe(false);

    // Leave about 20 ADA of plain funding, below any staging output's value.
    await submit(
      lucid,
      await lucid
        .newTx()
        .pay.ToAddress(funder.address, {
          lovelace: lovelaceOf(wallet) - 20_000_000n,
        })
        .complete({ presetWalletInputs: wallet }),
    );
    for (const utxo of staged) {
      expect(utxo.assets["lovelace"]).toBeGreaterThan(20_000_000n);
    }

    // An unrelated transaction with default coin selection, only just funded.
    await submit(
      lucid,
      await lucid
        .newTx()
        .pay.ToAddress(funder.address, { lovelace: 16_000_000n })
        .complete(),
    );
    expect(await lucid.utxosByOutRef(refs.map(outRefOf))).toHaveLength(2);

    // A step-shaped transaction still references every staging output.
    await submit(
      lucid,
      await lucid
        .newTx()
        .readFrom(staged)
        .pay.ToAddress(funder.address, { lovelace: 1_000_000n })
        .complete(),
    );
    const after = await lucid.utxosByOutRef(refs.map(outRefOf));
    expect(after).toHaveLength(2);
    expect(
      after.every((utxo) => utxo.address === stagingAddressOf(stager.address)),
    ).toBe(true);
  });

  it("funds staging from plain-Ada outputs only", async () => {
    const other = Data.to(new Constr(0, [["ab", 1n]]));
    const { stager, lucid } = await stagerHolding([
      { lovelace: 300_000_000n, datum: other },
      { lovelace: 25_000_000n },
    ]);
    const [run] = planMidgardCekStaging([operand(5_000, 6)]);
    const unsigned = await Effect.runPromise(
      buildUnsignedMidgardCekStagingProgram(lucid, { run: run! }),
    );
    await submit(lucid, unsigned);
    const wallet = await lucid.wallet().getUtxos();
    expect(wallet.filter((utxo) => utxo.datum === other)).toHaveLength(1);
    expect(
      await lucid.utxosByOutRef([
        outRefOf(midgardCekStagingOutRef(unsigned, run!)),
      ]),
    ).toEqual([
      expect.objectContaining({ address: stagingAddressOf(stager.address) }),
    ]);
  });

  it("refuses to stage from a wallet with no plain-Ada outputs", async () => {
    const { lucid } = await stagerHolding([
      {
        lovelace: 300_000_000n,
        datum: Data.to(new Constr(0, [["ab", 1n]])),
      },
    ]);
    const [run] = planMidgardCekStaging([operand(5_000, 7)]);
    const refused = await Effect.runPromise(
      Effect.either(
        buildUnsignedMidgardCekStagingProgram(lucid, { run: run! }),
      ),
    );
    expect(Either.isLeft(refused)).toBe(true);
    if (Either.isLeft(refused)) {
      expect(refused.left.message).toContain(
        "staging requires plain-Ada funding",
      );
    }
  });
});
