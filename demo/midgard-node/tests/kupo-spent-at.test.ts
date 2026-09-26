import { compareOutRefs } from "@al-ft/midgard-core/out-ref";
import * as SDK from "@al-ft/midgard-sdk";
import {
  Data,
  Emulator,
  generateEmulatorAccount,
  Lucid as makeLucid,
  type LucidEvolution,
  PROTOCOL_PARAMETERS_DEFAULT,
  type TxSignBuilder,
  type UTxO,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { afterAll, afterEach, beforeAll, describe, expect, it } from "vitest";

import {
  fetchKupoAncestorPoint,
  fetchKupoSpend,
  readOgmiosBlockTransaction,
} from "../src/l1-tx-order-carriage.js";
import { AlwaysSucceedsContract } from "../src/services/always-succeeds.js";
import {
  type KupoSpentAt,
  type LocalL1,
  type LocalL1Block,
  startLocalL1Observation,
} from "./helpers/local-l1-observation.js";

/**
 * What the node reads out of Kupo's `spent_at`, against a real Plutus spend.
 *
 * A live preprod node's correction observer failed every pass with "Kupo
 * spent_at.redeemer is not base16 data": Kupo v2.11.0 served `redeemer: null` for
 * the scheduler UTxO, which `AppointFirstOperator` spent through its script with
 * spend redeemer 1. The v2.11.0 schema makes the field nullable, and Kupo's
 * `matchBlock` attributes a transaction's inputs in mirrored order — the input at
 * ledger position `i` of `n` is reported at `n - 1 - i` and handed the redeemer
 * found there — so the null was a *key* input's empty redeemer slot.
 *
 * Here the spend is real: an always-succeeds script output and a key output, both
 * created and then spent together by transactions built and submitted on the
 * Lucid emulator, and served back by the local L1 harness, whose Kupo reproduces
 * v2.11.0's attribution from the spending transaction's own CBOR. So the script
 * spend reads back `redeemer: null` — the live shape — and the key spend reads
 * back the script's redeemer. The read must accept Kupo's schema, surface neither
 * of those values, and leave the redeemer to the transaction itself; and it must
 * still refuse a `spent_at` that is not Kupo's schema, at the check that names it.
 */

const SPEND_REDEEMER = Data.to(7n);

type Harness = {
  readonly lucid: LucidEvolution;
  readonly emulator: Emulator;
  readonly l1: LocalL1;
  readonly scriptOutput: UTxO;
  readonly keyOutput: UTxO;
  readonly spendTxHash: string;
  readonly spendBlock: LocalL1Block;
};

const submitAndObserve = async (
  emulator: Emulator,
  l1: LocalL1,
  tx: TxSignBuilder,
): Promise<{ readonly txHash: string; readonly block: LocalL1Block }> => {
  const signed = await tx.sign.withWallet().complete();
  const cbor = signed.toCBOR();
  await signed.submit();
  emulator.awaitBlock(1);
  return { txHash: signed.toHash(), block: l1.appendBlock([cbor]) };
};

const makeHarness = async (): Promise<Harness> => {
  const creator = generateEmulatorAccount({ lovelace: 1_000_000_000n });
  const emulator = new Emulator([creator], PROTOCOL_PARAMETERS_DEFAULT);
  const lucid = await makeLucid(emulator, "Custom");
  lucid.selectWallet.fromSeed(creator.seedPhrase);
  const creatorAddress = await lucid.wallet().address();
  const contracts = await Effect.runPromise(
    Effect.gen(function* () {
      return yield* AlwaysSucceedsContract;
    }).pipe(Effect.provide(AlwaysSucceedsContract.Default)),
  );
  const validator = (contracts as unknown as SDK.MidgardValidators).scheduler;
  const l1 = await startLocalL1Observation();

  const created = await submitAndObserve(
    emulator,
    l1,
    await lucid
      .newTx()
      .pay.ToContract(
        validator.spendingScriptAddress,
        { kind: "inline", value: Data.void() },
        { lovelace: 5_000_000n },
      )
      .pay.ToAddress(creatorAddress, { lovelace: 7_000_000n })
      .complete({ localUPLCEval: true }),
  );
  const [scriptOutput, keyOutput] = await lucid.utxosByOutRef([
    { txHash: created.txHash, outputIndex: 0 },
    { txHash: created.txHash, outputIndex: 1 },
  ]);
  if (scriptOutput === undefined || keyOutput === undefined) {
    throw new Error("the emulator did not index the created outputs");
  }

  const spent = await submitAndObserve(
    emulator,
    l1,
    await lucid
      .newTx()
      .collectFrom([scriptOutput], SPEND_REDEEMER)
      .collectFrom([keyOutput])
      .attach.SpendingValidator(validator.spendingScript)
      .complete({ localUPLCEval: true }),
  );
  return {
    lucid,
    emulator,
    l1,
    scriptOutput,
    keyOutput,
    spendTxHash: spent.txHash,
    spendBlock: spent.block,
  };
};

/** Kupo's raw match for an output, exactly as the reader requests it. */
const rawSpentAt = async (l1: LocalL1, utxo: UTxO): Promise<unknown> => {
  const response = await fetch(
    `${l1.kupoUrl}/matches/${utxo.outputIndex.toString()}@${utxo.txHash}?resolve_hashes`,
  );
  const [match] = (await response.json()) as { spent_at: unknown }[];
  return match?.spent_at;
};

describe("Kupo spent_at, read off a real Plutus spend", () => {
  let harness: Harness;

  beforeAll(async () => {
    harness = await makeHarness();
  }, 120_000);

  afterAll(async () => {
    await harness?.l1.close();
  });

  afterEach(() => {
    harness.l1.rewriteSpentAt(null);
  });

  it("serves Kupo v2.11.0's attribution: null on the script spend, the script's redeemer on the key spend", async () => {
    // Guards the double, not the reader: if the harness stopped reproducing the
    // live shape, the positives below would pass without exercising it.
    const observed = await readOgmiosBlockTransaction({
      ogmiosUrl: harness.l1.ogmiosUrl,
      intersection: await fetchKupoAncestorPoint({
        kupoUrl: harness.l1.kupoUrl,
        slot: harness.spendBlock.slot,
      }),
      blockPoint: harness.spendBlock,
      txHash: harness.spendTxHash,
    });
    const ledgerInputs = [...(observed.spentInputs ?? [])].sort(compareOutRefs);
    expect(ledgerInputs).toHaveLength(2);
    const scriptPointer = ledgerInputs.findIndex(
      (input) =>
        input.txHash === harness.scriptOutput.txHash &&
        input.outputIndex === harness.scriptOutput.outputIndex,
    );
    expect(scriptPointer).not.toBe(-1);

    expect(await rawSpentAt(harness.l1, harness.scriptOutput)).toMatchObject({
      transaction_id: harness.spendTxHash,
      input_index: 1 - scriptPointer,
      redeemer: null,
    });
    expect(await rawSpentAt(harness.l1, harness.keyOutput)).toMatchObject({
      transaction_id: harness.spendTxHash,
      input_index: scriptPointer,
      redeemer: SPEND_REDEEMER,
    });
  });

  it("reads the spend of a script output whose Kupo redeemer is null", async () => {
    await expect(
      fetchKupoSpend({
        kupoUrl: harness.l1.kupoUrl,
        outRef: harness.scriptOutput,
      }),
    ).resolves.toStrictEqual({
      point: {
        slot: harness.spendBlock.slot,
        headerHash: harness.spendBlock.headerHash,
      },
      transactionId: harness.spendTxHash,
    });
  });

  it("surfaces neither Kupo's input_index nor its redeemer for a key spend Kupo credits with one", async () => {
    await expect(
      fetchKupoSpend({
        kupoUrl: harness.l1.kupoUrl,
        outRef: harness.keyOutput,
      }),
    ).resolves.toStrictEqual({
      point: {
        slot: harness.spendBlock.slot,
        headerHash: harness.spendBlock.headerHash,
      },
      transactionId: harness.spendTxHash,
    });
  });

  it("finds the script's redeemer on the spending transaction, at the ledger's pointer", async () => {
    const spend = await fetchKupoSpend({
      kupoUrl: harness.l1.kupoUrl,
      outRef: harness.scriptOutput,
    });
    expect(spend).not.toBeNull();
    const observed = await readOgmiosBlockTransaction({
      ogmiosUrl: harness.l1.ogmiosUrl,
      intersection: await fetchKupoAncestorPoint({
        kupoUrl: harness.l1.kupoUrl,
        slot: spend!.point.slot,
      }),
      blockPoint: spend!.point,
      txHash: spend!.transactionId,
    });
    const scriptPointer = [...(observed.spentInputs ?? [])]
      .sort(compareOutRefs)
      .findIndex(
        (input) =>
          input.txHash === harness.scriptOutput.txHash &&
          input.outputIndex === harness.scriptOutput.outputIndex,
      );
    expect(
      observed.redeemers.filter(({ purpose }) => purpose === "spend"),
    ).toEqual([
      { purpose: "spend", index: scriptPointer, redeemer: SPEND_REDEEMER },
    ]);
  });

  it.each<[string, (spentAt: KupoSpentAt) => unknown, string]>([
    [
      "a redeemer that is not a string",
      (spentAt) => ({ ...spentAt, redeemer: 7 }),
      "Kupo spent_at.redeemer is not base16 data",
    ],
    [
      "a redeemer that is not base16",
      (spentAt) => ({ ...spentAt, redeemer: "d8798" }),
      "Kupo spent_at.redeemer is not base16 data",
    ],
    [
      "a redeemer that is empty",
      (spentAt) => ({ ...spentAt, redeemer: "" }),
      "Kupo spent_at.redeemer is not base16 data",
    ],
    [
      // Schema-legal for a row indexed before Kupo's v2.7.0 migration, and still
      // no spend the observer can locate.
      "a null transaction id",
      (spentAt) => ({ ...spentAt, transaction_id: null }),
      "Kupo spent_at.transaction_id is not a transaction id",
    ],
    [
      "a negative input index",
      (spentAt) => ({ ...spentAt, input_index: -1 }),
      "Kupo spent_at.input_index is not an input index",
    ],
  ])("refuses a spent_at with %s", async (_label, rewrite, message) => {
    harness.l1.rewriteSpentAt(rewrite);
    await expect(
      fetchKupoSpend({
        kupoUrl: harness.l1.kupoUrl,
        outRef: harness.scriptOutput,
      }),
    ).rejects.toThrow(message);
  });
});
