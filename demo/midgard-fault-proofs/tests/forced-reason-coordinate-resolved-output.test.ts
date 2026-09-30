import { Store, Trie } from "@aiken-lang/merkle-patricia-forestry";
import {
  computeMidgardNativeTxId,
  encodeMidgardSpendInputItem,
  type MidgardNativeTxFull,
} from "@al-ft/midgard-core";
import {
  encodeMidgardForcedTxCanonical,
  materializeMidgardForcedTxFromCanonical,
} from "@al-ft/midgard-core/codec/forced";
import { describe, expect, it } from "vitest";

import { expectOnchainRefusal } from "./support/emulator/expect-onchain-refusal.js";
import { makeNativeTx } from "./support/emulator/native-tx.js";
import { nodeForcedVerdict } from "./support/emulator/node-forced-verdict.js";
import {
  signedKeyOutput,
  signNativeTx,
} from "./support/forced-reason-signed-native-tx.js";
import {
  buildSubjectTransaction,
  commitBlock,
  descriptorFor,
  makeResolvedOutputContext,
  makeResolvedOutputStages,
  type PriorLedgerFixture,
  resolvedOutputEvidence,
  resolvedOutputReason,
  smallMalformedOutput,
} from "./support/resolved-output-non-canonical-emulator.js";

/**
 * A forced InputSpentOutputNonCanonical reason names an input by source kind
 * and field position, and resolvedOutputNonCanonical reopens the prior-ledger
 * output that input resolves to. Input 0 resolves to a canonical output and
 * input 1 to a non-canonical one, in the spend field and in the reference
 * field. The verdict is the one the node's classifier writes over that
 * ledger, so the suite fails if the writer and the proof disagree on the
 * source kind or on how inputs are counted: one position early names the
 * canonical output and convicts; the written position is refused on chain.
 */

const priorEntries = [
  { txId: "11".repeat(32), output: signedKeyOutput() },
  { txId: "22".repeat(32), output: smallMalformedOutput() },
] as const;

const outRefOf = (txId: string): Buffer =>
  encodeMidgardSpendInputItem({
    txId: Buffer.from(txId, "hex"),
    outputIndex: 0,
  });

/** Field items in canonical (sorted) order: the canonical output first. */
const inputItems = priorEntries.map(({ txId }) => outRefOf(txId));

/** The prior ledger holding both outputs, opened on input `named`'s output. */
const priorLedgerOf = async (named: number): Promise<PriorLedgerFixture> => {
  const store = new Store(undefined);
  await store.ready();
  const trie = new Trie(store);
  const entries = priorEntries.map(({ txId, output }) => ({
    txId,
    output,
    key: outRefOf(txId),
    descriptor: descriptorFor(0, output),
  }));
  for (const entry of entries) await trie.insert(entry.key, entry.descriptor);
  const self = entries[named]!;
  const sibling = entries[1 - named]!;
  return {
    priorRoot: Buffer.from(trie.hash).toString("hex"),
    priorTxId: self.txId,
    outputIndex: 0,
    outRefBytes: self.key,
    output: self.output,
    descriptorCbor: self.descriptor,
    proofCborHex: (await trie.prove(self.key)).toCBOR().toString("hex"),
    siblingProofCborHex: (await trie.prove(sibling.key))
      .toCBOR()
      .toString("hex"),
    siblingKeyBytes: sibling.key,
    siblingDescriptorCbor: sibling.descriptor,
  };
};

const shapes = [
  {
    label: "spend",
    sourceKind: 0,
    // Signed, so Phase B passes input 0 and reaches input 1.
    nativeTx: signNativeTx(
      makeNativeTx({ spendInputCbors: inputItems, fee: 7n, outputCbors: [] }),
    ),
  },
  {
    label: "reference",
    sourceKind: 1,
    // Reference inputs resolve before any spend input is checked.
    nativeTx: buildSubjectTransaction({
      spendInputCbors: [outRefOf("56".repeat(32))],
      referenceInputCbors: inputItems,
    }),
  },
] as const satisfies readonly {
  label: string;
  sourceKind: 0 | 1;
  nativeTx: MidgardNativeTxFull;
}[];

const writtenInputIndex = async (
  shape: (typeof shapes)[number],
): Promise<number> => {
  const forced = materializeMidgardForcedTxFromCanonical(shape.nativeTx);
  const verdict = await nodeForcedVerdict({
    transactionId: computeMidgardNativeTxId(forced),
    forcedCanonicalCbor: encodeMidgardForcedTxCanonical(forced),
    ledger: priorEntries.map(({ txId, output }) => [outRefOf(txId), output]),
  });
  expect(verdict).toStrictEqual({
    ForcedTxInvalid: {
      reason: resolvedOutputReason({
        sourceKind: shape.sourceKind,
        inputIndex: 1,
      }),
    },
  });
  return 1;
};

/**
 * A block whose forced leaf rejects the shape's transaction with
 * `InputSpentOutputNonCanonical { sourceKind, written + offset }`, and the
 * stage runners a resolvedOutputNonCanonical proof drives against it.
 */
const setupScenario = async (
  shape: (typeof shapes)[number],
  offset: number,
) => {
  const inputIndex = (await writtenInputIndex(shape)) + offset;
  const context = await makeResolvedOutputContext();
  const prior = await priorLedgerOf(inputIndex);
  const coordinate = { sourceKind: shape.sourceKind, inputIndex };
  const block = await commitBlock({
    context,
    nativeTx: shape.nativeTx,
    priorRoot: prior.priorRoot,
    reason: resolvedOutputReason(coordinate),
  });
  expect(block.nativeTxId).toBe(
    computeMidgardNativeTxId(
      materializeMidgardForcedTxFromCanonical(shape.nativeTx),
    ).toString("hex"),
  );
  const stages = await makeResolvedOutputStages(context, block);
  return { context, prior, coordinate, block, stages };
};

for (const shape of shapes) {
  describe(`forced ${shape.label} InputSpentOutputNonCanonical coordinate the node writes`, () => {
    it("convicts a coordinate one position early, where the resolved output is canonical", async () => {
      const { context, prior, coordinate, block, stages } = await setupScenario(
        shape,
        -1,
      );
      const evidence = resolvedOutputEvidence({ block, prior, coordinate });
      expect(evidence.outputIsNonCanonical).toBe(false);
      const bound = await stages.step01Forced(
        stages.threadOf(await stages.init()),
        evidence,
      );
      const opened = await stages.step02(
        bound.result.nextThreadOutRef,
        evidence,
      );
      const staged = await stages.step03(
        opened.result.nextThreadOutRef,
        evidence,
      );
      const walked = await stages.reconstruct(
        staged.result.nextThreadOutRef,
        evidence,
      );
      expect(walked.final.action).toBe("finalize");
      const minted = await stages.step05(walked.threadOutRef, evidence);
      expect(minted.result.fraudProofUnit).toBeTruthy();
      await stages.remove();
      const [txHash, outputIndex] = block.fraudulentBlockOutRef.split("#");
      expect(
        await context.harness.proverLucid.utxosByOutRef([
          { txHash: txHash!, outputIndex: Number(outputIndex) },
        ]),
      ).toHaveLength(0);
    }, 900_000);

    it("refuses the written coordinate on chain", async () => {
      const { prior, coordinate, block, stages } = await setupScenario(
        shape,
        0,
      );
      // The written input resolves to the non-canonical output: the
      // rejection holds, and the prover's planner refuses to contradict it.
      expect(() =>
        resolvedOutputEvidence({ block, prior, coordinate }),
      ).toThrow(/agrees with the operator verdict/u);
      // Every step before the terminal one runs on what a lying prover
      // holds: the evidence planned under the contradicting subject.
      const lying = resolvedOutputEvidence({
        block,
        prior,
        coordinate,
        claim: "lying",
      });
      expect(lying.outputIsNonCanonical).toBe(true);
      const bound = await stages.step01Forced(
        stages.threadOf(await stages.init()),
        lying,
      );
      const opened = await stages.step02(bound.result.nextThreadOutRef, lying);
      const staged = await stages.step03(opened.result.nextThreadOutRef, lying);
      const walked = await stages.reconstruct(
        staged.result.nextThreadOutRef,
        lying,
      );
      // The step authenticates the reconstructed verdict and the terminal
      // rule, which convicts only a verdict the reason contradicts, returns
      // false.
      await expectOnchainRefusal(
        () => stages.step05Raw(walked.threadOutRef),
        /^Validator returned false$/u,
      );
    }, 900_000);
  });
}
