import { Store, Trie } from "@aiken-lang/merkle-patricia-forestry";
import {
  encodeMidgardFieldPreimage,
  encodeMidgardSpendInputItem,
  type MidgardNativeTxFull,
} from "@al-ft/midgard-core";
import { forcedVerdictSubject } from "@al-ft/midgard-sdk/fraud-proof";
import { expect } from "vitest";

import {
  prepareResolvedOutputAbsentInputEvidence,
  type ResolvedOutputAbsentInputEvidence,
} from "../../src/resolved-output-non-canonical/resolved-output-non-canonical.prepare-resolved-output-non-canonical-evidence.js";
import { makeNativeTx } from "./emulator/native-tx.js";
import {
  signedKeyOutput,
  signNativeTx,
} from "./forced-reason-signed-native-tx.js";
import {
  buildSubjectTransaction,
  descriptorFor,
  makeResolvedOutputContext,
  type PriorLedgerFixture,
  smallMalformedOutput,
} from "./resolved-output-non-canonical-emulator.build-prior-ledger.js";
import { commitBlock } from "./resolved-output-non-canonical-emulator.commit-block.js";
import { makeResolvedOutputStages } from "./resolved-output-non-canonical-emulator.make-resolved-output-stages.js";

/**
 * Fixtures for forced InputSpentOutputNonCanonical coordinates: a prior
 * ledger holding a canonical and a non-canonical output, transactions that
 * name both from the spend field and from the reference field, and the
 * stage drivers an absent-input proof runs against a committed forced leaf.
 */

export const priorEntries = [
  { txId: "11".repeat(32), output: signedKeyOutput() },
  { txId: "22".repeat(32), output: smallMalformedOutput() },
] as const;

export const outRefOf = (txId: string): Buffer =>
  encodeMidgardSpendInputItem({
    txId: Buffer.from(txId, "hex"),
    outputIndex: 0,
  });

/** Field items in canonical (sorted) order: the canonical output first. */
export const inputItems = priorEntries.map(({ txId }) => outRefOf(txId));

/** The prior ledger holding both outputs, opened on input `named`'s output. */
export const priorLedgerOf = async (
  named: number,
): Promise<PriorLedgerFixture> => {
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

export const shapes = [
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

/** A committed forced leaf with the stage runners a proof drives against it. */
export type CommittedCoordinate = Awaited<
  ReturnType<typeof commitForcedCoordinate>
>;

/** The rejection's subject over the committed forced leaf. */
export const forcedSubjectOf = ({
  block,
}: Pick<CommittedCoordinate, "block">) => {
  if (block.forced === undefined) throw new Error("block is not forced");
  return forcedVerdictSubject({
    transactionId: block.nativeTxId,
    sourceKey: block.forced.membership.key,
    rejectionReason: block.forced.reason,
  });
};

/** A block whose forced leaf rejects `nativeTx` for `coordinate`. */
export const commitForcedCoordinate = async (
  nativeTx: MidgardNativeTxFull,
  coordinate: {
    readonly sourceKind: bigint | number;
    readonly inputIndex: bigint | number;
  },
) => {
  const context = await makeResolvedOutputContext();
  const prior = await priorLedgerOf(0);
  const block = await commitBlock({
    context,
    nativeTx,
    priorRoot: prior.priorRoot,
    reason: {
      InputSpentOutputNonCanonical: {
        source_kind: BigInt(coordinate.sourceKind),
        input_index: BigInt(coordinate.inputIndex),
      },
    },
  });
  const stages = await makeResolvedOutputStages(context, block);
  return { context, prior, block, stages };
};

/**
 * Drives `evidence` from Init through step 02 and returns step 02's capture.
 */
export const authenticateAbsent = async (
  stages: CommittedCoordinate["stages"],
  evidence: ResolvedOutputAbsentInputEvidence,
) => {
  const bound = await stages.step01Forced(
    stages.threadOf(await stages.init()),
    evidence,
  );
  return await stages.step02(bound.result.nextThreadOutRef, evidence);
};

/** Convicts with the absent verdict and removes the block. */
export const convictAbsent = async (
  {
    context,
    block,
    stages,
  }: Pick<CommittedCoordinate, "context" | "block" | "stages">,
  evidence: ResolvedOutputAbsentInputEvidence,
) => {
  const opened = await authenticateAbsent(stages, evidence);
  expect(opened.result.nextStage).toBe("step05");
  const minted = await stages.step05(opened.result.nextThreadOutRef, evidence);
  expect(minted.result.fraudProofUnit).toBeTruthy();
  await stages.remove();
  const [txHash, outputIndex] = block.fraudulentBlockOutRef.split("#");
  expect(
    await context.harness.proverLucid.utxosByOutRef([
      { txHash: txHash!, outputIndex: Number(outputIndex) },
    ]),
  ).toHaveLength(0);
  return { opened, minted };
};

export const absentEvidenceOf = (
  scenario: Pick<CommittedCoordinate, "block" | "prior">,
) =>
  prepareResolvedOutputAbsentInputEvidence({
    subject: forcedSubjectOf(scenario),
    canonicalTransactionCbor: scenario.block.canonicalCbor,
    priorRoot: scenario.prior.priorRoot,
  });

/** The most distinct inputs a field capped at `capBytes` holds. */
export const maxShapeInputs = (capBytes: number): readonly Buffer[] => {
  const inputs: Buffer[] = [];
  for (;;) {
    const next = outRefOf(inputs.length.toString(16).padStart(64, "0"));
    if (encodeMidgardFieldPreimage([...inputs, next]).length > capBytes)
      return inputs;
    inputs.push(next);
  }
};
