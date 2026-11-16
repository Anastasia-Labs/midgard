import { Store, Trie } from "@aiken-lang/merkle-patricia-forestry";
import {
  computeMidgardNativeTxId,
  encodeMidgardFieldPreimage,
  encodeMidgardSpendInputItem,
  MIDGARD_CONSENSUS_LIMITS,
  type MidgardNativeTxFull,
} from "@al-ft/midgard-core";
import {
  encodeMidgardForcedTxCanonical,
  materializeMidgardForcedTxFromCanonical,
} from "@al-ft/midgard-core/codec/forced";
import { forcedVerdictSubject } from "@al-ft/midgard-sdk";
import { describe, expect, it } from "vitest";

import {
  VAN_ROSSEM_MAX_CPU_UNITS,
  VAN_ROSSEM_MAX_MEMORY_UNITS,
} from "../src/proof-fit/van-rossem.js";
import {
  prepareResolvedOutputAbsentInputEvidence,
  type ResolvedOutputAbsentInputEvidence,
} from "../src/resolved-output-non-canonical/index.js";
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
 *
 * A coordinate that names no input of the transaction (one past the field's
 * last input, a negative position, or a source kind outside the spend and
 * reference fields) is a wrongful rejection too: step 02 counts the named
 * field, or reads nothing for a negative position or a kind outside both, and
 * closes with no non-canonical output, which convicts. A claim that the
 * written input is absent is refused at step 02, and both routes convict over
 * the spend field at its byte cap.
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
      await expectOnchainRefusal(() => stages.step05Raw(walked.threadOutRef), {
        refusedBy: "fraud_proofs/resolved_output_non_canonical/step_05",
        check: /^Validator returned false$/u,
      });
    }, 900_000);
  });
}

type Scenario = Awaited<ReturnType<typeof setupScenario>>;

/** The rejection's subject over the committed forced leaf. */
const forcedSubjectOf = ({ block }: Pick<Scenario, "block">) => {
  if (block.forced === undefined) throw new Error("block is not forced");
  return forcedVerdictSubject({
    transactionId: block.nativeTxId,
    sourceKey: block.forced.membership.key,
    rejectionReason: block.forced.reason,
  });
};

/** A block whose forced leaf rejects `nativeTx` for `coordinate`. */
const commitForcedCoordinate = async (
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
const authenticateAbsent = async (
  stages: Scenario["stages"],
  evidence: ResolvedOutputAbsentInputEvidence,
) => {
  const bound = await stages.step01Forced(
    stages.threadOf(await stages.init()),
    evidence,
  );
  return await stages.step02(bound.result.nextThreadOutRef, evidence);
};

/** Convicts with the absent verdict and removes the block. */
const convictAbsent = async (
  { context, block, stages }: Pick<Scenario, "context" | "block" | "stages">,
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

const absentEvidenceOf = (scenario: Pick<Scenario, "block" | "prior">) =>
  prepareResolvedOutputAbsentInputEvidence({
    subject: forcedSubjectOf(scenario),
    canonicalTransactionCbor: scenario.block.canonicalCbor,
    priorRoot: scenario.prior.priorRoot,
  });

for (const shape of shapes) {
  describe(`forced ${shape.label} InputSpentOutputNonCanonical coordinate the transaction lacks`, () => {
    it("convicts a coordinate past the last input", async () => {
      const scenario = await commitForcedCoordinate(shape.nativeTx, {
        sourceKind: shape.sourceKind,
        inputIndex: inputItems.length,
      });
      const evidence = absentEvidenceOf(scenario);
      expect(evidence.namesInputField).toBe(true);
      await convictAbsent(scenario, evidence);
    }, 900_000);
  });
}

describe("forced InputSpentOutputNonCanonical coordinate outside the input fields", () => {
  it.each([
    {
      coordinateName: "a source kind naming neither field",
      sourceKind: 2,
      inputIndex: 0,
    },
    {
      coordinateName: "a negative spend position",
      sourceKind: 0,
      inputIndex: -1,
    },
  ])(
    "convicts $coordinateName with no opening",
    async ({ sourceKind, inputIndex }) => {
      const scenario = await commitForcedCoordinate(shapes[0].nativeTx, {
        sourceKind,
        inputIndex,
      });
      const evidence = absentEvidenceOf(scenario);
      expect(evidence.namesInputField).toBe(false);
      const { opened } = await convictAbsent(scenario, evidence);
      expect(opened.result.carriageTier).toBeNull();
    },
    900_000,
  );

  it("refuses on chain a claim that the written input is absent", async () => {
    const shape = shapes[0];
    const scenario = await setupScenario(shape, 0);
    expect(() => absentEvidenceOf(scenario)).toThrow(
      /input coordinate is present/u,
    );
    // What a lying prover holds: the written input, claimed absent.
    const lying: ResolvedOutputAbsentInputEvidence = {
      absent: true,
      subject: forcedSubjectOf(scenario),
      coordinate: {
        sourceKind: BigInt(scenario.coordinate.sourceKind),
        inputIndex: BigInt(scenario.coordinate.inputIndex),
      },
      resolved: { priorRoot: scenario.prior.priorRoot },
      outputIsNonCanonical: false,
      canonicalTransactionCborHex: scenario.block.canonicalCbor.toString("hex"),
      namesInputField: true,
    };
    await expectOnchainRefusal(
      () => authenticateAbsent(scenario.stages, lying),
      {
        refusedBy: "fraud_proofs/resolved_output_non_canonical/step_02",
        check: /^Validator returned false$/u,
      },
    );
  }, 900_000);
});

describe("forced InputSpentOutputNonCanonical coordinate past 2^53", () => {
  it.each([
    {
      coordinateName: "spend position 2^53 + 1",
      sourceKind: 0n,
      inputIndex: 2n ** 53n + 1n,
    },
    {
      coordinateName: "the codec's largest spend position, 2^64 - 1",
      sourceKind: 0n,
      inputIndex: 2n ** 64n - 1n,
    },
    {
      coordinateName: "the codec's smallest spend position, -2^64",
      sourceKind: 0n,
      inputIndex: -(2n ** 64n),
    },
    {
      coordinateName: "the codec's largest source kind, 2^64 - 1",
      sourceKind: 2n ** 64n - 1n,
      inputIndex: 0n,
    },
  ])(
    "convicts $coordinateName",
    async ({ sourceKind, inputIndex }) => {
      const scenario = await commitForcedCoordinate(shapes[0].nativeTx, {
        sourceKind,
        inputIndex,
      });
      const evidence = absentEvidenceOf(scenario);
      expect(evidence.coordinate).toStrictEqual({ sourceKind, inputIndex });
      const named = sourceKind === 0n && inputIndex >= 0n;
      expect(evidence.namesInputField).toBe(named);
      const { opened } = await convictAbsent(scenario, evidence);
      // A position past the field is closed by the field's count; any
      // other coordinate names no input without opening a field.
      expect(opened.result.carriageTier !== null).toBe(named);
    },
    900_000,
  );
});

const maxShapeInputs = (): readonly Buffer[] => {
  const inputs: Buffer[] = [];
  for (;;) {
    const next = outRefOf(inputs.length.toString(16).padStart(64, "0"));
    if (
      encodeMidgardFieldPreimage([...inputs, next]).length >
      MIDGARD_CONSENSUS_LIMITS.maxSpendInputsPreimageBytes
    )
      return inputs;
    inputs.push(next);
  }
};

describe("forced InputSpentOutputNonCanonical coordinate at the consensus maximum", () => {
  it.each([
    { position: "past the last", past: true },
    { position: "at a negative position", past: false },
  ])(
    "convicts a coordinate $position of the most spend inputs field 0 holds",
    async ({ past }) => {
      const inputs = maxShapeInputs();
      const inputIndex = past ? inputs.length : -1;
      const scenario = await commitForcedCoordinate(
        makeNativeTx({ spendInputCbors: inputs, fee: 9n, outputCbors: [] }),
        { sourceKind: 0, inputIndex },
      );
      const evidence = absentEvidenceOf(scenario);
      const { opened, minted } = await convictAbsent(scenario, evidence);
      // A negative position is absent without opening the field.
      expect(opened.result.carriageTier).toBe(past ? "Certified" : null);
      for (const { measurement } of [opened, minted]) {
        expect(measurement.executionMemory).toBeLessThanOrEqual(
          VAN_ROSSEM_MAX_MEMORY_UNITS,
        );
        expect(measurement.executionSteps).toBeLessThanOrEqual(
          VAN_ROSSEM_MAX_CPU_UNITS,
        );
      }
      console.info(
        `max-shape absent input ${inputIndex.toString()} (${inputs.length.toString()} spend inputs): step-02 mem=${opened.measurement.executionMemory.toString()} cpu=${opened.measurement.executionSteps.toString()}; step-05 mem=${minted.measurement.executionMemory.toString()} cpu=${minted.measurement.executionSteps.toString()}`,
      );
    },
    1_800_000,
  );
});
