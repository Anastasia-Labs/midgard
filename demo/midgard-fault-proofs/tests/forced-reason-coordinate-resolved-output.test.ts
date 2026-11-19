import { computeMidgardNativeTxId } from "@al-ft/midgard-core";
import {
  encodeMidgardForcedTxCanonical,
  materializeMidgardForcedTxFromCanonical,
} from "@al-ft/midgard-core/codec/forced";
import { describe, expect, it } from "vitest";

import type { ResolvedOutputAbsentInputEvidence } from "../src/resolved-output-non-canonical/resolved-output-non-canonical.prepare-resolved-output-non-canonical-evidence.js";
import {
  ResolvedOutputStep02RedeemerSchema,
  ResolvedOutputStep05DatumSchema,
} from "../src/resolved-output-non-canonical/schemas.js";
import { expectOnchainRefusal } from "./support/emulator/expect-onchain-refusal.js";
import { nodeForcedVerdict } from "./support/emulator/node-forced-verdict.js";
import {
  absentEvidenceOf,
  authenticateAbsent,
  commitForcedCoordinate,
  convictAbsent,
  forcedSubjectOf,
  inputItems,
  outRefOf,
  priorEntries,
  priorLedgerOf,
  shapes,
} from "./support/forced-reason-resolved-output-coordinate.js";
import {
  makeResolvedOutputContext,
  resolvedOutputReason,
} from "./support/resolved-output-non-canonical-emulator.build-prior-ledger.js";
import { commitBlock } from "./support/resolved-output-non-canonical-emulator.commit-block.js";
import { makeResolvedOutputStages } from "./support/resolved-output-non-canonical-emulator.make-resolved-output-stages.js";
import { resolvedOutputEvidence } from "./support/resolved-output-non-canonical-emulator.resolved-output-evidence.js";

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
 * written input is absent is refused at step 02, whether it counts the named
 * field or reads none.
 */

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

describe("forced InputSpentOutputNonCanonical present coordinate claimed absent without an opening", () => {
  // Step 02 reads no field only for a coordinate naming neither input field.
  // Input 0 of each field is present, so a successor recording no
  // non-canonical output there with no opening is refused.
  it.each([
    { field: "spend", sourceKind: 0n },
    { field: "reference", sourceKind: 1n },
  ])(
    "refuses on chain an unopened absence of $field input 0",
    async ({ sourceKind }) => {
      const coordinate = { sourceKind, inputIndex: 0n };
      const scenario = await commitForcedCoordinate(
        shapes[1].nativeTx,
        coordinate,
      );
      expect(() => absentEvidenceOf(scenario)).toThrow(
        /input coordinate is present/u,
      );
      const subject = forcedSubjectOf(scenario);
      // What a lying prover holds: the present input, claimed absent.
      const lying: ResolvedOutputAbsentInputEvidence = {
        absent: true,
        subject,
        coordinate,
        resolved: { priorRoot: scenario.prior.priorRoot },
        outputIsNonCanonical: false,
        canonicalTransactionCborHex:
          scenario.block.canonicalCbor.toString("hex"),
        namesInputField: false,
      };
      const { stages } = scenario;
      const bound = await stages.step01Forced(
        stages.threadOf(await stages.init()),
        lying,
      );
      await expectOnchainRefusal(
        () =>
          stages.continueRaw({
            threadOutRef: bound.result.nextThreadOutRef,
            stepIndex: 1,
            nextStepIndex: 5,
            nextData: { subject, output_is_non_canonical: false },
            nextDatumSchema: ResolvedOutputStep05DatumSchema,
            redeemerSchema: ResolvedOutputStep02RedeemerSchema,
            args: (input_index, output_index) => ({
              input_index,
              output_index,
              opening: null,
            }),
          }),
        {
          refusedBy: "fraud_proofs/resolved_output_non_canonical/step_02",
          check: /^Validator returned false$/u,
        },
      );
    },
    900_000,
  );
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
