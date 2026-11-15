/**
 * A forced rejection naming an output's reference script at a position field
 * 2 does not have is wrongful: canonical validation decodes no reference
 * script there. Step 02 closes a negative coordinate at once and one past the
 * field by its authenticated count, and hands the thread straight to step 06,
 * which convicts. A claim that a held output is absent is refused on chain,
 * and the route holds at the 761-output tier-3 maximum, through the field
 * grammar certificate for a coordinate past the field.
 */
import { encodeCbor } from "@al-ft/midgard-core";
import { forcedVerdictSubject } from "@al-ft/midgard-sdk";
import { describe, expect, it } from "vitest";

import { FIELD_GRAMMAR_WALKED_COUNT_LIMIT } from "../src/field-grammar-certificate.js";
import {
  type OutputReferenceScriptDecodingEvidence,
  OutputReferenceScriptResultClasses,
  prepareOutputReferenceScriptDecodingEvidence,
} from "../src/output-reference-script-decoding/index.js";
import {
  VAN_ROSSEM_MAX_CPU_UNITS,
  VAN_ROSSEM_MAX_MEMORY_UNITS,
} from "../src/proof-fit/van-rossem.js";
import { expectOnchainRefusal } from "./support/emulator/expect-onchain-refusal.js";
import { deployHarnessFieldGrammarCertificate } from "./support/field-grammar-certificate-deployment-fixture.js";
import {
  commitForcedBlock,
  createMeasurementRecorder,
  type ForcedLeaf,
  makeOutputReferenceHarness,
  makeOutputReferenceStages,
  outputWithNativeScript,
  publishFamilyReferences,
  signatureScript,
  subjectTransaction,
} from "./support/output-reference-script-decoding-emulator.js";

const recorder = createMeasurementRecorder();

const malformedAt = (outputIndex: number) =>
  ({
    OutputReferenceScriptMalformed: { output_index: BigInt(outputIndex) },
  }) as never;

const forcedEvidence = (leaf: ForcedLeaf, outputIndex: number) =>
  prepareOutputReferenceScriptDecodingEvidence({
    subject: forcedVerdictSubject({
      transactionId: leaf.transaction.tx_id,
      sourceKey: leaf.membership.key,
      rejectionReason: leaf.reason,
    }),
    outputIndex,
    canonicalTransactionCbor: leaf.canonicalCbor,
  });

const forcedSourceOf = (leaf: ForcedLeaf) => ({
  compactCborHex: leaf.transaction.submitted_source.compact_cbor,
  witnessSetCompactCborHex:
    leaf.transaction.submitted_source.witness_set_compact_cbor,
});

/** A forced block of `leaves`, the family's references and its stage drivers. */
const forcedScenario = async (
  leaves: Parameters<typeof commitForcedBlock>[1],
  label: string,
) => {
  const context = await makeOutputReferenceHarness();
  const block = await commitForcedBlock(context, leaves);
  const published = await publishFamilyReferences(context, recorder, label);
  const stages = makeOutputReferenceStages({
    context,
    fraudulentBlockOutRef: block.setup.fraudulentBlockOutRef,
    ...published,
  });
  return { context, block, stages };
};

/** 761 outputs of 43 preimage bytes fill field 2 to the tier-3 maximum. */
const MAX_OUTPUT_COUNT = 761;

/** One canonical output: enterprise pubkey address, lovelace only. */
const maxShapeOutputCbor = (index: number): Buffer =>
  Buffer.concat([
    Buffer.from([0xa2, 0x00, 0x58, 0x1d, 0x60]),
    Buffer.alloc(28, (0x40 + index) % 0x100),
    Buffer.from([0x01, 0x82]),
    encodeCbor(5_000_000n + BigInt(index)),
    Buffer.from([0xa0]),
  ]);

describe("outputReferenceScriptDecoding forced rejection of an absent output", () => {
  it("convicts rejections naming an output past the last and a negative output, and refuses a claim that a held output is absent", async () => {
    const { block, stages } = await forcedScenario(
      [
        {
          nativeTx: subjectTransaction([
            outputWithNativeScript(signatureScript()),
          ]),
          reason: malformedAt(1),
        },
        {
          nativeTx: subjectTransaction(
            [outputWithNativeScript(signatureScript())],
            8n,
          ),
          reason: malformedAt(0),
        },
        {
          nativeTx: subjectTransaction(
            [outputWithNativeScript(signatureScript())],
            9n,
          ),
          reason: malformedAt(-1),
        },
      ],
      "output-reference-absent",
    );
    const [absentLeaf, heldLeaf, negativeLeaf] = block.leaves;
    if (
      absentLeaf === undefined ||
      heldLeaf === undefined ||
      negativeLeaf === undefined
    )
      throw new Error("forced leaves absent");

    for (const [leaf, outputIndex] of [
      [absentLeaf, 1],
      [negativeLeaf, -1],
    ] as const) {
      const evidence = forcedEvidence(leaf, outputIndex);
      expect(evidence.absent).toBe(true);
      const s1 = await stages.step01Forced(
        stages.threadOf((await stages.init()).result),
        evidence,
        leaf,
        block.header,
      );
      const s2 = await stages.step02(
        s1.result.nextThreadOutRef,
        evidence,
        forcedSourceOf(leaf),
      );
      expect(s2.result.nextStage).toBe("step06");
      const s6 = await stages.step06(s2.result.nextThreadOutRef, evidence);
      expect(s6.result.fraudProofUnit).toBeTruthy();
    }

    // Output 0 is held: its rejection is closed by scanning it, and a step 02
    // that claims it absent is refused.
    const held = forcedEvidence(heldLeaf, 0);
    expect(held.absent).toBe(false);
    const claimedAbsent: OutputReferenceScriptDecodingEvidence = {
      ...held,
      absent: true,
      outputLength: 0,
      referenceScriptItemHex: "",
      referenceScriptItemCommitmentHex: forcedEvidence(absentLeaf, 1)
        .referenceScriptItemCommitmentHex,
      initialControlCbor: "",
      resultClass: OutputReferenceScriptResultClasses.NoFault,
    };
    const h1 = await stages.step01Forced(
      stages.threadOf((await stages.init()).result),
      held,
      heldLeaf,
      block.header,
    );
    await expectOnchainRefusal(
      async () =>
        await stages.step02(
          h1.result.nextThreadOutRef,
          claimedAbsent,
          forcedSourceOf(heldLeaf),
        ),
      {
        refusedBy: "fraud_proofs/output_reference_script_decoding/step_02",
        check: /^Validator returned false$/u,
      },
    );

    await stages.remove(block.headerHash);
  }, 1_800_000);

  it.each([
    { coordinate: "past the field", outputIndex: MAX_OUTPUT_COUNT },
    { coordinate: "negative", outputIndex: -1 },
  ])(
    "convicts a $coordinate output coordinate over a 761-output field",
    async ({ outputIndex }) => {
      const { context, block, stages } = await forcedScenario(
        [
          {
            nativeTx: subjectTransaction(
              Array.from({ length: MAX_OUTPUT_COUNT }, (_, index) =>
                maxShapeOutputCbor(index),
              ),
            ),
            reason: malformedAt(outputIndex),
          },
        ],
        "output-reference-absent-max",
      );
      const [leaf] = block.leaves;
      if (leaf === undefined) throw new Error("forced leaf absent");
      const evidence = forcedEvidence(leaf, outputIndex);
      expect(evidence.absent).toBe(true);
      expect(evidence.carriage).toBe("Certified");
      expect(MAX_OUTPUT_COUNT).toBeGreaterThan(
        FIELD_GRAMMAR_WALKED_COUNT_LIMIT,
      );
      // A coordinate past the field reads its count through the field
      // grammar certificate; a negative one reads no count.
      const fieldGrammar =
        outputIndex < 0
          ? undefined
          : (
              await deployHarnessFieldGrammarCertificate(
                context.harness,
                "outputReferenceScriptDecoding",
              )
            ).fieldGrammar;
      const s1 = await stages.step01Forced(
        stages.threadOf((await stages.init()).result),
        evidence,
        leaf,
        block.header,
      );
      const s2 = await stages.step02(
        s1.result.nextThreadOutRef,
        evidence,
        forcedSourceOf(leaf),
        fieldGrammar === undefined ? {} : { fieldGrammar },
      );
      expect(s2.result.nextStage).toBe("step06");
      const s6 = await stages.step06(s2.result.nextThreadOutRef, evidence);
      expect(s6.result.fraudProofUnit).toBeTruthy();
      for (const { measurement } of [s1, s2, s6]) {
        expect(measurement.executionMemory).toBeLessThanOrEqual(
          VAN_ROSSEM_MAX_MEMORY_UNITS,
        );
        expect(measurement.executionSteps).toBeLessThanOrEqual(
          VAN_ROSSEM_MAX_CPU_UNITS,
        );
      }
      console.info(
        `max-shape absent output reference (${MAX_OUTPUT_COUNT.toString()} outputs, coordinate ${outputIndex.toString()}): step-01 mem=${s1.measurement.executionMemory.toString()} cpu=${s1.measurement.executionSteps.toString()}; step-02 mem=${s2.measurement.executionMemory.toString()} cpu=${s2.measurement.executionSteps.toString()}; step-06 mem=${s6.measurement.executionMemory.toString()} cpu=${s6.measurement.executionSteps.toString()}`,
      );
      await stages.remove(block.headerHash);
    },
    1_800_000,
  );
});
