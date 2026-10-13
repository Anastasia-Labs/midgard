/**
 * resolvedOutputNonCanonical (`00000026`) — the datum step over the registered
 * reference scripts. When the envelope of the resolved output closes
 * canonical over an inline datum, the datum step reads that datum as
 * canonical Data, a planned number of nodes per transaction, to the verdict
 * step 05 judges. These cases convict the acceptance of a non-canonical datum
 * and a forced rejection of the densest canonical datums, refuse an honest
 * acceptance of a canonical datum, refuse forged datum steps and cancel a
 * thread parked on the datum step. The envelope's lifecycle is in
 * resolved-output-non-canonical-lifecycle.test.ts; the two files together
 * write the fit ledger.
 */
import { afterAll, describe, expect, it } from "vitest";

import {
  beginMeasuredCase,
  coverage,
  DATUM_SEAMS,
  expectMeasuredFit,
  MEASURED_CASES,
  measureFitPart,
  passMeasuredCase,
  progress,
  record,
} from "./resolved-output-non-canonical-lifecycle.record-carriage.js";
import { refuseDatumSeams } from "./resolved-output-non-canonical-lifecycle.refuse-datum-seams.js";
import { densePlutusData } from "./support/dense-plutus-data.js";
import { expectOnchainRefusal } from "./support/emulator/expect-onchain-refusal.js";
import type { CompleteSignedTransactionMeasurement } from "./support/emulator/measurement.js";
import {
  buildPriorLedger,
  commitBlock,
  makeResolvedOutputContext,
  makeResolvedOutputStages,
  MAXIMUM_DATUM_BYTES,
  outputWithDatumBytes,
  RESOLVED_OUTPUT_MAXIMUM_BYTES,
  RESOLVED_OUTPUT_REASON_ARM,
  resolvedOutputEvidence,
  resolvedOutputReason,
  subjectTransactionFor,
} from "./support/resolved-output-non-canonical-emulator.js";

describe("resolvedOutputNonCanonical inline-datum verdict", () => {
  measureFitPart("datum", [MEASURED_CASES.densestDatum]);

  it("convicts the acceptance of an output whose inline datum is not canonical under a canonical envelope, refusing forged datum steps, and cancels a thread parked on the datum step", async () => {
    const context = await makeResolvedOutputContext();
    // Zero written with a one-byte argument it does not need.
    const prior = await buildPriorLedger({
      output: outputWithDatumBytes(Buffer.from("1800", "hex")),
      adversarialDepth: false,
    });
    const coordinate = { sourceKind: 0, inputIndex: 0 } as const;
    const block = await commitBlock({
      context,
      nativeTx: subjectTransactionFor({
        sourceKind: 0,
        outRefBytes: prior.outRefBytes,
        count: 1,
      }),
      priorRoot: prior.priorRoot,
    });
    const evidence = resolvedOutputEvidence({ block, prior, coordinate });
    expect(evidence.outputIsNonCanonical).toBe(true);
    expect(evidence.datumPlan?.canonical).toBe(false);
    const stages = await makeResolvedOutputStages(context, block);
    const toDatum = async () => {
      const bound = await stages.step01Accepted(await stages.init(), evidence);
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
      expect(walked.final.nextStage).toBe("step04Datum");
      return walked.threadOutRef;
    };
    const atDatum = await toDatum();
    await refuseDatumSeams(stages, atDatum, evidence);
    const read = await stages.readDatum(atDatum, evidence);
    expect(read.steps).toBe(1);
    const minted = await stages.step05(read.threadOutRef, evidence);
    expect(minted.result.fraudProofUnit).toBeTruthy();
    coverage.reason(RESOLVED_OUTPUT_REASON_ARM, "accepted_invalid");
    await stages.cancel(await toDatum(), 4);
    coverage.cancelled("step-04-datum");
  }, 900_000);

  it("refuses to convict an honest acceptance of an output whose inline datum is canonical: the datum reads canonical and cannot mint under an accepted subject", async () => {
    const context = await makeResolvedOutputContext();
    const prior = await buildPriorLedger({
      output: outputWithDatumBytes(Buffer.from("1818", "hex")),
      adversarialDepth: false,
    });
    const coordinate = { sourceKind: 1, inputIndex: 0 } as const;
    const block = await commitBlock({
      context,
      nativeTx: subjectTransactionFor({
        sourceKind: 1,
        outRefBytes: prior.outRefBytes,
        count: 1,
      }),
      priorRoot: prior.priorRoot,
    });
    expect(() =>
      resolvedOutputEvidence({ block, prior, coordinate }),
    ).toThrow();
    const lying = resolvedOutputEvidence({
      block,
      prior,
      coordinate,
      claim: "lying",
    });
    expect(lying.outputIsNonCanonical).toBe(false);
    expect(lying.datumPlan?.canonical).toBe(true);
    const stages = await makeResolvedOutputStages(context, block);
    const bound = await stages.step01Accepted(await stages.init(), lying);
    const opened = await stages.step02(bound.result.nextThreadOutRef, lying);
    const staged = await stages.step03(opened.result.nextThreadOutRef, lying);
    const walked = await stages.reconstruct(
      staged.result.nextThreadOutRef,
      lying,
    );
    expect(walked.final.nextStage).toBe("step04Datum");
    const read = await stages.readDatum(walked.threadOutRef, lying);
    await expect(stages.step05(read.threadOutRef, lying)).rejects.toThrow();
    await expectOnchainRefusal(() => stages.step05Raw(read.threadOutRef), {
      refusedBy: "fraud_proofs/resolved_output_non_canonical/step_05",
      check: /^Validator returned false$/u,
    });
    coverage.scenario("honest_accepted_block_refusal");
  }, 900_000);

  it("convicts a forced rejection of maximum outputs whose inline datums are the densest canonical Data, reading each to its verdict, and measures the costliest datum step", async () => {
    beginMeasuredCase(MEASURED_CASES.densestDatum);
    let costliest: CompleteSignedTransactionMeasurement | undefined;
    let costliestShape = "";
    for (const shape of [
      "map-chain",
      "generic-constructors",
      "flat-integers",
      "chunked-bytes",
    ] as const) {
      const context = await makeResolvedOutputContext();
      const output = outputWithDatumBytes(
        densePlutusData(shape, MAXIMUM_DATUM_BYTES),
      );
      expect(output).toHaveLength(RESOLVED_OUTPUT_MAXIMUM_BYTES);
      const prior = await buildPriorLedger({ output, adversarialDepth: false });
      const coordinate = { sourceKind: 0, inputIndex: 0 } as const;
      const block = await commitBlock({
        context,
        nativeTx: subjectTransactionFor({
          sourceKind: 0,
          outRefBytes: prior.outRefBytes,
          count: 1,
        }),
        priorRoot: prior.priorRoot,
        reason: resolvedOutputReason(coordinate),
      });
      const evidence = resolvedOutputEvidence({ block, prior, coordinate });
      expect(evidence.outputIsNonCanonical).toBe(false);
      const plan = evidence.datumPlan!;
      expect(plan.canonical).toBe(true);
      const stages = await makeResolvedOutputStages(context, block);
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
      expect(walked.final.nextStage).toBe("step04Datum");
      // Every datum step stays within its planned estimate, which the
      // planner keeps below one transaction's budget.
      const keep = (
        captured: {
          readonly measurement: CompleteSignedTransactionMeasurement;
        },
        index: number,
      ) => {
        const { executionMemory, executionSteps } = captured.measurement;
        expect(executionMemory).toBeLessThanOrEqual(
          BigInt(plan.steps[index]!.estimatedMemory),
        );
        expect(executionMemory).toBeLessThanOrEqual(13_200_000n);
        expect(executionSteps).toBeLessThanOrEqual(8_000_000_000n);
        if (
          costliest === undefined ||
          captured.measurement.executionMemory > costliest.executionMemory
        ) {
          costliest = captured.measurement;
          costliestShape = shape;
        }
      };
      const first = await stages.step04Datum(walked.threadOutRef, evidence);
      expect(first.result.finished).toBe(false);
      keep(first, 0);
      // A container is open at the second checkpoint.
      await refuseDatumSeams(stages, first.result.nextThreadOutRef, evidence);
      const read = await stages.readDatum(
        first.result.nextThreadOutRef,
        evidence,
        (captured, index) => keep(captured, index + 1),
      );
      expect(read.steps + 1).toBe(plan.steps.length);
      progress(
        `${shape}: ${walked.transitions.toString()} envelope and ${plan.steps.length.toString()} datum transactions`,
      );
      const minted = await stages.step05(read.threadOutRef, evidence);
      expect(minted.result.fraudProofUnit).toBeTruthy();
      coverage.reason(RESOLVED_OUTPUT_REASON_ARM, "forced_rejection_wrong");
    }
    record("ronc_datum_verdict_step_max", costliest!, {
      maximumShape: `${MAXIMUM_DATUM_BYTES.toLocaleString("en-US")}-byte inline datum (${costliestShape}, the costliest of map chains, generic constructors, flat integers and chunked byte strings) filling a ${RESOLVED_OUTPUT_MAXIMUM_BYTES.toLocaleString("en-US")}-byte output`,
    });
    passMeasuredCase();
  }, 3_600_000);

  afterAll(() => {
    const observed = coverage.snapshot();
    expect(
      observed.successfulDirectionByReason[RESOLVED_OUTPUT_REASON_ARM],
    ).toEqual(
      expect.arrayContaining(["accepted_invalid", "forced_rejection_wrong"]),
    );
    expect(observed.scenarios).toContain("honest_accepted_block_refusal");
    expect(observed.authenticatedSeamsMutated).toEqual(
      expect.arrayContaining([...DATUM_SEAMS]),
    );
    expect(observed.cancelledPhysicalSteps).toContain("step-04-datum");
    expectMeasuredFit();
  });
});
