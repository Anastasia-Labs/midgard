/**
 * `outputReferenceScriptDecoding` registered-chain lifecycle (§5.3 + Wave 4).
 *
 * Every journey runs from the generic `Init` through the six applied
 * validators the catalogue registers, on the shared Van Rossem emulator
 * parameters with local UPLC evaluation. Refusals are asserted on chain
 * (`expectOnchainRefusal`), never through an off-chain guard alone.
 */
import { createHash } from "node:crypto";
import { readFileSync } from "node:fs";
import { fileURLToPath } from "node:url";

import {
  computeHash32,
  decodeMidgardFieldPreimage,
  deriveMidgardNativeTxFaultEvidenceMaterial,
  encodeMidgardNativeTxCanonical,
} from "@al-ft/midgard-core";
import {
  acceptedVerdictSubject,
  forcedVerdictSubject,
} from "@al-ft/midgard-sdk";
import { describe, expect, it } from "vitest";

import {
  nativeScriptDecodingScanArgsEvidence,
  nativeScriptDecodingWindowProofs,
} from "../src/native-script-decoding/evidence.js";
import { buildNativeScriptDecodingScanPlan } from "../src/native-script-decoding/scan-plan.js";
import {
  outputReferenceScriptCheckpoint,
  type OutputReferenceScriptDecodingEvidence,
  OutputReferenceScriptResultClasses,
  planOutputReferenceScriptDecodingStep05,
  prepareOutputReferenceScriptDecodingEvidence,
  readOutputReferenceScriptScanState,
} from "../src/output-reference-script-decoding/index.js";
import {
  buildVanRossemFitLedger,
  writeVanRossemFitLedger,
} from "../src/proof-fit/van-rossem-fit-ledger.js";
import { assertCompleteLifecycleCoverage } from "../src/testing/complete-lifecycle.js";
import { expectOnchainRefusal } from "./support/emulator/expect-onchain-refusal.js";
import { createLifecycleCoverageRecorder } from "./support/lifecycle-coverage.js";
import {
  commitAcceptedBlock,
  commitForcedBlock,
  createMeasurementRecorder,
  type ForcedLeaf,
  foreignForcedMembership,
  makeOutputReferenceHarness,
  makeOutputReferenceStages,
  maximumWideScriptOutput,
  nestedScript,
  OUTPUT_REFERENCE_CATEGORY_ID,
  OUTPUT_REFERENCE_MAX_OUTPUT_BYTES,
  OUTPUT_REFERENCE_REASON_ARMS,
  type OutputReferenceContext,
  outputWithNativeScript,
  outputWithRawNativePayload,
  publishFamilyReferences,
  rawNativeOutputOfLength,
  signatureScript,
  subjectTransaction,
} from "./support/output-reference-script-decoding-emulator.js";
import { realBlueprintPath } from "./support/submit-init-emulator-shared.js";

const AUTHENTICATION_SEAMS = [
  "tx_membership",
  "field_certificate",
  "forced_leaf_root",
  "forced_leaf_header",
  "forced_leaf_direction",
  "forced_leaf_reason_coordinate",
  "output_chunk",
  "scan_checkpoint",
  "wrong_successor",
  "premature_close",
] as const;
const CANCELLABLE_STEPS = [
  "step-01",
  "step-02",
  "step-03",
  "step-04",
  "step-05",
  "step-06",
] as const;
const SMALL_SHAPE = "small canonical output with Inline field-2 carriage";
const ledgerPath = fileURLToPath(
  new URL(
    "../../../docs/fault-proofs/size-plans/output-reference-script-decoding-v1-fit-ledger.json",
    import.meta.url,
  ),
);

const coverage = createLifecycleCoverageRecorder();
const recorder = createMeasurementRecorder();
const { record, recordCarriage } = recorder;
const progress = (message: string) =>
  console.info(`[output-reference-script-decoding-progress] ${message}`);

const forcedReason = (arm: (typeof OUTPUT_REFERENCE_REASON_ARMS)[number]) =>
  ({ [arm]: { output_index: 0n } }) as never;
const forcedEvidence = (leaf: ForcedLeaf, admitHonestVerdict = false) =>
  prepareOutputReferenceScriptDecodingEvidence({
    subject: forcedVerdictSubject({
      transactionId: leaf.transaction.tx_id,
      sourceKey: leaf.membership.key,
      rejectionReason: leaf.reason,
    }),
    outputIndex: 0,
    canonicalTransactionCbor: leaf.canonicalCbor,
    admitHonestVerdict,
  });
const scanStateOf = async (
  context: OutputReferenceContext,
  threadOutRef: string,
) =>
  (
    await readOutputReferenceScriptScanState({
      lucid: context.harness.proverLucid,
      contracts: context.contracts,
      categoryId: context.category.categoryId,
      signer: context.harness.proverSigner,
      threadOutRef,
    })
  ).state;
const forcedSourceOf = (leaf: ForcedLeaf) => ({
  compactCborHex: leaf.transaction.submitted_source.compact_cbor,
  witnessSetCompactCborHex:
    leaf.transaction.submitted_source.witness_set_compact_cbor,
});

describe("outputReferenceScriptDecoding registered-chain lifecycle", () => {
  it("contradicts a wrongful acceptance at the 16,384-byte output bound: refuses every accepted seam and the adjacent over-bound output, resumes the descriptor scan, cancels every nonterminal step, mints and removes", async () => {
    const context = await makeOutputReferenceHarness();
    const maximum = rawNativeOutputOfLength(OUTPUT_REFERENCE_MAX_OUTPUT_BYTES);
    const adjacent = rawNativeOutputOfLength(
      OUTPUT_REFERENCE_MAX_OUTPUT_BYTES + 1,
    );
    const block = await commitAcceptedBlock(context, [
      subjectTransaction([maximum], 7n),
      subjectTransaction([maximum], 8n),
      subjectTransaction([adjacent], 9n),
    ]);
    const [subject, foreign, overBound] = block.subjects;
    if (
      subject === undefined ||
      foreign === undefined ||
      overBound === undefined
    )
      throw new Error("accepted block subjects absent");
    const published = await publishFamilyReferences(
      context,
      recorder,
      "output-reference",
    );
    const stages = makeOutputReferenceStages({
      context,
      fraudulentBlockOutRef: block.setup.fraudulentBlockOutRef,
      ...published,
    });
    const evidence = prepareOutputReferenceScriptDecodingEvidence({
      subject: acceptedVerdictSubject(subject.nativeTxId),
      outputIndex: 0,
      canonicalTransactionCbor: subject.canonicalCbor,
    });
    expect(evidence.outputLength).toBe(OUTPUT_REFERENCE_MAX_OUTPUT_BYTES);
    expect(evidence.carriage).toBe("Certified");
    expect(evidence.resultClass).toBe(
      OutputReferenceScriptResultClasses.Malformed,
    );
    coverage.scenario("maximum_supported_evidence");
    // The consensus bound, off chain: one byte over is refused before any
    // transaction is built.
    expect(() =>
      prepareOutputReferenceScriptDecodingEvidence({
        subject: acceptedVerdictSubject(overBound.nativeTxId),
        outputIndex: 0,
        canonicalTransactionCbor: overBound.canonicalCbor,
      }),
    ).toThrow(/exceeds canonical size bound/u);

    progress("init and transaction-membership seam");
    const initialized = await stages.init();
    record("init", initialized.measurement);
    // A foreign transactions root cannot bind the header's counted root,
    // whatever proof rides with it.
    await expectOnchainRefusal(() =>
      stages.step01Accepted(initialized.result, evidence, {
        ...subject.txInclusion,
        transactionsPhasRoot: "11".repeat(32),
      }),
    );
    coverage.seamMutated("tx_membership");
    const s1 = await stages.step01Accepted(
      initialized.result,
      evidence,
      subject.txInclusion,
    );
    record("step01", s1.measurement);

    progress("step 02 with certified carriage");
    const s2 = await stages.step02(
      s1.result.nextThreadOutRef,
      evidence,
      subject,
    );
    recordCarriage("step02", s2);
    record("step02-certified", s2.measurement);
    const opened = {
      carriageUtxos: s2.result.carriageUtxos,
      certificateUtxo: s2.result.certificateUtxo,
    };

    progress("field-certificate seam");
    // A genuine certificate over the same field-2 bytes, anchored to another
    // transaction: the opening door refuses the anchor mismatch on chain.
    const foreignField = await stages.certifyForeignField(
      foreign,
      evidence.outputFieldPreimageHex,
    );
    const certificateThread = await stages.step01Accepted(
      (await stages.init()).result,
      evidence,
      subject.txInclusion,
    );
    await expectOnchainRefusal(() =>
      stages.step02Raw(
        certificateThread.result.nextThreadOutRef,
        evidence,
        foreignField,
      ),
    );
    coverage.seamMutated("field_certificate");
    record(
      "cancel-step02",
      (await stages.cancel(certificateThread.result.nextThreadOutRef, 1))
        .measurement,
    );
    coverage.cancelled("step-02");

    progress("out-of-range coordinate");
    // Step 01 binds any coordinate; the field opening at step 02 refuses one
    // the committed field does not carry.
    const outOfRange = { ...evidence, outputIndex: 1 };
    const rangeThread = await stages.step01Accepted(
      (await stages.init()).result,
      outOfRange,
      subject.txInclusion,
    );
    await expectOnchainRefusal(() =>
      stages.step02(
        rangeThread.result.nextThreadOutRef,
        outOfRange,
        subject,
        opened,
      ),
    );
    coverage.scenario("reason_or_subject_coordinate_mutation");
    await stages.cancel(rangeThread.result.nextThreadOutRef, 1);

    progress("adjacent over-bound output");
    // One byte past `max_output_canonical_cbor_bytes`: the committed output
    // is real, its field carriage is real, and step 02 refuses the size on
    // chain.
    const overField = deriveMidgardNativeTxFaultEvidenceMaterial(
      overBound.canonicalCbor,
    ).fieldPreimages[2]!;
    const overOutput = decodeMidgardFieldPreimage(overField)[0]!;
    expect(overOutput.length).toBe(OUTPUT_REFERENCE_MAX_OUTPUT_BYTES + 1);
    const overEvidence: OutputReferenceScriptDecodingEvidence = {
      ...evidence,
      subject: acceptedVerdictSubject(overBound.nativeTxId),
      canonicalTransactionCborHex: overBound.canonicalCbor.toString("hex"),
      outputFieldPreimageHex: overField.toString("hex"),
      outputCborHex: overOutput.toString("hex"),
      outputLength: overOutput.length,
      outputHashHex: computeHash32(overOutput).toString("hex"),
      outputChunkHashes: Array.from(
        { length: Math.ceil(overOutput.length / 4_095) },
        (_, index) =>
          computeHash32(
            overOutput.subarray(index * 4_095, (index + 1) * 4_095),
          ).toString("hex"),
      ),
    };
    const overThread = await stages.step01Accepted(
      (await stages.init()).result,
      overEvidence,
      overBound.txInclusion,
    );
    await expectOnchainRefusal(() =>
      stages.step02(
        overThread.result.nextThreadOutRef,
        overEvidence,
        overBound,
      ),
    );
    coverage.adjacentOverBoundRefused();
    await stages.cancel(overThread.result.nextThreadOutRef, 1);

    progress("descriptor scan windows");
    const s3 = await stages.step03Loop(
      s2.result.nextThreadOutRef,
      evidence,
      (window, index) =>
        record(`step03-window-${index.toString()}`, window.measurement),
    );
    expect(s3.windows).toBeGreaterThan(1);
    coverage.resumed();
    const s4 = await stages.step04(
      s3.threadOutRef,
      evidence,
      subject.compactCborHex,
      opened,
    );
    record("step04-certified-reference-bind", s4.measurement);

    progress("step 05 seams");
    const scanThread = s4.result.nextThreadOutRef;
    const planned = planOutputReferenceScriptDecodingStep05({
      contracts: context.contracts,
      state: await scanStateOf(context, scanThread),
      evidence,
    });
    expect(planned.nextStepIndex).toBe(5);
    expect(planned.nextState.result_class).toBe(
      BigInt(OutputReferenceScriptResultClasses.Malformed),
    );
    // The adjacent chunk in place of the cursor's chunk: the bounded-item
    // coordinate check refuses it.
    await expectOnchainRefusal(() =>
      stages.step05Raw(
        scanThread,
        { ...planned.args, chunk_proof: planned.args["next_chunk_proof"] },
        planned.nextState,
        5,
      ),
    );
    coverage.seamMutated("output_chunk");
    await expectOnchainRefusal(() =>
      stages.step05Raw(
        scanThread,
        planned.args,
        { ...planned.nextState, checkpoint_hash: "00".repeat(32) },
        5,
      ),
    );
    coverage.seamMutated("scan_checkpoint");
    // A refusal verdict may only continue at step 06.
    await expectOnchainRefusal(() =>
      stages.step05Raw(scanThread, planned.args, planned.nextState, 4),
    );
    coverage.seamMutated("wrong_successor");
    const s5 = await stages.step05(scanThread, evidence);
    expect(s5.result.closed).toBe(true);
    record("step05-verdict-malformed", s5.measurement);

    progress("terminal mint");
    const s6 = await stages.step06(s5.result.nextThreadOutRef, evidence);
    expect(s6.result.fraudProofUnit).toBeTruthy();
    record("step06-final-mint", s6.measurement);
    coverage.reason("OutputReferenceScriptMalformed", "accepted_invalid");
    coverage.scenario("wrongful_acceptance_success");

    progress("cancellations");
    record(
      "cancel-step01",
      (await stages.cancel(stages.threadOf((await stages.init()).result), 0))
        .measurement,
    );
    coverage.cancelled("step-01");
    const branch = async (upTo: 2 | 3 | 4) => {
      const b1 = await stages.step01Accepted(
        (await stages.init()).result,
        evidence,
        subject.txInclusion,
      );
      const b2 = await stages.step02(
        b1.result.nextThreadOutRef,
        evidence,
        subject,
        opened,
      );
      if (upTo === 2) return b2.result.nextThreadOutRef;
      const b3 = await stages.step03Loop(b2.result.nextThreadOutRef, evidence);
      if (upTo === 3) return b3.threadOutRef;
      return (
        await stages.step04(
          b3.threadOutRef,
          evidence,
          subject.compactCborHex,
          opened,
        )
      ).result.nextThreadOutRef;
    };
    record(
      "cancel-step03",
      (await stages.cancel(await branch(2), 2)).measurement,
    );
    coverage.cancelled("step-03");
    record(
      "cancel-step04",
      (await stages.cancel(await branch(3), 3)).measurement,
    );
    coverage.cancelled("step-04");
    record(
      "cancel-step05",
      (await stages.cancel(await branch(4), 4)).measurement,
    );
    coverage.cancelled("step-05");

    progress("leased removal");
    const removal = await stages.remove(block.setup.headerHash);
    expect(removal.result.fraudCategoryId).toBe(OUTPUT_REFERENCE_CATEGORY_ID);
    record("leased-removal", removal.measurement);
    coverage.scenario("permanent_proof_token_and_descendant_removal");
  }, 1_500_000);

  it("contradicts a wrongful acceptance of an empty native payload at the bind, and refuses to convict an honest accepted signature script", async () => {
    const context = await makeOutputReferenceHarness();
    const block = await commitAcceptedBlock(context, [
      subjectTransaction([outputWithRawNativePayload(Buffer.alloc(0))], 7n),
      subjectTransaction([outputWithNativeScript(signatureScript())], 8n),
    ]);
    const [empty, honest] = block.subjects;
    if (empty === undefined || honest === undefined)
      throw new Error("accepted block subjects absent");
    const published = await publishFamilyReferences(
      context,
      recorder,
      "output-reference-small",
    );
    const stages = makeOutputReferenceStages({
      context,
      fraudulentBlockOutRef: block.setup.fraudulentBlockOutRef,
      ...published,
    });
    const small = { maximumShape: SMALL_SHAPE };

    progress("empty native payload");
    // `[0, h'']`: canonical validation classes it InvalidReferenceScript; the
    // family closes malformed at the bind and never opens the scan.
    const evidence = prepareOutputReferenceScriptDecodingEvidence({
      subject: acceptedVerdictSubject(empty.nativeTxId),
      outputIndex: 0,
      canonicalTransactionCbor: empty.canonicalCbor,
    });
    expect(evidence.resultClass).toBe(
      OutputReferenceScriptResultClasses.Malformed,
    );
    expect(evidence.initialControlCbor).toBe("");
    expect(evidence.referenceScriptItemHex).toBe("820040");
    const s1 = await stages.step01Accepted(
      (await stages.init()).result,
      evidence,
      empty.txInclusion,
    );
    record("small-step01", s1.measurement, small);
    const s2 = await stages.step02(s1.result.nextThreadOutRef, evidence, empty);
    record("small-step02", s2.measurement, small);
    const s3 = await stages.step03Loop(s2.result.nextThreadOutRef, evidence);
    const s4 = await stages.step04(
      s3.threadOutRef,
      evidence,
      empty.compactCborHex,
      s2.result,
    );
    record("small-step04-bind-malformed", s4.measurement, small);
    const s5 = await stages.step05(s4.result.nextThreadOutRef, evidence);
    expect(s5.result.closed).toBe(true);
    record("small-step05-closed-passthrough", s5.measurement, small);
    const s6 = await stages.step06(s5.result.nextThreadOutRef, evidence);
    expect(s6.result.fraudProofUnit).toBeTruthy();
    record("small-step06-final-mint", s6.measurement, small);
    coverage.reason("OutputReferenceScriptMalformed", "accepted_invalid");

    progress("honest accepted signature script");
    expect(() =>
      prepareOutputReferenceScriptDecodingEvidence({
        subject: acceptedVerdictSubject(honest.nativeTxId),
        outputIndex: 0,
        canonicalTransactionCbor: honest.canonicalCbor,
      }),
    ).toThrow(/agrees with operator verdict/u);
    const lying = prepareOutputReferenceScriptDecodingEvidence({
      subject: acceptedVerdictSubject(honest.nativeTxId),
      outputIndex: 0,
      canonicalTransactionCbor: honest.canonicalCbor,
      admitHonestVerdict: true,
    });
    expect(lying.resultClass).toBe(OutputReferenceScriptResultClasses.NoFault);
    const h1 = await stages.step01Accepted(
      (await stages.init()).result,
      lying,
      honest.txInclusion,
    );
    const h2 = await stages.step02(h1.result.nextThreadOutRef, lying, honest);
    const h3 = await stages.step03Loop(h2.result.nextThreadOutRef, lying);
    const h4 = await stages.step04(
      h3.threadOutRef,
      lying,
      honest.compactCborHex,
      h2.result,
    );
    // The only admissible scan closure of a decodable item is no-fault at the
    // exact terminal; the step-06 validator then refuses to mint under an
    // accepted subject.
    const state = await scanStateOf(context, h4.result.nextThreadOutRef);
    const item = Buffer.from(lying.referenceScriptItemHex, "hex");
    const plan = buildNativeScriptDecodingScanPlan({
      itemBytes: item,
      direction: 1,
    });
    expect(plan.segments).toHaveLength(1);
    const step06Hash = context.contracts.steps[5].spendingScriptHash;
    const closed = await stages.step05Raw(
      h4.result.nextThreadOutRef,
      nativeScriptDecodingScanArgsEvidence({
        segment: plan.segments[0]!,
        fieldIndex: 2,
        itemIndex: 0,
        itemBytes: item,
      }),
      {
        ...state,
        next_expected_script_hash: step06Hash,
        checkpoint_hash: outputReferenceScriptCheckpoint({
          evidence: lying,
          controlCbor: state.control_cbor,
          nextExpectedScriptHash: step06Hash,
        }),
        result_class: BigInt(OutputReferenceScriptResultClasses.NoFault),
      },
      5,
    );
    await expect(stages.step06(closed.nextThreadOutRef, lying)).rejects.toThrow(
      /retained contradiction/u,
    );
    await expectOnchainRefusal(() => stages.step06Raw(closed.nextThreadOutRef));
    coverage.scenario("honest_accepted_block_refusal");
    record(
      "cancel-step06",
      (await stages.cancel(closed.nextThreadOutRef, 5)).measurement,
      small,
    );
    coverage.cancelled("step-06");
  }, 900_000);

  it("contradicts a wrongful forced NodeLimit rejection of the widest all-of script: refuses every forced-door seam, a wrong successor and a premature close, resumes the native scan across chunk windows, and refuses an honest forced rejection", async () => {
    const context = await makeOutputReferenceHarness();
    const wide = maximumWideScriptOutput();
    expect(wide.output.length).toBeGreaterThan(16_300);
    const block = await commitForcedBlock(context, [
      {
        nativeTx: subjectTransaction([wide.output]),
        reason: forcedReason("OutputReferenceScriptNodeLimit"),
      },
      {
        nativeTx: subjectTransaction([
          outputWithRawNativePayload(Buffer.from("820700", "hex")),
        ]),
        reason: forcedReason("OutputReferenceScriptMalformed"),
      },
      {
        nativeTx: subjectTransaction([
          outputWithNativeScript(signatureScript()),
        ]),
        reason: {
          InputNotFound: { source_kind: 0n, input_index: 0n },
        } as never,
      },
    ]);
    const [accused, honestLeaf, otherReasonLeaf] = block.leaves;
    if (
      accused === undefined ||
      honestLeaf === undefined ||
      otherReasonLeaf === undefined
    )
      throw new Error("forced leaves absent");
    const published = await publishFamilyReferences(
      context,
      recorder,
      "output-reference-forced-node",
    );
    const stages = makeOutputReferenceStages({
      context,
      fraudulentBlockOutRef: block.setup.fraudulentBlockOutRef,
      ...published,
    });
    const evidence = forcedEvidence(accused);
    expect(evidence.resultClass).toBe(
      OutputReferenceScriptResultClasses.NoFault,
    );
    expect(evidence.accusedClass).toBe(
      OutputReferenceScriptResultClasses.NodeLimit,
    );
    expect(evidence.chunkProofCount).toBeGreaterThan(1);

    progress("forced-door seams");
    const thread = stages.threadOf((await stages.init()).result);
    const bound = {
      subject: evidence.subject,
      outputIndex: 0,
      accusedClass: evidence.accusedClass,
    };
    const forcedSource = {
      header: block.header,
      membership: accused.membership,
      direction: 1n,
    };
    const door = async (
      seam: (typeof AUTHENTICATION_SEAMS)[number],
      patch: Partial<Parameters<typeof stages.step01ForcedRaw>[0]>,
    ) => {
      await expectOnchainRefusal(() =>
        stages.step01ForcedRaw({
          threadOutRef: thread,
          bound,
          forcedSource,
          ...patch,
        }),
      );
      coverage.seamMutated(seam);
    };
    // The leaf commits output 0; a datum or redeemer naming output 1 is
    // refused by the exact reason binding, off chain and on chain.
    expect(() =>
      prepareOutputReferenceScriptDecodingEvidence({
        subject: evidence.subject,
        outputIndex: 1,
        canonicalTransactionCbor: accused.canonicalCbor,
      }),
    ).toThrow(/coordinate differs/u);
    await door("forced_leaf_reason_coordinate", {
      bound: { ...bound, outputIndex: 1 },
    });
    await door("forced_leaf_reason_coordinate", { claimedOutputIndex: 1 });
    // A leaf committed under another family's typed reason.
    await door("forced_leaf_reason_coordinate", {
      bound: {
        subject: forcedVerdictSubject({
          transactionId: otherReasonLeaf.transaction.tx_id,
          sourceKey: otherReasonLeaf.membership.key,
          rejectionReason: otherReasonLeaf.reason,
        }),
        outputIndex: 0,
        accusedClass: OutputReferenceScriptResultClasses.Malformed,
      },
      forcedSource: {
        ...forcedSource,
        membership: otherReasonLeaf.membership,
      },
    });
    coverage.scenario("reason_or_subject_coordinate_mutation");
    await door("forced_leaf_direction", {
      bound: { ...bound, subject: { ...evidence.subject, direction: 0n } },
      forcedSource: { ...forcedSource, direction: 0n },
    });
    await door("forced_leaf_header", {
      forcedSource: {
        ...forcedSource,
        header: { ...block.header, validationTracesRoot: "ff".repeat(32) },
      },
    });
    await door("forced_leaf_root", {
      forcedSource: {
        ...forcedSource,
        membership: await foreignForcedMembership(accused.membership),
      },
    });

    progress("forced chain to the native scan");
    const s1 = await stages.step01Forced(
      thread,
      evidence,
      accused,
      block.header,
    );
    record("forced-step01", s1.measurement);
    const s2 = await stages.step02(
      s1.result.nextThreadOutRef,
      evidence,
      forcedSourceOf(accused),
    );
    recordCarriage("forced-step02", s2);
    record("forced-step02-certified", s2.measurement);
    const s3 = await stages.step03Loop(
      s2.result.nextThreadOutRef,
      evidence,
      (window, index) =>
        record(`forced-step03-window-${index.toString()}`, window.measurement),
    );
    const s4 = await stages.step04(
      s3.threadOutRef,
      evidence,
      accused.transaction.submitted_source.compact_cbor,
      s2.result,
    );
    record("forced-step04-certified-reference-bind", s4.measurement);

    progress("scan successor seams");
    const scanThread = s4.result.nextThreadOutRef;
    const state = await scanStateOf(context, scanThread);
    const planned = planOutputReferenceScriptDecodingStep05({
      contracts: context.contracts,
      state,
      evidence,
    });
    expect(planned.nextStepIndex).toBe(4);
    const step06Hash = context.contracts.steps[5].spendingScriptHash;
    const closingAt = (controlCbor: string) => ({
      ...state,
      control_cbor: controlCbor,
      next_expected_script_hash: step06Hash,
      checkpoint_hash: outputReferenceScriptCheckpoint({
        evidence,
        controlCbor,
        nextExpectedScriptHash: step06Hash,
      }),
      result_class: BigInt(OutputReferenceScriptResultClasses.NoFault),
    });
    // A non-terminal scan segment may not claim step 06 ...
    await expectOnchainRefusal(() =>
      stages.step05Raw(
        scanThread,
        planned.args,
        closingAt(planned.nextState.control_cbor),
        5,
      ),
    );
    coverage.seamMutated("wrong_successor");
    // ... and a pending scan may not close without scanning.
    await expectOnchainRefusal(() =>
      stages.step05Raw(
        scanThread,
        {
          control_cbor: "",
          chunk_proof: null,
          next_chunk_proof: null,
          frames: [],
          step_budget: 0n,
        },
        closingAt(state.control_cbor),
        5,
      ),
    );
    coverage.seamMutated("premature_close");

    progress("resumable native scan");
    const s5 = await stages.step05Loop(scanThread, evidence, (scan, index) => {
      if (scan.result.closed)
        record("forced-step05-close-exact-end", scan.measurement);
      else if (index < 3 || index % 16 === 0)
        record(
          `forced-step05-scan-${index.toString().padStart(2, "0")}`,
          scan.measurement,
        );
      else
        expect(
          scan.measurement.l1ByteMargin,
          `scan ${index.toString()}`,
        ).toBeGreaterThan(0);
    });
    expect(s5.scans).toBeGreaterThan(1);
    coverage.resumed();
    const s6 = await stages.step06(s5.threadOutRef, evidence);
    expect(s6.result.fraudProofUnit).toBeTruthy();
    record("forced-step06-final-mint", s6.measurement);
    coverage.reason("OutputReferenceScriptNodeLimit", "forced_rejection_wrong");
    coverage.scenario("wrongful_forced_rejection_success");

    progress("honest forced rejection");
    expect(() => forcedEvidence(honestLeaf)).toThrow(
      /agrees with operator verdict/u,
    );
    const lying = forcedEvidence(honestLeaf, true);
    expect(lying.resultClass).toBe(
      OutputReferenceScriptResultClasses.Malformed,
    );
    const h1 = await stages.step01Forced(
      stages.threadOf((await stages.init()).result),
      lying,
      honestLeaf,
      block.header,
    );
    const h2 = await stages.step02(
      h1.result.nextThreadOutRef,
      lying,
      forcedSourceOf(honestLeaf),
    );
    const h3 = await stages.step03Loop(h2.result.nextThreadOutRef, lying);
    const h4 = await stages.step04(
      h3.threadOutRef,
      lying,
      honestLeaf.transaction.submitted_source.compact_cbor,
      h2.result,
    );
    const honestState = await scanStateOf(context, h4.result.nextThreadOutRef);
    const honestItem = Buffer.from(lying.referenceScriptItemHex, "hex");
    const honestPlan = buildNativeScriptDecodingScanPlan({
      itemBytes: honestItem,
      direction: 0,
    });
    expect(honestPlan.verdict.refusalClass).toBe(
      OutputReferenceScriptResultClasses.Malformed,
    );
    const closed = await stages.step05Raw(
      h4.result.nextThreadOutRef,
      {
        control_cbor: honestState.control_cbor,
        ...nativeScriptDecodingWindowProofs({
          window: honestPlan.verdict.window,
          fieldIndex: 2,
          itemIndex: 0,
          itemBytes: honestItem,
        }),
        frames: [],
        step_budget: 1n,
      },
      {
        ...honestState,
        next_expected_script_hash: step06Hash,
        checkpoint_hash: outputReferenceScriptCheckpoint({
          evidence: lying,
          controlCbor: honestState.control_cbor,
          nextExpectedScriptHash: step06Hash,
        }),
        result_class: BigInt(OutputReferenceScriptResultClasses.Malformed),
      },
      5,
    );
    await expect(stages.step06(closed.nextThreadOutRef, lying)).rejects.toThrow(
      /retained contradiction/u,
    );
    await expectOnchainRefusal(() => stages.step06Raw(closed.nextThreadOutRef));
    coverage.scenario("honest_forced_rejection_refusal");
    record(
      "forced-cancel-step06",
      (await stages.cancel(closed.nextThreadOutRef, 5)).measurement,
      { maximumShape: SMALL_SHAPE },
    );
    coverage.cancelled("step-06");
  }, 1_800_000);

  it("contradicts a wrongful forced DepthLimit rejection of nested containers through the frame stack", async () => {
    const context = await makeOutputReferenceHarness();
    const block = await commitForcedBlock(context, [
      {
        nativeTx: subjectTransaction([
          outputWithNativeScript(nestedScript(12)),
        ]),
        reason: forcedReason("OutputReferenceScriptDepthLimit"),
      },
    ]);
    const leaf = block.leaves[0];
    if (leaf === undefined) throw new Error("forced leaf absent");
    const published = await publishFamilyReferences(
      context,
      recorder,
      "output-reference-forced-depth",
    );
    const stages = makeOutputReferenceStages({
      context,
      fraudulentBlockOutRef: block.setup.fraudulentBlockOutRef,
      ...published,
    });
    const evidence = forcedEvidence(leaf);
    expect(evidence.accusedClass).toBe(
      OutputReferenceScriptResultClasses.DepthLimit,
    );
    const small = {
      maximumShape: "12-deep nested all-of containers, Inline carriage",
    };
    const s1 = await stages.step01Forced(
      stages.threadOf((await stages.init()).result),
      evidence,
      leaf,
      block.header,
    );
    record("forced-depth-step01", s1.measurement, small);
    const s2 = await stages.step02(
      s1.result.nextThreadOutRef,
      evidence,
      forcedSourceOf(leaf),
    );
    record("forced-depth-step02", s2.measurement, small);
    const s3 = await stages.step03Loop(s2.result.nextThreadOutRef, evidence);
    const s4 = await stages.step04(
      s3.threadOutRef,
      evidence,
      leaf.transaction.submitted_source.compact_cbor,
      s2.result,
    );
    record("forced-depth-step04", s4.measurement, small);
    const s5 = await stages.step05Loop(
      s4.result.nextThreadOutRef,
      evidence,
      (scan, index) =>
        record(
          scan.result.closed
            ? "forced-depth-step05-close"
            : `forced-depth-step05-scan-${index.toString()}`,
          scan.measurement,
          small,
        ),
    );
    expect(s5.scans).toBeGreaterThan(1);
    const s6 = await stages.step06(s5.threadOutRef, evidence);
    expect(s6.result.fraudProofUnit).toBeTruthy();
    record("forced-depth-step06-final-mint", s6.measurement, small);
    coverage.reason(
      "OutputReferenceScriptDepthLimit",
      "forced_rejection_wrong",
    );
  }, 900_000);

  it("contradicts a wrongful forced Malformed rejection of a decodable signature script", async () => {
    const context = await makeOutputReferenceHarness();
    const block = await commitForcedBlock(context, [
      {
        nativeTx: subjectTransaction([
          outputWithNativeScript(signatureScript()),
        ]),
        reason: forcedReason("OutputReferenceScriptMalformed"),
      },
    ]);
    const leaf = block.leaves[0];
    if (leaf === undefined) throw new Error("forced leaf absent");
    const published = await publishFamilyReferences(
      context,
      recorder,
      "output-reference-forced",
    );
    const stages = makeOutputReferenceStages({
      context,
      fraudulentBlockOutRef: block.setup.fraudulentBlockOutRef,
      ...published,
    });
    expect(() =>
      prepareOutputReferenceScriptDecodingEvidence({
        subject: acceptedVerdictSubject(leaf.transaction.tx_id),
        outputIndex: 0,
        canonicalTransactionCbor: encodeMidgardNativeTxCanonical(leaf.nativeTx),
      }),
    ).toThrow(/agrees with operator verdict/u);
    expect(() =>
      prepareOutputReferenceScriptDecodingEvidence({
        subject: forcedVerdictSubject({
          transactionId: "ff".repeat(32),
          sourceKey: leaf.membership.key,
          rejectionReason: leaf.reason,
        }),
        outputIndex: 0,
        canonicalTransactionCbor: leaf.canonicalCbor,
      }),
    ).toThrow(/identity was substituted/u);
    const evidence = forcedEvidence(leaf);
    expect(evidence.resultClass).toBe(
      OutputReferenceScriptResultClasses.NoFault,
    );
    const small = { maximumShape: "signature script, Inline carriage" };
    const s1 = await stages.step01Forced(
      stages.threadOf((await stages.init()).result),
      evidence,
      leaf,
      block.header,
    );
    record("forced-sig-step01", s1.measurement, small);
    const s2 = await stages.step02(
      s1.result.nextThreadOutRef,
      evidence,
      forcedSourceOf(leaf),
    );
    record("forced-sig-step02", s2.measurement, small);
    const s3 = await stages.step03Loop(s2.result.nextThreadOutRef, evidence);
    const s4 = await stages.step04(
      s3.threadOutRef,
      evidence,
      leaf.transaction.submitted_source.compact_cbor,
      s2.result,
    );
    record("forced-sig-step04", s4.measurement, small);
    const s5 = await stages.step05(s4.result.nextThreadOutRef, evidence);
    expect(s5.result.closed).toBe(true);
    record("forced-sig-step05-close", s5.measurement, small);
    const s6 = await stages.step06(s5.result.nextThreadOutRef, evidence);
    expect(s6.result.fraudProofUnit).toBeTruthy();
    record("forced-sig-step06-final-mint", s6.measurement, small);
    coverage.reason("OutputReferenceScriptMalformed", "forced_rejection_wrong");
    coverage.scenario("wrongful_forced_rejection_success");
  }, 600_000);

  it("declares the lifecycle coverage it exercised and reproduces the Van Rossem fit ledger", async () => {
    // Recorded while the suites above ran, never pre-filled. The only
    // omissions are the wrongful-ACCEPTANCE directions of the NodeLimit and
    // DepthLimit arms: `native_script_scan_v1` bounds both at 16,384 and a
    // node costs at least three bytes, so no output inside the 16,384-byte
    // consensus bound can reach either limit
    // (`one_shot_default_bounds_are_the_staged_machine_bounds`). No accepted
    // block can carry such a fault, and §5.3 forbids starting from a
    // fabricated mid-thread datum; the exact and adjacent node/depth edges
    // are pinned at the rule level instead.
    expect(() =>
      assertCompleteLifecycleCoverage({
        coverage: coverage.snapshot(),
        expectedReasonArms: [...OUTPUT_REFERENCE_REASON_ARMS],
        authenticationSeams: [...AUTHENTICATION_SEAMS],
        cancellablePhysicalSteps: [...CANCELLABLE_STEPS],
        resumable: true,
        hasAdjacentConsensusBound: true,
      }),
    ).toThrow(
      "incomplete fault-proof lifecycle coverage: OutputReferenceScriptNodeLimit success directions: accepted_invalid; OutputReferenceScriptDepthLimit success directions: accepted_invalid",
    );
    const blueprintBytes = readFileSync(realBlueprintPath);
    const preamble = JSON.parse(blueprintBytes.toString("utf8")) as {
      readonly preamble?: { readonly compiler?: { readonly version?: string } };
    };
    const ledger = buildVanRossemFitLedger({
      category: `outputReferenceScriptDecoding:${OUTPUT_REFERENCE_CATEGORY_ID}:testnet`,
      blueprintSha256: createHash("sha256")
        .update(blueprintBytes)
        .digest("hex"),
      compilerVersion: `aiken ${preamble.preamble?.compiler?.version ?? "unknown"}`,
      measurements: recorder.measurements,
    });
    expect(ledger.entries.length).toBeGreaterThan(30);
    for (const entry of ledger.entries) {
      expect(entry.signedByteMargin, entry.name).toBeGreaterThan(0);
      expect(BigInt(entry.memoryUnitMargin), entry.name).toBeGreaterThan(0n);
      expect(BigInt(entry.cpuUnitMargin), entry.name).toBeGreaterThan(0n);
      if (entry.kind === "publication")
        expect(
          entry.publicationReserveMargin,
          entry.name,
        ).toBeGreaterThanOrEqual(0);
    }
    if (process.env["MIDGARD_WRITE_FIT_LEDGER"] === "1") {
      await writeVanRossemFitLedger(ledgerPath, ledger);
      console.info(
        `[output-reference-script-decoding-fit-ledger] wrote ${ledgerPath}`,
      );
    }
    console.info(
      `[output-reference-script-decoding-fit-ledger] ${JSON.stringify(ledger.entries)}`,
    );
  });
});
