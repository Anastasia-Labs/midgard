/**
 * `executionSourceScriptDecoding` registered-chain lifecycle (§5.3 + Wave 4).
 *
 * Every journey runs from the generic `Init` through the five applied
 * validators the catalogue registers, on the shared Van Rossem emulator
 * parameters with local UPLC evaluation. Refusals are asserted on chain
 * (`expectOnchainRefusal`), never through an off-chain guard alone. Coverage
 * is recorded while the journeys run and declared at the end.
 */
import { createHash } from "node:crypto";
import { readFileSync } from "node:fs";
import { fileURLToPath } from "node:url";

import {
  encodeRetainedValidationWitness,
  encodeRetainedValidationWitnessKey,
  EventKeySchema,
  type RetainedValidationWitness,
  type RetainedValidationWitnessKey,
  ValidationAuxiliaryWitnessSchema,
  validationMachineStateDataFromCore,
  ValidationTraceDescriptorSchema,
  validationTraceProofDataFromCore,
} from "@al-ft/midgard-sdk";
import { validationAuxiliaryWitnessData } from "@al-ft/midgard-validation";
import { Data } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import type { CanonicalBlockEvidence } from "../src/evidence/canonical-block-evidence.js";
import {
  executionSourceAuthenticatedSource,
  executionSourceScriptDecodingCheckpoint,
  ExecutionSourceScriptDecodingResultClasses,
  prepareExecutionSourceScriptDecodingArtifact,
  prepareExecutionSourceScriptDecodingEvidence,
} from "../src/execution-source-script-decoding/index.js";
import { buildNativeScriptDecodingChunkProof } from "../src/native-script-decoding/evidence.js";
import { buildNativeScriptDecodingScanPlan } from "../src/native-script-decoding/scan-plan.js";
import {
  buildVanRossemFitLedger,
  writeVanRossemFitLedger,
} from "../src/proof-fit/van-rossem-fit-ledger.js";
import { assertCompleteLifecycleCoverage } from "../src/testing/complete-lifecycle.js";
import {
  buildSubjectFixture,
  commitSubjectBlock,
  createMeasurementRecorder,
  emptyAllScript,
  EXECUTION_SOURCE_CATEGORY_ID,
  EXECUTION_SOURCE_MAX_FIELD_BYTES,
  EXECUTION_SOURCE_REASON_ARMS,
  type ExecutionSourceContext,
  expectOnchainRefusal,
  forcedReason,
  foreignForcedMembership,
  makeExecutionSourceHarness,
  makeExecutionSourceStages,
  maximumMalformedItem,
  maximumWideScript,
  type Measurement,
  nestedScript,
  publishFamilyReferences,
  rawNativeItem,
  type SubjectFixture,
} from "./support/execution-source-script-decoding-emulator.js";
import { createLifecycleCoverageRecorder } from "./support/lifecycle-coverage.js";
import { realBlueprintPath } from "./support/submit-init-emulator-shared.js";

const AUTHENTICATION_SEAMS = [
  "tx_membership",
  "validation_trace_root",
  "machine_state",
  "execution_descriptor",
  "forced_leaf_root",
  "forced_leaf_header",
  "forced_leaf_direction",
  "forced_leaf_reason_coordinate",
  "source_item_chunk",
  "bind_result",
  "scan_window_chunk",
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
] as const;
const ledgerPath = fileURLToPath(
  new URL(
    "../../../docs/fault-proofs/size-plans/execution-source-script-decoding-v1-fit-ledger.json",
    import.meta.url,
  ),
);

const coverage = createLifecycleCoverageRecorder();
const recorder = createMeasurementRecorder();
const { record } = recorder;
const progress = (message: string) =>
  console.info(`[execution-source-script-decoding-progress] ${message}`);
const Classes = ExecutionSourceScriptDecodingResultClasses;

/** The block, references and stages one subject fixture is driven through. */
const stand = async (
  context: ExecutionSourceContext,
  fixture: SubjectFixture,
  label: string,
) => {
  const setup = await commitSubjectBlock(context, fixture);
  const references = await publishFamilyReferences(context, recorder, label);
  return makeExecutionSourceStages({ context, setup, references });
};
/** The step-02 datum a thread bound to `fixture` carries at `executionIndex`. */
const boundOf = (fixture: SubjectFixture, executionIndex = 0n) => ({
  subject: fixture.subject,
  validation_traces_root: fixture.header.validationTracesRoot,
  validation_trace_count: fixture.header.validationTraceCount,
  execution_index: executionIndex,
  accused_class: BigInt(fixture.evidence.finding.accusedClass),
});
/** The step-04 state step 03 opens for `fixture`, given its validators. */
const bindStateOf = (
  context: ExecutionSourceContext,
  fixture: SubjectFixture,
) => {
  const { evidence } = fixture;
  const nextExpectedScriptHash = context.contracts.steps[3].spendingScriptHash;
  return {
    source: executionSourceAuthenticatedSource(
      boundOf(fixture),
      fixture.authentication.authentication,
    ),
    control_cbor: evidence.initialControlCbor,
    next_expected_script_hash: nextExpectedScriptHash,
    checkpoint_hash: executionSourceScriptDecodingCheckpoint({
      evidence,
      controlCbor: evidence.initialControlCbor,
      nextExpectedScriptHash,
    }),
    result_class: BigInt(
      evidence.initialControlCbor === ""
        ? evidence.resultClass
        : Classes.Pending,
    ),
  };
};
const chunkOf = (fixture: SubjectFixture, chunkIndex: number) =>
  buildNativeScriptDecodingChunkProof({
    fieldIndex: 6,
    itemIndex: fixture.evidence.descriptor.sourceIndex,
    itemBytes: fixture.scriptItem,
    chunkIndex,
  });

describe("executionSourceScriptDecoding registered-chain lifecycle", () => {
  it("contradicts a wrongful acceptance of a malformed source item at the 32,768-byte field cap: refuses every accepted seam, closes the two-chunk verdict window, mints and removes", async () => {
    const context = await makeExecutionSourceHarness();
    const item = maximumMalformedItem();
    const fixture = await buildSubjectFixture({
      harness: context.harness,
      direction: "accepted",
      item: { kind: "raw", item },
    });
    expect(fixture.canonicalVerdict).toBe("rejected");
    expect(fixture.evidence.resultClass).toBe(Classes.Malformed);
    expect(fixture.evidence.initialControlCbor).not.toBe("");
    expect(fixture.evidence.chunkProofCount).toBe(9);
    expect(fixture.evidence.itemLength).toBeGreaterThan(
      EXECUTION_SOURCE_MAX_FIELD_BYTES - 8,
    );
    coverage.scenario("maximum_supported_evidence");
    const stages = await stand(context, fixture, "execution-source-accepted");

    progress("init and transaction-membership seam");
    const initialized = await stages.init();
    record("init", initialized.measurement);
    const thread = initialized.result.nextThreadOutRef;
    const inclusion = fixture.block.txInclusion;
    if (inclusion === null) throw new Error("accepted inclusion absent");
    await expectOnchainRefusal(() =>
      stages.step01Accepted(thread, fixture, 0n, {
        ...inclusion,
        transactionsPhasRoot: "11".repeat(32),
      }),
    );
    coverage.seamMutated("tx_membership");
    const s1 = await stages.step01Accepted(thread, fixture);
    record("step01-accepted", s1.measurement);

    progress("step 02 seams");
    const authentication = fixture.authentication.authentication;
    // A substituted script hash breaks the purpose leaf; a foreign validation
    // root breaks the counted-root binding; a tampered machine state breaks
    // the state hash the trace proof commits.
    await expectOnchainRefusal(() =>
      stages.step02(s1.result.nextThreadOutRef, fixture, {
        ...authentication,
        script_hash: "ff".repeat(28),
      }),
    );
    coverage.seamMutated("execution_descriptor");
    await expectOnchainRefusal(() =>
      stages.step02(s1.result.nextThreadOutRef, fixture, {
        ...authentication,
        trace_membership: {
          ...authentication.trace_membership,
          root: "22".repeat(32),
        },
      }),
    );
    coverage.seamMutated("validation_trace_root");
    await expectOnchainRefusal(() =>
      stages.step02(s1.result.nextThreadOutRef, fixture, {
        ...authentication,
        machine_state: {
          ...authentication.machine_state,
          prior_ledger_root: "33".repeat(32),
        },
      }),
    );
    coverage.seamMutated("machine_state");

    progress("out-of-range execution coordinate");
    // Step 01 binds any accepted coordinate; step 02 refuses one past the
    // control's execution count on chain. The classified builder refuses it
    // before that, so the raw entry drives the validator.
    expect(() =>
      prepareExecutionSourceScriptDecodingEvidence({
        finding: { subject: fixture.subject, executionIndex: 1 },
        descriptor: fixture.evidence.descriptor,
      }),
    ).toThrow(/membership was substituted/u);
    const rangeThread = await stages.step01Accepted(
      await stages.freshThread(),
      fixture,
      1n,
    );
    await expectOnchainRefusal(() =>
      stages.step02Raw({
        threadOutRef: rangeThread.result.nextThreadOutRef,
        authentication,
        source: executionSourceAuthenticatedSource(
          boundOf(fixture, 1n),
          authentication,
        ),
      }),
    );
    coverage.scenario("reason_or_subject_coordinate_mutation");
    record(
      "cancel-step02",
      (await stages.cancel(rangeThread.result.nextThreadOutRef, 1)).measurement,
    );
    coverage.cancelled("step-02");

    const s2 = await stages.step02(s1.result.nextThreadOutRef, fixture);
    record("step02-authenticate", s2.measurement);

    progress("step 03 seams");
    const bindState = bindStateOf(context, fixture);
    const firstChunk = chunkOf(fixture, 0);
    const forgedChunk = Buffer.from(firstChunk.chunk, "hex");
    forgedChunk[8] = forgedChunk[8]! ^ 0x01;
    await expectOnchainRefusal(() =>
      stages.step03Raw({
        threadOutRef: s2.result.nextThreadOutRef,
        firstChunk: { ...firstChunk, chunk: forgedChunk.toString("hex") },
        nextState: bindState,
      }),
    );
    coverage.seamMutated("source_item_chunk");
    // A bind that claims no fault for a bound native payload contradicts the
    // engine's own bind result.
    await expectOnchainRefusal(() =>
      stages.step03Raw({
        threadOutRef: s2.result.nextThreadOutRef,
        firstChunk,
        nextState: {
          ...bindState,
          control_cbor: "",
          checkpoint_hash: executionSourceScriptDecodingCheckpoint({
            evidence: fixture.evidence,
            controlCbor: "",
            nextExpectedScriptHash: bindState.next_expected_script_hash,
          }),
          result_class: BigInt(Classes.NoFault),
        },
      }),
    );
    coverage.seamMutated("bind_result");
    const s3 = await stages.step03(s2.result.nextThreadOutRef, fixture);
    record("step03-open-item", s3.measurement);
    expect(
      (await stages.scanState(s3.result.nextThreadOutRef)).result_class,
    ).toBe(BigInt(Classes.Pending));

    progress("step 04 seams and the two-chunk verdict window");
    const scanThread = s3.result.nextThreadOutRef;
    const planned = await stages.plan04(scanThread, fixture);
    expect(planned.closes).toBe(true);
    expect(planned.args.chunk_proof).not.toBeNull();
    expect(planned.args.next_chunk_proof).not.toBeNull();
    // The third chunk in place of the cursor's adjacent chunk: the bounded
    // item coordinate check refuses it.
    await expectOnchainRefusal(() =>
      stages.step04Raw({
        threadOutRef: scanThread,
        args: { ...planned.args, next_chunk_proof: chunkOf(fixture, 2) },
        nextState: planned.nextState,
        nextStepIndex: planned.nextStepIndex,
      }),
    );
    coverage.seamMutated("scan_window_chunk");
    await expectOnchainRefusal(() =>
      stages.step04Raw({
        threadOutRef: scanThread,
        args: planned.args,
        nextState: { ...planned.nextState, checkpoint_hash: "44".repeat(32) },
        nextStepIndex: planned.nextStepIndex,
      }),
    );
    coverage.seamMutated("scan_checkpoint");
    // A refusal must close to step 05; a self-loop successor is refused.
    const selfHash = context.contracts.steps[3].spendingScriptHash;
    await expectOnchainRefusal(() =>
      stages.step04Raw({
        threadOutRef: scanThread,
        args: planned.args,
        nextState: {
          ...planned.nextState,
          next_expected_script_hash: selfHash,
          checkpoint_hash: executionSourceScriptDecodingCheckpoint({
            evidence: fixture.evidence,
            controlCbor: planned.nextState.control_cbor,
            nextExpectedScriptHash: selfHash,
          }),
        },
        nextStepIndex: 3,
      }),
    );
    coverage.seamMutated("wrong_successor");
    const s4 = await stages.step04(scanThread, fixture);
    expect(s4.result.closed).toBe(true);
    record("step04-verdict-malformed", s4.measurement);
    expect(
      (await stages.scanState(s4.result.nextThreadOutRef, 4)).result_class,
    ).toBe(BigInt(Classes.Malformed));

    progress("mint and removal");
    const s5 = await stages.step05(s4.result.nextThreadOutRef, fixture);
    expect(s5.result.txHash).toMatch(/^[0-9a-f]{64}$/u);
    record("step05-mint", s5.measurement);
    coverage.reason("ExecutionNativeScriptMalformed", "accepted_invalid");
    coverage.scenario("wrongful_acceptance_success");
    const removal = await stages.remove();
    expect(removal.result.txHash).toMatch(/^[0-9a-f]{64}$/u);
    record("remove", removal.measurement);
    coverage.scenario("permanent_proof_token_and_descendant_removal");
  }, 900_000);

  it("contradicts a wrongful forced NodeLimit rejection of the widest any-of script: refuses every forced-door seam, a substituted window, a premature close and a wrong successor, and resumes the scan across all nine chunk windows", async () => {
    const context = await makeExecutionSourceHarness();
    const wide = maximumWideScript();
    expect(wide.fieldBytes).toBeGreaterThan(
      EXECUTION_SOURCE_MAX_FIELD_BYTES - 40,
    );
    const fixture = await buildSubjectFixture({
      harness: context.harness,
      direction: "forced",
      item: { kind: "script", script: wide.script },
      reason: forcedReason("ExecutionNativeScriptNodeLimit"),
    });
    expect(fixture.canonicalVerdict).toBe("accepted");
    expect(fixture.evidence.resultClass).toBe(Classes.NoFault);
    expect(fixture.evidence.finding.accusedClass).toBe(Classes.NodeLimit);
    expect(fixture.evidence.chunkProofCount).toBe(9);
    coverage.scenario("maximum_supported_evidence");
    const stages = await stand(
      context,
      fixture,
      "execution-source-forced-node",
    );
    if (fixture.membership === null) throw new Error("forced leaf absent");

    progress("forced-door seams");
    const thread = await stages.freshThread();
    const bound = {
      subject: fixture.subject,
      header: fixture.header,
      executionIndex: 0n,
      accusedClass: BigInt(Classes.NodeLimit),
    };
    const forcedSource = {
      header: fixture.header,
      membership: fixture.membership,
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
    // The leaf's typed reason names execution 0; a datum or redeemer naming
    // execution 1, or a datum carrying another arm, is refused by the exact
    // reason binding.
    await door("forced_leaf_reason_coordinate", {
      bound: { ...bound, executionIndex: 1n },
    });
    await door("forced_leaf_reason_coordinate", { claimedExecutionIndex: 1n });
    await door("forced_leaf_reason_coordinate", {
      bound: {
        ...bound,
        subject: {
          ...fixture.subject,
          rejection_reason: forcedReason("ExecutionNativeScriptMalformed"),
        },
        accusedClass: BigInt(Classes.Malformed),
      },
    });
    coverage.scenario("reason_or_subject_coordinate_mutation");
    await door("forced_leaf_direction", {
      bound: { ...bound, subject: { ...fixture.subject, direction: 0n } },
      forcedSource: { ...forcedSource, direction: 0n },
    });
    const otherHeader = {
      ...fixture.header,
      startTime: fixture.header.startTime + 1n,
    };
    await door("forced_leaf_header", {
      bound: { ...bound, header: otherHeader },
      forcedSource: { ...forcedSource, header: otherHeader },
    });
    await door("forced_leaf_root", {
      forcedSource: {
        ...forcedSource,
        membership: await foreignForcedMembership(fixture.membership),
      },
    });
    const s1 = await stages.step01Forced(thread, fixture);
    record("forced-step01", s1.measurement);
    const s2 = await stages.step02(s1.result.nextThreadOutRef, fixture);
    record("forced-step02-authenticate", s2.measurement);
    const s3 = await stages.step03(s2.result.nextThreadOutRef, fixture);
    record("forced-step03-open-item", s3.measurement);

    progress("scan seams");
    const scanThread = s3.result.nextThreadOutRef;
    const planned = await stages.plan04(scanThread, fixture);
    expect(planned.closes).toBe(false);
    expect(planned.args.chunk_proof).not.toBeNull();
    // The adjacent chunk in place of the cursor's chunk.
    await expectOnchainRefusal(() =>
      stages.step04Raw({
        threadOutRef: scanThread,
        args: {
          ...planned.args,
          chunk_proof: chunkOf(fixture, 1),
          next_chunk_proof: chunkOf(fixture, 2),
        },
        nextState: planned.nextState,
        nextStepIndex: 3,
      }),
    );
    coverage.seamMutated("scan_window_chunk");
    // Closing no-fault before the exact terminal.
    const step05Hash = context.contracts.steps[4].spendingScriptHash;
    await expectOnchainRefusal(() =>
      stages.step04Raw({
        threadOutRef: scanThread,
        args: planned.args,
        nextState: {
          ...planned.nextState,
          control_cbor: planned.args.control_cbor,
          next_expected_script_hash: step05Hash,
          checkpoint_hash: executionSourceScriptDecodingCheckpoint({
            evidence: fixture.evidence,
            controlCbor: planned.args.control_cbor,
            nextExpectedScriptHash: step05Hash,
          }),
          result_class: BigInt(Classes.NoFault),
        },
        nextStepIndex: 4,
      }),
    );
    coverage.seamMutated("premature_close");
    // Advancing towards step 05 instead of the self-loop successor.
    await expectOnchainRefusal(() =>
      stages.step04Raw({
        threadOutRef: scanThread,
        args: planned.args,
        nextState: {
          ...planned.nextState,
          next_expected_script_hash: step05Hash,
          checkpoint_hash: executionSourceScriptDecodingCheckpoint({
            evidence: fixture.evidence,
            controlCbor: planned.nextState.control_cbor,
            nextExpectedScriptHash: step05Hash,
          }),
        },
        nextStepIndex: 3,
      }),
    );
    coverage.seamMutated("wrong_successor");

    progress("resumable scan across the chunk windows");
    let widest: Measurement | null = null;
    const s4 = await stages.step04Loop(scanThread, fixture, (scan, index) => {
      if (index === 0) record("forced-step04-scan-00", scan.measurement);
      if (scan.result.closed)
        record("forced-step04-close-exact-end", scan.measurement);
      if (
        widest === null ||
        scan.measurement.completeSignedBytes > widest.completeSignedBytes
      )
        widest = scan.measurement;
      if (index % 25 === 0) progress(`scan ${index.toString()} submitted`);
    });
    expect(s4.scans).toBeGreaterThan(9);
    if (widest !== null) record("forced-step04-scan-widest-window", widest);
    coverage.resumed();
    expect((await stages.scanState(s4.threadOutRef, 4)).result_class).toBe(
      BigInt(Classes.NoFault),
    );
    const s5 = await stages.step05(s4.threadOutRef, fixture);
    expect(s5.result.txHash).toMatch(/^[0-9a-f]{64}$/u);
    record("forced-step05-mint", s5.measurement);
    coverage.reason("ExecutionNativeScriptNodeLimit", "forced_rejection_wrong");
    coverage.scenario("wrongful_forced_rejection_success");

    progress("retained-DA replay of the forced witness");
    // The production evidence path reconstructs the same authentication from
    // the retained validation witness bundle alone.
    const stateIndex = fixture.trace.witnesses.findIndex(
      ({ phase, auxiliary }) =>
        phase === "nativeScripts" &&
        auxiliary?.kind === "nativeExecutionDescriptor",
    );
    const retained = fixture.trace.witnesses[stateIndex];
    const retainedState = fixture.trace.states[stateIndex];
    const retainedProof = fixture.trace.tree.proofs[stateIndex];
    if (
      retained?.auxiliary?.kind !== "nativeExecutionDescriptor" ||
      retainedState === undefined ||
      retainedProof === undefined
    )
      throw new Error("forced retained witness fixture is incomplete");
    const retainedKey: RetainedValidationWitnessKey = {
      event_key: fixture.eventKey,
      execution_index: 0n,
    };
    const retainedValue: RetainedValidationWitness = {
      machine_state: validationMachineStateDataFromCore(retainedState),
      trace_proof: validationTraceProofDataFromCore(retainedProof),
      phase: 9n,
      program_counter: BigInt(retained.programCounter),
      witness_cbor: retained.cbor.toString("hex"),
      auxiliary: Data.from(
        Data.to(validationAuxiliaryWitnessData(retained.auxiliary) as never),
        ValidationAuxiliaryWitnessSchema,
      ) as unknown as RetainedValidationWitness["auxiliary"],
    };
    const membership = fixture.authentication.authentication.trace_membership;
    const artifact = await prepareExecutionSourceScriptDecodingArtifact({
      headerHash: fixture.block.headerHash,
      header: fixture.header,
      reconstruction: {
        ...fixture.block.reconstruction,
        payload: {
          ...fixture.block.reconstruction.payload,
          block_body: {
            ...fixture.block.reconstruction.payload.block_body,
            validation_traces: [
              [
                Data.to(membership.key as never, EventKeySchema),
                Data.to(
                  membership.value as never,
                  ValidationTraceDescriptorSchema,
                ),
              ],
            ],
            validation_trace_witnesses: [
              [
                encodeRetainedValidationWitnessKey(retainedKey).toString("hex"),
                encodeRetainedValidationWitness(retainedValue).toString("hex"),
              ],
            ],
          },
        },
      },
      transactions: [],
    } as unknown as CanonicalBlockEvidence);
    expect(artifact.forcedMembership).toBeDefined();
    expect(artifact.acceptedInclusion).toBeUndefined();
  }, 1_800_000);

  it("contradicts a wrongful forced DepthLimit rejection of nested containers through the frame stack", async () => {
    const context = await makeExecutionSourceHarness();
    const fixture = await buildSubjectFixture({
      harness: context.harness,
      direction: "forced",
      item: { kind: "script", script: nestedScript(40) },
      reason: forcedReason("ExecutionNativeScriptDepthLimit"),
    });
    expect(fixture.canonicalVerdict).toBe("accepted");
    expect(fixture.evidence.resultClass).toBe(Classes.NoFault);
    expect(fixture.evidence.finding.accusedClass).toBe(Classes.DepthLimit);
    const stages = await stand(
      context,
      fixture,
      "execution-source-forced-depth",
    );
    const s1 = await stages.step01Forced(await stages.freshThread(), fixture);
    const s2 = await stages.step02(s1.result.nextThreadOutRef, fixture);
    const s3 = await stages.step03(s2.result.nextThreadOutRef, fixture);
    // The opening segments push containers; the closing segments consume
    // frame witnesses through the stack.
    const plan = buildNativeScriptDecodingScanPlan({
      itemBytes: fixture.scriptItem,
      direction: 1,
    });
    expect(plan.segments.some(({ frames }) => frames.length > 0)).toBe(true);
    const s4 = await stages.step04Loop(
      s3.result.nextThreadOutRef,
      fixture,
      (scan, index) => {
        if (index === 0)
          record("forced-depth-step04-scan-00", scan.measurement);
      },
    );
    expect(s4.scans).toBeGreaterThan(1);
    coverage.resumed();
    const s5 = await stages.step05(s4.threadOutRef, fixture);
    record("forced-depth-step05-mint", s5.measurement);
    coverage.reason(
      "ExecutionNativeScriptDepthLimit",
      "forced_rejection_wrong",
    );
    coverage.scenario("wrongful_forced_rejection_success");
  }, 900_000);

  it("contradicts a wrongful forced Malformed rejection of a decodable script and cancels from every physical step", async () => {
    const context = await makeExecutionSourceHarness();
    const fixture = await buildSubjectFixture({
      harness: context.harness,
      direction: "forced",
      item: { kind: "script", script: emptyAllScript() },
      reason: forcedReason("ExecutionNativeScriptMalformed"),
    });
    expect(fixture.canonicalVerdict).toBe("accepted");
    expect(fixture.evidence.resultClass).toBe(Classes.NoFault);
    const stages = await stand(
      context,
      fixture,
      "execution-source-forced-malformed",
    );
    const small = { maximumShape: "single-node native script, one-chunk item" };
    const chainTo = async (stepIndex: 0 | 1 | 2 | 3 | 4) => {
      const thread = await stages.freshThread();
      if (stepIndex === 0) return thread;
      const s1 = await stages.step01Forced(thread, fixture);
      if (stepIndex === 1) return s1.result.nextThreadOutRef;
      const s2 = await stages.step02(s1.result.nextThreadOutRef, fixture);
      if (stepIndex === 2) return s2.result.nextThreadOutRef;
      const s3 = await stages.step03(s2.result.nextThreadOutRef, fixture);
      if (stepIndex === 3) return s3.result.nextThreadOutRef;
      const s4 = await stages.step04Loop(s3.result.nextThreadOutRef, fixture);
      return s4.threadOutRef;
    };
    for (const stepIndex of [0, 2, 3, 4] as const) {
      const cancelled = await stages.cancel(
        await chainTo(stepIndex),
        stepIndex,
      );
      record(
        `cancel-step0${(stepIndex + 1).toString()}`,
        cancelled.measurement,
        small,
      );
      coverage.cancelled(`step-0${(stepIndex + 1).toString()}`);
    }
    const closed = await chainTo(4);
    const s5 = await stages.step05(closed, fixture);
    expect(s5.result.txHash).toMatch(/^[0-9a-f]{64}$/u);
    record("forced-sig-step05-mint", s5.measurement, small);
    coverage.reason("ExecutionNativeScriptMalformed", "forced_rejection_wrong");
    coverage.scenario("wrongful_forced_rejection_success");
  }, 900_000);

  it("refuses to convict an honest forced Malformed rejection of a malformed item", async () => {
    const context = await makeExecutionSourceHarness();
    const fixture = await buildSubjectFixture({
      harness: context.harness,
      direction: "forced",
      item: { kind: "raw", item: rawNativeItem(Buffer.from("820700", "hex")) },
      reason: forcedReason("ExecutionNativeScriptMalformed"),
    });
    expect(fixture.canonicalVerdict).toBe("rejected");
    expect(fixture.evidence.resultClass).toBe(Classes.Malformed);
    const stages = await stand(
      context,
      fixture,
      "execution-source-honest-forced",
    );
    const s1 = await stages.step01Forced(await stages.freshThread(), fixture);
    const s2 = await stages.step02(s1.result.nextThreadOutRef, fixture);
    const s3 = await stages.step03(s2.result.nextThreadOutRef, fixture);
    // The bound payload refuses at its first token: no wrongful-rejection
    // plan exists, so the canonical fold is driven under the
    // wrongful-acceptance polarity to exhibit the refusal on chain.
    await expect(
      stages.plan04(s3.result.nextThreadOutRef, fixture),
    ).rejects.toThrow(/no wrongful rejection/u);
    const s4 = await stages.step04(s3.result.nextThreadOutRef, fixture, 0);
    expect(s4.result.closed).toBe(true);
    expect(
      (await stages.scanState(s4.result.nextThreadOutRef, 4)).result_class,
    ).toBe(BigInt(Classes.Malformed));
    record("honest-forced-step04-close", s4.measurement, {
      maximumShape: "malformed payload item, one chunk",
    });
    // The operator's rejection is the engine's own verdict: step 05 refuses.
    await expectOnchainRefusal(() =>
      stages.step05Raw(s4.result.nextThreadOutRef),
    );
    coverage.scenario("honest_forced_rejection_refusal");
    await stages.cancel(s4.result.nextThreadOutRef, 4);
  }, 900_000);

  it("refuses to convict an honest accepted block whose source item decodes to the exact terminal", async () => {
    const context = await makeExecutionSourceHarness();
    const fixture = await buildSubjectFixture({
      harness: context.harness,
      direction: "accepted",
      item: { kind: "script", script: emptyAllScript() },
    });
    expect(fixture.canonicalVerdict).toBe("accepted");
    expect(fixture.evidence.resultClass).toBe(Classes.NoFault);
    const stages = await stand(
      context,
      fixture,
      "execution-source-honest-accepted",
    );
    const s1 = await stages.step01Accepted(await stages.freshThread(), fixture);
    const s2 = await stages.step02(s1.result.nextThreadOutRef, fixture);
    const s3 = await stages.step03(s2.result.nextThreadOutRef, fixture);
    // No wrongful-acceptance plan exists; the canonical fold is driven under
    // the wrongful-rejection polarity to reach the exact terminal on chain.
    await expect(
      stages.plan04(s3.result.nextThreadOutRef, fixture),
    ).rejects.toThrow(/no wrongful acceptance/u);
    const s4 = await stages.step04Loop(
      s3.result.nextThreadOutRef,
      fixture,
      undefined,
      1,
    );
    expect((await stages.scanState(s4.threadOutRef, 4)).result_class).toBe(
      BigInt(Classes.NoFault),
    );
    await expectOnchainRefusal(() => stages.step05Raw(s4.threadOutRef));
    coverage.scenario("honest_accepted_block_refusal");
    await stages.cancel(s4.threadOutRef, 4);
  }, 900_000);

  it("contradicts a wrongful acceptance of an empty native payload at the bind", async () => {
    const context = await makeExecutionSourceHarness();
    const fixture = await buildSubjectFixture({
      harness: context.harness,
      direction: "accepted",
      item: { kind: "raw", item: rawNativeItem(Buffer.alloc(0)) },
    });
    expect(fixture.scriptItem.toString("hex")).toBe("820040");
    expect(fixture.canonicalVerdict).toBe("rejected");
    expect(fixture.evidence.resultClass).toBe(Classes.Malformed);
    expect(fixture.evidence.initialControlCbor).toBe("");
    const stages = await stand(context, fixture, "execution-source-empty");
    const small = { maximumShape: "empty native payload, one chunk" };
    const s1 = await stages.step01Accepted(await stages.freshThread(), fixture);
    const s2 = await stages.step02(s1.result.nextThreadOutRef, fixture);
    const s3 = await stages.step03(s2.result.nextThreadOutRef, fixture);
    record("accepted-empty-step03-bind-close", s3.measurement, small);
    expect(
      (await stages.scanState(s3.result.nextThreadOutRef)).result_class,
    ).toBe(BigInt(Classes.Malformed));
    const s4 = await stages.step04(s3.result.nextThreadOutRef, fixture);
    expect(s4.result.closed).toBe(true);
    const s5 = await stages.step05(s4.result.nextThreadOutRef, fixture);
    record("accepted-empty-step05-mint", s5.measurement, small);
    coverage.reason("ExecutionNativeScriptMalformed", "accepted_invalid");
    coverage.scenario("wrongful_acceptance_success");
  }, 900_000);

  it("declares the lifecycle coverage it exercised and reproduces the Van Rossem fit ledger", async () => {
    // Recorded while the journeys above ran, never pre-filled. The declared
    // omissions are the wrongful-ACCEPTANCE directions of the NodeLimit and
    // DepthLimit arms and the adjacent-over-bound refusal: the frozen scan
    // bounds both frontiers at 16,384 and a node costs at least three bytes,
    // so no item inside the 32,768-byte field cap can reach either limit or
    // its adjacent edge. No accepted block can carry such a fault, and §5.3
    // forbids starting from a fabricated mid-thread datum; the exact and
    // adjacent node/depth edges are pinned at the rule level instead
    // (`exact_node_boundary_advances_and_adjacent_node_refuses`,
    // `exact_depth_boundary_advances_and_adjacent_depth_refuses`). The
    // byte cap on the field-6 preimage is not a bound of this family's chain:
    // no applied step carries a byte limit of its own, and the deterministic
    // machine still emits a native execution descriptor for a 32,769-byte
    // preimage (its `E_FIELD_PREIMAGE_SIZE` bound is not applied ahead of the
    // native-script phase), so there is no on-chain refusal to exhibit here.
    expect(() =>
      assertCompleteLifecycleCoverage({
        coverage: coverage.snapshot(),
        expectedReasonArms: [...EXECUTION_SOURCE_REASON_ARMS],
        authenticationSeams: [...AUTHENTICATION_SEAMS],
        cancellablePhysicalSteps: [...CANCELLABLE_STEPS],
        resumable: true,
        hasAdjacentConsensusBound: true,
      }),
    ).toThrow(
      "incomplete fault-proof lifecycle coverage: ExecutionNativeScriptNodeLimit success directions: accepted_invalid; ExecutionNativeScriptDepthLimit success directions: accepted_invalid; adjacent-over-bound refusal",
    );
    const blueprintBytes = readFileSync(realBlueprintPath);
    const preamble = JSON.parse(blueprintBytes.toString("utf8")) as {
      readonly preamble?: { readonly compiler?: { readonly version?: string } };
    };
    const ledger = buildVanRossemFitLedger({
      category: `executionSourceScriptDecoding:${EXECUTION_SOURCE_CATEGORY_ID}:testnet`,
      blueprintSha256: createHash("sha256")
        .update(blueprintBytes)
        .digest("hex"),
      compilerVersion: `aiken ${preamble.preamble?.compiler?.version ?? "unknown"}`,
      measurements: recorder.measurements,
    });
    expect(ledger.entries.length).toBeGreaterThan(25);
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
        `[execution-source-script-decoding-fit-ledger] wrote ${ledgerPath}`,
      );
    }
    console.info(
      `[execution-source-script-decoding-fit-ledger] ${JSON.stringify(ledger.entries)}`,
    );
  });
});
