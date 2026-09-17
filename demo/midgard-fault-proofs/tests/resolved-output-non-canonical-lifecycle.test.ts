/**
 * resolvedOutputNonCanonical (`00000026`) — complete Lucid lifecycle over the
 * five registered reference scripts.
 *
 * Every thread starts from the generic computation-thread `Init` and walks
 * step 01 (input coordinate, prior root and, for a forced leaf, the exact
 * typed reason) → step 02 (field 0/1 opening, exact out-ref) → step 03
 * (prior-ledger membership, descriptor and item commitment) → step 04 (the
 * resumable canonical reconstruction) → step 05 (terminal polarity, permanent
 * mint) → state-queue target-and-descendant removal. Evidence is built from
 * the retained prior ledger and the committed transaction, never from a
 * fabricated mid-thread datum. The suite covers every §5.3 item for both
 * source kinds and both directions: wrongful acceptance and wrongful forced
 * rejection at the maximum shape, honest refusals at the exact on-chain
 * predicate, reason/coordinate mutation, substitution at every seam, cancel
 * from every physical step, resume from a live checkpoint, mint plus removal.
 * Every positive transaction is measured against the Van Rossem envelope;
 * the final case pins the coverage gate and the fit ledger.
 */
import { createHash } from "node:crypto";
import { readFileSync } from "node:fs";
import { fileURLToPath } from "node:url";

import { Store, Trie } from "@aiken-lang/merkle-patricia-forestry";
import {
  buildMidgardBoundedItem,
  buildMidgardBoundedItemChunkProof,
} from "@al-ft/midgard-core";
import {
  ForcedInclusionTxV1Schema,
  OutputReference,
  Proof,
  ROOT_DOMAINS,
} from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  buildVanRossemFitLedger,
  type VanRossemFitMeasurement,
  writeVanRossemFitLedger,
} from "../src/proof-fit/van-rossem-fit-ledger.js";
import {
  classifyResolvedOutputNonCanonicalFinding,
  locateResolvedOutputScanControl,
  resolvedOutputChunkProofData,
  resolvedOutputScanControlData,
} from "../src/resolved-output-non-canonical/index.js";
import { assertCompleteLifecycleCoverage } from "../src/testing/complete-lifecycle.js";
import { buildCountedRoot } from "../src/transition-trace/phas.js";
import { expectOnchainRefusal } from "./support/emulator/expect-onchain-refusal.js";
import type { CompleteSignedTransactionMeasurement } from "./support/emulator/measurement.js";
import { createLifecycleCoverageRecorder } from "./support/lifecycle-coverage.js";
import {
  buildPriorLedger,
  buildSubjectTransaction,
  commitBlock,
  forcedSourceOf,
  makeResolvedOutputContext,
  makeResolvedOutputStages,
  MAXIMUM_INPUT_ITEM_COUNT,
  maximumCanonicalOutput,
  maximumMalformedOutput,
  RESOLVED_OUTPUT_MAXIMUM_BYTES,
  RESOLVED_OUTPUT_REASON_ARM,
  resolvedOutputEvidence,
  resolvedOutputReason,
  smallCanonicalOutput,
  smallMalformedOutput,
  subjectTransactionFor,
} from "./support/resolved-output-non-canonical-emulator.js";
import { realBlueprintPath } from "./support/submit-init-emulator-shared.js";

const AUTHENTICATION_SEAMS = [
  "tx_membership",
  "prior_root",
  "prior_output_descriptor",
  "prior_output_membership",
  "field_certificate",
  "forced_leaf_reason_coordinate",
  "forced_leaf_header",
  "forced_leaf_root",
  "forced_leaf_direction",
  "output_chunk",
  "scan_checkpoint",
  "wrong_successor",
  "finishable_advance",
  "premature_finalize",
] as const;
const CANCELLABLE_STEPS = [
  "step-01",
  "step-02",
  "step-03",
  "step-04",
  "step-05",
] as const;
const MAXIMUM_SHAPE = `${RESOLVED_OUTPUT_MAXIMUM_BYTES.toLocaleString("en-US")}-byte prior-ledger output at adversarial membership depth and a Certified ${MAXIMUM_INPUT_ITEM_COUNT.toString()}-item input field`;
const ledgerPath = fileURLToPath(
  new URL(
    "../../../docs/fault-proofs/size-plans/resolved-output-non-canonical-v1-fit-ledger.json",
    import.meta.url,
  ),
);

const coverage = createLifecycleCoverageRecorder();
const measurements: VanRossemFitMeasurement[] = [];
let publicationsRecorded = false;

const expectFit = (
  name: string,
  measurement: CompleteSignedTransactionMeasurement,
  runsScripts: boolean,
) => {
  expect(measurement.l1ByteMargin, name).toBeGreaterThan(0);
  if (!runsScripts) return;
  expect(measurement.executionMemory, name).toBeGreaterThan(0n);
  expect(measurement.executionSteps, name).toBeGreaterThan(0n);
};
const record = (
  name: string,
  measurement: CompleteSignedTransactionMeasurement,
  { runsScripts = true, maximumShape = MAXIMUM_SHAPE } = {},
) => {
  expectFit(name, measurement, runsScripts);
  measurements.push({
    name,
    kind: "lifecycle",
    maximumShape,
    signedBytes: measurement.completeSignedBytes,
    memoryUnits: measurement.executionMemory,
    cpuUnits: measurement.executionSteps,
  });
};
const recordPublication = (
  stepIndex: number,
  measurement: CompleteSignedTransactionMeasurement,
) => {
  expect(
    measurement.completeSignedBytes,
    `step ${(stepIndex + 1).toString()} publication`,
  ).toBeLessThanOrEqual(15_872);
  if (publicationsRecorded) return;
  measurements.push({
    name: `publish-step0${(stepIndex + 1).toString()}`,
    kind: "publication",
    maximumShape: "fully applied testnet validator",
    signedBytes: measurement.completeSignedBytes,
    memoryUnits: measurement.executionMemory,
    cpuUnits: measurement.executionSteps,
  });
  if (stepIndex === 4) publicationsRecorded = true;
};
/** Step 02 submits its carriage chunks and certificate before the step itself. */
const recordCarriage = (
  prefix: string,
  captured: {
    readonly measurements: readonly CompleteSignedTransactionMeasurement[];
  },
) => {
  const auxiliary = captured.measurements.slice(0, -1);
  expect(auxiliary.length).toBeGreaterThanOrEqual(2);
  auxiliary.forEach((measurement, index) => {
    const last = index === auxiliary.length - 1;
    // Chunk publications carry bytes only; the certificate runs its mint.
    record(
      last
        ? `${prefix}-carriage-certificate`
        : `${prefix}-carriage-chunk${(index + 1).toString().padStart(2, "0")}`,
      measurement,
      { runsScripts: last },
    );
  });
};
const progress = (message: string) =>
  console.info(`[resolved-output-non-canonical-progress] ${message}`);

describe("resolvedOutputNonCanonical registered-chain lifecycle", () => {
  it("contradicts a wrongful acceptance of a non-canonical spend input at the maximum shape: refuses the accepted seams, resumes the scan, mints and removes", async () => {
    const context = await makeResolvedOutputContext();
    const prior = await buildPriorLedger({
      output: maximumMalformedOutput(),
      adversarialDepth: true,
    });
    const nativeTx = subjectTransactionFor({
      sourceKind: 0,
      outRefBytes: prior.outRefBytes,
      count: MAXIMUM_INPUT_ITEM_COUNT,
    });
    const block = await commitBlock({
      context,
      nativeTx,
      priorRoot: prior.priorRoot,
    });
    const coordinate = { sourceKind: 0, inputIndex: 0 } as const;
    const evidence = resolvedOutputEvidence({ block, prior, coordinate });
    expect(evidence.outputIsNonCanonical).toBe(true);
    expect(evidence.carriage).toBe("Certified");
    expect(Buffer.from(evidence.resolved.outputCborHex, "hex")).toHaveLength(
      RESOLVED_OUTPUT_MAXIMUM_BYTES,
    );
    coverage.scenario("maximum_supported_evidence");
    const stages = await makeResolvedOutputStages(
      context,
      block,
      recordPublication,
    );

    progress("accepted cancel from step 01");
    const cancelled = await stages.init();
    record("accepted-init", cancelled.measurement);
    record(
      "accepted-cancel-step01",
      (await stages.cancel(stages.threadOf(cancelled), 0)).measurement,
    );
    coverage.cancelled("step-01");

    progress("accepted step 01");
    const initialized = await stages.init();
    // Transaction-membership seam: a root that carries the transaction but
    // which the challenged header never committed.
    const inclusion = block.accepted!.txInclusion;
    const foreign = await (async () => {
      const store = new Store(undefined);
      await store.ready();
      const trie = new Trie(store);
      const key = Buffer.from(inclusion.nativeTxId, "hex");
      const value = Buffer.from(inclusion.l2TransactionSourceCbor, "hex");
      await trie.insert(key, value);
      await trie.insert(Buffer.from("ee".repeat(32), "hex"), value);
      const proofCbor = (await trie.prove(key)).toCBOR().toString("hex");
      return {
        ...inclusion,
        transactionsPhasRoot: Buffer.from(trie.hash).toString("hex"),
        txMembershipProof: Data.from(proofCbor, Proof),
        txMembershipProofCbor: proofCbor,
      };
    })();
    await expectOnchainRefusal(() =>
      stages.step01Accepted(initialized, evidence, foreign),
    );
    coverage.seamMutated("tx_membership");
    const bound = await stages.step01Accepted(initialized, evidence);
    record("accepted-step01", bound.measurement);

    progress("accepted step 02");
    const opened = await stages.step02(bound.result.nextThreadOutRef, evidence);
    recordCarriage("accepted", opened);
    record("accepted-step02", opened.measurement);

    progress("accepted step 03 seams");
    // Prior-ledger seams: the sibling entry's descriptor under its own valid
    // proof (the bound out-ref key is not the proven key), and the bound
    // descriptor under the sibling's proof.
    await expectOnchainRefusal(() =>
      stages.step03(opened.result.nextThreadOutRef, {
        ...evidence,
        resolved: {
          ...evidence.resolved,
          descriptorCborHex: prior.siblingDescriptorCbor.toString("hex"),
          membershipProofCborHex: prior.siblingProofCborHex,
          membershipProof: Data.from(prior.siblingProofCborHex, Proof),
        },
      }),
    );
    coverage.seamMutated("prior_output_descriptor");
    await expectOnchainRefusal(() =>
      stages.step03(opened.result.nextThreadOutRef, {
        ...evidence,
        resolved: {
          ...evidence.resolved,
          membershipProofCborHex: prior.siblingProofCborHex,
          membershipProof: Data.from(prior.siblingProofCborHex, Proof),
        },
      }),
    );
    coverage.seamMutated("prior_output_membership");
    const staged = await stages.step03(
      opened.result.nextThreadOutRef,
      evidence,
    );
    record("accepted-step03-prior-membership", staged.measurement);

    progress("accepted step 04");
    // One transition, then every step-04 seam against the live checkpoint,
    // then resume from that checkpoint alone.
    const first = await stages.step04(staged.result.nextThreadOutRef, evidence);
    expect(first.result.terminal).toBe(false);
    record("accepted-step04-resume-00", first.measurement);
    const checkpoint = await stages.readReconstruction(
      first.result.nextThreadOutRef,
    );
    expect(checkpoint.control.cursor).toBeGreaterThan(0n);
    const honestIndex = locateResolvedOutputScanControl(
      evidence,
      checkpoint.control,
    );
    expect(honestIndex).toBeGreaterThan(0);
    const honest = evidence.scanControls[honestIndex]!;
    const next = evidence.scanControls[honestIndex + 1]!;
    const item = buildMidgardBoundedItem({
      fieldIndex: 2,
      itemIndex: 0,
      bytes: Buffer.from(evidence.resolved.outputCborHex, "hex"),
    });
    const chunkIndex = Math.floor(honest.cursor / 4_095);
    const advance = (proofItem = item) => ({
      Advance: {
        chunk_proof: resolvedOutputChunkProofData(
          buildMidgardBoundedItemChunkProof(proofItem, chunkIndex),
        ),
        next_chunk_proof: resolvedOutputChunkProofData(
          buildMidgardBoundedItemChunkProof(proofItem, chunkIndex + 1),
        ),
      },
    });
    const reconstruction = (control: typeof next) => ({
      descriptor_cbor: evidence.resolved.descriptorCborHex,
      control: resolvedOutputScanControlData(control),
    });
    // A chunk proof for another item (one byte flipped) with a valid frontier.
    const substituted = Buffer.from(evidence.resolved.outputCborHex, "hex");
    substituted[honest.cursor] = substituted[honest.cursor]! ^ 0xff;
    await expectOnchainRefusal(() =>
      stages.step04Raw({
        threadOutRef: first.result.nextThreadOutRef,
        evidence,
        action: advance(
          buildMidgardBoundedItem({
            fieldIndex: 2,
            itemIndex: 0,
            bytes: substituted,
          }),
        ),
        nextData: reconstruction(next),
        nextStepIndex: 3,
      }),
    );
    coverage.seamMutated("output_chunk");
    // A successor checkpoint the engine never produced.
    await expectOnchainRefusal(() =>
      stages.step04Raw({
        threadOutRef: first.result.nextThreadOutRef,
        evidence,
        action: advance(),
        nextData: reconstruction({ ...next, cursor: next.cursor + 1 }),
        nextStepIndex: 3,
      }),
    );
    coverage.seamMutated("scan_checkpoint");
    // The right transition sent to the terminal script.
    await expectOnchainRefusal(() =>
      stages.step04Raw({
        threadOutRef: first.result.nextThreadOutRef,
        evidence,
        action: advance(),
        nextData: { output_is_non_canonical: true },
        nextStepIndex: 4,
      }),
    );
    coverage.seamMutated("wrong_successor");
    // Closing as canonical before the engine's closing edge admits it.
    await expectOnchainRefusal(() =>
      stages.step04Raw({
        threadOutRef: first.result.nextThreadOutRef,
        evidence,
        action: "FinalizeCanonical",
        nextData: { output_is_non_canonical: false },
        nextStepIndex: 4,
      }),
    );
    coverage.seamMutated("premature_finalize");
    const walked = await stages.reconstruct(
      first.result.nextThreadOutRef,
      evidence,
      (captured, index) =>
        record(
          captured.result.terminal
            ? "accepted-step04-terminal"
            : `accepted-step04-resume-${(index + 1).toString().padStart(2, "0")}`,
          captured.measurement,
        ),
    );
    expect(walked.transitions).toBeGreaterThan(1);
    expect(walked.final.action).toBe("advance");
    coverage.resumed();

    progress("accepted step 05 and removal");
    const minted = await stages.step05(walked.threadOutRef, evidence);
    expect(minted.result.fraudProofUnit).toBeTruthy();
    record("accepted-step05-proof-mint", minted.measurement);
    coverage.reason(RESOLVED_OUTPUT_REASON_ARM, "accepted_invalid");
    coverage.scenario("wrongful_acceptance_success");
    record(
      "accepted-remove-fraudulent-block",
      (await stages.remove()).measurement,
    );
    coverage.scenario("permanent_proof_token_and_descendant_removal");
  }, 1_800_000);

  it("contradicts a wrongful forced rejection of a canonical reference input at the maximum shape: refuses every forced-door and certificate seam, closes at the exact end, mints and removes", async () => {
    const context = await makeResolvedOutputContext();
    const prior = await buildPriorLedger({
      output: maximumCanonicalOutput(),
      adversarialDepth: true,
    });
    const items = Array.from(
      { length: MAXIMUM_INPUT_ITEM_COUNT },
      () => prior.outRefBytes,
    );
    // Both input fields carry the maximum item count so a genuine field-0
    // certificate exists to substitute into the field-1 opening.
    const nativeTx = buildSubjectTransaction({
      spendInputCbors: items,
      referenceInputCbors: items,
    });
    const coordinate = { sourceKind: 1, inputIndex: 0 } as const;
    const block = await commitBlock({
      context,
      nativeTx,
      priorRoot: prior.priorRoot,
      reason: resolvedOutputReason(coordinate),
    });
    const evidence = resolvedOutputEvidence({ block, prior, coordinate });
    expect(evidence.outputIsNonCanonical).toBe(false);
    expect(evidence.carriage).toBe("Certified");
    expect(evidence.canonicalTrace).not.toBeNull();
    const stages = await makeResolvedOutputStages(
      context,
      block,
      recordPublication,
    );
    progress("forced init");
    const initialized = await stages.init();
    record("forced-init", initialized.measurement);
    const thread = stages.threadOf(initialized);
    const membership = forcedSourceOf(block).membership;

    const door = async (
      seam: (typeof AUTHENTICATION_SEAMS)[number],
      patch: Partial<Parameters<typeof stages.step01ForcedRaw>[0]>,
    ) => {
      progress(`forced-door refusal: ${seam}`);
      await expectOnchainRefusal(() =>
        stages.step01ForcedRaw({
          threadOutRef: thread,
          coordinate,
          priorRoot: prior.priorRoot,
          ...patch,
        }),
      );
      coverage.seamMutated(seam);
    };
    // The leaf commits (1, 0); a claim of (1, 1) or (0, 0) is refused by the
    // exact reason binding, off chain and on chain.
    expect(() =>
      classifyResolvedOutputNonCanonicalFinding({
        subject: evidence.subject,
        coordinate: { sourceKind: 1, inputIndex: 1 },
      }),
    ).toThrow(/coordinate was substituted/u);
    await door("forced_leaf_reason_coordinate", {
      coordinate: { sourceKind: 1, inputIndex: 1 },
    });
    await door("forced_leaf_reason_coordinate", {
      coordinate: { sourceKind: 0, inputIndex: 0 },
    });
    coverage.scenario("reason_or_subject_coordinate_mutation");
    await door("prior_root", { priorRoot: "ee".repeat(32) });
    await door("forced_leaf_header", {
      header: { ...block.header, validationTracesRoot: "ff".repeat(32) },
    });
    await door("forced_leaf_direction", { direction: 0n });
    // A root that does carry the leaf, but which the header never committed.
    const foreign = await (async () => {
      const key = Buffer.from(Data.to(membership.key, OutputReference), "hex");
      const value = Buffer.from(
        Data.to(membership.value as never, ForcedInclusionTxV1Schema as never),
        "hex",
      );
      const decoy = Buffer.from(
        Data.to(
          { transactionId: "ab".repeat(32), outputIndex: 7n },
          OutputReference,
        ),
        "hex",
      );
      const root = await buildCountedRoot(ROOT_DOMAINS.forcedTransactionsV1, [
        { key, value },
        { key: decoy, value },
      ]);
      const store = new Store(undefined);
      await store.ready();
      const trie = new Trie(store);
      await trie.insert(key, value);
      await trie.insert(decoy, value);
      return {
        ...membership,
        root: root.root,
        phas_root: root.phasRoot,
        count: root.count,
        proof: Data.from(
          (await trie.prove(key)).toCBOR().toString("hex"),
          Proof,
        ),
      };
    })();
    await door("forced_leaf_root", { membership: foreign });

    progress("forced step 01");
    const bound = await stages.step01Forced(thread, evidence);
    record("forced-step01", bound.measurement);

    progress("forced step 02 certificate seam");
    const opened = await stages.step02(bound.result.nextThreadOutRef, evidence);
    recordCarriage("forced", opened);
    record("forced-step02", opened.measurement);
    // A genuine certificate for field 0 of the same transaction, named by a
    // second thread's field-1 opening as its certificate: the door refuses the
    // wrong field. That thread then cancels from step 02.
    const otherCertificate = await stages.certifyField(0);
    const second = await stages.step01Forced(
      stages.threadOf(await stages.init()),
      evidence,
    );
    await expectOnchainRefusal(() =>
      stages.step02Raw({
        threadOutRef: second.result.nextThreadOutRef,
        evidence,
        otherCertificate,
      }),
    );
    coverage.seamMutated("field_certificate");
    record(
      "forced-cancel-step02",
      (await stages.cancel(second.result.nextThreadOutRef, 1)).measurement,
    );
    coverage.cancelled("step-02");

    progress("forced step 03");
    const staged = await stages.step03(
      opened.result.nextThreadOutRef,
      evidence,
    );
    record("forced-step03-prior-membership", staged.measurement);

    progress("forced step 04");
    const walked = await stages.reconstruct(
      staged.result.nextThreadOutRef,
      evidence,
      (captured, index) =>
        record(
          captured.result.terminal
            ? "forced-step04-finalize-canonical"
            : `forced-step04-resume-${index.toString().padStart(2, "0")}`,
          captured.measurement,
        ),
    );
    expect(walked.transitions).toBeGreaterThan(1);
    expect(walked.final.action).toBe("finalize");
    coverage.resumed();

    progress("forced step 05 and removal");
    const minted = await stages.step05(walked.threadOutRef, evidence);
    expect(minted.result.fraudProofUnit).toBeTruthy();
    record("forced-step05-proof-mint", minted.measurement);
    coverage.reason(RESOLVED_OUTPUT_REASON_ARM, "forced_rejection_wrong");
    coverage.scenario("wrongful_forced_rejection_success");
    record(
      "forced-remove-fraudulent-block",
      (await stages.remove()).measurement,
    );
  }, 1_800_000);

  it("refuses to convict an honest accepted block: the finishable control cannot be advanced and a canonical verdict cannot mint under an accepted subject", async () => {
    const context = await makeResolvedOutputContext();
    const prior = await buildPriorLedger({
      output: smallCanonicalOutput(),
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
    expect(() => resolvedOutputEvidence({ block, prior, coordinate })).toThrow(
      /agrees with the operator verdict/u,
    );
    const lying = resolvedOutputEvidence({
      block,
      prior,
      coordinate,
      claim: "lying",
    });
    expect(lying.outputIsNonCanonical).toBe(false);
    const stages = await makeResolvedOutputStages(context, block);
    const initialized = await stages.init();
    const bound = await stages.step01Accepted(initialized, lying);
    const opened = await stages.step02(bound.result.nextThreadOutRef, lying);
    const staged = await stages.step03(opened.result.nextThreadOutRef, lying);
    // Walk to the finishable control by hand: every honest transition but the
    // closing one.
    let current = staged.result.nextThreadOutRef;
    for (;;) {
      const checkpoint = await stages.readReconstruction(current);
      const index = locateResolvedOutputScanControl(lying, checkpoint.control);
      expect(index).toBeGreaterThanOrEqual(0);
      if (index === lying.scanControls.length - 2) break;
      current = (await stages.step04(current, lying)).result.nextThreadOutRef;
    }
    const finishable = lying.scanControls[lying.scanControls.length - 2]!;
    expect(BigInt(finishable.cursor)).toBe(
      BigInt(Buffer.from(lying.resolved.outputCborHex, "hex").length),
    );
    // The decisive predicate: the exhausted window is not a structural fault.
    const item = buildMidgardBoundedItem({
      fieldIndex: 2,
      itemIndex: 0,
      bytes: Buffer.from(lying.resolved.outputCborHex, "hex"),
    });
    await expectOnchainRefusal(() =>
      stages.step04Raw({
        threadOutRef: current,
        evidence: lying,
        action: {
          Advance: {
            chunk_proof: resolvedOutputChunkProofData(
              buildMidgardBoundedItemChunkProof(item, 0),
            ),
            next_chunk_proof: null,
          },
        },
        nextData: { output_is_non_canonical: true },
        nextStepIndex: 4,
      }),
    );
    coverage.seamMutated("finishable_advance");
    // The only admissible closure is canonical, which the terminal step
    // refuses under an accepted subject, before signing and on chain.
    const closed = await stages.step04(current, lying);
    expect(closed.result).toEqual(
      expect.objectContaining({ terminal: true, action: "finalize" }),
    );
    await expect(
      stages.step05(closed.result.nextThreadOutRef, lying),
    ).rejects.toThrow(/does not contradict verdict/u);
    await expectOnchainRefusal(() =>
      stages.step05Raw(closed.result.nextThreadOutRef),
    );
    await stages.cancel(closed.result.nextThreadOutRef, 4);
    coverage.cancelled("step-05");
    coverage.scenario("honest_accepted_block_refusal");
  }, 900_000);

  it("refuses to contradict an honest forced rejection: the structural fault closes non-canonical and cannot mint under a rejection subject", async () => {
    const context = await makeResolvedOutputContext();
    const prior = await buildPriorLedger({
      output: smallMalformedOutput(),
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
      reason: resolvedOutputReason(coordinate),
    });
    expect(() => resolvedOutputEvidence({ block, prior, coordinate })).toThrow(
      /agrees with the operator verdict/u,
    );
    const lying = resolvedOutputEvidence({
      block,
      prior,
      coordinate,
      claim: "lying",
    });
    expect(lying.outputIsNonCanonical).toBe(true);
    const stages = await makeResolvedOutputStages(context, block);
    const initialized = await stages.init();
    const thread = stages.threadOf(initialized);
    const bound = await stages.step01Forced(thread, lying);
    const opened = await stages.step02(bound.result.nextThreadOutRef, lying);
    const staged = await stages.step03(opened.result.nextThreadOutRef, lying);
    const walked = await stages.reconstruct(
      staged.result.nextThreadOutRef,
      lying,
    );
    expect(walked.final.action).toBe("advance");
    await expect(stages.step05(walked.threadOutRef, lying)).rejects.toThrow(
      /does not contradict verdict/u,
    );
    await expectOnchainRefusal(() => stages.step05Raw(walked.threadOutRef));
    await stages.cancel(walked.threadOutRef, 4);
    coverage.cancelled("step-05");
    coverage.scenario("honest_forced_rejection_refusal");
  }, 900_000);

  it("refuses a forced leaf committed under another typed reason", async () => {
    const context = await makeResolvedOutputContext();
    const prior = await buildPriorLedger({
      output: smallCanonicalOutput(),
      adversarialDepth: false,
    });
    const block = await commitBlock({
      context,
      nativeTx: subjectTransactionFor({
        sourceKind: 0,
        outRefBytes: prior.outRefBytes,
        count: 1,
      }),
      priorRoot: prior.priorRoot,
      reason: { InputNotFound: { source_kind: 0n, input_index: 0n } },
    });
    // Off chain the family never admits the adjacent reason; on chain the
    // exact reason binding refuses the leaf.
    expect(() =>
      resolvedOutputEvidence({
        block,
        prior,
        coordinate: { sourceKind: 0, inputIndex: 0 },
      }),
    ).toThrow(/wrong typed rejection reason/u);
    const stages = await makeResolvedOutputStages(context, block);
    const initialized = await stages.init();
    await expectOnchainRefusal(() =>
      stages.step01ForcedRaw({
        threadOutRef: stages.threadOf(initialized),
        coordinate: { sourceKind: 0, inputIndex: 0 },
        priorRoot: prior.priorRoot,
      }),
    );
    coverage.seamMutated("forced_leaf_reason_coordinate");
    coverage.scenario("reason_or_subject_coordinate_mutation");
  }, 600_000);

  it("corrects both remaining source-kind/direction pairs at a small shape and cancels from steps 02, 03 and 04", async () => {
    // Accepted, reference input (field 1), non-canonical.
    {
      const context = await makeResolvedOutputContext();
      const prior = await buildPriorLedger({
        output: smallMalformedOutput(),
        adversarialDepth: false,
      });
      const coordinate = { sourceKind: 1, inputIndex: 2 } as const;
      const block = await commitBlock({
        context,
        nativeTx: subjectTransactionFor({
          sourceKind: 1,
          outRefBytes: prior.outRefBytes,
          count: 3,
        }),
        priorRoot: prior.priorRoot,
      });
      const evidence = resolvedOutputEvidence({ block, prior, coordinate });
      expect(evidence.outputIsNonCanonical).toBe(true);
      const stages = await makeResolvedOutputStages(context, block);
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
      const minted = await stages.step05(walked.threadOutRef, evidence);
      expect(minted.result.fraudProofUnit).toBeTruthy();
      coverage.reason(RESOLVED_OUTPUT_REASON_ARM, "accepted_invalid");
      // Cancel from step 02 on a second thread of the same block.
      const second = await stages.step01Accepted(await stages.init(), evidence);
      await stages.cancel(second.result.nextThreadOutRef, 1);
      coverage.cancelled("step-02");
    }
    // Forced, spend input (field 0), canonical.
    {
      const context = await makeResolvedOutputContext();
      const prior = await buildPriorLedger({
        output: smallCanonicalOutput(),
        adversarialDepth: false,
      });
      const coordinate = { sourceKind: 0, inputIndex: 1 } as const;
      const block = await commitBlock({
        context,
        nativeTx: subjectTransactionFor({
          sourceKind: 0,
          outRefBytes: prior.outRefBytes,
          count: 2,
        }),
        priorRoot: prior.priorRoot,
        reason: resolvedOutputReason(coordinate),
      });
      const evidence = resolvedOutputEvidence({ block, prior, coordinate });
      expect(evidence.outputIsNonCanonical).toBe(false);
      const stages = await makeResolvedOutputStages(context, block);
      const through03 = async () => {
        const bound = await stages.step01Forced(
          stages.threadOf(await stages.init()),
          evidence,
        );
        const opened = await stages.step02(
          bound.result.nextThreadOutRef,
          evidence,
        );
        return (await stages.step03(opened.result.nextThreadOutRef, evidence))
          .result.nextThreadOutRef;
      };
      const walked = await stages.reconstruct(await through03(), evidence);
      expect(walked.final.action).toBe("finalize");
      const minted = await stages.step05(walked.threadOutRef, evidence);
      expect(minted.result.fraudProofUnit).toBeTruthy();
      coverage.reason(RESOLVED_OUTPUT_REASON_ARM, "forced_rejection_wrong");
      // Cancel from step 03 (a thread that has opened its field), from step
      // 04 at the initial control, and from a mid-walk step-04 checkpoint.
      const atStep03 = await stages.step02(
        (
          await stages.step01Forced(
            stages.threadOf(await stages.init()),
            evidence,
          )
        ).result.nextThreadOutRef,
        evidence,
      );
      await stages.cancel(atStep03.result.nextThreadOutRef, 2);
      coverage.cancelled("step-03");
      await stages.cancel(await through03(), 3);
      const midWalk = await stages.step04(await through03(), evidence);
      expect(midWalk.result.terminal).toBe(false);
      await stages.cancel(midWalk.result.nextThreadOutRef, 3);
      coverage.cancelled("step-04");
    }
  }, 1_200_000);

  it("closes the coverage gate and the Van Rossem fit ledger", async () => {
    assertCompleteLifecycleCoverage({
      coverage: coverage.snapshot(),
      expectedReasonArms: [RESOLVED_OUTPUT_REASON_ARM],
      authenticationSeams: [...AUTHENTICATION_SEAMS],
      cancellablePhysicalSteps: [...CANCELLABLE_STEPS],
      resumable: true,
      hasAdjacentConsensusBound: false,
    });
    const blueprintBytes = readFileSync(realBlueprintPath);
    const preamble = JSON.parse(blueprintBytes.toString("utf8")) as {
      readonly preamble?: { readonly compiler?: { readonly version?: string } };
    };
    const ledger = buildVanRossemFitLedger({
      category: "resolvedOutputNonCanonical:00000026:testnet",
      blueprintSha256: createHash("sha256")
        .update(blueprintBytes)
        .digest("hex"),
      compilerVersion: `aiken ${preamble.preamble?.compiler?.version ?? "unknown"}`,
      measurements,
    });
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
    if (process.env.MIDGARD_WRITE_FIT_LEDGER === "1") {
      await writeVanRossemFitLedger(ledgerPath, ledger);
      console.info(
        `[resolved-output-non-canonical-fit-ledger] wrote ${ledgerPath}`,
      );
    }
    console.info(
      `[resolved-output-non-canonical-fit-ledger] ${JSON.stringify(ledger.entries)}`,
    );
  });
});
