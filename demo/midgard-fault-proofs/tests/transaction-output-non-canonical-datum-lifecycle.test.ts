import { writeFile } from "node:fs/promises";

import {
  computeMidgardNativeTxId,
  isCanonicalMidgardDataStream,
} from "@al-ft/midgard-core";
import {
  encodeMidgardForcedTxCanonical,
  materializeMidgardForcedTxFromCanonical,
} from "@al-ft/midgard-core/codec/forced";
import { acceptedVerdictSubject } from "@al-ft/midgard-sdk";
import { describe, expect, it } from "vitest";

import {
  submitTransactionOutputNonCanonicalStep01Accepted,
  submitTransactionOutputNonCanonicalStep02,
  submitTransactionOutputNonCanonicalStep03Datum,
  submitTransactionOutputNonCanonicalStep04,
  transactionOutputDatumScanState,
  transactionOutputDatumStepArgs,
} from "../src/transaction-output-non-canonical/index.js";
import { expectOnchainRefusal } from "./support/emulator/expect-onchain-refusal.js";
import {
  captureEmulatorSubmission,
  type CompleteSignedTransactionMeasurement,
} from "./support/emulator/measurement.js";
import { makeNativeTx } from "./support/emulator/native-tx.js";
import { densePlutusData } from "./support/dense-plutus-data.js";
import { nodeForcedVerdict } from "./support/emulator/node-forced-verdict.js";
import { setupFraudulentBlock } from "./support/submit-init-emulator-fixtures.js";
import {
  outputWithInlineDatum,
  readOutputScanState,
  submitOutputStep03DatumRaw,
  submitOutputStep04Raw,
  TRANSACTION_OUTPUT_SCAN_CHUNK_BYTES,
} from "./support/transaction-output-non-canonical-emulator.js";
import {
  DATUM_OFFSET,
  EXACT_TERMINAL_DATUM,
  EXACT_TERMINAL_OUTPUT_BYTES,
  forcedDatumScenario,
  hex,
  makeHarness,
  nodeTransaction,
  NON_CANONICAL_DATUM,
  scanToDatum,
  stepCommon,
} from "./transaction-output-non-canonical-datum-lifecycle.scenario.js";
import {
  committedInclusions,
  coverage,
  evidenceOf,
  fit,
  MAXIMUM_OUTPUT_BYTES,
  network,
  record,
  registeredContracts,
  witnessSetCborOf,
} from "./transaction-output-non-canonical-lifecycle.registered-contracts.js";
import { datumToVerdict } from "./transaction-output-non-canonical-lifecycle.scan-to-terminal.js";

/**
 * The datum step of transactionOutputNonCanonical: an output whose envelope
 * is canonical is judged by its inline datum, read as canonical Data a
 * bounded number of nodes per transaction over the scan's authenticated
 * chunks, with the open containers carried as a hash chain.
 */

describe("transactionOutputNonCanonical inline-datum verdict", () => {
  it("upholds an honest forced rejection of a non-canonical inline datum read across chunk boundaries and transactions, refusing every forged datum checkpoint, the early verdicts and the proof mint", async () => {
    const output = outputWithInlineDatum(NON_CANONICAL_DATUM);
    expect(isCanonicalMidgardDataStream(NON_CANONICAL_DATUM)).toBe(false);
    const transaction = nodeTransaction(output);
    // The node writes this rejection for the forced order.
    const forcedTx = materializeMidgardForcedTxFromCanonical(transaction);
    expect(
      await nodeForcedVerdict({
        transactionId: computeMidgardNativeTxId(forcedTx),
        forcedCanonicalCbor: encodeMidgardForcedTxCanonical(forcedTx),
      }),
    ).toStrictEqual({
      ForcedTxInvalid: { reason: { OutputNonCanonical: { output_index: 0n } } },
    });
    const s = await forcedDatumScenario(transaction);
    const { evidence, registered } = s;
    expect(evidence.envelopeCanonical).toBe(true);
    expect(evidence.canonical).toBe(false);
    expect(evidence.decisiveFaultHolds).toBe(true);
    const plan = evidence.datumPlan!;
    expect(plan.canonical).toBe(false);
    // The read spans transactions, windows of two chunks, and a stack whose
    // deeper frames stay hidden behind the tail root.
    expect(plan.steps.length).toBeGreaterThan(1);
    expect(
      plan.steps.some(
        (step) => step.windowLength > TRANSACTION_OUTPUT_SCAN_CHUNK_BYTES,
      ),
    ).toBe(true);
    expect(
      plan.steps.some((step) => step.revealedCount < step.stack.length),
    ).toBe(true);

    const thread = await s.bindToDatum();
    const scan = transactionOutputDatumScanState(evidence, 1n);
    const first = plan.steps[0]!;
    const firstArgs = transactionOutputDatumStepArgs(evidence, first);
    if (first.next.kind !== "continue") throw new Error("one-step plan");
    const firstNext = {
      cursor: BigInt(first.next.cursor),
      stack_root: first.next.stackRoot,
    };
    const refuseDatumStep = async (
      seam: string,
      input: Partial<Parameters<typeof submitOutputStep03DatumRaw>[0]>,
      threadOutRef = thread,
    ) => {
      await expectOnchainRefusal(
        async () =>
          await submitOutputStep03DatumRaw({
            ...registered.common(threadOutRef, 3),
            args: firstArgs,
            scan,
            next: firstNext,
            nextStepIndex: 3,
            ...input,
          }),
      );
      coverage.seamMutated(seam);
    };
    // Early verdicts: neither outcome may be claimed before the datum ends.
    await refuseDatumStep("datum_verdict", {
      next: { outcome: 1n },
      nextStepIndex: 4,
    });
    await refuseDatumStep("datum_verdict", {
      next: { outcome: 2n },
      nextStepIndex: 4,
    });
    // The honest checkpoint handed to the terminal step's address.
    await refuseDatumStep("datum_successor", { nextStepIndex: 4 });
    // A checkpoint that skips a byte, or carries another stack.
    await refuseDatumStep("datum_checkpoint", {
      next: { ...firstNext, cursor: firstNext.cursor + 1n },
    });
    await refuseDatumStep("datum_checkpoint", {
      next: { ...firstNext, stack_root: "00".repeat(32) },
    });
    // A window whose datum bytes differ from the authenticated chunk.
    const window = Buffer.from(firstArgs.window, "hex");
    window[DATUM_OFFSET + 50 - first.windowStart] ^= 0x01;
    await refuseDatumStep("datum_window", {
      args: { ...firstArgs, window: window.toString("hex") },
    });

    // Honest first read, then forged reveals against the stored stack root.
    const firstStep = await submitTransactionOutputNonCanonicalStep03Datum({
      ...stepCommon(registered),
      threadOutRef: thread,
      evidence,
      referenceScriptUtxo: registered.references[3]!,
    });
    expect(firstStep.finished).toBe(false);
    const second = plan.steps[1]!;
    expect(second.stack.length).toBeGreaterThan(0);
    const secondArgs = transactionOutputDatumStepArgs(evidence, second);
    const secondNext =
      second.next.kind === "continue"
        ? {
            cursor: BigInt(second.next.cursor),
            stack_root: second.next.stackRoot,
          }
        : { outcome: second.next.canonical ? 1n : 2n };
    const secondInput = {
      args: secondArgs,
      next: secondNext,
      nextStepIndex: second.next.kind === "continue" ? 3 : 4,
    } as const;
    await refuseDatumStep(
      "datum_stack",
      {
        ...secondInput,
        args: { ...secondArgs, tail_root: "00".repeat(32) },
      },
      firstStep.nextThreadOutRef,
    );
    await refuseDatumStep(
      "datum_stack",
      {
        ...secondInput,
        args: {
          ...secondArgs,
          revealed: [{ ListFrame: { started: false } }, ...secondArgs.revealed],
        },
      },
      firstStep.nextThreadOutRef,
    );

    // The rest of the honest read reaches the non-canonical verdict.
    const verdict = await datumToVerdict({
      registered,
      threadOutRef: firstStep.nextThreadOutRef,
      evidence,
      label: "forced-honest-datum",
    });
    expect(verdict.datumSteps).toBe(plan.steps.length - 1);
    const terminal = await readOutputScanState(
      registered.common(verdict.threadOutRef, 4),
      4,
    );
    expect(terminal.outcome).toBe(2n);
    await expect(
      submitTransactionOutputNonCanonicalStep04({
        ...stepCommon(registered),
        threadOutRef: verdict.threadOutRef,
        evidence,
        referenceScriptUtxo: registered.references[4]!,
        witnessReferenceScripts: s.harness.witnessReferenceScripts,
      }),
    ).rejects.toThrow(/does not contradict/u);
    await expectOnchainRefusal(
      async () =>
        await submitOutputStep04Raw({
          ...registered.common(verdict.threadOutRef, 4),
          witnessReferenceScripts: s.harness.witnessReferenceScripts,
        }),
      {
        refusedBy: "fraud_proofs/transaction_output_non_canonical/step_04",
        check: /^Validator returned false$/u,
      },
    );
    coverage.scenario("honest_forced_rejection_refusal");

    // A thread parked on the datum step cancels.
    const parked = await s.bindToDatum();
    const cancelled = await registered.cancel(parked, 3);
    record("forced-cancel-step03-datum", cancelled.measurement);
    coverage.cancelled("step-03-datum");
  }, 900_000);

  it("convicts a forced rejection of a canonical inline datum that ends at the output's last byte on a chunk boundary, then mints and removes", async () => {
    const output = outputWithInlineDatum(EXACT_TERMINAL_DATUM);
    expect(output).toHaveLength(EXACT_TERMINAL_OUTPUT_BYTES);
    expect(isCanonicalMidgardDataStream(EXACT_TERMINAL_DATUM)).toBe(true);
    const s = await forcedDatumScenario(nodeTransaction(output));
    const { evidence, registered } = s;
    expect(evidence.canonical).toBe(true);
    expect(evidence.decisiveFaultHolds).toBe(false);
    const plan = evidence.datumPlan!;
    expect(plan.steps.length).toBeGreaterThan(2);
    const last = plan.steps.at(-1)!;
    expect(last.windowStart + last.windowLength).toBe(
      EXACT_TERMINAL_OUTPUT_BYTES,
    );
    const thread = await s.bindToDatum("forced-false-rejection");
    const verdict = await datumToVerdict({
      registered,
      threadOutRef: thread,
      evidence,
      label: "forced-false-rejection",
    });
    expect(verdict.datumSteps).toBe(plan.steps.length);
    const minted = await captureEmulatorSubmission(s.harness.emulator, () =>
      submitTransactionOutputNonCanonicalStep04({
        ...stepCommon(registered),
        threadOutRef: verdict.threadOutRef,
        evidence,
        referenceScriptUtxo: registered.references[4]!,
        witnessReferenceScripts: s.harness.witnessReferenceScripts,
      }),
    );
    expect(minted.result.fraudProofUnit).toBeTruthy();
    record("forced-false-rejection-proof-mint", minted.measurement);
    coverage.reason("OutputNonCanonical", "forced_rejection_wrong");
    const removed = await registered.removal(s.setup.headerHash);
    record("forced-false-rejection-remove", removed.measurement);
  }, 900_000);

  it("convicts an accepted output whose inline datum is an unchunked 100-byte string", async () => {
    // A byte string above 64 bytes must be chunked; this one is not.
    const datum = Buffer.concat([hex("5864"), Buffer.alloc(100, 0x11)]);
    expect(isCanonicalMidgardDataStream(datum)).toBe(false);
    const transaction = makeNativeTx({
      spendInputCbors: [],
      fee: 7n,
      outputCbors: [outputWithInlineDatum(datum)],
    });
    const harness = await makeHarness();
    const registered = await registeredContracts(harness);
    const committed = await committedInclusions([transaction]);
    const inclusion = committed.inclusions[0]!;
    const setup = await setupFraudulentBlock({
      funderLucid: harness.funderLucid,
      emulator: harness.emulator,
      contracts: harness.contracts,
      catalogue: registered.catalogue,
      fixture: {
        transactionsRoot: committed.transactionsRoot,
        l2TransactionCount: 1n,
      },
    });
    const certificateReference = await registered.publishReferences();
    const evidence = evidenceOf(
      acceptedVerdictSubject(inclusion.nativeTxId),
      transaction,
      0,
    );
    expect(evidence.envelopeCanonical).toBe(true);
    expect(evidence.canonical).toBe(false);
    const init = await registered.init(setup.fraudulentBlockOutRef);
    const bound = await submitTransactionOutputNonCanonicalStep01Accepted({
      lucid: harness.proverLucid,
      blueprint: harness.realBlueprint,
      network,
      contracts: registered.contracts,
      signer: harness.proverSigner,
      finding: evidence,
      threadUtxo: await registered.threadUtxoOf(
        init.result.txHash,
        init.result.firstStepOutputIndex,
      ),
      threadToken: {
        unit: init.result.computationThreadUnit,
        fraudulentHeaderHash: init.result.fraudulentHeaderHash,
      },
      stateQueueBlockOutRef: setup.fraudulentBlockOutRef,
      txInclusion: inclusion,
      referenceScriptUtxo: registered.references[0]!,
      witnessReferenceScripts: harness.witnessReferenceScripts,
    });
    const authenticated = await submitTransactionOutputNonCanonicalStep02({
      ...stepCommon(registered),
      threadOutRef: bound.nextThreadOutRef,
      evidence,
      nativeTxCompactCbor: inclusion.nativeTxCompactCbor,
      witnessSetCompactCbor: witnessSetCborOf(transaction),
      referenceScriptUtxo: registered.references[1]!,
      certificateReferenceScriptUtxo: certificateReference,
    });
    const thread = await scanToDatum(
      registered,
      authenticated.nextThreadOutRef,
      evidence,
    );
    const verdict = await datumToVerdict({
      registered,
      threadOutRef: thread,
      evidence,
      label: "accepted-datum",
    });
    expect(verdict.datumSteps).toBe(1);
    const minted = await submitTransactionOutputNonCanonicalStep04({
      ...stepCommon(registered),
      threadOutRef: verdict.threadOutRef,
      evidence,
      referenceScriptUtxo: registered.references[4]!,
      witnessReferenceScripts: harness.witnessReferenceScripts,
    });
    expect(minted.fraudProofUnit).toBeTruthy();
    coverage.reason("OutputNonCanonical", "accepted_invalid");
  }, 900_000);

  it("convicts forced rejections of maximum outputs whose inline datums are the densest canonical Data, fitting every datum step within its planned memory and one transaction's budget", async () => {
    const datumLength = MAXIMUM_OUTPUT_BYTES - DATUM_OFFSET;
    let costliest: CompleteSignedTransactionMeasurement | undefined;
    for (const shape of [
      "map-chain",
      "generic-constructors",
      "flat-integers",
      "chunked-bytes",
    ] as const) {
      const datum = densePlutusData(shape, datumLength);
      expect(isCanonicalMidgardDataStream(datum)).toBe(true);
      const output = outputWithInlineDatum(datum);
      expect(output).toHaveLength(MAXIMUM_OUTPUT_BYTES);
      const s = await forcedDatumScenario(nodeTransaction(output));
      expect(s.evidence.canonical).toBe(true);
      const plan = s.evidence.datumPlan!;
      const thread = await s.bindToDatum();
      const verdict = await datumToVerdict({
        registered: s.registered,
        threadOutRef: thread,
        evidence: s.evidence,
        label: `dense-${shape}`,
        // Never above the planned estimate, which the planner keeps below
        // one transaction's budget.
        onStep: ({ executionMemory, executionSteps }, index) => {
          expect(executionMemory).toBeLessThanOrEqual(
            BigInt(plan.steps[index]!.estimatedMemory),
          );
          expect(executionMemory).toBeLessThanOrEqual(13_200_000n);
          expect(executionSteps).toBeLessThanOrEqual(8_000_000_000n);
        },
      });
      expect(verdict.datumSteps).toBe(plan.steps.length);
      if (
        costliest === undefined ||
        verdict.costliest!.executionMemory > costliest.executionMemory
      )
        costliest = verdict.costliest;
      const minted = await submitTransactionOutputNonCanonicalStep04({
        ...stepCommon(s.registered),
        threadOutRef: verdict.threadOutRef,
        evidence: s.evidence,
        referenceScriptUtxo: s.registered.references[4]!,
        witnessReferenceScripts: s.harness.witnessReferenceScripts,
      });
      expect(minted.fraudProofUnit).toBeTruthy();
    }
    record("tonc_datum_verdict_step_max", costliest!);
  }, 3_600_000);

  it("writes the measured fit when asked", async () => {
    for (const [name, measurement] of fit) {
      expect(measurement.l1ByteMargin, name).toBeGreaterThan(0);
    }
    const output = process.env.MIDGARD_FIT_OUT;
    if (output !== undefined) {
      await writeFile(
        output,
        JSON.stringify(
          fit.map(([name, measurement]) => ({
            name,
            signedBytes: measurement.completeSignedBytes,
            memoryUnits: measurement.executionMemory.toString(),
            cpuUnits: measurement.executionSteps.toString(),
          })),
          null,
          2,
        ),
      );
    }
  });
});
