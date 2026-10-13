import { writeFile } from "node:fs/promises";

import {
  deriveMidgardNativeTxWitnessSetCompact,
  encodeMidgardNativeTxWitnessSetCompact,
  midgardFieldCommitment,
} from "@al-ft/midgard-core";
import {
  acceptedVerdictSubject,
  requireInputIndex,
  requireUniqueOutputIndex,
} from "@al-ft/midgard-sdk";
import { type BuildTxWithRedeemer, Data } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  requireLinearFaultReferenceScript,
  requireLinearFaultStepState,
  requireLinearFaultThreadUtxo,
} from "../src/linear-fault-family.js";
import { submitLinearFaultContinue } from "../src/linear-fault-submit.js";
import {
  prepareRedeemerCanonicityEvidence,
  redeemerCanonicityEvidenceCloses,
  redeemerDataScanState,
  redeemerDataStepArgs,
  RedeemerCanonicityStep02DataDatumSchema,
  RedeemerCanonicityStep02DataRedeemerSchema,
  RedeemerCanonicityStep03DatumSchema,
  submitRedeemerCanonicityStep01Accepted,
  submitRedeemerCanonicityStep02,
  submitRedeemerCanonicityStep02Data,
  submitRedeemerCanonicityStep03,
} from "../src/redeemer-canonicity/index.js";
import { computationThreadOutputPredicate } from "../src/tx-layout.js";
import {
  familyContracts,
  finalizeWithoutGuards,
  initThread,
  publishFamilyReferences,
} from "./redeemer-canonicity-lifecycle.init-thread.js";
import {
  type CommittedFieldShapeEmulatorHarness,
  setupCommittedFieldShapeScenario,
} from "./support/committed-field-shape-emulator.js";
import {
  type DensePlutusDataShape,
  densePlutusData,
} from "./support/dense-plutus-data.js";
import { network } from "./support/emulator/blueprints.js";
import { expectOnchainRefusal } from "./support/emulator/expect-onchain-refusal.js";
import { makeFaultProofEmulatorHarness } from "./support/emulator/harness.js";
import {
  captureEmulatorSubmission,
  type CompleteSignedTransactionMeasurement,
} from "./support/emulator/measurement.js";
import { publishPlainReferenceScriptUtxo } from "./support/emulator/reference-scripts.js";

/** The largest redeemer data a field-8 item can carry. */
const MAXIMUM_REDEEMER_DATA_BYTES = 32_743;

/** Below this, a data step's execution budget is never at risk. */
const STEP_MEMORY_CAP = 13_200_000n;
const STEP_CPU_CAP = 8_000_000_000n;

const fit: [string, CompleteSignedTransactionMeasurement][] = [];

const progress = (message: string): void => {
  if (process.env.MIDGARD_PRINT_FIT === "1")
    console.info(`[redeemer-canonicity-streamed] ${message}`);
};

const hex = (text: string) => Buffer.from(text, "hex");

/**
 * Commits one transaction whose redeemer field holds a single Spend item
 * carrying `redeemerData`, in an accepted block, and opens a thread claiming
 * that acceptance wrong: init, step 01 and step 02. Returns the thread at the
 * data step.
 */
const openAcceptedThread = async (redeemerData: Buffer) => {
  const harness = await makeFaultProofEmulatorHarness({
    contractOptions: {
      realRedeemerCanonicity: true,
      alwaysFraudProofCatalogue: true,
    },
  });
  const contracts = familyContracts(harness);
  const category = harness.catalogue.categories.redeemerCanonicity;
  if (category === undefined) throw new Error("redeemer category absent");
  const scenario = await setupCommittedFieldShapeScenario({
    harness: harness as unknown as CommittedFieldShapeEmulatorHarness,
    kind: "redeemer-canonicity",
    redeemerData,
  });
  const references = await publishFamilyReferences(harness);
  const certificateReference = (
    await publishPlainReferenceScriptUtxo({
      lucid: harness.funderLucid,
      script: harness.contracts.fieldPreimageCertificate.mintingScript,
      label: "redeemer-canonicity-streamed-certificate",
    })
  ).utxo;
  const evidence = prepareRedeemerCanonicityEvidence({
    finding: {
      subject: acceptedVerdictSubject(scenario.nativeTxId),
      redeemerIndex: 0,
    },
    fieldPreimage: scenario.committedPreimage,
    committedFieldHashHex: midgardFieldCommitment(
      scenario.committedPreimage,
    ).toString("hex"),
  });
  const init = await captureEmulatorSubmission(harness.emulator, () =>
    initThread({
      harness,
      contracts,
      category,
      fraudulentBlockOutRef: scenario.setup.fraudulentBlockOutRef,
    }),
  );
  const [threadUtxo] = await harness.proverLucid.utxosByOutRef([
    {
      txHash: init.result.txHash,
      outputIndex: init.result.firstStepOutputIndex,
    },
  ]);
  if (threadUtxo === undefined) throw new Error("init thread absent");
  const step01 = await captureEmulatorSubmission(harness.emulator, () =>
    submitRedeemerCanonicityStep01Accepted({
      lucid: harness.proverLucid,
      blueprint: harness.realBlueprint,
      network,
      contracts,
      signer: harness.proverSigner,
      finding: evidence,
      threadUtxo,
      threadToken: {
        unit: init.result.computationThreadUnit,
        fraudulentHeaderHash: init.result.fraudulentHeaderHash,
      },
      stateQueueBlockOutRef: scenario.setup.fraudulentBlockOutRef,
      txInclusion: scenario.inclusion,
      referenceScriptUtxo: references[0],
      witnessReferenceScripts: harness.witnessReferenceScripts,
    }),
  );
  const step02 = await captureEmulatorSubmission(harness.emulator, () =>
    submitRedeemerCanonicityStep02({
      lucid: harness.proverLucid,
      contracts,
      categoryId: category.categoryId,
      signer: harness.proverSigner,
      threadOutRef: step01.result.nextThreadOutRef,
      evidence,
      nativeTxCompactCbor: scenario.compactCbor,
      witnessSetCompactCbor: encodeMidgardNativeTxWitnessSetCompact(
        deriveMidgardNativeTxWitnessSetCompact(scenario.fullTx!.witnessSet),
      ).toString("hex"),
      referenceScriptUtxo: references[1],
      certificateReferenceScriptUtxo: certificateReference,
    }),
  );
  expect(step02.result.nextStage).toBe("step02Data");
  const thread = {
    harness,
    contracts,
    categoryId: category.categoryId,
    references,
    evidence,
  };
  return {
    ...thread,
    envelope: [init, step01, step02] as const,
    threadOutRef: step02.result.nextThreadOutRef,
  };
};

type Thread = Omit<
  Awaited<ReturnType<typeof openAcceptedThread>>,
  "envelope" | "threadOutRef"
>;

/** One honest data step from `threadOutRef`. */
const dataStep = async (thread: Thread, threadOutRef: string) =>
  await captureEmulatorSubmission(thread.harness.emulator, () =>
    submitRedeemerCanonicityStep02Data({
      lucid: thread.harness.proverLucid,
      contracts: thread.contracts,
      categoryId: thread.categoryId,
      signer: thread.harness.proverSigner,
      threadOutRef,
      evidence: thread.evidence,
      referenceScriptUtxo: thread.references[2],
    }),
  );

/** Honest data steps from `threadOutRef` until the verdict is handed on. */
const readToVerdict = async (
  thread: Thread,
  threadOutRef: string,
  onStep?: (measurement: CompleteSignedTransactionMeasurement) => void,
) => {
  let current = threadOutRef;
  for (let steps = 1; ; steps += 1) {
    const captured = await dataStep(thread, current);
    onStep?.(captured.measurement);
    current = captured.result.nextThreadOutRef;
    if (captured.result.finished) return { threadOutRef: current, steps };
  }
};

/** The verdict the chain recorded at the terminal step. */
const recordedCanonical = async (thread: Thread, threadOutRef: string) => {
  const { threadUtxo } = await requireLinearFaultThreadUtxo({
    lucid: thread.harness.proverLucid,
    contracts: thread.contracts,
    categoryId: thread.categoryId,
    family: "redeemer-canonicity",
    stepIndex: 3,
    threadOutRef,
  });
  return requireLinearFaultStepState<{ canonical: boolean }>({
    threadUtxo,
    signer: thread.harness.proverSigner,
    schema: RedeemerCanonicityStep03DatumSchema as never,
    family: "redeemer-canonicity",
    stepIndex: 3,
  }).canonical;
};

const mint = async (thread: Thread, threadOutRef: string) =>
  await captureEmulatorSubmission(thread.harness.emulator, () =>
    submitRedeemerCanonicityStep03({
      lucid: thread.harness.proverLucid,
      contracts: thread.contracts,
      categoryId: thread.categoryId,
      signer: thread.harness.proverSigner,
      threadOutRef,
      evidence: thread.evidence,
      referenceScriptUtxo: thread.references[3],
      witnessReferenceScripts: thread.harness.witnessReferenceScripts,
    }),
  );

/**
 * A false acceptance claim walked to the chain's own verdict: the builder
 * will not finalize it, and the finalize built without its guards is refused
 * by the terminal step's contradiction check.
 */
const refuseFinalize = async (thread: Thread, threadOutRef: string) => {
  expect(await recordedCanonical(thread, threadOutRef)).toBe(true);
  await expect(mint(thread, threadOutRef)).rejects.toThrow();
  await expectOnchainRefusal(
    () =>
      finalizeWithoutGuards({
        harness: thread.harness,
        contracts: thread.contracts,
        categoryId: thread.categoryId,
        threadOutRef,
        referenceScriptUtxo: thread.references[3],
      }),
    {
      refusedBy: "fraud_proofs/redeemer_canonicity/step_03",
      check: /^Validator returned false$/u,
    },
  );
};

/**
 * The data step spent from `threadOutRef` with the given arguments and
 * successor state, exactly as the honest builder lays it out.
 */
const dataStepRaw = async ({
  thread,
  threadOutRef,
  args,
  next,
}: {
  readonly thread: Thread;
  readonly threadOutRef: string;
  readonly args: ReturnType<typeof redeemerDataStepArgs>;
  readonly next:
    | { readonly stepIndex: 2; readonly data: unknown }
    | { readonly stepIndex: 3; readonly data: unknown };
}) => {
  const { harness, contracts } = thread;
  const { threadUtxo, threadToken } = await requireLinearFaultThreadUtxo({
    lucid: harness.proverLucid,
    contracts,
    categoryId: thread.categoryId,
    family: "redeemer-canonicity",
    stepIndex: 2,
    threadOutRef,
  });
  const nextDatum = Data.to(
    {
      fraud_prover: harness.proverSigner.paymentKeyHash,
      data: next.data,
    } as never,
    (next.stepIndex === 2
      ? RedeemerCanonicityStep02DataDatumSchema
      : RedeemerCanonicityStep03DatumSchema) as never,
  );
  const nextAddress = contracts.steps[next.stepIndex].spendingScriptAddress;
  const outputMatches = computationThreadOutputPredicate({
    address: nextAddress,
    datum: nextDatum,
    unit: threadToken.unit,
  });
  const redeemer = ((ctx) =>
    Data.to(
      {
        Continue: [
          {
            input_index: requireInputIndex(ctx, threadUtxo, "data step"),
            output_index: requireUniqueOutputIndex(
              ctx.outputs,
              outputMatches,
              "data step output",
            ),
            ...args,
          },
        ],
      } as never,
      RedeemerCanonicityStep02DataRedeemerSchema as never,
    )) satisfies BuildTxWithRedeemer;
  return await submitLinearFaultContinue({
    lucid: harness.proverLucid,
    signerPaymentKeyHash: harness.proverSigner.paymentKeyHash,
    threadUtxo,
    threadUnit: threadToken.unit,
    stepReference: requireLinearFaultReferenceScript({
      utxo: thread.references[2],
      expectedScriptHash: contracts.steps[2].spendingScriptHash,
      family: "redeemer-canonicity",
      stepIndex: 2,
    }),
    stepScript: contracts.steps[2].spendingScript,
    stepRole: "redeemer-canonicity data step",
    nextAddress,
    nextDatum,
    redeemer,
    awaitConfirmation: true,
  });
};

/**
 * At the checkpoint `threadOutRef` holds, the data step refuses a window
 * that differs from the committed data, a successor other than the honest
 * one, and (with frames hidden) a tail that does not hash to the stored
 * stack.
 */
const refuseDataSeams = async (thread: Thread, threadOutRef: string) => {
  const plan = thread.evidence.dataPlan!;
  const { threadUtxo } = await requireLinearFaultThreadUtxo({
    lucid: thread.harness.proverLucid,
    contracts: thread.contracts,
    categoryId: thread.categoryId,
    family: "redeemer-canonicity",
    stepIndex: 2,
    threadOutRef,
  });
  const state = requireLinearFaultStepState<
    ReturnType<typeof redeemerDataScanState>
  >({
    threadUtxo,
    signer: thread.harness.proverSigner,
    schema: RedeemerCanonicityStep02DataDatumSchema as never,
    family: "redeemer-canonicity",
    stepIndex: 2,
  });
  const step = plan.steps.find(
    (candidate) =>
      BigInt(candidate.cursor) === state.cursor &&
      candidate.stackRoot === state.stack_root,
  );
  if (step === undefined) throw new Error("data checkpoint is not planned");
  const args = redeemerDataStepArgs(plan, step);
  const honest =
    step.next.kind === "finish"
      ? {
          stepIndex: 3 as const,
          data: { bound: state.bound, canonical: step.next.canonical },
        }
      : {
          stepIndex: 2 as const,
          data: redeemerDataScanState(state.bound, plan, step.next),
        };
  const window = Buffer.from(args.window, "hex");
  window[step.cursor - step.windowStart] =
    window[step.cursor - step.windowStart]! ^ 0xff;
  await expectOnchainRefusal(() =>
    dataStepRaw({
      thread,
      threadOutRef,
      args: { ...args, window: window.toString("hex") },
      next: honest,
    }),
  );
  const forged =
    step.next.kind === "finish"
      ? {
          stepIndex: 3 as const,
          data: { bound: state.bound, canonical: !step.next.canonical },
        }
      : {
          stepIndex: 2 as const,
          data: {
            ...redeemerDataScanState(state.bound, plan, step.next),
            cursor: BigInt(step.next.cursor) + 1n,
          },
        };
  await expectOnchainRefusal(() =>
    dataStepRaw({ thread, threadOutRef, args, next: forged }),
  );
  if (step.stack.length > step.revealedCount)
    await expectOnchainRefusal(() =>
      dataStepRaw({
        thread,
        threadOutRef,
        args: { ...args, tail_root: "00".repeat(32) },
        next: honest,
      }),
    );
  return step;
};

describe("redeemer-canonicity streamed data verdict", () => {
  it("convicts the acceptance of redeemer data whose indefinite map sits five levels deep", async () => {
    const data = hex("9f9f9f9fbf0101ffffffffff");
    const opened = await openAcceptedThread(data);
    expect(opened.evidence.canonical).toBe(false);
    expect(redeemerCanonicityEvidenceCloses(opened.evidence)).toBe(true);
    const read = await readToVerdict(opened, opened.threadOutRef);
    expect(await recordedCanonical(opened, read.threadOutRef)).toBe(false);
    const minted = await mint(opened, read.threadOutRef);
    expect(minted.result.fraudProofUnit).toBeTruthy();
  }, 600_000);

  it("convicts the acceptance of redeemer data whose only flaw lies past its first 30,000 bytes", async () => {
    const flawAt = 1 + 455 * 66;
    const data = Buffer.concat([
      hex("9f"),
      ...Array.from({ length: 455 }, () =>
        Buffer.concat([hex("5840"), Buffer.alloc(64, 0x5a)]),
      ),
      hex("1800"),
      hex("ff"),
    ]);
    expect(flawAt).toBeGreaterThan(30_000);
    expect(data.subarray(flawAt, flawAt + 2)).toEqual(hex("1800"));
    const opened = await openAcceptedThread(data);
    expect(opened.evidence.canonical).toBe(false);
    expect(opened.evidence.carriage).toBe("Certified");
    const read = await readToVerdict(opened, opened.threadOutRef);
    expect(read.steps).toBe(opened.evidence.dataPlan!.steps.length);
    expect(read.steps).toBeGreaterThan(1);
    expect(await recordedCanonical(opened, read.threadOutRef)).toBe(false);
    const minted = await mint(opened, read.threadOutRef);
    expect(minted.result.fraudProofUnit).toBeTruthy();
  }, 900_000);

  it("refuses a false acceptance claim against canonical redeemer data nested 4,000 lists deep, and every forged data step on the way", async () => {
    const data = Buffer.concat([
      Buffer.alloc(4_000, 0x9f),
      hex("00"),
      Buffer.alloc(4_000, 0xff),
    ]);
    const opened = await openAcceptedThread(data);
    expect(opened.evidence.canonical).toBe(true);
    expect(redeemerCanonicityEvidenceCloses(opened.evidence)).toBe(false);
    const plan = opened.evidence.dataPlan!;
    progress(`4,000 levels: ${plan.steps.length.toString()} data steps`);
    // Forge at the opening checkpoint, then again once frames are hidden
    // below the revealed ones, and at the verdict.
    await refuseDataSeams(opened, opened.threadOutRef);
    const first = await dataStep(opened, opened.threadOutRef);
    expect(first.result.finished).toBe(false);
    const deep = await refuseDataSeams(opened, first.result.nextThreadOutRef);
    expect(deep.stack.length).toBeGreaterThan(deep.revealedCount);
    let current = first.result.nextThreadOutRef;
    for (let index = 1; index < plan.steps.length - 1; index += 1)
      current = (await dataStep(opened, current)).result.nextThreadOutRef;
    const last = await refuseDataSeams(opened, current);
    expect(last.next.kind).toBe("finish");
    const verdict = await dataStep(opened, current);
    expect(verdict.result.finished).toBe(true);
    await refuseFinalize(opened, verdict.result.nextThreadOutRef);
  }, 1_800_000);

  it("refuses a false acceptance claim against canonical 32,743-byte redeemer data in each of the densest shapes, reading each to its verdict, and measures the costliest data step", async () => {
    let costliest: CompleteSignedTransactionMeasurement | undefined;
    let costliestShape = "";
    const counts: Record<string, number> = {};
    // Each data step's measured memory beside the planner's estimate.
    const perStep: Record<string, [string, number][]> = {};
    for (const shape of [
      "map-chain",
      "generic-constructors",
      "flat-integers",
      "chunked-bytes",
    ] as const satisfies readonly DensePlutusDataShape[]) {
      const data = densePlutusData(shape, MAXIMUM_REDEEMER_DATA_BYTES);
      expect(data).toHaveLength(MAXIMUM_REDEEMER_DATA_BYTES);
      const opened = await openAcceptedThread(data);
      expect(opened.evidence.canonical).toBe(true);
      expect(opened.evidence.carriage).toBe("Certified");
      for (const [index, envelope] of opened.envelope.entries())
        fit.push([
          `${shape}-envelope-${index.toString()}`,
          envelope.measurement,
        ]);
      progress(
        `${shape}: ${opened.evidence.dataPlan!.steps.length.toString()} data steps planned`,
      );
      const planned = opened.evidence.dataPlan!.steps;
      const stepMemory: [string, number][] = [];
      perStep[shape] = stepMemory;
      const read = await readToVerdict(opened, opened.threadOutRef, (m) => {
        stepMemory.push([
          m.executionMemory.toString(),
          planned[stepMemory.length]!.estimatedMemory,
        ]);
        expect(m.executionMemory).toBeLessThanOrEqual(STEP_MEMORY_CAP);
        expect(m.executionSteps).toBeLessThanOrEqual(STEP_CPU_CAP);
        expect(m.l1ByteMargin).toBeGreaterThan(0);
        if (
          costliest === undefined ||
          m.executionMemory > costliest.executionMemory
        ) {
          costliest = m;
          costliestShape = shape;
        }
      });
      expect(read.steps).toBe(opened.evidence.dataPlan!.steps.length);
      counts[shape] = read.steps;
      progress(`${shape}: ${read.steps.toString()} data steps taken`);
      await refuseFinalize(opened, read.threadOutRef);
    }
    progress(
      `costliest data step: ${costliestShape} ${costliest!.executionMemory.toString()} mem ${costliest!.executionSteps.toString()} cpu; steps ${JSON.stringify(counts)}`,
    );
    fit.push(["redeemer_canonicity_w1_step_max", costliest!]);
    const output = process.env.MIDGARD_FIT_OUT;
    if (output !== undefined)
      await writeFile(
        output,
        JSON.stringify(
          {
            costliestShape,
            dataSteps: counts,
            perStep,
            rows: fit.map(([name, measurement]) => ({
              name,
              signedBytes: measurement.completeSignedBytes,
              memoryUnits: measurement.executionMemory.toString(),
              cpuUnits: measurement.executionSteps.toString(),
            })),
          },
          null,
          2,
        ),
      );
  }, 7_200_000);
});
