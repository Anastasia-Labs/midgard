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
import { expect } from "vitest";

import {
  requireLinearFaultReferenceScript,
  requireLinearFaultStepState,
  requireLinearFaultThreadUtxo,
} from "../src/linear-fault-family.js";
import { submitLinearFaultContinue } from "../src/linear-fault-submit.js";
import {
  prepareRedeemerCanonicityEvidence,
  REDEEMER_DATA_CHUNK_BYTES,
  RedeemerCanonicityStep02DataDatumSchema,
  RedeemerCanonicityStep02DataRedeemerSchema,
  RedeemerCanonicityStep03DatumSchema,
  redeemerDataScanState,
  redeemerDataStepArgs,
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
import { network } from "./support/emulator/blueprints.js";
import { expectOnchainRefusal } from "./support/emulator/expect-onchain-refusal.js";
import { makeFaultProofEmulatorHarness } from "./support/emulator/harness.js";
import {
  captureEmulatorSubmission,
  type CompleteSignedTransactionMeasurement,
} from "./support/emulator/measurement.js";
import { publishPlainReferenceScriptUtxo } from "./support/emulator/reference-scripts.js";

/**
 * Commits one transaction whose redeemer field holds a single Spend item
 * carrying `redeemerData`, in an accepted block, and opens a thread claiming
 * that acceptance wrong: init, step 01 and step 02. Returns the thread at the
 * data step.
 */
export const openAcceptedThread = async (redeemerData: Buffer) => {
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
export const dataStep = async (thread: Thread, threadOutRef: string) =>
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
export const readToVerdict = async (
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
export const recordedCanonical = async (
  thread: Thread,
  threadOutRef: string,
) => {
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

export const mint = async (thread: Thread, threadOutRef: string) =>
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
export const refuseFinalize = async (thread: Thread, threadOutRef: string) => {
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
 * that differs from the committed data, at the cursor or past the cursor
 * chunk's boundary, a successor other than the honest one, and (with frames
 * hidden) a tail that does not hash to the stored stack.
 */
export const refuseDataSeams = async (thread: Thread, threadOutRef: string) => {
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
  // A window into the next chunk: the cursor's chunk is the span's, the
  // bytes past the boundary are not.
  if (step.windowLength > REDEEMER_DATA_CHUNK_BYTES) {
    const crossing = Buffer.from(args.window, "hex");
    crossing[REDEEMER_DATA_CHUNK_BYTES] =
      crossing[REDEEMER_DATA_CHUNK_BYTES]! ^ 0xff;
    await expectOnchainRefusal(() =>
      dataStepRaw({
        thread,
        threadOutRef,
        args: { ...args, window: crossing.toString("hex") },
        next: honest,
      }),
    );
  }
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
