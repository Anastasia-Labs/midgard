import {
  requireInputIndex,
  requireOwnSpendPurpose,
  requireUniqueOutputIndex,
} from "@al-ft/midgard-sdk";
import {
  type BuildTxWithRedeemer,
  Data,
  type LucidEvolution,
  type UTxO,
} from "@lucid-evolution/lucid";

import {
  requireLinearFaultReferenceScript,
  requireLinearFaultStepState,
  requireLinearFaultThreadUtxo,
} from "../linear-fault-family.js";
import { submitLinearFaultContinue } from "../linear-fault-submit.js";
import type { ResolvedProverSigner } from "../runtime.js";
import { computationThreadOutputPredicate } from "../tx-layout.js";
import type { FraudProofPreSubmitBoundary } from "../workflow/transaction-boundary.js";
import type { RedeemerCanonicityContracts } from "./contracts.js";
import { redeemerDataScanState, redeemerDataStepArgs } from "./data-plan.js";
import type { RedeemerCanonicityEvidence } from "./family.js";
import {
  RedeemerCanonicityDataScanStateSchema,
  RedeemerCanonicityStep02DataDatumSchema,
  RedeemerCanonicityStep02DataRedeemerSchema,
  RedeemerCanonicityStep03DatumSchema,
} from "./schemas.js";

const FAMILY = "redeemer-canonicity";
const STEP_INDEX = 2;
const TERMINAL_STEP_INDEX = 3;

type DataScanState = {
  bound: {
    subject: unknown;
    witness_set_hash: string;
    redeemer_index: bigint;
  };
  data_length: bigint;
  chunk_hashes: readonly string[];
  cursor: bigint;
  stack_root: string;
};

/**
 * Spends the data step once: reads the planned nodes of the redeemer's data
 * and continues on the same step, or finishes on the terminal step with the
 * data's verdict. The planned step is the one whose checkpoint the thread
 * holds, so a restart resumes where the chain stands.
 */
export const submitRedeemerCanonicityStep02Data = async ({
  lucid,
  contracts,
  categoryId,
  signer,
  threadOutRef,
  evidence,
  referenceScriptUtxo,
  preSubmitBoundary,
  awaitConfirmation = true,
}: {
  readonly lucid: LucidEvolution;
  readonly contracts: RedeemerCanonicityContracts;
  readonly categoryId: string;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly evidence: RedeemerCanonicityEvidence;
  readonly referenceScriptUtxo: UTxO;
  readonly preSubmitBoundary?: FraudProofPreSubmitBoundary;
  readonly awaitConfirmation?: boolean;
}) => {
  const plan = evidence.dataPlan;
  if (plan === null)
    throw new Error(`${FAMILY}: the redeemer has no data step to run`);
  const { threadUtxo, threadToken } = await requireLinearFaultThreadUtxo({
    lucid,
    contracts,
    categoryId,
    family: FAMILY,
    stepIndex: STEP_INDEX,
    threadOutRef,
  });
  const state = requireLinearFaultStepState<DataScanState>({
    threadUtxo,
    signer,
    schema: RedeemerCanonicityStep02DataDatumSchema as never,
    family: FAMILY,
    stepIndex: STEP_INDEX,
  });
  const step = plan.steps.find(
    (candidate) =>
      BigInt(candidate.cursor) === state.cursor &&
      candidate.stackRoot === state.stack_root,
  );
  if (step === undefined)
    throw new Error(`${FAMILY}: data checkpoint is not in the planned read`);
  const expected = redeemerDataScanState(state.bound, plan, step);
  if (
    state.bound.redeemer_index !== BigInt(evidence.redeemerIndex) ||
    Data.to(state as never, RedeemerCanonicityDataScanStateSchema as never) !==
      Data.to(expected as never, RedeemerCanonicityDataScanStateSchema as never)
  )
    throw new Error(`${FAMILY}: data step state differs from evidence`);
  const stepReference = requireLinearFaultReferenceScript({
    utxo: referenceScriptUtxo,
    expectedScriptHash: contracts.steps[STEP_INDEX].spendingScriptHash,
    family: FAMILY,
    stepIndex: STEP_INDEX,
  });
  const finished = step.next.kind === "finish";
  const nextDatum =
    step.next.kind === "finish"
      ? Data.to(
          {
            fraud_prover: signer.paymentKeyHash,
            data: { bound: state.bound, canonical: step.next.canonical },
          } as never,
          RedeemerCanonicityStep03DatumSchema as never,
        )
      : Data.to(
          {
            fraud_prover: signer.paymentKeyHash,
            data: redeemerDataScanState(state.bound, plan, step.next),
          } as never,
          RedeemerCanonicityStep02DataDatumSchema as never,
        );
  const nextStepIndex = finished ? TERMINAL_STEP_INDEX : STEP_INDEX;
  const outputMatches = computationThreadOutputPredicate({
    address: contracts.steps[nextStepIndex].spendingScriptAddress,
    datum: nextDatum,
    unit: threadToken.unit,
  });
  let outputIndex: bigint | undefined;
  const redeemer = ((ctx) => {
    requireOwnSpendPurpose(ctx, threadUtxo, `${FAMILY} step-02 data`);
    const inputIndex = requireInputIndex(ctx, threadUtxo, FAMILY);
    outputIndex = requireUniqueOutputIndex(
      ctx.outputs,
      outputMatches,
      `${FAMILY} step-02 data output`,
    );
    return Data.to(
      {
        Continue: [
          {
            input_index: inputIndex,
            output_index: outputIndex,
            ...redeemerDataStepArgs(plan, step),
          },
        ],
      } as never,
      RedeemerCanonicityStep02DataRedeemerSchema as never,
    );
  }) satisfies BuildTxWithRedeemer;
  const txHash = await submitLinearFaultContinue({
    lucid,
    signerPaymentKeyHash: signer.paymentKeyHash,
    threadUtxo,
    threadUnit: threadToken.unit,
    stepReference,
    stepScript: contracts.steps[STEP_INDEX].spendingScript,
    stepRole: `${FAMILY} step-02 data`,
    nextAddress: contracts.steps[nextStepIndex].spendingScriptAddress,
    nextDatum,
    redeemer,
    carriageUtxos: [],
    preSubmitBoundary,
    awaitConfirmation,
  });
  if (outputIndex === undefined)
    throw new Error(`${FAMILY}: step-02 data layout unresolved`);
  return {
    txHash,
    nextThreadOutRef: `${txHash}#${outputIndex.toString()}`,
    finished,
    nextStage: finished ? ("step03" as const) : ("step02Data" as const),
  };
};
