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
import type { TransactionOutputNonCanonicalContracts } from "./contracts.js";
import { canonicalDataStreamFrameData } from "../canonical-data-stream-plan.js";
import type { TransactionOutputDatumStep } from "./datum-plan.js";
import {
  TransactionOutputScanStateSchema,
  TransactionOutputStep03DatumDatumSchema,
  TransactionOutputStep03DatumRedeemerSchema,
  TransactionOutputStep04DatumSchema,
} from "./schemas.js";
import {
  type TransactionOutputEvidence,
  transactionOutputScanControlData,
} from "./transaction-output-non-canonical.js";

const FAMILY = "transaction-output-non-canonical";
const STEP_INDEX = 3;
const TERMINAL_STEP_INDEX = 4;

/** The envelope scan's canonical terminal state, which the datum step carries. */
export const transactionOutputDatumScanState = (
  evidence: TransactionOutputEvidence,
  outcome: bigint,
) => ({
  subject: evidence.subject,
  output_index: BigInt(evidence.itemIndex),
  item_length: BigInt(evidence.itemLength),
  item_hash: evidence.itemHash,
  chunk_hashes: evidence.chunkHashes,
  control: transactionOutputScanControlData(evidence.scanControls.at(-1)!),
  outcome,
});

/** The redeemer arguments of one planned datum step, before indexes. */
export const transactionOutputDatumStepArgs = (
  evidence: TransactionOutputEvidence,
  step: TransactionOutputDatumStep,
) => ({
  window: Buffer.from(evidence.itemHex, "hex")
    .subarray(step.windowStart, step.windowStart + step.windowLength)
    .toString("hex"),
  revealed: step.stack
    .slice(0, step.revealedCount)
    .map(canonicalDataStreamFrameData),
  tail_root: step.tailRoot,
  node_budget: BigInt(step.nodeBudget),
});

/**
 * Spends the datum step once: reads the planned nodes of the output's inline
 * datum and continues on the same step, or finishes on the terminal step
 * with the datum's verdict.
 */
export const submitTransactionOutputNonCanonicalStep03Datum = async ({
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
  readonly contracts: TransactionOutputNonCanonicalContracts;
  readonly categoryId: string;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly evidence: TransactionOutputEvidence;
  readonly referenceScriptUtxo: UTxO;
  readonly preSubmitBoundary?: FraudProofPreSubmitBoundary;
  readonly awaitConfirmation?: boolean;
}) => {
  const plan = evidence.datumPlan;
  if (plan === null)
    throw new Error(`${FAMILY}: the output has no datum step to run`);
  const { threadUtxo, threadToken } = await requireLinearFaultThreadUtxo({
    lucid,
    contracts,
    categoryId,
    family: FAMILY,
    stepIndex: STEP_INDEX,
    threadOutRef,
  });
  const state = requireLinearFaultStepState<{
    scan: unknown;
    cursor: bigint;
    stack_root: string;
  }>({
    threadUtxo,
    signer,
    schema: TransactionOutputStep03DatumDatumSchema as never,
    family: FAMILY,
    stepIndex: STEP_INDEX,
  });
  const scan = transactionOutputDatumScanState(evidence, 1n);
  if (
    Data.to(state.scan as never, TransactionOutputScanStateSchema as never) !==
    Data.to(scan as never, TransactionOutputScanStateSchema as never)
  )
    throw new Error(`${FAMILY}: datum step scan identity changed`);
  const step = plan.steps.find(
    (candidate) =>
      BigInt(candidate.cursor) === state.cursor &&
      candidate.stackRoot === state.stack_root,
  );
  if (step === undefined)
    throw new Error(`${FAMILY}: datum checkpoint is not in the planned read`);
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
            data: transactionOutputDatumScanState(
              evidence,
              step.next.canonical ? 1n : 2n,
            ),
          } as never,
          TransactionOutputStep04DatumSchema as never,
        )
      : Data.to(
          {
            fraud_prover: signer.paymentKeyHash,
            data: {
              scan,
              cursor: BigInt(step.next.cursor),
              stack_root: step.next.stackRoot,
            },
          } as never,
          TransactionOutputStep03DatumDatumSchema as never,
        );
  const nextStepIndex = finished ? TERMINAL_STEP_INDEX : STEP_INDEX;
  const outputMatches = computationThreadOutputPredicate({
    address: contracts.steps[nextStepIndex].spendingScriptAddress,
    datum: nextDatum,
    unit: threadToken.unit,
  });
  let outputIndex: bigint | undefined;
  const redeemer = ((ctx) => {
    requireOwnSpendPurpose(ctx, threadUtxo, `${FAMILY} step-03 datum`);
    const inputIndex = requireInputIndex(ctx, threadUtxo, FAMILY);
    outputIndex = requireUniqueOutputIndex(
      ctx.outputs,
      outputMatches,
      `${FAMILY} step-03 datum output`,
    );
    return Data.to(
      {
        Continue: [
          {
            input_index: inputIndex,
            output_index: outputIndex,
            ...transactionOutputDatumStepArgs(evidence, step),
          },
        ],
      } as never,
      TransactionOutputStep03DatumRedeemerSchema as never,
    );
  }) satisfies BuildTxWithRedeemer;
  const txHash = await submitLinearFaultContinue({
    lucid,
    signerPaymentKeyHash: signer.paymentKeyHash,
    threadUtxo,
    threadUnit: threadToken.unit,
    stepReference,
    stepScript: contracts.steps[STEP_INDEX].spendingScript,
    stepRole: `${FAMILY} step-03 datum`,
    nextAddress: contracts.steps[nextStepIndex].spendingScriptAddress,
    nextDatum,
    redeemer,
    carriageUtxos: [],
    preSubmitBoundary,
    awaitConfirmation,
  });
  if (outputIndex === undefined)
    throw new Error(`${FAMILY}: step-03 datum layout unresolved`);
  return {
    txHash,
    nextThreadOutRef: `${txHash}#${outputIndex.toString()}`,
    finished,
    nextStage: finished ? ("step04" as const) : ("step03Datum" as const),
  };
};
