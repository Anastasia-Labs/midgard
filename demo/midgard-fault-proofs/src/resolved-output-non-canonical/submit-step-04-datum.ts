import {
  buildMidgardBoundedItem,
  buildMidgardBoundedItemChunkProof,
  decodeMidgardLedgerOutputCommitment,
} from "@al-ft/midgard-core";
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
  canonicalDataStreamFrameData,
  type CanonicalDataStreamStep,
} from "../canonical-data-stream-plan.js";
import {
  requireLinearFaultReferenceScript,
  requireLinearFaultStepState,
  requireLinearFaultThreadUtxo,
} from "../linear-fault-family.js";
import { submitLinearFaultContinue } from "../linear-fault-submit.js";
import type { ResolvedProverSigner } from "../runtime.js";
import { computationThreadOutputPredicate } from "../tx-layout.js";
import type { FraudProofPreSubmitBoundary } from "../workflow/transaction-boundary.js";
import type { ResolvedOutputNonCanonicalContracts } from "./contracts.js";
import {
  RESOLVED_OUTPUT_DATUM_CHUNK_BYTES,
  type ResolvedOutputDatumPlan,
  resolvedOutputDatumState,
} from "./datum-plan.js";
import { resolvedOutputChunkProofData } from "./reconstruction-plan.js";
import type { ResolvedOutputEvidence } from "./resolved-output-non-canonical.js";
import {
  ResolvedOutputDatumVerdictStateSchema,
  ResolvedOutputStep04DatumDatumSchema,
  ResolvedOutputStep04DatumRedeemerSchema,
  ResolvedOutputStep05DatumSchema,
} from "./schemas.js";

const FAMILY = "resolved-output-non-canonical";
const STEP_INDEX = 4;
const TERMINAL_STEP_INDEX = 5;

type DatumVerdictState = {
  subject: unknown;
  item_commitment: string;
  datum_start: bigint;
  datum_end: bigint;
  cursor: bigint;
  stack_root: string;
};

/**
 * The datum step's spend arguments for one planned step: the proofs of the
 * output chunk holding the cursor and, while the datum continues past it, of
 * the next one, plus the revealed frames and the node budget.
 */
export const resolvedOutputDatumStepArgs = ({
  evidence,
  plan,
  step,
  item = evidence.canonicalTrace?.item ??
    buildMidgardBoundedItem({
      fieldIndex: 2,
      itemIndex: evidence.resolved.outputIndex,
      bytes: Buffer.from(evidence.resolved.outputCborHex, "hex"),
    }),
}: {
  readonly evidence: ResolvedOutputEvidence;
  readonly plan: ResolvedOutputDatumPlan;
  readonly step: CanonicalDataStreamStep;
  /** The bounded item the chunk proofs open; the retained output's by default. */
  readonly item?: ReturnType<typeof buildMidgardBoundedItem>;
}) => {
  const outputLength = evidence.resolved.outputCborHex.length / 2;
  const chunkIndex = Math.floor(
    Math.min(step.cursor, outputLength - 1) / RESOLVED_OUTPUT_DATUM_CHUNK_BYTES,
  );
  const currentEnd = Math.min(
    (chunkIndex + 1) * RESOLVED_OUTPUT_DATUM_CHUNK_BYTES,
    outputLength,
  );
  return {
    chunk_proof: resolvedOutputChunkProofData(
      buildMidgardBoundedItemChunkProof(item, chunkIndex),
    ),
    next_chunk_proof:
      plan.datumEnd > currentEnd
        ? resolvedOutputChunkProofData(
            buildMidgardBoundedItemChunkProof(item, chunkIndex + 1),
          )
        : null,
    revealed: step.stack
      .slice(0, step.revealedCount)
      .map(canonicalDataStreamFrameData),
    tail_root: step.tailRoot,
    node_budget: BigInt(step.nodeBudget),
  };
};

/**
 * Spends the datum step once: reads the planned nodes of the output's inline
 * datum and continues on the same step, or finishes on the terminal step with
 * the output's verdict. The planned step is the one whose checkpoint the
 * thread holds, so a restart resumes where the chain stands.
 */
export const submitResolvedOutputNonCanonicalStep04Datum = async ({
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
  readonly contracts: ResolvedOutputNonCanonicalContracts;
  readonly categoryId: string;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly evidence: ResolvedOutputEvidence;
  readonly referenceScriptUtxo: UTxO;
  readonly preSubmitBoundary?: FraudProofPreSubmitBoundary;
  readonly awaitConfirmation?: boolean;
}) => {
  const plan = evidence.datumPlan;
  if (plan === null)
    throw new Error(`${FAMILY}: the resolved output has no datum to read`);
  const { threadUtxo, threadToken } = await requireLinearFaultThreadUtxo({
    lucid,
    contracts,
    categoryId,
    family: FAMILY,
    stepIndex: STEP_INDEX,
    threadOutRef,
  });
  const state = requireLinearFaultStepState<DatumVerdictState>({
    threadUtxo,
    signer,
    schema: ResolvedOutputStep04DatumDatumSchema as never,
    family: FAMILY,
    stepIndex: STEP_INDEX,
  });
  const step = plan.steps.find(
    (candidate) =>
      BigInt(candidate.cursor) === state.cursor &&
      candidate.stackRoot === state.stack_root,
  );
  if (step === undefined)
    throw new Error(`${FAMILY}: datum checkpoint is not in the planned read`);
  const itemCommitment = decodeMidgardLedgerOutputCommitment(
    Buffer.from(evidence.resolved.descriptorCborHex, "hex"),
  ).itemCommitment.toString("hex");
  const expected = resolvedOutputDatumState(
    evidence.subject,
    itemCommitment,
    plan,
    step,
  );
  if (
    Data.to(state as never, ResolvedOutputDatumVerdictStateSchema as never) !==
    Data.to(expected as never, ResolvedOutputDatumVerdictStateSchema as never)
  )
    throw new Error(`${FAMILY}: datum step state differs from evidence`);
  const args = resolvedOutputDatumStepArgs({ evidence, plan, step });
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
            data: {
              subject: evidence.subject,
              output_is_non_canonical: !step.next.canonical,
            },
          } as never,
          ResolvedOutputStep05DatumSchema as never,
        )
      : Data.to(
          {
            fraud_prover: signer.paymentKeyHash,
            data: resolvedOutputDatumState(
              evidence.subject,
              itemCommitment,
              plan,
              step.next,
            ),
          } as never,
          ResolvedOutputStep04DatumDatumSchema as never,
        );
  const nextStepIndex = finished ? TERMINAL_STEP_INDEX : STEP_INDEX;
  const outputMatches = computationThreadOutputPredicate({
    address: contracts.steps[nextStepIndex].spendingScriptAddress,
    datum: nextDatum,
    unit: threadToken.unit,
  });
  let outputIndex: bigint | undefined;
  const redeemer = ((ctx) => {
    requireOwnSpendPurpose(ctx, threadUtxo, `${FAMILY} step-04 datum`);
    const inputIndex = requireInputIndex(ctx, threadUtxo, FAMILY);
    outputIndex = requireUniqueOutputIndex(
      ctx.outputs,
      outputMatches,
      `${FAMILY} step-04 datum output`,
    );
    return Data.to(
      {
        Continue: [
          {
            input_index: inputIndex,
            output_index: outputIndex,
            ...args,
          },
        ],
      } as never,
      ResolvedOutputStep04DatumRedeemerSchema as never,
    );
  }) satisfies BuildTxWithRedeemer;
  const txHash = await submitLinearFaultContinue({
    lucid,
    signerPaymentKeyHash: signer.paymentKeyHash,
    threadUtxo,
    threadUnit: threadToken.unit,
    stepReference,
    stepScript: contracts.steps[STEP_INDEX].spendingScript,
    stepRole: `${FAMILY} step-04 datum`,
    nextAddress: contracts.steps[nextStepIndex].spendingScriptAddress,
    nextDatum,
    redeemer,
    carriageUtxos: [],
    preSubmitBoundary,
    awaitConfirmation,
  });
  if (outputIndex === undefined)
    throw new Error(`${FAMILY}: step-04 datum layout unresolved`);
  return {
    txHash,
    nextThreadOutRef: `${txHash}#${outputIndex.toString()}`,
    finished,
    nextStage: finished ? ("step05" as const) : ("step04Datum" as const),
  };
};
