import {
  buildMidgardBoundedItem,
  buildMidgardBoundedItemChunkProof,
  type MidgardBoundedItemChunkProof,
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
  type ResolvedOutputEvidence,
  resolvedOutputScanControlData,
} from "./resolved-output-non-canonical.js";
import {
  ResolvedOutputScanControlSchema,
  ResolvedOutputStep04DatumSchema,
  ResolvedOutputStep04RedeemerSchema,
  ResolvedOutputStep05DatumSchema,
} from "./schemas.js";

const proofData = (proof: MidgardBoundedItemChunkProof) => ({
  version: BigInt(proof.version),
  field_index: BigInt(proof.fieldIndex),
  item_index: BigInt(proof.itemIndex),
  total_length: BigInt(proof.totalLength),
  chunk_index: BigInt(proof.chunkIndex),
  chunk: proof.chunk.toString("hex"),
  frontier: proof.frontier.peaks.map(({ height, hash }) => ({
    height: BigInt(height),
    hash: hash.toString("hex"),
  })),
  siblings: proof.siblings.map((hash) => hash.toString("hex")),
});

export const submitResolvedOutputNonCanonicalStep04 = async ({
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
  const stepIndex = 3;
  const { threadUtxo, threadToken } = await requireLinearFaultThreadUtxo({
    lucid,
    contracts,
    categoryId,
    family: "resolved-output-non-canonical",
    stepIndex,
    threadOutRef,
  });
  const state = requireLinearFaultStepState<{
    subject: unknown;
    descriptor_cbor: string;
    control: { cursor: bigint };
  }>({
    threadUtxo,
    signer,
    schema: ResolvedOutputStep04DatumSchema as never,
    family: "resolved-output-non-canonical",
    stepIndex,
  });
  if (state.descriptor_cbor !== evidence.resolved.descriptorCborHex)
    throw new Error(
      "resolved-output-non-canonical: descriptor checkpoint changed",
    );
  const encodedState = Data.to(
    state.control as never,
    ResolvedOutputScanControlSchema as never,
  );
  const controlIndex = evidence.scanControls.findIndex(
    (control) =>
      Data.to(
        resolvedOutputScanControlData(control) as never,
        ResolvedOutputScanControlSchema as never,
      ) === encodedState,
  );
  if (controlIndex < 0)
    throw new Error(
      "resolved-output-non-canonical: reconstruction checkpoint is outside authenticated trace",
    );
  const nextControl = evidence.scanControls[controlIndex + 1];
  // A terminal scan control is itself an authenticated checkpoint. Persist it
  // before asking `finish_v1` (canonical) or `step_v1` (trailing/malformed)
  // to produce the final verdict on the following transaction.
  const terminal = nextControl === undefined;
  const item =
    evidence.canonicalTrace?.item ??
    buildMidgardBoundedItem({
      fieldIndex: 2,
      itemIndex: evidence.resolved.outputIndex,
      bytes: Buffer.from(evidence.resolved.outputCborHex, "hex"),
    });
  const chunkIndex = Math.floor(Number(state.control.cursor) / 4_095);
  const nextChunkIndex =
    chunkIndex + 1 < item.chunkHashes.length ? chunkIndex + 1 : null;
  const action =
    nextControl === undefined && !evidence.outputIsNonCanonical
      ? ("FinalizeCanonical" as const)
      : {
          Advance: {
            chunk_proof: proofData(
              buildMidgardBoundedItemChunkProof(item, chunkIndex),
            ),
            next_chunk_proof:
              nextChunkIndex === null
                ? null
                : proofData(
                    buildMidgardBoundedItemChunkProof(item, nextChunkIndex),
                  ),
          },
        };
  const nextDatum = Data.to(
    {
      fraud_prover: signer.paymentKeyHash,
      data: terminal
        ? {
            subject: evidence.subject,
            output_is_non_canonical: evidence.outputIsNonCanonical,
          }
        : {
            subject: evidence.subject,
            descriptor_cbor: evidence.resolved.descriptorCborHex,
            control: resolvedOutputScanControlData(nextControl!),
          },
    } as never,
    (terminal
      ? ResolvedOutputStep05DatumSchema
      : ResolvedOutputStep04DatumSchema) as never,
  );
  const nextStepIndex = terminal ? 4 : 3;
  const outputMatches = computationThreadOutputPredicate({
    address: contracts.steps[nextStepIndex].spendingScriptAddress,
    datum: nextDatum,
    unit: threadToken.unit,
  });
  let outputIndex: bigint | undefined;
  const redeemer = ((ctx) => {
    requireOwnSpendPurpose(
      ctx,
      threadUtxo,
      "resolved-output-non-canonical step-04",
    );
    const inputIndex = requireInputIndex(
      ctx,
      threadUtxo,
      "resolved-output-non-canonical step-04",
    );
    outputIndex = requireUniqueOutputIndex(
      ctx.outputs,
      outputMatches,
      "resolved-output-non-canonical step-04 output",
    );
    return Data.to(
      {
        Continue: [
          { input_index: inputIndex, output_index: outputIndex, action },
        ],
      } as never,
      ResolvedOutputStep04RedeemerSchema as never,
    );
  }) satisfies BuildTxWithRedeemer;
  const txHash = await submitLinearFaultContinue({
    lucid,
    signerPaymentKeyHash: signer.paymentKeyHash,
    threadUtxo,
    threadUnit: threadToken.unit,
    stepReference: requireLinearFaultReferenceScript({
      utxo: referenceScriptUtxo,
      expectedScriptHash: contracts.steps[3].spendingScriptHash,
      family: "resolved-output-non-canonical",
      stepIndex,
    }),
    stepScript: contracts.steps[3].spendingScript,
    stepRole: "resolved-output-non-canonical step-04",
    nextAddress: contracts.steps[nextStepIndex].spendingScriptAddress,
    nextDatum,
    redeemer,
    carriageUtxos: [],
    preSubmitBoundary,
    awaitConfirmation,
  });
  if (outputIndex === undefined)
    throw new Error("resolved-output-non-canonical: step-04 layout unresolved");
  return {
    txHash,
    nextThreadOutRef: `${txHash}#${outputIndex.toString()}`,
    terminal,
  };
};
