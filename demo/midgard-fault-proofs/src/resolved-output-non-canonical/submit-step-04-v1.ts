import {
  buildMidgardBoundedItemChunkProofV1,
  buildMidgardBoundedItemV1,
  type MidgardBoundedItemChunkProofV1,
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
  requireLinearFaultReferenceScriptV1,
  requireLinearFaultStepStateV1,
  requireLinearFaultThreadUtxoV1,
} from "../linear-fault-family-v1.js";
import { submitLinearFaultContinueV1 } from "../linear-fault-submit-v1.js";
import type { ResolvedProverSigner } from "../runtime.js";
import { computationThreadOutputPredicate } from "../tx-layout.js";
import type { FraudProofPreSubmitBoundaryV1 } from "../workflow/transaction-boundary-v1.js";
import type { ResolvedOutputNonCanonicalContractsV1 } from "./contracts-v1.js";
import {
  type ResolvedOutputEvidenceV1,
  resolvedOutputScanControlDataV1,
} from "./resolved-output-non-canonical-v1.js";
import {
  ResolvedOutputScanControlV1Schema,
  ResolvedOutputStep04DatumV1Schema,
  ResolvedOutputStep04RedeemerV1Schema,
  ResolvedOutputStep05DatumV1Schema,
} from "./schemas-v1.js";

const proofData = (proof: MidgardBoundedItemChunkProofV1) => ({
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

export const submitResolvedOutputNonCanonicalStep04V1 = async ({
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
  readonly contracts: ResolvedOutputNonCanonicalContractsV1;
  readonly categoryId: string;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly evidence: ResolvedOutputEvidenceV1;
  readonly referenceScriptUtxo: UTxO;
  readonly preSubmitBoundary?: FraudProofPreSubmitBoundaryV1;
  readonly awaitConfirmation?: boolean;
}) => {
  const stepIndex = 3;
  const { threadUtxo, threadToken } = await requireLinearFaultThreadUtxoV1({
    lucid,
    contracts,
    categoryId,
    family: "resolved-output-non-canonical",
    stepIndex,
    threadOutRef,
  });
  const state = requireLinearFaultStepStateV1<{
    subject: unknown;
    descriptor_cbor: string;
    control: { cursor: bigint };
  }>({
    threadUtxo,
    signer,
    schema: ResolvedOutputStep04DatumV1Schema as never,
    family: "resolved-output-non-canonical",
    stepIndex,
  });
  if (state.descriptor_cbor !== evidence.resolved.descriptorCborHex)
    throw new Error(
      "resolved-output-non-canonical: descriptor checkpoint changed",
    );
  const encodedState = Data.to(
    state.control as never,
    ResolvedOutputScanControlV1Schema as never,
  );
  const controlIndex = evidence.scanControls.findIndex(
    (control) =>
      Data.to(
        resolvedOutputScanControlDataV1(control) as never,
        ResolvedOutputScanControlV1Schema as never,
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
    buildMidgardBoundedItemV1({
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
              buildMidgardBoundedItemChunkProofV1(item, chunkIndex),
            ),
            next_chunk_proof:
              nextChunkIndex === null
                ? null
                : proofData(
                    buildMidgardBoundedItemChunkProofV1(item, nextChunkIndex),
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
            control: resolvedOutputScanControlDataV1(nextControl!),
          },
    } as never,
    (terminal
      ? ResolvedOutputStep05DatumV1Schema
      : ResolvedOutputStep04DatumV1Schema) as never,
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
      ResolvedOutputStep04RedeemerV1Schema as never,
    );
  }) satisfies BuildTxWithRedeemer;
  const txHash = await submitLinearFaultContinueV1({
    lucid,
    signerPaymentKeyHash: signer.paymentKeyHash,
    threadUtxo,
    threadUnit: threadToken.unit,
    stepReference: requireLinearFaultReferenceScriptV1({
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
