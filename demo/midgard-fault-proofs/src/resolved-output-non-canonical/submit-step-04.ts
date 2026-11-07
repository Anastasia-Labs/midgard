import { decodeMidgardLedgerOutputCommitment } from "@al-ft/midgard-core";
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
import { resolvedOutputDatumState } from "./datum-plan.js";
import { planResolvedOutputReconstructionTransition } from "./reconstruction-plan.js";
import {
  type ResolvedOutputEvidence,
  resolvedOutputScanControlData,
} from "./resolved-output-non-canonical.js";
import {
  ResolvedOutputStep04DatumDatumSchema,
  ResolvedOutputStep04DatumSchema,
  ResolvedOutputStep04RedeemerSchema,
  ResolvedOutputStep05DatumSchema,
} from "./schemas.js";

/**
 * One step-04 transition from the live checkpoint. The action is the pure
 * `planResolvedOutputReconstructionTransition` of the retained output and the
 * thread's own control: `Advance` continues the self-loop or closes at a
 * structural fault, `FinalizeCanonical` ends the envelope at the exact
 * canonical end, opening the datum step when the output has an inline datum.
 */
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
  const transition = planResolvedOutputReconstructionTransition({
    evidence,
    control: state.control,
  });
  const { terminal } = transition;
  const next = transition.terminal
    ? {
        stage: "step05" as const,
        stepIndex: 5,
        datum: Data.to(
          {
            fraud_prover: signer.paymentKeyHash,
            data: {
              subject: evidence.subject,
              output_is_non_canonical: transition.outputIsNonCanonical,
            },
          } as never,
          ResolvedOutputStep05DatumSchema as never,
        ),
      }
    : transition.kind === "finalize"
      ? {
          stage: "step04Datum" as const,
          stepIndex: 4,
          datum: Data.to(
            {
              fraud_prover: signer.paymentKeyHash,
              data: resolvedOutputDatumState(
                evidence.subject,
                decodeMidgardLedgerOutputCommitment(
                  Buffer.from(evidence.resolved.descriptorCborHex, "hex"),
                ).itemCommitment.toString("hex"),
                transition.openDatum,
              ),
            } as never,
            ResolvedOutputStep04DatumDatumSchema as never,
          ),
        }
      : {
          stage: "reconstructing" as const,
          stepIndex: 3,
          datum: Data.to(
            {
              fraud_prover: signer.paymentKeyHash,
              data: {
                subject: evidence.subject,
                descriptor_cbor: evidence.resolved.descriptorCborHex,
                control: resolvedOutputScanControlData(transition.nextControl),
              },
            } as never,
            ResolvedOutputStep04DatumSchema as never,
          ),
        };
  const nextDatum = next.datum;
  const nextStepIndex = next.stepIndex;
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
          {
            input_index: inputIndex,
            output_index: outputIndex,
            action: transition.action,
          },
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
    nextStage: next.stage,
    action: transition.kind,
  };
};
