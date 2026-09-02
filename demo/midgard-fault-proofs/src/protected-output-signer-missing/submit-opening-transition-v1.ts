import {
  type FieldOpeningV1,
  requireInputIndex,
  requireOwnSpendPurpose,
  requireUniqueOutputIndex,
} from "@al-ft/midgard-sdk";
import {
  type BuildTxWithRedeemer,
  Data,
  type Data as PlutusData,
  type LucidEvolution,
  type UTxO,
} from "@lucid-evolution/lucid";

import {
  requireLinearFaultReferenceScriptV1,
  requireLinearFaultThreadUtxoV1,
} from "../linear-fault-family-v1.js";
import { submitLinearFaultContinueV1 } from "../linear-fault-submit-v1.js";
import type { ResolvedProverSigner } from "../runtime.js";
import { computationThreadOutputPredicate } from "../tx-layout.js";
import type { FraudProofPreSubmitBoundaryV1 } from "../workflow/transaction-boundary-v1.js";
import type { ProtectedOutputSignerMissingContractsV1 } from "./contracts-v1.js";

export const submitProtectedOutputSignerOpeningTransitionV1 = async ({
  lucid,
  contracts,
  categoryId,
  signer,
  threadOutRef,
  stepIndex,
  nextStepIndex,
  nextDatum,
  opening,
  checkpointCbor,
  referenceScriptUtxo,
  carriageReferenceInputs,
  redeemerSchema,
  preSubmitBoundary,
  awaitConfirmation = true,
}: {
  readonly lucid: LucidEvolution;
  readonly contracts: ProtectedOutputSignerMissingContractsV1;
  readonly categoryId: string;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly stepIndex: 1 | 2 | 3;
  readonly nextStepIndex: 2 | 3 | 4;
  readonly nextDatum: string;
  readonly opening: FieldOpeningV1;
  readonly checkpointCbor?: string;
  readonly referenceScriptUtxo: UTxO;
  readonly carriageReferenceInputs: readonly UTxO[];
  readonly redeemerSchema: PlutusData;
  readonly preSubmitBoundary?: FraudProofPreSubmitBoundaryV1;
  readonly awaitConfirmation?: boolean;
}) => {
  const { threadUtxo, threadToken } = await requireLinearFaultThreadUtxoV1({
    lucid,
    contracts,
    categoryId,
    family: "protected-output-signer-missing",
    stepIndex,
    threadOutRef,
  });
  signer.selectWallet(lucid);
  const stepReference = requireLinearFaultReferenceScriptV1({
    utxo: referenceScriptUtxo,
    expectedScriptHash: contracts.steps[stepIndex].spendingScriptHash,
    family: "protected-output-signer-missing",
    stepIndex,
  });
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
      `protected-output-signer-missing step-${(stepIndex + 1).toString().padStart(2, "0")}`,
    );
    const inputIndex = requireInputIndex(
      ctx,
      threadUtxo,
      "protected-output-signer-missing",
    );
    outputIndex = requireUniqueOutputIndex(
      ctx.outputs,
      outputMatches,
      "protected-output-signer-missing next output",
    );
    return Data.to(
      {
        Continue: [
          {
            input_index: inputIndex,
            output_index: outputIndex,
            opening,
            ...(checkpointCbor === undefined
              ? {}
              : { checkpoint_cbor: checkpointCbor }),
          },
        ],
      } as never,
      redeemerSchema as never,
    );
  }) satisfies BuildTxWithRedeemer;
  const txHash = await submitLinearFaultContinueV1({
    lucid,
    signerPaymentKeyHash: signer.paymentKeyHash,
    threadUtxo,
    threadUnit: threadToken.unit,
    stepReference,
    stepScript: contracts.steps[stepIndex].spendingScript,
    stepRole: `protected-output-signer-missing step-${(stepIndex + 1).toString().padStart(2, "0")}`,
    nextAddress: contracts.steps[nextStepIndex].spendingScriptAddress,
    nextDatum,
    redeemer,
    carriageUtxos: carriageReferenceInputs.filter(
      (utxo) =>
        utxo.txHash !== stepReference.txHash ||
        utxo.outputIndex !== stepReference.outputIndex,
    ),
    preSubmitBoundary,
    awaitConfirmation,
  });
  if (outputIndex === undefined)
    throw new Error("protected-output-signer-missing: layout unresolved");
  return {
    txHash,
    nextThreadOutRef: `${txHash}#${outputIndex.toString()}`,
  };
};
