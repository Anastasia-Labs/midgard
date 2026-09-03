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
} from "../linear-fault-family-v1.js";
import { submitLinearFaultContinue } from "../linear-fault-submit-v1.js";
import type { ResolvedProverSigner } from "../runtime.js";
import { computationThreadOutputPredicate } from "../tx-layout.js";
import type { FraudProofPreSubmitBoundary } from "../workflow/transaction-boundary-v1.js";
import type { TransactionOutputNonCanonicalContracts } from "./contracts-v1.js";
import {
  TransactionOutputScanControlSchema,
  TransactionOutputStep03DatumSchema,
  TransactionOutputStep03RedeemerSchema,
} from "./schemas-v1.js";
import {
  type TransactionOutputEvidence,
  transactionOutputScanControlData,
} from "./transaction-output-non-canonical-v1.js";

export const submitTransactionOutputNonCanonicalStep03 = async ({
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
  readonly nativeTxCompactCbor: string;
  readonly witnessSetCompactCbor: string;
  readonly publishCarriage?: boolean;
  readonly publishedCarriageUtxos?: readonly UTxO[];
  readonly certificateUtxo?: UTxO;
  readonly certificateReferenceScriptUtxo?: UTxO;
  readonly referenceScriptUtxo: UTxO;
  readonly publicationPreSubmitBoundary?: FraudProofPreSubmitBoundary;
  readonly certificatePreSubmitBoundary?: FraudProofPreSubmitBoundary;
  readonly onCarriageReady?: () => Promise<void>;
  readonly preSubmitBoundary?: FraudProofPreSubmitBoundary;
  readonly awaitConfirmation?: boolean;
}) => {
  const stepIndex = 2;
  const { threadUtxo, threadToken } = await requireLinearFaultThreadUtxo({
    lucid,
    contracts,
    categoryId,
    family: "transaction-output-non-canonical",
    stepIndex,
    threadOutRef,
  });
  const state = requireLinearFaultStepState<{
    subject: unknown;
    output_index: bigint;
    item_length: bigint;
    item_hash: string;
    chunk_hashes: readonly string[];
    control: { readonly cursor: bigint; readonly stage: bigint };
    outcome: bigint;
  }>({
    threadUtxo,
    signer,
    schema: TransactionOutputStep03DatumSchema as never,
    family: "transaction-output-non-canonical",
    stepIndex,
  });
  if (
    state.output_index !== BigInt(evidence.itemIndex) ||
    state.item_length !== BigInt(evidence.itemLength) ||
    state.item_hash !== evidence.itemHash ||
    state.chunk_hashes.join(":") !== evidence.chunkHashes.join(":") ||
    state.outcome !== 0n
  ) {
    throw new Error(
      "transaction-output-non-canonical: scan checkpoint identity changed",
    );
  }
  const stateControl = Data.to(
    state.control as never,
    TransactionOutputScanControlSchema as never,
  );
  const controlIndex = evidence.scanControls.findIndex(
    (control) =>
      Data.to(
        transactionOutputScanControlData(control) as never,
        TransactionOutputScanControlSchema as never,
      ) === stateControl,
  );
  if (controlIndex < 0)
    throw new Error(
      "transaction-output-non-canonical: scan checkpoint is not in authenticated trace",
    );
  const nextControl = evidence.scanControls[controlIndex + 1];
  const outcome =
    nextControl === undefined
      ? evidence.canonical
        ? -1n
        : 2n
      : controlIndex + 1 === evidence.scanControls.length - 1 &&
          evidence.canonical
        ? 1n
        : 0n;
  if (outcome < 0n)
    throw new Error(
      "transaction-output-non-canonical: canonical scan trace ended without terminal control",
    );
  const selectedControl = nextControl ?? evidence.scanControls[controlIndex]!;
  const stepReference = requireLinearFaultReferenceScript({
    utxo: referenceScriptUtxo,
    expectedScriptHash: contracts.steps[2].spendingScriptHash,
    family: "transaction-output-non-canonical",
    stepIndex,
  });
  const nextDatum = Data.to(
    {
      fraud_prover: signer.paymentKeyHash,
      data: {
        subject: evidence.subject,
        output_index: BigInt(evidence.itemIndex),
        item_length: BigInt(evidence.itemLength),
        item_hash: evidence.itemHash,
        chunk_hashes: evidence.chunkHashes,
        control: transactionOutputScanControlData(selectedControl),
        outcome,
      },
    } as never,
    TransactionOutputStep03DatumSchema as never,
  );
  const nextStepIndex = outcome === 0n ? 2 : 3;
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
      "transaction-output-non-canonical step-03",
    );
    const inputIndex = requireInputIndex(
      ctx,
      threadUtxo,
      "transaction-output-non-canonical",
    );
    outputIndex = requireUniqueOutputIndex(
      ctx.outputs,
      outputMatches,
      "transaction-output-non-canonical step-03 output",
    );
    const item = Buffer.from(evidence.itemHex, "hex");
    const chunkStart = Math.floor(Number(state.control.cursor) / 4_095) * 4_095;
    const window = item
      .subarray(
        chunkStart,
        chunkStart + (state.control.stage <= 4n ? 8_190 : 4_095),
      )
      .toString("hex");
    return Data.to(
      {
        Continue: [
          { input_index: inputIndex, output_index: outputIndex, window },
        ],
      } as never,
      TransactionOutputStep03RedeemerSchema as never,
    );
  }) satisfies BuildTxWithRedeemer;
  const txHash = await submitLinearFaultContinue({
    lucid,
    signerPaymentKeyHash: signer.paymentKeyHash,
    threadUtxo,
    threadUnit: threadToken.unit,
    stepReference,
    stepScript: contracts.steps[2].spendingScript,
    stepRole: "transaction-output-non-canonical step-03",
    nextAddress: contracts.steps[nextStepIndex].spendingScriptAddress,
    nextDatum,
    redeemer,
    carriageUtxos: [],
    preSubmitBoundary,
    awaitConfirmation,
  });
  if (outputIndex === undefined)
    throw new Error(
      "transaction-output-non-canonical: step-03 layout unresolved",
    );
  return {
    txHash,
    nextThreadOutRef: `${txHash}#${outputIndex.toString()}`,
    terminal: outcome !== 0n,
  };
};
