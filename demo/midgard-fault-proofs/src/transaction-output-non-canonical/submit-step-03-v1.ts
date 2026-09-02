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
import type { TransactionOutputNonCanonicalContractsV1 } from "./contracts-v1.js";
import {
  TransactionOutputScanControlV1Schema,
  TransactionOutputStep03DatumV1Schema,
  TransactionOutputStep03RedeemerV1Schema,
} from "./schemas-v1.js";
import {
  type TransactionOutputEvidenceV1,
  transactionOutputScanControlDataV1,
} from "./transaction-output-non-canonical-v1.js";

export const submitTransactionOutputNonCanonicalStep03V1 = async ({
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
  readonly contracts: TransactionOutputNonCanonicalContractsV1;
  readonly categoryId: string;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly evidence: TransactionOutputEvidenceV1;
  readonly nativeTxCompactCbor: string;
  readonly witnessSetCompactCbor: string;
  readonly publishCarriage?: boolean;
  readonly publishedCarriageUtxos?: readonly UTxO[];
  readonly certificateUtxo?: UTxO;
  readonly certificateReferenceScriptUtxo?: UTxO;
  readonly referenceScriptUtxo: UTxO;
  readonly publicationPreSubmitBoundary?: FraudProofPreSubmitBoundaryV1;
  readonly certificatePreSubmitBoundary?: FraudProofPreSubmitBoundaryV1;
  readonly onCarriageReady?: () => Promise<void>;
  readonly preSubmitBoundary?: FraudProofPreSubmitBoundaryV1;
  readonly awaitConfirmation?: boolean;
}) => {
  const stepIndex = 2;
  const { threadUtxo, threadToken } = await requireLinearFaultThreadUtxoV1({
    lucid,
    contracts,
    categoryId,
    family: "transaction-output-non-canonical",
    stepIndex,
    threadOutRef,
  });
  const state = requireLinearFaultStepStateV1<{
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
    schema: TransactionOutputStep03DatumV1Schema as never,
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
    TransactionOutputScanControlV1Schema as never,
  );
  const controlIndex = evidence.scanControls.findIndex(
    (control) =>
      Data.to(
        transactionOutputScanControlDataV1(control) as never,
        TransactionOutputScanControlV1Schema as never,
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
  const stepReference = requireLinearFaultReferenceScriptV1({
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
        control: transactionOutputScanControlDataV1(selectedControl),
        outcome,
      },
    } as never,
    TransactionOutputStep03DatumV1Schema as never,
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
      TransactionOutputStep03RedeemerV1Schema as never,
    );
  }) satisfies BuildTxWithRedeemer;
  const txHash = await submitLinearFaultContinueV1({
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
