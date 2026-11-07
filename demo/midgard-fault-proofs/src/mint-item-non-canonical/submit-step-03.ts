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
import type { MintItemNonCanonicalContracts } from "./contracts.js";
import {
  type MintItemEvidence,
  mintItemScanControlData,
} from "./mint-item-non-canonical.js";
import {
  MintItemScanControlSchema,
  MintItemStep03DatumSchema,
  MintItemStep03RedeemerSchema,
} from "./schemas.js";

export const submitMintItemNonCanonicalStep03 = async ({
  lucid,
  contracts,
  categoryId,
  signer,
  threadOutRef,
  evidence,
  referenceScriptUtxo,
  preSubmitBoundary,
  onTransitionReady,
  awaitConfirmation = true,
}: {
  readonly lucid: LucidEvolution;
  readonly contracts: MintItemNonCanonicalContracts;
  readonly categoryId: string;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly evidence: MintItemEvidence;
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
  readonly onTransitionReady?: (terminal: boolean) => Promise<void>;
  readonly awaitConfirmation?: boolean;
}) => {
  const stepIndex = 2;
  const { threadUtxo, threadToken } = await requireLinearFaultThreadUtxo({
    lucid,
    contracts,
    categoryId,
    family: "mint-item-non-canonical",
    stepIndex,
    threadOutRef,
  });
  const state = requireLinearFaultStepState<{
    subject: unknown;
    item_index: bigint;
    item_length: bigint;
    item_hash: string;
    chunk_hashes: readonly string[];
    control: { readonly cursor: bigint; readonly stage: bigint };
    outcome: bigint;
  }>({
    threadUtxo,
    signer,
    schema: MintItemStep03DatumSchema as never,
    family: "mint-item-non-canonical",
    stepIndex,
  });
  if (
    state.item_index !== BigInt(evidence.itemIndex) ||
    state.item_length !== BigInt(evidence.itemLength) ||
    state.item_hash !== evidence.itemHash ||
    state.chunk_hashes.join(":") !== evidence.chunkHashes.join(":") ||
    state.outcome !== 0n
  ) {
    throw new Error(
      "mint-item-non-canonical: scan checkpoint identity changed",
    );
  }
  const stateControl = Data.to(
    state.control as never,
    MintItemScanControlSchema as never,
  );
  const controlIndex = evidence.scanControls.findIndex(
    (control) =>
      Data.to(
        mintItemScanControlData(control) as never,
        MintItemScanControlSchema as never,
      ) === stateControl,
  );
  if (controlIndex < 0)
    throw new Error(
      "mint-item-non-canonical: scan checkpoint is not in authenticated trace",
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
      "mint-item-non-canonical: canonical scan trace ended without terminal control",
    );
  const selectedControl = nextControl ?? evidence.scanControls[controlIndex]!;
  const stepReference = requireLinearFaultReferenceScript({
    utxo: referenceScriptUtxo,
    expectedScriptHash: contracts.steps[2].spendingScriptHash,
    family: "mint-item-non-canonical",
    stepIndex,
  });
  const nextDatum = Data.to(
    {
      fraud_prover: signer.paymentKeyHash,
      data: {
        subject: evidence.subject,
        item_index: BigInt(evidence.itemIndex),
        item_length: BigInt(evidence.itemLength),
        item_hash: evidence.itemHash,
        chunk_hashes: evidence.chunkHashes,
        control: mintItemScanControlData(selectedControl),
        outcome,
      },
    } as never,
    MintItemStep03DatumSchema as never,
  );
  const nextStepIndex = outcome === 0n ? 2 : 3;
  await onTransitionReady?.(outcome !== 0n);
  const outputMatches = computationThreadOutputPredicate({
    address: contracts.steps[nextStepIndex].spendingScriptAddress,
    datum: nextDatum,
    unit: threadToken.unit,
  });
  let outputIndex: bigint | undefined;
  const redeemer = ((ctx) => {
    requireOwnSpendPurpose(ctx, threadUtxo, "mint-item-non-canonical step-03");
    const inputIndex = requireInputIndex(
      ctx,
      threadUtxo,
      "mint-item-non-canonical",
    );
    outputIndex = requireUniqueOutputIndex(
      ctx.outputs,
      outputMatches,
      "mint-item-non-canonical step-03 output",
    );
    const item = Buffer.from(evidence.itemHex, "hex");
    const chunkStart = Math.floor(Number(state.control.cursor) / 4_095) * 4_095;
    const window = item
      .subarray(chunkStart, chunkStart + 8_190)
      .toString("hex");
    return Data.to(
      {
        Continue: [
          { input_index: inputIndex, output_index: outputIndex, window },
        ],
      } as never,
      MintItemStep03RedeemerSchema as never,
    );
  }) satisfies BuildTxWithRedeemer;
  const txHash = await submitLinearFaultContinue({
    lucid,
    signerPaymentKeyHash: signer.paymentKeyHash,
    threadUtxo,
    threadUnit: threadToken.unit,
    stepReference,
    stepScript: contracts.steps[2].spendingScript,
    stepRole: "mint-item-non-canonical step-03",
    nextAddress: contracts.steps[nextStepIndex].spendingScriptAddress,
    nextDatum,
    redeemer,
    carriageUtxos: [],
    preSubmitBoundary,
    awaitConfirmation,
  });
  if (outputIndex === undefined)
    throw new Error("mint-item-non-canonical: step-03 layout unresolved");
  return {
    txHash,
    nextThreadOutRef: `${txHash}#${outputIndex.toString()}`,
    terminal: outcome !== 0n,
  };
};
