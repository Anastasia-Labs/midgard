/**
 * `cross-block-duplicate-event` thread cancellation (offchain plan §5).
 *
 * `ct.Cancel` is available at every step, so this submitter works wherever
 * the thread currently sits: it locates the step by address, burns the
 * computation-thread NFT through the mint policy's `BurnForCancellation`
 * arm, and reclaims the thread's min-ADA to the prover wallet. Only the
 * fraud prover named in the step datum can cancel — the validator demands
 * that signature, and this submitter refuses any other signer up front.
 *
 * Cancellation is always an explicit operator/prover decision: the proving
 * core never cancels on its own (§4.3 — an unexpected abort surfaces as a
 * stalled outcome instead).
 */
import { asDataType } from "@al-ft/midgard-core/lucid-data";
import {
  CROSS_BLOCK_DUPLICATE_EVENT_FRAUD_CATEGORY_ID,
  faultProofStepRedeemerSchema,
  FraudProofComputationThreadRedeemer,
  FraudProofComputationThreadStepDatum,
  requireInputIndex,
  requireMintRedeemerIndex,
  requireOwnMintPurpose,
  requireOwnSpendPurpose,
} from "@al-ft/midgard-sdk";
import {
  type BuildTxWithRedeemer,
  Data,
  type LucidEvolution,
  type UTxO,
} from "@lucid-evolution/lucid";

import {
  DEFAULT_CONFIRMATION_POLL_MS,
  fetchUtxoByOutRef,
  outRefLabel,
  parseOutRef,
  type ResolvedProverSigner,
} from "../runtime.js";
import {
  requireComputationThreadToken,
  selectFeeInput,
} from "../submit-step-01.js";
import {
  type FaultProofWitnessReferenceScripts,
  witnessMintingPolicyCarriage,
} from "../witness-reference-scripts.js";
import {
  CROSS_BLOCK_DUPLICATE_EVENT_CATEGORY_LABEL,
  type CrossBlockDuplicateEventContracts,
} from "./contracts.js";
import {
  crossBlockDuplicateEventStepLabel,
  crossBlockDuplicateEventSubmitError,
  requireCrossBlockDuplicateEventReferenceScript,
} from "./submit-common.js";

/**
 * Every step's spend redeemer shares the `Cancel` head; the `Continue` arm
 * is never encoded here, so its argument schema is irrelevant.
 */
const CancelSpendRedeemerSchema = faultProofStepRedeemerSchema(Data.Any());
type CancelSpendRedeemer = Data.Static<typeof CancelSpendRedeemerSchema>;
const CancelSpendRedeemer = asDataType<CancelSpendRedeemer>(
  CancelSpendRedeemerSchema,
);

export type SubmitCrossBlockDuplicateEventCancelResult = {
  readonly txHash: string;
  readonly walletSource: string;
  readonly proverAddress: string;
  readonly fraudProver: string;
  readonly threadOutRef: string;
  /** The step the thread was cancelled out of. */
  readonly cancelledStepIndex: 0 | 1;
  readonly fraudulentHeaderHash: string;
  readonly computationThreadUnit: string;
  /** The thread UTxO's lovelace, returned to the prover wallet as change. */
  readonly reclaimedLovelace: bigint;
  readonly inputIndex: number;
  readonly computationThreadMintRedeemerIndex: number;
  readonly awaitedConfirmation: boolean;
};

/** Finds which of the family's two step addresses holds the thread UTxO. */
const locateStepIndex = ({
  threadUtxo,
  contracts,
}: {
  readonly threadUtxo: UTxO;
  readonly contracts: CrossBlockDuplicateEventContracts;
}): 0 | 1 => {
  for (const stepIndex of [0, 1] as const) {
    if (
      threadUtxo.address === contracts.steps[stepIndex].spendingScriptAddress
    ) {
      return stepIndex;
    }
  }
  throw crossBlockDuplicateEventSubmitError(
    `thread UTxO ${outRefLabel(threadUtxo)} is not locked at any of the family's two step addresses — a finished or already-cancelled thread has nothing to cancel.`,
  );
};

export const submitCrossBlockDuplicateEventCancel = async ({
  lucid,
  contracts,
  signer,
  threadOutRef,
  referenceScriptUtxo,
  witnessReferenceScripts,
  awaitConfirmation = true,
}: {
  readonly lucid: LucidEvolution;
  readonly contracts: CrossBlockDuplicateEventContracts;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  /** Mandatory published reference script for the step being cancelled. */
  readonly referenceScriptUtxo: UTxO;
  readonly witnessReferenceScripts: FaultProofWitnessReferenceScripts;
  readonly awaitConfirmation?: boolean;
}): Promise<SubmitCrossBlockDuplicateEventCancelResult> => {
  const threadUtxo = await fetchUtxoByOutRef({
    lucid,
    outRef: parseOutRef(threadOutRef, "--thread-out-ref"),
    label: `${CROSS_BLOCK_DUPLICATE_EVENT_CATEGORY_LABEL} computation-thread UTxO`,
  });
  const stepIndex = locateStepIndex({ threadUtxo, contracts });
  const stepLabel = crossBlockDuplicateEventStepLabel(stepIndex);
  const threadToken = requireComputationThreadToken({
    utxo: threadUtxo,
    computationThreadPolicyId: contracts.computationThread.policyId,
    categoryId: CROSS_BLOCK_DUPLICATE_EVENT_FRAUD_CATEGORY_ID,
    categoryLabel: CROSS_BLOCK_DUPLICATE_EVENT_CATEGORY_LABEL,
  });

  // The validator releases the thread to its named fraud prover alone; any
  // other signer would build an unexecutable transaction.
  if (threadUtxo.datum == null) {
    throw crossBlockDuplicateEventSubmitError(
      `thread UTxO ${outRefLabel(threadUtxo)} at ${stepLabel} has no inline datum.`,
    );
  }
  const datum = Data.from(
    threadUtxo.datum,
    FraudProofComputationThreadStepDatum,
  );
  if (datum.fraud_prover !== signer.paymentKeyHash) {
    throw crossBlockDuplicateEventSubmitError(
      `${stepLabel} thread names fraud prover ${datum.fraud_prover}, not the signing wallet ${signer.paymentKeyHash} — only the prover can cancel.`,
    );
  }

  signer.selectWallet(lucid);
  const feeInput = selectFeeInput(await lucid.wallet().getUtxos());
  let inputIndex: bigint | undefined;
  let mintRedeemerIndex: bigint | undefined;

  const spendRedeemer = ((ctx) => {
    requireOwnSpendPurpose(ctx, threadUtxo, `${stepLabel} cancel`);
    const resolvedInputIndex = requireInputIndex(
      ctx,
      threadUtxo,
      `${stepLabel} cancel`,
    );
    const resolvedMintRedeemerIndex = requireMintRedeemerIndex(
      ctx,
      contracts.computationThread.policyId,
      `${stepLabel} cancel computation-thread burn`,
    );
    inputIndex = resolvedInputIndex;
    mintRedeemerIndex = resolvedMintRedeemerIndex;
    return Data.to(
      {
        Cancel: {
          input_index: resolvedInputIndex,
          computation_thread_mint_redeemer_index: resolvedMintRedeemerIndex,
        },
      },
      CancelSpendRedeemer,
    );
  }) satisfies BuildTxWithRedeemer;

  const threadBurnRedeemer = ((ctx) => {
    requireOwnMintPurpose(
      ctx,
      contracts.computationThread.policyId,
      `${stepLabel} cancel computation-thread burn`,
    );
    return Data.to(
      {
        BurnForCancellation: {
          burning_token_asset_name: threadToken.assetName,
        },
      },
      FraudProofComputationThreadRedeemer,
    );
  }) satisfies BuildTxWithRedeemer;

  const computationThreadMintCarriage = witnessMintingPolicyCarriage({
    script: contracts.computationThread.mintingScript,
    referenceUtxo: witnessReferenceScripts?.computationThreadMint,
    label: `${stepLabel} cancel computation-thread mint`,
  });
  const referenceInputs = [
    requireCrossBlockDuplicateEventReferenceScript({
      utxo: referenceScriptUtxo,
      contracts,
      stepIndex,
    }),
    ...computationThreadMintCarriage.referenceInputs,
  ];
  const base = lucid
    .newTx()
    .collectFrom([feeInput])
    .collectFrom([threadUtxo], spendRedeemer)
    .mintAssets({ [threadToken.unit]: -1n }, threadBurnRedeemer)
    .addSignerKey(signer.paymentKeyHash)
    .readFrom(referenceInputs);
  const tx = computationThreadMintCarriage.attach(base);

  const unsigned = await tx.complete({ localUPLCEval: true });
  if (inputIndex === undefined || mintRedeemerIndex === undefined) {
    throw crossBlockDuplicateEventSubmitError(
      "BuildTxWithRedeemer did not resolve the cancel layout.",
    );
  }
  const signed = await unsigned.sign.withWallet().complete();
  const txHash = await signed.submit();
  if (awaitConfirmation) {
    await lucid.awaitTx(txHash, DEFAULT_CONFIRMATION_POLL_MS);
  }

  return {
    txHash,
    walletSource: signer.source,
    proverAddress: signer.address,
    fraudProver: signer.paymentKeyHash,
    threadOutRef,
    cancelledStepIndex: stepIndex,
    fraudulentHeaderHash: threadToken.fraudulentHeaderHash,
    computationThreadUnit: threadToken.unit,
    reclaimedLovelace: threadUtxo.assets.lovelace ?? 0n,
    inputIndex: Number(inputIndex),
    computationThreadMintRedeemerIndex: Number(mintRedeemerIndex),
    awaitedConfirmation: awaitConfirmation,
  };
};
