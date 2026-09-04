/**
 * `native-script-decoding` thread cancellation (offchain plan §7.1).
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
  NATIVE_SCRIPT_DECODING_CATEGORY_LABEL,
  type NativeScriptDecodingContracts,
} from "./contracts.js";
import {
  nativeScriptDecodingStepLabel,
  nativeScriptDecodingSubmitError,
  requireNativeScriptDecodingReferenceScript,
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

export type SubmitNativeScriptDecodingCancelResult = {
  readonly txHash: string;
  readonly walletSource: string;
  readonly proverAddress: string;
  readonly fraudProver: string;
  readonly threadOutRef: string;
  /** The step the thread was cancelled out of. */
  readonly cancelledStepIndex: 0 | 1 | 2 | 3 | 4 | 5;
  readonly fraudulentHeaderHash: string;
  readonly computationThreadUnit: string;
  /** The thread UTxO's lovelace, returned to the prover wallet as change. */
  readonly reclaimedLovelace: bigint;
  readonly inputIndex: number;
  readonly computationThreadMintRedeemerIndex: number;
  readonly awaitedConfirmation: boolean;
};

/** Finds which of the family's six validator addresses holds the thread UTxO. */
const locateStepIndex = ({
  threadUtxo,
  contracts,
}: {
  readonly threadUtxo: UTxO;
  readonly contracts: NativeScriptDecodingContracts;
}): 0 | 1 | 2 | 3 | 4 | 5 => {
  for (const stepIndex of [0, 1, 2, 3, 4, 5] as const) {
    if (
      threadUtxo.address === contracts.steps[stepIndex].spendingScriptAddress
    ) {
      return stepIndex;
    }
  }
  throw nativeScriptDecodingSubmitError(
    `thread UTxO ${outRefLabel(threadUtxo)} is not locked at any of the family's six validator addresses — a finished or already-cancelled thread has nothing to cancel.`,
  );
};

export const submitNativeScriptDecodingCancel = async ({
  lucid,
  contracts,
  categoryId,
  signer,
  threadOutRef,
  referenceScriptUtxo,
  witnessReferenceScripts,
  awaitConfirmation = true,
}: {
  readonly lucid: LucidEvolution;
  readonly contracts: NativeScriptDecodingContracts;
  readonly categoryId: string;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  /** Q3: the located step's mandatory published reference script. */
  readonly referenceScriptUtxo: UTxO;
  /** Required published witness reference scripts for this transaction. */
  readonly witnessReferenceScripts?: FaultProofWitnessReferenceScripts;
  readonly awaitConfirmation?: boolean;
}): Promise<SubmitNativeScriptDecodingCancelResult> => {
  const threadUtxo = await fetchUtxoByOutRef({
    lucid,
    outRef: parseOutRef(threadOutRef, "--thread-out-ref"),
    label: `${NATIVE_SCRIPT_DECODING_CATEGORY_LABEL} computation-thread UTxO`,
  });
  const stepIndex = locateStepIndex({ threadUtxo, contracts });
  const stepLabel = nativeScriptDecodingStepLabel(stepIndex);
  const threadToken = requireComputationThreadToken({
    utxo: threadUtxo,
    computationThreadPolicyId: contracts.computationThread.policyId,
    categoryId,
    categoryLabel: NATIVE_SCRIPT_DECODING_CATEGORY_LABEL,
  });

  // The validator releases the thread to its named fraud prover alone; any
  // other signer would build an unexecutable transaction.
  if (threadUtxo.datum == null) {
    throw nativeScriptDecodingSubmitError(
      `thread UTxO ${outRefLabel(threadUtxo)} at ${stepLabel} has no inline datum.`,
    );
  }
  const datum = Data.from(
    threadUtxo.datum,
    FraudProofComputationThreadStepDatum,
  );
  if (datum.fraud_prover !== signer.paymentKeyHash) {
    throw nativeScriptDecodingSubmitError(
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

  const computationThreadBurnCarriage = witnessMintingPolicyCarriage({
    script: contracts.computationThread.mintingScript,
    referenceUtxo: witnessReferenceScripts?.computationThreadMint,
    label: `${stepLabel} cancel computation-thread burn`,
  });
  const referenceInputs = [
    requireNativeScriptDecodingReferenceScript({
      utxo: referenceScriptUtxo,
      expectedScriptHash: contracts.steps[stepIndex].spendingScriptHash,
      stepIndex,
    }),
    ...computationThreadBurnCarriage.referenceInputs,
  ];
  const base = lucid
    .newTx()
    .collectFrom([feeInput])
    .collectFrom([threadUtxo], spendRedeemer)
    .mintAssets({ [threadToken.unit]: -1n }, threadBurnRedeemer)
    .addSignerKey(signer.paymentKeyHash)
    .readFrom(referenceInputs);
  const tx = computationThreadBurnCarriage.attach(base);

  const unsigned = await tx.complete({ localUPLCEval: true });
  if (inputIndex === undefined || mintRedeemerIndex === undefined) {
    throw nativeScriptDecodingSubmitError(
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
