/** Explicit prover cancellation from either family step. */
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
  CANONICAL_DECODABILITY_CATEGORY_LABEL,
  type CanonicalDecodabilityContractsV1,
} from "./contracts-v1.js";
import {
  canonicalDecodabilityStepLabelV1,
  canonicalDecodabilitySubmitError,
  requireCanonicalDecodabilityReferenceScriptV1,
} from "./submit-common-v1.js";

const CancelSpendRedeemerSchema = faultProofStepRedeemerSchema(Data.Any());
type CancelSpendRedeemer = Data.Static<typeof CancelSpendRedeemerSchema>;
const CancelSpendRedeemer =
  CancelSpendRedeemerSchema as unknown as CancelSpendRedeemer;

export type SubmitCanonicalDecodabilityCancelResult = {
  readonly txHash: string;
  readonly walletSource: string;
  readonly proverAddress: string;
  readonly fraudProver: string;
  readonly threadOutRef: string;
  readonly cancelledStepIndex: 0 | 1;
  readonly fraudulentHeaderHash: string;
  readonly computationThreadUnit: string;
  readonly reclaimedLovelace: bigint;
  readonly inputIndex: number;
  readonly computationThreadMintRedeemerIndex: number;
  readonly awaitedConfirmation: boolean;
};

const locateStepIndex = ({
  threadUtxo,
  contracts,
}: {
  readonly threadUtxo: UTxO;
  readonly contracts: CanonicalDecodabilityContractsV1;
}): 0 | 1 => {
  for (const stepIndex of [0, 1] as const) {
    if (
      threadUtxo.address === contracts.steps[stepIndex].spendingScriptAddress
    ) {
      return stepIndex;
    }
  }
  throw canonicalDecodabilitySubmitError(
    `thread UTxO ${outRefLabel(threadUtxo)} is not locked at either family step.`,
  );
};

export const submitCanonicalDecodabilityCancel = async ({
  lucid,
  contracts,
  categoryId,
  signer,
  threadOutRef,
  referenceScriptUtxo,
  awaitConfirmation = true,
}: {
  readonly lucid: LucidEvolution;
  readonly contracts: CanonicalDecodabilityContractsV1;
  readonly categoryId: string;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  /** Published reference script for the located step; mandatory. */
  readonly referenceScriptUtxo: UTxO;
  readonly awaitConfirmation?: boolean;
}): Promise<SubmitCanonicalDecodabilityCancelResult> => {
  const threadUtxo = await fetchUtxoByOutRef({
    lucid,
    outRef: parseOutRef(threadOutRef, "--thread-out-ref"),
    label: `${CANONICAL_DECODABILITY_CATEGORY_LABEL} computation-thread UTxO`,
  });
  const stepIndex = locateStepIndex({ threadUtxo, contracts });
  const stepLabel = canonicalDecodabilityStepLabelV1(stepIndex);
  const threadToken = requireComputationThreadToken({
    utxo: threadUtxo,
    computationThreadPolicyId: contracts.computationThread.policyId,
    categoryId,
    categoryLabel: CANONICAL_DECODABILITY_CATEGORY_LABEL,
  });
  if (threadUtxo.datum == null) {
    throw canonicalDecodabilitySubmitError(
      `thread UTxO ${outRefLabel(threadUtxo)} at ${stepLabel} has no inline datum.`,
    );
  }
  const datum = Data.from(
    threadUtxo.datum,
    FraudProofComputationThreadStepDatum,
  );
  if (datum.fraud_prover !== signer.paymentKeyHash) {
    throw canonicalDecodabilitySubmitError(
      `${stepLabel} thread names fraud prover ${datum.fraud_prover}, not signing wallet ${signer.paymentKeyHash}.`,
    );
  }
  const stepReference = requireCanonicalDecodabilityReferenceScriptV1({
    utxo: referenceScriptUtxo,
    expectedScriptHash: contracts.steps[stepIndex].spendingScriptHash,
    stepIndex,
  });

  signer.selectWallet(lucid);
  const feeInput = selectFeeInput(await lucid.wallet().getUtxos());
  let inputIndex: bigint | undefined;
  let mintRedeemerIndex: bigint | undefined;
  const spendRedeemer = ((ctx) => {
    requireOwnSpendPurpose(ctx, threadUtxo, `${stepLabel} cancel`);
    inputIndex = requireInputIndex(ctx, threadUtxo, `${stepLabel} cancel`);
    mintRedeemerIndex = requireMintRedeemerIndex(
      ctx,
      contracts.computationThread.policyId,
      `${stepLabel} cancellation burn`,
    );
    return Data.to(
      {
        Cancel: {
          input_index: inputIndex,
          computation_thread_mint_redeemer_index: mintRedeemerIndex,
        },
      },
      CancelSpendRedeemer,
    );
  }) satisfies BuildTxWithRedeemer;
  const threadBurnRedeemer = ((ctx) => {
    requireOwnMintPurpose(
      ctx,
      contracts.computationThread.policyId,
      `${stepLabel} cancellation burn`,
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

  const unsigned = await lucid
    .newTx()
    .collectFrom([feeInput])
    .collectFrom([threadUtxo], spendRedeemer)
    .readFrom([stepReference])
    .mintAssets({ [threadToken.unit]: -1n }, threadBurnRedeemer)
    .addSignerKey(signer.paymentKeyHash)
    .attach.MintingPolicy(contracts.computationThread.mintingScript)
    .complete({ localUPLCEval: true });
  if (inputIndex === undefined || mintRedeemerIndex === undefined) {
    throw canonicalDecodabilitySubmitError(
      "BuildTxWithRedeemer did not resolve cancellation layout.",
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
