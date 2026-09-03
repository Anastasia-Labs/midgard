/** Explicit prover cancellation from either min-fee step. */
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
  MIN_FEE_CATEGORY_LABEL,
  type MinFeeContracts,
} from "./min-fee-contracts.js";
import {
  minFeeStepLabel,
  minFeeSubmitError,
  requireMinFeeReferenceScript,
} from "./min-fee-submit-common.js";
import {
  DEFAULT_CONFIRMATION_POLL_MS,
  fetchUtxoByOutRef,
  outRefLabel,
  parseOutRef,
  type ResolvedProverSigner,
} from "./runtime.js";
import {
  requireComputationThreadToken,
  selectFeeInput,
} from "./submit-step-01.js";
import {
  type FaultProofWitnessReferenceScripts,
  witnessMintingPolicyCarriage,
} from "./witness-reference-scripts.js";

const CancelRedeemerSchema = faultProofStepRedeemerSchema(Data.Any());
type CancelRedeemer = Data.Static<typeof CancelRedeemerSchema>;
const CancelRedeemer = CancelRedeemerSchema as unknown as CancelRedeemer;

const locateStepIndex = ({
  threadUtxo,
  contracts,
}: {
  readonly threadUtxo: UTxO;
  readonly contracts: MinFeeContracts;
}): 0 | 1 => {
  if (threadUtxo.address === contracts.steps[0].spendingScriptAddress) return 0;
  if (threadUtxo.address === contracts.steps[1].spendingScriptAddress) return 1;
  throw minFeeSubmitError(
    `thread UTxO ${outRefLabel(threadUtxo)} is not at either min-fee step.`,
  );
};

export type SubmitMinFeeCancelResult = {
  readonly txHash: string;
  readonly cancelledStepIndex: 0 | 1;
  readonly computationThreadUnit: string;
  readonly fraudulentHeaderHash: string;
  readonly reclaimedLovelace: bigint;
  readonly awaitedConfirmation: boolean;
};

export const submitMinFeeCancel = async ({
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
  readonly contracts: MinFeeContracts;
  readonly categoryId: string;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  /** Mandatory: min-fee validators are reference-script-only. */
  readonly referenceScriptUtxo: UTxO;
  /** Required published witness reference scripts for this transaction. */
  readonly witnessReferenceScripts?: FaultProofWitnessReferenceScripts;
  readonly awaitConfirmation?: boolean;
}): Promise<SubmitMinFeeCancelResult> => {
  const threadUtxo = await fetchUtxoByOutRef({
    lucid,
    outRef: parseOutRef(threadOutRef, "--thread-out-ref"),
    label: `${MIN_FEE_CATEGORY_LABEL} computation thread`,
  });
  const stepIndex = locateStepIndex({ threadUtxo, contracts });
  const stepLabel = minFeeStepLabel(stepIndex);
  const reference = requireMinFeeReferenceScript({
    utxo: referenceScriptUtxo,
    expectedScriptHash: contracts.steps[stepIndex].spendingScriptHash,
    stepIndex,
  });
  const threadToken = requireComputationThreadToken({
    utxo: threadUtxo,
    computationThreadPolicyId: contracts.computationThread.policyId,
    categoryId,
    categoryLabel: MIN_FEE_CATEGORY_LABEL,
  });
  if (threadUtxo.datum == null) {
    throw minFeeSubmitError(`${stepLabel} thread has no inline datum.`);
  }
  const datum = Data.from(
    threadUtxo.datum,
    FraudProofComputationThreadStepDatum,
  );
  if (datum.fraud_prover !== signer.paymentKeyHash) {
    throw minFeeSubmitError(
      `${stepLabel} names ${datum.fraud_prover}; only that prover may cancel.`,
    );
  }

  signer.selectWallet(lucid);
  const feeInput = selectFeeInput(await lucid.wallet().getUtxos());
  const spendRedeemer = ((ctx) => {
    requireOwnSpendPurpose(ctx, threadUtxo, `${stepLabel} cancel`);
    return Data.to(
      {
        Cancel: {
          input_index: requireInputIndex(
            ctx,
            threadUtxo,
            `${stepLabel} cancel`,
          ),
          computation_thread_mint_redeemer_index: requireMintRedeemerIndex(
            ctx,
            contracts.computationThread.policyId,
            `${stepLabel} cancellation burn`,
          ),
        },
      },
      CancelRedeemer,
    );
  }) satisfies BuildTxWithRedeemer;
  const burnRedeemer = ((ctx) => {
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
  const computationThreadMintCarriage = witnessMintingPolicyCarriage({
    script: contracts.computationThread.mintingScript,
    referenceUtxo: witnessReferenceScripts?.computationThreadMint,
    label: `${stepLabel} cancellation computation-thread mint`,
  });
  const base = lucid
    .newTx()
    .collectFrom([feeInput])
    .collectFrom([threadUtxo], spendRedeemer)
    .readFrom([reference, ...computationThreadMintCarriage.referenceInputs])
    .mintAssets({ [threadToken.unit]: -1n }, burnRedeemer)
    .addSignerKey(signer.paymentKeyHash);
  const tx = computationThreadMintCarriage.attach(base);
  const unsigned = await tx.complete({ localUPLCEval: true });
  const signed = await unsigned.sign.withWallet().complete();
  const txHash = await signed.submit();
  if (awaitConfirmation) {
    await lucid.awaitTx(txHash, DEFAULT_CONFIRMATION_POLL_MS);
  }
  return {
    txHash,
    cancelledStepIndex: stepIndex,
    computationThreadUnit: threadToken.unit,
    fraudulentHeaderHash: threadToken.fraudulentHeaderHash,
    reclaimedLovelace: threadUtxo.assets.lovelace ?? 0n,
    awaitedConfirmation: awaitConfirmation,
  };
};
