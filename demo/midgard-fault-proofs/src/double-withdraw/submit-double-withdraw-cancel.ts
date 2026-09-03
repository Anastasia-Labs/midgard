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
} from "../witness-reference-scripts-v1.js";
import type { DoubleWithdrawContracts } from "./contracts-v1.js";
import {
  doubleWithdrawStepLabel,
  doubleWithdrawSubmitError,
  requireDoubleWithdrawReferenceScript,
} from "./submit-common-v1.js";

const CancelSpendRedeemerSchema = faultProofStepRedeemerSchema(Data.Any());
type CancelSpendRedeemer = Data.Static<typeof CancelSpendRedeemerSchema>;
const CancelSpendRedeemer =
  CancelSpendRedeemerSchema as unknown as CancelSpendRedeemer;

const locateStepIndex = ({
  threadUtxo,
  contracts,
}: {
  readonly threadUtxo: UTxO;
  readonly contracts: DoubleWithdrawContracts;
}): 0 | 1 => {
  if (threadUtxo.address === contracts.steps[0].spendingScriptAddress) return 0;
  if (threadUtxo.address === contracts.steps[1].spendingScriptAddress) return 1;
  throw doubleWithdrawSubmitError(
    `thread UTxO ${outRefLabel(threadUtxo)} is not locked at either family step.`,
  );
};

export type SubmitDoubleWithdrawCancelResult = {
  readonly txHash: string;
  readonly cancelledStepIndex: 0 | 1;
  readonly fraudulentHeaderHash: string;
  readonly computationThreadUnit: string;
  readonly inputIndex: number;
  readonly computationThreadMintRedeemerIndex: number;
  readonly awaitedConfirmation: boolean;
};

export const submitDoubleWithdrawCancel = async ({
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
  readonly contracts: DoubleWithdrawContracts;
  readonly categoryId: string;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly referenceScriptUtxo: UTxO;
  /** Required published witness reference scripts for this transaction. */
  readonly witnessReferenceScripts?: FaultProofWitnessReferenceScripts;
  readonly awaitConfirmation?: boolean;
}): Promise<SubmitDoubleWithdrawCancelResult> => {
  const threadUtxo = await fetchUtxoByOutRef({
    lucid,
    outRef: parseOutRef(threadOutRef, "--thread-out-ref"),
    label: "double-withdraw computation-thread UTxO",
  });
  const stepIndex = locateStepIndex({ threadUtxo, contracts });
  const stepLabel = doubleWithdrawStepLabel(stepIndex);
  const threadToken = requireComputationThreadToken({
    utxo: threadUtxo,
    computationThreadPolicyId: contracts.computationThread.policyId,
    categoryId,
    categoryLabel: "double-withdraw",
  });
  if (threadUtxo.datum == null) {
    throw doubleWithdrawSubmitError(
      `thread UTxO ${outRefLabel(threadUtxo)} has no inline datum.`,
    );
  }
  const datum = Data.from(
    threadUtxo.datum,
    FraudProofComputationThreadStepDatum,
  );
  if (datum.fraud_prover !== signer.paymentKeyHash) {
    throw doubleWithdrawSubmitError(
      `${stepLabel} thread names fraud prover ${datum.fraud_prover}, not ${signer.paymentKeyHash}; only the prover can cancel.`,
    );
  }
  signer.selectWallet(lucid);
  const feeInput = selectFeeInput(await lucid.wallet().getUtxos());
  let inputIndex: bigint | undefined;
  let mintIndex: bigint | undefined;
  const spendRedeemer = ((ctx) => {
    requireOwnSpendPurpose(ctx, threadUtxo, `${stepLabel} cancel`);
    inputIndex = requireInputIndex(ctx, threadUtxo, `${stepLabel} cancel`);
    mintIndex = requireMintRedeemerIndex(
      ctx,
      contracts.computationThread.policyId,
      `${stepLabel} cancel burn`,
    );
    return Data.to(
      {
        Cancel: {
          input_index: inputIndex,
          computation_thread_mint_redeemer_index: mintIndex,
        },
      },
      CancelSpendRedeemer,
    );
  }) satisfies BuildTxWithRedeemer;
  const burnRedeemer = ((ctx) => {
    requireOwnMintPurpose(
      ctx,
      contracts.computationThread.policyId,
      `${stepLabel} cancel burn`,
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
    requireDoubleWithdrawReferenceScript({
      utxo: referenceScriptUtxo,
      expectedScriptHash: contracts.steps[stepIndex].spendingScriptHash,
      stepIndex,
    }),
    ...computationThreadMintCarriage.referenceInputs,
  ];
  const base = lucid
    .newTx()
    .collectFrom([feeInput])
    .collectFrom([threadUtxo], spendRedeemer)
    .mintAssets({ [threadToken.unit]: -1n }, burnRedeemer)
    .addSignerKey(signer.paymentKeyHash);
  const tx = computationThreadMintCarriage.attach(
    base.readFrom(referenceInputs),
  );
  const unsigned = await tx.complete({ localUPLCEval: true });
  if (inputIndex === undefined || mintIndex === undefined) {
    throw doubleWithdrawSubmitError("cancel layout was not resolved.");
  }
  const signed = await unsigned.sign.withWallet().complete();
  const txHash = await signed.submit();
  if (awaitConfirmation)
    await lucid.awaitTx(txHash, DEFAULT_CONFIRMATION_POLL_MS);
  return {
    txHash,
    cancelledStepIndex: stepIndex,
    fraudulentHeaderHash: threadToken.fraudulentHeaderHash,
    computationThreadUnit: threadToken.unit,
    inputIndex: Number(inputIndex),
    computationThreadMintRedeemerIndex: Number(mintIndex),
    awaitedConfirmation: awaitConfirmation,
  };
};
