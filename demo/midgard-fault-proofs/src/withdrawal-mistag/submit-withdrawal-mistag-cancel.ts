/** Prover-signed cancellation at any one of the five live step addresses. */
import {
  faultProofStepRedeemerSchema,
  FraudProofComputationThreadRedeemer,
  FraudProofComputationThreadStepDatum,
  requireInputIndex,
  requireMintRedeemerIndex,
  requireOwnMintPurpose,
  requireOwnSpendPurpose,
  WITHDRAWAL_MISTAG_FRAUD_CATEGORY_ID,
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
  WITHDRAWAL_MISTAG_CATEGORY_LABEL,
  type WithdrawalMistagContracts,
} from "./contracts.js";
import {
  requireWithdrawalMistagReferenceScript,
  withdrawalMistagError,
  withdrawalMistagStepLabel,
} from "./submit-common.js";

const CancelSchema = faultProofStepRedeemerSchema(Data.Any());
type StepIndex = 0 | 1 | 2 | 3 | 4;

const locate = (
  utxo: UTxO,
  contracts: WithdrawalMistagContracts,
): StepIndex => {
  for (const index of [0, 1, 2, 3, 4] as const) {
    if (utxo.address === contracts.steps[index].spendingScriptAddress)
      return index;
  }
  throw withdrawalMistagError(
    `${outRefLabel(utxo)} is not at a live withdrawal-mistag step`,
  );
};

export const submitWithdrawalMistagCancel = async ({
  lucid,
  contracts,
  signer,
  threadOutRef,
  referenceScriptUtxo,
  witnessReferenceScripts,
  awaitConfirmation = true,
}: {
  readonly lucid: LucidEvolution;
  readonly contracts: WithdrawalMistagContracts;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly referenceScriptUtxo: UTxO;
  /** Required published witness reference scripts for this transaction. */
  readonly witnessReferenceScripts?: FaultProofWitnessReferenceScripts;
  readonly awaitConfirmation?: boolean;
}) => {
  const threadUtxo = await fetchUtxoByOutRef({
    lucid,
    outRef: parseOutRef(threadOutRef, "--thread-out-ref"),
    label: "withdrawal-mistag thread",
  });
  const stepIndex = locate(threadUtxo, contracts);
  const threadToken = requireComputationThreadToken({
    utxo: threadUtxo,
    computationThreadPolicyId: contracts.computationThread.policyId,
    categoryId: WITHDRAWAL_MISTAG_FRAUD_CATEGORY_ID,
    categoryLabel: WITHDRAWAL_MISTAG_CATEGORY_LABEL,
  });
  if (threadUtxo.datum == null)
    throw withdrawalMistagError("thread has no datum");
  const datum = Data.from(
    threadUtxo.datum,
    FraudProofComputationThreadStepDatum,
  );
  if (datum.fraud_prover !== signer.paymentKeyHash) {
    throw withdrawalMistagError("only the thread's fraud prover can cancel");
  }

  let inputIndex: bigint | undefined;
  let mintIndex: bigint | undefined;
  const spendRedeemer = ((ctx) => {
    requireOwnSpendPurpose(
      ctx,
      threadUtxo,
      `${withdrawalMistagStepLabel(stepIndex)} cancel`,
    );
    inputIndex = requireInputIndex(ctx, threadUtxo, "withdrawal-mistag cancel");
    mintIndex = requireMintRedeemerIndex(
      ctx,
      contracts.computationThread.policyId,
      "withdrawal-mistag cancel burn",
    );
    return Data.to(
      {
        Cancel: {
          input_index: inputIndex,
          computation_thread_mint_redeemer_index: mintIndex,
        },
      } as never,
      CancelSchema as never,
    );
  }) satisfies BuildTxWithRedeemer;
  const burnRedeemer = ((ctx) => {
    requireOwnMintPurpose(
      ctx,
      contracts.computationThread.policyId,
      "cancel burn",
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
  signer.selectWallet(lucid);
  const feeInput = selectFeeInput(await lucid.wallet().getUtxos());
  const computationThreadMintCarriage = witnessMintingPolicyCarriage({
    script: contracts.computationThread.mintingScript,
    referenceUtxo: witnessReferenceScripts?.computationThreadMint,
    label: "withdrawal-mistag cancel computation-thread mint",
  });
  const referenceInputs = [
    requireWithdrawalMistagReferenceScript({
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
    .mintAssets({ [threadToken.unit]: -1n }, burnRedeemer)
    .addSignerKey(signer.paymentKeyHash);
  const tx = computationThreadMintCarriage.attach(
    base.readFrom(referenceInputs),
  );
  const unsigned = await tx.complete({ localUPLCEval: true });
  if (inputIndex === undefined || mintIndex === undefined) {
    throw withdrawalMistagError(
      "transaction builder did not resolve cancel layout",
    );
  }
  const signed = await unsigned.sign.withWallet().complete();
  const txHash = await signed.submit();
  if (awaitConfirmation)
    await lucid.awaitTx(txHash, DEFAULT_CONFIRMATION_POLL_MS);
  return {
    txHash,
    threadOutRef,
    cancelledStepIndex: stepIndex,
    computationThreadUnit: threadToken.unit,
    inputIndex: Number(inputIndex),
    computationThreadMintRedeemerIndex: Number(mintIndex),
    reclaimedLovelace: threadUtxo.assets.lovelace ?? 0n,
    awaitedConfirmation: awaitConfirmation,
  };
};
