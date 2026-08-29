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
  type FaultProofWitnessReferenceScriptsV1,
  witnessMintingPolicyCarriageV1,
} from "../witness-reference-scripts-v1.js";
import {
  L2_TX_MISTAG_CATEGORY_LABEL,
  type L2TxMistagContractsV1,
} from "./contracts-v1.js";
import {
  l2TxMistagStepLabelV1,
  l2TxMistagSubmitError,
  requireL2TxMistagReferenceScriptV1,
} from "./submit-common-v1.js";

const CancelSpendRedeemerSchema = faultProofStepRedeemerSchema(Data.Any());
type CancelSpendRedeemer = Data.Static<typeof CancelSpendRedeemerSchema>;
const CancelSpendRedeemer =
  CancelSpendRedeemerSchema as unknown as CancelSpendRedeemer;

export type SubmitL2TxMistagCancelResult = {
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

const locateStepIndex = (
  threadUtxo: UTxO,
  contracts: L2TxMistagContractsV1,
): 0 | 1 => {
  for (const stepIndex of [0, 1] as const) {
    if (
      threadUtxo.address === contracts.steps[stepIndex].spendingScriptAddress
    ) {
      return stepIndex;
    }
  }
  throw l2TxMistagSubmitError(
    `thread UTxO ${outRefLabel(threadUtxo)} is not locked at either family step.`,
  );
};

export const submitL2TxMistagCancel = async ({
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
  readonly contracts: L2TxMistagContractsV1;
  readonly categoryId: string;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  /** Mandatory reference script for the located step. */
  readonly referenceScriptUtxo: UTxO;
  readonly witnessReferenceScripts: FaultProofWitnessReferenceScriptsV1;
  readonly awaitConfirmation?: boolean;
}): Promise<SubmitL2TxMistagCancelResult> => {
  const threadUtxo = await fetchUtxoByOutRef({
    lucid,
    outRef: parseOutRef(threadOutRef, "--thread-out-ref"),
    label: `${L2_TX_MISTAG_CATEGORY_LABEL} computation-thread UTxO`,
  });
  const stepIndex = locateStepIndex(threadUtxo, contracts);
  const stepLabel = l2TxMistagStepLabelV1(stepIndex);
  const reference = requireL2TxMistagReferenceScriptV1({
    utxo: referenceScriptUtxo,
    expectedScriptHash: contracts.steps[stepIndex].spendingScriptHash,
    stepIndex,
  });
  const threadToken = requireComputationThreadToken({
    utxo: threadUtxo,
    computationThreadPolicyId: contracts.computationThread.policyId,
    categoryId,
    categoryLabel: L2_TX_MISTAG_CATEGORY_LABEL,
  });
  if (threadUtxo.datum == null) {
    throw l2TxMistagSubmitError("thread has no inline step datum.");
  }
  const datum = Data.from(
    threadUtxo.datum,
    FraudProofComputationThreadStepDatum,
  );
  if (datum.fraud_prover !== signer.paymentKeyHash) {
    throw l2TxMistagSubmitError(
      `${stepLabel} belongs to fraud prover ${datum.fraud_prover}, not the signing wallet.`,
    );
  }

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
      `${stepLabel} computation-thread burn`,
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
  const burnRedeemer = ((ctx) => {
    requireOwnMintPurpose(
      ctx,
      contracts.computationThread.policyId,
      `${stepLabel} computation-thread burn`,
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

  const computationThreadMintCarriage = witnessMintingPolicyCarriageV1({
    script: contracts.computationThread.mintingScript,
    referenceUtxo: witnessReferenceScripts?.computationThreadMint,
    label: `${stepLabel} cancel computation-thread mint`,
  });
  const referenceInputs = [
    reference,
    ...computationThreadMintCarriage.referenceInputs,
  ];
  const base = lucid
    .newTx()
    .collectFrom([feeInput])
    .collectFrom([threadUtxo], spendRedeemer)
    .readFrom(referenceInputs)
    .mintAssets({ [threadToken.unit]: -1n }, burnRedeemer)
    .addSignerKey(signer.paymentKeyHash);
  const unsigned = await computationThreadMintCarriage
    .attach(base)
    .complete({ localUPLCEval: true });
  if (inputIndex === undefined || mintRedeemerIndex === undefined) {
    throw l2TxMistagSubmitError("cancel layout was not resolved.");
  }
  const txHash = await (await unsigned.sign.withWallet().complete()).submit();
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
