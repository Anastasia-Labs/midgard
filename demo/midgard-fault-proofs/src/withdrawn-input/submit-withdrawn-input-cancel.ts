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
import {
  WITHDRAWN_INPUT_CATEGORY_LABEL,
  type WithdrawnInputContracts,
} from "./contracts-v1.js";
import {
  requireWithdrawnInputReferenceScript,
  withdrawnInputStepLabel,
  withdrawnInputSubmitError,
} from "./submit-common-v1.js";

const CancelSpendRedeemerSchema = faultProofStepRedeemerSchema(Data.Any());
type CancelSpendRedeemer = Data.Static<typeof CancelSpendRedeemerSchema>;
const CancelSpendRedeemer =
  CancelSpendRedeemerSchema as unknown as CancelSpendRedeemer;

export type SubmitWithdrawnInputCancelResult = {
  readonly txHash: string;
  readonly cancelledStepIndex: 0 | 1 | 2;
  readonly computationThreadUnit: string;
  readonly reclaimedLovelace: bigint;
};

export const submitWithdrawnInputCancel = async ({
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
  readonly contracts: WithdrawnInputContracts;
  readonly categoryId: string;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly referenceScriptUtxo: UTxO;
  readonly witnessReferenceScripts: FaultProofWitnessReferenceScripts;
  readonly awaitConfirmation?: boolean;
}): Promise<SubmitWithdrawnInputCancelResult> => {
  const threadUtxo = await fetchUtxoByOutRef({
    lucid,
    outRef: parseOutRef(threadOutRef, "--thread-out-ref"),
    label: `${WITHDRAWN_INPUT_CATEGORY_LABEL} thread`,
  });
  const stepIndex = ([0, 1, 2] as const).find(
    (index) =>
      threadUtxo.address === contracts.steps[index].spendingScriptAddress,
  );
  if (stepIndex === undefined) {
    throw withdrawnInputSubmitError(
      `thread UTxO ${outRefLabel(threadUtxo)} is not at a family step.`,
    );
  }
  const stepReference = requireWithdrawnInputReferenceScript({
    utxo: referenceScriptUtxo,
    contracts,
    stepIndex,
  });
  const threadToken = requireComputationThreadToken({
    utxo: threadUtxo,
    computationThreadPolicyId: contracts.computationThread.policyId,
    categoryId,
    categoryLabel: WITHDRAWN_INPUT_CATEGORY_LABEL,
  });
  if (threadUtxo.datum == null) {
    throw withdrawnInputSubmitError("thread has no inline datum.");
  }
  const datum = Data.from(
    threadUtxo.datum,
    FraudProofComputationThreadStepDatum,
  );
  if (datum.fraud_prover !== signer.paymentKeyHash) {
    throw withdrawnInputSubmitError(
      `${withdrawnInputStepLabel(stepIndex)} belongs to ${datum.fraud_prover}, not ${signer.paymentKeyHash}.`,
    );
  }
  signer.selectWallet(lucid);
  const feeInput = selectFeeInput(await lucid.wallet().getUtxos());
  let inputIndex: bigint | undefined;
  let mintIndex: bigint | undefined;
  const spendRedeemer = ((ctx) => {
    requireOwnSpendPurpose(
      ctx,
      threadUtxo,
      `${WITHDRAWN_INPUT_CATEGORY_LABEL} cancel`,
    );
    inputIndex = requireInputIndex(
      ctx,
      threadUtxo,
      `${WITHDRAWN_INPUT_CATEGORY_LABEL} cancel`,
    );
    mintIndex = requireMintRedeemerIndex(
      ctx,
      contracts.computationThread.policyId,
      `${WITHDRAWN_INPUT_CATEGORY_LABEL} cancel burn`,
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
      `${WITHDRAWN_INPUT_CATEGORY_LABEL} cancel burn`,
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
    label: "withdrawn-input cancel computation-thread mint",
  });
  const referenceInputs = [
    stepReference,
    ...computationThreadMintCarriage.referenceInputs,
  ];
  const base = lucid
    .newTx()
    .collectFrom([feeInput])
    .collectFrom([threadUtxo], spendRedeemer)
    .readFrom(referenceInputs)
    .mintAssets({ [threadToken.unit]: -1n }, burnRedeemer)
    .addSignerKey(signer.paymentKeyHash);
  const tx = computationThreadMintCarriage.attach(base);
  const unsigned = await tx.complete({ localUPLCEval: true });
  if (inputIndex === undefined || mintIndex === undefined) {
    throw withdrawnInputSubmitError("cancel layout was not resolved.");
  }
  const signed = await unsigned.sign.withWallet().complete();
  const txHash = await signed.submit();
  if (awaitConfirmation) {
    await lucid.awaitTx(txHash, DEFAULT_CONFIRMATION_POLL_MS);
  }
  return {
    txHash,
    cancelledStepIndex: stepIndex,
    computationThreadUnit: threadToken.unit,
    reclaimedLovelace: threadUtxo.assets.lovelace ?? 0n,
  };
};
