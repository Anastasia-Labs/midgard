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
  type Script,
  type UTxO,
} from "@lucid-evolution/lucid";

import {
  linearFaultStepLabel,
  requireLinearFaultReferenceScript,
} from "./linear-fault-family.js";
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
import {
  type FraudProofPreSubmitBoundary,
  reachFraudProofPreSubmitBoundary,
  workflowReferenceScript,
} from "./workflow/transaction-boundary.js";

const CancelSchema = faultProofStepRedeemerSchema(Data.Any());
type Cancel = Data.Static<typeof CancelSchema>;
const Cancel = asDataType<Cancel>(CancelSchema);

export const submitLinearFaultCancel = async ({
  lucid,
  family,
  steps,
  computationThread,
  categoryId,
  signer,
  threadOutRef,
  referenceScriptUtxo,
  witnessReferenceScripts,
  preSubmitBoundary,
  awaitConfirmation = true,
}: {
  readonly lucid: LucidEvolution;
  readonly family: string;
  readonly steps: readonly {
    readonly spendingScript: Script;
    readonly spendingScriptHash: string;
    readonly spendingScriptAddress: string;
  }[];
  readonly computationThread: {
    readonly policyId: string;
    readonly mintingScript: Script;
  };
  readonly categoryId: string;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly referenceScriptUtxo: UTxO;
  readonly witnessReferenceScripts: FaultProofWitnessReferenceScripts;
  readonly preSubmitBoundary?: FraudProofPreSubmitBoundary;
  readonly awaitConfirmation?: boolean;
}) => {
  const threadUtxo = await fetchUtxoByOutRef({
    lucid,
    outRef: parseOutRef(threadOutRef, "--thread-out-ref"),
    label: `${family} thread`,
  });
  const stepIndex = steps.findIndex(
    ({ spendingScriptAddress }) => spendingScriptAddress === threadUtxo.address,
  );
  const step = steps[stepIndex];
  if (stepIndex < 0 || step === undefined) {
    throw new Error(`${family}: ${outRefLabel(threadUtxo)} is not at a step`);
  }
  const label = linearFaultStepLabel(family, stepIndex);
  const token = requireComputationThreadToken({
    utxo: threadUtxo,
    computationThreadPolicyId: computationThread.policyId,
    categoryId,
    categoryLabel: family,
  });
  if (threadUtxo.datum == null) throw new Error(`${family}: no datum`);
  const datum = Data.from(
    threadUtxo.datum,
    FraudProofComputationThreadStepDatum,
  );
  if (datum.fraud_prover !== signer.paymentKeyHash) {
    throw new Error(`${family}: signer does not own thread`);
  }
  signer.selectWallet(lucid);
  const stepReference = requireLinearFaultReferenceScript({
    utxo: referenceScriptUtxo,
    expectedScriptHash: step.spendingScriptHash,
    family,
    stepIndex,
  });
  const burn = witnessMintingPolicyCarriage({
    script: computationThread.mintingScript,
    referenceUtxo: witnessReferenceScripts.computationThreadMint,
    label: `${label} burn`,
  });
  let inputIndex: bigint | undefined;
  let mintRedeemerIndex: bigint | undefined;
  const spendRedeemer = ((ctx) => {
    requireOwnSpendPurpose(ctx, threadUtxo, label);
    inputIndex = requireInputIndex(ctx, threadUtxo, label);
    mintRedeemerIndex = requireMintRedeemerIndex(
      ctx,
      computationThread.policyId,
      label,
    );
    return Data.to(
      {
        Cancel: {
          input_index: inputIndex,
          computation_thread_mint_redeemer_index: mintRedeemerIndex,
        },
      },
      Cancel,
    );
  }) satisfies BuildTxWithRedeemer;
  const burnRedeemer = ((ctx) => {
    requireOwnMintPurpose(ctx, computationThread.policyId, label);
    return Data.to(
      { BurnForCancellation: { burning_token_asset_name: token.assetName } },
      FraudProofComputationThreadRedeemer,
    );
  }) satisfies BuildTxWithRedeemer;
  const walletUtxos = await lucid.wallet().getUtxos();
  const usableWalletUtxos = walletUtxos;
  const unsigned = await burn
    .attach(
      lucid
        .newTx()
        .collectFrom([selectFeeInput(usableWalletUtxos)])
        .collectFrom([threadUtxo], spendRedeemer)
        .readFrom([stepReference, ...burn.referenceInputs])
        .mintAssets({ [token.unit]: -1n }, burnRedeemer)
        .addSignerKey(signer.paymentKeyHash),
    )
    .complete({ localUPLCEval: true });
  if (inputIndex === undefined || mintRedeemerIndex === undefined) {
    throw new Error(`${label}: unresolved cancellation layout`);
  }
  const signed = await unsigned.sign.withWallet().complete();
  const expectedTxHash = await reachFraudProofPreSubmitBoundary({
    signed,
    referenceScripts: [
      workflowReferenceScript({
        role: `${label}-cancel`,
        utxo: stepReference,
        expectedScript: step.spendingScript,
      }),
      workflowReferenceScript({
        role: `${label}-cancel-burn`,
        utxo: witnessReferenceScripts.computationThreadMint,
        expectedScript: computationThread.mintingScript,
      }),
    ],
    boundary: preSubmitBoundary,
  });
  const txHash = await signed.submit();
  if (txHash !== expectedTxHash) throw new Error(`${label}: hash mismatch`);
  if (awaitConfirmation)
    await lucid.awaitTx(txHash, DEFAULT_CONFIRMATION_POLL_MS);
  return {
    txHash,
    cancelledStepIndex: stepIndex,
    fraudulentHeaderHash: token.fraudulentHeaderHash,
    computationThreadUnit: token.unit,
    reclaimedLovelace: threadUtxo.assets.lovelace ?? 0n,
  };
};
