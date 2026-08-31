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

import { type PreparedClaimRegistryMutationV1 } from "./claim-registry-transaction-v1.js";
import {
  linearFaultStepLabelV1,
  requireLinearFaultReferenceScriptV1,
} from "./linear-fault-family-v1.js";
import {
  DEFAULT_CONFIRMATION_POLL_MS,
  fetchUtxoByOutRef,
  outRefLabel,
  parseOutRef,
  type ResolvedProverSigner,
} from "./runtime.js";
import { excludeUtxo } from "./spend-input-witness.js";
import {
  requireComputationThreadToken,
  selectFeeInput,
} from "./submit-step-01.js";
import {
  type FaultProofWitnessReferenceScriptsV1,
  witnessMintingPolicyCarriageV1,
} from "./witness-reference-scripts-v1.js";
import {
  type FraudProofPreSubmitBoundaryV1,
  reachFraudProofPreSubmitBoundaryV1,
  workflowReferenceScriptV1,
} from "./workflow/transaction-boundary-v1.js";

const CancelSchema = faultProofStepRedeemerSchema(Data.Any());
type Cancel = Data.Static<typeof CancelSchema>;
const Cancel = CancelSchema as unknown as Cancel;

export const submitLinearFaultCancelV1 = async ({
  lucid,
  family,
  steps,
  computationThread,
  categoryId,
  signer,
  threadOutRef,
  referenceScriptUtxo,
  witnessReferenceScripts,
  claimRegistryMutation,
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
  readonly witnessReferenceScripts: FaultProofWitnessReferenceScriptsV1;
  readonly claimRegistryMutation: PreparedClaimRegistryMutationV1;
  readonly preSubmitBoundary?: FraudProofPreSubmitBoundaryV1;
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
  const label = linearFaultStepLabelV1(family, stepIndex);
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
  const stepReference = requireLinearFaultReferenceScriptV1({
    utxo: referenceScriptUtxo,
    expectedScriptHash: step.spendingScriptHash,
    family,
    stepIndex,
  });
  const burn = witnessMintingPolicyCarriageV1({
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
  const usableWalletUtxos = claimRegistryMutation.referenceInputs.reduce<
    readonly UTxO[]
  >((utxos, reference) => excludeUtxo(utxos, reference), walletUtxos);
  const unsigned = await burn
    .attach(
      claimRegistryMutation.apply(
        lucid
          .newTx()
          .collectFrom([selectFeeInput(usableWalletUtxos)])
          .collectFrom([threadUtxo], spendRedeemer)
          .readFrom([stepReference, ...burn.referenceInputs])
          .mintAssets({ [token.unit]: -1n }, burnRedeemer)
          .addSignerKey(signer.paymentKeyHash),
      ),
    )
    .complete({ localUPLCEval: true });
  if (inputIndex === undefined || mintRedeemerIndex === undefined) {
    throw new Error(`${label}: unresolved cancellation layout`);
  }
  const signed = await unsigned.sign.withWallet().complete();
  const expectedTxHash = await reachFraudProofPreSubmitBoundaryV1({
    signed,
    referenceScripts: [
      workflowReferenceScriptV1({
        role: `${label}-cancel`,
        utxo: stepReference,
        expectedScript: step.spendingScript,
      }),
      workflowReferenceScriptV1({
        role: `${label}-cancel-burn`,
        utxo: witnessReferenceScripts.computationThreadMint,
        expectedScript: computationThread.mintingScript,
      }),
      workflowReferenceScriptV1({
        role: "claim-registry spending",
        utxo: claimRegistryMutation.referenceScriptUtxo,
        expectedScript: claimRegistryMutation.registryScript,
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
