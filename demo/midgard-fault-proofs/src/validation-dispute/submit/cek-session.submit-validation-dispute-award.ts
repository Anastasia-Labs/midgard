import {
  sharedRedeemerItemReferenceScripts,
  ValidationAwardSpendRedeemer,
  validationTraceDescriptorDataFromCore,
} from "@al-ft/midgard-sdk";
import {
  Data,
  type LucidEvolution,
  type Network,
  type UTxO,
} from "@lucid-evolution/lucid";

import { submitLinearFaultCancel } from "../../linear-fault-cancel.js";
import {
  fetchUtxoByOutRef,
  outRefLabel,
  parseOutRef,
  type ResolvedProverSigner,
  resolveValidationTraceDisputeDeploymentContracts,
} from "../../runtime.js";
import { requireComputationThreadToken } from "../../step-support.js";
import { type FaultProofWitnessReferenceScripts } from "../../witness-reference-scripts.js";
import { type FraudProofPreSubmitBoundary } from "../../workflow/transaction-boundary.js";
import { type SubmitValidationDisputeAwardResult } from "./cek-session.resume-validation-cek-context.js";
import { requireWinningResolutionDatum } from "./resolution.js";
import { submitValidationFinalizationTransaction } from "./semantic-redeemers.js";
import {
  requireValidityRange,
  type ValidationDisputeValidityRange,
  validationDisputeValidityRange,
} from "./validity.js";

export const submitValidationDisputeAward = async ({
  lucid,
  blueprint,
  deploymentInfo,
  network,
  signer,
  threadOutRef,
  awardReferenceScriptUtxo,
  witnessReferenceScripts,
  validityRange = validationDisputeValidityRange(Date.now()),
  awaitConfirmation = true,
  preSubmitBoundary,
}: {
  readonly lucid: LucidEvolution;
  readonly blueprint: unknown;
  readonly deploymentInfo: unknown;
  readonly network: Network;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  /** The mandatory published V1 validation-trace award script. */
  readonly awardReferenceScriptUtxo?: UTxO;
  /** Required published shared minting witnesses for this transaction. */
  readonly witnessReferenceScripts?: FaultProofWitnessReferenceScripts;
  readonly validityRange?: ValidationDisputeValidityRange;
  readonly awaitConfirmation?: boolean;
  /** Optional Q51 pre-submit boundary (workflow ruling R5). */
  readonly preSubmitBoundary?: FraudProofPreSubmitBoundary;
}): Promise<SubmitValidationDisputeAwardResult> => {
  const range = requireValidityRange(validityRange);
  const { validationTraceDisputeCategory, contracts } =
    await resolveValidationTraceDisputeDeploymentContracts({
      blueprint,
      deploymentInfo,
      network,
      requireFraudProofSpend: true,
    });
  const threadUtxo = await fetchUtxoByOutRef({
    lucid,
    outRef: parseOutRef(threadOutRef, "--thread-out-ref"),
    label: "winning validation award UTxO",
  });
  const awardContract = contracts.validationTraceDispute.award;
  if (threadUtxo.address !== awardContract.spendingScriptAddress) {
    throw new Error(
      `Thread UTxO ${outRefLabel(threadUtxo)} is not locked at the validation award validator`,
    );
  }
  const token = requireComputationThreadToken({
    utxo: threadUtxo,
    computationThreadPolicyId: contracts.computationThread.policyId,
    categoryId: validationTraceDisputeCategory.categoryId,
    categoryLabel: "validation-trace-dispute",
  });
  const inputDatum = requireWinningResolutionDatum(threadUtxo);
  if (inputDatum.fraud_prover !== signer.paymentKeyHash) {
    throw new Error(
      `Validation award requires fraud prover ${inputDatum.fraud_prover}, got ${signer.paymentKeyHash}`,
    );
  }
  return await submitValidationFinalizationTransaction({
    lucid,
    blueprint,
    deploymentInfo,
    network,
    contracts,
    signer,
    threadUtxo,
    threadOutRef,
    token,
    spendingScript: awardContract,
    spendingScriptReferenceUtxo: awardReferenceScriptUtxo,
    witnessReferenceScripts,
    spendLabel: "Validation-dispute award",
    encodeSpendRedeemer: (layout) =>
      Data.to(
        {
          Continue: [
            {
              input_index: layout.inputIndex,
              output_index: layout.outputIndex,
              fraud_proof_mint_redeemer_index:
                layout.fraudProofMintRedeemerIndex,
            },
          ],
        },
        ValidationAwardSpendRedeemer,
      ),
    validityRange: range,
    awaitConfirmation,
    preSubmitBoundary,
  });
};

export const validationDisputeDescriptorData =
  validationTraceDescriptorDataFromCore;

/** Cancel an owned prepared semantic thread by its live out-ref. */
export const cancelValidationSemanticResolution = async ({
  lucid,
  blueprint,
  deploymentInfo,
  network,
  signer,
  threadOutRef,
  referenceScriptUtxo,
  witnessReferenceScripts,
  preSubmitBoundary,
  awaitConfirmation = true,
}: {
  readonly lucid: LucidEvolution;
  readonly blueprint: unknown;
  readonly deploymentInfo: unknown;
  readonly network: Network;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly referenceScriptUtxo: UTxO;
  readonly witnessReferenceScripts: FaultProofWitnessReferenceScripts;
  /** Production workflow seam: invoked after local evaluation, before I/O. */
  readonly preSubmitBoundary?: FraudProofPreSubmitBoundary;
  readonly awaitConfirmation?: boolean;
}) => {
  const { validationTraceDisputeCategory, contracts } =
    await resolveValidationTraceDisputeDeploymentContracts({
      blueprint,
      deploymentInfo,
      network,
    });
  return await submitLinearFaultCancel({
    lucid,
    family: "validation semantic resolution",
    steps: [
      ...contracts.validationTraceDispute.semanticResolvers,
      ...sharedRedeemerItemReferenceScripts(
        contracts.validationTraceDispute.scriptSourcesStageOneRedeemerStages,
      ).map(({ validator }) => validator),
      contracts.validationTraceDispute.scriptSourcesStageOneRedeemerStages
        .settlement,
    ],
    computationThread: contracts.computationThread,
    categoryId: validationTraceDisputeCategory.categoryId,
    signer,
    threadOutRef,
    referenceScriptUtxo,
    witnessReferenceScripts,
    preSubmitBoundary,
    awaitConfirmation,
  });
};
