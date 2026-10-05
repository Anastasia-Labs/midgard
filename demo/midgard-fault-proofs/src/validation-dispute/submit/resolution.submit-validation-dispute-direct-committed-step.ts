import {
  type CommittedValidationStepEvidence,
  ValidationResolutionDatum,
} from "@al-ft/midgard-sdk";
import {
  Data,
  type LucidEvolution,
  type Network,
  type UTxO,
} from "@lucid-evolution/lucid";

import {
  DEFAULT_CONFIRMATION_POLL_MS,
  fetchUtxoByOutRef,
  outRefLabel,
  parseOutRef,
  type ResolvedProverSigner,
  resolveValidationTraceDisputeDeploymentContracts,
} from "../../runtime.js";
import {
  requireComputationThreadToken,
  selectFeeInput,
} from "../../step-support.js";
import { witnessSpendingValidatorCarriage } from "../../witness-reference-scripts.js";
import { type FraudProofPreSubmitBoundary } from "../../workflow/transaction-boundary.js";
import { type ContinueLayout, makeGameHandoffRedeemer } from "./redeemers.js";
import { validationResolverIndex } from "./resolution.submit-validation-dispute-enter-resolution.js";
import { requireDisputeDatum } from "./reveal.js";
import {
  requireL1ProofEnvelope,
  threadAssets,
} from "./transaction-material.js";
import {
  reachOptionalPreSubmitBoundary,
  requireValidityRange,
  type ValidationDisputeValidityRange,
  validationDisputeValidityRange,
} from "./validity.js";

export type SubmitValidationDisputeDirectCommittedStepResult = {
  readonly txHash: string;
  readonly threadOutRef: string;
  readonly nextThreadOutRef: string;
  readonly inputIndex: number;
  readonly outputIndex: number;
  readonly awaitedConfirmation: boolean;
};

export const submitValidationDisputeDirectCommittedStep = async ({
  lucid,
  blueprint,
  deploymentInfo,
  network,
  signer,
  threadOutRef,
  gameReferenceScriptUtxo,
  validityRange = validationDisputeValidityRange(Date.now()),
  awaitConfirmation = true,
  preSubmitBoundary,
  evidence,
}: {
  readonly evidence: CommittedValidationStepEvidence;
  readonly lucid: LucidEvolution;
  readonly blueprint: unknown;
  readonly deploymentInfo: unknown;
  readonly network: Network;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  /** The mandatory published V1 validation-trace game script. */
  readonly gameReferenceScriptUtxo?: UTxO;
  readonly validityRange?: ValidationDisputeValidityRange;
  readonly awaitConfirmation?: boolean;
  /** Optional Q51 pre-submit boundary (workflow ruling R5). */
  readonly preSubmitBoundary?: FraudProofPreSubmitBoundary;
}): Promise<SubmitValidationDisputeDirectCommittedStepResult> => {
  if (["Signatures", "Cek", "Terminal"].includes(evidence.pre_state.phase)) {
    throw new Error(
      `Direct committed-step resolution is unavailable for validation phase ${evidence.pre_state.phase}`,
    );
  }
  const range = requireValidityRange(validityRange);
  const { validationTraceDisputeCategory, contracts } =
    await resolveValidationTraceDisputeDeploymentContracts({
      blueprint,
      deploymentInfo,
      network,
    });
  const threadUtxo = await fetchUtxoByOutRef({
    lucid,
    outRef: parseOutRef(threadOutRef, "--thread-out-ref"),
    label: "validation-dispute game UTxO",
  });
  const gameContract = contracts.validationTraceDispute.game;
  if (threadUtxo.address !== gameContract.spendingScriptAddress) {
    throw new Error(
      `Thread UTxO ${outRefLabel(threadUtxo)} is not locked at the validation-dispute game validator`,
    );
  }
  const token = requireComputationThreadToken({
    utxo: threadUtxo,
    computationThreadPolicyId: contracts.computationThread.policyId,
    categoryId: validationTraceDisputeCategory.categoryId,
    categoryLabel: "validation-trace-dispute",
  });
  const inputDatum = requireDisputeDatum(threadUtxo);
  if (inputDatum.fraud_prover !== signer.paymentKeyHash) {
    throw new Error(
      `Validation-dispute resolution handoff requires fraud prover ${inputDatum.fraud_prover}, got ${signer.paymentKeyHash}`,
    );
  }
  const resolverIndex = validationResolverIndex(evidence.pre_state.phase);
  const outputAddress =
    contracts.validationTraceDispute.prepareResolvers[resolverIndex]!
      .spendingScriptAddress;
  const outputDatum = Data.to(
    {
      fraud_prover: inputDatum.fraud_prover,
      data: {
        version: 1n,
        pre_state: evidence.pre_state,
        operator_successor_hash: evidence.post_proof.state_hash,
        challenger_successor_hash: evidence.challenger_successor_hash,
      },
    },
    ValidationResolutionDatum,
  );
  let layout: ContinueLayout | undefined;
  signer.selectWallet(lucid);
  const feeInput = selectFeeInput(await lucid.wallet().getUtxos());
  const gameScriptCarriage = witnessSpendingValidatorCarriage({
    script: gameContract.spendingScript,
    referenceUtxo: gameReferenceScriptUtxo,
    label: "validation-dispute game validator",
  });
  const base = lucid
    .newTx()
    .collectFrom([feeInput])
    .collectFrom(
      [threadUtxo],
      makeGameHandoffRedeemer({
        threadUtxo,
        outputAddress,
        outputDatum,
        threadUnit: token.unit,
        destination: "resolution",
        committedStep: { resolverIndex, evidence },
        onLayout: (resolvedLayout) => {
          layout = resolvedLayout;
        },
      }),
    )
    .pay.ToContract(
      outputAddress,
      { kind: "inline", value: outputDatum },
      threadAssets(threadUtxo, token.unit),
    )
    .validFrom(range.validFrom)
    .validTo(range.validTo)
    .addSignerKey(signer.paymentKeyHash);
  const withReferenceScript =
    gameScriptCarriage.referenceInputs.length === 0
      ? base
      : base.readFrom([...gameScriptCarriage.referenceInputs]);
  const tx = gameScriptCarriage.attach(withReferenceScript);
  const unsigned = await tx.complete({ localUPLCEval: true });
  if (layout === undefined) {
    throw new Error(
      "BuildTxWithRedeemer did not resolve validation-dispute resolution handoff layout",
    );
  }
  const signed = await unsigned.sign.withWallet().complete();
  requireL1ProofEnvelope(
    signed.toCBOR(),
    "Validation-dispute resolution handoff",
  );
  await reachOptionalPreSubmitBoundary({
    signed,
    boundary: preSubmitBoundary,
    referenceScriptCandidates: [
      {
        role: "validation-dispute game validator",
        utxo: gameReferenceScriptUtxo,
      },
    ],
  });
  const txHash = await signed.submit();
  if (awaitConfirmation) {
    await lucid.awaitTx(txHash, DEFAULT_CONFIRMATION_POLL_MS);
  }
  return {
    txHash,
    threadOutRef,
    nextThreadOutRef: `${txHash}#${layout.outputIndex.toString()}`,
    inputIndex: Number(layout.inputIndex),
    outputIndex: Number(layout.outputIndex),
    awaitedConfirmation: awaitConfirmation,
  };
};
