import {
  type MidgardValidationTraceProof,
  revealMidgardValidationChallengerMidpoint,
  revealMidgardValidationOperatorMidpoint,
} from "@al-ft/midgard-core";
import {
  validationDisputeCoreFromData,
  validationDisputeDataFromCore,
  ValidationDisputeDatum,
  type ValidationDisputeDatum as ValidationDisputeDatumData,
  validationTraceProofCoreFromData,
  validationTraceProofDataFromCore,
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
import {
  type ContinueLayout,
  makeRevealRedeemer,
  type SubmitValidationDisputeRevealResult,
} from "./redeemers.js";
import {
  requireL1ProofEnvelope,
  threadAssets,
} from "./transaction-material.js";
import {
  inclusiveValidityUpperBound,
  ledgerPresentedValidationDisputeValidityRange,
  reachOptionalPreSubmitBoundary,
  type ValidationDisputeValidityRange,
  validationDisputeValidityRange,
} from "./validity.js";

export const requireDisputeDatum = (
  threadUtxo: UTxO,
): ValidationDisputeDatumData & {
  readonly data: NonNullable<ValidationDisputeDatumData["data"]>;
} => {
  if (threadUtxo.datum == null) {
    throw new Error(
      `Validation-dispute thread UTxO ${outRefLabel(threadUtxo)} is missing datum`,
    );
  }
  const datum = Data.from(threadUtxo.datum, ValidationDisputeDatum);
  if (datum.data === null) {
    throw new Error("Validation-dispute reveal requires initialized state");
  }
  return datum as ValidationDisputeDatumData & {
    readonly data: NonNullable<ValidationDisputeDatumData["data"]>;
  };
};

export const submitValidationDisputeReveal = async ({
  lucid,
  blueprint,
  deploymentInfo,
  network,
  signer,
  threadOutRef,
  role,
  proof,
  gameReferenceScriptUtxo,
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
  readonly role: "operator" | "challenger";
  readonly proof: MidgardValidationTraceProof;
  /** The mandatory published V1 validation-trace game script. */
  readonly gameReferenceScriptUtxo?: UTxO;
  readonly validityRange?: ValidationDisputeValidityRange;
  readonly awaitConfirmation?: boolean;
  /** Optional Q51 pre-submit boundary (workflow ruling R5). */
  readonly preSubmitBoundary?: FraudProofPreSubmitBoundary;
}): Promise<SubmitValidationDisputeRevealResult> => {
  const range = ledgerPresentedValidationDisputeValidityRange(
    lucid,
    validityRange,
  );
  const { validationTraceDisputeCategory, contracts } =
    await resolveValidationTraceDisputeDeploymentContracts({
      blueprint,
      deploymentInfo,
      network,
    });
  const threadUtxo = await fetchUtxoByOutRef({
    lucid,
    outRef: parseOutRef(threadOutRef, "--thread-out-ref"),
    label: "validation-dispute computation-thread UTxO",
  });
  const disputeContract = contracts.validationTraceDispute.game;
  if (threadUtxo.address !== disputeContract.spendingScriptAddress) {
    throw new Error(
      `Thread UTxO ${outRefLabel(threadUtxo)} is not locked at the validation-dispute validator`,
    );
  }
  const token = requireComputationThreadToken({
    utxo: threadUtxo,
    computationThreadPolicyId: contracts.computationThread.policyId,
    categoryId: validationTraceDisputeCategory.categoryId,
    categoryLabel: "validation-trace-dispute",
  });
  const inputDatum = requireDisputeDatum(threadUtxo);
  const expectedSigner =
    role === "operator"
      ? inputDatum.data.operator_vkey
      : inputDatum.fraud_prover;
  if (signer.paymentKeyHash !== expectedSigner) {
    throw new Error(
      `Validation-dispute ${role} reveal requires signer ${expectedSigner}, got ${signer.paymentKeyHash}`,
    );
  }
  const inputDispute = validationDisputeCoreFromData(inputDatum.data.dispute);
  const currentTimeUpper = inclusiveValidityUpperBound(range);
  const nextDispute =
    role === "operator"
      ? revealMidgardValidationOperatorMidpoint({
          dispute: inputDispute,
          proof,
          currentTime: currentTimeUpper,
        })
      : revealMidgardValidationChallengerMidpoint({
          dispute: inputDispute,
          proof,
          currentTime: currentTimeUpper,
        });
  const outputDatum = Data.to(
    {
      fraud_prover: inputDatum.fraud_prover,
      data: {
        challenged_header_hash: inputDatum.data.challenged_header_hash,
        operator_vkey: inputDatum.data.operator_vkey,
        dispute: validationDisputeDataFromCore(nextDispute),
      },
    },
    ValidationDisputeDatum,
  );
  const proofData = validationTraceProofDataFromCore(proof);
  // Round-trip before construction so non-canonical or out-of-range proof
  // fields fail before wallet selection and never reach balancing.
  validationTraceProofCoreFromData(proofData);
  let layout: ContinueLayout | undefined;
  signer.selectWallet(lucid);
  const feeInput = selectFeeInput(await lucid.wallet().getUtxos());
  const gameScriptCarriage = witnessSpendingValidatorCarriage({
    script: disputeContract.spendingScript,
    referenceUtxo: gameReferenceScriptUtxo,
    label: "validation-dispute game validator",
  });
  const base = lucid
    .newTx()
    .collectFrom([feeInput])
    .collectFrom(
      [threadUtxo],
      makeRevealRedeemer({
        threadUtxo,
        outputAddress: disputeContract.spendingScriptAddress,
        outputDatum,
        threadUnit: token.unit,
        role,
        proof: proofData,
        onLayout: (resolvedLayout) => {
          layout = resolvedLayout;
        },
      }),
    )
    .pay.ToContract(
      disputeContract.spendingScriptAddress,
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
      `BuildTxWithRedeemer did not resolve validation-dispute ${role} reveal layout`,
    );
  }
  const signed = await unsigned.sign.withWallet().complete();
  requireL1ProofEnvelope(signed.toCBOR(), `Validation-dispute ${role} reveal`);
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
    role,
    inputIndex: Number(layout.inputIndex),
    outputIndex: Number(layout.outputIndex),
    responseDeadline: nextDispute.responseDeadline,
    awaitedConfirmation: awaitConfirmation,
  };
};
