import {
  PendingValidationClaimDatum,
  type PendingValidationClaimDatum as PendingValidationClaimDatumData,
  validationDisputeDataFromCore,
  ValidationDisputeDatum,
  validationTraceDescriptorCoreFromData,
  WinningValidationResolutionDatum,
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
import { committedValidationClaimEndpointsAndSourceAreValid } from ".././claim-endpoints.js";
import { openValidationDisputeAfterSourceVerification } from "./open.js";
import { type ContinueLayout, makeVerifySourceRedeemer } from "./redeemers.js";
import {
  requireL1ProofEnvelope,
  threadAssets,
} from "./transaction-material.js";
import {
  ledgerPresentedValidationDisputeValidityRange,
  reachOptionalPreSubmitBoundary,
  type ValidationDisputeValidityRange,
  validationDisputeValidityRange,
} from "./validity.js";

const requirePendingClaimDatum = (
  threadUtxo: UTxO,
): PendingValidationClaimDatumData & {
  readonly data: NonNullable<PendingValidationClaimDatumData["data"]>;
} => {
  if (threadUtxo.datum == null) {
    throw new Error(
      `Validation-dispute source UTxO ${outRefLabel(threadUtxo)} is missing datum`,
    );
  }
  const datum = Data.from(threadUtxo.datum, PendingValidationClaimDatum);
  if (datum.data === null) {
    throw new Error(
      "Validation-dispute source verification requires pending claim state",
    );
  }
  return datum as PendingValidationClaimDatumData & {
    readonly data: NonNullable<PendingValidationClaimDatumData["data"]>;
  };
};

export type SubmitValidationDisputeVerifySourceResult = {
  readonly txHash: string;
  readonly threadOutRef: string;
  readonly nextThreadOutRef: string;
  readonly inputIndex: number;
  readonly outputIndex: number;
  readonly outcome: "game" | "award";
  readonly responseDeadline: number | null;
  readonly awaitedConfirmation: boolean;
};

export const submitValidationDisputeVerifySource = async ({
  lucid,
  blueprint,
  deploymentInfo,
  network,
  signer,
  threadOutRef,
  sourceReferenceScriptUtxo,
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
  /** The mandatory published V1 validation-trace source script. */
  readonly sourceReferenceScriptUtxo?: UTxO;
  readonly validityRange?: ValidationDisputeValidityRange;
  readonly awaitConfirmation?: boolean;
  /** Optional Q51 pre-submit boundary (workflow ruling R5). */
  readonly preSubmitBoundary?: FraudProofPreSubmitBoundary;
}): Promise<SubmitValidationDisputeVerifySourceResult> => {
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
    label: "validation-dispute source-verification UTxO",
  });
  const sourceContract = contracts.validationTraceDispute.source;
  if (threadUtxo.address !== sourceContract.spendingScriptAddress) {
    throw new Error(
      `Thread UTxO ${outRefLabel(threadUtxo)} is not locked at the validation-dispute source validator`,
    );
  }
  const token = requireComputationThreadToken({
    utxo: threadUtxo,
    computationThreadPolicyId: contracts.computationThread.policyId,
    categoryId: validationTraceDisputeCategory.categoryId,
    categoryLabel: "validation-trace-dispute",
  });
  const inputDatum = requirePendingClaimDatum(threadUtxo);
  if (inputDatum.fraud_prover !== signer.paymentKeyHash) {
    throw new Error(
      `Validation-dispute source verification requires fraud prover ${inputDatum.fraud_prover}, got ${signer.paymentKeyHash}`,
    );
  }
  const endpointsAreValid = committedValidationClaimEndpointsAndSourceAreValid(
    inputDatum.data.challenged_header,
    inputDatum.data.claim,
  );
  const dispute = endpointsAreValid
    ? openValidationDisputeAfterSourceVerification({
        operatorDescriptor: validationTraceDescriptorCoreFromData(
          inputDatum.data.claim.descriptor_membership.value,
        ),
        challengerDescriptor: validationTraceDescriptorCoreFromData(
          inputDatum.data.challenger_descriptor,
        ),
        openTimeUpper: inputDatum.data.open_time_upper,
        challengedBlockEndTime: inputDatum.data.challenged_header.endTime,
        sourceValidityRange: range,
      })
    : null;
  const outputAddress =
    dispute === null
      ? contracts.validationTraceDispute.award.spendingScriptAddress
      : contracts.validationTraceDispute.game.spendingScriptAddress;
  const outputDatum =
    dispute === null
      ? Data.to(
          { fraud_prover: inputDatum.fraud_prover, data: { version: 1n } },
          WinningValidationResolutionDatum,
        )
      : Data.to(
          {
            fraud_prover: inputDatum.fraud_prover,
            data: {
              challenged_header_hash: inputDatum.data.challenged_header_hash,
              operator_vkey: inputDatum.data.challenged_header.operatorVkey,
              dispute: validationDisputeDataFromCore(dispute),
            },
          },
          ValidationDisputeDatum,
        );
  let layout: ContinueLayout | undefined;
  signer.selectWallet(lucid);
  const feeInput = selectFeeInput(await lucid.wallet().getUtxos());
  const sourceScriptCarriage = witnessSpendingValidatorCarriage({
    script: sourceContract.spendingScript,
    referenceUtxo: sourceReferenceScriptUtxo,
    label: "validation-dispute source validator",
  });
  const base = lucid
    .newTx()
    .collectFrom([feeInput])
    .collectFrom(
      [threadUtxo],
      makeVerifySourceRedeemer({
        threadUtxo,
        outputAddress,
        outputDatum,
        threadUnit: token.unit,
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
    sourceScriptCarriage.referenceInputs.length === 0
      ? base
      : base.readFrom([...sourceScriptCarriage.referenceInputs]);
  const tx = sourceScriptCarriage.attach(withReferenceScript);
  const unsigned = await tx.complete({ localUPLCEval: true });
  if (layout === undefined) {
    throw new Error(
      "BuildTxWithRedeemer did not resolve validation-dispute source layout",
    );
  }
  const signed = await unsigned.sign.withWallet().complete();
  requireL1ProofEnvelope(
    signed.toCBOR(),
    "Validation-dispute source verification",
  );
  await reachOptionalPreSubmitBoundary({
    signed,
    boundary: preSubmitBoundary,
    referenceScriptCandidates: [
      {
        role: "validation-dispute source validator",
        utxo: sourceReferenceScriptUtxo,
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
    outcome: dispute === null ? "award" : "game",
    responseDeadline: dispute?.responseDeadline ?? null,
    awaitedConfirmation: awaitConfirmation,
  };
};
