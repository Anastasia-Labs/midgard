import {
  PreparedValidationResolutionDatum,
  type PreparedValidationResolutionDatum as PreparedValidationResolutionDatumData,
  validationDisputeCoreFromData,
  type ValidationMachineState,
  ValidationResolutionDatum,
  type ValidationResolutionDatum as ValidationResolutionDatumData,
  type ValidationTraceProof,
  WinningValidationResolutionDatum,
  type WinningValidationResolutionDatum as WinningValidationResolutionDatumData,
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
import { type ContinueLayout } from "./redeemers.js";
import {
  makePrepareResolutionRedeemer,
  type SubmitValidationDisputePrepareResolutionResult,
  validationResolverIndex,
} from "./resolution.submit-validation-dispute-enter-resolution.js";
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

export const submitValidationDisputePrepareResolution = async ({
  lucid,
  blueprint,
  deploymentInfo,
  network,
  signer,
  threadOutRef,
  preState,
  operatorPost,
  challengerPost,
  boundaryReferenceScriptUtxo,
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
  readonly preState: ValidationMachineState;
  readonly operatorPost: ValidationTraceProof;
  readonly challengerPost: ValidationTraceProof;
  /** The mandatory published V1 validation-trace boundary script. */
  readonly boundaryReferenceScriptUtxo?: UTxO;
  readonly validityRange?: ValidationDisputeValidityRange;
  readonly awaitConfirmation?: boolean;
  /** Optional Q51 pre-submit boundary (workflow ruling R5). */
  readonly preSubmitBoundary?: FraudProofPreSubmitBoundary;
}): Promise<SubmitValidationDisputePrepareResolutionResult> => {
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
    label: "validation-dispute boundary UTxO",
  });
  const boundaryContract = contracts.validationTraceDispute.boundary;
  if (threadUtxo.address !== boundaryContract.spendingScriptAddress) {
    throw new Error(
      `Thread UTxO ${outRefLabel(threadUtxo)} is not locked at the validation-dispute boundary validator`,
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
      `Validation-dispute boundary preparation requires fraud prover ${inputDatum.fraud_prover}, got ${signer.paymentKeyHash}`,
    );
  }
  const dispute = validationDisputeCoreFromData(inputDatum.data.dispute);
  if (dispute.turn.type !== "readyForOneStep") {
    throw new Error(
      "Validation dispute must finish bisection before boundary preparation",
    );
  }
  const resolverIndex = validationResolverIndex(preState.phase);
  const resolverContract =
    contracts.validationTraceDispute.resolvers[resolverIndex];
  if (resolverContract === undefined) {
    throw new Error(
      `Validation resolver ${resolverIndex.toString()} is missing from the deployment`,
    );
  }
  if (
    operatorPost.state_hash !== inputDatum.data.dispute.operator_high_hash ||
    challengerPost.state_hash !== inputDatum.data.dispute.challenger_high_hash
  ) {
    throw new Error(
      "Validation boundary successor proofs do not match the authenticated dispute",
    );
  }
  const outputDatum = Data.to(
    {
      fraud_prover: inputDatum.fraud_prover,
      data: {
        version: 1n,
        pre_state: preState,
        operator_successor_hash: operatorPost.state_hash,
        challenger_successor_hash: challengerPost.state_hash,
      },
    },
    ValidationResolutionDatum,
  );
  let layout: ContinueLayout | undefined;
  signer.selectWallet(lucid);
  const feeInput = selectFeeInput(await lucid.wallet().getUtxos());
  const boundaryScriptCarriage = witnessSpendingValidatorCarriage({
    script: boundaryContract.spendingScript,
    referenceUtxo: boundaryReferenceScriptUtxo,
    label: "validation-dispute boundary validator",
  });
  const base = lucid
    .newTx()
    .collectFrom([feeInput])
    .collectFrom(
      [threadUtxo],
      makePrepareResolutionRedeemer({
        threadUtxo,
        outputAddress: resolverContract.spendingScriptAddress,
        outputDatum,
        threadUnit: token.unit,
        resolverIndex: BigInt(resolverIndex),
        preState,
        operatorPost,
        challengerPost,
        onLayout: (resolvedLayout) => {
          layout = resolvedLayout;
        },
      }),
    )
    .pay.ToContract(
      resolverContract.spendingScriptAddress,
      { kind: "inline", value: outputDatum },
      threadAssets(threadUtxo, token.unit),
    )
    .validFrom(range.validFrom)
    .validTo(range.validTo)
    .addSignerKey(signer.paymentKeyHash);
  const withReferenceScript =
    boundaryScriptCarriage.referenceInputs.length === 0
      ? base
      : base.readFrom([...boundaryScriptCarriage.referenceInputs]);
  const tx = boundaryScriptCarriage.attach(withReferenceScript);
  const unsigned = await tx.complete({ localUPLCEval: true });
  if (layout === undefined) {
    throw new Error(
      "BuildTxWithRedeemer did not resolve validation-dispute boundary layout",
    );
  }
  const signed = await unsigned.sign.withWallet().complete();
  requireL1ProofEnvelope(
    signed.toCBOR(),
    "Validation-dispute boundary preparation",
  );
  await reachOptionalPreSubmitBoundary({
    signed,
    boundary: preSubmitBoundary,
    referenceScriptCandidates: [
      {
        role: "validation-dispute boundary validator",
        utxo: boundaryReferenceScriptUtxo,
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
    resolverIndex,
    inputIndex: Number(layout.inputIndex),
    outputIndex: Number(layout.outputIndex),
    awaitedConfirmation: awaitConfirmation,
  };
};

export const requireResolutionDatum = (
  threadUtxo: UTxO,
): ValidationResolutionDatumData & {
  readonly data: NonNullable<ValidationResolutionDatumData["data"]>;
} => {
  if (threadUtxo.datum == null) {
    throw new Error(
      `Validation resolution UTxO ${outRefLabel(threadUtxo)} is missing datum`,
    );
  }
  const datum = Data.from(threadUtxo.datum, ValidationResolutionDatum);
  if (datum.data === null) {
    throw new Error("Validation resolution requires initialized V1 state");
  }
  return datum as ValidationResolutionDatumData & {
    readonly data: NonNullable<ValidationResolutionDatumData["data"]>;
  };
};

export const requirePreparedResolutionDatum = (
  threadUtxo: UTxO,
): PreparedValidationResolutionDatumData & {
  readonly data: NonNullable<PreparedValidationResolutionDatumData["data"]>;
} => {
  if (threadUtxo.datum == null) {
    throw new Error(
      `Prepared validation resolution UTxO ${outRefLabel(threadUtxo)} is missing datum`,
    );
  }
  const datum = Data.from(threadUtxo.datum, PreparedValidationResolutionDatum);
  if (datum.data === null) {
    throw new Error(
      "Prepared validation resolution requires initialized V1 state",
    );
  }
  return datum as PreparedValidationResolutionDatumData & {
    readonly data: NonNullable<PreparedValidationResolutionDatumData["data"]>;
  };
};

export const requireWinningResolutionDatum = (
  threadUtxo: UTxO,
): WinningValidationResolutionDatumData & {
  readonly data: NonNullable<WinningValidationResolutionDatumData["data"]>;
} => {
  if (threadUtxo.datum == null) {
    throw new Error(
      `Winning validation resolution UTxO ${outRefLabel(threadUtxo)} is missing datum`,
    );
  }
  const datum = Data.from(threadUtxo.datum, WinningValidationResolutionDatum);
  if (datum.data === null || datum.data.version !== 1n) {
    throw new Error(
      "Winning validation resolution requires canonical V1 state",
    );
  }
  return datum as WinningValidationResolutionDatumData & {
    readonly data: NonNullable<WinningValidationResolutionDatumData["data"]>;
  };
};

export type SubmitValidationDisputePrepareSelectedResult = {
  readonly txHash: string;
  readonly threadOutRef: string;
  readonly nextThreadOutRef: string;
  readonly resolverIndex: number;
  readonly semanticResolverIndex: number;
  readonly semanticResolverGlobalIndex: number;
  readonly inputIndex: number;
  readonly outputIndex: number;
  readonly awaitedConfirmation: boolean;
};
