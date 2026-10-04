import {
  requireInputIndex,
  requireOwnSpendPurpose,
  requireUniqueOutputIndex,
  ValidationBoundarySpendRedeemer,
  validationDisputeCoreFromData,
  type ValidationMachineState,
  WinningValidationResolutionDatum,
} from "@al-ft/midgard-sdk";
import {
  type BuildTxWithRedeemer,
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
import { computationThreadOutputPredicate } from "../../tx-layout.js";
import { witnessSpendingValidatorCarriage } from "../../witness-reference-scripts.js";
import { type FraudProofPreSubmitBoundary } from "../../workflow/transaction-boundary.js";
import { type ContinueLayout } from "./redeemers.js";
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

export type SubmitValidationDisputeAwardTerminalPaddingResult = {
  readonly txHash: string;
  readonly threadOutRef: string;
  readonly nextThreadOutRef: string;
  readonly inputIndex: number;
  readonly outputIndex: number;
  readonly awaitedConfirmation: boolean;
};

export const makeTerminalPaddingRedeemer = ({
  threadUtxo,
  outputAddress,
  outputDatum,
  threadUnit,
  terminalState,
  onLayout,
}: {
  readonly threadUtxo: UTxO;
  readonly outputAddress: string;
  readonly outputDatum: string;
  readonly threadUnit: string;
  readonly terminalState: ValidationMachineState;
  readonly onLayout: (layout: ContinueLayout) => void;
}): BuildTxWithRedeemer =>
  ((ctx) => {
    requireOwnSpendPurpose(ctx, threadUtxo, "terminal-padding award");
    const layout: ContinueLayout = {
      inputIndex: requireInputIndex(ctx, threadUtxo, "terminal-padding award"),
      outputIndex: requireUniqueOutputIndex(
        ctx.outputs,
        computationThreadOutputPredicate({
          address: outputAddress,
          datum: outputDatum,
          unit: threadUnit,
        }),
        "terminal-padding award",
      ),
    };
    onLayout(layout);
    return Data.to(
      {
        Continue: [
          {
            AwardTerminalPadding: {
              input_index: layout.inputIndex,
              output_index: layout.outputIndex,
              terminal_state: terminalState,
            },
          },
        ],
      },
      ValidationBoundarySpendRedeemer,
    );
  }) satisfies BuildTxWithRedeemer;

export const submitValidationDisputeAwardTerminalPadding = async ({
  lucid,
  blueprint,
  deploymentInfo,
  network,
  signer,
  threadOutRef,
  terminalState,
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
  readonly terminalState: ValidationMachineState;
  /** The mandatory published V1 validation-trace boundary script. */
  readonly boundaryReferenceScriptUtxo?: UTxO;
  readonly validityRange?: ValidationDisputeValidityRange;
  readonly awaitConfirmation?: boolean;
  /** Optional Q51 pre-submit boundary (workflow ruling R5). */
  readonly preSubmitBoundary?: FraudProofPreSubmitBoundary;
}): Promise<SubmitValidationDisputeAwardTerminalPaddingResult> => {
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
      `Validation-dispute terminal-padding award requires fraud prover ${inputDatum.fraud_prover}, got ${signer.paymentKeyHash}`,
    );
  }
  const dispute = validationDisputeCoreFromData(inputDatum.data.dispute);
  if (dispute.turn.type !== "readyForOneStep") {
    throw new Error(
      "Validation dispute must finish bisection before terminal-padding award",
    );
  }
  const awardContract = contracts.validationTraceDispute.award;
  const outputDatum = Data.to(
    {
      fraud_prover: inputDatum.fraud_prover,
      data: { version: 1n },
    },
    WinningValidationResolutionDatum,
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
      makeTerminalPaddingRedeemer({
        threadUtxo,
        outputAddress: awardContract.spendingScriptAddress,
        outputDatum,
        threadUnit: token.unit,
        terminalState,
        onLayout: (resolvedLayout) => {
          layout = resolvedLayout;
        },
      }),
    )
    .pay.ToContract(
      awardContract.spendingScriptAddress,
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
      "BuildTxWithRedeemer did not resolve validation-dispute terminal-padding layout",
    );
  }
  const signed = await unsigned.sign.withWallet().complete();
  requireL1ProofEnvelope(
    signed.toCBOR(),
    "Validation-dispute terminal-padding award",
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
    inputIndex: Number(layout.inputIndex),
    outputIndex: Number(layout.outputIndex),
    awaitedConfirmation: awaitConfirmation,
  };
};
