import {
  requireInputIndex,
  requireOwnSpendPurpose,
  requireUniqueOutputIndex,
  type WithdrawalMistagPreparedEvidence,
} from "@al-ft/midgard-sdk";
import {
  type BuildTxWithRedeemer,
  Data,
  type LucidEvolution,
  type UTxO,
} from "@lucid-evolution/lucid";

import {
  DEFAULT_CONFIRMATION_POLL_MS,
  type ResolvedProverSigner,
} from "../runtime.js";
import { selectFeeInput } from "../step-support.js";
import { computationThreadOutputPredicate } from "../tx-layout.js";
import { structuredDataPublicationPlan } from "../workflow/structured-data-preimage.js";
import {
  type FraudProofPreSubmitBoundary,
  reachFraudProofPreSubmitBoundary,
  workflowReferenceScriptsUsedByTransaction,
} from "../workflow/transaction-boundary.js";
import type { WithdrawalMistagContracts } from "./contracts.js";
import {
  requireWithdrawalMistagReferenceScript,
  requireWithdrawalMistagThreadUtxo,
  withdrawalMistagError,
  withdrawalMistagStepLabel,
} from "./submit-common.js";
import {
  datumSchemas,
  type IntermediateStepIndex,
  redeemerSchemas,
  referenceScriptRoles,
  requireLiveDatum,
  stepArgs,
  type SubmitWithdrawalMistagStepResult,
  withdrawalMistagStates,
  withdrawalMistagStepPayloadCbor,
} from "./submit-withdrawal-mistag-steps.step-args.js";

export const submitWithdrawalMistagIntermediateStep = async ({
  lucid,
  contracts,
  signer,
  prepared,
  stepIndex,
  threadOutRef,
  hubOracleUtxo,
  stateQueueBlockUtxo,
  referenceScriptUtxo,
  evidenceReferences,
  preSubmitBoundary,
  awaitConfirmation = true,
}: {
  readonly lucid: LucidEvolution;
  readonly contracts: WithdrawalMistagContracts;
  readonly signer: ResolvedProverSigner;
  readonly prepared: WithdrawalMistagPreparedEvidence;
  readonly stepIndex: IntermediateStepIndex;
  readonly threadOutRef: string;
  readonly hubOracleUtxo?: UTxO;
  readonly stateQueueBlockUtxo?: UTxO;
  /** Production reference script used by this proof step. */
  readonly referenceScriptUtxo: UTxO;
  readonly evidenceReferences?: readonly UTxO[];
  readonly preSubmitBoundary?: FraudProofPreSubmitBoundary;
  readonly awaitConfirmation?: boolean;
}): Promise<SubmitWithdrawalMistagStepResult> => {
  const { threadUtxo, threadToken } = await requireWithdrawalMistagThreadUtxo({
    lucid,
    contracts,
    stepIndex,
    threadOutRef,
  });
  const states = withdrawalMistagStates(prepared);
  requireLiveDatum({
    threadUtxo,
    signer,
    stepIndex,
    expectedState: states[stepIndex],
  });
  const nextState = states[stepIndex + 1];
  const nextDatum = Data.to(
    { fraud_prover: signer.paymentKeyHash, data: nextState } as never,
    datumSchemas[stepIndex + 1] as never,
  );
  const outputMatches = computationThreadOutputPredicate({
    address: contracts.steps[stepIndex + 1].spendingScriptAddress,
    datum: nextDatum,
    unit: threadToken.unit,
  });
  if (evidenceReferences !== undefined) {
    const planned = structuredDataPublicationPlan(
      withdrawalMistagStepPayloadCbor(prepared, stepIndex),
    );
    if (
      evidenceReferences.length !== planned.publicationDatums.length ||
      evidenceReferences.some(
        (ref, index) => ref.datum !== planned.publicationDatums[index],
      )
    )
      throw withdrawalMistagError(
        "published evidence changed retained payload",
      );
  }
  const references = [
    ...(evidenceReferences ?? []),
    ...(hubOracleUtxo === undefined ? [] : [hubOracleUtxo]),
    ...(stateQueueBlockUtxo === undefined ? [] : [stateQueueBlockUtxo]),
    requireWithdrawalMistagReferenceScript({
      utxo: referenceScriptUtxo,
      contracts,
      stepIndex,
    }),
  ];
  let resolved:
    | { readonly inputIndex: bigint; readonly outputIndex: bigint }
    | undefined;
  const redeemer = ((ctx) => {
    requireOwnSpendPurpose(
      ctx,
      threadUtxo,
      withdrawalMistagStepLabel(stepIndex),
    );
    const inputIndex = requireInputIndex(
      ctx,
      threadUtxo,
      withdrawalMistagStepLabel(stepIndex),
    );
    const outputIndex = requireUniqueOutputIndex(
      ctx.outputs,
      outputMatches,
      `${withdrawalMistagStepLabel(stepIndex)} output`,
    );
    resolved = { inputIndex, outputIndex };
    return Data.to(
      {
        Continue: [
          stepArgs({
            stepIndex,
            prepared,
            inputIndex,
            outputIndex,
            ctx,
            hubOracleUtxo,
            stateQueueBlockUtxo,
            evidenceReferences,
          }),
        ],
      } as never,
      redeemerSchemas[stepIndex] as never,
    );
  }) satisfies BuildTxWithRedeemer;

  signer.selectWallet(lucid);
  const feeInput = selectFeeInput(await lucid.wallet().getUtxos());
  const base = lucid
    .newTx()
    .collectFrom([feeInput])
    .collectFrom([threadUtxo], redeemer)
    .readFrom(references)
    .pay.ToContract(
      contracts.steps[stepIndex + 1].spendingScriptAddress,
      { kind: "inline", value: nextDatum },
      threadUtxo.assets,
    )
    .addSignerKey(signer.paymentKeyHash);
  const unsigned = await base.complete({ localUPLCEval: true });
  if (resolved === undefined) {
    throw withdrawalMistagError(
      "transaction builder did not resolve step layout",
    );
  }
  const signed = await unsigned.sign.withWallet().complete();
  const expectedTxHash = await reachFraudProofPreSubmitBoundary({
    signed,
    referenceScripts: workflowReferenceScriptsUsedByTransaction({
      signed,
      candidates: [
        {
          role: referenceScriptRoles[stepIndex],
          utxo: referenceScriptUtxo,
          expectedScript: contracts.steps[stepIndex].spendingScript,
        },
      ],
    }),
    boundary: preSubmitBoundary,
  });
  const txHash = await signed.submit();
  if (txHash !== expectedTxHash) {
    throw withdrawalMistagError(
      `${withdrawalMistagStepLabel(stepIndex)} provider returned ${txHash}, expected ${expectedTxHash}`,
    );
  }
  if (awaitConfirmation)
    await lucid.awaitTx(txHash, DEFAULT_CONFIRMATION_POLL_MS);
  return {
    txHash,
    threadOutRef,
    nextThreadOutRef: `${txHash}#${resolved.outputIndex.toString()}`,
    stepIndex,
    nextStepIndex: (stepIndex + 1) as 1 | 2 | 3 | 4,
    inputIndex: Number(resolved.inputIndex),
    outputIndex: Number(resolved.outputIndex),
    awaitedConfirmation: awaitConfirmation,
  };
};

export const submitWithdrawalMistagStep01 = (
  args: Omit<
    Parameters<typeof submitWithdrawalMistagIntermediateStep>[0],
    "stepIndex"
  >,
) => submitWithdrawalMistagIntermediateStep({ ...args, stepIndex: 0 });

export const submitWithdrawalMistagStep02 = (
  args: Omit<
    Parameters<typeof submitWithdrawalMistagIntermediateStep>[0],
    "stepIndex"
  >,
) => submitWithdrawalMistagIntermediateStep({ ...args, stepIndex: 1 });

export const submitWithdrawalMistagStep03 = (
  args: Omit<
    Parameters<typeof submitWithdrawalMistagIntermediateStep>[0],
    "stepIndex"
  >,
) => submitWithdrawalMistagIntermediateStep({ ...args, stepIndex: 2 });

export const submitWithdrawalMistagStep04 = (
  args: Omit<
    Parameters<typeof submitWithdrawalMistagIntermediateStep>[0],
    "stepIndex"
  >,
) => submitWithdrawalMistagIntermediateStep({ ...args, stepIndex: 3 });
