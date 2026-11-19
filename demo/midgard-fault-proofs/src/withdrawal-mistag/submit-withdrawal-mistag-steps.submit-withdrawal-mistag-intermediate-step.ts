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

import { type ResolvedProverSigner } from "../runtime.js";
import { completeStep, fundStep } from "../step-completion.js";
import { finishStep } from "../step-completion.js";
import { computationThreadOutputPredicate } from "../tx-layout.js";
import { structuredDataPublicationPlan } from "../workflow/structured-data-preimage.js";
import type { WithdrawalMistagContracts } from "./contracts.js";
import {
  requireWithdrawalMistagReferenceScript,
  requireWithdrawalMistagThreadUtxo,
  withdrawalMistagError,
  withdrawalMistagStepLabel,
} from "./submit-common.js";
import type { WithdrawalMistagStepBuild } from "./submit-withdrawal-mistag-steps.step-args.js";
import {
  datumSchemas,
  type IntermediateStepIndex,
  referenceScriptRoles,
  requireLiveDatum,
  withdrawalMistagStates,
  withdrawalMistagStepPayloadCbor,
  withdrawalMistagStepRedeemerCbor,
} from "./submit-withdrawal-mistag-steps.step-args.js";

export const buildWithdrawalMistagIntermediateStep = async ({
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
}): Promise<WithdrawalMistagStepBuild> => {
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
    return withdrawalMistagStepRedeemerCbor({
      stepIndex,
      prepared,
      inputIndex,
      outputIndex,
      ctx,
      hubOracleUtxo,
      stateQueueBlockUtxo,
      evidenceReferences,
    });
  }) satisfies BuildTxWithRedeemer;

  const funding = await fundStep(lucid, { signer });
  const base = funding.tx
    .collectFrom([threadUtxo], redeemer)
    .readFrom(references)
    .pay.ToContract(
      contracts.steps[stepIndex + 1].spendingScriptAddress,
      { kind: "inline", value: nextDatum },
      threadUtxo.assets,
    );
  const signed = await completeStep(base, funding);
  if (resolved === undefined) {
    throw withdrawalMistagError(
      "transaction builder did not resolve step layout",
    );
  }
  const transaction = finishStep(signed, {
    referenceScriptCandidates: [
      {
        role: referenceScriptRoles[stepIndex],
        utxo: referenceScriptUtxo,
        expectedScript: contracts.steps[stepIndex].spendingScript,
      },
    ],
  });
  return {
    transaction,
    threadOutRef,
    nextThreadOutRef: `${transaction.txHash}#${resolved.outputIndex.toString()}`,
    stepIndex,
    nextStepIndex: (stepIndex + 1) as 1 | 2 | 3 | 4,
    inputIndex: Number(resolved.inputIndex),
    outputIndex: Number(resolved.outputIndex),
  };
};

export const buildWithdrawalMistagStep01 = (
  args: Omit<
    Parameters<typeof buildWithdrawalMistagIntermediateStep>[0],
    "stepIndex"
  >,
) => buildWithdrawalMistagIntermediateStep({ ...args, stepIndex: 0 });

export const buildWithdrawalMistagStep02 = (
  args: Omit<
    Parameters<typeof buildWithdrawalMistagIntermediateStep>[0],
    "stepIndex"
  >,
) => buildWithdrawalMistagIntermediateStep({ ...args, stepIndex: 1 });

export const buildWithdrawalMistagStep03 = (
  args: Omit<
    Parameters<typeof buildWithdrawalMistagIntermediateStep>[0],
    "stepIndex"
  >,
) => buildWithdrawalMistagIntermediateStep({ ...args, stepIndex: 2 });

export const buildWithdrawalMistagStep04 = (
  args: Omit<
    Parameters<typeof buildWithdrawalMistagIntermediateStep>[0],
    "stepIndex"
  >,
) => buildWithdrawalMistagIntermediateStep({ ...args, stepIndex: 3 });
