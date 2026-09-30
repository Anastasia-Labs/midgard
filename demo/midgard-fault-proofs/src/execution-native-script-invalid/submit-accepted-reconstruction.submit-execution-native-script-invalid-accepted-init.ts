import {
  requireInputIndex,
  requireOwnSpendPurpose,
  requireUniqueOutputIndex,
} from "@al-ft/midgard-sdk";
import {
  type BuildTxWithRedeemer,
  Data,
  type LucidEvolution,
  type UTxO,
} from "@lucid-evolution/lucid";

import {
  requireLinearFaultReferenceScript,
  requireLinearFaultStepState,
  requireLinearFaultThreadUtxo,
} from "../linear-fault-family.js";
import { submitLinearFaultContinue } from "../linear-fault-submit.js";
import { type ResolvedProverSigner } from "../runtime.js";
import { computationThreadOutputPredicate } from "../tx-layout.js";
import { type FraudProofPreSubmitBoundary } from "../workflow/transaction-boundary.js";
import {
  type AcceptedReconstructionBound,
  initialAcceptedReconstructionState,
} from "./accepted-reconstruction-machine.js";
import type { ExecutionNativeScriptInvalidContracts } from "./contracts.js";
import {
  ExecutionNativeScriptInvalidAcceptedDatumSchema,
  ExecutionNativeScriptInvalidAcceptedInitRedeemerSchema,
  ExecutionNativeScriptInvalidStep02DatumSchema,
} from "./schemas.js";

export const FAMILY = "execution-native-script-invalid";

export const requireAcceptedPrelude = (
  contracts: ExecutionNativeScriptInvalidContracts,
) => {
  if (contracts.acceptedPrelude?.length !== 7)
    throw new Error(
      `${FAMILY}: seven accepted reconstruction scripts required`,
    );
  return contracts.acceptedPrelude;
};

/**
 * Enter the accepted-direction canonical reconstruction. The bound compact
 * transaction and prior-ledger root come solely from applied step 1; this API
 * accepts no verdict, coordinate, source descriptor, or callback actuator.
 */
export const submitExecutionNativeScriptInvalidAcceptedInit = async ({
  lucid,
  contracts,
  categoryId,
  signer,
  threadOutRef,
  referenceScriptUtxo,
  preSubmitBoundary,
  awaitConfirmation = true,
}: {
  lucid: LucidEvolution;
  contracts: ExecutionNativeScriptInvalidContracts;
  categoryId: string;
  signer: ResolvedProverSigner;
  threadOutRef: string;
  referenceScriptUtxo: UTxO;
  preSubmitBoundary?: FraudProofPreSubmitBoundary;
  awaitConfirmation?: boolean;
}) => {
  const accepted = requireAcceptedPrelude(contracts);
  const physicalContracts = { ...contracts, steps: accepted };
  const { threadUtxo, threadToken } = await requireLinearFaultThreadUtxo({
    lucid,
    contracts: physicalContracts,
    categoryId,
    family: FAMILY,
    stepIndex: 0,
    threadOutRef,
  });
  const bound = requireLinearFaultStepState<AcceptedReconstructionBound>({
    threadUtxo,
    signer,
    schema: ExecutionNativeScriptInvalidStep02DatumSchema as never,
    family: FAMILY,
    stepIndex: 0,
  });
  if (bound.subject.direction !== 0n || bound.subject.source_kind !== 0n)
    throw new Error(
      `${FAMILY}: accepted reconstruction requires accepted source`,
    );
  const state = initialAcceptedReconstructionState({
    bound,
    nextScriptHash: accepted[1]!.spendingScriptHash,
  });
  const nextDatum = Data.to(
    { fraud_prover: signer.paymentKeyHash, data: state } as never,
    ExecutionNativeScriptInvalidAcceptedDatumSchema as never,
  );
  const outputMatches = computationThreadOutputPredicate({
    address: accepted[1]!.spendingScriptAddress,
    datum: nextDatum,
    unit: threadToken.unit,
  });
  const stepReference = requireLinearFaultReferenceScript({
    utxo: referenceScriptUtxo,
    expectedScriptHash: accepted[0]!.spendingScriptHash,
    family: FAMILY,
    stepIndex: 6,
  });
  let outputIndex: bigint | undefined;
  const redeemer = ((ctx) => {
    requireOwnSpendPurpose(ctx, threadUtxo, `${FAMILY} accepted init`);
    const inputIndex = requireInputIndex(
      ctx,
      threadUtxo,
      `${FAMILY} accepted init`,
    );
    outputIndex = requireUniqueOutputIndex(
      ctx.outputs,
      outputMatches,
      `${FAMILY} accepted init output`,
    );
    return Data.to(
      {
        Continue: [{ input_index: inputIndex, output_index: outputIndex }],
      } as never,
      ExecutionNativeScriptInvalidAcceptedInitRedeemerSchema as never,
    );
  }) satisfies BuildTxWithRedeemer;
  signer.selectWallet(lucid);
  const txHash = await submitLinearFaultContinue({
    lucid,
    signerPaymentKeyHash: signer.paymentKeyHash,
    threadUtxo,
    threadUnit: threadToken.unit,
    stepReference,
    stepScript: accepted[0]!.spendingScript,
    stepRole: `${FAMILY} accepted init`,
    nextAddress: accepted[1]!.spendingScriptAddress,
    nextDatum,
    redeemer,
    preSubmitBoundary,
    awaitConfirmation,
  });
  if (outputIndex === undefined)
    throw new Error(`${FAMILY}: unresolved layout`);
  return { txHash, nextThreadOutRef: `${txHash}#${outputIndex.toString()}` };
};
