import { decodeMidgardFieldPreimage } from "@al-ft/midgard-core";
import {
  fieldOpeningForField,
  requireInputIndex,
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
  acceptedAppendPurpose,
  acceptedFinishPurposePhase,
  type AcceptedReconstructionState,
} from "./accepted-reconstruction-machine.js";
import type { ExecutionNativeScriptInvalidContracts } from "./contracts.js";
import {
  ExecutionNativeScriptInvalidAcceptedDatumSchema,
  ExecutionNativeScriptInvalidAcceptedMintRedeemerSchema,
  ExecutionNativeScriptInvalidAcceptedObserverRedeemerSchema,
} from "./schemas.js";
import {
  FAMILY,
  requireAcceptedPrelude,
} from "./submit-accepted-reconstruction.submit-execution-native-script-invalid-accepted-init.js";

/** Advance from a completely authenticated mint or observer prefix. */
export const submitExecutionNativeScriptInvalidAcceptedFinishPurpose = async ({
  lucid,
  contracts,
  categoryId,
  signer,
  threadOutRef,
  phase,
  nativeTxCompactCbor,
  fieldPreimageCbor,
  referenceScriptUtxo,
  preSubmitBoundary,
  awaitConfirmation = true,
}: {
  lucid: LucidEvolution;
  contracts: ExecutionNativeScriptInvalidContracts;
  categoryId: string;
  signer: ResolvedProverSigner;
  threadOutRef: string;
  phase: "mint" | "observer";
  nativeTxCompactCbor: string;
  fieldPreimageCbor: string;
  referenceScriptUtxo: UTxO;
  preSubmitBoundary?: FraudProofPreSubmitBoundary;
  awaitConfirmation?: boolean;
}) => {
  const accepted = requireAcceptedPrelude(contracts);
  const stepIndex = phase === "mint" ? 2 : 3;
  const fieldIndex = phase === "mint" ? 5 : 3;
  const nextIndex = stepIndex + 1;
  const schema =
    phase === "mint"
      ? ExecutionNativeScriptInvalidAcceptedMintRedeemerSchema
      : ExecutionNativeScriptInvalidAcceptedObserverRedeemerSchema;
  const physicalContracts = { ...contracts, steps: accepted };
  const { threadUtxo, threadToken } = await requireLinearFaultThreadUtxo({
    lucid,
    contracts: physicalContracts,
    categoryId,
    family: FAMILY,
    stepIndex,
    threadOutRef,
  });
  const state = requireLinearFaultStepState<AcceptedReconstructionState>({
    threadUtxo,
    signer,
    schema: ExecutionNativeScriptInvalidAcceptedDatumSchema as never,
    family: FAMILY,
    stepIndex,
  });
  const count = decodeMidgardFieldPreimage(
    Buffer.from(fieldPreimageCbor, "hex"),
  ).length;
  if (
    state.phase !== BigInt(stepIndex - 1) ||
    state.field_cursor !== BigInt(count)
  )
    throw new Error(`${FAMILY}: ${phase} prefix is incomplete`);
  const nextState = acceptedFinishPurposePhase({
    state,
    nextScriptHash: accepted[nextIndex]!.spendingScriptHash,
  });
  const nextDatum = Data.to(
    { fraud_prover: signer.paymentKeyHash, data: nextState } as never,
    ExecutionNativeScriptInvalidAcceptedDatumSchema as never,
  );
  const outputMatches = computationThreadOutputPredicate({
    address: accepted[nextIndex]!.spendingScriptAddress,
    datum: nextDatum,
    unit: threadToken.unit,
  });
  const stepReference = requireLinearFaultReferenceScript({
    utxo: referenceScriptUtxo,
    expectedScriptHash: accepted[stepIndex]!.spendingScriptHash,
    family: FAMILY,
    stepIndex: stepIndex + 6,
  });
  const opening = fieldOpeningForField({
    fieldIndex,
    nativeTxCompactCbor,
    carriage: { Inline: { preimage: fieldPreimageCbor } },
  });
  let outputIndex: bigint | undefined;
  const redeemer = ((ctx) => {
    const inputIndex = requireInputIndex(
      ctx,
      threadUtxo,
      `${FAMILY} finish ${phase}`,
    );
    outputIndex = requireUniqueOutputIndex(
      ctx.outputs,
      outputMatches,
      `${FAMILY} finish ${phase} output`,
    );
    const action =
      phase === "mint"
        ? {
            FinishMint: {
              input_index: inputIndex,
              output_index: outputIndex,
              mint_opening: opening,
            },
          }
        : {
            FinishObservers: {
              input_index: inputIndex,
              output_index: outputIndex,
              observer_opening: opening,
            },
          };
    return Data.to({ Continue: [action] } as never, schema as never);
  }) satisfies BuildTxWithRedeemer;
  signer.selectWallet(lucid);
  const txHash = await submitLinearFaultContinue({
    lucid,
    signerPaymentKeyHash: signer.paymentKeyHash,
    threadUtxo,
    threadUnit: threadToken.unit,
    stepReference,
    stepScript: accepted[stepIndex]!.spendingScript,
    stepRole: `${FAMILY} finish ${phase}`,
    nextAddress: accepted[nextIndex]!.spendingScriptAddress,
    nextDatum,
    redeemer,
    preSubmitBoundary,
    awaitConfirmation,
  });
  if (outputIndex === undefined)
    throw new Error(`${FAMILY}: unresolved layout`);
  return { txHash, nextThreadOutRef: `${txHash}#${outputIndex.toString()}` };
};

/** Authenticate one canonical observer purpose. */
export const submitExecutionNativeScriptInvalidAcceptedObserver = async ({
  lucid,
  contracts,
  categoryId,
  signer,
  threadOutRef,
  nativeTxCompactCbor,
  observersPreimageCbor,
  referenceScriptUtxo,
  preSubmitBoundary,
  awaitConfirmation = true,
}: {
  lucid: LucidEvolution;
  contracts: ExecutionNativeScriptInvalidContracts;
  categoryId: string;
  signer: ResolvedProverSigner;
  threadOutRef: string;
  nativeTxCompactCbor: string;
  observersPreimageCbor: string;
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
    stepIndex: 3,
    threadOutRef,
  });
  const state = requireLinearFaultStepState<AcceptedReconstructionState>({
    threadUtxo,
    signer,
    schema: ExecutionNativeScriptInvalidAcceptedDatumSchema as never,
    family: FAMILY,
    stepIndex: 3,
  });
  if (state.phase !== 2n)
    throw new Error(`${FAMILY}: observer scanner received another phase`);
  const item = decodeMidgardFieldPreimage(
    Buffer.from(observersPreimageCbor, "hex"),
  )[Number(state.field_cursor)];
  if (item === undefined || item.length !== 28)
    throw new Error(`${FAMILY}: observer cursor is outside canonical field`);
  const scriptHash = item.toString("hex");
  const selected = state.execution_cursor === state.bound.execution_index;
  const nextState = acceptedAppendPurpose({
    state,
    purposeKind: 2n,
    purposeIndex: state.field_cursor,
    scriptHash,
    subject: scriptHash,
    canonicalKey: scriptHash,
    nextScriptHash: selected
      ? accepted[5]!.spendingScriptHash
      : accepted[3]!.spendingScriptHash,
  });
  const nextContract = selected ? accepted[5]! : accepted[3]!;
  const nextDatum = Data.to(
    { fraud_prover: signer.paymentKeyHash, data: nextState } as never,
    ExecutionNativeScriptInvalidAcceptedDatumSchema as never,
  );
  const outputMatches = computationThreadOutputPredicate({
    address: nextContract.spendingScriptAddress,
    datum: nextDatum,
    unit: threadToken.unit,
  });
  const stepReference = requireLinearFaultReferenceScript({
    utxo: referenceScriptUtxo,
    expectedScriptHash: accepted[3]!.spendingScriptHash,
    family: FAMILY,
    stepIndex: 9,
  });
  const opening = fieldOpeningForField({
    fieldIndex: 3,
    nativeTxCompactCbor,
    carriage: { Inline: { preimage: observersPreimageCbor } },
  });
  let outputIndex: bigint | undefined;
  const redeemer = ((ctx) => {
    const inputIndex = requireInputIndex(ctx, threadUtxo, `${FAMILY} observer`);
    outputIndex = requireUniqueOutputIndex(
      ctx.outputs,
      outputMatches,
      `${FAMILY} observer output`,
    );
    return Data.to(
      {
        Continue: [
          {
            ScanObserver: {
              input_index: inputIndex,
              output_index: outputIndex,
              observer_opening: opening,
            },
          },
        ],
      } as never,
      ExecutionNativeScriptInvalidAcceptedObserverRedeemerSchema as never,
    );
  }) satisfies BuildTxWithRedeemer;
  signer.selectWallet(lucid);
  const txHash = await submitLinearFaultContinue({
    lucid,
    signerPaymentKeyHash: signer.paymentKeyHash,
    threadUtxo,
    threadUnit: threadToken.unit,
    stepReference,
    stepScript: accepted[3]!.spendingScript,
    stepRole: `${FAMILY} observer`,
    nextAddress: nextContract.spendingScriptAddress,
    nextDatum,
    redeemer,
    preSubmitBoundary,
    awaitConfirmation,
  });
  if (outputIndex === undefined)
    throw new Error(`${FAMILY}: unresolved layout`);
  return {
    txHash,
    nextThreadOutRef: `${txHash}#${outputIndex.toString()}`,
    selected,
  };
};
