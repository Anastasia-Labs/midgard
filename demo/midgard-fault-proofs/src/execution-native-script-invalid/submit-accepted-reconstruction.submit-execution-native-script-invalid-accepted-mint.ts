import {
  decodeMidgardFieldPreimage,
  decodeMidgardMintPolicyItem,
} from "@al-ft/midgard-core";
import {
  fieldOpeningForField,
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
  acceptedAppendPurpose,
  acceptedFinishPurposePhase,
  type AcceptedReconstructionState,
} from "./accepted-reconstruction-machine.js";
import type { ExecutionNativeScriptInvalidContracts } from "./contracts.js";
import {
  ExecutionNativeScriptInvalidAcceptedDatumSchema,
  ExecutionNativeScriptInvalidAcceptedMintRedeemerSchema,
  ExecutionNativeScriptInvalidAcceptedSpendRedeemerSchema,
} from "./schemas.js";
import {
  FAMILY,
  requireAcceptedPrelude,
} from "./submit-accepted-reconstruction.submit-execution-native-script-invalid-accepted-init.js";

/** Close the authenticated spend prefix after proving the complete empty field. */
export const submitExecutionNativeScriptInvalidAcceptedFinishSpends = async ({
  lucid,
  contracts,
  categoryId,
  signer,
  threadOutRef,
  nativeTxCompactCbor,
  spendInputsPreimageCbor,
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
  spendInputsPreimageCbor: string;
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
    stepIndex: 1,
    threadOutRef,
  });
  const state = requireLinearFaultStepState<AcceptedReconstructionState>({
    threadUtxo,
    signer,
    schema: ExecutionNativeScriptInvalidAcceptedDatumSchema as never,
    family: FAMILY,
    stepIndex: 1,
  });
  const count = decodeMidgardFieldPreimage(
    Buffer.from(spendInputsPreimageCbor, "hex"),
  ).length;
  if (state.phase !== 0n || state.field_cursor !== BigInt(count))
    throw new Error(`${FAMILY}: spend prefix is incomplete`);
  const nextState = acceptedFinishPurposePhase({
    state,
    nextScriptHash: accepted[2]!.spendingScriptHash,
  });
  const nextDatum = Data.to(
    { fraud_prover: signer.paymentKeyHash, data: nextState } as never,
    ExecutionNativeScriptInvalidAcceptedDatumSchema as never,
  );
  const outputMatches = computationThreadOutputPredicate({
    address: accepted[2]!.spendingScriptAddress,
    datum: nextDatum,
    unit: threadToken.unit,
  });
  const stepReference = requireLinearFaultReferenceScript({
    utxo: referenceScriptUtxo,
    expectedScriptHash: accepted[1]!.spendingScriptHash,
    family: FAMILY,
    stepIndex: 7,
  });
  const opening = fieldOpeningForField({
    fieldIndex: 0,
    nativeTxCompactCbor,
    carriage: { Inline: { preimage: spendInputsPreimageCbor } },
  });
  let outputIndex: bigint | undefined;
  const redeemer = ((ctx) => {
    requireOwnSpendPurpose(ctx, threadUtxo, `${FAMILY} finish spends`);
    const inputIndex = requireInputIndex(
      ctx,
      threadUtxo,
      `${FAMILY} finish spends`,
    );
    outputIndex = requireUniqueOutputIndex(
      ctx.outputs,
      outputMatches,
      `${FAMILY} finish spends output`,
    );
    return Data.to(
      {
        Continue: [
          {
            FinishSpends: {
              input_index: inputIndex,
              output_index: outputIndex,
              spend_inputs_opening: opening,
            },
          },
        ],
      } as never,
      ExecutionNativeScriptInvalidAcceptedSpendRedeemerSchema as never,
    );
  }) satisfies BuildTxWithRedeemer;
  signer.selectWallet(lucid);
  const txHash = await submitLinearFaultContinue({
    lucid,
    signerPaymentKeyHash: signer.paymentKeyHash,
    threadUtxo,
    threadUnit: threadToken.unit,
    stepReference,
    stepScript: accepted[1]!.spendingScript,
    stepRole: `${FAMILY} finish spends`,
    nextAddress: accepted[2]!.spendingScriptAddress,
    nextDatum,
    redeemer,
    preSubmitBoundary,
    awaitConfirmation,
  });
  if (outputIndex === undefined)
    throw new Error(`${FAMILY}: unresolved layout`);
  return { txHash, nextThreadOutRef: `${txHash}#${outputIndex.toString()}` };
};

/** Authenticate one canonical mint-policy purpose. */
export const submitExecutionNativeScriptInvalidAcceptedMint = async ({
  lucid,
  contracts,
  categoryId,
  signer,
  threadOutRef,
  nativeTxCompactCbor,
  mintPreimageCbor,
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
  mintPreimageCbor: string;
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
    stepIndex: 2,
    threadOutRef,
  });
  const state = requireLinearFaultStepState<AcceptedReconstructionState>({
    threadUtxo,
    signer,
    schema: ExecutionNativeScriptInvalidAcceptedDatumSchema as never,
    family: FAMILY,
    stepIndex: 2,
  });
  if (state.phase !== 1n)
    throw new Error(`${FAMILY}: mint scanner received another phase`);
  const itemIndex = Number(state.field_cursor);
  const item = decodeMidgardFieldPreimage(Buffer.from(mintPreimageCbor, "hex"))[
    itemIndex
  ];
  if (item === undefined)
    throw new Error(`${FAMILY}: mint cursor is outside retained field`);
  const policyId = Buffer.from(
    decodeMidgardMintPolicyItem(item).policyId,
  ).toString("hex");
  const selected = state.execution_cursor === state.bound.execution_index;
  const nextState = acceptedAppendPurpose({
    state,
    purposeKind: 1n,
    purposeIndex: state.field_cursor,
    scriptHash: policyId,
    subject: policyId,
    canonicalKey: policyId,
    nextScriptHash: selected
      ? accepted[5]!.spendingScriptHash
      : accepted[2]!.spendingScriptHash,
  });
  const nextContract = selected ? accepted[5]! : accepted[2]!;
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
    expectedScriptHash: accepted[2]!.spendingScriptHash,
    family: FAMILY,
    stepIndex: 8,
  });
  const opening = fieldOpeningForField({
    fieldIndex: 5,
    nativeTxCompactCbor,
    carriage: { Inline: { preimage: mintPreimageCbor } },
  });
  let outputIndex: bigint | undefined;
  const redeemer = ((ctx) => {
    requireOwnSpendPurpose(ctx, threadUtxo, `${FAMILY} accepted mint`);
    const inputIndex = requireInputIndex(
      ctx,
      threadUtxo,
      `${FAMILY} accepted mint`,
    );
    outputIndex = requireUniqueOutputIndex(
      ctx.outputs,
      outputMatches,
      `${FAMILY} accepted mint output`,
    );
    return Data.to(
      {
        Continue: [
          {
            ScanMint: {
              input_index: inputIndex,
              output_index: outputIndex,
              mint_opening: opening,
            },
          },
        ],
      } as never,
      ExecutionNativeScriptInvalidAcceptedMintRedeemerSchema as never,
    );
  }) satisfies BuildTxWithRedeemer;
  signer.selectWallet(lucid);
  const txHash = await submitLinearFaultContinue({
    lucid,
    signerPaymentKeyHash: signer.paymentKeyHash,
    threadUtxo,
    threadUnit: threadToken.unit,
    stepReference,
    stepScript: accepted[2]!.spendingScript,
    stepRole: `${FAMILY} accepted mint`,
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
