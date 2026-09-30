import {
  decodeMidgardAddressBytes,
  decodeMidgardFieldPreimage,
  decodeMidgardTxOutput,
} from "@al-ft/midgard-core";
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
  acceptedFinishReceivePass,
  type AcceptedReconstructionState,
  acceptedScanReceiveOutput,
} from "./accepted-reconstruction-machine.js";
import type { ExecutionNativeScriptInvalidContracts } from "./contracts.js";
import {
  ExecutionNativeScriptInvalidAcceptedDatumSchema,
  ExecutionNativeScriptInvalidAcceptedReceiveRedeemerSchema,
} from "./schemas.js";
import {
  FAMILY,
  requireAcceptedPrelude,
} from "./submit-accepted-reconstruction.submit-execution-native-script-invalid-accepted-init.js";

/** Scan one canonical output during receive-purpose set reconstruction. */
export const submitExecutionNativeScriptInvalidAcceptedReceive = async ({
  lucid,
  contracts,
  categoryId,
  signer,
  threadOutRef,
  nativeTxCompactCbor,
  outputsPreimageCbor,
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
  outputsPreimageCbor: string;
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
    stepIndex: 4,
    threadOutRef,
  });
  const state = requireLinearFaultStepState<AcceptedReconstructionState>({
    threadUtxo,
    signer,
    schema: ExecutionNativeScriptInvalidAcceptedDatumSchema as never,
    family: FAMILY,
    stepIndex: 4,
  });
  if (state.phase !== 3n)
    throw new Error(`${FAMILY}: receive scanner received another phase`);
  const item = decodeMidgardFieldPreimage(
    Buffer.from(outputsPreimageCbor, "hex"),
  )[Number(state.field_cursor)];
  if (item === undefined)
    throw new Error(`${FAMILY}: output cursor is outside canonical field`);
  const address = decodeMidgardAddressBytes(
    decodeMidgardTxOutput(item).address,
  );
  const candidate =
    address.protected && address.paymentCredential.kind === "Script"
      ? address.paymentCredential.hash.toString("hex")
      : null;
  const nextState = acceptedScanReceiveOutput({
    state,
    candidate,
    nextScriptHash: accepted[4]!.spendingScriptHash,
  });
  return await submitReceiveTransition({
    lucid,
    contracts,
    categoryId,
    signer,
    threadUtxo,
    threadToken,
    state: nextState,
    nativeTxCompactCbor,
    outputsPreimageCbor,
    referenceScriptUtxo,
    action: "ScanOutput",
    preSubmitBoundary,
    awaitConfirmation,
  });
};

/** Finish one output pass and emit the next unique receive purpose. */
export const submitExecutionNativeScriptInvalidAcceptedFinishReceivePass =
  async ({
    lucid,
    contracts,
    categoryId,
    signer,
    threadOutRef,
    nativeTxCompactCbor,
    outputsPreimageCbor,
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
    outputsPreimageCbor: string;
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
      stepIndex: 4,
      threadOutRef,
    });
    const state = requireLinearFaultStepState<AcceptedReconstructionState>({
      threadUtxo,
      signer,
      schema: ExecutionNativeScriptInvalidAcceptedDatumSchema as never,
      family: FAMILY,
      stepIndex: 4,
    });
    const count = decodeMidgardFieldPreimage(
      Buffer.from(outputsPreimageCbor, "hex"),
    ).length;
    if (state.phase !== 3n || state.field_cursor !== BigInt(count))
      throw new Error(`${FAMILY}: receive pass is incomplete`);
    const selected = state.execution_cursor === state.bound.execution_index;
    const nextState = acceptedFinishReceivePass({
      state,
      nextScanScriptHash: accepted[4]!.spendingScriptHash,
      nextSourceScriptHash: accepted[5]!.spendingScriptHash,
    });
    return await submitReceiveTransition({
      lucid,
      contracts,
      categoryId,
      signer,
      threadUtxo,
      threadToken,
      state: nextState,
      nativeTxCompactCbor,
      outputsPreimageCbor,
      referenceScriptUtxo,
      action: "FinishOutputPass",
      preSubmitBoundary,
      awaitConfirmation,
      selected,
    });
  };

const submitReceiveTransition = async ({
  lucid,
  contracts,
  signer,
  threadUtxo,
  threadToken,
  state,
  nativeTxCompactCbor,
  outputsPreimageCbor,
  referenceScriptUtxo,
  action,
  preSubmitBoundary,
  awaitConfirmation,
  selected = false,
}: {
  lucid: LucidEvolution;
  contracts: ExecutionNativeScriptInvalidContracts;
  categoryId: string;
  signer: ResolvedProverSigner;
  threadUtxo: UTxO;
  threadToken: { unit: string };
  state: AcceptedReconstructionState;
  nativeTxCompactCbor: string;
  outputsPreimageCbor: string;
  referenceScriptUtxo: UTxO;
  action: "ScanOutput" | "FinishOutputPass";
  preSubmitBoundary?: FraudProofPreSubmitBoundary;
  awaitConfirmation: boolean;
  selected?: boolean;
}) => {
  const accepted = requireAcceptedPrelude(contracts);
  const nextContract = selected ? accepted[5]! : accepted[4]!;
  const nextDatum = Data.to(
    { fraud_prover: signer.paymentKeyHash, data: state } as never,
    ExecutionNativeScriptInvalidAcceptedDatumSchema as never,
  );
  const outputMatches = computationThreadOutputPredicate({
    address: nextContract.spendingScriptAddress,
    datum: nextDatum,
    unit: threadToken.unit,
  });
  const stepReference = requireLinearFaultReferenceScript({
    utxo: referenceScriptUtxo,
    expectedScriptHash: accepted[4]!.spendingScriptHash,
    family: FAMILY,
    stepIndex: 10,
  });
  const opening = fieldOpeningForField({
    fieldIndex: 1,
    nativeTxCompactCbor,
    carriage: { Inline: { preimage: outputsPreimageCbor } },
  });
  let outputIndex: bigint | undefined;
  const redeemer = ((ctx) => {
    const inputIndex = requireInputIndex(ctx, threadUtxo, `${FAMILY} receive`);
    outputIndex = requireUniqueOutputIndex(
      ctx.outputs,
      outputMatches,
      `${FAMILY} receive output`,
    );
    const body = {
      input_index: inputIndex,
      output_index: outputIndex,
      outputs_opening: opening,
    };
    return Data.to(
      { Continue: [{ [action]: body }] } as never,
      ExecutionNativeScriptInvalidAcceptedReceiveRedeemerSchema as never,
    );
  }) satisfies BuildTxWithRedeemer;
  signer.selectWallet(lucid);
  const txHash = await submitLinearFaultContinue({
    lucid,
    signerPaymentKeyHash: signer.paymentKeyHash,
    threadUtxo,
    threadUnit: threadToken.unit,
    stepReference,
    stepScript: accepted[4]!.spendingScript,
    stepRole: `${FAMILY} receive`,
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
