import {
  buildMidgardBoundedItem,
  decodeMidgardFieldPreimage,
  decodeMidgardVersionedScript,
  encodeCbor,
  hashMidgardVersionedScript,
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
  acceptedAppendSource,
  acceptedFinishInlineSources,
  type AcceptedReconstructionState,
} from "./accepted-reconstruction-machine.js";
import type { ExecutionNativeScriptInvalidContracts } from "./contracts.js";
import {
  ExecutionNativeScriptInvalidAcceptedDatumSchema,
  ExecutionNativeScriptInvalidAcceptedInlineRedeemerSchema,
  ExecutionNativeScriptInvalidStep03DatumSchema,
} from "./schemas.js";
import {
  FAMILY,
  requireAcceptedPrelude,
} from "./submit-accepted-reconstruction.submit-execution-native-script-invalid-accepted-init.js";

/** Scan one authenticated inline source and enter the bounded evaluator. */
export const submitExecutionNativeScriptInvalidAcceptedInlineSource = async ({
  lucid,
  contracts,
  categoryId,
  signer,
  threadOutRef,
  nativeTxCompactCbor,
  witnessSet,
  scriptsPreimageCbor,
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
  witnessSet: Readonly<{
    addr_tx_wits_hash: string;
    script_tx_wits_hash: string;
    redeemer_tx_wits_hash: string;
  }>;
  scriptsPreimageCbor: string;
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
    stepIndex: 5,
    threadOutRef,
  });
  const state = requireLinearFaultStepState<AcceptedReconstructionState>({
    threadUtxo,
    signer,
    schema: ExecutionNativeScriptInvalidAcceptedDatumSchema as never,
    family: FAMILY,
    stepIndex: 5,
  });
  if (state.phase !== 4n || state.selected_purpose === null)
    throw new Error(`${FAMILY}: inline scanner lacks selected purpose`);
  const itemIndex = Number(state.field_cursor);
  const item = decodeMidgardFieldPreimage(
    Buffer.from(scriptsPreimageCbor, "hex"),
  )[itemIndex];
  if (item === undefined)
    throw new Error(
      `${FAMILY}: inline source cursor is outside retained field`,
    );
  const script = decodeMidgardVersionedScript(item);
  const scriptHash = hashMidgardVersionedScript(script);
  const languageTag =
    script.language === "NativeCardano"
      ? 0n
      : script.language === "PlutusV3"
        ? 3n
        : 128n;
  const bounded = buildMidgardBoundedItem({
    fieldIndex: 6,
    itemIndex,
    bytes: item,
  });
  const source = {
    source_index: state.source_cursor,
    origin_kind: 0n,
    source_key: Buffer.from(encodeCbor(BigInt(itemIndex))).toString("hex"),
    language_tag: languageTag,
    script_hash: scriptHash,
    total_length: BigInt(item.length),
    item_commitment: bounded.commitment.toString("hex"),
  } as const;
  const selected = scriptHash === state.selected_purpose.script_hash;
  const advanced = acceptedAppendSource({
    state,
    source,
    nextScriptHash: selected
      ? contracts.steps[2]!.spendingScriptHash
      : accepted[5]!.spendingScriptHash,
  });
  const nextContract = selected ? contracts.steps[2]! : accepted[5]!;
  const nextData = selected
    ? {
        bound: state.bound,
        prior_ledger_root: state.bound.prior_ledger_root,
        ...source,
        compact_cbor: state.bound.compact_cbor,
      }
    : advanced;
  const nextSchema = selected
    ? ExecutionNativeScriptInvalidStep03DatumSchema
    : ExecutionNativeScriptInvalidAcceptedDatumSchema;
  const nextDatum = Data.to(
    { fraud_prover: signer.paymentKeyHash, data: nextData } as never,
    nextSchema as never,
  );
  const outputMatches = computationThreadOutputPredicate({
    address: nextContract.spendingScriptAddress,
    datum: nextDatum,
    unit: threadToken.unit,
  });
  const stepReference = requireLinearFaultReferenceScript({
    utxo: referenceScriptUtxo,
    expectedScriptHash: accepted[5]!.spendingScriptHash,
    family: FAMILY,
    stepIndex: 11,
  });
  const opening = fieldOpeningForField({
    fieldIndex: 6,
    nativeTxCompactCbor,
    witnessSet,
    carriage: { Inline: { preimage: scriptsPreimageCbor } },
  });
  let outputIndex: bigint | undefined;
  const redeemer = ((ctx) => {
    requireOwnSpendPurpose(ctx, threadUtxo, `${FAMILY} accepted inline source`);
    const inputIndex = requireInputIndex(
      ctx,
      threadUtxo,
      `${FAMILY} accepted inline source`,
    );
    outputIndex = requireUniqueOutputIndex(
      ctx.outputs,
      outputMatches,
      `${FAMILY} accepted inline source output`,
    );
    return Data.to(
      {
        Continue: [
          {
            ScanInline: {
              input_index: inputIndex,
              output_index: outputIndex,
              scripts_opening: opening,
            },
          },
        ],
      } as never,
      ExecutionNativeScriptInvalidAcceptedInlineRedeemerSchema as never,
    );
  }) satisfies BuildTxWithRedeemer;
  signer.selectWallet(lucid);
  const txHash = await submitLinearFaultContinue({
    lucid,
    signerPaymentKeyHash: signer.paymentKeyHash,
    threadUtxo,
    threadUnit: threadToken.unit,
    stepReference,
    stepScript: accepted[5]!.spendingScript,
    stepRole: `${FAMILY} accepted inline source`,
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

/** Exhaust inline witnesses before authenticated reference-source discovery. */
export const submitExecutionNativeScriptInvalidAcceptedFinishInline = async ({
  lucid,
  contracts,
  categoryId,
  signer,
  threadOutRef,
  nativeTxCompactCbor,
  witnessSet,
  scriptsPreimageCbor,
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
  witnessSet: Readonly<{
    addr_tx_wits_hash: string;
    script_tx_wits_hash: string;
    redeemer_tx_wits_hash: string;
  }>;
  scriptsPreimageCbor: string;
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
    stepIndex: 5,
    threadOutRef,
  });
  const state = requireLinearFaultStepState<AcceptedReconstructionState>({
    threadUtxo,
    signer,
    schema: ExecutionNativeScriptInvalidAcceptedDatumSchema as never,
    family: FAMILY,
    stepIndex: 5,
  });
  const count = decodeMidgardFieldPreimage(
    Buffer.from(scriptsPreimageCbor, "hex"),
  ).length;
  if (
    state.phase !== 4n ||
    state.field_cursor !== BigInt(count) ||
    state.selected_source !== null
  )
    throw new Error(`${FAMILY}: inline source prefix is incomplete`);
  const nextState = acceptedFinishInlineSources({
    state,
    nextScriptHash: accepted[6]!.spendingScriptHash,
  });
  const nextDatum = Data.to(
    { fraud_prover: signer.paymentKeyHash, data: nextState } as never,
    ExecutionNativeScriptInvalidAcceptedDatumSchema as never,
  );
  const outputMatches = computationThreadOutputPredicate({
    address: accepted[6]!.spendingScriptAddress,
    datum: nextDatum,
    unit: threadToken.unit,
  });
  const stepReference = requireLinearFaultReferenceScript({
    utxo: referenceScriptUtxo,
    expectedScriptHash: accepted[5]!.spendingScriptHash,
    family: FAMILY,
    stepIndex: 11,
  });
  const opening = fieldOpeningForField({
    fieldIndex: 6,
    nativeTxCompactCbor,
    witnessSet,
    carriage: { Inline: { preimage: scriptsPreimageCbor } },
  });
  let outputIndex: bigint | undefined;
  const redeemer = ((ctx) => {
    const inputIndex = requireInputIndex(
      ctx,
      threadUtxo,
      `${FAMILY} finish inline`,
    );
    outputIndex = requireUniqueOutputIndex(
      ctx.outputs,
      outputMatches,
      `${FAMILY} finish inline output`,
    );
    return Data.to(
      {
        Continue: [
          {
            FinishInline: {
              input_index: inputIndex,
              output_index: outputIndex,
              scripts_opening: opening,
            },
          },
        ],
      } as never,
      ExecutionNativeScriptInvalidAcceptedInlineRedeemerSchema as never,
    );
  }) satisfies BuildTxWithRedeemer;
  signer.selectWallet(lucid);
  const txHash = await submitLinearFaultContinue({
    lucid,
    signerPaymentKeyHash: signer.paymentKeyHash,
    threadUtxo,
    threadUnit: threadToken.unit,
    stepReference,
    stepScript: accepted[5]!.spendingScript,
    stepRole: `${FAMILY} finish inline`,
    nextAddress: accepted[6]!.spendingScriptAddress,
    nextDatum,
    redeemer,
    preSubmitBoundary,
    awaitConfirmation,
  });
  if (outputIndex === undefined)
    throw new Error(`${FAMILY}: unresolved layout`);
  return { txHash, nextThreadOutRef: `${txHash}#${outputIndex.toString()}` };
};
