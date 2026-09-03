import {
  buildMidgardBoundedItem,
  decodeMidgardAddressBytes,
  decodeMidgardFieldPreimage,
  decodeMidgardLedgerOutputCommitment,
  decodeMidgardMintPolicyItem,
  decodeMidgardSpendInputItem,
  decodeMidgardTxOutput,
  decodeMidgardVersionedScript,
  encodeCbor,
  hashMidgardVersionedScript,
} from "@al-ft/midgard-core";
import {
  fieldOpeningForField,
  requireInputIndex,
  requireOwnSpendPurpose,
  requireUniqueOutputIndex,
  requireWithdrawalRedeemerIndex,
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
} from "../linear-fault-family-v1.js";
import { submitLinearFaultContinue } from "../linear-fault-submit-v1.js";
import {
  DEFAULT_CONFIRMATION_POLL_MS,
  encodeRawPhasMembershipProofRedeemer,
  phasMembershipRewardAddress,
  type ResolvedProverSigner,
} from "../runtime.js";
import { selectFeeInput } from "../submit-step-01.js";
import { computationThreadOutputPredicate } from "../tx-layout.js";
import { witnessWithdrawalValidatorCarriage } from "../witness-reference-scripts-v1.js";
import {
  type FraudProofPreSubmitBoundary,
  reachFraudProofPreSubmitBoundary,
  workflowReferenceScript,
} from "../workflow/transaction-boundary-v1.js";
import {
  acceptedAdvanceNonScript,
  acceptedAdvanceReferenceWithoutSource,
  acceptedAppendPurpose,
  acceptedAppendSource,
  acceptedFinishInlineSources,
  acceptedFinishPurposePhase,
  acceptedFinishReceivePass,
  type AcceptedReconstructionBound,
  type AcceptedReconstructionState,
  acceptedScanReceiveOutput,
  initialAcceptedReconstructionState,
} from "./accepted-reconstruction-machine-v1.js";
import type { ExecutionNativeScriptInvalidContracts } from "./contracts-v1.js";
import {
  ExecutionNativeScriptInvalidAcceptedDatumSchema,
  ExecutionNativeScriptInvalidAcceptedInitRedeemerSchema,
  ExecutionNativeScriptInvalidAcceptedInlineRedeemerSchema,
  ExecutionNativeScriptInvalidAcceptedMintRedeemerSchema,
  ExecutionNativeScriptInvalidAcceptedObserverRedeemerSchema,
  ExecutionNativeScriptInvalidAcceptedReceiveRedeemerSchema,
  ExecutionNativeScriptInvalidAcceptedReferenceRedeemerSchema,
  ExecutionNativeScriptInvalidAcceptedSpendRedeemerSchema,
  ExecutionNativeScriptInvalidStep02DatumSchema,
  ExecutionNativeScriptInvalidStep03DatumSchema,
} from "./schemas-v1.js";

const FAMILY = "execution-native-script-invalid";

const requireAcceptedPrelude = (
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

/** Authenticate and consume exactly one canonical spend-input descriptor. */
export const submitExecutionNativeScriptInvalidAcceptedSpend = async ({
  lucid,
  network,
  contracts,
  categoryId,
  signer,
  threadOutRef,
  nativeTxCompactCbor,
  spendInputsPreimageCbor,
  descriptorCbor,
  membershipProof,
  membershipProofCbor,
  membershipReferenceScriptUtxo,
  referenceScriptUtxo,
  preSubmitBoundary,
  awaitConfirmation = true,
}: {
  lucid: LucidEvolution;
  network: Parameters<typeof phasMembershipRewardAddress>[0];
  contracts: ExecutionNativeScriptInvalidContracts;
  categoryId: string;
  signer: ResolvedProverSigner;
  threadOutRef: string;
  nativeTxCompactCbor: string;
  spendInputsPreimageCbor: string;
  descriptorCbor: string;
  membershipProof: unknown;
  membershipProofCbor: string;
  membershipReferenceScriptUtxo: UTxO;
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
  if (state.phase !== 0n)
    throw new Error(`${FAMILY}: spend scanner received another phase`);
  const items = decodeMidgardFieldPreimage(
    Buffer.from(spendInputsPreimageCbor, "hex"),
  );
  const item = items[Number(state.field_cursor)];
  if (item === undefined)
    throw new Error(`${FAMILY}: spend cursor is outside retained field`);
  const descriptor = decodeMidgardLedgerOutputCommitment(
    Buffer.from(descriptorCbor, "hex"),
  );
  const credential = decodeMidgardAddressBytes(
    descriptor.address,
  ).paymentCredential;
  const selects =
    credential.kind === "Script" &&
    state.execution_cursor === state.bound.execution_index;
  const nextState =
    credential.kind === "Script"
      ? acceptedAppendPurpose({
          state,
          purposeKind: 0n,
          purposeIndex: state.field_cursor,
          scriptHash: credential.hash.toString("hex"),
          subject: item.toString("hex"),
          canonicalKey: item.toString("hex"),
          nextScriptHash: selects
            ? accepted[5]!.spendingScriptHash
            : accepted[1]!.spendingScriptHash,
        })
      : acceptedAdvanceNonScript({
          state,
          canonicalKey: item.toString("hex"),
          nextScriptHash: accepted[1]!.spendingScriptHash,
        });
  const nextContract = selects ? accepted[5]! : accepted[1]!;
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
    expectedScriptHash: accepted[1]!.spendingScriptHash,
    family: FAMILY,
    stepIndex: 7,
  });
  if (membershipReferenceScriptUtxo.scriptRef == null)
    throw new Error(`${FAMILY}: prior-ledger membership script absent`);
  const membershipScript = membershipReferenceScriptUtxo.scriptRef;
  const membershipAddress = phasMembershipRewardAddress(
    network,
    membershipScript,
  );
  const membershipCarriage = witnessWithdrawalValidatorCarriage({
    script: membershipScript,
    referenceUtxo: membershipReferenceScriptUtxo,
    label: `${FAMILY} accepted spend membership`,
  });
  const referenceInputs = [
    stepReference,
    ...membershipCarriage.referenceInputs,
  ];
  const opening = fieldOpeningForField({
    fieldIndex: 0,
    nativeTxCompactCbor,
    carriage: { Inline: { preimage: spendInputsPreimageCbor } },
  });
  let outputIndex: bigint | undefined;
  const redeemer = ((ctx) => {
    requireOwnSpendPurpose(ctx, threadUtxo, `${FAMILY} accepted spend`);
    const inputIndex = requireInputIndex(
      ctx,
      threadUtxo,
      `${FAMILY} accepted spend`,
    );
    outputIndex = requireUniqueOutputIndex(
      ctx.outputs,
      outputMatches,
      `${FAMILY} accepted spend output`,
    );
    return Data.to(
      {
        Continue: [
          {
            ScanSpend: {
              input_index: inputIndex,
              output_index: outputIndex,
              spend_inputs_opening: opening,
              descriptor_cbor: descriptorCbor,
              membership: {
                RedeemerCarriedMembership: {
                  membership_proof: membershipProof,
                  membership_proof_script_redeemer_index:
                    requireWithdrawalRedeemerIndex(
                      ctx,
                      membershipAddress,
                      `${FAMILY} accepted spend membership`,
                    ),
                },
              },
            },
          },
        ],
      } as never,
      ExecutionNativeScriptInvalidAcceptedSpendRedeemerSchema as never,
    );
  }) satisfies BuildTxWithRedeemer;
  signer.selectWallet(lucid);
  const feeInput = selectFeeInput(await lucid.wallet().getUtxos());
  const unsigned = await membershipCarriage
    .attach(
      lucid
        .newTx()
        .collectFrom([feeInput])
        .collectFrom([threadUtxo], redeemer)
        .readFrom(referenceInputs)
        .withdraw(
          membershipAddress,
          0n,
          encodeRawPhasMembershipProofRedeemer({
            root: state.bound.prior_ledger_root,
            keyBytes: item.toString("hex"),
            valueBytes: descriptorCbor,
            membershipProofCbor,
          }),
        )
        .pay.ToContract(
          nextContract.spendingScriptAddress,
          { kind: "inline", value: nextDatum },
          {
            lovelace: threadUtxo.assets.lovelace ?? 0n,
            [threadToken.unit]: 1n,
          },
        )
        .addSignerKey(signer.paymentKeyHash),
    )
    .complete({ localUPLCEval: true });
  if (outputIndex === undefined)
    throw new Error(`${FAMILY}: unresolved layout`);
  const signed = await unsigned.sign.withWallet().complete();
  const expectedHash = await reachFraudProofPreSubmitBoundary({
    signed,
    referenceScripts: [
      workflowReferenceScript({
        role: `${FAMILY}-accepted-spend`,
        utxo: stepReference,
        expectedScript: accepted[1]!.spendingScript,
      }),
      workflowReferenceScript({
        role: `${FAMILY}-accepted-spend-membership`,
        utxo: membershipReferenceScriptUtxo,
        expectedScript: membershipScript,
      }),
    ],
    boundary: preSubmitBoundary,
  });
  const txHash = await signed.submit();
  if (txHash !== expectedHash)
    throw new Error(`${FAMILY}: accepted spend transaction hash changed`);
  if (awaitConfirmation)
    await lucid.awaitTx(txHash, DEFAULT_CONFIRMATION_POLL_MS);
  return {
    txHash,
    nextThreadOutRef: `${txHash}#${outputIndex.toString()}`,
    selected: selects,
  };
};

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

/** Authenticate one resolved-reference source against the predecessor ledger. */
export const submitExecutionNativeScriptInvalidAcceptedReferenceSource =
  async ({
    lucid,
    network,
    contracts,
    categoryId,
    signer,
    threadOutRef,
    nativeTxCompactCbor,
    referenceInputsPreimageCbor,
    descriptorCbor,
    membershipProof,
    membershipProofCbor,
    membershipReferenceScriptUtxo,
    referenceScriptUtxo,
    preSubmitBoundary,
    awaitConfirmation = true,
  }: {
    lucid: LucidEvolution;
    network: Parameters<typeof phasMembershipRewardAddress>[0];
    contracts: ExecutionNativeScriptInvalidContracts;
    categoryId: string;
    signer: ResolvedProverSigner;
    threadOutRef: string;
    nativeTxCompactCbor: string;
    referenceInputsPreimageCbor: string;
    descriptorCbor: string;
    membershipProof: unknown;
    membershipProofCbor: string;
    membershipReferenceScriptUtxo: UTxO;
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
      stepIndex: 6,
      threadOutRef,
    });
    const state = requireLinearFaultStepState<AcceptedReconstructionState>({
      threadUtxo,
      signer,
      schema: ExecutionNativeScriptInvalidAcceptedDatumSchema as never,
      family: FAMILY,
      stepIndex: 6,
    });
    if (state.phase !== 5n || state.selected_purpose === null)
      throw new Error(`${FAMILY}: reference scanner lacks selected purpose`);
    const item = decodeMidgardFieldPreimage(
      Buffer.from(referenceInputsPreimageCbor, "hex"),
    )[Number(state.field_cursor)];
    if (item === undefined)
      throw new Error(`${FAMILY}: reference cursor is outside canonical field`);
    const outRef = decodeMidgardSpendInputItem(item);
    const descriptor = decodeMidgardLedgerOutputCommitment(
      Buffer.from(descriptorCbor, "hex"),
    );
    if (descriptor.outputIndex !== outRef.outputIndex)
      throw new Error(`${FAMILY}: reference descriptor output index changed`);
    const hasScript = descriptor.referenceScriptLanguage !== -1;
    const source = hasScript
      ? {
          source_index: state.source_cursor,
          origin_kind: 1n,
          source_key: item.toString("hex"),
          language_tag: BigInt(descriptor.referenceScriptLanguage),
          script_hash: descriptor.referenceScriptHash.toString("hex"),
          total_length: BigInt(descriptor.referenceScriptTotalLength),
          item_commitment:
            descriptor.referenceScriptItemCommitment.toString("hex"),
        }
      : null;
    const selected = source?.script_hash === state.selected_purpose.script_hash;
    const advanced =
      source === null
        ? acceptedAdvanceReferenceWithoutSource({
            state,
            nextScriptHash: accepted[6]!.spendingScriptHash,
          })
        : acceptedAppendSource({
            state,
            source,
            nextScriptHash: selected
              ? contracts.steps[2]!.spendingScriptHash
              : accepted[6]!.spendingScriptHash,
          });
    const nextContract = selected ? contracts.steps[2]! : accepted[6]!;
    const nextData = selected
      ? {
          bound: state.bound,
          prior_ledger_root: state.bound.prior_ledger_root,
          ...source!,
          compact_cbor: state.bound.compact_cbor,
        }
      : advanced;
    const nextDatum = Data.to(
      { fraud_prover: signer.paymentKeyHash, data: nextData } as never,
      (selected
        ? ExecutionNativeScriptInvalidStep03DatumSchema
        : ExecutionNativeScriptInvalidAcceptedDatumSchema) as never,
    );
    const outputMatches = computationThreadOutputPredicate({
      address: nextContract.spendingScriptAddress,
      datum: nextDatum,
      unit: threadToken.unit,
    });
    const stepReference = requireLinearFaultReferenceScript({
      utxo: referenceScriptUtxo,
      expectedScriptHash: accepted[6]!.spendingScriptHash,
      family: FAMILY,
      stepIndex: 12,
    });
    if (membershipReferenceScriptUtxo.scriptRef == null)
      throw new Error(`${FAMILY}: prior-ledger membership script absent`);
    const membershipScript = membershipReferenceScriptUtxo.scriptRef;
    const membershipAddress = phasMembershipRewardAddress(
      network,
      membershipScript,
    );
    const membershipCarriage = witnessWithdrawalValidatorCarriage({
      script: membershipScript,
      referenceUtxo: membershipReferenceScriptUtxo,
      label: `${FAMILY} accepted reference membership`,
    });
    const opening = fieldOpeningForField({
      fieldIndex: 2,
      nativeTxCompactCbor,
      carriage: { Inline: { preimage: referenceInputsPreimageCbor } },
    });
    let outputIndex: bigint | undefined;
    const redeemer = ((ctx) => {
      const inputIndex = requireInputIndex(
        ctx,
        threadUtxo,
        `${FAMILY} accepted reference`,
      );
      outputIndex = requireUniqueOutputIndex(
        ctx.outputs,
        outputMatches,
        `${FAMILY} accepted reference output`,
      );
      return Data.to(
        {
          Continue: [
            {
              input_index: inputIndex,
              output_index: outputIndex,
              reference_inputs_opening: opening,
              descriptor_cbor: descriptorCbor,
              membership: {
                RedeemerCarriedMembership: {
                  membership_proof: membershipProof,
                  membership_proof_script_redeemer_index:
                    requireWithdrawalRedeemerIndex(
                      ctx,
                      membershipAddress,
                      `${FAMILY} accepted reference membership`,
                    ),
                },
              },
            },
          ],
        } as never,
        ExecutionNativeScriptInvalidAcceptedReferenceRedeemerSchema as never,
      );
    }) satisfies BuildTxWithRedeemer;
    signer.selectWallet(lucid);
    const feeInput = selectFeeInput(await lucid.wallet().getUtxos());
    const unsigned = await membershipCarriage
      .attach(
        lucid
          .newTx()
          .collectFrom([feeInput])
          .collectFrom([threadUtxo], redeemer)
          .readFrom([stepReference, ...membershipCarriage.referenceInputs])
          .withdraw(
            membershipAddress,
            0n,
            encodeRawPhasMembershipProofRedeemer({
              root: state.bound.prior_ledger_root,
              keyBytes: item.toString("hex"),
              valueBytes: descriptorCbor,
              membershipProofCbor,
            }),
          )
          .pay.ToContract(
            nextContract.spendingScriptAddress,
            { kind: "inline", value: nextDatum },
            {
              lovelace: threadUtxo.assets.lovelace ?? 0n,
              [threadToken.unit]: 1n,
            },
          )
          .addSignerKey(signer.paymentKeyHash),
      )
      .complete({ localUPLCEval: true });
    if (outputIndex === undefined)
      throw new Error(`${FAMILY}: unresolved layout`);
    const signed = await unsigned.sign.withWallet().complete();
    const expectedHash = await reachFraudProofPreSubmitBoundary({
      signed,
      referenceScripts: [
        workflowReferenceScript({
          role: `${FAMILY}-accepted-reference`,
          utxo: stepReference,
          expectedScript: accepted[6]!.spendingScript,
        }),
        workflowReferenceScript({
          role: `${FAMILY}-accepted-reference-membership`,
          utxo: membershipReferenceScriptUtxo,
          expectedScript: membershipScript,
        }),
      ],
      boundary: preSubmitBoundary,
    });
    const txHash = await signed.submit();
    if (txHash !== expectedHash)
      throw new Error(`${FAMILY}: accepted reference transaction hash changed`);
    if (awaitConfirmation)
      await lucid.awaitTx(txHash, DEFAULT_CONFIRMATION_POLL_MS);
    return {
      txHash,
      nextThreadOutRef: `${txHash}#${outputIndex.toString()}`,
      selected,
    };
  };
