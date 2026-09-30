import {
  acceptedVerdictSubject,
  type ForcedInclusionTxV1,
  forcedVerdictSubject,
  type Header,
  type OutputReference,
  requireInputIndex,
  requireOwnSpendPurpose,
  requireUniqueOutputIndex,
  type RootMembershipProof,
} from "@al-ft/midgard-sdk";
import {
  type BuildTxWithRedeemer,
  Data,
  type LucidEvolution,
  type UTxO,
} from "@lucid-evolution/lucid";

import {
  requireLinearFaultInitialDatum,
  requireLinearFaultReferenceScript,
  requireLinearFaultStepState,
  requireLinearFaultThreadUtxo,
} from "../linear-fault-family.js";
import { submitLinearFaultContinue } from "../linear-fault-submit.js";
import type { MissingNativeScriptTxContracts } from "../missing-native-script-tx/contracts.js";
import { submitMissingNativeScriptTxBinding } from "../missing-native-script-tx/submit-native-binding.js";
import type { PublishedProofChunk } from "../proof-chunk-carriage.js";
import type { ResolvedProverSigner } from "../runtime.js";
import type { SubmitStep01TxInclusion } from "../step-support.js";
import { computationThreadOutputPredicate } from "../tx-layout.js";
import type { FaultProofWitnessReferenceScripts } from "../witness-reference-scripts.js";
import type { FraudProofPreSubmitBoundary } from "../workflow/transaction-boundary.js";
import type { ScriptIntegrityHashMismatchContracts } from "./contracts.js";
import { type ScriptIntegrityHashMismatchEvidence } from "./family.js";
import {
  IntegrityStep01RedeemerSchema,
  IntegrityStep02DatumSchema,
} from "./schemas.js";

export const FAMILY = "script-integrity-hash-mismatch";

export type Common = Readonly<{
  lucid: LucidEvolution;
  contracts: ScriptIntegrityHashMismatchContracts;
  categoryId: string;
  signer: ResolvedProverSigner;
  threadOutRef: string;
  referenceScriptUtxo: UTxO;
  preSubmitBoundary?: FraudProofPreSubmitBoundary;
  awaitConfirmation?: boolean;
}>;

const boundState = (
  subject: ReturnType<typeof acceptedVerdictSubject>,
  header: Header,
  scriptIntegrityHash: string,
) => ({
  subject,
  validation_traces_root: header.validationTracesRoot,
  validation_trace_count: header.validationTraceCount,
  script_integrity_hash: scriptIntegrityHash,
});

export const submitScriptIntegrityHashMismatchStep01Accepted = async (
  args: Common & {
    blueprint: unknown;
    network: Parameters<
      typeof submitMissingNativeScriptTxBinding
    >[0]["network"];
    stateQueueBlockOutRef: string;
    txInclusion: SubmitStep01TxInclusion;
    publishedProofChunks?: readonly PublishedProofChunk[];
    header: Header;
    evidence: ScriptIntegrityHashMismatchEvidence;
    witnessReferenceScripts?: FaultProofWitnessReferenceScripts;
  },
) => {
  const subject = acceptedVerdictSubject(args.txInclusion.nativeTxId);
  if (
    subject.transaction_id !== args.evidence.finding.subject.transaction_id ||
    args.evidence.finding.subject.direction !== subject.direction
  )
    throw new Error(`${FAMILY}: accepted source differs from evidence`);
  const { threadUtxo, threadToken } = await requireLinearFaultThreadUtxo({
    ...args,
    family: FAMILY,
    stepIndex: 0,
  });
  requireLinearFaultInitialDatum({
    threadUtxo,
    signer: args.signer,
    family: FAMILY,
  });
  const nextDatum = Data.to(
    {
      fraud_prover: args.signer.paymentKeyHash,
      data: boundState(subject, args.header, args.evidence.scriptIntegrityHash),
    } as never,
    IntegrityStep02DatumSchema as never,
  );
  return await submitMissingNativeScriptTxBinding({
    lucid: args.lucid,
    blueprint: args.blueprint,
    network: args.network,
    contracts: args.contracts as unknown as MissingNativeScriptTxContracts,
    signer: args.signer,
    stepIndex: 0,
    threadUtxo,
    threadToken,
    stateQueueBlockOutRef: args.stateQueueBlockOutRef,
    txInclusion: args.txInclusion,
    nextDatum,
    spendRedeemerSchema: IntegrityStep01RedeemerSchema,
    publishedProofChunks: args.publishedProofChunks,
    wrapInclusionCarriage: (inclusion) => ({
      source: {
        AcceptedSource: {
          inclusion,
        },
      },
    }),
    referenceScriptUtxo: args.referenceScriptUtxo,
    witnessReferenceScripts: args.witnessReferenceScripts,
    preSubmitBoundary: args.preSubmitBoundary,
    awaitConfirmation: args.awaitConfirmation ?? true,
  });
};

export const submitScriptIntegrityHashMismatchStep01Forced = async (
  args: Common & {
    header: Header;
    membership: RootMembershipProof<OutputReference, ForcedInclusionTxV1>;
    evidence: ScriptIntegrityHashMismatchEvidence;
  },
) => {
  const verdict = args.membership.value.verdict;
  if (verdict === "ForcedTxValid")
    throw new Error(`${FAMILY}: forced-valid source`);
  const subject = forcedVerdictSubject({
    transactionId: args.membership.value.tx_id,
    sourceKey: args.membership.key,
    rejectionReason: verdict.ForcedTxInvalid.reason,
  });
  if (
    subject.transaction_id !== args.evidence.finding.subject.transaction_id ||
    subject.source_key !== args.evidence.finding.subject.source_key ||
    subject.direction !== args.evidence.finding.subject.direction
  )
    throw new Error(`${FAMILY}: forced source differs from evidence`);
  const { threadUtxo, threadToken } = await requireLinearFaultThreadUtxo({
    ...args,
    family: FAMILY,
    stepIndex: 0,
  });
  requireLinearFaultInitialDatum({
    threadUtxo,
    signer: args.signer,
    family: FAMILY,
  });
  const nextDatum = Data.to(
    {
      fraud_prover: args.signer.paymentKeyHash,
      data: boundState(
        subject as never,
        args.header,
        args.evidence.scriptIntegrityHash,
      ),
    } as never,
    IntegrityStep02DatumSchema as never,
  );
  const stepReference = requireLinearFaultReferenceScript({
    utxo: args.referenceScriptUtxo,
    expectedScriptHash: args.contracts.steps[0].spendingScriptHash,
    family: FAMILY,
    stepIndex: 0,
  });
  const matches = computationThreadOutputPredicate({
    address: args.contracts.steps[1].spendingScriptAddress,
    datum: nextDatum,
    unit: threadToken.unit,
  });
  let outputIndex: bigint | undefined;
  const redeemer = ((ctx) => {
    requireOwnSpendPurpose(ctx, threadUtxo, `${FAMILY} step 01`);
    const inputIndex = requireInputIndex(ctx, threadUtxo, `${FAMILY} step 01`);
    outputIndex = requireUniqueOutputIndex(
      ctx.outputs,
      matches,
      `${FAMILY} step 01`,
    );
    return Data.to(
      {
        Continue: [
          {
            source: {
              ForcedSource: {
                input_index: inputIndex,
                output_index: outputIndex,
                header: args.header,
                membership: args.membership,
                direction: subject.direction,
              },
            },
          },
        ],
      } as never,
      IntegrityStep01RedeemerSchema as never,
    );
  }) satisfies BuildTxWithRedeemer;
  args.signer.selectWallet(args.lucid);
  const txHash = await submitLinearFaultContinue({
    lucid: args.lucid,
    signerPaymentKeyHash: args.signer.paymentKeyHash,
    threadUtxo,
    threadUnit: threadToken.unit,
    stepReference,
    stepScript: args.contracts.steps[0].spendingScript,
    stepRole: `${FAMILY} step 01`,
    nextAddress: args.contracts.steps[1].spendingScriptAddress,
    nextDatum,
    redeemer,
    preSubmitBoundary: args.preSubmitBoundary,
    awaitConfirmation: args.awaitConfirmation ?? true,
  });
  if (outputIndex === undefined)
    throw new Error(`${FAMILY}: unresolved layout`);
  return { txHash, nextThreadOutRef: `${txHash}#${outputIndex}` };
};

export const continueState = async <State>({
  args,
  stepIndex,
  currentSchema,
  nextSchema,
  nextState,
  redeemerSchema,
  nextStepIndex,
}: {
  args: Common;
  stepIndex: number;
  currentSchema: unknown;
  nextSchema: unknown;
  nextState: (state: State) => unknown;
  redeemerSchema: unknown;
  nextStepIndex: number;
}) => {
  const { threadUtxo, threadToken } = await requireLinearFaultThreadUtxo({
    ...args,
    family: FAMILY,
    stepIndex,
  });
  const state = requireLinearFaultStepState<State>({
    threadUtxo,
    signer: args.signer,
    schema: currentSchema as never,
    family: FAMILY,
    stepIndex,
  });
  const nextDatum = Data.to(
    {
      fraud_prover: args.signer.paymentKeyHash,
      data: nextState(state),
    } as never,
    nextSchema as never,
  );
  const stepReference = requireLinearFaultReferenceScript({
    utxo: args.referenceScriptUtxo,
    expectedScriptHash: args.contracts.steps[stepIndex]!.spendingScriptHash,
    family: FAMILY,
    stepIndex,
  });
  const matches = computationThreadOutputPredicate({
    address: args.contracts.steps[nextStepIndex]!.spendingScriptAddress,
    datum: nextDatum,
    unit: threadToken.unit,
  });
  let outputIndex: bigint | undefined;
  const redeemer = ((ctx) => {
    requireOwnSpendPurpose(ctx, threadUtxo, `${FAMILY} step ${stepIndex + 1}`);
    const inputIndex = requireInputIndex(
      ctx,
      threadUtxo,
      `${FAMILY} step ${stepIndex + 1}`,
    );
    outputIndex = requireUniqueOutputIndex(
      ctx.outputs,
      matches,
      `${FAMILY} step ${stepIndex + 1}`,
    );
    return Data.to(
      {
        Continue: [{ input_index: inputIndex, output_index: outputIndex }],
      } as never,
      redeemerSchema as never,
    );
  }) satisfies BuildTxWithRedeemer;
  args.signer.selectWallet(args.lucid);
  const txHash = await submitLinearFaultContinue({
    lucid: args.lucid,
    signerPaymentKeyHash: args.signer.paymentKeyHash,
    threadUtxo,
    threadUnit: threadToken.unit,
    stepReference,
    stepScript: args.contracts.steps[stepIndex]!.spendingScript,
    stepRole: `${FAMILY} step ${stepIndex + 1}`,
    nextAddress: args.contracts.steps[nextStepIndex]!.spendingScriptAddress,
    nextDatum,
    redeemer,
    preSubmitBoundary: args.preSubmitBoundary,
    awaitConfirmation: args.awaitConfirmation ?? true,
  });
  if (outputIndex === undefined)
    throw new Error(`${FAMILY}: unresolved layout`);
  return { txHash, nextThreadOutRef: `${txHash}#${outputIndex}` };
};
