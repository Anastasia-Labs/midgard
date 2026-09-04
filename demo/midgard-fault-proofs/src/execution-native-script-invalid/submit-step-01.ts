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
  requireLinearFaultThreadUtxo,
} from "../linear-fault-family.js";
import { submitLinearFaultContinue } from "../linear-fault-submit.js";
import type { MissingNativeScriptTxContracts } from "../missing-native-script-tx/contracts.js";
import { submitMissingNativeScriptTxBinding } from "../missing-native-script-tx/submit-native-binding.js";
import type { ResolvedProverSigner } from "../runtime.js";
import type { SubmitStep01TxInclusion } from "../submit-step-01.js";
import { computationThreadOutputPredicate } from "../tx-layout.js";
import type { FaultProofWitnessReferenceScripts } from "../witness-reference-scripts.js";
import type { FraudProofPreSubmitBoundary } from "../workflow/transaction-boundary.js";
import type { ExecutionNativeScriptInvalidContracts } from "./contracts.js";
import {
  classifyExecutionNativeScriptInvalidFinding,
  type ExecutionNativeScriptInvalidFinding,
} from "./family.js";
import {
  ExecutionNativeScriptInvalidStep01RedeemerSchema,
  ExecutionNativeScriptInvalidStep02DatumSchema,
} from "./schemas.js";

const FAMILY = "execution-native-script-invalid";
const boundState = ({
  subject,
  header,
  executionIndex,
  accusedClass,
  compactCbor,
}: {
  subject: ExecutionNativeScriptInvalidFinding["subject"];
  header: Header;
  executionIndex: bigint;
  accusedClass: bigint;
  compactCbor: string;
}) => ({
  subject,
  validation_traces_root: header.validationTracesRoot,
  validation_trace_count: header.validationTraceCount,
  execution_index: executionIndex,
  accused_class: accusedClass,
  prior_ledger_root: header.prevUtxosRoot,
  compact_cbor: compactCbor,
});

export const submitExecutionNativeScriptInvalidStep01Accepted = async ({
  lucid,
  blueprint,
  network,
  contracts,
  categoryId,
  signer,
  threadOutRef,
  stateQueueBlockOutRef,
  txInclusion,
  header,
  executionIndex,
  referenceScriptUtxo,
  witnessReferenceScripts,
  preSubmitBoundary,
  awaitConfirmation = true,
}: {
  lucid: LucidEvolution;
  blueprint: unknown;
  network: Parameters<typeof submitMissingNativeScriptTxBinding>[0]["network"];
  contracts: ExecutionNativeScriptInvalidContracts;
  categoryId: string;
  signer: ResolvedProverSigner;
  threadOutRef: string;
  stateQueueBlockOutRef: string;
  txInclusion: SubmitStep01TxInclusion;
  header: Header;
  executionIndex: bigint;
  referenceScriptUtxo: UTxO;
  witnessReferenceScripts?: FaultProofWitnessReferenceScripts;
  preSubmitBoundary?: FraudProofPreSubmitBoundary;
  awaitConfirmation?: boolean;
}) => {
  const subject = acceptedVerdictSubject(txInclusion.nativeTxId);
  const finding = classifyExecutionNativeScriptInvalidFinding({
    subject,
    executionIndex: Number(executionIndex),
  });
  const { threadUtxo, threadToken } = await requireLinearFaultThreadUtxo({
    lucid,
    contracts,
    categoryId,
    family: FAMILY,
    stepIndex: 0,
    threadOutRef,
  });
  requireLinearFaultInitialDatum({ threadUtxo, signer, family: FAMILY });
  const nextDatum = Data.to(
    {
      fraud_prover: signer.paymentKeyHash,
      data: boundState({
        subject: finding.subject,
        header,
        executionIndex,
        accusedClass: -1n,
        compactCbor: txInclusion.nativeTxCompactCbor,
      }),
    } as never,
    ExecutionNativeScriptInvalidStep02DatumSchema as never,
  );
  const acceptedInit = contracts.acceptedPrelude?.[0];
  if (acceptedInit === undefined)
    throw new Error(`${FAMILY}: accepted reconstruction contracts unavailable`);
  return await submitMissingNativeScriptTxBinding({
    lucid,
    blueprint,
    network,
    contracts: {
      ...contracts,
      steps: [contracts.steps[0]!, acceptedInit],
    } as unknown as MissingNativeScriptTxContracts,
    signer,
    stepIndex: 0,
    threadUtxo,
    threadToken,
    stateQueueBlockOutRef,
    txInclusion,
    nextDatum,
    spendRedeemerSchema: ExecutionNativeScriptInvalidStep01RedeemerSchema,
    wrapInclusionArgs: (args) => ({
      source: {
        AcceptedSource: { inclusion: { RedeemerCarriedInclusion: [args] } },
      },
      execution_index: executionIndex,
    }),
    referenceScriptUtxo,
    witnessReferenceScripts,
    preSubmitBoundary,
    awaitConfirmation,
  });
};

export const submitExecutionNativeScriptInvalidStep01Forced = async ({
  lucid,
  contracts,
  categoryId,
  signer,
  threadOutRef,
  header,
  membership,
  executionIndex,
  referenceScriptUtxo,
  preSubmitBoundary,
  awaitConfirmation = true,
}: {
  lucid: LucidEvolution;
  contracts: ExecutionNativeScriptInvalidContracts;
  categoryId: string;
  signer: ResolvedProverSigner;
  threadOutRef: string;
  header: Header;
  membership: RootMembershipProof<OutputReference, ForcedInclusionTxV1>;
  executionIndex: bigint;
  referenceScriptUtxo: UTxO;
  preSubmitBoundary?: FraudProofPreSubmitBoundary;
  awaitConfirmation?: boolean;
}) => {
  const verdict = membership.value.verdict;
  if (verdict === "ForcedTxValid")
    throw new Error(`${FAMILY}: forced-valid leaf has no rejection`);
  const subject = forcedVerdictSubject({
    transactionId: membership.value.tx_id,
    sourceKey: membership.key,
    rejectionReason: verdict.ForcedTxInvalid.reason,
  });
  const finding = classifyExecutionNativeScriptInvalidFinding({
    subject,
    executionIndex: Number(executionIndex),
  });
  const { threadUtxo, threadToken } = await requireLinearFaultThreadUtxo({
    lucid,
    contracts,
    categoryId,
    family: FAMILY,
    stepIndex: 0,
    threadOutRef,
  });
  requireLinearFaultInitialDatum({ threadUtxo, signer, family: FAMILY });
  const nextDatum = Data.to(
    {
      fraud_prover: signer.paymentKeyHash,
      data: boundState({
        subject: finding.subject,
        header,
        executionIndex,
        accusedClass: -1n,
        compactCbor: membership.value.source.compact_cbor,
      }),
    } as never,
    ExecutionNativeScriptInvalidStep02DatumSchema as never,
  );
  const stepReference = requireLinearFaultReferenceScript({
    utxo: referenceScriptUtxo,
    expectedScriptHash: contracts.steps[0].spendingScriptHash,
    family: FAMILY,
    stepIndex: 0,
  });
  const outputMatches = computationThreadOutputPredicate({
    address: contracts.steps[1].spendingScriptAddress,
    datum: nextDatum,
    unit: threadToken.unit,
  });
  let outputIndex: bigint | undefined;
  const redeemer = ((ctx) => {
    requireOwnSpendPurpose(ctx, threadUtxo, `${FAMILY} step 01`);
    const inputIndex = requireInputIndex(ctx, threadUtxo, `${FAMILY} step 01`);
    outputIndex = requireUniqueOutputIndex(
      ctx.outputs,
      outputMatches,
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
                header,
                membership,
                direction: subject.direction,
              },
            },
            execution_index: executionIndex,
          },
        ],
      } as never,
      ExecutionNativeScriptInvalidStep01RedeemerSchema as never,
    );
  }) satisfies BuildTxWithRedeemer;
  signer.selectWallet(lucid);
  const txHash = await submitLinearFaultContinue({
    lucid,
    signerPaymentKeyHash: signer.paymentKeyHash,
    threadUtxo,
    threadUnit: threadToken.unit,
    stepReference,
    stepScript: contracts.steps[0].spendingScript,
    stepRole: `${FAMILY} step 01`,
    nextAddress: contracts.steps[1].spendingScriptAddress,
    nextDatum,
    redeemer,
    preSubmitBoundary,
    awaitConfirmation,
  });
  if (outputIndex === undefined)
    throw new Error(`${FAMILY}: unresolved layout`);
  return { txHash, nextThreadOutRef: `${txHash}#${outputIndex.toString()}` };
};
