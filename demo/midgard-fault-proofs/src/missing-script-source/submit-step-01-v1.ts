import {
  acceptedVerdictSubjectV1,
  type ForcedInclusionTxV1,
  forcedVerdictSubjectV1,
  hashHexWithBlake2b,
  type HeaderV1,
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
import { Effect } from "effect";

import {
  requireLinearFaultInitialDatumV1,
  requireLinearFaultReferenceScriptV1,
  requireLinearFaultThreadUtxoV1,
} from "../linear-fault-family-v1.js";
import { submitLinearFaultContinueV1 } from "../linear-fault-submit-v1.js";
import type { MissingNativeScriptTxContractsV1 } from "../missing-native-script-tx/contracts-v1.js";
import { submitMissingNativeScriptTxBindingV1 } from "../missing-native-script-tx/submit-native-binding-v1.js";
import type { ResolvedProverSigner } from "../runtime.js";
import type { SubmitStep01TxInclusion } from "../submit-step-01.js";
import { computationThreadOutputPredicate } from "../tx-layout.js";
import type { FaultProofWitnessReferenceScriptsV1 } from "../witness-reference-scripts-v1.js";
import type { FraudProofPreSubmitBoundaryV1 } from "../workflow/transaction-boundary-v1.js";
import type { MissingScriptSourceContractsV1 } from "./contracts-v1.js";
import {
  classifyMissingScriptSourceFindingV1,
  type MissingScriptSourceFindingV1,
} from "./family-v1.js";
import {
  ExecutionSourceStep01RedeemerV1Schema,
  ExecutionSourceStep02DatumV1Schema,
} from "./schemas-v1.js";

const FAMILY = "missing-script-source";
const boundState = ({
  subject,
  header,
  purposeKind,
  purposeIndex,
  witnessSetHash,
}: {
  subject: MissingScriptSourceFindingV1["subject"];
  header: HeaderV1;
  purposeKind: 0 | 1 | 2 | 3;
  purposeIndex: bigint;
  witnessSetHash: string;
}) => ({
  subject,
  validation_traces_root: header.validationTracesRoot,
  validation_trace_count: header.validationTraceCount,
  witness_set_hash: witnessSetHash,
  purpose_kind: BigInt(purposeKind),
  purpose_index: purposeIndex,
});

export const submitMissingScriptSourceStep01AcceptedV1 = async ({
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
  purposeKind,
  purposeIndex,
  referenceScriptUtxo,
  witnessReferenceScripts,
  preSubmitBoundary,
  awaitConfirmation = true,
}: {
  lucid: LucidEvolution;
  blueprint: unknown;
  network: Parameters<
    typeof submitMissingNativeScriptTxBindingV1
  >[0]["network"];
  contracts: MissingScriptSourceContractsV1;
  categoryId: string;
  signer: ResolvedProverSigner;
  threadOutRef: string;
  stateQueueBlockOutRef: string;
  txInclusion: SubmitStep01TxInclusion;
  header: HeaderV1;
  executionIndex: bigint;
  purposeKind: 0 | 1 | 2 | 3;
  purposeIndex: bigint;
  referenceScriptUtxo: UTxO;
  witnessReferenceScripts?: FaultProofWitnessReferenceScriptsV1;
  preSubmitBoundary?: FraudProofPreSubmitBoundaryV1;
  awaitConfirmation?: boolean;
}) => {
  const subject = acceptedVerdictSubjectV1(txInclusion.nativeTxId);
  const finding = classifyMissingScriptSourceFindingV1({
    subject,
    purposeKind,
    purposeIndex: Number(purposeIndex),
    executionIndex: Number(executionIndex),
  });
  const { threadUtxo, threadToken } = await requireLinearFaultThreadUtxoV1({
    lucid,
    contracts,
    categoryId,
    family: FAMILY,
    stepIndex: 0,
    threadOutRef,
  });
  requireLinearFaultInitialDatumV1({ threadUtxo, signer, family: FAMILY });
  const nextDatum = Data.to(
    {
      fraud_prover: signer.paymentKeyHash,
      data: boundState({
        subject,
        header,
        purposeKind: finding.purposeKind,
        purposeIndex: BigInt(finding.purposeIndex),
        witnessSetHash: txInclusion.nativeTx.witness_set_hash,
      }),
    } as never,
    ExecutionSourceStep02DatumV1Schema as never,
  );
  return await submitMissingNativeScriptTxBindingV1({
    lucid,
    blueprint,
    network,
    contracts: contracts as unknown as MissingNativeScriptTxContractsV1,
    signer,
    stepIndex: 0,
    threadUtxo,
    threadToken,
    stateQueueBlockOutRef,
    txInclusion,
    nextDatum,
    spendRedeemerSchema: ExecutionSourceStep01RedeemerV1Schema,
    wrapInclusionArgs: (args) => ({
      source: {
        AcceptedSource: { inclusion: { RedeemerCarriedInclusion: [args] } },
      },
      purpose_kind: BigInt(finding.purposeKind),
      purpose_index: BigInt(finding.purposeIndex),
    }),
    referenceScriptUtxo,
    witnessReferenceScripts,
    preSubmitBoundary,
    awaitConfirmation,
  });
};

export const submitMissingScriptSourceStep01ForcedV1 = async ({
  lucid,
  contracts,
  categoryId,
  signer,
  threadOutRef,
  header,
  membership,
  executionIndex,
  purposeKind,
  purposeIndex,
  referenceScriptUtxo,
  preSubmitBoundary,
  awaitConfirmation = true,
}: {
  lucid: LucidEvolution;
  contracts: MissingScriptSourceContractsV1;
  categoryId: string;
  signer: ResolvedProverSigner;
  threadOutRef: string;
  header: HeaderV1;
  membership: RootMembershipProof<OutputReference, ForcedInclusionTxV1>;
  executionIndex: bigint;
  purposeKind: 0 | 1 | 2 | 3;
  purposeIndex: bigint;
  referenceScriptUtxo: UTxO;
  preSubmitBoundary?: FraudProofPreSubmitBoundaryV1;
  awaitConfirmation?: boolean;
}) => {
  const verdict = membership.value.verdict;
  if (verdict === "ForcedTxValid")
    throw new Error(`${FAMILY}: forced-valid leaf has no rejection`);
  const subject = forcedVerdictSubjectV1({
    transactionId: membership.value.tx_id,
    sourceKey: membership.key,
    rejectionReason: verdict.ForcedTxInvalid.reason,
  });
  const finding = classifyMissingScriptSourceFindingV1({
    subject,
    purposeKind,
    purposeIndex: Number(purposeIndex),
    executionIndex: Number(executionIndex),
  });
  const { threadUtxo, threadToken } = await requireLinearFaultThreadUtxoV1({
    lucid,
    contracts,
    categoryId,
    family: FAMILY,
    stepIndex: 0,
    threadOutRef,
  });
  requireLinearFaultInitialDatumV1({ threadUtxo, signer, family: FAMILY });
  const nextDatum = Data.to(
    {
      fraud_prover: signer.paymentKeyHash,
      data: boundState({
        subject,
        header,
        purposeKind: finding.purposeKind,
        purposeIndex: BigInt(finding.purposeIndex),
        witnessSetHash: Effect.runSync(
          hashHexWithBlake2b(
            membership.value.source.witness_set_compact_cbor,
            32,
          ),
        ),
      }),
    } as never,
    ExecutionSourceStep02DatumV1Schema as never,
  );
  const stepReference = requireLinearFaultReferenceScriptV1({
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
            purpose_kind: BigInt(finding.purposeKind),
            purpose_index: BigInt(finding.purposeIndex),
          },
        ],
      } as never,
      ExecutionSourceStep01RedeemerV1Schema as never,
    );
  }) satisfies BuildTxWithRedeemer;
  signer.selectWallet(lucid);
  const txHash = await submitLinearFaultContinueV1({
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
