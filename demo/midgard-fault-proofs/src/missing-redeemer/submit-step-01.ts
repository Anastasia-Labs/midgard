import {
  acceptedVerdictSubject,
  type ForcedInclusionTxV1,
  forcedVerdictSubject,
  hashHexWithBlake2b,
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
import { Effect } from "effect";

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
import type { MissingRedeemerContracts } from "./contracts.js";
import {
  classifyMissingRedeemerFinding,
  type MissingRedeemerPurposeKind,
} from "./family.js";
import {
  MissingRedeemerStep01RedeemerSchema,
  MissingRedeemerStep02DatumSchema,
} from "./schemas.js";

const FAMILY = "missing-redeemer";
const bound = (
  subject: ReturnType<typeof acceptedVerdictSubject>,
  header: Header,
  witnessSetHash: string,
  kind: MissingRedeemerPurposeKind,
  index: number,
) => ({
  subject,
  witness_set_hash: witnessSetHash,
  validation_traces_root: header.validationTracesRoot,
  validation_trace_count: header.validationTraceCount,
  purpose_kind: BigInt(kind),
  purpose_index: BigInt(index),
});

export const submitMissingRedeemerStep01Accepted = async ({
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
  purposeKind,
  purposeIndex,
  referenceScriptUtxo,
  witnessReferenceScripts,
  preSubmitBoundary,
  awaitConfirmation = true,
}: {
  lucid: LucidEvolution;
  blueprint: unknown;
  network: Parameters<typeof submitMissingNativeScriptTxBinding>[0]["network"];
  contracts: MissingRedeemerContracts;
  categoryId: string;
  signer: ResolvedProverSigner;
  threadOutRef: string;
  stateQueueBlockOutRef: string;
  txInclusion: SubmitStep01TxInclusion;
  header: Header;
  purposeKind: MissingRedeemerPurposeKind;
  purposeIndex: number;
  referenceScriptUtxo: UTxO;
  witnessReferenceScripts?: FaultProofWitnessReferenceScripts;
  preSubmitBoundary?: FraudProofPreSubmitBoundary;
  awaitConfirmation?: boolean;
}) => {
  const subject = acceptedVerdictSubject(txInclusion.nativeTxId);
  const finding = classifyMissingRedeemerFinding({
    subject,
    purposeKind,
    purposeIndex,
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
      data: bound(
        subject,
        header,
        txInclusion.nativeTx.witness_set_hash,
        finding.purposeKind,
        finding.purposeIndex,
      ),
    } as never,
    MissingRedeemerStep02DatumSchema as never,
  );
  return await submitMissingNativeScriptTxBinding({
    lucid,
    blueprint,
    network,
    contracts: contracts as unknown as MissingNativeScriptTxContracts,
    signer,
    stepIndex: 0,
    threadUtxo,
    threadToken,
    stateQueueBlockOutRef,
    txInclusion,
    nextDatum,
    spendRedeemerSchema: MissingRedeemerStep01RedeemerSchema,
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

export const submitMissingRedeemerStep01Forced = async ({
  lucid,
  contracts,
  categoryId,
  signer,
  threadOutRef,
  header,
  membership,
  purposeKind,
  purposeIndex,
  referenceScriptUtxo,
  preSubmitBoundary,
  awaitConfirmation = true,
}: {
  lucid: LucidEvolution;
  contracts: MissingRedeemerContracts;
  categoryId: string;
  signer: ResolvedProverSigner;
  threadOutRef: string;
  header: Header;
  membership: RootMembershipProof<OutputReference, ForcedInclusionTxV1>;
  purposeKind: MissingRedeemerPurposeKind;
  purposeIndex: number;
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
  const finding = classifyMissingRedeemerFinding({
    subject,
    purposeKind,
    purposeIndex,
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
      data: bound(
        subject as ReturnType<typeof acceptedVerdictSubject>,
        header,
        Effect.runSync(
          hashHexWithBlake2b(
            membership.value.source.witness_set_compact_cbor,
            32,
          ),
        ),
        finding.purposeKind,
        finding.purposeIndex,
      ),
    } as never,
    MissingRedeemerStep02DatumSchema as never,
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
            purpose_kind: BigInt(finding.purposeKind),
            purpose_index: BigInt(finding.purposeIndex),
          },
        ],
      } as never,
      MissingRedeemerStep01RedeemerSchema as never,
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
