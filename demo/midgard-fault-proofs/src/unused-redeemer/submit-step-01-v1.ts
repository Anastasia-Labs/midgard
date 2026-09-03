import {
  acceptedVerdictSubject,
  type ForcedInclusionTx,
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
} from "../linear-fault-family-v1.js";
import { submitLinearFaultContinue } from "../linear-fault-submit-v1.js";
import type { MissingNativeScriptTxContracts } from "../missing-native-script-tx/contracts-v1.js";
import { submitMissingNativeScriptTxBinding } from "../missing-native-script-tx/submit-native-binding-v1.js";
import type { ResolvedProverSigner } from "../runtime.js";
import type { SubmitStep01TxInclusion } from "../submit-step-01.js";
import { computationThreadOutputPredicate } from "../tx-layout.js";
import type { FaultProofWitnessReferenceScripts } from "../witness-reference-scripts-v1.js";
import type { FraudProofPreSubmitBoundary } from "../workflow/transaction-boundary-v1.js";
import type { UnusedRedeemerContracts } from "./contracts-v1.js";
import { classifyUnusedRedeemerFinding } from "./family-v1.js";
import {
  LegacyUnusedRedeemerStep01RedeemerSchema,
  LegacyUnusedRedeemerStep02DatumSchema,
} from "./schemas-v1.js";

const FAMILY = "unused-redeemer";
const state = (
  subject: ReturnType<typeof acceptedVerdictSubject>,
  header: Header,
  redeemerIndex: bigint,
) => ({
  subject,
  validation_traces_root: header.validationTracesRoot,
  validation_trace_count: header.validationTraceCount,
  redeemer_index: redeemerIndex,
});

export const submitUnusedRedeemerStep01Accepted = async ({
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
  redeemerIndex,
  referenceScriptUtxo,
  witnessReferenceScripts,
  preSubmitBoundary,
  awaitConfirmation = true,
}: {
  lucid: LucidEvolution;
  blueprint: unknown;
  network: Parameters<typeof submitMissingNativeScriptTxBinding>[0]["network"];
  contracts: UnusedRedeemerContracts;
  categoryId: string;
  signer: ResolvedProverSigner;
  threadOutRef: string;
  stateQueueBlockOutRef: string;
  txInclusion: SubmitStep01TxInclusion;
  header: Header;
  redeemerIndex: bigint;
  referenceScriptUtxo: UTxO;
  witnessReferenceScripts?: FaultProofWitnessReferenceScripts;
  preSubmitBoundary?: FraudProofPreSubmitBoundary;
  awaitConfirmation?: boolean;
}) => {
  const subject = acceptedVerdictSubject(txInclusion.nativeTxId);
  classifyUnusedRedeemerFinding({
    subject,
    redeemerIndex: Number(redeemerIndex),
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
      data: state(subject, header, redeemerIndex),
    } as never,
    LegacyUnusedRedeemerStep02DatumSchema as never,
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
    spendRedeemerSchema: LegacyUnusedRedeemerStep01RedeemerSchema,
    wrapInclusionArgs: (args) => ({
      source: {
        AcceptedSource: { inclusion: { RedeemerCarriedInclusion: [args] } },
      },
      redeemer_index: redeemerIndex,
    }),
    referenceScriptUtxo,
    witnessReferenceScripts,
    preSubmitBoundary,
    awaitConfirmation,
  });
};

export const submitUnusedRedeemerStep01Forced = async ({
  lucid,
  contracts,
  categoryId,
  signer,
  threadOutRef,
  header,
  membership,
  redeemerIndex,
  referenceScriptUtxo,
  preSubmitBoundary,
  awaitConfirmation = true,
}: {
  lucid: LucidEvolution;
  contracts: UnusedRedeemerContracts;
  categoryId: string;
  signer: ResolvedProverSigner;
  threadOutRef: string;
  header: Header;
  membership: RootMembershipProof<OutputReference, ForcedInclusionTx>;
  redeemerIndex: bigint;
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
  classifyUnusedRedeemerFinding({
    subject,
    redeemerIndex: Number(redeemerIndex),
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
      data: state(subject as never, header, redeemerIndex),
    } as never,
    LegacyUnusedRedeemerStep02DatumSchema as never,
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
            redeemer_index: redeemerIndex,
          },
        ],
      } as never,
      LegacyUnusedRedeemerStep01RedeemerSchema as never,
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
