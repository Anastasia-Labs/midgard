import {
  acceptedVerdictSubjectV1,
  type ForcedInclusionTxV1,
  forcedVerdictSubjectV1,
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
import type { ReceivePurposeLanguageContractsV1 } from "./contracts-v1.js";
import { classifyReceivePurposeLanguageFindingV1 } from "./family-v1.js";
import {
  ReceivePurposeStep01RedeemerV1Schema,
  ReceivePurposeStep02DatumV1Schema,
} from "./schemas-v1.js";

const FAMILY = "receive-purpose-language";
const state = (
  subject: ReturnType<typeof acceptedVerdictSubjectV1>,
  header: HeaderV1,
  executionIndex: bigint,
) => ({
  subject,
  validation_traces_root: header.validationTracesRoot,
  validation_trace_count: header.validationTraceCount,
  execution_index: executionIndex,
});

export const submitReceivePurposeLanguageStep01AcceptedV1 = async ({
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
  network: Parameters<
    typeof submitMissingNativeScriptTxBindingV1
  >[0]["network"];
  contracts: ReceivePurposeLanguageContractsV1;
  categoryId: string;
  signer: ResolvedProverSigner;
  threadOutRef: string;
  stateQueueBlockOutRef: string;
  txInclusion: SubmitStep01TxInclusion;
  header: HeaderV1;
  executionIndex: bigint;
  referenceScriptUtxo: UTxO;
  witnessReferenceScripts?: FaultProofWitnessReferenceScriptsV1;
  preSubmitBoundary?: FraudProofPreSubmitBoundaryV1;
  awaitConfirmation?: boolean;
}) => {
  const subject = acceptedVerdictSubjectV1(txInclusion.nativeTxId);
  classifyReceivePurposeLanguageFindingV1({
    subject,
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
      data: state(subject, header, executionIndex),
    } as never,
    ReceivePurposeStep02DatumV1Schema as never,
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
    spendRedeemerSchema: ReceivePurposeStep01RedeemerV1Schema,
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

export const submitReceivePurposeLanguageStep01ForcedV1 = async ({
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
  contracts: ReceivePurposeLanguageContractsV1;
  categoryId: string;
  signer: ResolvedProverSigner;
  threadOutRef: string;
  header: HeaderV1;
  membership: RootMembershipProof<OutputReference, ForcedInclusionTxV1>;
  executionIndex: bigint;
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
  classifyReceivePurposeLanguageFindingV1({
    subject,
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
      data: state(subject as never, header, executionIndex),
    } as never,
    ReceivePurposeStep02DatumV1Schema as never,
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
            execution_index: executionIndex,
          },
        ],
      } as never,
      ReceivePurposeStep01RedeemerV1Schema as never,
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
