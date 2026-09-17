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
  type WitnessScriptDecodingBound,
  WitnessScriptDecodingStep01RedeemerSchema,
  WitnessScriptDecodingStep02DatumSchema,
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
import {
  WITNESS_SCRIPT_DECODING_CATEGORY_LABEL as FAMILY,
  type WitnessScriptDecodingContracts,
} from "./contracts.js";

const boundState = ({
  subject,
  witnessSetHash,
  scriptIndex,
  accusedClass,
}: {
  readonly subject: WitnessScriptDecodingBound["subject"];
  readonly witnessSetHash: string;
  readonly scriptIndex: bigint;
  readonly accusedClass: bigint;
}): WitnessScriptDecodingBound => ({
  subject,
  witness_set_hash: witnessSetHash,
  script_index: scriptIndex,
  accused_class: accusedClass,
});

export const submitWitnessScriptDecodingStep01Accepted = async ({
  lucid,
  blueprint,
  network,
  contracts,
  categoryId,
  signer,
  threadOutRef,
  stateQueueBlockOutRef,
  txInclusion,
  scriptIndex,
  referenceScriptUtxo,
  witnessReferenceScripts,
  preSubmitBoundary,
  awaitConfirmation = true,
}: {
  readonly lucid: LucidEvolution;
  readonly blueprint: unknown;
  readonly network: Parameters<
    typeof submitMissingNativeScriptTxBinding
  >[0]["network"];
  readonly contracts: WitnessScriptDecodingContracts;
  readonly categoryId: string;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly stateQueueBlockOutRef: string;
  readonly txInclusion: SubmitStep01TxInclusion;
  readonly scriptIndex: bigint;
  readonly referenceScriptUtxo: UTxO;
  readonly witnessReferenceScripts?: FaultProofWitnessReferenceScripts;
  readonly preSubmitBoundary?: FraudProofPreSubmitBoundary;
  readonly awaitConfirmation?: boolean;
}) => {
  if (scriptIndex < 0n) throw new Error(`${FAMILY}: negative script index`);
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
        subject: acceptedVerdictSubject(txInclusion.nativeTxId),
        witnessSetHash: txInclusion.nativeTx.witness_set_hash,
        scriptIndex,
        accusedClass: -1n,
      }),
    } as never,
    WitnessScriptDecodingStep02DatumSchema as never,
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
    spendRedeemerSchema: WitnessScriptDecodingStep01RedeemerSchema,
    wrapInclusionArgs: (args) => ({
      source: {
        AcceptedSource: {
          inclusion: { RedeemerCarriedInclusion: [args] },
        },
      },
      script_index: scriptIndex,
    }),
    referenceScriptUtxo,
    witnessReferenceScripts,
    preSubmitBoundary,
    awaitConfirmation,
  });
};

const accusedClassOf = (
  reason: ForcedInclusionTxV1["verdict"],
  scriptIndex: bigint,
): bigint => {
  if (reason === "ForcedTxValid") return -1n;
  const value = reason.ForcedTxInvalid.reason;
  const entries: readonly [string, bigint][] = [
    ["WitnessScriptHeaderMalformed", 0n],
    ["WitnessNativeScriptMalformed", 1n],
    ["WitnessNativeScriptNodeLimit", 2n],
    ["WitnessNativeScriptDepthLimit", 3n],
  ];
  for (const [key, result] of entries) {
    if (key in (value as object)) {
      const coordinate = (
        value as unknown as Record<string, { script_index: bigint }>
      )[key];
      if (coordinate?.script_index !== scriptIndex) {
        throw new Error(`${FAMILY}: rejection coordinate differs`);
      }
      return result;
    }
  }
  throw new Error(`${FAMILY}: forced reason is outside family`);
};

export const submitWitnessScriptDecodingStep01Forced = async ({
  lucid,
  contracts,
  categoryId,
  signer,
  threadOutRef,
  header,
  membership,
  direction,
  witnessSetHash,
  scriptIndex,
  referenceScriptUtxo,
  preSubmitBoundary,
  awaitConfirmation = true,
}: {
  readonly lucid: LucidEvolution;
  readonly contracts: WitnessScriptDecodingContracts;
  readonly categoryId: string;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly header: Header;
  readonly membership: RootMembershipProof<
    OutputReference,
    ForcedInclusionTxV1
  >;
  readonly direction: bigint;
  readonly witnessSetHash: string;
  readonly scriptIndex: bigint;
  readonly referenceScriptUtxo: UTxO;
  readonly preSubmitBoundary?: FraudProofPreSubmitBoundary;
  readonly awaitConfirmation?: boolean;
}) => {
  const stepIndex = 0;
  const { threadUtxo, threadToken } = await requireLinearFaultThreadUtxo({
    lucid,
    contracts,
    categoryId,
    family: FAMILY,
    stepIndex,
    threadOutRef,
  });
  requireLinearFaultInitialDatum({ threadUtxo, signer, family: FAMILY });
  const verdict = membership.value.verdict;
  const rejectionReason =
    verdict === "ForcedTxValid" ? null : verdict.ForcedTxInvalid.reason;
  const subject = forcedVerdictSubject({
    transactionId: membership.value.tx_id,
    sourceKey: membership.key,
    rejectionReason,
  });
  if (subject.direction !== direction) {
    throw new Error(`${FAMILY}: direction differs from forced verdict`);
  }
  const nextDatum = Data.to(
    {
      fraud_prover: signer.paymentKeyHash,
      data: boundState({
        subject,
        witnessSetHash,
        scriptIndex,
        accusedClass: accusedClassOf(verdict, scriptIndex),
      }),
    } as never,
    WitnessScriptDecodingStep02DatumSchema as never,
  );
  const stepReference = requireLinearFaultReferenceScript({
    utxo: referenceScriptUtxo,
    expectedScriptHash: contracts.steps[0].spendingScriptHash,
    family: FAMILY,
    stepIndex,
  });
  const outputMatches = computationThreadOutputPredicate({
    address: contracts.steps[1].spendingScriptAddress,
    datum: nextDatum,
    unit: threadToken.unit,
  });
  let outputIndex: bigint | undefined;
  const redeemer = ((ctx) => {
    requireOwnSpendPurpose(ctx, threadUtxo, `${FAMILY} step 01 forced`);
    const inputIndex = requireInputIndex(
      ctx,
      threadUtxo,
      `${FAMILY} step 01 forced`,
    );
    outputIndex = requireUniqueOutputIndex(
      ctx.outputs,
      outputMatches,
      `${FAMILY} step 01 forced`,
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
                direction,
              },
            },
            script_index: scriptIndex,
          },
        ],
      } as never,
      WitnessScriptDecodingStep01RedeemerSchema as never,
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
    stepRole: `${FAMILY} step 01 forced`,
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
