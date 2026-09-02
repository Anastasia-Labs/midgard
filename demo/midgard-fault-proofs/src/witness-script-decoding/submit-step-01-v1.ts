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
  type WitnessScriptDecodingBoundV1,
  WitnessScriptDecodingStep01RedeemerV1Schema,
  WitnessScriptDecodingStep02DatumV1Schema,
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
import {
  WITNESS_SCRIPT_DECODING_CATEGORY_LABEL as FAMILY,
  type WitnessScriptDecodingContractsV1,
} from "./contracts-v1.js";

const boundState = ({
  subject,
  witnessSetHash,
  scriptIndex,
  accusedClass,
}: {
  readonly subject: WitnessScriptDecodingBoundV1["subject"];
  readonly witnessSetHash: string;
  readonly scriptIndex: bigint;
  readonly accusedClass: bigint;
}): WitnessScriptDecodingBoundV1 => ({
  subject,
  witness_set_hash: witnessSetHash,
  script_index: scriptIndex,
  accused_class: accusedClass,
});

export const submitWitnessScriptDecodingStep01AcceptedV1 = async ({
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
    typeof submitMissingNativeScriptTxBindingV1
  >[0]["network"];
  readonly contracts: WitnessScriptDecodingContractsV1;
  readonly categoryId: string;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly stateQueueBlockOutRef: string;
  readonly txInclusion: SubmitStep01TxInclusion;
  readonly scriptIndex: bigint;
  readonly referenceScriptUtxo: UTxO;
  readonly witnessReferenceScripts?: FaultProofWitnessReferenceScriptsV1;
  readonly preSubmitBoundary?: FraudProofPreSubmitBoundaryV1;
  readonly awaitConfirmation?: boolean;
}) => {
  if (scriptIndex < 0n) throw new Error(`${FAMILY}: negative script index`);
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
        subject: acceptedVerdictSubjectV1(txInclusion.nativeTxId),
        witnessSetHash: txInclusion.nativeTx.witness_set_hash,
        scriptIndex,
        accusedClass: -1n,
      }),
    } as never,
    WitnessScriptDecodingStep02DatumV1Schema as never,
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
    spendRedeemerSchema: WitnessScriptDecodingStep01RedeemerV1Schema,
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

export const submitWitnessScriptDecodingStep01ForcedV1 = async ({
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
  readonly contracts: WitnessScriptDecodingContractsV1;
  readonly categoryId: string;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly header: HeaderV1;
  readonly membership: RootMembershipProof<
    OutputReference,
    ForcedInclusionTxV1
  >;
  readonly direction: bigint;
  readonly witnessSetHash: string;
  readonly scriptIndex: bigint;
  readonly referenceScriptUtxo: UTxO;
  readonly preSubmitBoundary?: FraudProofPreSubmitBoundaryV1;
  readonly awaitConfirmation?: boolean;
}) => {
  const stepIndex = 0;
  const { threadUtxo, threadToken } = await requireLinearFaultThreadUtxoV1({
    lucid,
    contracts,
    categoryId,
    family: FAMILY,
    stepIndex,
    threadOutRef,
  });
  requireLinearFaultInitialDatumV1({ threadUtxo, signer, family: FAMILY });
  const verdict = membership.value.verdict;
  const rejectionReason =
    verdict === "ForcedTxValid" ? null : verdict.ForcedTxInvalid.reason;
  const subject = forcedVerdictSubjectV1({
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
    WitnessScriptDecodingStep02DatumV1Schema as never,
  );
  const stepReference = requireLinearFaultReferenceScriptV1({
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
      WitnessScriptDecodingStep01RedeemerV1Schema as never,
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
