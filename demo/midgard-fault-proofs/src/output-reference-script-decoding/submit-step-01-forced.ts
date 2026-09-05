import {
  requireInputIndex,
  requireOwnSpendPurpose,
  requireUniqueOutputIndex,
  type VerdictSubject,
} from "@al-ft/midgard-sdk";
import {
  type BuildTxWithRedeemer,
  Data,
  type LucidEvolution,
  type UTxO,
} from "@lucid-evolution/lucid";

import {
  requireLinearFaultReferenceScript,
  requireLinearFaultThreadUtxo,
} from "../linear-fault-family.js";
import { submitLinearFaultContinue } from "../linear-fault-submit.js";
import type { ResolvedProverSigner } from "../runtime.js";
import { requireInitialStepDatum } from "../submit-step-01.js";
import { computationThreadOutputPredicate } from "../tx-layout.js";
import type { FraudProofPreSubmitBoundary } from "../workflow/transaction-boundary.js";
import {
  OUTPUT_REFERENCE_SCRIPT_DECODING_CATEGORY_LABEL as FAMILY,
  type OutputReferenceScriptDecodingContracts,
} from "./contracts.js";
import {
  classifyOutputReferenceScriptDecodingFinding,
  type OutputReferenceScriptDecodingEvidence,
} from "./output-reference-script-decoding.js";
import {
  OutputReferenceStep01RedeemerSchema,
  OutputReferenceStep02DatumSchema,
} from "./schemas.js";

/**
 * The forced-door transaction exactly as the redeemer and datum name it: no
 * off-chain classification, so a lifecycle suite can put a substituted
 * coordinate, reason, direction, header, or leaf in front of the validator
 * and observe the on-chain refusal. Production callers use the classified
 * entry point below.
 */
export const submitOutputReferenceScriptDecodingStep01ForcedRaw = async ({
  lucid,
  contracts,
  categoryId,
  signer,
  threadOutRef,
  bound,
  claimedOutputIndex = bound.outputIndex,
  forcedSource,
  referenceScriptUtxo,
  preSubmitBoundary,
  awaitConfirmation = true,
}: {
  readonly lucid: LucidEvolution;
  readonly contracts: OutputReferenceScriptDecodingContracts;
  readonly categoryId: string;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly bound: {
    readonly subject: VerdictSubject;
    readonly outputIndex: number;
    readonly accusedClass: number;
  };
  /** The redeemer's output coordinate; defaults to the datum's. */
  readonly claimedOutputIndex?: number;
  readonly forcedSource: Readonly<Record<string, unknown>>;
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
  requireInitialStepDatum({ threadUtxo, signer });
  signer.selectWallet(lucid);
  const stepReference = requireLinearFaultReferenceScript({
    utxo: referenceScriptUtxo,
    expectedScriptHash: contracts.steps[0].spendingScriptHash,
    family: FAMILY,
    stepIndex,
  });
  const nextDatum = Data.to(
    {
      fraud_prover: signer.paymentKeyHash,
      data: {
        subject: bound.subject,
        output_index: BigInt(bound.outputIndex),
        accused_class: BigInt(bound.accusedClass),
      },
    } as never,
    OutputReferenceStep02DatumSchema as never,
  );
  const outputMatches = computationThreadOutputPredicate({
    address: contracts.steps[1].spendingScriptAddress,
    datum: nextDatum,
    unit: threadToken.unit,
  });
  let outputIndex: bigint | undefined;
  const redeemer = ((ctx) => {
    requireOwnSpendPurpose(ctx, threadUtxo, `${FAMILY} forced step01`);
    const inputIndex = requireInputIndex(
      ctx,
      threadUtxo,
      `${FAMILY} forced step01`,
    );
    outputIndex = requireUniqueOutputIndex(
      ctx.outputs,
      outputMatches,
      `${FAMILY} forced step01 output`,
    );
    return Data.to(
      {
        Continue: [
          {
            source: {
              ForcedSource: {
                ...forcedSource,
                input_index: inputIndex,
                output_index: outputIndex,
              },
            },
            output_index: BigInt(claimedOutputIndex),
          },
        ],
      } as never,
      OutputReferenceStep01RedeemerSchema as never,
    );
  }) satisfies BuildTxWithRedeemer;
  const txHash = await submitLinearFaultContinue({
    lucid,
    signerPaymentKeyHash: signer.paymentKeyHash,
    threadUtxo,
    threadUnit: threadToken.unit,
    stepReference,
    stepScript: contracts.steps[0].spendingScript,
    stepRole: `${FAMILY} forced step01`,
    nextAddress: contracts.steps[1].spendingScriptAddress,
    nextDatum,
    redeemer,
    preSubmitBoundary,
    awaitConfirmation,
  });
  if (outputIndex === undefined)
    throw new Error(`${FAMILY}: forced step01 layout unresolved`);
  return { txHash, nextThreadOutRef: `${txHash}#${outputIndex.toString()}` };
};

export const submitOutputReferenceScriptDecodingStep01Forced = async ({
  evidence,
  ...rest
}: Omit<
  Parameters<typeof submitOutputReferenceScriptDecodingStep01ForcedRaw>[0],
  "bound" | "claimedOutputIndex"
> & {
  readonly evidence: OutputReferenceScriptDecodingEvidence;
}) => {
  classifyOutputReferenceScriptDecodingFinding(evidence);
  return await submitOutputReferenceScriptDecodingStep01ForcedRaw({
    ...rest,
    bound: {
      subject: evidence.subject,
      outputIndex: evidence.outputIndex,
      accusedClass: evidence.accusedClass,
    },
  });
};
