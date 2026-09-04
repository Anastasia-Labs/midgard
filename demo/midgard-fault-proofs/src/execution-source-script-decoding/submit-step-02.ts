import {
  requireInputIndex,
  requireOwnSpendPurpose,
  requireUniqueOutputIndex,
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
} from "../linear-fault-family.js";
import { submitLinearFaultContinue } from "../linear-fault-submit.js";
import type { ResolvedProverSigner } from "../runtime.js";
import { computationThreadOutputPredicate } from "../tx-layout.js";
import type { FraudProofPreSubmitBoundary } from "../workflow/transaction-boundary.js";
import type { ExecutionSourceScriptDecodingContracts } from "./contracts.js";
import type { ExecutionSourceScriptDecodingEvidence } from "./family.js";
import {
  AuthenticatedExecutionSourceSchema,
  ExecutionSourceBoundSchema,
  ExecutionSourceStep02DatumSchema,
  ExecutionSourceStep02RedeemerSchema,
  ExecutionSourceStep03DatumSchema,
} from "./schemas.js";

const FAMILY = "execution-source-script-decoding";
export type ExecutionSourceAuthenticationData = Omit<
  Extract<
    Data.Static<typeof ExecutionSourceStep02RedeemerSchema>,
    { Continue: unknown }
  >["Continue"][0],
  "input_index" | "output_index"
>;

export const submitExecutionSourceScriptDecodingStep02 = async ({
  lucid,
  contracts,
  categoryId,
  signer,
  threadOutRef,
  evidence,
  authentication,
  referenceScriptUtxo,
  preSubmitBoundary,
  awaitConfirmation = true,
}: {
  lucid: LucidEvolution;
  contracts: ExecutionSourceScriptDecodingContracts;
  categoryId: string;
  signer: ResolvedProverSigner;
  threadOutRef: string;
  evidence: ExecutionSourceScriptDecodingEvidence;
  authentication: ExecutionSourceAuthenticationData;
  referenceScriptUtxo: UTxO;
  preSubmitBoundary?: FraudProofPreSubmitBoundary;
  awaitConfirmation?: boolean;
}) => {
  const stepIndex = 1;
  const { threadUtxo, threadToken } = await requireLinearFaultThreadUtxo({
    lucid,
    contracts,
    categoryId,
    family: FAMILY,
    stepIndex,
    threadOutRef,
  });
  const bound = requireLinearFaultStepState<
    Data.Static<typeof ExecutionSourceBoundSchema>
  >({
    threadUtxo,
    signer,
    schema: ExecutionSourceStep02DatumSchema as never,
    family: FAMILY,
    stepIndex,
  });
  if (
    bound.subject.transaction_id !== evidence.finding.subject.transaction_id ||
    bound.execution_index !== BigInt(evidence.finding.executionIndex) ||
    authentication.execution_siblings.length !==
      evidence.descriptor.executionMembership.siblings.length
  )
    throw new Error(
      `${FAMILY}: retained execution authentication differs from bound subject`,
    );
  const source: Data.Static<typeof AuthenticatedExecutionSourceSchema> = {
    bound,
    prior_ledger_root: authentication.machine_state.prior_ledger_root,
    source_index: authentication.source_index,
    origin_kind: authentication.origin_kind,
    source_key: authentication.source_key,
    language_tag: authentication.language_tag,
    script_hash: authentication.script_hash,
    total_length: authentication.total_length,
    item_commitment: authentication.item_commitment,
  };
  if (
    source.item_commitment !== evidence.itemCommitmentHex ||
    source.total_length !== BigInt(evidence.itemLength) ||
    source.source_index !== BigInt(evidence.descriptor.sourceIndex)
  )
    throw new Error(
      `${FAMILY}: authenticated source descriptor was substituted`,
    );
  const nextDatum = Data.to(
    { fraud_prover: signer.paymentKeyHash, data: source } as never,
    ExecutionSourceStep03DatumSchema as never,
  );
  const outputMatches = computationThreadOutputPredicate({
    address: contracts.steps[2].spendingScriptAddress,
    datum: nextDatum,
    unit: threadToken.unit,
  });
  const stepReference = requireLinearFaultReferenceScript({
    utxo: referenceScriptUtxo,
    expectedScriptHash: contracts.steps[stepIndex].spendingScriptHash,
    family: FAMILY,
    stepIndex,
  });
  let outputIndex: bigint | undefined;
  const redeemer = ((ctx) => {
    requireOwnSpendPurpose(ctx, threadUtxo, `${FAMILY} step 02`);
    const inputIndex = requireInputIndex(ctx, threadUtxo, `${FAMILY} step 02`);
    outputIndex = requireUniqueOutputIndex(
      ctx.outputs,
      outputMatches,
      `${FAMILY} step 02`,
    );
    return Data.to(
      {
        Continue: [
          {
            ...authentication,
            input_index: inputIndex,
            output_index: outputIndex,
          },
        ],
      } as never,
      ExecutionSourceStep02RedeemerSchema as never,
    );
  }) satisfies BuildTxWithRedeemer;
  signer.selectWallet(lucid);
  const txHash = await submitLinearFaultContinue({
    lucid,
    signerPaymentKeyHash: signer.paymentKeyHash,
    threadUtxo,
    threadUnit: threadToken.unit,
    stepReference,
    stepScript: contracts.steps[stepIndex].spendingScript,
    stepRole: `${FAMILY} step 02`,
    nextAddress: contracts.steps[2].spendingScriptAddress,
    nextDatum,
    redeemer,
    preSubmitBoundary,
    awaitConfirmation,
  });
  if (outputIndex === undefined)
    throw new Error(`${FAMILY}: unresolved layout`);
  return { txHash, nextThreadOutRef: `${txHash}#${outputIndex.toString()}` };
};
