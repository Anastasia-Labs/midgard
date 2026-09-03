import {
  PROOF_THREAD_DIRECTION_WRONGFUL_ACCEPTANCE,
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
} from "../linear-fault-family-v1.js";
import { submitLinearFaultContinue } from "../linear-fault-submit-v1.js";
import type { ResolvedProverSigner } from "../runtime.js";
import { computationThreadOutputPredicate } from "../tx-layout.js";
import type { FraudProofPreSubmitBoundary } from "../workflow/transaction-boundary-v1.js";
import type { MissingScriptSourceContracts } from "./contracts-v1.js";
import type { MissingScriptSourceEvidence } from "./family-v1.js";
import {
  AuthenticatedExecutionSourceTraceSchema,
  ExecutionSourceBoundSchema,
  ExecutionSourceStep02DatumSchema,
  ExecutionSourceStep02RedeemerSchema,
  ExecutionSourceStep03DatumSchema,
} from "./schemas-v1.js";

const FAMILY = "missing-script-source";
type ExecutionSourceStep02Authentication = Omit<
  Extract<
    Data.Static<typeof ExecutionSourceStep02RedeemerSchema>,
    { Continue: unknown }
  >["Continue"][0],
  "input_index" | "output_index" | "control"
>;
export type ExecutionSourceAuthenticationData =
  ExecutionSourceStep02Authentication &
    Readonly<{
      absolute_purpose_index: bigint;
      required_script_hash: string;
      purpose_kind: bigint;
      purpose_index: bigint;
      script_hash: string;
      purpose_subject: string;
      purpose_siblings: readonly string[];
      source_index: bigint;
      origin_kind: bigint;
      source_key: string;
      language_tag: bigint;
      total_length: bigint;
      item_commitment: string;
      source_siblings: readonly string[];
      redeemer_leaf: string;
      execution_siblings: readonly string[];
      control: Data.Static<
        typeof import("./schemas-v1.js").ScriptSourcesControlSchema
      >;
      control_data: Data;
    }>;

export const submitMissingScriptSourceStep02 = async ({
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
  contracts: MissingScriptSourceContracts;
  categoryId: string;
  signer: ResolvedProverSigner;
  threadOutRef: string;
  evidence: MissingScriptSourceEvidence;
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
    bound.purpose_kind !== BigInt(evidence.finding.purposeKind) ||
    bound.purpose_index !== BigInt(evidence.finding.purposeIndex) ||
    authentication.execution_siblings.length !==
      evidence.descriptor.executionMembership.siblings.length
  )
    throw new Error(
      `${FAMILY}: retained execution authentication differs from bound subject`,
    );
  const authenticatedTrace: Data.Static<
    typeof AuthenticatedExecutionSourceTraceSchema
  > = {
    bound,
    machine_state: authentication.machine_state,
  };
  if (
    authentication.script_hash !== evidence.descriptor.scriptHashHex ||
    authentication.control.source_count < BigInt(evidence.sourceCount) ||
    (evidence.finding.subject.direction ===
    PROOF_THREAD_DIRECTION_WRONGFUL_ACCEPTANCE
      ? authentication.control.source_count !== BigInt(evidence.sourceCount)
      : authentication.control.discovery.source_cursor + 1n !==
        BigInt(evidence.sourceCount)) ||
    authentication.purpose_kind !== BigInt(evidence.finding.purposeKind) ||
    authentication.purpose_index !== BigInt(evidence.finding.purposeIndex)
  )
    throw new Error(
      `${FAMILY}: authenticated source descriptor was substituted`,
    );
  const nextDatum = Data.to(
    { fraud_prover: signer.paymentKeyHash, data: authenticatedTrace } as never,
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
            trace_membership: authentication.trace_membership,
            machine_state: authentication.machine_state,
            trace_proof: authentication.trace_proof,
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
