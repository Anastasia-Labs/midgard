import {
  PROOF_THREAD_DIRECTION_WRONGFUL_ACCEPTANCE_V1,
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
  requireLinearFaultReferenceScriptV1,
  requireLinearFaultStepStateV1,
  requireLinearFaultThreadUtxoV1,
} from "../linear-fault-family-v1.js";
import { submitLinearFaultContinueV1 } from "../linear-fault-submit-v1.js";
import type { ResolvedProverSigner } from "../runtime.js";
import { computationThreadOutputPredicate } from "../tx-layout.js";
import type { FraudProofPreSubmitBoundaryV1 } from "../workflow/transaction-boundary-v1.js";
import type { MissingScriptSourceContractsV1 } from "./contracts-v1.js";
import type { MissingScriptSourceEvidenceV1 } from "./family-v1.js";
import {
  AuthenticatedExecutionSourceTraceV1Schema,
  ExecutionSourceBoundV1Schema,
  ExecutionSourceStep02DatumV1Schema,
  ExecutionSourceStep02RedeemerV1Schema,
  ExecutionSourceStep03DatumV1Schema,
} from "./schemas-v1.js";

const FAMILY = "missing-script-source";
type ExecutionSourceStep02AuthenticationV1 = Omit<
  Extract<
    Data.Static<typeof ExecutionSourceStep02RedeemerV1Schema>,
    { Continue: unknown }
  >["Continue"][0],
  "input_index" | "output_index" | "control"
>;
export type ExecutionSourceAuthenticationDataV1 =
  ExecutionSourceStep02AuthenticationV1 &
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
        typeof import("./schemas-v1.js").ScriptSourcesControlV1Schema
      >;
      control_data: Data;
    }>;

export const submitMissingScriptSourceStep02V1 = async ({
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
  contracts: MissingScriptSourceContractsV1;
  categoryId: string;
  signer: ResolvedProverSigner;
  threadOutRef: string;
  evidence: MissingScriptSourceEvidenceV1;
  authentication: ExecutionSourceAuthenticationDataV1;
  referenceScriptUtxo: UTxO;
  preSubmitBoundary?: FraudProofPreSubmitBoundaryV1;
  awaitConfirmation?: boolean;
}) => {
  const stepIndex = 1;
  const { threadUtxo, threadToken } = await requireLinearFaultThreadUtxoV1({
    lucid,
    contracts,
    categoryId,
    family: FAMILY,
    stepIndex,
    threadOutRef,
  });
  const bound = requireLinearFaultStepStateV1<
    Data.Static<typeof ExecutionSourceBoundV1Schema>
  >({
    threadUtxo,
    signer,
    schema: ExecutionSourceStep02DatumV1Schema as never,
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
    typeof AuthenticatedExecutionSourceTraceV1Schema
  > = {
    bound,
    machine_state: authentication.machine_state,
  };
  if (
    authentication.script_hash !== evidence.descriptor.scriptHashHex ||
    authentication.control.source_count < BigInt(evidence.sourceCount) ||
    (evidence.finding.subject.direction ===
    PROOF_THREAD_DIRECTION_WRONGFUL_ACCEPTANCE_V1
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
    ExecutionSourceStep03DatumV1Schema as never,
  );
  const outputMatches = computationThreadOutputPredicate({
    address: contracts.steps[2].spendingScriptAddress,
    datum: nextDatum,
    unit: threadToken.unit,
  });
  const stepReference = requireLinearFaultReferenceScriptV1({
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
      ExecutionSourceStep02RedeemerV1Schema as never,
    );
  }) satisfies BuildTxWithRedeemer;
  signer.selectWallet(lucid);
  const txHash = await submitLinearFaultContinueV1({
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
