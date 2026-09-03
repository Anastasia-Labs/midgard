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
import { type MissingScriptSourceEvidence } from "./family-v1.js";
import {
  AuthenticatedExecutionSourceSchema,
  AuthenticatedExecutionSourceTraceSchema,
  AuthenticatedTransactionSourcesSchema,
  ExecutionSourceStep03DatumSchema,
  ExecutionSourceStep03RedeemerSchema,
  ExecutionSourceStep04DatumSchema,
} from "./schemas-v1.js";
import type { ExecutionSourceAuthenticationData } from "./submit-step-02-v1.js";

const FAMILY = "missing-script-source";
export const submitMissingScriptSourceStep03 = async ({
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
  const stepIndex = 2;
  const { threadUtxo, threadToken } = await requireLinearFaultThreadUtxo({
    lucid,
    contracts,
    categoryId,
    family: FAMILY,
    stepIndex,
    threadOutRef,
  });
  const authenticatedTrace = requireLinearFaultStepState<
    Data.Static<typeof AuthenticatedExecutionSourceTraceSchema>
  >({
    threadUtxo,
    signer,
    schema: ExecutionSourceStep03DatumSchema as never,
    family: FAMILY,
    stepIndex,
  });
  if (
    authenticatedTrace.bound.subject.transaction_id !==
      evidence.finding.subject.transaction_id ||
    authentication.absolute_purpose_index !==
      BigInt(evidence.descriptor.purposeMembership.leafIndex) ||
    authentication.script_hash !== evidence.descriptor.scriptHashHex ||
    authentication.control.source_count < BigInt(evidence.sources.length) ||
    (evidence.finding.subject.direction ===
    PROOF_THREAD_DIRECTION_WRONGFUL_ACCEPTANCE
      ? authentication.control.source_count !== BigInt(evidence.sources.length)
      : authentication.control.discovery.source_cursor + 1n !==
        BigInt(evidence.sources.length))
  )
    throw new Error(
      `${FAMILY}: purpose frontier differs from retained evidence`,
    );
  const transactionSourceCount = evidence.sources.findIndex(
    ({ originKind }) => originKind !== 0,
  );
  const count =
    transactionSourceCount < 0
      ? evidence.sources.length
      : transactionSourceCount;
  if (evidence.sources.slice(count).some(({ originKind }) => originKind === 0))
    throw new Error(
      `${FAMILY}: transaction source partition is not contiguous`,
    );
  const nextState: Data.Static<typeof AuthenticatedTransactionSourcesSchema> = {
    purpose: {
      bound: authenticatedTrace.bound,
      prior_ledger_root: authentication.machine_state.prior_ledger_root,
      required_script_hash: authentication.script_hash,
      source_count: authentication.control.source_count,
      scan_limit: BigInt(evidence.sources.length),
      source_peaks: authentication.control.source_peaks,
    } satisfies Data.Static<typeof AuthenticatedExecutionSourceSchema>,
    transaction_source_count: BigInt(count),
  };
  const nextDatum = Data.to(
    { fraud_prover: signer.paymentKeyHash, data: nextState } as never,
    ExecutionSourceStep04DatumSchema as never,
  );
  const outputMatches = computationThreadOutputPredicate({
    address: contracts.steps[3].spendingScriptAddress,
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
    requireOwnSpendPurpose(ctx, threadUtxo, `${FAMILY} step 03`);
    const inputIndex = requireInputIndex(ctx, threadUtxo, `${FAMILY} step 03`);
    outputIndex = requireUniqueOutputIndex(
      ctx.outputs,
      outputMatches,
      `${FAMILY} step 03`,
    );
    return Data.to(
      {
        Continue: [
          {
            input_index: inputIndex,
            output_index: outputIndex,
            control: authentication.control_data,
            absolute_purpose_index: authentication.absolute_purpose_index,
            required_script_hash: authentication.script_hash,
            purpose_subject: authentication.purpose_subject,
            purpose_siblings: authentication.purpose_siblings,
            transaction_source_count: BigInt(count),
          },
        ],
      } as never,
      ExecutionSourceStep03RedeemerSchema as never,
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
    stepRole: `${FAMILY} step 03`,
    nextAddress: contracts.steps[3].spendingScriptAddress,
    nextDatum,
    redeemer,
    preSubmitBoundary,
    awaitConfirmation,
  });
  if (outputIndex === undefined)
    throw new Error(`${FAMILY}: unresolved layout`);
  return { txHash, nextThreadOutRef: `${txHash}#${outputIndex.toString()}` };
};
