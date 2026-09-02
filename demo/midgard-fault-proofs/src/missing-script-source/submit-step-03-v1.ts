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
import { type MissingScriptSourceEvidenceV1 } from "./family-v1.js";
import {
  AuthenticatedExecutionSourceTraceV1Schema,
  AuthenticatedExecutionSourceV1Schema,
  AuthenticatedTransactionSourcesV1Schema,
  ExecutionSourceStep03DatumV1Schema,
  ExecutionSourceStep03RedeemerV1Schema,
  ExecutionSourceStep04DatumV1Schema,
} from "./schemas-v1.js";
import type { ExecutionSourceAuthenticationDataV1 } from "./submit-step-02-v1.js";

const FAMILY = "missing-script-source";
export const submitMissingScriptSourceStep03V1 = async ({
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
  const stepIndex = 2;
  const { threadUtxo, threadToken } = await requireLinearFaultThreadUtxoV1({
    lucid,
    contracts,
    categoryId,
    family: FAMILY,
    stepIndex,
    threadOutRef,
  });
  const authenticatedTrace = requireLinearFaultStepStateV1<
    Data.Static<typeof AuthenticatedExecutionSourceTraceV1Schema>
  >({
    threadUtxo,
    signer,
    schema: ExecutionSourceStep03DatumV1Schema as never,
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
    PROOF_THREAD_DIRECTION_WRONGFUL_ACCEPTANCE_V1
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
  const nextState: Data.Static<typeof AuthenticatedTransactionSourcesV1Schema> =
    {
      purpose: {
        bound: authenticatedTrace.bound,
        prior_ledger_root: authentication.machine_state.prior_ledger_root,
        required_script_hash: authentication.script_hash,
        source_count: authentication.control.source_count,
        scan_limit: BigInt(evidence.sources.length),
        source_peaks: authentication.control.source_peaks,
      } satisfies Data.Static<typeof AuthenticatedExecutionSourceV1Schema>,
      transaction_source_count: BigInt(count),
    };
  const nextDatum = Data.to(
    { fraud_prover: signer.paymentKeyHash, data: nextState } as never,
    ExecutionSourceStep04DatumV1Schema as never,
  );
  const outputMatches = computationThreadOutputPredicate({
    address: contracts.steps[3].spendingScriptAddress,
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
      ExecutionSourceStep03RedeemerV1Schema as never,
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
