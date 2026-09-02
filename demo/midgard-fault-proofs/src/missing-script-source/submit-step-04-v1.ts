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
  AuthenticatedResolvedSourcesV1Schema,
  AuthenticatedTransactionSourcesV1Schema,
  ExecutionSourceScanStateV1Schema,
  ExecutionSourceStep04DatumV1Schema,
  ExecutionSourceStep04RedeemerV1Schema,
  ExecutionSourceStep05DatumV1Schema,
} from "./schemas-v1.js";
import {
  missingScriptSourceOnchainCheckpointV1,
  missingScriptSourceOnchainSourceIdentityV1,
} from "./universe-scan-v1.js";

const FAMILY = "missing-script-source";

/** Authenticates the resolved-spend/reference partitions and opens the scan. */
export const submitMissingScriptSourceStep04V1 = async ({
  lucid,
  contracts,
  categoryId,
  signer,
  threadOutRef,
  evidence,
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
  referenceScriptUtxo: UTxO;
  preSubmitBoundary?: FraudProofPreSubmitBoundaryV1;
  awaitConfirmation?: boolean;
}) => {
  const stepIndex = 3;
  const { threadUtxo, threadToken } = await requireLinearFaultThreadUtxoV1({
    lucid,
    contracts,
    categoryId,
    family: FAMILY,
    stepIndex,
    threadOutRef,
  });
  const staged = requireLinearFaultStepStateV1<
    Data.Static<typeof AuthenticatedTransactionSourcesV1Schema>
  >({
    threadUtxo,
    signer,
    schema: ExecutionSourceStep04DatumV1Schema as never,
    family: FAMILY,
    stepIndex,
  });
  const transactionCount = Number(staged.transaction_source_count);
  const referenceCount = evidence.sources.filter(
    ({ originKind }) => originKind === 1,
  ).length;
  if (
    transactionCount + referenceCount !== evidence.sources.length ||
    evidence.sources.some(({ originKind }, index) =>
      index < transactionCount ? originKind !== 0 : originKind !== 1,
    )
  )
    throw new Error(
      `${FAMILY}: resolved source partitions are incomplete or reordered`,
    );
  const resolvedReferenceSourceCount = BigInt(referenceCount);
  const sourceIdentity = missingScriptSourceOnchainSourceIdentityV1({
    priorLedgerRootHex: staged.purpose.prior_ledger_root,
    sourceCount: staged.purpose.source_count,
    scanLimit: staged.purpose.scan_limit,
    sourcePeaks: staged.purpose.source_peaks,
    transactionSourceCount: staged.transaction_source_count,
    resolvedReferenceSourceCount,
  });
  const authenticated: Data.Static<
    typeof AuthenticatedResolvedSourcesV1Schema
  > = {
    purpose: staged.purpose,
    transaction_source_count: staged.transaction_source_count,
    resolved_reference_source_count: resolvedReferenceSourceCount,
    source_identity_hash: sourceIdentity,
  };
  const nextExpectedScriptHash = contracts.steps[4].spendingScriptHash;
  const nextState: Data.Static<typeof ExecutionSourceScanStateV1Schema> = {
    authenticated,
    cursor: 0n,
    found: false,
    next_expected_script_hash: nextExpectedScriptHash,
    checkpoint_hash: missingScriptSourceOnchainCheckpointV1({
      sourceIdentityHex: sourceIdentity,
      cursor: 0n,
      found: false,
      nextExpectedScriptHashHex: nextExpectedScriptHash,
    }),
  };
  const nextDatum = Data.to(
    { fraud_prover: signer.paymentKeyHash, data: nextState } as never,
    ExecutionSourceStep05DatumV1Schema as never,
  );
  const outputMatches = computationThreadOutputPredicate({
    address: contracts.steps[4].spendingScriptAddress,
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
    requireOwnSpendPurpose(ctx, threadUtxo, `${FAMILY} step 04`);
    const inputIndex = requireInputIndex(ctx, threadUtxo, `${FAMILY} step 04`);
    outputIndex = requireUniqueOutputIndex(
      ctx.outputs,
      outputMatches,
      `${FAMILY} step 04`,
    );
    return Data.to(
      {
        Continue: [
          {
            input_index: inputIndex,
            output_index: outputIndex,
            resolved_reference_source_count: resolvedReferenceSourceCount,
          },
        ],
      } as never,
      ExecutionSourceStep04RedeemerV1Schema as never,
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
    stepRole: `${FAMILY} step 04`,
    nextAddress: contracts.steps[4].spendingScriptAddress,
    nextDatum,
    redeemer,
    preSubmitBoundary,
    awaitConfirmation,
  });
  if (outputIndex === undefined)
    throw new Error(`${FAMILY}: unresolved layout`);
  return { txHash, nextThreadOutRef: `${txHash}#${outputIndex.toString()}` };
};
