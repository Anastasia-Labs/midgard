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
} from "../linear-fault-family-v1.js";
import { submitLinearFaultContinue } from "../linear-fault-submit-v1.js";
import type { ResolvedProverSigner } from "../runtime.js";
import { computationThreadOutputPredicate } from "../tx-layout.js";
import type { FraudProofPreSubmitBoundary } from "../workflow/transaction-boundary-v1.js";
import type { MissingScriptSourceContracts } from "./contracts-v1.js";
import type { MissingScriptSourceEvidence } from "./family-v1.js";
import {
  AuthenticatedResolvedSourcesSchema,
  AuthenticatedTransactionSourcesSchema,
  ExecutionSourceScanStateSchema,
  ExecutionSourceStep04DatumSchema,
  ExecutionSourceStep04RedeemerSchema,
  ExecutionSourceStep05DatumSchema,
} from "./schemas-v1.js";
import {
  missingScriptSourceOnchainCheckpoint,
  missingScriptSourceOnchainSourceIdentity,
} from "./universe-scan-v1.js";

const FAMILY = "missing-script-source";

/** Authenticates the resolved-spend/reference partitions and opens the scan. */
export const submitMissingScriptSourceStep04 = async ({
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
  contracts: MissingScriptSourceContracts;
  categoryId: string;
  signer: ResolvedProverSigner;
  threadOutRef: string;
  evidence: MissingScriptSourceEvidence;
  referenceScriptUtxo: UTxO;
  preSubmitBoundary?: FraudProofPreSubmitBoundary;
  awaitConfirmation?: boolean;
}) => {
  const stepIndex = 3;
  const { threadUtxo, threadToken } = await requireLinearFaultThreadUtxo({
    lucid,
    contracts,
    categoryId,
    family: FAMILY,
    stepIndex,
    threadOutRef,
  });
  const staged = requireLinearFaultStepState<
    Data.Static<typeof AuthenticatedTransactionSourcesSchema>
  >({
    threadUtxo,
    signer,
    schema: ExecutionSourceStep04DatumSchema as never,
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
  const sourceIdentity = missingScriptSourceOnchainSourceIdentity({
    priorLedgerRootHex: staged.purpose.prior_ledger_root,
    sourceCount: staged.purpose.source_count,
    scanLimit: staged.purpose.scan_limit,
    sourcePeaks: staged.purpose.source_peaks,
    transactionSourceCount: staged.transaction_source_count,
    resolvedReferenceSourceCount,
  });
  const authenticated: Data.Static<typeof AuthenticatedResolvedSourcesSchema> =
    {
      purpose: staged.purpose,
      transaction_source_count: staged.transaction_source_count,
      resolved_reference_source_count: resolvedReferenceSourceCount,
      source_identity_hash: sourceIdentity,
    };
  const nextExpectedScriptHash = contracts.steps[4].spendingScriptHash;
  const nextState: Data.Static<typeof ExecutionSourceScanStateSchema> = {
    authenticated,
    cursor: 0n,
    found: false,
    next_expected_script_hash: nextExpectedScriptHash,
    checkpoint_hash: missingScriptSourceOnchainCheckpoint({
      sourceIdentityHex: sourceIdentity,
      cursor: 0n,
      found: false,
      nextExpectedScriptHashHex: nextExpectedScriptHash,
    }),
  };
  const nextDatum = Data.to(
    { fraud_prover: signer.paymentKeyHash, data: nextState } as never,
    ExecutionSourceStep05DatumSchema as never,
  );
  const outputMatches = computationThreadOutputPredicate({
    address: contracts.steps[4].spendingScriptAddress,
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
      ExecutionSourceStep04RedeemerSchema as never,
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
