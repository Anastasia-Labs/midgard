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
import type { ExecutionNativeScriptInvalidContractsV1 } from "./contracts-v1.js";
import type { ExecutionNativeScriptInvalidEvidenceV1 } from "./family-v1.js";
import {
  ExecutionNativeScriptInvalidBoundV1Schema,
  ExecutionNativeScriptInvalidSourceV1Schema,
  ExecutionNativeScriptInvalidStep02DatumV1Schema,
  ExecutionNativeScriptInvalidStep02RedeemerV1Schema,
  ExecutionNativeScriptInvalidStep03DatumV1Schema,
} from "./schemas-v1.js";

const FAMILY = "execution-native-script-invalid";
export type ExecutionSourceAuthenticationDataV1 = Omit<
  Extract<
    Data.Static<typeof ExecutionNativeScriptInvalidStep02RedeemerV1Schema>,
    { Continue: unknown }
  >["Continue"][0],
  "input_index" | "output_index"
>;

export const submitExecutionNativeScriptInvalidStep02V1 = async ({
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
  contracts: ExecutionNativeScriptInvalidContractsV1;
  categoryId: string;
  signer: ResolvedProverSigner;
  threadOutRef: string;
  evidence: ExecutionNativeScriptInvalidEvidenceV1;
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
    Data.Static<typeof ExecutionNativeScriptInvalidBoundV1Schema>
  >({
    threadUtxo,
    signer,
    schema: ExecutionNativeScriptInvalidStep02DatumV1Schema as never,
    family: FAMILY,
    stepIndex,
  });
  if (
    bound.subject.transaction_id !== evidence.finding.subject.transaction_id ||
    bound.execution_index !== BigInt(evidence.finding.executionIndex)
  )
    throw new Error(
      `${FAMILY}: retained execution authentication differs from bound subject`,
    );
  if (
    authentication.machine_state.prior_ledger_root !== bound.prior_ledger_root
  )
    throw new Error(`${FAMILY}: authenticated prior-ledger root changed`);
  if (authentication.control.compact_cbor !== bound.compact_cbor)
    throw new Error(`${FAMILY}: authenticated compact transaction changed`);
  const source: Data.Static<typeof ExecutionNativeScriptInvalidSourceV1Schema> =
    {
      bound,
      prior_ledger_root: authentication.machine_state.prior_ledger_root,
      source_index: authentication.source_index,
      origin_kind: authentication.origin_kind,
      source_key: authentication.source_key,
      language_tag: authentication.language_tag,
      script_hash: authentication.script_hash,
      total_length: authentication.total_length,
      item_commitment: authentication.item_commitment,
      compact_cbor: authentication.control.compact_cbor,
    };
  if (
    source.bound.subject.transaction_id !==
    evidence.authenticated.transactionIdHex
  )
    throw new Error(
      `${FAMILY}: authenticated source descriptor was substituted`,
    );
  const nextDatum = Data.to(
    { fraud_prover: signer.paymentKeyHash, data: source } as never,
    ExecutionNativeScriptInvalidStep03DatumV1Schema as never,
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
            ...authentication,
            input_index: inputIndex,
            output_index: outputIndex,
          },
        ],
      } as never,
      ExecutionNativeScriptInvalidStep02RedeemerV1Schema as never,
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
