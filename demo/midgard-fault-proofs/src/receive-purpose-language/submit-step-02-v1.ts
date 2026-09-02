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
import type { ReceivePurposeLanguageContractsV1 } from "./contracts-v1.js";
import type { ReceivePurposeLanguageEvidenceV1 } from "./family-v1.js";
import {
  AuthenticatedReceiveLanguageV1Schema,
  ReceivePurposeBoundExecutionV1Schema,
  ReceivePurposeStep02DatumV1Schema,
  ReceivePurposeStep02RedeemerV1Schema,
  ReceivePurposeStep03DatumV1Schema,
} from "./schemas-v1.js";

const FAMILY = "receive-purpose-language";
export type ReceivePurposeLanguageAuthenticationV1 = Omit<
  Extract<
    Data.Static<typeof ReceivePurposeStep02RedeemerV1Schema>,
    { Continue: unknown }
  >["Continue"][0],
  "input_index" | "output_index"
>;
export const submitReceivePurposeLanguageStep02V1 = async ({
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
  contracts: ReceivePurposeLanguageContractsV1;
  categoryId: string;
  signer: ResolvedProverSigner;
  threadOutRef: string;
  evidence: ReceivePurposeLanguageEvidenceV1;
  authentication: ReceivePurposeLanguageAuthenticationV1;
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
    Data.Static<typeof ReceivePurposeBoundExecutionV1Schema>
  >({
    threadUtxo,
    signer,
    schema: ReceivePurposeStep02DatumV1Schema as never,
    family: FAMILY,
    stepIndex,
  });
  if (
    bound.subject.transaction_id !== evidence.finding.subject.transaction_id ||
    bound.execution_index !== BigInt(evidence.finding.executionIndex) ||
    authentication.purpose_index !== BigInt(evidence.descriptor.purposeIndex) ||
    authentication.language_tag !== BigInt(evidence.descriptor.languageTag) ||
    authentication.script_hash !== evidence.descriptor.scriptHashHex
  )
    throw new Error(
      `${FAMILY}: retained purpose/language authentication differs from bound evidence`,
    );
  const authenticated: Data.Static<
    typeof AuthenticatedReceiveLanguageV1Schema
  > = {
    bound,
    prior_ledger_root: authentication.machine_state.prior_ledger_root,
    purpose_kind: 3n,
    purpose_index: authentication.purpose_index,
    source_index: authentication.source_index,
    origin_kind: authentication.origin_kind,
    source_key: authentication.source_key,
    language_tag: authentication.language_tag,
    script_hash: authentication.script_hash,
  };
  const nextDatum = Data.to(
    { fraud_prover: signer.paymentKeyHash, data: authenticated } as never,
    ReceivePurposeStep03DatumV1Schema as never,
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
      ReceivePurposeStep02RedeemerV1Schema as never,
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
