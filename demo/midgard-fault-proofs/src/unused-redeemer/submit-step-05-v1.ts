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
import { advanceUnusedRedeemerSelectionsV1 } from "./checkpoint-v1.js";
import type { UnusedRedeemerContractsV1 } from "./contracts-v1.js";
import type { UnusedRedeemerEvidenceV1 } from "./family-v1.js";
import {
  UnusedRedeemerReverseScanV1Schema,
  UnusedRedeemerStep05DatumV1Schema,
  UnusedRedeemerStep05RedeemerV1Schema,
  UnusedRedeemerStep06DatumV1Schema,
} from "./schemas-v1.js";

const FAMILY = "unused-redeemer";
export const submitUnusedRedeemerStep05V1 = async ({
  lucid,
  contracts,
  categoryId,
  signer,
  threadOutRef,
  evidence,
  referenceScriptUtxo,
  itemBudget = 16,
  preSubmitBoundary,
  awaitConfirmation = true,
}: {
  lucid: LucidEvolution;
  contracts: UnusedRedeemerContractsV1;
  categoryId: string;
  signer: ResolvedProverSigner;
  threadOutRef: string;
  evidence: UnusedRedeemerEvidenceV1;
  referenceScriptUtxo: UTxO;
  itemBudget?: number;
  preSubmitBoundary?: FraudProofPreSubmitBoundaryV1;
  awaitConfirmation?: boolean;
}) => {
  const stepIndex = 7;
  const { threadUtxo, threadToken } = await requireLinearFaultThreadUtxoV1({
    lucid,
    contracts,
    categoryId,
    family: FAMILY,
    stepIndex,
    threadOutRef,
  });
  const source = requireLinearFaultStepStateV1<
    Data.Static<typeof UnusedRedeemerReverseScanV1Schema>
  >({
    threadUtxo,
    signer,
    schema: UnusedRedeemerStep05DatumV1Schema as never,
    family: FAMILY,
    stepIndex,
  });
  const start = Number(source.cursor);
  const selected = evidence.selections.slice(start, start + itemBudget);
  if (
    selected.length === 0 &&
    !source.used &&
    source.cursor < source.authenticated.purpose_count
  )
    throw new Error(`${FAMILY}: purpose batch is empty`);
  const nextState = advanceUnusedRedeemerSelectionsV1({
    state: source,
    evidence,
    itemBudget,
  });
  const complete =
    nextState.used ||
    nextState.cursor === nextState.authenticated.purpose_count;
  const nextAddress = complete
    ? contracts.steps[8].spendingScriptAddress
    : contracts.steps[7].spendingScriptAddress;
  const nextData = complete
    ? {
        subject: nextState.authenticated.bound.subject,
        redeemer_index: nextState.authenticated.bound.redeemer_index,
        unused: !nextState.used,
      }
    : nextState;
  const nextDatum = Data.to(
    { fraud_prover: signer.paymentKeyHash, data: nextData } as never,
    (complete
      ? UnusedRedeemerStep06DatumV1Schema
      : UnusedRedeemerStep05DatumV1Schema) as never,
  );
  const openings = selected.map((opening) => ({
    frontier_index: BigInt(opening.frontierIndex),
    purpose_kind: BigInt(opening.purposeKind),
    purpose_index: BigInt(opening.purposeIndex),
    script_hash: opening.scriptHashHex,
    purpose_subject: opening.purposeSubjectHex,
    purpose_siblings: opening.purposeMembership.siblings.map((value) =>
      Buffer.from(value).toString("hex"),
    ),
    language_tag: BigInt(opening.languageTag),
    source_leaf: opening.sourceLeafHex,
    redeemer_leaf: opening.redeemerLeafHex,
    execution_siblings: opening.executionMembership.siblings.map((value) =>
      Buffer.from(value).toString("hex"),
    ),
  }));
  const outputMatches = computationThreadOutputPredicate({
    address: nextAddress,
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
    requireOwnSpendPurpose(ctx, threadUtxo, `${FAMILY} step 05`);
    const inputIndex = requireInputIndex(ctx, threadUtxo, `${FAMILY} step 05`);
    outputIndex = requireUniqueOutputIndex(
      ctx.outputs,
      outputMatches,
      `${FAMILY} step 05`,
    );
    return Data.to(
      {
        Continue: [
          {
            input_index: inputIndex,
            output_index: outputIndex,
            openings,
            item_budget: BigInt(itemBudget),
          },
        ],
      } as never,
      UnusedRedeemerStep05RedeemerV1Schema as never,
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
    stepRole: `${FAMILY} step 05`,
    nextAddress,
    nextDatum,
    redeemer,
    preSubmitBoundary,
    awaitConfirmation,
  });
  if (outputIndex === undefined)
    throw new Error(`${FAMILY}: unresolved layout`);
  return {
    txHash,
    nextThreadOutRef: `${txHash}#${outputIndex.toString()}`,
    complete,
  };
};
