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
import {
  advanceUnusedScriptWitnessSources,
  UNUSED_SCRIPT_WITNESS_MAXIMUM_SCAN_BATCH,
} from "./checkpoint.js";
import type { UnusedScriptWitnessContracts } from "./contracts.js";
import type { UnusedScriptWitnessEvidence } from "./family.js";
import {
  UnusedScriptReverseScanSchema,
  UnusedScriptStep04DatumSchema,
  UnusedScriptStep04RedeemerSchema,
  UnusedScriptStep05DatumSchema,
} from "./schemas.js";

const FAMILY = "unused-script-witness";

/**
 * One bounded batch of the alternate-source walk. The thread stays on the
 * step-04 script until every earlier inline source is authenticated, then
 * hands the same checkpointed state to step 05.
 */
export const submitUnusedScriptWitnessStep04 = async ({
  lucid,
  contracts,
  categoryId,
  signer,
  threadOutRef,
  evidence,
  referenceScriptUtxo,
  itemBudget = UNUSED_SCRIPT_WITNESS_MAXIMUM_SCAN_BATCH,
  preSubmitBoundary,
  awaitConfirmation = true,
}: {
  lucid: LucidEvolution;
  contracts: UnusedScriptWitnessContracts;
  categoryId: string;
  signer: ResolvedProverSigner;
  threadOutRef: string;
  evidence: UnusedScriptWitnessEvidence;
  referenceScriptUtxo: UTxO;
  itemBudget?: number;
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
  const source = requireLinearFaultStepState<
    Data.Static<typeof UnusedScriptReverseScanSchema>
  >({
    threadUtxo,
    signer,
    schema: UnusedScriptStep04DatumSchema as never,
    family: FAMILY,
    stepIndex,
  });
  const start = Number(source.alternate_cursor);
  const end = Math.min(start + itemBudget, evidence.finding.scriptIndex);
  const selected = evidence.sources.slice(start, end);
  if (selected.length === 0 && start < evidence.finding.scriptIndex)
    throw new Error(`${FAMILY}: alternate-source batch is empty`);
  const nextState = advanceUnusedScriptWitnessSources({
    state: source,
    evidence,
    itemBudget,
  });
  const complete =
    nextState.alternate_cursor === nextState.witness.bound.script_index;
  const nextAddress = complete
    ? contracts.steps[4].spendingScriptAddress
    : contracts.steps[3].spendingScriptAddress;
  const openings = selected.map((opening) => ({
    source_index: BigInt(opening.sourceIndex),
    language_tag: BigInt(opening.languageTag),
    script_hash: opening.scriptHashHex,
    total_length: BigInt(opening.scriptTotalLength),
    item_commitment: opening.itemCommitmentHex,
    siblings: opening.membership.siblings.map((value) =>
      Buffer.from(value).toString("hex"),
    ),
  }));
  const nextDatum = Data.to(
    { fraud_prover: signer.paymentKeyHash, data: nextState } as never,
    (complete
      ? UnusedScriptStep05DatumSchema
      : UnusedScriptStep04DatumSchema) as never,
  );
  const outputMatches = computationThreadOutputPredicate({
    address: nextAddress,
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
            openings,
            item_budget: BigInt(itemBudget),
          },
        ],
      } as never,
      UnusedScriptStep04RedeemerSchema as never,
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
