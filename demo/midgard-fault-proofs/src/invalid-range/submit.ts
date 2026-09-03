import {
  InvalidRangeStep01SpendRedeemer,
  InvalidRangeStep02Datum,
  InvalidRangeStep02SpendRedeemer,
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
import { submitLinearFaultFinalize } from "../linear-fault-finalize.js";
import { submitLinearFaultContinue } from "../linear-fault-submit.js";
import type { ResolvedProverSigner } from "../runtime.js";
import { requireInitialStepDatum } from "../submit-step-01.js";
import { computationThreadOutputPredicate } from "../tx-layout.js";
import type { FaultProofWitnessReferenceScripts } from "../witness-reference-scripts.js";
import type { FraudProofPreSubmitBoundary } from "../workflow/transaction-boundary.js";
import type { InvalidRangeContracts } from "./contracts.js";
import {
  type InvalidRangeEvidence,
  invalidRangeEvidenceCloses,
} from "./family.js";

export const submitInvalidRangeStep01Forced = async ({
  lucid,
  contracts,
  categoryId,
  signer,
  threadOutRef,
  evidence,
  forcedSource,
  referenceScriptUtxo,
  preSubmitBoundary,
  awaitConfirmation = true,
}: {
  lucid: LucidEvolution;
  contracts: InvalidRangeContracts;
  categoryId: string;
  signer: ResolvedProverSigner;
  threadOutRef: string;
  evidence: InvalidRangeEvidence;
  forcedSource: Readonly<Record<string, unknown>>;
  referenceScriptUtxo: UTxO;
  preSubmitBoundary?: FraudProofPreSubmitBoundary;
  awaitConfirmation?: boolean;
}) => {
  if (evidence.subject.direction !== 1n)
    throw new Error("invalidRange: forced direction changed");
  const { threadUtxo, threadToken } = await requireLinearFaultThreadUtxo({
    lucid,
    contracts,
    categoryId,
    family: "invalid-range",
    stepIndex: 0,
    threadOutRef,
  });
  requireInitialStepDatum({ threadUtxo, signer });
  signer.selectWallet(lucid);
  const stepReference = requireLinearFaultReferenceScript({
    utxo: referenceScriptUtxo,
    expectedScriptHash: contracts.steps[0].spendingScriptHash,
    family: "invalid-range",
    stepIndex: 0,
  });
  const datum = Data.to(
    {
      fraud_prover: signer.paymentKeyHash,
      data: {
        subject: evidence.subject,
        block_slot: evidence.blockSlot,
        bad_tx_normalized_validity_range: evidence.normalizedRange,
      },
    } as never,
    InvalidRangeStep02Datum as never,
  );
  const outputMatches = computationThreadOutputPredicate({
    address: contracts.steps[1].spendingScriptAddress,
    datum,
    unit: threadToken.unit,
  });
  let outputIndex: bigint | undefined;
  const redeemer = ((ctx) => {
    requireOwnSpendPurpose(ctx, threadUtxo, "invalid-range forced step-01");
    outputIndex = requireUniqueOutputIndex(
      ctx.outputs,
      outputMatches,
      "invalid-range forced output",
    );
    return Data.to(
      {
        Continue: [
          {
            source: {
              ForcedSource: {
                ...forcedSource,
                input_index: requireInputIndex(
                  ctx,
                  threadUtxo,
                  "invalid-range",
                ),
                output_index: outputIndex,
              },
            },
          },
        ],
      } as never,
      InvalidRangeStep01SpendRedeemer as never,
    );
  }) satisfies BuildTxWithRedeemer;
  const txHash = await submitLinearFaultContinue({
    lucid,
    signerPaymentKeyHash: signer.paymentKeyHash,
    threadUtxo,
    threadUnit: threadToken.unit,
    stepReference,
    stepScript: contracts.steps[0].spendingScript,
    stepRole: "invalid-range step-01 forced",
    nextAddress: contracts.steps[1].spendingScriptAddress,
    nextDatum: datum,
    redeemer,
    preSubmitBoundary,
    awaitConfirmation,
  });
  if (outputIndex === undefined)
    throw new Error("invalidRange: layout unresolved");
  return { txHash, nextThreadOutRef: `${txHash}#${outputIndex.toString()}` };
};

export const submitInvalidRangeStep02V1 = async ({
  lucid,
  contracts,
  categoryId,
  signer,
  threadOutRef,
  evidence,
  referenceScriptUtxo,
  witnessReferenceScripts,
  preSubmitBoundary,
  awaitConfirmation = true,
}: {
  lucid: LucidEvolution;
  contracts: InvalidRangeContracts;
  categoryId: string;
  signer: ResolvedProverSigner;
  threadOutRef: string;
  evidence: InvalidRangeEvidence;
  referenceScriptUtxo: UTxO;
  witnessReferenceScripts: FaultProofWitnessReferenceScripts;
  preSubmitBoundary?: FraudProofPreSubmitBoundary;
  awaitConfirmation?: boolean;
}) => {
  if (!invalidRangeEvidenceCloses(evidence))
    throw new Error("invalidRange: terminal evidence is honest");
  const { threadUtxo, threadToken } = await requireLinearFaultThreadUtxo({
    lucid,
    contracts,
    categoryId,
    family: "invalid-range",
    stepIndex: 1,
    threadOutRef,
  });
  const state = requireLinearFaultStepState<{
    subject: { transaction_id: string };
  }>({
    threadUtxo,
    signer,
    schema: InvalidRangeStep02Datum as never,
    family: "invalid-range",
    stepIndex: 1,
  });
  if (state.subject.transaction_id !== evidence.subject.transaction_id)
    throw new Error("invalidRange: bound transaction changed");
  return await submitLinearFaultFinalize({
    lucid,
    family: "invalid-range",
    stepIndex: 1,
    step: contracts.steps[1],
    computationThread: contracts.computationThread,
    fraudProof: contracts.fraudProof,
    signer,
    threadUtxo,
    threadToken,
    spendRedeemerSchema: InvalidRangeStep02SpendRedeemer as never,
    buildFamilyArgs: ({
      inputIndex,
      outputIndex,
      fraudProofMintRedeemerIndex,
    }) => ({
      input_index: inputIndex,
      output_index: outputIndex,
      fraud_proof_mint_redeemer_index: fraudProofMintRedeemerIndex,
    }),
    referenceScriptUtxo,
    carriageUtxos: [],
    witnessReferenceScripts,
    preSubmitBoundary,
    awaitConfirmation,
  });
};
