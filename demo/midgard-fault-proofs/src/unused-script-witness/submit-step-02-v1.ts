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
import type { UnusedScriptWitnessContractsV1 } from "./contracts-v1.js";
import type { UnusedScriptWitnessEvidenceV1 } from "./family-v1.js";
import {
  UnusedScriptAuthenticatedWitnessV1Schema,
  UnusedScriptBoundWitnessV1Schema,
  UnusedScriptStep02DatumV1Schema,
  UnusedScriptStep02RedeemerV1Schema,
  UnusedScriptStep03DatumV1Schema,
} from "./schemas-v1.js";

const FAMILY = "unused-script-witness";
export type UnusedScriptWitnessAuthenticationV1 = Omit<
  Extract<
    Data.Static<typeof UnusedScriptStep02RedeemerV1Schema>,
    { Continue: unknown }
  >["Continue"][0],
  "input_index" | "output_index"
>;
export const submitUnusedScriptWitnessStep02V1 = async ({
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
  contracts: UnusedScriptWitnessContractsV1;
  categoryId: string;
  signer: ResolvedProverSigner;
  threadOutRef: string;
  evidence: UnusedScriptWitnessEvidenceV1;
  authentication: UnusedScriptWitnessAuthenticationV1;
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
    Data.Static<typeof UnusedScriptBoundWitnessV1Schema>
  >({
    threadUtxo,
    signer,
    schema: UnusedScriptStep02DatumV1Schema as never,
    family: FAMILY,
    stepIndex,
  });
  if (
    bound.subject.transaction_id !== evidence.finding.subject.transaction_id ||
    bound.script_index !== BigInt(evidence.finding.scriptIndex) ||
    authentication.script_hash !== evidence.targetScriptHashHex ||
    authentication.item_commitment !== evidence.targetItemCommitmentHex
  )
    throw new Error(
      `${FAMILY}: retained script authentication differs from bound evidence`,
    );
  const authenticated: Data.Static<
    typeof UnusedScriptAuthenticatedWitnessV1Schema
  > = {
    bound,
    prior_ledger_root: authentication.machine_state.prior_ledger_root,
    language_tag: authentication.language_tag,
    script_hash: authentication.script_hash,
    script_total_length: authentication.total_length,
    item_commitment: authentication.item_commitment,
    source_count: BigInt(evidence.sources.length),
    source_peaks:
      evidence.sources[0]?.membership.frontier.peaks.map(
        ({ height, hash }) => ({
          height: BigInt(height),
          hash: Buffer.from(hash).toString("hex"),
        }),
      ) ?? [],
    purpose_count: BigInt(evidence.purposes.length),
    purpose_peaks:
      evidence.purposes[0]?.membership.frontier.peaks.map(
        ({ height, hash }) => ({
          height: BigInt(height),
          hash: Buffer.from(hash).toString("hex"),
        }),
      ) ?? [],
  };
  const nextDatum = Data.to(
    { fraud_prover: signer.paymentKeyHash, data: authenticated } as never,
    UnusedScriptStep03DatumV1Schema as never,
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
      UnusedScriptStep02RedeemerV1Schema as never,
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
