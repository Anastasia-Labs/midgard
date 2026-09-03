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
import { advanceUnusedScriptWitnessSources } from "./checkpoint.js";
import type { UnusedScriptWitnessContracts } from "./contracts.js";
import type { UnusedScriptWitnessEvidence } from "./family.js";
import {
  UnusedScriptReverseScanSchema,
  UnusedScriptStep04DatumSchema,
  UnusedScriptStep04RedeemerSchema,
  UnusedScriptStep05DatumSchema,
} from "./schemas.js";

const FAMILY = "unused-script-witness";
export const submitUnusedScriptWitnessStep04 = async ({
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
  contracts: UnusedScriptWitnessContracts;
  categoryId: string;
  signer: ResolvedProverSigner;
  threadOutRef: string;
  evidence: UnusedScriptWitnessEvidence;
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
  const source = requireLinearFaultStepState<
    Data.Static<typeof UnusedScriptReverseScanSchema>
  >({
    threadUtxo,
    signer,
    schema: UnusedScriptStep04DatumSchema as never,
    family: FAMILY,
    stepIndex,
  });
  const nextState = advanceUnusedScriptWitnessSources({
    state: source,
    evidence,
  });
  const openings = evidence.sources
    .slice(0, evidence.finding.scriptIndex)
    .map((opening) => ({
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
    UnusedScriptStep05DatumSchema as never,
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
          { input_index: inputIndex, output_index: outputIndex, openings },
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
