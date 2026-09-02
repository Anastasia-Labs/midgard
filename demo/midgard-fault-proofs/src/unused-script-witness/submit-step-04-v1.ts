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
import { advanceUnusedScriptWitnessSourcesV1 } from "./checkpoint-v1.js";
import type { UnusedScriptWitnessContractsV1 } from "./contracts-v1.js";
import type { UnusedScriptWitnessEvidenceV1 } from "./family-v1.js";
import {
  UnusedScriptReverseScanV1Schema,
  UnusedScriptStep04DatumV1Schema,
  UnusedScriptStep04RedeemerV1Schema,
  UnusedScriptStep05DatumV1Schema,
} from "./schemas-v1.js";

const FAMILY = "unused-script-witness";
export const submitUnusedScriptWitnessStep04V1 = async ({
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
  contracts: UnusedScriptWitnessContractsV1;
  categoryId: string;
  signer: ResolvedProverSigner;
  threadOutRef: string;
  evidence: UnusedScriptWitnessEvidenceV1;
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
  const source = requireLinearFaultStepStateV1<
    Data.Static<typeof UnusedScriptReverseScanV1Schema>
  >({
    threadUtxo,
    signer,
    schema: UnusedScriptStep04DatumV1Schema as never,
    family: FAMILY,
    stepIndex,
  });
  const nextState = advanceUnusedScriptWitnessSourcesV1({
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
    UnusedScriptStep05DatumV1Schema as never,
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
          { input_index: inputIndex, output_index: outputIndex, openings },
        ],
      } as never,
      UnusedScriptStep04RedeemerV1Schema as never,
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
