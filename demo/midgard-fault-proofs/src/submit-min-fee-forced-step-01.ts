import {
  MinFeeStep01SpendRedeemer,
  MinFeeStep02Datum,
  type MinFeeStep02State,
  minFeeTerminalContradiction,
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

import { submitLinearFaultContinue } from "./linear-fault-submit.js";
import type { MinFeeContracts } from "./min-fee-contracts.js";
import {
  requireMinFeeReferenceScript,
  requireMinFeeThreadUtxo,
} from "./min-fee-submit-common.js";
import type { ResolvedProverSigner } from "./runtime.js";
import { requireInitialStepDatum } from "./step-support.js";
import { computationThreadOutputPredicate } from "./tx-layout.js";
import type { FraudProofPreSubmitBoundary } from "./workflow/transaction-boundary.js";

type ForcedSource = Extract<
  MinFeeStep01SpendRedeemer,
  { Continue: unknown }
>["Continue"][0]["source"];
type ForcedPayload = Omit<
  Extract<ForcedSource, { ForcedSource: unknown }>["ForcedSource"],
  "input_index" | "output_index"
>;

export const submitMinFeeStep01Forced = async ({
  lucid,
  contracts,
  categoryId,
  signer,
  threadOutRef,
  state,
  forcedSource,
  referenceScriptUtxo,
  preSubmitBoundary,
  awaitConfirmation = true,
}: {
  readonly lucid: LucidEvolution;
  readonly contracts: MinFeeContracts;
  readonly categoryId: string;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly state: MinFeeStep02State;
  readonly forcedSource: ForcedPayload;
  readonly referenceScriptUtxo: UTxO;
  readonly preSubmitBoundary?: FraudProofPreSubmitBoundary;
  readonly awaitConfirmation?: boolean;
}) => {
  if (
    state.subject.direction !== 1n ||
    !minFeeTerminalContradiction(state.subject, false)
  )
    throw new Error("minFee: expected exact wrongful rejection subject");
  const { threadUtxo, threadToken } = await requireMinFeeThreadUtxo({
    lucid,
    contracts,
    categoryId,
    stepIndex: 0,
    threadOutRef,
  });
  requireInitialStepDatum({ threadUtxo, signer });
  signer.selectWallet(lucid);
  const stepReference = requireMinFeeReferenceScript({
    utxo: referenceScriptUtxo,
    expectedScriptHash: contracts.steps[0].spendingScriptHash,
    stepIndex: 0,
  });
  const datum = Data.to(
    { fraud_prover: signer.paymentKeyHash, data: state },
    MinFeeStep02Datum,
  );
  const outputMatches = computationThreadOutputPredicate({
    address: contracts.steps[1].spendingScriptAddress,
    datum,
    unit: threadToken.unit,
  });
  let outputIndex: bigint | undefined;
  const redeemer = ((ctx) => {
    requireOwnSpendPurpose(ctx, threadUtxo, "min-fee forced step-01");
    outputIndex = requireUniqueOutputIndex(
      ctx.outputs,
      outputMatches,
      "min-fee forced output",
    );
    return Data.to(
      {
        Continue: [
          {
            source: {
              ForcedSource: {
                ...forcedSource,
                input_index: requireInputIndex(ctx, threadUtxo, "min-fee"),
                output_index: outputIndex,
              },
            },
          },
        ],
      },
      MinFeeStep01SpendRedeemer,
    );
  }) satisfies BuildTxWithRedeemer;
  const txHash = await submitLinearFaultContinue({
    lucid,
    signerPaymentKeyHash: signer.paymentKeyHash,
    threadUtxo,
    threadUnit: threadToken.unit,
    stepReference,
    stepScript: contracts.steps[0].spendingScript,
    stepRole: "min-fee forced step-01",
    nextAddress: contracts.steps[1].spendingScriptAddress,
    nextDatum: datum,
    redeemer,
    preSubmitBoundary,
    awaitConfirmation,
  });
  if (outputIndex === undefined) throw new Error("minFee: layout unresolved");
  return { txHash, nextThreadOutRef: `${txHash}#${outputIndex.toString()}` };
};
