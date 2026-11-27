import {
  forcedLeafMembershipV1,
  type ForcedTransactionSourceMembershipProof,
  requireInputIndex,
  requireOwnSpendPurpose,
  requireUniqueOutputIndex,
} from "@al-ft/midgard-sdk/core";
import {
  MinFeeStep01SpendRedeemer,
  MinFeeStep02Datum,
  type MinFeeStep02State,
  minFeeTerminalContradiction,
} from "@al-ft/midgard-sdk/fraud-proof";
import {
  type BuildTxWithRedeemer,
  Data,
  type LucidEvolution,
  type UTxO,
} from "@lucid-evolution/lucid";

import { buildLinearFaultContinue } from "./linear-fault-submit.js";
import type { MinFeeContracts } from "./min-fee-contracts.js";
import {
  requireMinFeeReferenceScript,
  requireMinFeeThreadUtxo,
} from "./min-fee-submit-common.js";
import type { ResolvedProverSigner } from "./runtime.js";
import { requireInitialStepDatum } from "./step-support.js";
import { computationThreadOutputPredicate } from "./tx-layout.js";

type ForcedSource = Extract<
  MinFeeStep01SpendRedeemer,
  { Continue: unknown }
>["Continue"][0]["source"];
type ForcedPayload = Omit<
  Extract<ForcedSource, { ForcedSource: unknown }>["ForcedSource"],
  "input_index" | "output_index" | "membership"
> & {
  readonly membership: ForcedTransactionSourceMembershipProof;
};

export const buildMinFeeStep01Forced = async ({
  lucid,
  contracts,
  categoryId,
  signer,
  threadOutRef,
  state,
  forcedSource,
  referenceScriptUtxo,
}: {
  readonly lucid: LucidEvolution;
  readonly contracts: MinFeeContracts;
  readonly categoryId: string;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly state: MinFeeStep02State;
  readonly forcedSource: ForcedPayload;
  readonly referenceScriptUtxo: UTxO;
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
                membership: forcedLeafMembershipV1(forcedSource.membership),
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
  const transaction = await buildLinearFaultContinue({
    lucid,
    signer,
    threadUtxo,
    threadUnit: threadToken.unit,
    stepReference,
    stepScript: contracts.steps[0].spendingScript,
    stepRole: "min-fee forced step-01",
    nextAddress: contracts.steps[1].spendingScriptAddress,
    nextDatum: datum,
    redeemer,
  });
  if (outputIndex === undefined) throw new Error("minFee: layout unresolved");
  return {
    transaction,
    nextThreadOutRef: `${transaction.txHash}#${outputIndex.toString()}`,
  };
};
