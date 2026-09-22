import { asDataType } from "@al-ft/midgard-core/lucid-data";
import {
  MinAdaStep01SpendRedeemerSchema,
  MinAdaStep02DatumSchema,
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
  requireLinearFaultThreadUtxo,
} from "../linear-fault-family.js";
import { submitLinearFaultContinue } from "../linear-fault-submit.js";
import type { ResolvedProverSigner } from "../runtime.js";
import { requireInitialStepDatum } from "../submit-step-01.js";
import { computationThreadOutputPredicate } from "../tx-layout.js";
import type { FraudProofPreSubmitBoundary } from "../workflow/transaction-boundary.js";
import type { MinAdaContracts } from "./contracts.js";

type MinAdaStep01SpendRedeemer = Data.Static<
  typeof MinAdaStep01SpendRedeemerSchema
>;
const MinAdaStep01SpendRedeemer = asDataType<MinAdaStep01SpendRedeemer>(
  MinAdaStep01SpendRedeemerSchema,
);
type MinAdaStep02Datum = Data.Static<typeof MinAdaStep02DatumSchema>;
const MinAdaStep02Datum = asDataType<MinAdaStep02Datum>(
  MinAdaStep02DatumSchema,
);
type MinAdaStep02State = NonNullable<MinAdaStep02Datum["data"]>;
type ForcedPayload = Omit<
  NonNullable<
    Extract<
      MinAdaStep01SpendRedeemer,
      { Continue: unknown }
    >["Continue"][0]["forced_source"]
  >,
  "input_index" | "output_index"
>;

export const submitMinAdaStep01Forced = async ({
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
  readonly contracts: MinAdaContracts;
  readonly categoryId: string;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly state: MinAdaStep02State;
  readonly forcedSource: ForcedPayload;
  readonly referenceScriptUtxo: UTxO;
  readonly preSubmitBoundary?: FraudProofPreSubmitBoundary;
  readonly awaitConfirmation?: boolean;
}) => {
  if (state.direction !== 1n)
    throw new Error("minAda: expected exact wrongful rejection subject");
  const { threadUtxo, threadToken } = await requireLinearFaultThreadUtxo({
    lucid,
    contracts,
    categoryId,
    stepIndex: 0,
    family: "minAda",
    threadOutRef,
  });
  requireInitialStepDatum({ threadUtxo, signer });
  signer.selectWallet(lucid);
  const stepReference = requireLinearFaultReferenceScript({
    utxo: referenceScriptUtxo,
    expectedScriptHash: contracts.steps[0].spendingScriptHash,
    stepIndex: 0,
    family: "minAda",
  });
  const datum = Data.to(
    { fraud_prover: signer.paymentKeyHash, data: state },
    MinAdaStep02Datum,
  );
  const outputMatches = computationThreadOutputPredicate({
    address: contracts.steps[1].spendingScriptAddress,
    datum,
    unit: threadToken.unit,
  });
  let outputIndex: bigint | undefined;
  const redeemer = ((ctx) => {
    requireOwnSpendPurpose(ctx, threadUtxo, "min-ada forced step-01");
    outputIndex = requireUniqueOutputIndex(
      ctx.outputs,
      outputMatches,
      "min-ada forced output",
    );
    return Data.to(
      {
        Continue: [
          {
            fault: state.fault,
            tx_inclusion: null,
            post_utxo_membership: null,
            forced_source: {
              ...forcedSource,
              input_index: requireInputIndex(ctx, threadUtxo, "min-ada"),
              output_index: outputIndex,
            },
          },
        ],
      },
      MinAdaStep01SpendRedeemer,
    );
  }) satisfies BuildTxWithRedeemer;
  const txHash = await submitLinearFaultContinue({
    lucid,
    signerPaymentKeyHash: signer.paymentKeyHash,
    threadUtxo,
    threadUnit: threadToken.unit,
    stepReference,
    stepScript: contracts.steps[0].spendingScript,
    stepRole: "min-ada forced step-01",
    nextAddress: contracts.steps[1].spendingScriptAddress,
    nextDatum: datum,
    redeemer,
    preSubmitBoundary,
    awaitConfirmation,
  });
  if (outputIndex === undefined) throw new Error("minAda: layout unresolved");
  return { txHash, nextThreadOutRef: `${txHash}#${outputIndex.toString()}` };
};
