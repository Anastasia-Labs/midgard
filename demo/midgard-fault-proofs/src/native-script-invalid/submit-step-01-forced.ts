import { asDataType } from "@al-ft/midgard-core/lucid-data";
import {
  NativeScriptInvalidStep01SpendRedeemerSchema,
  NativeScriptInvalidStep02DatumSchema,
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
import { requireInitialStepDatum } from "../step-support.js";
import { computationThreadOutputPredicate } from "../tx-layout.js";
import type { FraudProofPreSubmitBoundary } from "../workflow/transaction-boundary.js";
import type { NativeScriptInvalidContracts } from "./contracts.js";

type NativeScriptInvalidStep01SpendRedeemer = Data.Static<
  typeof NativeScriptInvalidStep01SpendRedeemerSchema
>;
const NativeScriptInvalidStep01SpendRedeemer =
  asDataType<NativeScriptInvalidStep01SpendRedeemer>(
    NativeScriptInvalidStep01SpendRedeemerSchema,
  );
type NativeScriptInvalidStep02Datum = Data.Static<
  typeof NativeScriptInvalidStep02DatumSchema
>;
const NativeScriptInvalidStep02Datum =
  asDataType<NativeScriptInvalidStep02Datum>(
    NativeScriptInvalidStep02DatumSchema,
  );
type NativeScriptInvalidStep02State = NonNullable<
  NativeScriptInvalidStep02Datum["data"]
>;
type ForcedSource = Extract<
  NativeScriptInvalidStep01SpendRedeemer,
  { Continue: unknown }
>["Continue"][0]["source"];
type ForcedPayload = Omit<
  Extract<ForcedSource, { ForcedSource: unknown }>["ForcedSource"],
  "input_index" | "output_index"
>;

export const submitNativeScriptInvalidStep01Forced = async ({
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
  readonly contracts: NativeScriptInvalidContracts;
  readonly categoryId: string;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly state: NativeScriptInvalidStep02State;
  readonly forcedSource: ForcedPayload;
  readonly referenceScriptUtxo: UTxO;
  readonly preSubmitBoundary?: FraudProofPreSubmitBoundary;
  readonly awaitConfirmation?: boolean;
}) => {
  if (state.subject.direction !== 1n)
    throw new Error(
      "nativeScriptInvalid: expected exact wrongful rejection subject",
    );
  const { threadUtxo, threadToken } = await requireLinearFaultThreadUtxo({
    lucid,
    contracts,
    categoryId,
    stepIndex: 0,
    family: "nativeScriptInvalid",
    threadOutRef,
  });
  requireInitialStepDatum({ threadUtxo, signer });
  signer.selectWallet(lucid);
  const stepReference = requireLinearFaultReferenceScript({
    utxo: referenceScriptUtxo,
    expectedScriptHash: contracts.steps[0].spendingScriptHash,
    stepIndex: 0,
    family: "nativeScriptInvalid",
  });
  const datum = Data.to(
    { fraud_prover: signer.paymentKeyHash, data: state },
    NativeScriptInvalidStep02Datum,
  );
  const outputMatches = computationThreadOutputPredicate({
    address: contracts.steps[1].spendingScriptAddress,
    datum,
    unit: threadToken.unit,
  });
  let outputIndex: bigint | undefined;
  const redeemer = ((ctx) => {
    requireOwnSpendPurpose(
      ctx,
      threadUtxo,
      "native-script-invalid forced step-01",
    );
    outputIndex = requireUniqueOutputIndex(
      ctx.outputs,
      outputMatches,
      "native-script-invalid forced output",
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
                  "native-script-invalid",
                ),
                output_index: outputIndex,
              },
            },
          },
        ],
      },
      NativeScriptInvalidStep01SpendRedeemer,
    );
  }) satisfies BuildTxWithRedeemer;
  const txHash = await submitLinearFaultContinue({
    lucid,
    signerPaymentKeyHash: signer.paymentKeyHash,
    threadUtxo,
    threadUnit: threadToken.unit,
    stepReference,
    stepScript: contracts.steps[0].spendingScript,
    stepRole: "native-script-invalid forced step-01",
    nextAddress: contracts.steps[1].spendingScriptAddress,
    nextDatum: datum,
    redeemer,
    preSubmitBoundary,
    awaitConfirmation,
  });
  if (outputIndex === undefined)
    throw new Error("nativeScriptInvalid: layout unresolved");
  return { txHash, nextThreadOutRef: `${txHash}#${outputIndex.toString()}` };
};
