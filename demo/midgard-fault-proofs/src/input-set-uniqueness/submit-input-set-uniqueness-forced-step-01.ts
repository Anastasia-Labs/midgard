import {
  type ForcedInclusionTxV1,
  forcedVerdictSubject,
  type Header,
  InputSetUniquenessStep01SpendRedeemerSchema,
  InputSetUniquenessStep03DatumSchema,
  type OutputReference,
  requireInputIndex,
  requireOwnSpendPurpose,
  requireUniqueOutputIndex,
  type RootMembershipProof,
} from "@al-ft/midgard-sdk";
import {
  type BuildTxWithRedeemer,
  Data,
  type LucidEvolution,
  type UTxO,
} from "@lucid-evolution/lucid";

import { submitLinearFaultContinue } from "../linear-fault-submit.js";
import type { ResolvedProverSigner } from "../runtime.js";
import { requireInitialStepDatum } from "../submit-step-01.js";
import { computationThreadOutputPredicate } from "../tx-layout.js";
import type { FraudProofPreSubmitBoundary } from "../workflow/transaction-boundary.js";
import type { InputSetUniquenessContracts } from "./contracts.js";
import {
  requireInputSetUniquenessReferenceScript,
  requireInputSetUniquenessThreadUtxo,
} from "./submit-common.js";
import { bindForcedDuplicateInput } from "./wrongful-rejection.js";

export const submitInputSetUniquenessForcedStep01 = async ({
  lucid,
  contracts,
  categoryId,
  signer,
  threadOutRef,
  header,
  membership,
  referenceScriptUtxo,
  preSubmitBoundary,
  awaitConfirmation = true,
}: {
  readonly lucid: LucidEvolution;
  readonly contracts: InputSetUniquenessContracts;
  readonly categoryId: string;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly header: Header;
  readonly membership: RootMembershipProof<
    OutputReference,
    ForcedInclusionTxV1
  >;
  readonly referenceScriptUtxo: UTxO;
  readonly preSubmitBoundary?: FraudProofPreSubmitBoundary;
  readonly awaitConfirmation?: boolean;
}) => {
  const verdict = membership.value.verdict;
  if (verdict === "ForcedTxValid") {
    throw new Error("input-set-uniqueness: forced leaf is not rejected");
  }
  const subject = forcedVerdictSubject({
    transactionId: membership.value.tx_id,
    sourceKey: membership.key,
    rejectionReason: verdict.ForcedTxInvalid.reason,
  });
  const bound = bindForcedDuplicateInput(subject);
  const { threadUtxo, threadToken } = await requireInputSetUniquenessThreadUtxo(
    {
      lucid,
      contracts,
      categoryId,
      stepIndex: 0,
      threadOutRef,
    },
  );
  requireInitialStepDatum({ threadUtxo, signer });
  const nextDatum = Data.to(
    { fraud_prover: signer.paymentKeyHash, data: { bound } } as never,
    InputSetUniquenessStep03DatumSchema as never,
  );
  const outputMatches = computationThreadOutputPredicate({
    address: contracts.steps[2].spendingScriptAddress,
    datum: nextDatum,
    unit: threadToken.unit,
  });
  const stepReference = requireInputSetUniquenessReferenceScript({
    utxo: referenceScriptUtxo,
    expectedScriptHash: contracts.steps[0].spendingScriptHash,
    stepIndex: 0,
  });
  let outputIndex: bigint | undefined;
  const redeemer = ((ctx) => {
    requireOwnSpendPurpose(
      ctx,
      threadUtxo,
      "input-set-uniqueness forced step-01",
    );
    const inputIndex = requireInputIndex(
      ctx,
      threadUtxo,
      "input-set-uniqueness forced step-01",
    );
    outputIndex = requireUniqueOutputIndex(
      ctx.outputs,
      outputMatches,
      "input-set-uniqueness forced step-01",
    );
    return Data.to(
      {
        Continue: [
          {
            source: {
              ForcedSource: {
                input_index: inputIndex,
                output_index: outputIndex,
                header,
                membership,
              },
            },
          },
        ],
      } as never,
      InputSetUniquenessStep01SpendRedeemerSchema as never,
    );
  }) satisfies BuildTxWithRedeemer;
  signer.selectWallet(lucid);
  const txHash = await submitLinearFaultContinue({
    lucid,
    signerPaymentKeyHash: signer.paymentKeyHash,
    threadUtxo,
    threadUnit: threadToken.unit,
    stepReference,
    stepScript: contracts.steps[0].spendingScript,
    stepRole: "input-set-uniqueness forced step-01",
    nextAddress: contracts.steps[2].spendingScriptAddress,
    nextDatum,
    redeemer,
    preSubmitBoundary,
    awaitConfirmation,
  });
  if (outputIndex === undefined)
    throw new Error("input-set-uniqueness: unresolved layout");
  return Object.freeze({
    txHash,
    nextThreadOutRef: `${txHash}#${outputIndex.toString()}`,
    bound,
  });
};
