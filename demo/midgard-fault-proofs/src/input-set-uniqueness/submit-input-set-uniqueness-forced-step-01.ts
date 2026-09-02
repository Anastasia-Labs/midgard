import {
  type ForcedInclusionTxV1,
  forcedVerdictSubjectV1,
  type HeaderV1,
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

import { submitLinearFaultContinueV1 } from "../linear-fault-submit-v1.js";
import type { ResolvedProverSigner } from "../runtime.js";
import { requireInitialStepDatum } from "../submit-step-01.js";
import { computationThreadOutputPredicate } from "../tx-layout.js";
import type { FraudProofPreSubmitBoundaryV1 } from "../workflow/transaction-boundary-v1.js";
import type { InputSetUniquenessContractsV1 } from "./contracts-v1.js";
import {
  requireInputSetUniquenessReferenceScriptV1,
  requireInputSetUniquenessThreadUtxoV1,
} from "./submit-common-v1.js";
import { bindForcedDuplicateInputV1 } from "./wrongful-rejection-v1.js";

export const submitInputSetUniquenessForcedStep01V1 = async ({
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
  readonly contracts: InputSetUniquenessContractsV1;
  readonly categoryId: string;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly header: HeaderV1;
  readonly membership: RootMembershipProof<
    OutputReference,
    ForcedInclusionTxV1
  >;
  readonly referenceScriptUtxo: UTxO;
  readonly preSubmitBoundary?: FraudProofPreSubmitBoundaryV1;
  readonly awaitConfirmation?: boolean;
}) => {
  const verdict = membership.value.verdict;
  if (verdict === "ForcedTxValid") {
    throw new Error("input-set-uniqueness: forced leaf is not rejected");
  }
  const subject = forcedVerdictSubjectV1({
    transactionId: membership.value.tx_id,
    sourceKey: membership.key,
    rejectionReason: verdict.ForcedTxInvalid.reason,
  });
  const bound = bindForcedDuplicateInputV1(subject);
  const { threadUtxo, threadToken } =
    await requireInputSetUniquenessThreadUtxoV1({
      lucid,
      contracts,
      categoryId,
      stepIndex: 0,
      threadOutRef,
    });
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
  const stepReference = requireInputSetUniquenessReferenceScriptV1({
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
  const txHash = await submitLinearFaultContinueV1({
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
