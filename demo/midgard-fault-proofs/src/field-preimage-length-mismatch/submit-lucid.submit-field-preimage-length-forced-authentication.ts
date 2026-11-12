import {
  type CommittedFieldClaim,
  FieldPreimageLengthStep02RedeemerSchema,
  FieldPreimageLengthStep03DatumSchema,
  type ForcedInclusionTxV1,
  forcedLeafMembershipV1,
  forcedVerdictSubject,
  type Header,
  type OutputReference,
  requireInputIndex,
  requireOwnSpendPurpose,
  requireUniqueOutputIndex,
  type RootMembershipProof,
} from "@al-ft/midgard-sdk";
import {
  type BuildTxWithRedeemer,
  Data,
  type UTxO,
} from "@lucid-evolution/lucid";

import { completeStep, fundStep, submitStep } from "../step-completion.js";
import { computationThreadOutputPredicate } from "../tx-layout.js";
import type { FraudProofPreSubmitBoundary } from "../workflow/transaction-boundary.js";
import { workflowReferenceScriptsUsedByTransaction } from "../workflow/transaction-boundary.js";
import type { ManifestBoundFieldPreimageLengthConfig } from "./config.js";
import {
  type FieldPreimageLengthClaimResolver,
  LABEL,
  requireReference,
  requireThread,
  type SubmitFieldPreimageLengthForcedDispatchResult,
} from "./submit-lucid.submit-field-preimage-length-cancel.js";
import type { PreparedFieldPreimageLengthWorkflow } from "./workflow.js";

export const submitFieldPreimageLengthForcedAuthentication = async ({
  config,
  threadOutRef,
  header,
  membership,
  claim,
  claimResolver,
  prepared,
  carriageReferenceInputs = [],
  preSubmitBoundary,
  awaitConfirmation = true,
}: {
  readonly config: ManifestBoundFieldPreimageLengthConfig;
  readonly threadOutRef: string;
  readonly header: Header;
  readonly membership: RootMembershipProof<
    OutputReference,
    ForcedInclusionTxV1
  >;
  readonly claim?: CommittedFieldClaim;
  readonly claimResolver?: FieldPreimageLengthClaimResolver;
  readonly prepared: PreparedFieldPreimageLengthWorkflow;
  readonly carriageReferenceInputs?: readonly UTxO[];
  readonly preSubmitBoundary?: FraudProofPreSubmitBoundary;
  readonly awaitConfirmation?: boolean;
}): Promise<SubmitFieldPreimageLengthForcedDispatchResult> => {
  const { threadUtxo, threadToken, step } = await requireThread({
    config,
    threadOutRef,
    stepIndex: 2,
  });
  const verdict = membership.value.verdict;
  const rejectionReason =
    verdict === "ForcedTxValid" ? null : verdict.ForcedTxInvalid.reason;
  if (
    prepared.transactionId !== membership.value.tx_id ||
    (prepared.direction === "wrongfulAcceptance" ? 0n : 1n) !==
      (rejectionReason === null ? 0n : 1n)
  ) {
    throw new Error(`${LABEL}: forced leaf differs from admitted evidence`);
  }
  const subject = forcedVerdictSubject({
    transactionId: membership.value.tx_id,
    sourceKey: membership.key,
    rejectionReason,
  });
  const terminal = config.contracts.fieldPreimageLengthMismatch.steps[3];
  const datum = Data.to(
    {
      fraud_prover: config.signer.paymentKeyHash,
      data: {
        subject,
        field_index: BigInt(prepared.fieldIndex),
        declared_length: BigInt(prepared.declaredLength),
        actual_length: BigInt(prepared.actualLength),
      },
    } as never,
    FieldPreimageLengthStep03DatumSchema as never,
  );
  const outputMatches = computationThreadOutputPredicate({
    address: terminal.spendingScriptAddress,
    datum,
    unit: threadToken.unit,
  });
  let layout:
    | { readonly inputIndex: bigint; readonly outputIndex: bigint }
    | undefined;
  const redeemer = ((ctx) => {
    // §8.7 carriage indices count into the built reference-input set.
    const resolvedClaim =
      claimResolver === undefined ? claim! : claimResolver(ctx.referenceInputs);
    requireOwnSpendPurpose(ctx, threadUtxo, `${LABEL} forced auth`);
    layout = {
      inputIndex: requireInputIndex(ctx, threadUtxo, LABEL),
      outputIndex: requireUniqueOutputIndex(
        ctx.outputs,
        outputMatches,
        `${LABEL} terminal output`,
      ),
    };
    return Data.to(
      {
        Continue: [
          {
            AuthenticateForced: {
              input_index: layout.inputIndex,
              output_index: layout.outputIndex,
              header,
              membership: forcedLeafMembershipV1(membership),
              claim: resolvedClaim,
            },
          },
        ],
      } as never,
      FieldPreimageLengthStep02RedeemerSchema as never,
    );
  }) satisfies BuildTxWithRedeemer;
  const reference = requireReference({
    utxo: config.referenceScripts.step02Forced,
    expectedHash: step.spendingScriptHash,
    role: "forced step-02",
  });
  const completeReferences = [reference, ...carriageReferenceInputs];
  if (claimResolver === undefined && claim === undefined)
    throw new Error(`${LABEL}: forced authentication omitted field claim`);
  const funding = await fundStep(config.lucid, { signer: config.signer });
  const signed = await completeStep(
    funding.tx
      .collectFrom([threadUtxo], redeemer)
      .pay.ToContract(
        terminal.spendingScriptAddress,
        { kind: "inline", value: datum },
        { lovelace: threadUtxo.assets.lovelace ?? 0n, [threadToken.unit]: 1n },
      )
      .readFrom(completeReferences),
    funding,
  );
  if (layout === undefined) throw new Error(`${LABEL}: layout did not resolve`);
  const resolved = layout;
  const txHash = await submitStep(config.lucid, signed, {
    referenceScripts: workflowReferenceScriptsUsedByTransaction({
      signed,
      candidates: [
        {
          role: "V1 field-preimage-length forced step-02",
          utxo: reference,
          expectedScript: step.spendingScript,
        },
      ],
    }),
    boundary: preSubmitBoundary,
    awaitConfirmation,
  });
  return {
    txHash,
    nextThreadOutRef: `${txHash}#${resolved.outputIndex.toString()}`,
    computationThreadUnit: threadToken.unit,
    inputIndex: Number(resolved.inputIndex),
    outputIndex: Number(resolved.outputIndex),
  };
};
