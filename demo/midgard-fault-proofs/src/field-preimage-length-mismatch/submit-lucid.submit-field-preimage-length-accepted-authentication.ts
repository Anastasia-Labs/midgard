import {
  acceptedVerdictSubject,
  type CommittedFieldClaim,
  FieldPreimageLengthStep01RedeemerSchema,
  FieldPreimageLengthStep02DatumSchema,
  FieldPreimageLengthStep02RedeemerSchema,
  FieldPreimageLengthStep03DatumSchema,
  requireInputIndex,
  requireOwnSpendPurpose,
  requireUniqueOutputIndex,
} from "@al-ft/midgard-sdk";
import {
  type BuildTxWithRedeemer,
  Data,
  type UTxO,
} from "@lucid-evolution/lucid";

import { completeStep, fundStep, submitStep } from "../step-completion.js";
import { requireInitialStepDatum } from "../step-support.js";
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

/** Real Lucid step-01 dispatch; step 02 derives direction from the authenticated leaf. */
export const submitFieldPreimageLengthForcedDispatch = async ({
  config,
  threadOutRef,
  preSubmitBoundary,
  awaitConfirmation = true,
}: {
  readonly config: ManifestBoundFieldPreimageLengthConfig;
  readonly threadOutRef: string;
  readonly preSubmitBoundary?: FraudProofPreSubmitBoundary;
  readonly awaitConfirmation?: boolean;
}): Promise<SubmitFieldPreimageLengthForcedDispatchResult> => {
  const { threadUtxo, threadToken, step } = await requireThread({
    config,
    threadOutRef,
    stepIndex: 0,
  });
  requireInitialStepDatum({ threadUtxo, signer: config.signer });
  const funding = await fundStep(config.lucid, { signer: config.signer });
  const next = config.contracts.fieldPreimageLengthMismatch.forcedStep02;
  const datum = Data.to(
    {
      fraud_prover: config.signer.paymentKeyHash,
      data: "PendingForced",
    } as never,
    FieldPreimageLengthStep02DatumSchema as never,
  );
  const outputMatches = computationThreadOutputPredicate({
    address: next.spendingScriptAddress,
    datum,
    unit: threadToken.unit,
  });
  let layout:
    | { readonly inputIndex: bigint; readonly outputIndex: bigint }
    | undefined;
  const redeemer = ((ctx) => {
    requireOwnSpendPurpose(ctx, threadUtxo, `${LABEL} forced dispatch`);
    layout = {
      inputIndex: requireInputIndex(ctx, threadUtxo, LABEL),
      outputIndex: requireUniqueOutputIndex(
        ctx.outputs,
        outputMatches,
        `${LABEL} forced output`,
      ),
    };
    return Data.to(
      {
        Continue: [
          {
            RecordForced: {
              input_index: layout.inputIndex,
              output_index: layout.outputIndex,
            },
          },
        ],
      } as never,
      FieldPreimageLengthStep01RedeemerSchema as never,
    );
  }) satisfies BuildTxWithRedeemer;
  const reference = requireReference({
    utxo: config.referenceScripts.step01,
    expectedHash: step.spendingScriptHash,
    role: "step-01",
  });
  const tx = funding.tx
    .collectFrom([threadUtxo], redeemer)
    .pay.ToContract(
      next.spendingScriptAddress,
      { kind: "inline", value: datum },
      {
        lovelace: threadUtxo.assets.lovelace ?? 0n,
        [threadToken.unit]: 1n,
      },
    )
    .readFrom([reference]);
  const signed = await completeStep(tx, funding);
  if (layout === undefined) {
    throw new Error(`${LABEL}: forced dispatch layout did not resolve`);
  }
  const resolvedLayout = layout;
  const txHash = await submitStep(config.lucid, signed, {
    referenceScripts: workflowReferenceScriptsUsedByTransaction({
      signed,
      candidates: [
        {
          role: "V1 field-preimage-length step-01",
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
    nextThreadOutRef: `${txHash}#${resolvedLayout.outputIndex.toString()}`,
    computationThreadUnit: threadToken.unit,
    inputIndex: Number(resolvedLayout.inputIndex),
    outputIndex: Number(resolvedLayout.outputIndex),
  };
};

/** Authenticates an accepted source's inline field opening into terminal state. */
export const submitFieldPreimageLengthAcceptedAuthentication = async ({
  config,
  threadOutRef,
  claim,
  claimResolver,
  prepared,
  carriageReferenceInputs = [],
  preSubmitBoundary,
  awaitConfirmation = true,
}: {
  readonly config: ManifestBoundFieldPreimageLengthConfig;
  readonly threadOutRef: string;
  readonly claim?: CommittedFieldClaim;
  readonly claimResolver?: FieldPreimageLengthClaimResolver;
  readonly prepared: PreparedFieldPreimageLengthWorkflow;
  readonly carriageReferenceInputs?: readonly UTxO[];
  readonly preSubmitBoundary?: FraudProofPreSubmitBoundary;
  readonly awaitConfirmation?: boolean;
}): Promise<SubmitFieldPreimageLengthForcedDispatchResult> => {
  if (prepared.direction !== "wrongfulAcceptance") {
    throw new Error(
      `${LABEL}: accepted authenticator received forced evidence`,
    );
  }
  const { threadUtxo, threadToken, step } = await requireThread({
    config,
    threadOutRef,
    stepIndex: 1,
  });
  const terminal = config.contracts.fieldPreimageLengthMismatch.steps[3];
  const state = {
    subject: acceptedVerdictSubject(prepared.transactionId),
    field_index: BigInt(prepared.fieldIndex),
    declared_length: BigInt(prepared.declaredLength),
    actual_length: BigInt(prepared.actualLength),
  };
  const datum = Data.to(
    { fraud_prover: config.signer.paymentKeyHash, data: state } as never,
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
    requireOwnSpendPurpose(ctx, threadUtxo, `${LABEL} accepted auth`);
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
            AuthenticateAccepted: {
              input_index: layout.inputIndex,
              output_index: layout.outputIndex,
              claim: resolvedClaim,
            },
          },
        ],
      } as never,
      FieldPreimageLengthStep02RedeemerSchema as never,
    );
  }) satisfies BuildTxWithRedeemer;
  const reference = requireReference({
    utxo: config.referenceScripts.step02Accepted,
    expectedHash: step.spendingScriptHash,
    role: "accepted step-02",
  });
  if (claimResolver === undefined && claim === undefined)
    throw new Error(`${LABEL}: accepted authentication omitted field claim`);
  const funding = await fundStep(config.lucid, { signer: config.signer });
  const signed = await completeStep(
    funding.tx
      .collectFrom([threadUtxo], redeemer)
      .pay.ToContract(
        terminal.spendingScriptAddress,
        { kind: "inline", value: datum },
        {
          lovelace: threadUtxo.assets.lovelace ?? 0n,
          [threadToken.unit]: 1n,
        },
      )
      .readFrom([reference, ...carriageReferenceInputs]),
    funding,
  );
  if (layout === undefined) throw new Error(`${LABEL}: layout did not resolve`);
  const resolved = layout;
  const txHash = await submitStep(config.lucid, signed, {
    referenceScripts: workflowReferenceScriptsUsedByTransaction({
      signed,
      candidates: [
        {
          role: "V1 field-preimage-length accepted step-02",
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
