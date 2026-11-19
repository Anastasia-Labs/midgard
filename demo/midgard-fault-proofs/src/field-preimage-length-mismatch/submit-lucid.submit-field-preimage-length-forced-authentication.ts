import { MIDGARD_FIELD_COUNT } from "@al-ft/midgard-core";
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
  type VerdictSubject,
} from "@al-ft/midgard-sdk";
import {
  type BuildTxWithRedeemer,
  Data,
  type UTxO,
} from "@lucid-evolution/lucid";

import { completeStep, fundStep } from "../step-completion.js";
import { finishStep } from "../step-completion.js";
import { computationThreadOutputPredicate } from "../tx-layout.js";
import type { ManifestBoundFieldPreimageLengthConfig } from "./config.js";
import type { FieldPreimageLengthForcedDispatchBuild } from "./submit-lucid.submit-field-preimage-length-cancel.js";
import {
  type FieldPreimageLengthClaimResolver,
  LABEL,
  requireReference,
  requireThread,
} from "./submit-lucid.submit-field-preimage-length-cancel.js";
import type { PreparedFieldPreimageLengthWorkflow } from "./workflow.js";

type ForcedMembership = RootMembershipProof<
  OutputReference,
  ForcedInclusionTxV1
>;

type ForcedLayout = {
  readonly inputIndex: bigint;
  readonly outputIndex: bigint;
};

const forcedSubject = (membership: ForcedMembership) => {
  const verdict = membership.value.verdict;
  const rejectionReason =
    verdict === "ForcedTxValid" ? null : verdict.ForcedTxInvalid.reason;
  return {
    rejectionReason,
    subject: forcedVerdictSubject({
      transactionId: membership.value.tx_id,
      sourceKey: membership.key,
      rejectionReason,
    }),
  };
};

/**
 * Forced step 02 to the terminal: spends the pending thread with the redeemer
 * `args` builds from the transaction's layout and writes `step_03.State`.
 */
const buildForcedStep02 = async ({
  config,
  threadOutRef,
  state,
  args,
  carriageReferenceInputs,
}: {
  readonly config: ManifestBoundFieldPreimageLengthConfig;
  readonly threadOutRef: string;
  readonly state: {
    readonly subject: VerdictSubject;
    readonly fieldIndex: bigint;
    readonly declaredLength: bigint;
    readonly actualLength: bigint;
  };
  readonly args: (
    layout: ForcedLayout,
    referenceInputs: Parameters<BuildTxWithRedeemer>[0]["referenceInputs"],
  ) => unknown;
  readonly carriageReferenceInputs: readonly UTxO[];
}): Promise<FieldPreimageLengthForcedDispatchBuild> => {
  const { threadUtxo, threadToken, step } = await requireThread({
    config,
    threadOutRef,
    stepIndex: 2,
  });
  const terminal = config.contracts.fieldPreimageLengthMismatch.steps[3];
  const datum = Data.to(
    {
      fraud_prover: config.signer.paymentKeyHash,
      data: {
        subject: state.subject,
        field_index: state.fieldIndex,
        declared_length: state.declaredLength,
        actual_length: state.actualLength,
      },
    } as never,
    FieldPreimageLengthStep03DatumSchema as never,
  );
  const outputMatches = computationThreadOutputPredicate({
    address: terminal.spendingScriptAddress,
    datum,
    unit: threadToken.unit,
  });
  let layout: ForcedLayout | undefined;
  const redeemer = ((ctx) => {
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
      { Continue: [args(layout, ctx.referenceInputs)] } as never,
      FieldPreimageLengthStep02RedeemerSchema as never,
    );
  }) satisfies BuildTxWithRedeemer;
  const reference = requireReference({
    utxo: config.referenceScripts.step02Forced,
    expectedHash: step.spendingScriptHash,
    role: "forced step-02",
  });
  const funding = await fundStep(config.lucid, { signer: config.signer });
  const signed = await completeStep(
    funding.tx
      .collectFrom([threadUtxo], redeemer)
      .pay.ToContract(
        terminal.spendingScriptAddress,
        { kind: "inline", value: datum },
        { lovelace: threadUtxo.assets.lovelace ?? 0n, [threadToken.unit]: 1n },
      )
      .readFrom([reference, ...carriageReferenceInputs]),
    funding,
  );
  if (layout === undefined) throw new Error(`${LABEL}: layout did not resolve`);
  const resolved: ForcedLayout = layout;
  const transaction = finishStep(signed, {
    referenceScriptCandidates: [
      {
        role: "V1 field-preimage-length forced step-02",
        utxo: reference,
        expectedScript: step.spendingScript,
      },
    ],
  });
  return {
    transaction,
    nextThreadOutRef: `${transaction.txHash}#${resolved.outputIndex.toString()}`,
    computationThreadUnit: threadToken.unit,
    inputIndex: Number(resolved.inputIndex),
    outputIndex: Number(resolved.outputIndex),
  };
};

export const buildFieldPreimageLengthForcedAuthentication = async ({
  config,
  threadOutRef,
  header,
  membership,
  claim,
  claimResolver,
  prepared,
  carriageReferenceInputs = [],
}: {
  readonly config: ManifestBoundFieldPreimageLengthConfig;
  readonly threadOutRef: string;
  readonly header: Header;
  readonly membership: ForcedMembership;
  readonly claim?: CommittedFieldClaim;
  readonly claimResolver?: FieldPreimageLengthClaimResolver;
  readonly prepared: PreparedFieldPreimageLengthWorkflow;
  readonly carriageReferenceInputs?: readonly UTxO[];
}): Promise<FieldPreimageLengthForcedDispatchBuild> => {
  const { subject, rejectionReason } = forcedSubject(membership);
  if (
    prepared.transactionId !== membership.value.tx_id ||
    (prepared.direction === "wrongfulAcceptance" ? 0n : 1n) !==
      (rejectionReason === null ? 0n : 1n)
  ) {
    throw new Error(`${LABEL}: forced leaf differs from admitted evidence`);
  }
  if (claimResolver === undefined && claim === undefined)
    throw new Error(`${LABEL}: forced authentication omitted field claim`);
  return await buildForcedStep02({
    config,
    threadOutRef,
    state: {
      subject,
      fieldIndex: BigInt(prepared.fieldIndex),
      declaredLength: BigInt(prepared.declaredLength),
      actualLength: BigInt(prepared.actualLength),
    },
    // §8.7 carriage indices count into the built reference-input set.
    args: (layout, referenceInputs) => ({
      AuthenticateForced: {
        input_index: layout.inputIndex,
        output_index: layout.outputIndex,
        header,
        membership: forcedLeafMembershipV1(membership),
        claim:
          claimResolver === undefined ? claim! : claimResolver(referenceInputs),
      },
    }),
    carriageReferenceInputs,
  });
};

/**
 * Forced step 02 for a rejection whose `FieldPreimageLengthMismatch` names a
 * field outside the native transaction's layout: no field is opened, the
 * terminal state records that field with zero lengths, and the terminal holds
 * no fault there, so the rejection is contradicted.
 */
export const buildFieldPreimageLengthForcedOutOfDomain = async ({
  config,
  threadOutRef,
  header,
  membership,
}: {
  readonly config: ManifestBoundFieldPreimageLengthConfig;
  readonly threadOutRef: string;
  readonly header: Header;
  readonly membership: ForcedMembership;
}): Promise<FieldPreimageLengthForcedDispatchBuild> => {
  const { subject, rejectionReason } = forcedSubject(membership);
  if (
    rejectionReason === null ||
    typeof rejectionReason !== "object" ||
    !("FieldPreimageLengthMismatch" in rejectionReason)
  )
    throw new Error(
      `${LABEL}: forced leaf does not reject with a field-length mismatch`,
    );
  const fieldIndex = rejectionReason.FieldPreimageLengthMismatch.field_index;
  if (fieldIndex >= 0n && fieldIndex < BigInt(MIDGARD_FIELD_COUNT))
    throw new Error(
      `${LABEL}: field ${fieldIndex.toString()} is inside the native layout`,
    );
  return await buildForcedStep02({
    config,
    threadOutRef,
    state: { subject, fieldIndex, declaredLength: 0n, actualLength: 0n },
    args: (layout) => ({
      AuthenticateForcedOutOfDomain: {
        input_index: layout.inputIndex,
        output_index: layout.outputIndex,
        header,
        membership: forcedLeafMembershipV1(membership),
      },
    }),
    carriageReferenceInputs: [],
  });
};
