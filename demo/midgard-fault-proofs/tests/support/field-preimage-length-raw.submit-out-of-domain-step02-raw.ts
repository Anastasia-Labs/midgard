import {
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
import { type BuildTxWithRedeemer, Data } from "@lucid-evolution/lucid";

import type { ManifestBoundFieldPreimageLengthConfig } from "../../src/field-preimage-length-mismatch/config.js";
import { requireThread } from "../../src/field-preimage-length-mismatch/submit-lucid.submit-field-preimage-length-cancel.js";
import { buildLinearFaultContinue } from "../../src/linear-fault-submit.js";
import { computationThreadOutputPredicate } from "../../src/tx-layout.js";
import { buildAndSubmitTransactionUnjournaled } from "./submit-unjournaled.js";

/**
 * Forced step 02 built by hand on the out-of-layout route: the thread moves to
 * the terminal with `fieldIndex` and zero lengths, whatever the forced leaf's
 * verdict or the field it names. No builder check runs on whether that field
 * lies outside the native layout.
 */
export const submitRawFieldPreimageLengthOutOfDomainStep02 = async ({
  config,
  threadOutRef,
  header,
  membership,
  fieldIndex,
}: {
  readonly config: ManifestBoundFieldPreimageLengthConfig;
  readonly threadOutRef: string;
  readonly header: Header;
  readonly membership: RootMembershipProof<
    OutputReference,
    ForcedInclusionTxV1
  >;
  readonly fieldIndex: bigint;
}): Promise<string> => {
  const label = "raw field-preimage-length out-of-domain step-02";
  const { threadUtxo, threadToken, step } = await requireThread({
    config,
    threadOutRef,
    stepIndex: 2,
  });
  const verdict = membership.value.verdict;
  const subject = forcedVerdictSubject({
    transactionId: membership.value.tx_id,
    sourceKey: membership.key,
    rejectionReason:
      verdict === "ForcedTxValid" ? null : verdict.ForcedTxInvalid.reason,
  });
  const terminal = config.contracts.fieldPreimageLengthMismatch.steps[3];
  const nextDatum = Data.to(
    {
      fraud_prover: config.signer.paymentKeyHash,
      data: {
        subject,
        field_index: fieldIndex,
        declared_length: 0n,
        actual_length: 0n,
      },
    } as never,
    FieldPreimageLengthStep03DatumSchema as never,
  );
  const outputMatches = computationThreadOutputPredicate({
    address: terminal.spendingScriptAddress,
    datum: nextDatum,
    unit: threadToken.unit,
  });
  const redeemer = ((ctx) => {
    requireOwnSpendPurpose(ctx, threadUtxo, label);
    return Data.to(
      {
        Continue: [
          {
            AuthenticateForcedOutOfDomain: {
              input_index: requireInputIndex(ctx, threadUtxo, label),
              output_index: requireUniqueOutputIndex(
                ctx.outputs,
                outputMatches,
                label,
              ),
              header,
              membership: forcedLeafMembershipV1(membership),
            },
          },
        ],
      } as never,
      FieldPreimageLengthStep02RedeemerSchema as never,
    );
  }) satisfies BuildTxWithRedeemer;
  return await buildAndSubmitTransactionUnjournaled(buildLinearFaultContinue, {
    lucid: config.lucid,
    signer: config.signer,
    threadUtxo,
    threadUnit: threadToken.unit,
    stepReference: config.referenceScripts.step02Forced,
    stepScript: step.spendingScript,
    stepRole: label,
    nextAddress: terminal.spendingScriptAddress,
    nextDatum,
    redeemer,
    awaitConfirmation: true,
  });
};
