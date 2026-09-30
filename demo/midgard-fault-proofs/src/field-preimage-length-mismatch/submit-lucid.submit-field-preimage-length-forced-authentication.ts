import {
  type CommittedFieldClaim,
  FieldPreimageLengthStep02RedeemerSchema,
  FieldPreimageLengthStep03DatumSchema,
  type ForcedInclusionTxV1,
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

import { DEFAULT_CONFIRMATION_POLL_MS } from "../runtime.js";
import { selectFeeInput } from "../step-support.js";
import { computationThreadOutputPredicate } from "../tx-layout.js";
import type { FraudProofPreSubmitBoundary } from "../workflow/transaction-boundary.js";
import {
  reachFraudProofPreSubmitBoundary,
  workflowReferenceScriptsUsedByTransaction,
} from "../workflow/transaction-boundary.js";
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
              membership,
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
  const resolvedClaim =
    claimResolver?.(completeReferences) ??
    claim ??
    (() => {
      throw new Error(`${LABEL}: forced authentication omitted field claim`);
    })();
  config.signer.selectWallet(config.lucid);
  const feeInput = selectFeeInput(await config.lucid.wallet().getUtxos());
  const unsigned = await config.lucid
    .newTx()
    .collectFrom([feeInput])
    .collectFrom([threadUtxo], redeemer)
    .pay.ToContract(
      terminal.spendingScriptAddress,
      { kind: "inline", value: datum },
      { lovelace: threadUtxo.assets.lovelace ?? 0n, [threadToken.unit]: 1n },
    )
    .addSignerKey(config.signer.paymentKeyHash)
    .readFrom(completeReferences)
    .complete({ localUPLCEval: true });
  if (layout === undefined) throw new Error(`${LABEL}: layout did not resolve`);
  const resolved = layout;
  const signed = await unsigned.sign.withWallet().complete();
  const expectedTxHash = await reachFraudProofPreSubmitBoundary({
    signed,
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
  });
  const txHash = await signed.submit();
  if (txHash !== expectedTxHash)
    throw new Error(`${LABEL}: provider returned a different transaction id`);
  if (awaitConfirmation)
    await config.lucid.awaitTx(txHash, DEFAULT_CONFIRMATION_POLL_MS);
  return {
    txHash,
    nextThreadOutRef: `${txHash}#${resolved.outputIndex.toString()}`,
    computationThreadUnit: threadToken.unit,
    inputIndex: Number(resolved.inputIndex),
    outputIndex: Number(resolved.outputIndex),
  };
};
