import {
  acceptedVerdictSubject,
  type CommittedFieldClaim,
  FieldPreimageLengthStep01RedeemerSchema,
  FieldPreimageLengthStep02DatumSchema,
  HUB_ORACLE_ASSET_NAME,
  requireInputIndex,
  requireOwnSpendPurpose,
  requireReferenceInputIndex,
  requireUniqueOutputIndex,
  requireWithdrawalRedeemerIndex,
} from "@al-ft/midgard-sdk";
import {
  type BuildTxWithRedeemer,
  credentialToAddress,
  Data,
  type Script,
  scriptHashToCredential,
  toUnit,
  type UTxO,
} from "@lucid-evolution/lucid";

import {
  encodeRawPhasMembershipProofRedeemer,
  fetchUtxoByOutRef,
  getCompiledScript,
  parseOutRef,
  phasMembershipRewardAddress,
  requireSingletonUtxo,
  resolveFraudulentHeaderHash,
} from "../runtime.js";
import { completeStep, fundStep, submitStep } from "../step-completion.js";
import {
  PHAS_MEMBERSHIP_WITHDRAW_TITLE,
  requireInitialStepDatum,
  requireNativeTxMatchesCompactCbor,
  type SubmitStep01TxInclusion,
} from "../step-support.js";
import { computationThreadOutputPredicate } from "../tx-layout.js";
import { witnessWithdrawalValidatorCarriage } from "../witness-reference-scripts.js";
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

/** Real accepted-source dispatch with an authenticated PHAS inclusion. */
export const submitFieldPreimageLengthAcceptedDispatch = async ({
  config,
  threadOutRef,
  stateQueueBlockOutRef,
  inclusion: txInclusion,
  claim,
  claimResolver,
  carriageReferenceInputs = [],
  preSubmitBoundary,
  awaitConfirmation = true,
}: {
  readonly config: ManifestBoundFieldPreimageLengthConfig;
  readonly threadOutRef: string;
  readonly stateQueueBlockOutRef: string;
  readonly inclusion: SubmitStep01TxInclusion;
  readonly claim?: CommittedFieldClaim;
  readonly claimResolver?: FieldPreimageLengthClaimResolver;
  readonly carriageReferenceInputs?: readonly UTxO[];
  readonly preSubmitBoundary?: FraudProofPreSubmitBoundary;
  readonly awaitConfirmation?: boolean;
}): Promise<SubmitFieldPreimageLengthForcedDispatchResult> => {
  const { threadUtxo, threadToken, step } = await requireThread({
    config,
    threadOutRef,
    stepIndex: 0,
  });
  requireInitialStepDatum({ threadUtxo, signer: config.signer });
  requireNativeTxMatchesCompactCbor(txInclusion);
  const [stateQueueBlockUtxo, hubOracleUtxo] = await Promise.all([
    fetchUtxoByOutRef({
      lucid: config.lucid,
      outRef: parseOutRef(stateQueueBlockOutRef, "--state-queue-block-out-ref"),
      label: `${LABEL} state-queue block`,
    }),
    requireSingletonUtxo({
      lucid: config.lucid,
      address: credentialToAddress(
        config.binding.network,
        scriptHashToCredential(
          config.binding.resolvedContracts.hubOraclePolicyId,
        ),
      ),
      unit: toUnit(
        config.binding.resolvedContracts.hubOraclePolicyId,
        HUB_ORACLE_ASSET_NAME,
      ),
      label: `${LABEL} hub oracle`,
    }),
  ]);
  const headerHash = resolveFraudulentHeaderHash({
    stateQueuePolicyId: config.binding.definition.stateQueue.policyId,
    fraudulentBlockUtxo: stateQueueBlockUtxo,
  });
  if (headerHash !== threadToken.fraudulentHeaderHash) {
    throw new Error(`${LABEL}: accepted source targets a different header`);
  }
  const phasScript: Script = {
    type: "PlutusV3",
    script: getCompiledScript(
      config.binding.blueprint,
      PHAS_MEMBERSHIP_WITHDRAW_TITLE,
    ),
  };
  const phasAddress = phasMembershipRewardAddress(
    config.binding.network,
    phasScript,
  );
  const carriage = witnessWithdrawalValidatorCarriage({
    script: phasScript,
    referenceUtxo: config.referenceScripts.witnesses.phasMembershipWithdraw,
    label: `${LABEL} PHAS membership`,
  });
  const reference = requireReference({
    utxo: config.referenceScripts.step01,
    expectedHash: step.spendingScriptHash,
    role: "step-01",
  });
  const references = [
    hubOracleUtxo,
    stateQueueBlockUtxo,
    reference,
    ...carriage.referenceInputs,
    ...carriageReferenceInputs,
  ];
  if (claimResolver === undefined && claim === undefined)
    throw new Error(`${LABEL}: accepted dispatch omitted field claim`);
  const next = config.contracts.fieldPreimageLengthMismatch.acceptedStep02;
  const datum = Data.to(
    {
      fraud_prover: config.signer.paymentKeyHash,
      data: {
        BoundSource: {
          subject: acceptedVerdictSubject(txInclusion.nativeTxId),
          source_cbor: txInclusion.l2TransactionSourceCbor,
        },
      },
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
    // §8.7 carriage indices count into the built reference-input set.
    const resolvedClaim =
      claimResolver === undefined ? claim! : claimResolver(ctx.referenceInputs);
    requireOwnSpendPurpose(ctx, threadUtxo, `${LABEL} accepted dispatch`);
    layout = {
      inputIndex: requireInputIndex(ctx, threadUtxo, LABEL),
      outputIndex: requireUniqueOutputIndex(
        ctx.outputs,
        outputMatches,
        `${LABEL} accepted output`,
      ),
    };
    const inclusion = {
      RedeemerCarriedInclusion: [
        {
          input_index: layout.inputIndex,
          output_index: layout.outputIndex,
          hub_ref_input_index: requireReferenceInputIndex(
            ctx,
            hubOracleUtxo,
            `${LABEL} hub oracle`,
          ),
          state_queue_node_ref_input_index: requireReferenceInputIndex(
            ctx,
            stateQueueBlockUtxo,
            `${LABEL} state queue`,
          ),
          native_tx_id: txInclusion.nativeTxId,
          l2_transaction_source_cbor: txInclusion.l2TransactionSourceCbor,
          transactions_phas_root: txInclusion.transactionsPhasRoot,
          tx_membership_proof: txInclusion.txMembershipProof,
          inclusion_proof_script_withdraw_redeemer_index:
            requireWithdrawalRedeemerIndex(
              ctx,
              phasAddress,
              `${LABEL} PHAS membership`,
            ),
        },
      ],
    };
    return Data.to(
      {
        Continue: [{ BindAccepted: { inclusion, claim: resolvedClaim } }],
      } as never,
      FieldPreimageLengthStep01RedeemerSchema as never,
    );
  }) satisfies BuildTxWithRedeemer;
  const funding = await fundStep(config.lucid, { signer: config.signer });
  const base = funding.tx
    .collectFrom([threadUtxo], redeemer)
    .readFrom(references)
    .withdraw(
      phasAddress,
      0n,
      encodeRawPhasMembershipProofRedeemer({
        root: txInclusion.transactionsPhasRoot,
        keyBytes: txInclusion.nativeTxId,
        valueBytes: txInclusion.l2TransactionSourceCbor,
        membershipProofCbor: txInclusion.txMembershipProofCbor,
      }),
    )
    .pay.ToContract(
      next.spendingScriptAddress,
      { kind: "inline", value: datum },
      {
        lovelace: threadUtxo.assets.lovelace ?? 0n,
        [threadToken.unit]: 1n,
      },
    );
  const signed = await completeStep(carriage.attach(base), funding);
  if (layout === undefined) throw new Error(`${LABEL}: layout did not resolve`);
  const resolved = layout;
  const txHash = await submitStep(config.lucid, signed, {
    label: `${LABEL} step-01 accepted`,
    referenceScripts: workflowReferenceScriptsUsedByTransaction({
      signed,
      candidates: [
        {
          role: "V1 field-preimage-length step-01",
          utxo: reference,
          expectedScript: step.spendingScript,
        },
        {
          role: "membership proof withdrawal",
          utxo: config.referenceScripts.witnesses.phasMembershipWithdraw,
          expectedScript: phasScript,
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
