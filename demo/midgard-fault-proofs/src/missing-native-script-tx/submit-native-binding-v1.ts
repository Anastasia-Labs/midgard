import {
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
  type LucidEvolution,
  type Network,
  type Script,
  scriptHashToCredential,
  toUnit,
  type UTxO,
} from "@lucid-evolution/lucid";

import {
  DEFAULT_CONFIRMATION_POLL_MS,
  encodeRawPhasMembershipProofRedeemer,
  fetchUtxoByOutRef,
  getCompiledScript,
  parseOutRef,
  phasMembershipRewardAddress,
  requireSingletonUtxo,
  type ResolvedProverSigner,
  resolveFraudulentHeaderHash,
} from "../runtime.js";
import {
  PHAS_MEMBERSHIP_WITHDRAW_TITLE,
  requireNativeTxMatchesCompactCbor,
  selectFeeInput,
  type SubmitStep01TxInclusion,
} from "../submit-step-01.js";
import { computationThreadOutputPredicate } from "../tx-layout.js";
import type { MissingNativeScriptTxContractsV1 } from "./contracts-v1.js";
import {
  type MissingNativeScriptTxStepIndexV1,
  missingNativeScriptTxStepLabelV1,
  missingNativeScriptTxSubmitError,
  requireMissingNativeScriptTxReferenceScriptV1,
} from "./submit-common-v1.js";

export type MissingNativeScriptTxBindingResultV1 = {
  readonly txHash: string;
  readonly nextThreadOutRef: string;
  readonly inputIndex: number;
  readonly outputIndex: number;
  readonly hubOracleRefInputIndex: number;
  readonly stateQueueNodeRefInputIndex: number;
};

/** Shared body of the two bare-`NativeTxInclusionArgs` binding steps. */
export const submitMissingNativeScriptTxBindingV1 = async ({
  lucid,
  blueprint,
  network,
  contracts,
  signer,
  stepIndex,
  threadUtxo,
  threadToken,
  stateQueueBlockOutRef,
  txInclusion,
  nextDatum,
  spendRedeemerSchema,
  referenceScriptUtxo,
  awaitConfirmation,
}: {
  readonly lucid: LucidEvolution;
  readonly blueprint: unknown;
  readonly network: Network;
  readonly contracts: MissingNativeScriptTxContractsV1;
  readonly signer: ResolvedProverSigner;
  readonly stepIndex: 0 | 2;
  readonly threadUtxo: UTxO;
  readonly threadToken: {
    readonly unit: string;
    readonly fraudulentHeaderHash: string;
  };
  readonly stateQueueBlockOutRef: string;
  readonly txInclusion: SubmitStep01TxInclusion;
  readonly nextDatum: string;
  readonly spendRedeemerSchema: Parameters<typeof Data.to>[1];
  readonly referenceScriptUtxo: UTxO;
  readonly awaitConfirmation: boolean;
}): Promise<MissingNativeScriptTxBindingResultV1> => {
  const label = missingNativeScriptTxStepLabelV1(
    stepIndex as MissingNativeScriptTxStepIndexV1,
  );
  const [hubOracleUtxo, stateQueueBlockUtxo] = await Promise.all([
    requireSingletonUtxo({
      lucid,
      address: credentialToAddress(
        network,
        scriptHashToCredential(contracts.hubOraclePolicyId),
      ),
      unit: toUnit(contracts.hubOraclePolicyId, HUB_ORACLE_ASSET_NAME),
      label: `${label} hub oracle`,
    }),
    fetchUtxoByOutRef({
      lucid,
      outRef: parseOutRef(stateQueueBlockOutRef, "--state-queue-block-out-ref"),
      label: `${label} state-queue block`,
    }),
  ]);
  const headerHash = resolveFraudulentHeaderHash({
    stateQueuePolicyId: contracts.stateQueuePolicyId,
    fraudulentBlockUtxo: stateQueueBlockUtxo,
  });
  if (headerHash !== threadToken.fraudulentHeaderHash) {
    throw missingNativeScriptTxSubmitError(
      `state-queue header ${headerHash} does not match thread header ${threadToken.fraudulentHeaderHash}.`,
    );
  }
  requireNativeTxMatchesCompactCbor(txInclusion);

  signer.selectWallet(lucid);
  const feeInput = selectFeeInput(await lucid.wallet().getUtxos());
  const phasScript: Script = {
    type: "PlutusV3",
    script: getCompiledScript(blueprint, PHAS_MEMBERSHIP_WITHDRAW_TITLE),
  };
  const phasAddress = phasMembershipRewardAddress(network, phasScript);
  const nextStep = contracts.steps[stepIndex + 1];
  const outputMatches = computationThreadOutputPredicate({
    address: nextStep.spendingScriptAddress,
    datum: nextDatum,
    unit: threadToken.unit,
  });
  let layout:
    | {
        inputIndex: bigint;
        outputIndex: bigint;
        hubOracleRefInputIndex: bigint;
        stateQueueNodeRefInputIndex: bigint;
      }
    | undefined;
  const redeemer = ((ctx) => {
    requireOwnSpendPurpose(ctx, threadUtxo, label);
    const resolved = {
      inputIndex: requireInputIndex(ctx, threadUtxo, label),
      outputIndex: requireUniqueOutputIndex(
        ctx.outputs,
        outputMatches,
        `${label} output`,
      ),
      hubOracleRefInputIndex: requireReferenceInputIndex(
        ctx,
        hubOracleUtxo,
        `${label} hub oracle`,
      ),
      stateQueueNodeRefInputIndex: requireReferenceInputIndex(
        ctx,
        stateQueueBlockUtxo,
        `${label} state-queue node`,
      ),
    };
    layout = resolved;
    return Data.to(
      {
        Continue: [
          {
            input_index: resolved.inputIndex,
            output_index: resolved.outputIndex,
            hub_ref_input_index: resolved.hubOracleRefInputIndex,
            state_queue_node_ref_input_index:
              resolved.stateQueueNodeRefInputIndex,
            native_tx_id: txInclusion.nativeTxId,
            native_tx_compact_cbor: txInclusion.nativeTxCompactCbor,
            transactions_phas_root: txInclusion.transactionsPhasRoot,
            tx_membership_proof: txInclusion.txMembershipProof,
            inclusion_proof_script_withdraw_redeemer_index:
              requireWithdrawalRedeemerIndex(
                ctx,
                phasAddress,
                `${label} PHAS membership`,
              ),
          },
        ],
      },
      spendRedeemerSchema,
    );
  }) satisfies BuildTxWithRedeemer;
  const referenceInputs = [
    hubOracleUtxo,
    stateQueueBlockUtxo,
    requireMissingNativeScriptTxReferenceScriptV1({
      utxo: referenceScriptUtxo,
      expectedScriptHash: contracts.steps[stepIndex].spendingScriptHash,
      stepIndex,
    }),
  ];
  const base = lucid
    .newTx()
    .collectFrom([feeInput])
    .collectFrom([threadUtxo], redeemer)
    .readFrom(referenceInputs)
    .withdraw(
      phasAddress,
      0n,
      encodeRawPhasMembershipProofRedeemer({
        root: txInclusion.transactionsPhasRoot,
        keyBytes: txInclusion.nativeTxId,
        valueBytes: txInclusion.nativeTxCompactCbor,
        membershipProofCbor: txInclusion.txMembershipProofCbor,
      }),
    )
    .pay.ToContract(
      nextStep.spendingScriptAddress,
      { kind: "inline", value: nextDatum },
      {
        lovelace: threadUtxo.assets.lovelace ?? 0n,
        [threadToken.unit]: 1n,
      },
    )
    .addSignerKey(signer.paymentKeyHash)
    .attach.WithdrawalValidator(phasScript);
  const unsigned = await base.complete({ localUPLCEval: true });
  if (layout === undefined) {
    throw missingNativeScriptTxSubmitError(
      `BuildTxWithRedeemer did not resolve ${label} layout.`,
    );
  }
  const signed = await unsigned.sign.withWallet().complete();
  const txHash = await signed.submit();
  if (awaitConfirmation) {
    await lucid.awaitTx(txHash, DEFAULT_CONFIRMATION_POLL_MS);
  }
  return {
    txHash,
    nextThreadOutRef: `${txHash}#${layout.outputIndex.toString()}`,
    inputIndex: Number(layout.inputIndex),
    outputIndex: Number(layout.outputIndex),
    hubOracleRefInputIndex: Number(layout.hubOracleRefInputIndex),
    stateQueueNodeRefInputIndex: Number(layout.stateQueueNodeRefInputIndex),
  };
};
